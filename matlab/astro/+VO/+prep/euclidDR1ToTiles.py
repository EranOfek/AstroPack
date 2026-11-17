"""Convert the Euclid DR1 MER catalog (+ PHZ/SPE classification and SPE
redshift) into RA/Dec tiles for the catsHTM builder VO.prep.buildHTMfromTiles.

The data are read from the IRSA HATS Parquet files on AWS S3
(bucket nasa-irsa-euclid-dr1, anonymous access). Only the needed columns
are transferred.

Pass 1 ('frag'): for each MER HATS partition, read the MER columns, join
    the PHZ classification, SPE classification and the best (spe_rank=0)
    SPE galaxy/QSO redshift candidates by object_id, and write one
    36-column Parquet fragment sorted by Dec (FragDir/frag_<o>_<n>.parquet).
    A fragment is written to a temporary name and renamed, so an
    interrupted run resumes by skipping the existing fragments.
Pass 2 ('tiles'): cut the fragments into Dec rows and RA tiles. Each
    tile file holds the sources inside the tile plus a margin around it
    (the RA margin is scaled by 1/cos(Dec) and wraps at RA 0/360), so
    every HTM cell whose center is in the tile can be built from that
    file alone. Tiles are HDF5 files with dataset /data [36 x N] (read in
    MATLAB as N x 36) and attributes MinRA, MaxRA, MinDec, MaxDec [rad]
    giving the exact tile bounds (without the margin).

Output columns (RA/Dec in rad, fluxes in uJy, NaN = no value):
    see COLNAMES below.

Example:
    python euclidDR1ToTiles.py --survey deep --outdir ~/tmp/EuclidDR1deep --step all --workers 8

Author: Dana Kovaleva (Nov 2026)
"""
import argparse
import json
import math
import os
import time
from concurrent.futures import ProcessPoolExecutor

import h5py
import numpy as np
import pyarrow as pa
import pyarrow.csv as pacsv
import pyarrow.fs as pafs
import pyarrow.parquet as pq

BUCKET = 'nasa-irsa-euclid-dr1/contributed/dr1/hats/reformatted_catalogs'

TABLES = {
    'wide': {'mer': 'mer_catalogue_wide_survey',
             'phz': 'phz_classification_wide',
             'spe': 'spectro_z_spe_classification_wide_survey',
             'gal': 'spectro_z_spe_galaxy_candidates_wide_survey',
             'qso': 'spectro_z_spe_qso_candidates_wide_survey'},
    'deep': {'mer': 'mer_catalogue_deep_survey',
             'phz': 'phz_classification_deep_survey',
             'spe': 'spectro_z_spe_classification_deep',
             'gal': 'spectro_z_spe_galaxy_candidates_deep',
             'qso': 'spectro_z_spe_qso_candidates_deep'},
}

VIS_PIXSCALE = 0.1   # [arcsec/pix] of the MER VIS mosaics (semimajor_axis)

MER_COLS = ['object_id', 'ra', 'dec',
            'flux_vis_psf', 'fluxerr_vis_psf',
            'flux_y_templfit', 'fluxerr_y_templfit',
            'flux_j_templfit', 'fluxerr_j_templfit',
            'flux_h_templfit', 'fluxerr_h_templfit',
            'flux_detection_total', 'fluxerr_detection_total',
            'flux_vis_sersic', 'fluxerr_vis_sersic',
            'semimajor_axis', 'ellipticity', 'position_angle',
            'point_like_prob', 'spurious_flag', 'det_quality_flag',
            'flag_vis', 'vis_det', 'has_spectrum']
PHZ_COLS = ['object_id', 'phz_star_prob', 'phz_gal_prob', 'phz_qso_prob',
            'phz_glob_cl_prob', 'phz_classification']
SPE_COLS = ['object_id', 'spe_star_prob', 'spe_gal_prob', 'spe_qso_prob', 'spe_class']
Z_COLS = ['object_id', 'spe_rank', 'spe_z', 'spe_z_err', 'spe_z_prob', 'spe_z_rel']

COLNAMES = ['RA', 'Dec',
            'Flux_VIS_PSF', 'FluxErr_VIS_PSF',
            'Flux_Y', 'FluxErr_Y', 'Flux_J', 'FluxErr_J', 'Flux_H', 'FluxErr_H',
            'Flux_Total', 'FluxErr_Total',
            'Flux_VIS_Sersic', 'FluxErr_VIS_Sersic',
            'SemiMajorAxis', 'Ellipticity', 'PA',
            'PointLikeProb', 'SpuriousFlag', 'DetQualityFlag', 'FlagVIS', 'VisDet', 'HasSpectrum',
            'PHZ_StarProb', 'PHZ_GalProb', 'PHZ_QSOProb', 'PHZ_GlobClProb', 'PHZ_Class',
            'SPE_StarProb', 'SPE_GalProb', 'SPE_QSOProb', 'SPE_Class',
            'SPE_z', 'SPE_zErr', 'SPE_zProb', 'SPE_zRel']
COLUNITS = ['rad', 'rad'] + ['uJy'] * 12 + ['arcsec', '', 'deg'] + [''] * 19

# SPE class (lowercase in the data: galaxy, star, qso) coded with the PHZ_Class bits
SPE_CLASS_CODE = {'STAR': 1, 'GALAXY': 2, 'QSO': 4, 'UNDEF': 0}


def s3():
    return pafs.S3FileSystem(anonymous=True, region='us-east-1')


def hats_dir(table):
    return f'{BUCKET}/{table}/euclid_dr1_reformatted_catalogs_{table}-hats'


def partitions(fs, table):
    """List of (order, pix) of the HATS partitions of a table."""
    with fs.open_input_stream(hats_dir(table) + '/partition_info.csv') as f:
        T = pacsv.read_csv(f)
    return list(zip(T['Norder'].to_pylist(), T['Npix'].to_pylist()))


def part_path(table, order, pix):
    return (f'{hats_dir(table)}/dataset/Norder={order}/Dir={10000 * (pix // 10000)}'
            f'/Npix={pix}/part0.snappy.parquet')


def hp29_range(order, pix):
    """[start, end) of the order-29 pixel indices inside a HEALPix pixel."""
    shift = 2 * (29 - order)
    return pix << shift, (pix + 1) << shift


def overlapping(parts, order, pix):
    """Partitions of another table that overlap the pixel (order, pix)."""
    lo, hi = hp29_range(order, pix)
    out = []
    for o, n in parts:
        a, b = hp29_range(o, n)
        if a < hi and b > lo:
            out.append((o, n))
    return out


def read_parquet(fs, path, columns, filters=None, retries=3):
    for attempt in range(retries):
        try:
            return pq.read_table(path, filesystem=fs, columns=columns, filters=filters)
        except OSError:
            if attempt == retries - 1:
                raise
            time.sleep(10 * (attempt + 1))


class AuxCache:
    """Joined-table partitions kept in memory while MER partitions that
    overlap them are processed (MER partitions are processed in HEALPix
    order, so each auxiliary partition is read once per worker)."""

    def __init__(self, fs, table, columns, filters=None):
        self.fs = fs
        self.table = table
        self.columns = columns
        self.filters = filters
        self.parts = partitions(fs, table)
        self.cache = {}

    def get(self, order, pix):
        """Sorted object_id array and column dict for the MER pixel."""
        need = overlapping(self.parts, order, pix)
        lo, _ = hp29_range(order, pix)
        for key in list(self.cache):
            if key not in need and hp29_range(*key)[1] <= lo:
                del self.cache[key]
        tabs = []
        for key in need:
            if key not in self.cache:
                self.cache[key] = read_parquet(self.fs, part_path(self.table, *key),
                                               self.columns, self.filters)
            tabs.append(self.cache[key])
        if not tabs:
            return np.zeros(0, np.int64), {}
        T = pa.concat_tables(tabs)
        ids = T['object_id'].to_numpy()
        order_ids = np.argsort(ids, kind='stable')
        cols = {c: T[c].to_numpy(zero_copy_only=False)[order_ids]
                for c in self.columns if c != 'object_id'}
        return ids[order_ids], cols


def lookup(ids_sorted, query):
    """Index into ids_sorted for each query id, -1 if absent (first match)."""
    if ids_sorted.size == 0:
        return np.full(query.size, -1)
    k = np.searchsorted(ids_sorted, query)
    k[k == ids_sorted.size] = 0
    k[ids_sorted[k] != query] = -1
    return k


def take(col, k):
    out = np.full(k.size, np.nan)
    ok = k >= 0
    out[ok] = col[k[ok]]
    return out


def to_float(arr):
    """Numeric (possibly null-masked) array to float64 with NaN."""
    return np.asarray(arr.to_numpy(zero_copy_only=False), dtype=np.float64) \
        if hasattr(arr, 'to_numpy') else np.asarray(arr, dtype=np.float64)


def make_fragment(fs, aux, order, pix, survey, outfile):
    t0 = time.time()
    M = read_parquet(fs, part_path(TABLES[survey]['mer'], order, pix), MER_COLS)
    oid = M['object_id'].to_numpy()
    N = oid.size
    out = {}
    out['RA'] = np.deg2rad(to_float(M['ra']))
    out['Dec'] = np.deg2rad(to_float(M['dec']))
    for name, col in [('Flux_VIS_PSF', 'flux_vis_psf'), ('FluxErr_VIS_PSF', 'fluxerr_vis_psf'),
                      ('Flux_Y', 'flux_y_templfit'), ('FluxErr_Y', 'fluxerr_y_templfit'),
                      ('Flux_J', 'flux_j_templfit'), ('FluxErr_J', 'fluxerr_j_templfit'),
                      ('Flux_H', 'flux_h_templfit'), ('FluxErr_H', 'fluxerr_h_templfit'),
                      ('Flux_Total', 'flux_detection_total'), ('FluxErr_Total', 'fluxerr_detection_total'),
                      ('Flux_VIS_Sersic', 'flux_vis_sersic'), ('FluxErr_VIS_Sersic', 'fluxerr_vis_sersic'),
                      ('Ellipticity', 'ellipticity'), ('PA', 'position_angle'),
                      ('PointLikeProb', 'point_like_prob'), ('SpuriousFlag', 'spurious_flag'),
                      ('DetQualityFlag', 'det_quality_flag'), ('FlagVIS', 'flag_vis'),
                      ('VisDet', 'vis_det'), ('HasSpectrum', 'has_spectrum')]:
        out[name] = to_float(M[col])
    out['SemiMajorAxis'] = to_float(M['semimajor_axis']) * VIS_PIXSCALE

    stats = {'order': order, 'pix': pix, 'n': int(N)}

    ids, c = aux['phz'].get(order, pix)
    k = lookup(ids, oid)
    stats['n_phz'] = int((k >= 0).sum())
    for name, col in [('PHZ_StarProb', 'phz_star_prob'), ('PHZ_GalProb', 'phz_gal_prob'),
                      ('PHZ_QSOProb', 'phz_qso_prob'), ('PHZ_GlobClProb', 'phz_glob_cl_prob'),
                      ('PHZ_Class', 'phz_classification')]:
        out[name] = take(np.asarray(c.get(col, []), dtype=np.float64), k)

    ids, c = aux['spe'].get(order, pix)
    k = lookup(ids, oid)
    stats['n_spe'] = int((k >= 0).sum())
    for name, col in [('SPE_StarProb', 'spe_star_prob'), ('SPE_GalProb', 'spe_gal_prob'),
                      ('SPE_QSOProb', 'spe_qso_prob')]:
        out[name] = take(np.asarray(c.get(col, []), dtype=np.float64), k)
    cls = np.array([SPE_CLASS_CODE.get(s.strip().upper(), np.nan) if s is not None else np.nan
                    for s in c.get('spe_class', [])], dtype=np.float64)
    out['SPE_Class'] = take(cls, k)

    # redshift of the class picked by SPE: galaxy or QSO best candidate
    for name in ['SPE_z', 'SPE_zErr', 'SPE_zProb', 'SPE_zRel']:
        out[name] = np.full(N, np.nan)
    for key, code in [('gal', 2), ('qso', 4)]:
        ids, c = aux[key].get(order, pix)
        k = lookup(ids, oid)
        k[out['SPE_Class'] != code] = -1
        stats['n_z_' + key] = int((k >= 0).sum())
        sel = k >= 0
        for name, col in [('SPE_z', 'spe_z'), ('SPE_zErr', 'spe_z_err'),
                          ('SPE_zProb', 'spe_z_prob'), ('SPE_zRel', 'spe_z_rel')]:
            out[name][sel] = np.asarray(c[col], dtype=np.float64)[k[sel]]
    stats['n_class_gal'] = int((out['SPE_Class'] == 2).sum())
    stats['n_class_qso'] = int((out['SPE_Class'] == 4).sum())

    srt = np.argsort(out['Dec'], kind='stable')
    T = pa.table({name: out[name][srt] for name in COLNAMES})
    tmp = outfile + '.tmp'
    pq.write_table(T, tmp, compression='zstd', row_group_size=500_000)
    os.replace(tmp, outfile)
    stats['dec_min'] = float(out['Dec'].min()) if N else float('nan')
    stats['dec_max'] = float(out['Dec'].max()) if N else float('nan')
    stats['sec'] = round(time.time() - t0, 1)
    with open(outfile.replace('.parquet', '.json'), 'w') as f:
        json.dump(stats, f)
    return stats


def frag_worker(survey, fragdir, chunk):
    fs = s3()
    tabs = TABLES[survey]
    rank0 = [('spe_rank', '=', 0)]
    aux = {'phz': AuxCache(fs, tabs['phz'], PHZ_COLS),
           'spe': AuxCache(fs, tabs['spe'], SPE_COLS),
           'gal': AuxCache(fs, tabs['gal'], Z_COLS, rank0),
           'qso': AuxCache(fs, tabs['qso'], Z_COLS, rank0)}
    done = []
    for order, pix in chunk:
        outfile = os.path.join(fragdir, f'frag_{order}_{pix}.parquet')
        if os.path.exists(outfile):
            continue
        st = make_fragment(fs, aux, order, pix, survey, outfile)
        print(json.dumps(st), flush=True)
        done.append(st)
    return done


def run_frag(survey, outdir, workers):
    fragdir = os.path.join(outdir, 'frag')
    os.makedirs(fragdir, exist_ok=True)
    parts = sorted(partitions(s3(), TABLES[survey]['mer']), key=lambda p: hp29_range(*p)[0])
    # contiguous chunks so that each worker reads every auxiliary partition once
    nchunk = max(1, min(len(parts), 4 * workers))
    chunks = [list(c) for c in np.array_split(np.array(parts, dtype=object), nchunk) if len(c)]
    chunks = [[(int(o), int(n)) for o, n in c] for c in chunks]
    print(f'{len(parts)} MER partitions in {len(chunks)} chunks, {workers} workers', flush=True)
    with ProcessPoolExecutor(max_workers=workers) as ex:
        list(ex.map(frag_worker, [survey] * len(chunks), [fragdir] * len(chunks), chunks))


def tile_grid(row_height, tile_width):
    """Dec row edges [deg] and number of RA tiles per row."""
    edges = np.arange(-90.0, 90.0 + row_height / 2, row_height)
    ntile = []
    for lo, hi in zip(edges[:-1], edges[1:]):
        cosd = math.cos(math.radians(min(abs(lo), abs(hi)))) if lo * hi > 0 else 1.0
        ntile.append(max(1, int(360.0 * cosd // tile_width)))
    return edges, ntile


def run_tiles(outdir, row_height, tile_width, margin):
    fragdir = os.path.join(outdir, 'frag')
    tiledir = os.path.join(outdir, 'tiles')
    os.makedirs(tiledir, exist_ok=True)
    frags = []
    for f in sorted(os.listdir(fragdir)):
        if f.endswith('.json'):
            with open(os.path.join(fragdir, f)) as fh:
                st = json.load(fh)
            if st['n'] > 0:
                frags.append((os.path.join(fragdir, f.replace('.json', '.parquet')),
                              math.degrees(st['dec_min']), math.degrees(st['dec_max'])))
    if not frags:
        raise RuntimeError('no fragments in ' + fragdir)
    dmin = min(f[1] for f in frags)
    dmax = max(f[2] for f in frags)
    edges, ntile = tile_grid(row_height, tile_width)
    ntot = 0
    for irow in range(len(edges) - 1):
        lo, hi = edges[irow], edges[irow + 1]
        if hi + margin < dmin or lo - margin > dmax:
            continue
        marker = os.path.join(tiledir, f'row_{irow:04d}.done')
        if os.path.exists(marker):
            continue
        t0 = time.time()
        # sources of the row plus the Dec margin
        tabs = []
        for path, f_lo, f_hi in frags:
            if f_hi >= lo - margin and f_lo <= hi + margin:
                flt = [('Dec', '>=', math.radians(lo - margin)), ('Dec', '<', math.radians(hi + margin))]
                tabs.append(pq.read_table(path, filters=flt))
        nrow = 0
        if tabs:
            T = pa.concat_tables(tabs)
            data = np.column_stack([T[c].to_numpy() for c in COLNAMES])
            del T, tabs
            ra = np.degrees(data[:, 0])
            n = ntile[irow]
            width = 360.0 / n
            maxdec = min(89.999, max(abs(lo), abs(hi)) + margin)
            mra = min(180.0, margin / math.cos(math.radians(maxdec)))
            # tile index range covered by each source (with the RA margin)
            k_lo = np.floor((ra - mra) / width).astype(np.int64)
            k_hi = np.floor((ra + mra) / width).astype(np.int64)
            span = int((k_hi - k_lo).max()) + 1 if ra.size else 0
            members = [[] for _ in range(n)]
            for s in range(min(span, n)):
                k = k_lo + s
                ok = k <= k_hi
                kk = np.mod(k[ok], n)
                idx = np.nonzero(ok)[0]
                srt = np.argsort(kk, kind='stable')
                kk, idx = kk[srt], idx[srt]
                cuts = np.searchsorted(kk, np.arange(n + 1))
                for t in range(n):
                    if cuts[t + 1] > cuts[t]:
                        members[t].append(idx[cuts[t]:cuts[t + 1]])
            for t in range(n):
                if not members[t]:
                    continue
                sel = np.unique(np.concatenate(members[t]))
                name = os.path.join(tiledir, f'tile_{irow:04d}_{t:04d}.hdf5')
                tmp = name + '.tmp'
                with h5py.File(tmp, 'w') as h:
                    d = h.create_dataset('data', data=np.ascontiguousarray(data[sel].T))
                    d.attrs['MinRA'] = math.radians(t * width)
                    d.attrs['MaxRA'] = math.radians((t + 1) * width)
                    d.attrs['MinDec'] = math.radians(lo)
                    d.attrs['MaxDec'] = math.radians(hi)
                os.replace(tmp, name)
                nrow += sel.size
        open(marker, 'w').close()
        ntot += nrow
        print(f'row {irow} Dec [{lo:+.2f},{hi:+.2f}] {ntile[irow]} tiles: '
              f'{nrow} rows with margin ({time.time() - t0:.1f} s)', flush=True)
    with open(os.path.join(tiledir, 'columns.json'), 'w') as f:
        json.dump({'ColNames': COLNAMES, 'ColUnits': COLUNITS,
                   'RowHeight': row_height, 'TileWidth': tile_width, 'Margin': margin}, f)


def main():
    p = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    p.add_argument('--survey', choices=['wide', 'deep'], required=True)
    p.add_argument('--outdir', required=True)
    p.add_argument('--step', choices=['frag', 'tiles', 'all'], default='all')
    p.add_argument('--workers', type=int, default=8)
    p.add_argument('--row-height', type=float, default=0.5, help='Dec row height [deg]')
    p.add_argument('--tile-width', type=float, default=1.0, help='target RA tile width on the sky [deg]')
    p.add_argument('--margin', type=float, default=0.3,
                   help='margin [deg]; must exceed the HTM cell radius (0.25 deg at level 10)')
    a = p.parse_args()
    outdir = os.path.expanduser(a.outdir)
    if a.step in ('frag', 'all'):
        run_frag(a.survey, outdir, a.workers)
    if a.step in ('tiles', 'all'):
        run_tiles(outdir, a.row_height, a.tile_width, a.margin)


if __name__ == '__main__':
    main()
