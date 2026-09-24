#!/usr/bin/env python3
"""Variance vs mean of the individual pixels of the DESY region, one figure per regime.

Input are the per-pixel dumps written by `desy_ptc_perpixel_extract.m` (one binary
per ladder and quantity plus meta.json):

    <tag>_bright_mean.bin        [Ny Nx Nstep] single, column-major
    <tag>_bright_vartemporal.bin      "        per-pixel temporal variance over the
    <tag>_dark_mean.bin               "        3 repeats of each step (2 dof)
    <tag>_dark_vartemporal.bin        "
    <tag>_zeronoise.bin          [Ny Nx]       per-pixel std over the 5 ZE frames

Each figure shows, for one ladder (light = bright, dark):
  upper panel - the 2-D density of the (mean, variance) pairs of all pixels and all
                steps, the per-step statistics (median/ln2 over pixels, robust for a
                2-dof variance, and the plain mean over pixels), the line fitted to
                the per-step medians inside --fit-range, and the line Var = g*Mean;
  lower panel - the same points as the ratio (Var - RN^2)/(g*Mean), which is flat at
                1 for an ideal Poisson detector with gain g.

Examples
    ./desy_var_mean_plots.py --tag run32_W04_D07
    ./desy_var_mean_plots.py --tag run31_W08_D02 --ladder dark --fit-range 300 2500
    ./desy_var_mean_plots.py --tag run32_W04_D07 --gain 1.07 --out /tmp/figs
"""
import argparse, json, os
import numpy as np
import matplotlib; matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm

LN2 = np.log(2.0)
LADDERS = {'light': ('bright', 'BrightX'), 'dark': ('dark', 'DarkX')}


def load_cube(indir, tag, kind, field, ny, nx, ns):
    a = np.fromfile(os.path.join(indir, '%s_%s_%s.bin' % (tag, kind, field)), dtype=np.float32)
    return a.reshape((ny, nx, ns), order='F').astype(float).reshape(ny*nx, ns)


def wfit(x, y, se):
    """Weighted straight-line fit; returns (slope, intercept), their errors and chi2/dof."""
    w = 1.0/se**2
    A = np.vstack([x, np.ones_like(x)]).T
    cov = np.linalg.inv(A.T @ (A*w[:, None]))
    p = cov @ (A.T @ (w*y))
    chi2 = float(np.sum(w*(y - A@p)**2))
    return p, np.sqrt(np.diag(cov)), chi2, max(len(x)-2, 1)


def one_figure(meta, indir, outdir, ladder, fit_range, gain_override, satlevel, minmean):
    kind, xkey = LADDERS[ladder]
    ny, nx = [int(v) for v in meta['Size']]
    ns = len(np.atleast_1d(meta[xkey]))
    M = load_cube(indir, meta['Tag'], kind, 'mean', ny, nx, ns)
    V = load_cube(indir, meta['Tag'], kind, 'vartemporal', ny, nx, ns)
    rn2 = np.fromfile(os.path.join(indir, meta['Tag']+'_zeronoise.bin'),
                      dtype=np.float32).astype(float).ravel()**2
    npix = M.shape[0]

    step_med = np.median(M, axis=0)
    keep = (step_med > minmean) & (step_med < satlevel)
    M, V = M[:, keep], V[:, keep]
    xs = step_med[keep]

    # per-step statistics over the pixels
    vmed = np.median(V, axis=0)/LN2                  # robust, unbiased for 2 dof
    vmean = V.mean(axis=0)                           # sensitive to particle hits
    mmean = M.mean(axis=0)
    se = vmed/(LN2*np.sqrt(npix))*LN2                # ~1.4 % per step

    sel = (xs >= fit_range[0]) & (xs <= fit_range[1])
    fit = None
    if sel.sum() >= 2:
        fit = wfit(mmean[sel], vmed[sel], se[sel])
    gain = gain_override if gain_override else (fit[0][0] if fit else float(meta['Gain']))

    # pixel cloud
    x = M.ravel(); y = V.ravel()
    good = (x > 0) & (y > 0)
    x, y = x[good], y[good]
    r = (y - np.repeat(rn2, M.shape[1])[good])/(x*gain)

    fig, ax = plt.subplots(2, 1, figsize=(9, 10), sharex=True,
                           gridspec_kw={'height_ratios': [2, 1]})
    hb = ax[0].hexbin(x, y, xscale='log', yscale='log', gridsize=90, mincnt=1,
                      norm=LogNorm(), cmap='Blues')
    plt.colorbar(hb, ax=ax[0], label='pixels per bin')
    ax[0].plot(mmean, vmed, 'o-', color='crimson', ms=5, lw=1.2, label='median over pixels / ln2')
    ax[0].plot(mmean, vmean, 's--', color='darkorange', ms=4, lw=1, label='mean over pixels')
    xx = np.logspace(np.log10(x.min()), np.log10(x.max()), 50)
    ax[0].plot(xx, gain*xx, 'k:', lw=1.2, label='Var = %.3f x Mean' % gain)
    if fit is not None:
        p, e, chi2, dof = fit
        ax[0].plot(xx, p[0]*xx + p[1], 'k-', lw=1.2,
                   label='fit %d-%d ADU: %.4f$\\pm$%.4f, offset %.0f$\\pm$%.0f ($\\chi^2$/dof %.0f/%d)'
                         % (fit_range[0], fit_range[1], p[0], e[0], p[1], e[1], chi2, dof))
    ax[0].set_ylim(max(np.percentile(y, 0.2), vmed.min()/50), vmed.max()*30)
    ax[0].set_ylabel('temporal variance of the pixel [ADU$^2$]')
    ax[0].set_title('%s run %s - %s ladder: variance vs mean, %d pixels x %d steps'
                    % (meta['Die'], meta['Run'], ladder, npix, M.shape[1]))
    ax[0].grid(alpha=.35, which='both'); ax[0].legend(fontsize=8, loc='upper left')

    hb2 = ax[1].hexbin(x, r, xscale='log', gridsize=90, mincnt=1, norm=LogNorm(), cmap='Blues')
    plt.colorbar(hb2, ax=ax[1], label='pixels per bin')
    ax[1].errorbar(mmean, (vmed-np.median(rn2))/(mmean*gain), yerr=se/(mmean*gain),
                   fmt='o-', color='crimson', ms=5, lw=1.2, capsize=2, label='median over pixels')
    ax[1].axhline(1, color='k', lw=.8, ls='--')
    ax[1].set_ylim(0, 2.5)
    ax[1].set_xlabel('mean signal of the pixel [ADU]')
    ax[1].set_ylabel('(Var - RN$^2$) / (%.3f x Mean)' % gain)
    ax[1].grid(alpha=.35, which='both'); ax[1].legend(fontsize=8, loc='upper right')

    fig.tight_layout()
    fn = os.path.join(outdir, '%s_varmean_%s.png' % (meta['Tag'], ladder))
    fig.savefig(fn, dpi=110); plt.close(fig)

    print('%s  %s ladder: %d steps, %d pixels, gain used %.4f' % (meta['Tag'], ladder, M.shape[1], npix, gain))
    if fit is not None:
        p, e, chi2, dof = fit
        print('    fit %d-%d ADU -> slope %.4f +- %.4f, offset %.1f +- %.1f ADU^2, chi2/dof %.0f/%d'
              % (fit_range[0], fit_range[1], p[0], e[0], p[1], e[1], chi2, dof))
    print('    step medians: ' + ', '.join('%.0f:%.3f' % (a, b) for a, b in zip(mmean, vmed/(mmean*gain))))
    print('    written ' + fn)
    return fn


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--indir', default='/home/sasha/claude/ptc_gain_check/perpixel',
                    help='directory with the per-pixel dumps and meta.json')
    ap.add_argument('--out', default=None, help='output directory (default: --indir/..)')
    ap.add_argument('--tag', default='run32_W04_D07', help='die-run tag, or "all"')
    ap.add_argument('--ladder', default='both', choices=['light', 'dark', 'both'])
    ap.add_argument('--fit-range', nargs=2, type=float, default=[300, 2500],
                    metavar=('LO', 'HI'), help='mean-signal window of the straight-line fit [ADU]')
    ap.add_argument('--gain', type=float, default=None, help='force this gain instead of the fitted one')
    ap.add_argument('--satlevel', type=float, default=13000, help='ignore steps above this median [ADU]')
    ap.add_argument('--min-mean', type=float, default=20, help='ignore steps below this median [ADU]')
    a = ap.parse_args()
    outdir = a.out or os.path.dirname(a.indir.rstrip('/'))
    os.makedirs(outdir, exist_ok=True)
    meta = json.load(open(os.path.join(a.indir, 'meta.json')))
    meta = [meta] if isinstance(meta, dict) else meta
    tags = [m for m in meta if a.tag == 'all' or m['Tag'] == a.tag]
    if not tags:
        raise SystemExit('tag %s not found; available: %s' % (a.tag, ', '.join(m['Tag'] for m in meta)))
    for m in tags:
        for lad in (['light', 'dark'] if a.ladder == 'both' else [a.ladder]):
            one_figure(m, a.indir, outdir, lad, a.fit_range, a.gain, a.satlevel, a.min_mean)


if __name__ == '__main__':
    main()
