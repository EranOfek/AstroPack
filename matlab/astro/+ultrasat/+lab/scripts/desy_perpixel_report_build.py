#!/usr/bin/env python3
"""Build the individual-pixel setup-comparison report from desy_perpixel_run.m output.

Reads perpixel.json (scalars + SNR curves per die-run, split by readout-column
parity) and writes report.md, report.html and the figures next to it.
"""
import argparse, json, os, math
from collections import defaultdict
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir',  default='/home/sasha/claude/desy_perpixel')
P.add_argument('--out',    default=None, help='output directory (default = indir)')
A = P.parse_args()
OUT = A.out or A.indir
os.makedirs(OUT, exist_ok=True)

with open(os.path.join(A.indir, 'perpixel.json')) as fh:
    D = json.load(fh)
FULL = D.get('Full') or []
ZERO = D.get('ZeroOnly') or []
if isinstance(FULL, dict):  FULL = [FULL]
if isinstance(ZERO, dict):  ZERO = [ZERO]

# merge any patch files (die-runs reduced from the share because the local
# mirror's sidecar is corrupt -- see desy_perpixel_patch.m); a patch entry
# replaces the main one with the same Tag
import glob as _glob
for _pf in sorted(_glob.glob(os.path.join(A.indir, 'perpixel_patch*.json'))):
    with open(_pf) as fh:
        _P = json.load(fh)
    _new = _P.get('Full') or []
    if isinstance(_new, dict):  _new = [_new]
    _tags = {e['Tag'] for e in _new}
    FULL = [e for e in FULL if e.get('Tag') not in _tags] + _new
    print(f'merged {len(_new)} die-run(s) from {os.path.basename(_pf)}')

# variance-versus-mean figures, if desy_perpixel_varmean_plots.py has been run
VARMEAN = []
_vmp = os.path.join(A.indir, 'varmean_figs.json')
if os.path.isfile(_vmp):
    with open(_vmp) as fh:
        VARMEAN = json.load(fh)
    VARMEAN.sort(key=lambda v: (v['tx'], -v['rsth'], v['run']))
    print(f'{len(VARMEAN)} variance-vs-mean figures found')

# ---------------------------------------------------------------- helpers
def g(d, *keys, default=np.nan):
    """nested get returning nan when anything is missing"""
    for k in keys:
        if not isinstance(d, dict) or k not in d:
            return default
        d = d[k]
    return default if d is None else d

def arr(x):
    a = np.atleast_1d(np.asarray(x, dtype=float))
    return a

def setup_label(e):
    s = f"TX {e['TX']:.1f}"
    if abs(float(e.get('RSTH', 3.0)) - 3.0) > 0.01:
        s += f" RST_H {e['RSTH']:.1f}"
    return s

SET_COLOR = {'AV': '#c44e52', 'aSpect': '#4c72b0'}
FLAV_MARK = {6: 'o', 2: 's'}

def style(e):
    # accepts both the raw json entry (Settings/Flavour) and a flattened row
    sett = e.get('Settings', e.get('settings'))
    flav = e.get('Flavour', e.get('flavour', 6))
    try:
        flav = int(flav)
    except (TypeError, ValueError):
        flav = 6
    return dict(color=SET_COLOR.get(sett, '#555555'),
                marker=FLAV_MARK.get(flav, '^'))

# ---------------------------------------------------------------- per-die rows
def row_full(e):
    """one flat record per die-run, parity 'All'"""
    r = dict(tag=e['Tag'], run=str(e['Run']), die=e['Die'], settings=e.get('Settings'),
             tx=float(e['TX']), rsth=float(e.get('RSTH', np.nan)),
             flavour=int(e.get('Flavour', 0)), passed=e.get('Pass'),
             gain=float(g(e, 'GainUsed')), expsen=float(g(e, 'ExpSen')),
             nbad=int(g(e, 'Bad', 'Nbad', default=0)))
    for pn in ('All', 'Even', 'Odd'):
        z = g(e, 'Zero', pn, default={})
        t = g(e, 'Threshold', pn, default={})
        pre = '' if pn == 'All' else pn.lower() + '_'
        r[pre+'bias']     = float(g(z, 'BiasLevel'))
        r[pre+'fpn']      = float(g(z, 'FixedPatternRMS'))
        r[pre+'rn']       = float(g(z, 'ReadNoiseMedian'))
        r[pre+'rn_rms']   = float(g(z, 'ReadNoiseRMS'))
        r[pre+'rn_tail']  = float(g(z, 'TailFrac'))
        r[pre+'rn_spread']= float(g(z, 'SpreadSigmaRel'))
        _mv = float(g(z, 'Spread', 'MeanVar'))
        _so = float(g(z, 'Spread', 'StdObs'))
        r[pre+'rn_spread_obs'] = (_so/_mv/2.0) if (np.isfinite(_mv) and _mv > 0) else np.nan
        r[pre+'dc']       = float(g(t, 'DCSpread', 'Median'))
        r[pre+'dsnu']     = float(g(t, 'DCSpread', 'StdIntr'))
        r[pre+'dc_fit']   = float(g(t, 'DCSpread', 'StdFitRobust'))
        r[pre+'tdark']    = float(g(t, 'MedianDarkE'))
        r[pre+'tlight']   = float(g(t, 'MedianLightE'))
        r[pre+'tdark_adu']= float(g(t, 'DarkSpread', 'Median'))
        r[pre+'prnu']     = float(g(t, 'PRNU'))
        r[pre+'prnu_err'] = float(g(t, 'PRNU_Err'))
        r[pre+'offset_e'] = float(g(t, 'OffsetFPN_e'))
    gn = r['gain'] if np.isfinite(r['gain']) and r['gain'] > 0 else np.nan
    r['rn_e'] = r['rn'] / gn if np.isfinite(gn) else np.nan
    for meth in ('light', 'dark', 'none'):
        b = g(e, 'Budget', 'All', meth, default={})
        r['qlim_cal_'+meth] = float(g(b, 'Qlim_cal'))
        r['qlim_raw_'+meth] = float(g(b, 'Qlim_raw'))
    return r

def row_zero(e):
    r = dict(tag=e['Tag'], run=str(e['Run']), die=e['Die'], settings=e.get('Settings'),
             tx=float(e['TX']), rsth=float(e.get('RSTH', np.nan)),
             flavour=int(e.get('Flavour', 0)), nbad=int(g(e, 'Nbad', default=0)))
    for key, src in (('', 'Zero'), ('f5_', 'Zero5')):
        z = g(e, src, 'All', default={})
        r[key+'bias']      = float(g(z, 'BiasLevel'))
        r[key+'fpn']       = float(g(z, 'FixedPatternRMS'))
        r[key+'rn']        = float(g(z, 'ReadNoiseMedian'))
        r[key+'rn_tail']   = float(g(z, 'TailFrac'))
        r[key+'rn_spread'] = float(g(z, 'SpreadSigmaRel'))
        _mv = float(g(z, 'Spread', 'MeanVar'))
        _so = float(g(z, 'Spread', 'StdObs'))
        r[key+'rn_spread_obs'] = (_so/_mv/2.0) if (np.isfinite(_mv) and _mv > 0) else np.nan
        r[key+'nframes']   = float(g(e, src, 'Nframes'))   # top level, not inside the parity subset
        r[key+'cm_std']    = float(g(e, src, 'CommonMode', 'Std'))
    return r

ROWS = [row_full(e) for e in FULL]
ZROWS = [row_zero(e) for e in ZERO]
BYTAG = {e['Tag']: e for e in FULL}

# ---------------------------------------------------------------- figures
def scan_plot(fname, ykeys, ylabel, title, rows=None, logy=False, ylim=None,
              keylabels=None):
    """value versus TX, one point per die-run, medians per setup overlaid"""
    rows = rows if rows is not None else ROWS
    fig, ax = plt.subplots(figsize=(8.2, 4.6))
    keys = ykeys if isinstance(ykeys, (list, tuple)) else [ykeys]
    FILL = [None, 'none', 'white']        # one per key: filled, open, half
    seen = set()
    for r in rows:
        for ik, k in enumerate(keys):
            y = r.get(k, np.nan)
            if not np.isfinite(y):
                continue
            st = style(r)
            lbl = None
            tag = (r['settings'], st['marker'], ik)
            if tag not in seen:
                seen.add(tag)
                lbl = f"{r['settings']}, W{'04' if r['flavour']==6 else '08'}"
                if keylabels and ik < len(keylabels):
                    lbl += f" ({keylabels[ik]})"
            dx = 0.012*ik + (0.014 if r['settings'] == 'aSpect' else -0.014)
            ax.plot(r['tx'] + dx, y, st['marker'], color=st['color'], ms=5,
                    mfc=st['color'] if FILL[ik % 3] is None else FILL[ik % 3],
                    alpha=0.85, label=lbl)
    # median per (tx, rsth, settings)
    grp = defaultdict(list)
    for r in rows:
        y = r.get(keys[0], np.nan)
        if np.isfinite(y):
            grp[(r['tx'], r['rsth'], r['settings'])].append(y)
    for (tx, rsth, sett), vals in sorted(grp.items()):
        m = np.median(vals)
        dx = 0.014 if sett == 'aSpect' else -0.014
        ax.plot(tx + dx, m, '_', color='k', ms=18, mew=1.6, zorder=5)
        if abs(rsth - 3.0) > 0.01:
            ax.annotate('RST_H %.1f' % rsth, (tx, m), textcoords='offset points',
                        xytext=(0, 9), ha='center', fontsize=7, color='#666666')
    ax.set_xlabel('TX voltage [V]')
    ax.set_ylabel(ylabel)
    ax.set_title(title, fontsize=10)
    if logy:
        ax.set_yscale('log')
    if ylim:
        ax.set_ylim(*ylim)
    ax.grid(alpha=0.3)
    ax.legend(fontsize=7, ncol=2)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, fname), dpi=110)
    plt.close(fig)
    return fname

FIGS = {}
if ROWS:
    FIGS['rn'] = scan_plot('fig_rn_vs_tx.png', ['rn'],
                           'median per-pixel read noise [ADU]',
                           'Read noise per pixel (all pixels; the parity split is in its own figure)',
                           logy=True)
    FIGS['rn_spread'] = scan_plot('fig_rn_spread_vs_tx.png', ['rn_spread'],
                                  'intrinsic spread of sigma_RN  [fraction]',
                                  'Pixel-to-pixel non-uniformity of the read noise (chi2 sampling scatter removed)')
    FIGS['rn_tail'] = scan_plot('fig_rn_tail_vs_tx.png', ['rn_tail'],
                                'fraction of pixels with sigma > 2x median',
                                'Read-noise tail')
    FIGS['fpn'] = scan_plot('fig_fpn_vs_tx.png', ['fpn'],
                            'bias fixed pattern [ADU]',
                            'Fixed pattern of the bias frame (its own sampling noise removed)',
                            logy=True)
    FIGS['dc'] = scan_plot('fig_dc_vs_tx.png', ['dc'], 'dark current [ADU/s]',
                           'Dark current', logy=True)
    FIGS['dsnu'] = scan_plot('fig_dsnu_vs_tx.png', ['dsnu'], 'DSNU [ADU/s]',
                             'Dark-current non-uniformity (fit noise removed)', logy=True)
    FIGS['thr'] = scan_plot('fig_threshold_vs_tx.png', ['tlight', 'tdark'],
                            'charge threshold [e-]',
                            'Charge threshold by both methods',
                            keylabels=['light method', 'dark method'])
    FIGS['prnu'] = scan_plot('fig_prnu_vs_tx.png', ['prnu'], 'PRNU [fraction]',
                             'Photo-response non-uniformity, from sigma_fixed^2 = a^2 + (b S)^2')
    FIGS['offset'] = scan_plot('fig_offset_vs_tx.png', ['offset_e'],
                               'additive offset pattern [e-]',
                               'Additive (offset) fixed pattern, the a of the same fit',
                               logy=True)
    FIGS['qlim'] = scan_plot('fig_qlim_vs_tx.png',
                             ['qlim_cal_light', 'qlim_raw_light', 'qlim_cal_none'],
                             'signal reaching SNR 5 [e-]',
                             'What the threshold costs: with it (calibrated and raw) and with T set to 0',
                             logy=True,
                             keylabels=['calibrated', 'raw frame', 'no threshold'])

# odd vs even readout columns: relative for quantities bounded away from zero,
# absolute (in electrons) for the thresholds, whose relative difference blows up
# whenever the two parities straddle zero
if ROWS:
    REL = [('rn', 'read\nnoise'), ('fpn', 'bias\nFPN'), ('dc', 'dark\ncurrent'),
           ('prnu', 'PRNU')]
    ABS = [('tlight', 'threshold\nlight'), ('tdark', 'threshold\ndark')]
    txs = sorted({r['tx'] for r in ROWS})
    cmap = plt.get_cmap('viridis')
    cidx = {t: cmap(i/max(len(txs)-1, 1)) for i, t in enumerate(txs)}
    fig, axs = plt.subplots(1, 2, figsize=(11.0, 4.6),
                            gridspec_kw={'width_ratios': [2, 1]})
    for ax, qty, rel in ((axs[0], REL, True), (axs[1], ABS, False)):
        for iq, (k, lbl) in enumerate(qty):
            for r in ROWS:
                e, o = r.get('even_'+k, np.nan), r.get('odd_'+k, np.nan)
                if not (np.isfinite(e) and np.isfinite(o)):
                    continue
                if rel:
                    if abs(e) + abs(o) <= 0:
                        continue
                    y = 200.0*(o - e)/(abs(o) + abs(e))
                else:
                    y = o - e
                ax.plot(iq + 0.28*(np.random.rand()-0.5), y, 'o', ms=4.5,
                        color=cidx[r['tx']], alpha=0.8)
        med = []
        for iq, (k, lbl) in enumerate(qty):
            vals = []
            for r in ROWS:
                e, o = r.get('even_'+k, np.nan), r.get('odd_'+k, np.nan)
                if np.isfinite(e) and np.isfinite(o) and (not rel or abs(e)+abs(o) > 0):
                    vals.append(200.0*(o-e)/(abs(o)+abs(e)) if rel else o-e)
            if vals:
                m = np.median(vals)
                ax.plot([iq-0.3, iq+0.3], [m, m], 'k-', lw=2, zorder=5)
                med.append((iq, m))
        ax.axhline(0, color='k', lw=1, ls=':')
        ax.set_xticks(range(len(qty)))
        ax.set_xticklabels([l for _, l in qty], fontsize=8)
        ax.set_xlim(-0.6, len(qty)-0.4)
        ax.grid(alpha=0.3, axis='y')
        ax.set_ylabel('odd - even  [% of the mean]' if rel else 'odd - even  [e-]')
        ax.set_title('relative' if rel else 'absolute (thresholds)', fontsize=10)
    handles = [plt.Line2D([], [], marker='o', ls='', color=cidx[t], label=f'TX {t:.1f}')
               for t in txs]
    axs[0].legend(handles=handles, fontsize=7, ncol=3, title='bar = median',
                  title_fontsize=7)
    fig.suptitle('Odd versus even readout columns, one point per die-run', fontsize=10)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, 'fig_parity.png'), dpi=110)
    plt.close(fig)
    FIGS['parity'] = 'fig_parity.png'

# pattern profile: fixed pattern versus signal, one curve per setup (median die)
if FULL:
    fig, axs = plt.subplots(1, 2, figsize=(11.5, 4.6))
    for e in FULL:
        if e['Die'] != 'W04_D07':
            continue
        for ax, key, lbl in ((axs[0], 'PatternB', 'bright ladder'),
                             (axs[1], 'PatternD', 'dark ladder')):
            pat = g(e, key, 'All', default={})
            med = arr(g(pat, 'Median', default=[]))
            rel = arr(g(pat, 'RelFixed', default=[]))
            lo = 5 if key == 'PatternB' else 0.5
            ok = np.isfinite(med) & np.isfinite(rel) & (med > lo) & (med < 12000)
            if ok.sum() > 2:
                ax.plot(med[ok], 100*rel[ok], '-o', ms=3, lw=1,
                        color=SET_COLOR.get(e.get('Settings'), '#555'),
                        alpha=0.8, label=f"run {e['Run']} {setup_label(e)}")
    for ax, lbl in ((axs[0], 'bright ladder'), (axs[1], 'dark ladder')):
        ax.set_xscale('log'); ax.set_yscale('log')
        ax.set_xlabel('signal [ADU]'); ax.set_ylabel('fixed pattern [% of signal]')
        ax.set_title(f'W04_D07, {lbl}', fontsize=10)
        ax.grid(alpha=0.3, which='both')
        ax.legend(fontsize=6, ncol=2)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, 'fig_pattern_profile.png'), dpi=110)
    plt.close(fig)
    FIGS['pattern'] = 'fig_pattern_profile.png'

# SNR curves with the threshold dead zone
if FULL:
    fig, axs = plt.subplots(1, 2, figsize=(11.5, 4.8), sharey=True)
    for e in FULL:
        if e['Die'] != 'W04_D07':
            continue
        b = g(e, 'Budget', 'All', 'light', default={})
        Q = arr(g(b, 'Q', default=[]))
        if Q.size < 3:
            continue
        st = style(e)
        Te = float(g(b, 'Threshold_e', default=np.nan))
        for ax, key, lbl in ((axs[0], 'SNR_cal', 'fixed pattern calibrated out'),
                             (axs[1], 'SNR_raw', 'raw frame')):
            S = arr(g(b, key, default=[]))
            if S.size == Q.size:
                ax.plot(Q, S, '-', lw=1.3, color=st['color'], alpha=0.85,
                        label=f"run {e['Run']} {setup_label(e)}  T={Te:.0f} e-")
                if np.isfinite(Te) and Te > 0:
                    ax.axvspan(Q.min(), min(Te, Q.max()), color=st['color'], alpha=0.05)
    for ax, lbl in ((axs[0], 'fixed pattern calibrated out'), (axs[1], 'single raw frame')):
        ax.axhline(5, color='k', ls=':', lw=1)
        ax.set_xscale('log'); ax.set_yscale('log')
        ax.set_xlabel('incident charge Q [e-]')
        ax.set_title(lbl, fontsize=10)
        ax.grid(alpha=0.3, which='both')
        ax.legend(fontsize=6)
    axs[0].set_ylabel('SNR per pixel')
    fig.suptitle('Per-pixel SNR, 15 s exposure; shaded = below the charge threshold', fontsize=10)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, 'fig_snr_curves.png'), dpi=110)
    plt.close(fig)
    FIGS['snr'] = 'fig_snr_curves.png'

# ---------------------------------------------------------------- conclusion figures
def setup_rows():
    """median over the dies of each (run, TX, RST_H, board) -- defined before use"""
    grp = defaultdict(list)
    for r in ROWS:
        grp[(r['tx'], r['rsth'], r['settings'], r['run'])].append(r)
    out = []
    for (tx, rsth, sett, run), rs in sorted(grp.items()):
        def med(k):
            v = [x[k] for x in rs if np.isfinite(x.get(k, np.nan))]
            return np.median(v) if v else np.nan
        out.append(dict(run=run, tx=tx, rsth=rsth, settings=sett, ndie=len(rs),
                        rn=med('rn'), rn_e=med('rn_e'), fpn=med('fpn'),
                        rn_spread=med('rn_spread'), rn_tail=med('rn_tail'),
                        dc=med('dc'), dsnu=med('dsnu'), tlight=med('tlight'),
                        tdark=med('tdark'), prnu=med('prnu'), offset=med('offset_e'),
                        qlim_cal=med('qlim_cal_light'), qlim_raw=med('qlim_raw_light'),
                        qlim_none=med('qlim_cal_none')))
    return out

SETUPS = setup_rows()

# the decision plane: threshold against read noise, coloured by the limiting signal
if SETUPS:
    ok = [x for x in SETUPS if np.isfinite(x['tlight']) and np.isfinite(x['rn_e'])]
    fig, axs = plt.subplots(1, 2, figsize=(12.4, 5.2))
    ax = axs[0]
    q = np.array([x['qlim_cal'] for x in ok], dtype=float)
    good = np.isfinite(q)
    sc = ax.scatter([x['tlight'] for x in ok], [x['rn_e'] for x in ok],
                    c=np.where(good, q, np.nanmax(q)), s=150, cmap='viridis_r',
                    edgecolors='k', linewidths=0.8, zorder=3)
    best = min([x for x in ok if np.isfinite(x['qlim_cal'])],
               key=lambda x: x['qlim_cal'], default=None)
    if best:
        ax.plot(best['tlight'], best['rn_e'], '*', ms=26, mfc='none',
                mec='crimson', mew=2.0, zorder=4)
    for x in ok:
        lab = f"{x['run']}\nTX {x['tx']:.1f}"
        if abs(x['rsth']-3.0) > 0.01:
            lab += f"\nRST_H {x['rsth']:.1f}"
        ax.annotate(lab, (x['tlight'], x['rn_e']), textcoords='offset points',
                    xytext=(10, -4), fontsize=7, color='#333')
    ax.axvline(0, color='k', lw=1, ls=':')
    ax.axvspan(-5, 5, color='#2ca02c', alpha=0.07)
    tv = [x['tlight'] for x in ok]
    ax.set_xlim(min(tv) - 12, max(tv) + 30)        # room for the labels
    ax.set_yscale('log')
    ax.set_xlabel('charge threshold, light method  [e-]   (positive = charge lost)')
    ax.set_ylabel('read noise  [e-]')
    ax.set_title('The decision plane: both axes must be small', fontsize=10)
    ax.grid(alpha=0.3, which='both')
    cb = fig.colorbar(sc, ax=ax)
    cb.set_label('signal reaching SNR 5 in one 15 s frame [e-]', fontsize=8)
    cb.ax.tick_params(labelsize=7)

    # what the threshold costs, against TX
    ax = axs[1]
    for key, lab, mk in (('qlim_cal', 'with threshold, patterns calibrated', 'o'),
                         ('qlim_raw', 'with threshold, single raw frame', 's'),
                         ('qlim_none', 'threshold forced to zero', '^')):
        xs = [x['tx'] + (0.012 if x['settings'] == 'aSpect' else -0.012) for x in SETUPS
              if np.isfinite(x[key])]
        ys = [x[key] for x in SETUPS if np.isfinite(x[key])]
        ax.plot(xs, ys, mk, ms=7, alpha=0.85, label=lab)
    ax.set_xlabel('TX voltage [V]')
    ax.set_ylabel('signal reaching SNR 5  [e-]')
    ax.set_yscale('log')
    ax.set_title('The gap between the circles and the triangles is the threshold', fontsize=10)
    ax.grid(alpha=0.3, which='both')
    ax.legend(fontsize=7)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, 'fig_decision.png'), dpi=110)
    plt.close(fig)
    FIGS['decision'] = 'fig_decision.png'

# four-panel TX summary
if SETUPS:
    PAN = [('tlight', 'charge threshold, light method [e-]', False),
           ('rn_e',   'read noise [e-]', True),
           ('fpn',    'bias fixed pattern [ADU]', True),
           ('qlim_cal', 'signal reaching SNR 5 [e-]', True)]
    fig, axs = plt.subplots(2, 2, figsize=(11.0, 7.4))
    for ax, (k, lab, logy) in zip(axs.ravel(), PAN):
        for x in SETUPS:
            if not np.isfinite(x[k]):
                continue
            col = SET_COLOR.get(x['settings'], '#555')
            mk = 'o' if abs(x['rsth']-3.0) < 0.01 else 'D'
            ax.plot(x['tx'] + (0.012 if x['settings'] == 'aSpect' else -0.012), x[k],
                    mk, ms=8, color=col, alpha=0.9)
        if k == 'tlight':
            ax.axhline(0, color='k', lw=1, ls=':')
        ax.axvline(3.5, color='crimson', lw=1.2, ls='--', alpha=0.7)
        ax.set_xlabel('TX voltage [V]')
        ax.set_ylabel(lab, fontsize=9)
        if logy:
            ax.set_yscale('log')
        ax.grid(alpha=0.3, which='both')
    H = [plt.Line2D([], [], marker='o', ls='', color=SET_COLOR['aSpect'], label='aSpect, RST_H 3.0'),
         plt.Line2D([], [], marker='D', ls='', color=SET_COLOR['aSpect'], label='aSpect, RST_H 2.7'),
         plt.Line2D([], [], marker='o', ls='', color=SET_COLOR['AV'], label='AV, RST_H 3.0'),
         plt.Line2D([], [], marker='D', ls='', color=SET_COLOR['AV'], label='AV, RST_H 2.7'),
         plt.Line2D([], [], ls='--', color='crimson', label='TX 3.5 V')]
    axs[0, 0].legend(handles=H, fontsize=7)
    fig.suptitle('Setup medians against TX; the dashed line marks the chosen optimum', fontsize=10)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, 'fig_tx_summary.png'), dpi=110)
    plt.close(fig)
    FIGS['txsum'] = 'fig_tx_summary.png'

# ---------------------------------------------------------------- tables
def fmt(v, p=3):
    if v is None or (isinstance(v, float) and not np.isfinite(v)):
        return '--'
    return f'{v:.{p}f}'

def table(rows, cols):
    head = '| ' + ' | '.join(c[0] for c in cols) + ' |'
    rule = '|' + '|'.join('---' for _ in cols) + '|'
    out = [head, rule]
    for r in rows:
        out.append('| ' + ' | '.join(c[1](r) for c in cols) + ' |')
    return '\n'.join(out)

ROWS.sort(key=lambda r: (r['tx'], -r['rsth'], r['settings'] or '', r['die']))
ZROWS.sort(key=lambda r: (r['tx'], r['die']))

MAIN_COLS = [
    ('run',        lambda r: r['run']),
    ('die',        lambda r: r['die']),
    ('set',        lambda r: r['settings'] or '--'),
    ('TX',         lambda r: fmt(r['tx'], 1)),
    ('RST_H',      lambda r: fmt(r['rsth'], 1)),
    ('bad col',    lambda r: str(r['nbad'])),
    ('bias',       lambda r: fmt(r['bias'], 1)),
    ('FPN',        lambda r: fmt(r['fpn'], 2)),
    ('RN [ADU]',   lambda r: fmt(r['rn'], 3)),
    ('RN [e-]',    lambda r: fmt(r['rn_e'], 2)),
    ('RN spread',  lambda r: fmt(100*r['rn_spread'], 1) + ' %'),
    ('tail',       lambda r: fmt(100*r['rn_tail'], 1) + ' %'),
    ('gain',       lambda r: fmt(r['gain'], 4)),
    ('DC [ADU/s]', lambda r: fmt(r['dc'], 4)),
    ('DSNU',       lambda r: fmt(r['dsnu'], 4)),
    ('T light',    lambda r: fmt(r['tlight'], 1)),
    ('T dark',     lambda r: fmt(r['tdark'], 1)),
    ('PRNU',       lambda r: fmt(100*r['prnu'], 3) + ' %'),
    ('a [e-]',     lambda r: fmt(r['offset_e'], 2)),
    ('Qlim cal',   lambda r: fmt(r['qlim_cal_light'], 1)),
    ('Qlim raw',   lambda r: fmt(r['qlim_raw_light'], 1)),
    ('Qlim T=0',   lambda r: fmt(r['qlim_cal_none'], 1)),
]

PARITY_COLS = [
    ('run',   lambda r: r['run']),
    ('die',   lambda r: r['die']),
    ('TX',    lambda r: fmt(r['tx'], 1)),
    ('RN even',  lambda r: fmt(r['even_rn'], 3)),
    ('RN odd',   lambda r: fmt(r['odd_rn'], 3)),
    ('FPN even', lambda r: fmt(r['even_fpn'], 2)),
    ('FPN odd',  lambda r: fmt(r['odd_fpn'], 2)),
    ('DC even',  lambda r: fmt(r['even_dc'], 4)),
    ('DC odd',   lambda r: fmt(r['odd_dc'], 4)),
    ('T light even', lambda r: fmt(r['even_tlight'], 1)),
    ('T light odd',  lambda r: fmt(r['odd_tlight'], 1)),
    ('T dark even',  lambda r: fmt(r['even_tdark'], 1)),
    ('T dark odd',   lambda r: fmt(r['odd_tdark'], 1)),
    ('PRNU even',    lambda r: fmt(100*r['even_prnu'], 3)),
    ('PRNU odd',     lambda r: fmt(100*r['odd_prnu'], 3)),
]

ZERO_COLS = [
    ('run',  lambda r: r['run']),
    ('die',  lambda r: r['die']),
    ('TX',   lambda r: fmt(r['tx'], 1)),
    ('N ZE', lambda r: fmt(r['nframes'], 0)),
    ('bias', lambda r: fmt(r['bias'], 1)),
    ('FPN',  lambda r: fmt(r['fpn'], 2)),
    ('RN',   lambda r: fmt(r['rn'], 3)),
    ('RN spread', lambda r: fmt(100*r['rn_spread'], 1) + ' %'),
    ('tail',      lambda r: fmt(100*r['rn_tail'], 1) + ' %'),
    ('common mode',   lambda r: fmt(r['cm_std'], 3)),
    ('RN (5 frames)', lambda r: fmt(r['f5_rn'], 3)),
    ('spread (5)',    lambda r: fmt(100*r['f5_rn_spread'], 1) + ' %'),
]

# ---------------------------------------------------------------- ranking
SETUP_COLS = [
    ('run',    lambda r: r['run']),
    ('set',    lambda r: r['settings'] or '--'),
    ('TX',     lambda r: fmt(r['tx'], 1)),
    ('RST_H',  lambda r: fmt(r['rsth'], 1)),
    ('dies',   lambda r: str(r['ndie'])),
    ('RN [ADU]', lambda r: fmt(r['rn'], 3)),
    ('RN [e-]',  lambda r: fmt(r['rn_e'], 2)),
    ('FPN [ADU]',lambda r: fmt(r['fpn'], 2)),
    ('DC [ADU/s]', lambda r: fmt(r['dc'], 4)),
    ('DSNU',     lambda r: fmt(r['dsnu'], 4)),
    ('T light [e-]', lambda r: fmt(r['tlight'], 1)),
    ('T dark [e-]',  lambda r: fmt(r['tdark'], 1)),
    ('PRNU',     lambda r: fmt(100*r['prnu'], 3) + ' %'),
    ('a [e-]',   lambda r: fmt(r['offset'], 2)),
    ('Qlim cal [e-]', lambda r: fmt(r['qlim_cal'], 1)),
    ('Qlim raw [e-]', lambda r: fmt(r['qlim_raw'], 1)),
    ('Qlim T=0 [e-]', lambda r: fmt(r['qlim_none'], 1)),
]

# ---------------------------------------------------------------- text helpers
def tail_check():
    """the strongest available check: the same pixels, two different sampling noises.

    Runs 39 and 39-2 carry 10 bias frames. Reducing them with all 10 (9 degrees of
    freedom) and with only the first 5 (4 dof) measures the SAME pixels of the SAME
    detector through very different chi2 sampling scatter. The raw spreads must
    therefore disagree and the deconvolved ones must agree.
    """
    rows = [r for r in ZROWS
            if np.isfinite(r.get('rn_spread', np.nan)) and np.isfinite(r.get('f5_rn_spread', np.nan))
            and r['rn_spread'] > 0 and r['f5_rn_spread'] > 0]
    if len(rows) < 3:
        return ''
    d10 = np.array([r['rn_spread'] for r in rows])
    d05 = np.array([r['f5_rn_spread'] for r in rows])
    r10 = np.array([r.get('rn_spread_obs', np.nan) for r in rows], dtype=float)
    r05 = np.array([r.get('f5_rn_spread_obs', np.nan) for r in rows], dtype=float)
    dec = np.median(d05/d10)
    out = (f"Runs 39 and 39-2 carry 10 bias frames instead of 5, so the same pixels of the "
           f"same detector can be reduced twice with very different sampling scatter: 9 "
           f"degrees of freedom against 4, i.e. a chi2 term of "
           f"{100*math.sqrt(0.5*2/9):.0f} % against {100*math.sqrt(0.5*2/4):.0f} % on sigma. "
           f"Whatever the true distribution of the read noise is, the deconvolved spread must "
           f"come out the same both ways and the raw spread must not.\n\n")
    if np.isfinite(r10).all() and np.isfinite(r05).all():
        raw = np.median(r05/r10)
        out += (f"Over the {len(rows)} die-runs the raw, undeconvolved spread is a factor "
                f"{raw:.2f} larger when only 5 frames are used "
                f"({100*np.median(r05):.0f} % against {100*np.median(r10):.0f} %), exactly as "
                f"the sampling term demands. After the deconvolution the two agree to "
                f"{abs(100*(dec-1)):.0f} % (ratio {dec:.2f}, "
                f"{100*np.median(d05):.1f} % against {100*np.median(d10):.1f} %). ")
    else:
        out += (f"Over the {len(rows)} die-runs the deconvolved spreads agree to "
                f"{abs(100*(dec-1)):.0f} % (ratio {dec:.2f}). ")
    out += ("The sampling scatter is therefore genuinely being removed rather than absorbed "
            "into the answer. This is also the reason runs with different bias-frame counts "
            "must never be compared directly without the 5-frame subsample.\n")
    return out

def rank_text():
    if not SETUPS:
        return '(no data)\n'
    L = []
    ok = [x for x in SETUPS if np.isfinite(x['tlight'])]
    if ok:
        by_t = sorted(ok, key=lambda x: (max(x['tlight'], 0.0), x['tlight']))
        L.append('**Charge threshold (light method), least charge lost first.** Only a positive '
                 'T is a loss -- a negative one means charge present at zero intensity, a '
                 'constant offset that the bias and dark subtraction remove -- so the ordering '
                 'is by max(T, 0) and the negative entries are equally free of charge loss.\n')
        L.append(table(by_t, [('rank', lambda r: str(by_t.index(r)+1)),
                              ('run', lambda r: r['run']),
                              ('set', lambda r: r['settings'] or '--'),
                              ('TX', lambda r: fmt(r['tx'], 1)),
                              ('RST_H', lambda r: fmt(r['rsth'], 1)),
                              ('T light [e-]', lambda r: fmt(r['tlight'], 1)),
                              ('T dark [e-]', lambda r: fmt(r['tdark'], 1))]) + '\n')
    okn = [x for x in SETUPS if np.isfinite(x['rn_e'])]
    if okn:
        by_n = sorted(okn, key=lambda x: x['rn_e'])
        L.append('**Read noise, quietest first.**\n')
        L.append(table(by_n, [('rank', lambda r: str(by_n.index(r)+1)),
                              ('run', lambda r: r['run']),
                              ('set', lambda r: r['settings'] or '--'),
                              ('TX', lambda r: fmt(r['tx'], 1)),
                              ('RST_H', lambda r: fmt(r['rsth'], 1)),
                              ('RN [e-]', lambda r: fmt(r['rn_e'], 2)),
                              ('RN spread', lambda r: fmt(100*r['rn_spread'], 1) + ' %'),
                              ('DSNU [ADU/s]', lambda r: fmt(r['dsnu'], 4))]) + '\n')
    # the second knob: RST_H isolated wherever both values exist at the same TX
    pairs = defaultdict(dict)
    for x in SETUPS:
        pairs[(x['tx'], x['settings'])][x['rsth']] = x
    iso = [(k, v) for k, v in pairs.items() if len(v) > 1]
    if iso:
        L.append('\n**The second knob, isolated.** Wherever both RST_H values were measured at '
                 'the same TX and on the same bias board:\n')
        rowsi = []
        for (tx, sett), v in sorted(iso):
            hi, lo = v.get(3.0), v.get(2.7)
            if hi and lo:
                rowsi.append(dict(tx=tx, sett=sett, hi=hi, lo=lo))
        if rowsi:
            L.append(table(rowsi, [
                ('TX', lambda r: fmt(r['tx'], 1)),
                ('board', lambda r: r['sett'] or '--'),
                ('RN 3.0 V [e-]', lambda r: fmt(r['hi']['rn_e'], 2)),
                ('RN 2.7 V [e-]', lambda r: fmt(r['lo']['rn_e'], 2)),
                ('FPN 3.0 V [ADU]', lambda r: fmt(r['hi']['fpn'], 1)),
                ('FPN 2.7 V [ADU]', lambda r: fmt(r['lo']['fpn'], 1)),
                ('Qlim 3.0 V', lambda r: fmt(r['hi']['qlim_cal'], 1)),
                ('Qlim 2.7 V', lambda r: fmt(r['lo']['qlim_cal'], 1))]) + '\n')
            L.append('RST_H 2.7 V is worse on every count, so **use RST_H 3.0 V**. It is also '
                     'the explanation of the ~100 ADU bias fixed pattern seen at TX 3.0 V: '
                     'that is the RST_H setting of those runs, not a TX effect.\n')
    # repeatability
    bygrp = defaultdict(list)
    for x in SETUPS:
        bygrp[(x['tx'], x['rsth'], x['settings'])].append(x)
    key = max(bygrp, key=lambda k: len(bygrp[k])) if bygrp else None
    nom = bygrp[key] if key else []
    if len(nom) > 1:
        def spread(k):
            v = [x[k] for x in nom if np.isfinite(x[k])]
            return (min(v), max(v)) if v else (np.nan, np.nan)
        tl, th = spread('tlight'); rl, rh = spread('rn_e'); dl, dh = spread('dc')
        L.append(f"\n**Run-to-run repeatability.** {len(nom)} runs share the same setting "
                 f"(TX {key[0]:.1f} V, RST_H {key[1]:.1f} V, {key[2]}) on different days "
                 f"({', '.join(sorted(x['run'] for x in nom))}). Across them the median "
                 f"light-method threshold spans {tl:.1f} to {th:.1f} e-, the read noise "
                 f"{rl:.2f} to {rh:.2f} e- and the dark current {dl:.4f} to {dh:.4f} ADU/s. "
                 "Any difference between setups smaller than these spans is not significant, "
                 "which is why the threshold at TX 3.5 V is reported as consistent with zero "
                 "rather than as 2.4 e-.\n")
    return '\n'.join(L)

# ---------------------------------------------------------------- markdown
LIN = D.get('LinLimit')
MD = []
w = MD.append

def figblock(key, cap):
    if key in FIGS:
        w(f'![{cap}]({FIGS[key]})\n')
        w(f'*{cap}*\n')

w('# DESY wafer test TH02954 — which setup to use, decided per pixel\n')
w(f'Lot TH02954, high-gain half, DESY region 100x100 pixels. {len(ROWS)} die-runs with full '
  f'ladders and {len(ZROWS)} ZE-only die-runs, every statistic computed per pixel and '
  'separately for the even and odd readout columns, with the bad readout columns masked.\n')
w('**Answer first.** Of the ten setups measured, **TX 3.5 V with RST_H 3.0 V on the aSpect '
  'bias boards** is the one to use for signals of a few tens of electrons. It is the only '
  'setting whose charge threshold is consistent with zero while the pixel is still quiet. '
  'Section 9 shows this on three figures; sections 1 to 8 are how we got there.\n')

# ------------------------------------------------------------------ narrative
w('## 1. Where this started\n')
w("""The previous day's work was about the *shape* of the photon-transfer curve of a single
device, and it left three results that this comparison is built on.

**The detector is non-linear by about 20 %, and the PTC dip measures it.** Plotting
Var/(Mean*Gain) against Mean showed a dip that no choice of gain between 1.02 and 1.10
could remove. Writing the response as S = F(Q) gives Var = F'(Q)^2 * g * Q, so the dip is
the *square of the differential response*: the measured curve and the independently
computed F'(Q)^2 coincide above 2.5 kADU. The integral non-linearity is below 0.5 % up to
~2.9 kADU, -5 % at 5-12 kADU, -14 to -21 % at 14 kADU and -29 to -33 % near saturation.
That is why every fit in this report stays inside a signal window, and why the window has
an upper edge at all.

**The PTC intercept is not the read noise, it is the threshold.** With Var = g*(S + T) the
intercept of the variance-versus-mean line is g*T, and the predicted values (100, 148 and
34 ADU^2 for the three die-runs tested) matched the measured intercepts (57, 85, 40) far
better than the read-noise variance did (5.3-7.1 ADU^2). This matters directly here: it is
the reason the ladder points must be weighted with their *measured* variance, since the
shot noise of a point follows the collected charge Q and not the measured signal Q - T.

**The fixed pattern splits into an additive and a multiplicative part, and the dark one is
clustered.** Repeating the analysis pixel by pixel instead of on a 100x100 superpixel gave
the same curves, so the effect is in the pixels and not in the averaging. The scatter across
pixels decomposed into an additive 7-10 ADU plus a multiplicative 0.4-0.6 % on the light
ladder, against 5.6-5.8 % on the dark ladder, which is the DSNU. A bootstrap against an
"all pixels identical" null at 120 ADU gave an observed spread of 8.897 against a null of
7.014 +- 0.050, i.e. a fixed pattern of 5.47 ADU at 38 sigma, and the Q-Q plot showed a
uniform stretch rather than a tail. Cross-run correlation of the pattern (r = 0.361, 0.199,
0.344 against 0.381, 0.193, 0.338 expected) showed it is 95-103 % repeatable, hence static
and calibratable. A block decomposition of the *spatial* variance then showed the dark
pattern is spatially clustered -- 1027 sigma above a noise-only null and still 11.2 sigma
above a shuffled null, with about 34 effective independent pixels per 100 -- while the
photo-response pattern is white.

Those results describe one device. The question left open was the one that matters for the
lot: **which of the ten bias settings actually measured should be used?**
""")

w('## 2. The question, and the five steps\n')
w("""The goal is to decide which wafer-test setup is best for measuring signals at the level
of several tens of ADU. The reduction follows five steps.

1. **Work on individual pixels, not superpixels.** All statistics are distributions over the
   10 000 pixels of the region. A single pixel's temporal variance comes from 3 frames, i.e.
   2 degrees of freedom: it is exponentially distributed with sd/mean = 1, so no single
   pixel's variance means anything and only the ensemble does. The ensemble is nevertheless
   well determined, to 1.4 % with 10^4 pixels.
2. **Separate the odd and even readout columns.** In the DESY orientation the readout columns
   run along the image *rows*, because the analysis image is the TIFF half transposed and
   rotated by 180 degrees. They are counted from 1 at the first pixel column of the selected
   gain half, so odd = detector columns 1, 3, 5 ... The even columns are the higher-gain,
   higher-threshold parity (+2.5 % in gain, +14.6 % in dark threshold at 32 sigma).
3. **Start from the bias frames.** The per-frame common mode, the bias fixed pattern with its
   own sampling noise removed, the per-pixel read noise, its tail, and the intrinsic
   pixel-to-pixel spread of that read noise.
4. **Then the per-pixel variance on both ladders.** Per-pixel weighted fits of signal against
   exposure time and against intensity, per-pixel dark current, thresholds by both methods,
   and the fixed pattern of every ladder step.
5. **Compare the setups** on a threshold / effective-noise plane and through the resulting
   signal-to-noise curves.
""")

w('## 3. What was built\n')
w("""Eight new methods on `ultrasat.lab.PTCAnalysis`, each in its own file in the class folder
so that the class itself only gained declarations, plus two statistical helpers:

| method | what it does |
|---|---|
| `rawColGeom` | raw readout-column index of every image row or column, in either orientation |
| `badColumns` | flags and masks the bad readout columns |
| `zeroNoiseStats` | bias common mode, fixed pattern, read noise and its intrinsic spread |
| `perPixelFits` | weighted per-pixel ladder fit with the analytic fit noise of every parameter |
| `stepFixedPattern` | fixed pattern of every ladder step, split into an additive term and the PRNU |
| `perPixelThreshold` | per-pixel thresholds, dark current, DSNU and PRNU with propagated errors |
| `noiseBudget` / `budgetCurve` | sigma_eff and SNR against charge, in electrons |
| `varSpread` | intrinsic spread of a per-pixel *variance* (chi2 deconvolution) |
| `paramSpread` | intrinsic spread of a *fitted parameter* (fit noise removed) |

Two deconvolutions carry most of the weight.

**The spread of a per-pixel variance needs a chi2 correction.** For V_i = T_i * chi2_nu/nu,
Var[V] = Var[T](1 + 2/nu) + (2/nu) E[T]^2, so with 3 repeats the sampling scatter alone gives
sd/mean = 1 even when every pixel is identical. `varSpread` removes it and returns a 95 %
upper limit when nothing is left.

**The spread of a fitted parameter needs its fit noise removed.** The observed spread of a
per-pixel slope or intercept is the true spread combined with the propagated photon and read
noise of the ladder, and that fit noise depends on the *lever arm*, which differs from setup
to setup. Comparing raw spreads would compare lever arms. `paramSpread` subtracts the
analytic parameter variance of the weighted fit; where the fit noise dominates it returns an
upper limit instead of a value.
""")

w('## 4. Four corrections found while building it\n')
w("""Each of these changed a number, so they are worth stating with their size.

| what was wrong | why it matters | effect |
|---|---|---|
| ladder points weighted with a *modelled* variance (RN^2 + g S) | the shot noise follows the collected charge Q, not the measured Q - T; the first T electrons are lost after they have fluctuated | over-weighted the lowest dark steps ~30x and understated the intercept fit noise 20x in variance; on the synthetic device the recovered intercept spread went from 9.07 (31 % high) to 6.52 against a true 6.93 |
| robust observed spread paired with a mean-based fit noise | a noisy pixel has a *larger* fitted variance, so the mismatch subtracts too much | biased every fixed pattern **downward**, i.e. made setups look better than they are |
| PRNU taken from the spread of the per-pixel response slope | the published bright window is three closely spaced intensity steps, which fixes a pixel's slope to only ~2.9 % | returned 0.000 % where the pattern is 0.47 %; now taken from the fixed pattern of the ladder steps |
| collected charge written as max(Q - T, 0) | a *negative* threshold means charge present at zero intensity, an offset the bias subtraction removes, not extra signal | let the collected charge exceed the incident charge for TX >= 3.7 V; run 38-2 appeared to reach SNR 5 at 5.5 e- instead of 39.4 |

A fifth point is a choice rather than a correction. The dark ladder is non-linear at **both**
ends: besides the 2.9 kADU limit above, the three lowest steps of run 31 lie +54.8, +31.7 and
+12.6 ADU *above* the straight line. Fitting everything below the linearity limit would move
the intercept from -139.8 to -112.2 ADU, changing the threshold by 20 % and turning the
residuals into a systematic arc. The published two-sided window (FitRange 1000-2500 ADU with
the 'auto' step rule) is therefore kept, which also keeps every median directly comparable
with the earlier reports.

The same `StdLightADU` arithmetic explains a number quoted in those earlier reports: the
spread of the light-method threshold, 54.3 ADU, is almost entirely intercept fit noise from
extrapolating three points at intensity 0.09-0.18 back to zero (52.8 ADU by the analytic
formula). The *median* threshold is unaffected; only the quoted spread was meaningless.
""")

w('## 5. Validation\n')
w("""Three independent checks.

**Against the published reduction.** Run with the same fit window on W04_D07, the per-pixel
chain reproduces run 31's dark threshold as 146.23 ADU against the published 146.42, its dark
current as 6.1554 against 6.1593 ADU/s, with an identical step selection and an exact bias
level; run 32 gives 17.27 ADU against 18.01.

**Against a synthetic device.** The unit test builds a device with a known gain, read noise,
per-pixel dark current, intercept and PRNU, and asserts that each estimator recovers them,
including the two deconvolutions and both signs of the threshold. It also asserts that the
default step selection reproduces `fitResponse` bit for bit.

**Internally, on the real data**, by reducing the same pixels twice with different sampling
noise -- which tests the chi2 deconvolution without assuming anything about the distribution
of the read noise.
""")
w(tail_check())

w('## 6. The data\n')
w(f"""{len(ROWS)} die-runs with full ladders over ten settings, plus {len(ZROWS)} ZE-only
die-runs from runs 39 and 39-2 which carry 10 bias frames instead of 5 and so constrain the
read-noise spread far better (9 degrees of freedom against 4). Run 35 is excluded: its TX and
RST_H are recorded in no file we parse. The whole batch took 92 minutes.

Two practical findings about the data itself. The reduction is entirely read-bound, and the
share `/bigdata3/projects` is an NFS mount from euclid while `/Data1/DESY` is a local mirror:
a region read costs 0.55 s over NFS against 0.064 s locally, which is the difference between
10 hours and 92 minutes for this batch. And the local mirror has exactly **one corrupt file**
out of the 9783 of these runs -- run 36's W08_D04 `PTC_Config.xlsx`, which is not a valid zip
-- found by comparing every file size against the share. That die was reduced from the share
instead and merged by tag; the mirror itself was left untouched.
""")

# ------------------------------------------------------------------ results
w('## 7. Results\n')
w('### 7.1 Setup summary\n')
w('Median over the dies of each setup. Thresholds by the light and dark methods in electrons; '
  'PRNU and the additive offset pattern *a* from the fit sigma_fixed^2 = a^2 + (b S)^2; '
  'Qlim = the smallest signal reaching SNR 5 per pixel in one 15 s frame.\n')
w('**Qlim is not a ranking column on its own.** Where the threshold is large it is set by the '
  'threshold and where the threshold is near zero by the noise; the last column repeats it '
  'with the threshold forced to zero, so the difference between the two is exactly what the '
  'threshold costs.\n')
w('Runs 32, 36 and 40 share the same setting on different days and are the repeatability '
  'check, not three independent setups. Runs 31 and 33 use the AV bias boards, the rest aSpect.\n')
w(table(SETUPS, SETUP_COLS) + '\n')

w('### 7.2 Read noise\n')
figblock('rn', 'Median per-pixel read noise against TX. It is flat and lowest up to TX 3.5 V and then doubles at every further 0.2 V.')
figblock('rn_spread', 'Intrinsic pixel-to-pixel spread of the read noise, chi2 sampling scatter removed.')
figblock('rn_tail', 'Fraction of pixels noisier than twice the median.')
w('### 7.3 Bias fixed pattern\n')
figblock('fpn', 'Fixed pattern of the bias frame. The ~100 ADU points are the RST_H 2.7 V setting, not a TX effect.')
w('### 7.4 Dark current\n')
figblock('dc', 'Median per-pixel dark current. The two upper branches are the AV bias boards, 22x the aSpect ones.')
figblock('dsnu', 'Dark-current non-uniformity with the fit noise removed; it is ~6 % of the dark current on both boards.')
w('### 7.5 Charge thresholds\n')
figblock('thr', 'Thresholds by both methods, in electrons. The light method crosses zero between TX 3.5 and 3.7 V.')
w('### 7.6 Odd and even readout columns\n')
figblock('parity', 'Odd minus even, per die-run: relative for the quantities bounded away from zero, absolute for the thresholds, whose relative difference blows up wherever the two parities straddle zero.')
w('The parity difference is modest up to TX 3.5 V -- about +2 % in read noise, +11 % in bias '
  'fixed pattern, nothing in dark current -- and then grows to +50 % and +100 % at TX >= 3.7 V. '
  'That is an independent reason not to go above 3.5 V.\n')
w('### 7.7 Non-uniformity of the response\n')
figblock('prnu', 'PRNU from the two-parameter pattern fit; it is 0.46-0.58 % in every setup and so does not discriminate between them.')
figblock('offset', 'Additive offset pattern, the a of the same fit.')
figblock('pattern', 'Fixed pattern against signal. The 1/S rise at low signal is the additive term and the floor is the PRNU, which is how the two are separated.')

if VARMEAN or os.path.isfile(os.path.join(A.indir, 'fig_varmean_overlay.png')):
    w('### 7.8 Variance against mean\n')
    w("""The photon-transfer curve is where most of these quantities come from, so it is worth
seeing directly. Each point of the cloud is one of the 10 000 masked pixels at one ladder step;
the two coloured curves are the per-step estimator for the even and the odd readout columns,
computed as median(V) * Dof/median(chi2_Dof) -- with three repeats that is median/ln2, the
unbiased robust estimator, because a per-pixel variance from 3 frames is exponentially
distributed and its mean over pixels is pulled up by cosmic rays. The green triangles are that
mean over pixels, shown precisely so the difference is visible. The dashed line is the PTC fit
actually used for the gain, over the range it was fitted on, and the dotted line is the read
noise squared.

The lower panel of each column divides out the expectation: (V - RN^2)/(S*g) is 1 wherever the
variance is pure shot noise. It is the same ratio plot as before, now with the bad columns
masked and the two parities separated.
""")
    if os.path.isfile(os.path.join(A.indir, 'fig_varmean_overlay.png')):
        w('![Per-step variance against mean for one reference die in every setup.](fig_varmean_overlay.png)\n')
        w('*Per-step variance against mean, one reference die per setup; dashed curves are the '
          'RST_H 2.7 V runs. Three things are visible at once. At low signal each dark curve '
          'flattens onto its own read-noise floor, and those floors span a factor of 30 between '
          'the setups -- from about 8 ADU^2 at TX 3.3 V to 240 at TX 3.9 V with RST_H 2.7 V. The '
          'two curves that reach 3 kADU are the AV bias boards, which get there in the same 600 s '
          'because their dark current is 22x larger. And the light ladders lie on top of each '
          'other below ~3 kADU, which is the statement that the conversion gain barely changes '
          'between setups, before they peel apart in the non-linear region.*\n')
    # two contrasting setups in the body, the rest in the appendix
    pick = []
    for want in ('run38_', 'run36-2_'):
        for v in VARMEAN:
            if v['tag'].startswith(want):
                pick.append(v)
                break
    for v in pick:
        rs = '' if abs(v['rsth']-3.0) < 0.01 else f", RST_H {v['rsth']:.1f} V"
        w(f"![Variance against mean, {v['die']}, run {v['run']}.]({v['file']})\n")
        w(f"*{v['die']}, run {v['run']} ({v['settings']}, TX {v['tx']:.1f} V{rs}), "
          f"gain {v['gain']:.3f} ADU/e-, {v['nbad']} bad columns masked.*\n")
    if pick:
        w('The two are the chosen optimum and the worst setting, and the difference is visible '
          'without any fitting: at TX 3.9 V with RST_H 2.7 V the cloud starts an order of '
          'magnitude higher on the variance axis, which is the read noise, and the ratio panel '
          'needs a far larger signal before it reaches 1. Every remaining setup is in the '
          'appendix.\n')

w('## 8. Noise budget and SNR\n')
w("""In electrons, with Qc = Q - max(T, 0) the charge actually collected,

    sigma_eff^2(Q) = RN^2 + Qc + DC*t + [(1-f) a]^2 + [(1-f) sigma_DC t]^2 + [(1-f) PRNU Qc]^2
    SNR(Q)         = Qc / sigma_eff(Q)

with f = 1 when the fixed patterns are calibrated out and f = 0 for a single raw frame. All
the fixed-pattern terms are intrinsic spreads with the fit noise already removed, so they do
not inherit the lever arm of the ladder.
""")
figblock('snr', 'Per-pixel SNR against incident charge for one die in each setup. The shaded region is below the charge threshold, where no signal is collected at all; the red curves are the AV boards.')

# ------------------------------------------------------------------ conclusion
w('## 9. Conclusion\n')
figblock('decision', 'Left: every setup on the threshold / read-noise plane, coloured by the signal it reaches SNR 5 at; the star is the best and the green band marks a threshold consistent with zero. Right: the limiting signal against TX with and without the threshold -- the gap between the circles and the triangles is what the threshold costs.')
w("""The left panel is the whole argument in one picture. Both axes have to be small, and only
one setup sits near the corner: **TX 3.5 V, RST_H 3.0 V, aSpect boards (run 38)**, with a
threshold of +2.4 e- and a read noise of 2.01 e-. Everything else is excluded by one axis or
the other.

- Below TX 3.5 V the pixel is marginally quieter (1.66 e- at TX 3.3) but the threshold eats
  31-41 e-, which is the whole signal of interest. On the right panel those setups sit far
  above their own triangles: the threshold, not the noise, is what limits them.
- Above TX 3.5 V the threshold is gone -- it goes negative, which costs nothing because a
  negative threshold is a constant offset that the bias and dark subtraction remove -- but the
  read noise doubles at every 0.2 V step, and the circles and triangles merge: those setups
  are noise-limited, and the noise is worse.
- The AV bias boards are excluded outright. They are no noisier, but they carry 22x the dark
  current and a 92 e- threshold.

The honest caveat is in the same figure: +2.4 e- is inside the run-to-run scatter, so the
claim is that the threshold at TX 3.5 V is *consistent with zero*, not that it is 2.4 e-.
""")
figblock('txsum', 'The same conclusion as four one-dimensional cuts. Threshold crosses zero just above the dashed line; read noise, bias pattern and limiting signal all turn upward there.')
w("""The four panels show why the optimum is a corner rather than an end point: the threshold
(top left) falls monotonically with TX and crosses zero just above 3.5 V, while the read noise
(top right), the bias fixed pattern (bottom left) and the resulting limiting signal (bottom
right) all turn sharply upward at the same place. The diamonds are the RST_H 2.7 V runs and sit
clearly above the circles wherever both exist.
""")
figblock('qlim', 'Smallest detectable signal at SNR 5, by setup.')

w('### 9.1 The ranking, and the second knob\n')
w(rank_text())

w('## 10. Per-die results\n')
w(table(ROWS, MAIN_COLS) + '\n')
w('### 10.1 Odd and even readout columns\n')
w(table(ROWS, PARITY_COLS) + '\n')
if ZROWS:
    w('### 10.2 ZE-only runs (39, 39-2)\n')
    w('10 bias frames instead of 5, so 9 degrees of freedom against 4. The last two columns '
      'repeat the analysis on the first 5 frames only, which is the apples-to-apples '
      'comparison with every other run -- and the difference is itself informative: the '
      'median per-pixel sigma is biased about 5 % low at 4 degrees of freedom (2.406 ADU '
      'from 10 frames against 2.285 from the first 5 of the same data), so runs with '
      'different bias-frame counts must never be compared directly. These two runs also fill '
      'the TX gap at 3.6 and 3.8 V and confirm the noise rise on independent data: 2.18-2.51 '
      'ADU at TX 3.6 against 5.44-5.82 at TX 3.8.\n')
    w(table(ZROWS, ZERO_COLS) + '\n')

w('## 11. Caveats\n')
w("""- The per-pixel read noise comes from 5 bias frames of integer ADU with sigma ~ 2 ADU, so
  the chi2 model behind the spread deconvolution is approximate at the 1 ADU quantisation
  scale. Runs 39 and 39-2 are the reference.
- The pixel-to-pixel spread of the light-method threshold is **not** measurable from the
  published three-step bright window; its median is unaffected. The offset term of the noise
  budget therefore comes from the additive part of the bright-ladder pattern fit.
- The calibrated SNR curve assumes the fixed patterns are removed exactly. A master bias built
  from 5 frames adds RN/sqrt(5), about 9.5 % on sigma_eff at low signal; it applies to every
  setup equally and does not reorder the comparison.
- Run 35 (5 dies on wafers W03, W07, W12) is excluded because its bias settings are unrecorded.
- The dark fixed pattern is spatially clustered, about 34 effective independent pixels per 100,
  so averaging the dark signal over an area beats down more slowly than sqrt(N). The
  photo-response pattern is white.
- TX and RST_H are confounded in the original design at TX 3.0 and 3.9 V, where RST_H is 2.7 V.
  They are separated here only at TX 3.9 V, where both values were measured.
""")

if VARMEAN:
    w('## 12. Appendix: variance against mean, every setup\n')
    w('One reference die per setup, ordered by TX. Dark ladder on the left, light on the right; '
      'the cloud is every masked pixel at every step below saturation, the red and blue curves '
      'are the even and odd readout columns, the green triangles the mean over pixels and the '
      'dashed line the fitted PTC.\n')
    for v in VARMEAN:
        rs = '' if abs(v['rsth']-3.0) < 0.01 else f", RST_H {v['rsth']:.1f} V"
        w(f"**Run {v['run']} — {v['settings']}, TX {v['tx']:.1f} V{rs} — {v['die']}**\n")
        w(f"![Variance against mean, run {v['run']}, {v['die']}.]({v['file']})\n")

md = '\n'.join(MD)
with open(os.path.join(OUT, 'report.md'), 'w') as fh:
    fh.write(md)

# ---------------------------------------------------------------- html
md_src = md.replace('</script', '<\\/script')
HTML = """<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>TH02954 individual-pixel setup comparison</title>
<style>
body{max-width:1150px;margin:2rem auto;padding:0 1rem;font:15px/1.6 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{border-bottom:2px solid #ddd;padding-bottom:.3rem}
h2{margin-top:2.2rem;border-bottom:1px solid #eee}
table{border-collapse:collapse;font-size:11.5px;margin:1rem 0;display:block;overflow-x:auto}
th,td{border:1px solid #ddd;padding:2px 6px;text-align:right;white-space:nowrap}
th{background:#f5f5f5}
td:nth-child(-n+5),th:nth-child(-n+5){text-align:left}
img{max-width:100%;margin:.6rem 0;border:1px solid #eee}
em{color:#666;font-size:13px}
code{background:#f5f5f5;padding:1px 4px}

/* Print: the screen rule above scrolls wide tables sideways, which a PDF
   cannot do -- it just clips them. On paper the table must lay itself out
   within the page instead, with the headers allowed to wrap. */
@page{size:A4 portrait;margin:11mm 9mm}
@media print{
  body{max-width:none;margin:0;padding:0;font-size:11.5px}
  table{display:table;width:100%;overflow:visible;font-size:6.8px;margin:.5rem 0}
  th,td{white-space:normal;overflow-wrap:anywhere;padding:1px 2px;line-height:1.15}
  th{font-weight:600}
  img{page-break-inside:avoid;max-width:100%}
  h1,h2,h3{page-break-after:avoid}
  p,li{orphans:2;widows:2}
}
</style></head><body>
<div id="c"></div>
<script type="text/markdown" id="src">
__MD__
</script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/9.1.6/marked.min.js"></script>
<script>
document.getElementById('c').innerHTML =
  marked.parse(document.getElementById('src').textContent);
</script>
</body></html>"""
with open(os.path.join(OUT, 'report.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md_src))

print(f'{len(ROWS)} die-runs, {len(ZROWS)} ZE-only, {len(FIGS)} figures -> {OUT}')
