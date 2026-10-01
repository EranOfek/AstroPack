#!/usr/bin/env python3
"""Variance-versus-mean figures for the individual-pixel comparison report.

Per setup: the cloud of all 10^4 masked pixels at every ladder step, with the
per-step estimators for the even and odd readout columns on top and the PTC
line, plus a ratio panel (Var - RN^2)/(Mean*Gain). Dark and light side by side.
Reads what desy_perpixel_varmean_extract.m wrote.

Also one overlay of the per-step curves of every setup, taken from the stored
results of desy_perpixel_run.m (no pixel arrays needed).
"""
import argparse, json, os, math, glob
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_perpixel/varmean')
P.add_argument('--results', default='/home/sasha/claude/desy_perpixel/perpixel.json')
P.add_argument('--out', default='/home/sasha/claude/desy_perpixel')
A = P.parse_args()
os.makedirs(A.out, exist_ok=True)

LN2 = math.log(2.0)          # chi2(2) median / 2 : median(V) = ln2 * sigma^2

def chi2_med_factor(nframes):
    """Dof / median of chi2(Dof): turns a median of per-pixel variances into a variance"""
    from math import lgamma
    dof = max(int(nframes) - 1, 1)
    if dof == 2:
        return 1.0/LN2
    # median of chi2(dof) by bisection on the regularised lower incomplete gamma
    try:
        from scipy.stats import chi2            # not required, used when present
        return dof/chi2.median(dof)
    except Exception:
        a = dof/2.0
        def plower(q):
            # regularised lower incomplete gamma P(a, q/2) by its series
            x = q/2.0
            term, ssum = 1.0/a, 1.0/a
            for n in range(1, 500):
                term *= x/(a + n)
                ssum += term
                if abs(term) < 1e-14*abs(ssum):
                    break
            return ssum*math.exp(-x + a*math.log(x) - lgamma(a))
        lo, hi = 1e-6, 100.0*dof
        for _ in range(200):
            mid = 0.5*(lo + hi)
            if plower(mid) < 0.5:
                lo = mid
            else:
                hi = mid
        return dof/(0.5*(lo + hi))

def load(tag, name, shape, dtype=np.float32):
    p = os.path.join(A.indir, f'{tag}_{name}.bin')
    if not os.path.isfile(p):
        return None
    a = np.fromfile(p, dtype=dtype)
    return a.reshape(shape, order='F')

def setup_label(m):
    s = f"run {m['Run']}  {m['Settings']}  TX {m['TX']:.1f} V"
    if abs(float(m['RSTH']) - 3.0) > 0.01:
        s += f"  RST_H {m['RSTH']:.1f} V"
    return s

# ---------------------------------------------------------------- per setup
META = []
mp = os.path.join(A.indir, 'varmean_meta.json')
if os.path.isfile(mp):
    with open(mp) as fh:
        META = json.load(fh)
    if isinstance(META, dict):
        META = [META]

def panel(axd, axr, M, V, mask, parity, rn2, gain, offset, nfr, gainrange,
          satlevel, title, xlabel):
    """one ladder: density + per-step estimators (top), ratio (bottom)"""
    ny, nx, ns = M.shape
    good = mask.astype(bool)
    med = np.array([np.median(M[:, :, k][good]) for k in range(ns)])
    use = med < satlevel
    # the pixel cloud
    xs, ys = [], []
    for k in range(ns):
        if not use[k]:
            continue
        xs.append(M[:, :, k][good].ravel())
        ys.append(V[:, :, k][good].ravel())
    if not xs:
        return med, use
    xs = np.concatenate(xs); ys = np.concatenate(ys)
    logx = np.nanmin(med[use]) > 5.0
    ok = np.isfinite(xs) & np.isfinite(ys) & (ys > 0)
    if logx:
        ok &= xs > 0
    xp, yp = xs[ok], ys[ok]
    # the axes follow the ladder, not the cosmic rays: a handful of hit pixels
    # otherwise stretch the range by an order of magnitude and flatten the cloud
    fac0 = chi2_med_factor(np.atleast_1d(nfr)[0])
    vstep = np.array([np.median(V[:, :, k][good])*fac0 for k in range(ns)])
    mlo, mhi = np.nanmin(med[use]), np.nanmax(med[use])
    if logx:
        xlo, xhi = max(mlo/1.6, 1e-2), mhi*1.6
    else:
        pad = 0.08*max(mhi - mlo, 1.0)
        xlo, xhi = mlo - pad, mhi + pad
    ylo = max(rn2/6.0, 1e-2)
    yhi = max(np.nanmax(vstep[use])*25.0, ylo*10)
    xb = (np.logspace(np.log10(xlo), np.log10(xhi), 110) if logx
          else np.linspace(xlo, xhi, 110))
    yb = np.logspace(np.log10(ylo), np.log10(yhi), 110)
    axd.hist2d(xp, yp, bins=[xb, yb], norm=LogNorm(), cmap='Greys', cmin=1)
    axd.set_xlim(xlo, xhi)
    axd.set_ylim(ylo, yhi)
    # per-step estimators, per parity
    fac = chi2_med_factor(np.atleast_1d(nfr)[0])
    for sel, lab, col in ((good & ~parity, 'even columns', '#c44e52'),
                          (good & parity,  'odd columns',  '#4c72b0')):
        mm = np.array([np.median(M[:, :, k][sel]) for k in range(ns)])
        vv = np.array([np.median(V[:, :, k][sel])*fac for k in range(ns)])
        axd.plot(mm[use], vv[use], 'o-', ms=3.5, lw=1.1, color=col, label=lab)
    mm = np.array([np.median(M[:, :, k][good]) for k in range(ns)])
    vmean = np.array([np.mean(V[:, :, k][good]) for k in range(ns)])
    axd.plot(mm[use], vmean[use], '^', ms=4, color='#55a868', alpha=0.9,
             label='mean over pixels (cosmic rays)')
    # the PTC line actually fitted
    gl = np.array(gainrange, dtype=float)
    xx = np.linspace(max(gl[0], np.nanmin(mm[use])), min(gl[1], np.nanmax(mm[use])), 20)
    if xx.size > 1 and xx[-1] > xx[0]:
        axd.plot(xx, gain*xx + offset, '--', lw=1.4, color='k',
                 label=f'PTC fit, g = {gain:.3f} ADU/e-')
    axd.axhline(rn2, color='#8172b2', lw=1, ls=':', label=f'RN$^2$ = {rn2:.1f} ADU$^2$')
    if logx:
        axd.set_xscale('log')
    axd.set_yscale('log')
    axd.set_ylabel('per-pixel variance [ADU$^2$]')
    axd.set_title(title, fontsize=9)
    axd.grid(alpha=0.25, which='both')
    axd.legend(fontsize=6.5, loc='lower right', framealpha=0.9)
    # ratio panel
    for sel, lab, col in ((good & ~parity, 'even', '#c44e52'),
                          (good & parity,  'odd',  '#4c72b0')):
        mm2 = np.array([np.median(M[:, :, k][sel]) for k in range(ns)])
        vv2 = np.array([np.median(V[:, :, k][sel])*fac for k in range(ns)])
        with np.errstate(divide='ignore', invalid='ignore'):
            rr = (vv2 - rn2)/(mm2*gain)
        g2 = use & (mm2 > 20)
        axr.plot(mm2[g2], rr[g2], 'o-', ms=3.5, lw=1.1, color=col, label=lab)
    axr.axhline(1.0, color='k', lw=1, ls='--')
    if logx:
        axr.set_xscale('log')
    axr.set_xlim(xlo, xhi)
    axr.set_ylim(0, 1.6)
    axr.set_xlabel(xlabel)
    axr.set_ylabel(r'$(V-RN^2)/(S\,g)$', fontsize=8)
    axr.grid(alpha=0.25, which='both')
    axr.legend(fontsize=6.5, ncol=2)
    return med, use

FIGS = []
for m in META:
    tag = m['Tag']
    ny, nx = [int(v) for v in m['Size']]
    nd, nb = len(np.atleast_1d(m['DarkX'])), len(np.atleast_1d(m['BrightX']))
    dm = load(tag, 'dark_mean', (ny, nx, nd))
    dv = load(tag, 'dark_var', (ny, nx, nd))
    bm = load(tag, 'bright_mean', (ny, nx, nb))
    bv = load(tag, 'bright_var', (ny, nx, nb))
    zn = load(tag, 'zeronoise', (ny, nx))
    mask = load(tag, 'mask', (ny, nx), np.uint8)
    par = load(tag, 'parity', (ny, nx), np.uint8)
    if any(v is None for v in (dm, dv, bm, bv, zn, mask, par)):
        print(f'  {tag}: arrays missing, skipped')
        continue
    mask = mask.astype(bool); par = par.astype(bool)
    rn2 = float(np.median(zn[mask])**2)
    fig, axs = plt.subplots(2, 2, figsize=(12.6, 7.0),
                            gridspec_kw={'height_ratios': [3, 1.25]})
    panel(axs[0, 0], axs[1, 0], dm, dv, mask, par, rn2, float(m['Gain']),
          float(m['GainOffset']), m['DarkNframes'], m['GainRange'], float(m['SatLevel']),
          'dark ladder', 'per-pixel signal [ADU]')
    panel(axs[0, 1], axs[1, 1], bm, bv, mask, par, rn2, float(m['Gain']),
          float(m['GainOffset']), m['BrightNframes'], m['GainRange'], float(m['SatLevel']),
          'light ladder', 'per-pixel signal [ADU]')
    fig.suptitle(f"Variance against mean, {m['Die']}, {setup_label(m)}   "
                 f"({int(m['Npix'])} pixels, {int(m['Nbad'])} bad columns masked)",
                 fontsize=10)
    fig.tight_layout()
    out = f'fig_varmean_{tag}.png'
    fig.savefig(os.path.join(A.out, out), dpi=105)
    plt.close(fig)
    FIGS.append((tag, out, m))
    print(f'  {tag} -> {out}')

# ---------------------------------------------------------------- overlay
with open(A.results) as fh:
    D = json.load(fh)
FULL = D.get('Full') or []
for pf in sorted(glob.glob(os.path.join(os.path.dirname(A.results), 'perpixel_patch*.json'))):
    with open(pf) as fh:
        Pp = json.load(fh)
    nw = Pp.get('Full') or []
    tags = {e['Tag'] for e in nw}
    FULL = [e for e in FULL if e.get('Tag') not in tags] + nw

REF = {}
for e in FULL:
    die = 'W04_D07' if str(e['Run']) != '40' else 'W04_D04'
    if e['Die'] == die:
        REF[str(e['Run'])] = e

if REF:
    cmap = plt.get_cmap('viridis')
    txs = sorted({float(e['TX']) for e in REF.values()})
    cidx = {t: cmap(i/max(len(txs)-1, 1)) for i, t in enumerate(txs)}
    fig, axs = plt.subplots(1, 2, figsize=(12.4, 5.0))
    for ax, key, lab in ((axs[0], 'PatternD', 'dark ladder'), (axs[1], 'PatternB', 'light ladder')):
        for run in sorted(REF, key=lambda r: (float(REF[r]['TX']), r)):
            e = REF[run]
            pat = e.get(key, {}).get('All', {})
            med = np.atleast_1d(np.asarray(pat.get('Median', []), dtype=float))
            sdn = np.atleast_1d(np.asarray(pat.get('StdNoise', []), dtype=float))
            nfr = np.atleast_1d(np.asarray(e[key].get('Nframes', []), dtype=float))
            if med.size < 3 or sdn.size != med.size:
                continue
            var = sdn**2 * nfr                       # variance of one frame
            ok = np.isfinite(med) & np.isfinite(var) & (med > 0) & (var > 0) & (med < 14000)
            ls = '-' if abs(float(e['RSTH']) - 3.0) < 0.01 else '--'
            ax.plot(med[ok], var[ok], ls, lw=1.4, color=cidx[float(e['TX'])], alpha=0.9,
                    label=f"{run}  TX {float(e['TX']):.1f}"
                          + ('' if abs(float(e['RSTH'])-3.0) < 0.01 else f"  RST_H {float(e['RSTH']):.1f}"))
        ax.plot([1, 1e4], [1, 1e4], ':', color='grey', lw=1, label='Var = Mean (g = 1)')
        ax.set_xscale('log'); ax.set_yscale('log')
        ax.set_xlabel('median signal [ADU]')
        ax.set_ylabel('variance of one frame [ADU$^2$]')
        ax.set_title(lab, fontsize=10)
        ax.grid(alpha=0.25, which='both')
        ax.legend(fontsize=6.5, ncol=2)
    fig.suptitle('Per-step variance against mean, one reference die per setup '
                 '(dashed = RST_H 2.7 V)', fontsize=10)
    fig.tight_layout()
    fig.savefig(os.path.join(A.out, 'fig_varmean_overlay.png'), dpi=110)
    plt.close(fig)
    print('  overlay -> fig_varmean_overlay.png')

with open(os.path.join(A.out, 'varmean_figs.json'), 'w') as fh:
    json.dump([{'tag': t, 'file': f,
                'run': str(m['Run']), 'die': m['Die'], 'tx': float(m['TX']),
                'rsth': float(m['RSTH']), 'settings': m['Settings'],
                'gain': float(m['Gain']), 'nbad': int(m['Nbad'])} for t, f, m in FIGS], fh)
print(f'{len(FIGS)} per-setup figures')
