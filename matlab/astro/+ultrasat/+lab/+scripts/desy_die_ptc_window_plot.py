#!/usr/bin/env python3
"""The bright photon-transfer curve with the region the gain is fitted over.

The counterpart of the ladder-fit figure: it shows WHERE the gain comes from,
which the report had only as a number. Variance minus read noise against mean
signal, the fitted points filled and the rest open, the fitted line over the
window and dashed outside it, and the intercept -- g*T, not the read noise --
marked at zero signal.
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir', required=True)
P.add_argument('--out', default=None)
P.add_argument('--name', default='fig_ptc_window.png')
A = P.parse_args()
OUT = A.out or A.indir

PT = json.load(open(os.path.join(A.indir, 'ptc_points.json')))
ME = json.load(open(os.path.join(A.indir, 'methods.json')))
DK = json.load(open(os.path.join(A.indir, 'dark.json')))
TAG = f"{DK['Lot']} {DK['Die']}, run {DK['Run']}, {DK['GainHalf']} gain"

g = float(ME['Routes']['d']['Gain'])
c = float(ME['Routes']['d']['Intercept'])
ge = float(np.hypot(float(ME['Routes']['d']['GainStat']), float(ME['Routes']['d']['GainSyst'])))
win = [float(v) for v in PT['Meta']['GainWindow']]
used = set(int(s) for s in np.atleast_1d(np.array(json.load(
    open(os.path.join(A.indir, 'ptc.json')))['Steps'], dtype=int)).tolist())

pts = [p for p in PT['Points'] if p['Type'] == 'B' and not p.get('Saturated')]
pts.sort(key=lambda p: float(p['SignalMean']))
x = np.array([float(p['SignalMean']) for p in pts])
y = np.array([float(p['ExcessMean']) for p in pts])
sel = np.array([int(p['Step']) in used for p in pts])
show = x <= 3.0*x[sel].max()

fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.0))
for ax, zoom in zip(axs, (False, True)):
    m = (x >= 0) & (show if not zoom else (x <= 1.25*x[sel].max()))
    xs = np.linspace(0, (x[m].max() if m.any() else 1)*1.03, 200)
    inw = (xs >= x[sel].min()) & (xs <= x[sel].max())
    ax.axvspan(x[sel].min(), x[sel].max(), color='#dd8452', alpha=0.13,
               label='gain fit window')
    ax.plot(xs[~inw], g*xs[~inw] + c, 'k--', lw=1.2, label='extrapolated')
    ax.plot(xs[inw], g*xs[inw] + c, 'k-', lw=1.9,
            label=f'fit: Var-RN$^2$ = {g:.4f}$\\cdot$S {c:+.1f}')
    ax.plot(x[m & ~sel], y[m & ~sel], 'o', ms=8, mfc='none', color='#c44e52')
    ax.plot(x[m & sel], y[m & sel], 'o', ms=8, color='#c44e52')
    ax.plot([0], [c], 's', ms=9, color='#4c72b0')
    ax.axhline(0, color='#999999', lw=0.8)
    ax.set_xlabel('mean signal [ADU]')
    ax.set_ylabel('variance - RN$^2$ [ADU$^2$]')
    ax.grid(alpha=0.25)
    if not zoom:
        ax.set_title('Bright photon-transfer curve: the gain is the slope', fontsize=10)
        ax.legend(fontsize=8.5, loc='upper left')
    else:
        ax.set_title('The fitted window, and the intercept at zero signal', fontsize=10)
        ax.annotate(f'gain = {g:.4f} $\\pm$ {ge:.4f} ADU/e-\n'
                    f'intercept = {c:.1f} ADU$^2$ = g$\\cdot$T, not RN$^2$\n'
                    f'fitted over {x[sel].min():.0f}-{x[sel].max():.0f} ADU, '
                    f'{int(sel.sum())} points',
                    (0.04, 0.96), xycoords='axes fraction', va='top', fontsize=9.5,
                    bbox=dict(fc='white', ec='#cccccc', alpha=0.92))
fig.suptitle(f'Where the conversion gain is measured — {TAG}', fontsize=11)
fig.tight_layout()
fig.savefig(os.path.join(OUT, A.name), dpi=125)
print('wrote', os.path.join(OUT, A.name))
