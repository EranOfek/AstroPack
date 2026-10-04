#!/usr/bin/env python3
"""One photon transfer curve carrying both ladders.

Variance against mean signal for every step of the dark and the bright ladder
on the same axes, with the fitted PTC line of stage 5. If dark charge and
photo-charge are the same kind of charge, the two ladders lie on one line: the
shot noise does not know where the electrons came from. Where they separate,
part of the dark signal is reaching the pixel without full shot noise.

Variances are the per-step medians corrected for the chi2 median bias
(x Dof/(2*gammaincinv(0.5,Dof/2))), i.e. robust estimates of the mean, taken
from the stage 7 dump; the fit and the gain come from stage 5.
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_die/run32_W04_D07_high')
P.add_argument('--out', default=None)
P.add_argument('--xmax', type=float, default=1000.0,
               help='upper signal of the left panel [ADU]; the ratio panel always shows all steps')
A = P.parse_args()
OUT = A.out or A.indir

with open(os.path.join(A.indir, 'ptc.json')) as fh:
    PT = json.load(fh)
with open(os.path.join(A.indir, 'varspread.json')) as fh:
    VS = json.load(fh)
ST = VS['Steps'] if not isinstance(VS['Steps'], dict) else [VS['Steps']]
TAG = f"{PT['Lot']} {PT['Die']}, run {PT['Run']}, {PT['GainHalf']} gain"
G   = float(PT['GainEnsemble'])
C   = float(PT['OffsetEnsemble'])
WIN = [float(v) for v in PT['GainRange']]
RN2 = float(VS['Steps'][0]['SigmaNull'])**2 if ST else np.nan

def pts(ty):
    s = [e for e in ST if e['Type'] == ty and not e.get('Saturated')]
    x = np.array([float(e['Signal']) for e in s])
    y = np.array([float(e['SigmaNull'])**2 for e in s])
    n = np.array([int(e['Step']) for e in s])
    return x, y, n

xd, yd, nd = pts('D')
xb, yb, nb = pts('B')
xz, yz, _  = pts('ZE')
XMAX = A.xmax                      # left panel: the band where both ladders live
XALL = 1.15*max(xb.max() if xb.size else 0, xd.max() if xd.size else 0)

fig, axs = plt.subplots(1, 2, figsize=(13.4, 5.4))

# ------------------------------------------------------------------ 1. the curve
ax = axs[0]
ax.axvspan(WIN[0], WIN[1], color='#dd8452', alpha=0.12,
           label=f'PTC fit window {WIN[0]:.0f}-{WIN[1]:.0f} ADU')
xs = np.linspace(0, XMAX, 200)
ax.plot(xs, G*xs + C, 'k--', lw=1.6,
        label=f'fit to the bright ladder: Var = {G:.4f}·S + {C:.1f}')
ax.plot(xb, yb, 's-', ms=7, lw=1.0, color='#c44e52', label='bright ladder')
ax.plot(xd, yd, 'o-', ms=7, lw=1.0, color='#4c72b0', label='dark ladder')
if xz.size:
    ax.plot(xz, yz, '^', ms=10, color='#55a868', label='bias frames (zero signal)')
for x, y, n in zip(xd, yd, nd):
    if x < XMAX:
        ax.annotate(f'{n}', (x, y), textcoords='offset points', xytext=(4, -11), fontsize=7,
                    color='#4c72b0')
for x, y, n in zip(xb, yb, nb):
    if x < XMAX:
        ax.annotate(f'{n}', (x, y), textcoords='offset points', xytext=(4, 5), fontsize=7,
                    color='#c44e52')
ax.set_xlim(0, XMAX)
ax.set_ylim(0, 1.15*max(yb[xb < XMAX].max() if np.any(xb < XMAX) else 0,
                        yd[xd < XMAX].max() if np.any(xd < XMAX) else 0))
ax.set_xlabel('mean signal [ADU]')
ax.set_ylabel('variance [ADU$^2$]')
ax.set_title(f'Both ladders on one curve, below {XMAX:.0f} ADU', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8, loc='upper left')

# ------------------------------------------------------------------ 2. ratio to the line
ax = axs[1]
ax.axhline(1, color='k', lw=1.1)
ax.axvspan(WIN[0], WIN[1], color='#dd8452', alpha=0.12)
for x, y, lab, col, mk in ((xb, yb, 'bright ladder', '#c44e52', 's'),
                           (xd, yd, 'dark ladder', '#4c72b0', 'o')):
    ok = x > 1
    ax.plot(x[ok], y[ok]/(G*x[ok] + C), mk + '-', ms=7, lw=1.2, color=col, label=lab)
if xz.size:
    ax.plot([1.0], [yz[0]/(G*0 + C)], '^', ms=10, color='#55a868',
            label='bias frames (at S = 0)')
ax.set_xscale('log')
ax.axvline(XMAX, color='#888888', ls=':', lw=1.2)
ax.set_xlim(1, XALL)
ax.set_xlabel('mean signal [ADU]  (dotted line: the left panel ends here)')
ax.set_ylabel('variance / fitted line')
ax.set_title('The same points as a ratio: dark charge makes less noise than\n'
             'photo-charge of the same measured size', fontsize=9.5)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5, loc='lower right')
fig.suptitle(f'Photon transfer curve, dark and light together — {TAG}', fontsize=11)
fig.tight_layout()
fig.savefig(os.path.join(OUT, 'fig_ptc_both.png'), dpi=130)
print('wrote', os.path.join(OUT, 'fig_ptc_both.png'))

# a number to quote with it
if xd.size and xb.size:
    hi = xd.max()
    r  = yd[np.argmax(xd)]/(G*hi + C)
    print(f'at the top of the dark ladder ({hi:.0f} ADU) the dark variance is '
          f'{100*(r-1):+.1f} % against the bright-ladder line')
