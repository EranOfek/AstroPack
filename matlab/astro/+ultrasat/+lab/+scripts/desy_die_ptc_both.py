#!/usr/bin/env python3
"""One photon transfer curve carrying both ladders.

Variance against mean signal for every step of the dark and the bright ladder
on the same axes, with the fitted PTC line of stage 5. If dark charge and
photo-charge are the same kind of charge, the two ladders lie on one line: the
shot noise does not know where the electrons came from. Where they separate,
part of the dark signal is reaching the pixel without full shot noise.

The ordinate is Var - RN^2, each pixel's own read noise removed before averaging
rather than subtracted from the intercept afterwards. The term is small (3.5 of
150 ADU^2 at the lowest bright step) but it makes the intercept mean one thing,
g*T, and takes a whole-die constant out of a quantity that varies pixel to pixel.

Both axes are MEANS over one common set of pixels -- those outside the top 0.1 %
of the variance, where the cosmic rays are. That is the only consistent choice:
Var = g*S + c holds per pixel, so averaging over pixels needs E[Var] against
E[S]. An earlier version plotted a median signal against a mean variance, which
is not a point on any curve, and because the dark signal is right-skewed (its
mean is 6 % above its median) while the bright signal is not, it halved the
apparent gap between the two ladders. Those points are kept in the ratio panel
as open symbols.

For reference, the discarded recipe was: per-step medians corrected twice. The first
correction is the chi2 median bias, x Dof/(2*gammaincinv(0.5,Dof/2)): a
per-pixel variance is chi2 distributed, so its median sits below its mean by a
known factor. The plain mean cannot be used -- a cosmic ray in one of three
frames puts a pixel at 10^7 ADU^2, and 0.001 % of pixels carry 99.9 % of it.

The second correction matters more than it looks. The first one is exact only
if every pixel has the SAME true variance; when the true variance spreads by a
relative width s, the corrected median estimates the median rather than the
mean, and the two differ by sqrt(1+s^2). The dark ladder spreads 27 % and the
bright ladder under 6 % (stage 7), so leaving it out biases the two ladders
differently and inflates the apparent gap between them by about 3 percentage
points. s is taken per step from the stage 7 dump.
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
with open(os.path.join(A.indir, 'methods.json')) as fh:
    ME = json.load(fh)
with open(os.path.join(A.indir, 'ptc_points.json')) as fh:
    PP = json.load(fh)
ST = PP['Points'] if not isinstance(PP['Points'], dict) else [PP['Points']]
TAG = f"{PT['Lot']} {PT['Die']}, run {PT['Run']}, {PT['GainHalf']} gain"
# the line is the bright-ladder PTC of stage 10: unweighted, means on both axes,
# fitted on Var - RN^2 so its intercept is g*T and nothing else
G   = float(ME['Routes']['d']['Gain'])
C   = float(ME['Routes']['d']['Intercept'])
WIN = [float(v) for v in PT['GainRange']]


def pts(ty):
    s = [e for e in ST if e['Type'] == ty and not e.get('Saturated')]
    # The pair that belongs on a PTC: means of BOTH the signal and the variance,
    # over one common set of pixels (those outside the top 0.1 % of the variance,
    # which is where the cosmic rays are). Var = g*S + c holds per pixel, so
    # averaging over pixels needs E[Var] against E[S]; a median on one axis and a
    # mean on the other is not a point on any curve, and because the dark signal
    # is right-skewed and the bright signal is not, mixing them halves the
    # apparent gap between the two ladders.
    x  = np.array([float(e['SignalMean']) for e in s])
    y  = np.array([float(e['ExcessMean']) for e in s])     # Var - RN^2, read noise removed
    x0 = np.array([float(e['SignalMedian']) for e in s])     # the earlier, mixed pair
    y0 = np.array([float(e['VarMeanCorrected']) for e in s])
    n  = np.array([int(e['Step']) for e in s])
    return x, y, n, (x0, y0)

xd, yd, nd, md = pts('D')
xb, yb, nb, mb = pts('B')
xz, yz, _,  mz = pts('ZE')
XMAX = A.xmax                      # left panel: the band where both ladders live
XALL = 1.15*max(xb.max() if xb.size else 0, xd.max() if xd.size else 0)

_xlo0 = 1.0
fig, axs = plt.subplots(1, 2, figsize=(13.4, 5.4))

# ------------------------------------------------------------------ 1. the curve
ax = axs[0]
ax.axvspan(WIN[0], WIN[1], color='#dd8452', alpha=0.12,
           label=f'PTC fit window {WIN[0]:.0f}-{WIN[1]:.0f} ADU')
xs = np.logspace(np.log10(max(_xlo0, 0.5)), np.log10(XMAX), 200)
ax.plot(xs, G*xs + C, 'k--', lw=1.6,
        label=f'bright ladder: Var-RN$^2$ = {G:.4f}·S + {C:.1f}')
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
ax.set_xscale('log'); ax.set_yscale('log')
_xlo = max(1.0, 0.7*min(xd[xd > 0].min() if np.any(xd > 0) else 1, xb.min()))
ax.set_xlim(_xlo, XMAX)
_yv = np.concatenate([yd[(xd > 0) & (xd < XMAX)], yb[xb < XMAX]])
ax.set_ylim(0.6*_yv.min(), 1.6*_yv.max())
ax.set_xlabel('mean signal [ADU]')
ax.set_ylabel('variance - RN$^2$ [ADU$^2$]')
ax.set_title(f'Both ladders on one curve, below {XMAX:.0f} ADU (log-log)', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8, loc='upper left')

# ------------------------------------------------------------------ 2. ratio to the line
ax = axs[1]
ax.axhline(1, color='k', lw=1.1)
ax.axvspan(WIN[0], WIN[1], color='#dd8452', alpha=0.12)
for x, y, m0, lab, col, mk in ((xb, yb, mb, 'bright ladder', '#c44e52', 's'),
                               (xd, yd, md, 'dark ladder', '#4c72b0', 'o')):
    ok = x > 1
    ok0 = m0[0] > 1
    ax.plot(m0[0][ok0], m0[1][ok0]/(G*m0[0][ok0] + C), mk, ms=6, mfc='none', color=col, alpha=0.5)
    ax.plot(x[ok], y[ok]/(G*x[ok] + C), mk + '-', ms=7, lw=1.2, color=col, label=lab)
if xz.size:
    ax.plot([1.0], [yz[0]/C], '^', ms=10, color='#55a868', label='bias frames (at S = 0)')
ax.set_xscale('log')
ax.axvline(XMAX, color='#888888', ls=':', lw=1.2)
ax.set_xlim(1, XALL)
ax.set_xlabel('mean signal [ADU]  (dotted line: the left panel ends here)')
ax.set_ylabel('variance / fitted line')
ax.set_title('The same points as a ratio to the fitted line\n'
             '(open symbols: the earlier mixed median/mean pair)', fontsize=9.5)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5, loc='lower right')
fig.suptitle(f'Photon transfer curve, dark and light together — {TAG}', fontsize=11)
fig.tight_layout()
fig.savefig(os.path.join(OUT, 'fig_ptc_both.png'), dpi=130)
print('wrote', os.path.join(OUT, 'fig_ptc_both.png'))

# a number to quote with it
if xd.size and xb.size:
    hi = xd.max()
    j  = int(np.argmax(xd))
    r  = yd[j]/(G*hi + C)
    r0 = md[1][j]/(G*md[0][j] + C)
    print(f'at the top of the dark ladder ({hi:.0f} ADU) the dark variance is '
          f'{100*(r-1):+.1f} % against the bright-ladder line '
          f'({100*(r0-1):+.1f} % with the earlier mixed median/mean pair)')
    # The deficit of the whole dark ladder against the bright line, which the
    # report quotes as the ensemble measurement of the dark deficit. Only the
    # steps inside the PTC fit window count: outside it the bright line itself
    # is an extrapolation, so a ratio there would not be a comparison of the
    # two ladders but of one ladder with an extrapolation.
    inw = (xd >= WIN[0]) & (xd <= WIN[1])
    if not np.any(inw):
        inw = xd > 1
    rat = yd[inw]/(G*xd[inw] + C)
    with open(os.path.join(OUT, 'ptc_both.json'), 'w') as fh:
        json.dump({'Gain': G, 'Intercept': C, 'Window': WIN,
                   'DarkSteps': [int(v) for v in nd[inw]],
                   'DarkSignal': [float(v) for v in xd[inw]],
                   'DarkRatio': [float(v) for v in rat],
                   'DarkDeficit': float(1 - np.mean(rat)),
                   'DarkDeficitTop': float(1 - r),
                   'TopSignal': float(hi)}, fh)
    print(f'dark-ladder deficit against the bright line, inside the window: '
          f'{100*(1-np.mean(rat)):+.1f} %')
