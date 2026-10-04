#!/usr/bin/env python3
"""The two ladder fits of a single die, with the extrapolation to the intercept.

Every threshold on the response routes is an extrapolation of one of these two
lines to zero signal, so the figure has to show three things at once: which
steps were fitted, the line they define, and how far that line has to reach to
reach x = 0. The residual panel is the same quantity the report tabulates in
section 2, computed the same way, so the figure and the table cannot disagree.

The line drawn is the one the report quotes: the MEDIAN per-pixel slope against
the MEDIAN per-pixel intercept, not a new fit to the plotted medians. The points
are the per-step median signal over all pixels.

usage: desy_die_ladder_plots.py --indir <die dir>
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_die/run32_W04_D07_high')
P.add_argument('--out', default=None)
A = P.parse_args()
OUT = A.out or A.indir
os.makedirs(OUT, exist_ok=True)

def load(name, must=True):
    p = os.path.join(A.indir, name)
    if not os.path.isfile(p):
        if must:
            raise SystemExit(f'{p} missing: run the earlier stages first')
        return None
    with open(p) as fh:
        return json.load(fh)

D  = load('dark.json')
L  = load('light.json')
ME = load('methods.json', must=False)
DW = load('darkwindow.json', must=False)
TAG = f"{D['Lot']} {D['Die']}, run {D['Run']}, {D['GainHalf']} gain"
GAIN = None
if ME is not None:
    GAIN = float(ME['Routes']['d']['Gain'])          # ADU/e-, bright-ladder PTC

def arr(v):
    return np.atleast_1d(np.array(v, dtype=float))

def ladder(S, kind):
    x  = arr(S['PatternX'])
    y  = arr(S['Pattern']['All']['Median'])
    n  = np.atleast_1d(np.array(S['PatternStep'], dtype=int))
    fs = np.atleast_1d(np.array(S['FitSteps'], dtype=int))
    a  = float(S['Fit']['All']['SlopeSpread']['Median'])
    b  = float(S['Fit']['All']['InterceptSpread']['Median'])
    sel = np.isin(n, fs)
    return x, y, n, sel, a, b

def route_err(key):
    '''statistical and systematic error of that route's threshold, in ADU'''
    if ME is None or key not in ME['Routes']:
        return None, None
    Q = ME['Routes'][key]
    return float(Q['ThresholdStat']), float(Q['ThresholdSyst'])

def plot(S, kind):
    x, y, n, sel, a, b = ladder(S, kind)
    # The bright ladder runs to saturation over three decades of intensity, so a
    # plot of all 34 steps shows nothing about the fit. Only the part that is
    # relevant to judging linearity is drawn: up to three times the top of the
    # fitted window, and never above the level where the pattern fit stops.
    top = 3.0*np.nanmax(y[sel])
    if 'PatternFitMax' in S:
        top = min(top, float(S['PatternFitMax']))
    vis = (y <= top) | sel
    if kind == 'dark':
        xl    = 'exposure time [s]'
        title = 'Dark ladder'
        name  = 'fig_dark_ladder_fit.png'
        # T_dark is simply -intercept
        tval  = -b
        tname = 'T$_{dark}$ = -intercept'
        extra = ''
        est, esy = route_err('a')
    else:
        xl    = f"intensity [units of {float(S['IntensityScale']):.0f}]"
        title = 'Bright ladder'
        name  = 'fig_light_ladder_fit.png'
        tval  = -b
        tname = '-intercept'
        # the light route adds the charge the dark current collected during the
        # exposure, which the intercept of a response-against-intensity line
        # cannot know about
        extra = (f"\nT$_{{light}}$ = -intercept + DC$\\cdot t_{{exp}}$ = "
                 f"{float(S['Threshold']['Median']):.2f} ADU")
        est, esy = route_err('b')
    fig, axs = plt.subplots(1, 3, figsize=(16.2, 5.0))

    # ---------------------------------------------------------- 1. whole ladder
    ax = axs[0]
    xf = x[sel]
    xs = np.linspace(0, 1.03*x[vis].max(), 200)
    inw = (xs >= xf.min()) & (xs <= xf.max())
    ax.axvspan(xf.min(), xf.max(), color='#dd8452', alpha=0.12, label='fitted range')
    ax.plot(xs[~inw], a*xs[~inw] + b, 'k--', lw=1.2, label='extrapolated')
    ax.plot(xs[inw],  a*xs[inw]  + b, 'k-',  lw=1.8,
            label=f'fit: {a:.4g}$\\cdot$x {b:+.2f}')
    ax.plot(x[vis & ~sel], y[vis & ~sel], 'o', ms=8, mfc='none', color='#4c72b0',
            label='step not fitted')
    ax.plot(x[vis &  sel], y[vis &  sel], 'o', ms=8, color='#4c72b0', label='step fitted')
    for xi, yi, ni in zip(x[vis], y[vis], n[vis]):
        ax.annotate(f'{ni}', (xi, yi), textcoords='offset points', xytext=(5, -11),
                    fontsize=7, color='#4c72b0')
    ax.axhline(0, color='#888888', lw=0.8)
    ax.set_xlabel(xl); ax.set_ylabel('median signal [ADU]')
    nhide = int((~vis).sum())
    ax.set_title(f'{title}: the window and its surroundings'
                 + (f', {nhide} higher steps not shown' if nhide else ''), fontsize=10)
    ax.grid(alpha=0.25); ax.legend(fontsize=8, loc='upper left')

    # ------------------------------------------- 2. the extrapolation to x = 0
    ax = axs[1]
    # the point of this panel is the GAP between zero and the first fitted step,
    # so it ends in the middle of the window rather than beyond it
    xmax = max(0.5*(xf.min() + xf.max()), 1.2*xf.min())
    xs = np.linspace(0, xmax, 200)
    inw = (xs >= xf.min()) & (xs <= xf.max())
    ax.axvspan(xf.min(), xf.max(), color='#dd8452', alpha=0.12)
    ax.plot(xs[~inw], a*xs[~inw] + b, 'k--', lw=1.2)
    ax.plot(xs[inw],  a*xs[inw]  + b, 'k-',  lw=1.8)
    show = x <= xmax
    ax.plot(x[show & ~sel], y[show & ~sel], 'o', ms=8, mfc='none', color='#4c72b0')
    ax.plot(x[show &  sel], y[show &  sel], 'o', ms=8, color='#4c72b0')
    ax.axhline(0, color='#888888', lw=0.8)
    ax.axvline(0, color='#888888', lw=0.8)
    if est is not None:
        ax.errorbar([0], [b], yerr=[np.hypot(est, esy)], fmt='s', ms=9, color='#c44e52',
                    capsize=4, label='intercept, stat $\\oplus$ syst')
    else:
        ax.plot([0], [b], 's', ms=9, color='#c44e52', label='intercept')
    txt = f'{tname} = {tval:+.2f} ADU'
    if est is not None:
        txt += f' $\\pm$ {est:.2f} $\\pm$ {esy:.2f}'
    if GAIN:
        txt += f'\n= {tval/GAIN:+.1f} e-'
    txt += extra
    ax.annotate(txt, (0.04, 0.96), xycoords='axes fraction', va='top', fontsize=9,
                bbox=dict(fc='white', ec='#cccccc', alpha=0.9))
    ax.set_xlim(-0.04*xmax, xmax)
    ax.set_xlabel(xl); ax.set_ylabel('median signal [ADU]')
    ax.set_title('Extrapolation of the fitted line to zero', fontsize=10)
    ax.grid(alpha=0.25); ax.legend(fontsize=8, loc='lower right')

    # ---------------------------------------------------------- 3. residuals
    ax = axs[2]
    r = y - (a*x + b)
    ax.axvspan(xf.min(), xf.max(), color='#dd8452', alpha=0.12)
    ax.axhline(0, color='k', lw=1.1)
    ax.plot(x[vis & ~sel], r[vis & ~sel], 'o-', ms=8, lw=0.8, mfc='none', color='#4c72b0')
    ax.plot(x[vis &  sel], r[vis &  sel], 'o',  ms=8, color='#4c72b0')
    for xi, ri, ni in zip(x[vis], r[vis], n[vis]):
        ax.annotate(f'{ni}', (xi, ri), textcoords='offset points', xytext=(5, 4),
                    fontsize=7, color='#4c72b0')
    ax.set_xlabel(xl); ax.set_ylabel('median signal - fit [ADU]')
    ax.set_title('Residuals: the ladder bends away from the window', fontsize=10)
    ax.grid(alpha=0.25)

    sub = ''
    if kind == 'dark' and DW is not None:
        sub = (f"  (window chosen by goodness of fit, ratio "
               f"{float(DW['ChosenRatio']):.3f})")
    fig.suptitle(f'{title} fit and its extrapolation to the intercept — {TAG}{sub}',
                 fontsize=11)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=125)
    plt.close(fig)
    print(f'wrote {os.path.join(OUT, name)}: fitted steps '
          f'{np.atleast_1d(np.array(S["FitSteps"], dtype=int)).tolist()}, '
          f'slope {a:.4g}, intercept {b:+.3f} ADU')

plot(D, 'dark')
plot(L, 'light')
