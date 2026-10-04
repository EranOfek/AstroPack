#!/usr/bin/env python3
"""Histograms of the per-pixel PTC fit parameters, from desy_die_ptc_perpixel.m.

Each ladder is fitted separately to every pixel, and every parameter is drawn
against a null in which all pixels are identical, put through the whole
measurement -- integer frames, the mean and variance taken from them, the same
weighted fit. The null is wide and skewed: with three frames a single pixel's
gain is good only to tens of per cent and its median sits below the truth.
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
with open(os.path.join(A.indir, 'ptc_perpixel.json')) as fh:
    S = json.load(fh)
TAG = f"{S['Lot']} {S['Die']}, run {S['Run']}, {S['GainHalf']} gain"
LD  = S['Ladder']
COL = {'D': '#4c72b0', 'B': '#c44e52'}

def savefig(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=130)
    plt.close(fig)
    print('wrote', os.path.join(OUT, name))

def hist(ax, Q, col, lab, xlabel):
    ed = np.array(Q['Edges'], dtype=float)
    c  = 0.5*(ed[1:] + ed[:-1])
    m  = np.array(Q['Counts'], dtype=float);     m = m/max(m.sum(), 1)
    n  = np.array(Q['CountsNull'], dtype=float); n = n/max(n.sum(), 1)
    ax.plot(c, m, '-', lw=1.5, color=col, label=f'{lab} measured')
    ax.plot(c, n, '--', lw=1.5, color='#dd8452', label='null: identical pixels')
    ax.set_xlabel(xlabel)
    ax.set_ylabel('fraction of pixels')
    ax.set_yscale('log')
    ax.set_ylim(max(min(m[m > 0].min(), n[n > 0].min()), 1e-6), 1.6*max(m.max(), n.max()))
    ax.grid(alpha=0.25)
    ax.legend(fontsize=8)

# ---------------------------------------------------------------- 1. slope and intercept
fig, axs = plt.subplots(2, 2, figsize=(13.0, 9.0))
for r, ty in enumerate(('D', 'B')):
    Q = LD[ty]
    hist(axs[r][0], Q['Slope'], COL[ty], Q['Name'], 'fitted slope = gain [ADU/e-]')
    axs[r][0].axvline(float(Q['GainEnsemble']), color='k', ls=':', lw=1.3,
                      label=f"ensemble {float(Q['GainEnsemble']):.4f}")
    axs[r][0].set_title(f"{Q['Name']} ladder: gain, mean {float(Q['Slope']['Mean']):.4f}, "
                        f"median {float(Q['Slope']['Median']):.4f}\n"
                        f"width {float(Q['Slope']['MADoverNull']):.4f} x the null — no pixel-to-pixel "
                        'variation detected', fontsize=9.5)
    axs[r][0].legend(fontsize=8)
    hist(axs[r][1], Q['Inter'], COL[ty], Q['Name'], 'fitted intercept = RN$^2$ + gT [ADU$^2$]')
    axs[r][1].set_title(f"{Q['Name']} ladder: intercept, mean {float(Q['Inter']['Mean']):.2f} ADU$^2$\n"
                        f"width {float(Q['Inter']['MADoverNull']):.4f} x the null", fontsize=9.5)
fig.suptitle(f'Per-pixel PTC fit parameters against the identical-pixel null — {TAG}', fontsize=11)
savefig(fig, 'fig_pp_params.png')

# ---------------------------------------------------------------- 2. joint density
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.4))
for ax, ty in zip(axs, ('D', 'B')):
    Q = LD[ty]
    J = Q['Joint']
    es = np.array(J['EdgesSlope'], dtype=float)
    ei = np.array(J['EdgesInter'], dtype=float)
    H  = np.array(J['Counts'], dtype=float).T
    pc = ax.pcolormesh(es, ei, np.log10(H + 1), cmap='viridis', shading='auto')
    cb = fig.colorbar(pc, ax=ax, pad=0.02)
    cb.set_label('log$_{10}$(pixels per cell + 1)', fontsize=8)
    cb.ax.tick_params(labelsize=7)
    Cv = Q['Cov']
    ax.set_xlabel('slope [ADU/e-]')
    ax.set_ylabel('intercept [ADU$^2$]')
    ax.set_title(f"{Q['Name']} ladder: r = {float(Cv['CorrMeasuredRobust']):+.3f} measured, "
                 f"{float(Cv['CorrNullRobust']):+.3f} null, {float(Cv['CorrAnalytic']):+.3f} from the fit\n"
                 '(robust, on a common central window; log colour scale)', fontsize=9.5)
fig.suptitle(f'Slope against intercept, per pixel — the anti-correlation is the fit\'s own, {TAG}',
             fontsize=11)
savefig(fig, 'fig_pp_joint.png')

# ---------------------------------------------------------------- 3. chi2 and the difference
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.2))
ax = axs[0]
for ty in ('D', 'B'):
    Q = LD[ty]['Chi2']
    ed = np.array(Q['Edges'], dtype=float)
    c  = 0.5*(ed[1:] + ed[:-1])
    m  = np.array(Q['Counts'], dtype=float);     m = m/max(m.sum(), 1)
    n  = np.array(Q['CountsNull'], dtype=float); n = n/max(n.sum(), 1)
    ax.plot(c, m, '-', lw=1.6, color=COL[ty], label=f"{LD[ty]['Name']} measured")
    ax.plot(c, n, ':', lw=1.4, color=COL[ty], label=f"{LD[ty]['Name']} null")
ax.set_yscale('log')
ax.set_xlabel('chi2 per degree of freedom of the per-pixel fit')
ax.set_ylabel('fraction of pixels')
ax.set_title('Goodness of fit against the null', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8)

ax = axs[1]
Q  = LD['Difference']
ed = np.array(Q['Edges'], dtype=float)
c  = 0.5*(ed[1:] + ed[:-1])
m  = np.array(Q['Counts'], dtype=float);     m = m/max(m.sum(), 1)
n  = np.array(Q['CountsNull'], dtype=float); n = n/max(n.sum(), 1)
ax.plot(c, m, '-', lw=1.8, color='#55a868', label='measured')
ax.plot(c, n, '--', lw=1.6, color='#dd8452', label='null: two independent identical pixels')
ax.axvline(0, color='k', lw=1.0)
ax.axvline(float(Q['Mean']), color='#55a868', ls=':', lw=1.4,
           label=f"mean {float(Q['Mean']):+.4f} ADU/e- ({100*float(Q['MeanRel']):+.1f} %)")
ax.set_yscale('log')
ax.set_xlabel('dark gain - bright gain of the SAME pixel [ADU/e-]')
ax.set_ylabel('fraction of pixels')
ax.set_title(f"The deficit, pixel by pixel: width {float(Q['MADoverNull']):.3f} x the null,\n"
             'so every pixel shows it — it is not carried by a subset', fontsize=9.5)
ax.grid(alpha=0.25)
ax.legend(fontsize=8)
fig.suptitle(f'Goodness of fit, and the dark-minus-bright gain of each pixel — {TAG}', fontsize=11)
savefig(fig, 'fig_pp_diff.png')
