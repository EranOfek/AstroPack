#!/usr/bin/env python3
"""Low-signal variance prediction, from desy_die_lowsignal.m output.

Every pixel's variance is predicted from its own read noise (stage 1) and its
own measured signal, then MEASURED the way the data were -- sampled through a
chi2 with the frames rounded to integers -- so the two sides go through the
same processing. The level comparison is dilution-free; the binned curve is
diluted by the predictor's own noise and is read for its shape only.
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
with open(os.path.join(A.indir, 'lowsignal.json')) as fh:
    S = json.load(fh)
TAG = f"{S['Lot']} {S['Die']}, run {S['Run']}, {S['GainHalf']} gain"
ST  = S['Steps']
if isinstance(ST, dict):
    ST = [ST]
NBY, NBX, NST = [int(v) for v in S['ResidSize']]
RES = np.fromfile(os.path.join(A.indir, 'lowsignal_resid.bin'),
                  dtype=np.float32).reshape((NBY, NBX, NST), order='F')
NX, NY = int(S['Size'][1]), int(S['Size'][0])

def savefig(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=130)
    plt.close(fig)
    print('wrote', os.path.join(OUT, name))

# ------------------------------------------------------------------ 1. level and distributions
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.2))
ax = axs[0]
for lab, ty, col, mk in (('dark ladder', 'D', '#4c72b0', 'o'), ('bright ladder', 'B', '#c44e52', 's')):
    sel = [e for e in ST if e['Type'] == ty]
    ax.plot([max(float(e['Signal']), 0.3) for e in sel],
            [100*float(e['ResidRel']) for e in sel], mk + '-', ms=7, lw=1.4, color=col, label=lab)
ze = [e for e in ST if e['Type'] == 'ZE']
if ze:
    ax.plot([0.3], [100*float(ze[0]['ResidRel'])], '^', ms=9, color='#55a868',
            label='bias (circular, must be 0)')
ax.axhline(0, color='k', lw=1.0)
ax.set_xscale('log')
ax.set_xlabel('signal of the step [ADU]')
ax.set_ylabel('(measured - predicted) / predicted  [%]')
ax.set_title('Is the variance level explained by RN$^2$ + g(S + T)?', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5)

ax = axs[1]
for lab, ty, col, mk in (('dark ladder', 'D', '#4c72b0', 'o'), ('bright ladder', 'B', '#c44e52', 's')):
    sel = [e for e in ST if e['Type'] == ty]
    ax.plot([max(float(e['Signal']), 0.3) for e in sel],
            [float(e['WidthMeas'])/float(e['WidthPred']) for e in sel],
            mk + '-', ms=7, lw=1.4, color=col, label=lab)
ax.axhline(1, color='k', lw=1.0)
ax.set_xscale('log')
ax.set_xlabel('signal of the step [ADU]')
ax.set_ylabel('width measured / width predicted')
ax.set_title('and is its pixel-to-pixel spread explained?\n(above 1 the die is less uniform than '
             'the prediction, below 1 the predictor\'s own noise shows)', fontsize=9.5)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5)
fig.suptitle(f'Predicted against measured per-pixel variance below {int(S["LowMax"])} ADU, {TAG}',
             fontsize=11)
savefig(fig, 'fig_lowsig_level.png')

# ------------------------------------------------------------------ 2. distributions per step
sel = [e for e in ST if not e['Circular']]
ncol = 5
nrow = int(np.ceil(len(sel)/ncol))
fig, axs = plt.subplots(nrow, ncol, figsize=(3.0*ncol, 2.6*nrow), squeeze=False)
for ax in axs.ravel()[len(sel):]:
    ax.axis('off')
for i, E in enumerate(sel):
    ax = axs[i//ncol][i % ncol]
    H  = E['Hist']
    ed = np.array(H['Edges'], dtype=float)
    c  = 0.5*(ed[1:] + ed[:-1])
    m  = np.array(H['Meas'], dtype=float); m = m/max(m.sum(), 1)
    p  = np.array(H['Pred'], dtype=float); p = p/max(p.sum(), 1)
    ax.plot(c, 100*np.cumsum(m), '-', lw=1.5, color='#333333', label='measured')
    ax.plot(c, 100*np.cumsum(p), '--', lw=1.5, color='#dd8452', label='predicted')
    ax.set_xlim(0, 4); ax.set_ylim(0, 100)
    ax.text(0.97, 0.06, f"{E['Type']} step {E['Step']}\n{float(E['Signal']):.0f} ADU\n"
                        f"level {100*float(E['ResidRel']):+.1f} %",
            transform=ax.transAxes, ha='right', va='bottom', fontsize=7.5)
    ax.tick_params(labelsize=7)
    if i % ncol == 0:
        ax.set_ylabel('pixels below [%]', fontsize=8)
    if i//ncol == nrow-1:
        ax.set_xlabel('variance / median', fontsize=8)
    if i == 0:
        ax.legend(fontsize=7, loc='upper left')
    ax.grid(alpha=0.2)
fig.suptitle(f'Measured variance against the measured prediction, step by step — {TAG}', fontsize=11)
savefig(fig, 'fig_lowsig_dist.png')

# ------------------------------------------------------------------ 3. calibration curves
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.2))
for ax, ty, ttl in ((axs[0], 'D', 'dark ladder'), (axs[1], 'B', 'bright ladder')):
    sel = [e for e in ST if e['Type'] == ty]
    cm = plt.get_cmap('viridis')
    for j, E in enumerate(sel):
        x = np.array(E['Calib']['X'], dtype=float)
        y = np.array(E['Calib']['Y'], dtype=float)
        ok = np.isfinite(x) & np.isfinite(y)
        ax.plot(x[ok], y[ok], '-', lw=1.4, color=cm(j/max(len(sel)-1, 1)),
                label=f"step {E['Step']}, {float(E['Signal']):.0f} ADU")
    lim = ax.get_xlim()
    ax.plot(lim, lim, 'k--', lw=1.1, label='measured = predicted')
    ax.set_xlabel('median predicted variance in the bin [ADU$^2$]')
    ax.set_ylabel('median measured variance [ADU$^2$]')
    ax.set_title(f'{ttl} (the slope is diluted by the predictor\'s noise;\nread the shape, not the value)',
                 fontsize=9.5)
    ax.grid(alpha=0.25)
    ax.legend(fontsize=7, ncol=2)
fig.suptitle(f'Binned calibration, {TAG}', fontsize=11)
savefig(fig, 'fig_lowsig_calib.png')

# ------------------------------------------------------------------ 4. residual maps
sel = [(i, e) for i, e in enumerate(ST) if not e['Circular']]
ncol = 5
nrow = int(np.ceil(len(sel)/ncol))
fig, axs = plt.subplots(nrow, ncol, figsize=(2.9*ncol, 2.9*nrow), squeeze=False)
for ax in axs.ravel()[len(sel):]:
    ax.axis('off')
for k, (i, E) in enumerate(sel):
    ax = axs[k//ncol][k % ncol]
    R = 100*RES[:, :, i]
    lo, hi = np.nanpercentile(R, [2, 98])
    v = max(abs(lo), abs(hi))
    im = ax.imshow(R, origin='lower', cmap='RdBu_r', vmin=-v, vmax=v,
                   extent=[0, NX, 0, NY], interpolation='nearest')
    fig.colorbar(im, ax=ax, fraction=0.046, pad=0.03)
    ax.set_title(f"{E['Type']} step {E['Step']}, {float(E['Signal']):.0f} ADU", fontsize=9)
    ax.tick_params(labelsize=6)
fig.suptitle(f'Where the prediction fails: (measured - predicted)/predicted [%], '
             f'{int(S["Block"])}x{int(S["Block"])} blocks — {TAG}', fontsize=11)
savefig(fig, 'fig_lowsig_resid.png')
