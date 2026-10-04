#!/usr/bin/env python3
"""PTC-gain figures for a single die, from desy_die_ptc.m output.

Everything here is read against a NULL in which every pixel has exactly the
same gain, re-simulated from the step medians and variances in ptc.json. With
3 frames a variance carries 2 degrees of freedom, so the per-pixel estimator
is skewed and ~70 % wide: the null is what tells observed scatter apart from
estimator noise, and a Gaussian deconvolution would not.
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_die/run32_W04_D07_high')
P.add_argument('--out', default=None)
P.add_argument('--seed', type=int, default=11)
A = P.parse_args()
OUT = A.out or A.indir
os.makedirs(OUT, exist_ok=True)

with open(os.path.join(A.indir, 'ptc.json')) as fh:
    S = json.load(fh)
NY, NX = [int(v) for v in S['Size']]
TAG  = f"{S['Lot']} {S['Die']}, run {S['Run']}, {S['GainHalf']} gain"
WIN  = [float(v) for v in S['GainRange']]
GENS, CENS = float(S['GainEnsemble']), float(S['OffsetEnsemble'])
NULL = S['Null']
MEDC = np.atleast_1d(np.array(S['CloudMedian'], dtype=float))
SIDC = np.atleast_1d(np.array(S['CloudSteps'], dtype=int))
MEDF = np.atleast_1d(np.array(S['StepMedian'], dtype=float))
VARF = np.atleast_1d(np.array(S['StepVariance'], dtype=float))
NREP = np.atleast_1d(np.array(S['Nframes'], dtype=float))
DIM  = int(S['ReadoutDim'])
BLK  = int(S['Block'])
rng  = np.random.default_rng(A.seed)

def rd(name, dtype=np.float32, shape=(NY, NX)):
    a = np.fromfile(os.path.join(A.indir, name), dtype=dtype)
    return a.reshape(shape, order='F') if shape else a

GAIN  = rd('gain.bin')
GCOL  = rd('gain_col.bin', shape=None)
NBY, NBX = [int(v) for v in S['Nblock']]
GBLK  = rd('gain_block.bin', shape=(NBY, NBX))
RAWC  = rd('rawcol.bin', np.int32, None)
CLOUD = rd('ptc_cloud.bin', shape=(int(S['CloudN']), 2*len(SIDC)))

def savefig(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=130)
    plt.close(fig)
    print('wrote', os.path.join(OUT, name))

def null_slopes(n):
    """fitted slopes when every pixel has the same gain"""
    sig2 = GENS*MEDF + CENS
    dof  = np.maximum(NREP - 1, 1)
    w    = dof/(2*sig2**2)
    Sw, Swx, Swxx = w.sum(), (w*MEDF).sum(), (w*MEDF**2).sum()
    D = Sw*Swxx - Swx**2
    swy = np.zeros(n); swxy = np.zeros(n)
    for i in range(MEDF.size):
        y = sig2[i]*(rng.standard_normal((n, int(dof[i])))**2).sum(axis=1)/dof[i]
        swy += w[i]*y; swxy += w[i]*y*MEDF[i]
    return (Sw*swxy - Swx*swy)/D

# ------------------------------------------------------------------ 1. the PTC
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.2))
ax = axs[0]
for i in range(len(SIDC)):
    m, v = CLOUD[:, 2*i], CLOUD[:, 2*i+1]
    ax.plot(m[::6], v[::6], '.', ms=1.0, alpha=0.12, color='#4c72b0',
            label='individual pixels' if i == 0 else None)
ax.plot(MEDC, np.array([GENS*x + CENS for x in MEDC]), ':', color='#888888', lw=1.0)
xs = np.linspace(0, WIN[1], 100)
ax.plot(xs, GENS*xs + CENS, 'k--', lw=1.6,
        label=f'fit in {WIN[0]:.0f}-{WIN[1]:.0f} ADU: g = {GENS:.4f} ADU/e-')
ens_v = np.array(S['PerStepThreshold']['Variance'], dtype=float)
ens_m = np.array(S['PerStepThreshold']['Median'], dtype=float)
ax.plot(ens_m, ens_v, 'o', ms=7, mfc='#c44e52', mec='k', mew=0.8, zorder=5,
        label='ensemble (median over 22.5 M pixels)')
ax.axvspan(WIN[0], WIN[1], color='#dd8452', alpha=0.12, label='fit window')
ax.set_xlim(0, 1.05*ens_m.max()); ax.set_ylim(0, 1.05*ens_v.max())
ax.set_xlabel('mean signal [ADU]')
ax.set_ylabel('temporal variance [ADU$^2$]')
ax.set_title('Photon transfer curve', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8, loc='upper left')

ax = axs[1]
res = ens_v - (GENS*ens_m + CENS)
ax.axhline(0, color='k', lw=1.0)
ax.plot(ens_m, res, 'o-', ms=6, color='#c44e52')
ax.axvspan(WIN[0], WIN[1], color='#dd8452', alpha=0.12)
for x, y in zip(ens_m, res):
    ax.annotate(f'{y:+.0f}', (x, y), textcoords='offset points', xytext=(0, 8),
                ha='center', fontsize=7.5)
ax.set_xscale('log')
ax.set_xlabel('mean signal [ADU]')
ax.set_ylabel('variance - fit [ADU$^2$]')
ax.set_title('The curve bends above the window: that is the variance dip, '
             'not a gain change', fontsize=9.5)
ax.grid(alpha=0.25, which='both')
fig.suptitle(f'PTC, {TAG}', fontsize=11)
savefig(fig, 'fig_ptc_curve.png')

# ------------------------------------------------------------------ 2. per-pixel gain vs the null
fig, ax = plt.subplots(figsize=(8.2, 5.6))
fin = np.isfinite(GAIN)
bins = np.linspace(-2, 4, 220)
ax.hist(GAIN[fin], bins=bins, histtype='stepfilled', color='#cccccc',
        edgecolor='#555555', lw=0.7, density=True,
        label=f'measured, {fin.sum()/1e6:.1f} M pixels')
nul = null_slopes(2_000_000)
ax.hist(nul, bins=bins, histtype='step', color='#dd8452', lw=1.9, ls='--',
        density=True, label='null: every pixel the same gain')
ax.axvline(float(NULL['Truth']), color='k', ls=':', lw=1.3,
           label=f"true gain {float(NULL['Truth']):.4f} (= the MEAN of both)")
ax.axvline(float(NULL['Median']), color='#c44e52', ls=':', lw=1.3,
           label=f"median of the estimator {float(NULL['Median']):.4f}, {100*(1-float(NULL['Median'])/float(NULL['Truth'])):.0f} % low")
ax.set_yscale('log')
ax.set_xlim(-2, 4)
ax.set_xlabel('fitted gain of one pixel [ADU/e-]')
ax.set_ylabel('density')
U = S['Unmasked']['All']
ax.set_title(f'Per-pixel gain against the null, {TAG}\nobserved MAD {float(U["GainMAD"]):.4f} vs null '
             f'{float(NULL["MAD"]):.4f} (ratio {float(U["MADoverNull"]):.4f}): the spread is the estimator,\n'
             f'so a single pixel\'s gain differs by at most {100*float(U["IntrFromMAD"])/float(U["GainMean"]):.0f} %',
             fontsize=9.5)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5, loc='upper left')
savefig(fig, 'fig_ptc_gain_null.png')

# ------------------------------------------------------------------ 3. gain per readout column
COL = S['Column']
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.0))
o = np.argsort(RAWC)
ax = axs[0]
ax.plot(RAWC[o], GCOL[o], '-', lw=0.6, color='#4c72b0')
ax.axhline(float(COL['Median']), color='k', ls='--', lw=1.0,
           label=f"median {float(COL['Median']):.4f} ADU/e-")
ax.set_ylim(np.nanpercentile(GCOL, 0.3), np.nanpercentile(GCOL, 99.7))
ax.set_xlabel('raw readout column')
ax.set_ylabel('gain of the column [ADU/e-]')
ax.set_title(f"Averaging {int(S['NpixPerCol'])} pixels first: now the gain is measurable", fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5)

ax = axs[1]
g = GCOL[np.isfinite(GCOL)]
md = float(COL['Median'])
bins = np.linspace(md - 6*float(COL['StdObs']), md + 6*float(COL['StdObs']), 90)
ax.hist(g, bins=bins, histtype='stepfilled', color='#cccccc', edgecolor='#555555',
        lw=0.7, density=True, label=f'{g.size} columns')
xs = np.linspace(bins[0], bins[-1], 300)
sn = float(COL['StdNull'])
ax.plot(xs, np.exp(-0.5*((xs-md)/sn)**2)/(sn*np.sqrt(2*np.pi)), '--', lw=1.9,
        color='#dd8452', label=f'null width {sn:.4f}')
if 'Even' in COL:
    for key, col in (('Even', '#c44e52'), ('Odd', '#4c72b0')):
        ax.axvline(float(COL[key]['Median']), color=col, ls=':', lw=1.5,
                   label=f"{key.lower()} median {float(COL[key]['Median']):.4f}")
ax.set_xlabel('gain of the column [ADU/e-]')
ax.set_ylabel('density')
ax.set_title(f"observed {float(COL['StdObs']):.4f}, null {sn:.4f} -> real column-to-column "
             f"spread {float(COL['StdIntr']):.4f} ({100*float(COL['RelIntr']):.2f} %)", fontsize=9.5)
ax.grid(alpha=0.25)
ax.legend(fontsize=8)
fig.suptitle(f'Gain per readout column, {TAG}', fontsize=11)
savefig(fig, 'fig_ptc_gain_column.png')

# ------------------------------------------------------------------ 4. block gain map
stats = S.get('BlockGain')
fig, ax = plt.subplots(figsize=(7.8, 7.2))
lo, hi = np.nanpercentile(GBLK, [1, 99])
im = ax.imshow(GBLK, origin='lower', cmap='viridis', vmin=lo, vmax=hi,
               extent=[0, NX, 0, NY], interpolation='nearest')
fig.colorbar(im, ax=ax, label='gain [ADU/e-]', shrink=0.85)
ax.set_xlabel('image column')
ax.set_ylabel('image row  (readout column index runs along the rows)')
ttl = f'Gain per {BLK}x{BLK} block, {TAG}'
if stats is not None:
    ttl += (f"\nobserved spread {float(stats['StdObs']):.4f}, null {float(stats['StdNull']):.4f} -> real "
            f"{float(stats['StdIntr']):.4f} ADU/e- ({100*float(stats['RelIntr']):.2f} %)")
ax.set_title(ttl, fontsize=10)
savefig(fig, 'fig_ptc_gain_block.png')

# ------------------------------------------------------------------ 5. threshold from the PTC
PS = S['PerStepThreshold']
fig, ax = plt.subplots(figsize=(8.6, 5.6))
m = np.array(PS['Median'], dtype=float)
t = np.array(PS['ThresholdADU'], dtype=float)
ax.axhline(0, color='k', lw=0.9)
ax.plot(m, t, 'o-', ms=7, lw=1.4, color='#4c72b0', label='T = (Var - RN$^2$)/g - S, per step')
ax.axvspan(WIN[0], WIN[1], color='#dd8452', alpha=0.15, label='PTC fit window')
CL = S['Closure']
ax.axhline(float(CL['ThresholdPTC']), color='#55a868', ls='--', lw=1.5,
           label=f"PTC intercept: {float(CL['ThresholdPTC']):.1f} ADU")
ax.axhline(float(CL['ThresholdADU']), color='#c44e52', ls='--', lw=1.5,
           label=f"{CL['Method']} method (stage 3): {float(CL['ThresholdADU']):.1f} ADU")
ax.set_xscale('log')
ax.set_xlabel('mean signal of the step [ADU]')
ax.set_ylabel('implied charge threshold [ADU]')
ax.set_title(f'What the shot noise says about the threshold, {TAG}\n'
             'a real threshold is the same at every step; the drift above the window is\n'
             'the PTC bending, so an intercept fitted there is not a threshold', fontsize=9.5)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5, loc='lower left')
savefig(fig, 'fig_ptc_threshold.png')
