#!/usr/bin/env python3
"""Bad-column figures for a single die, from desy_die_badcol.m output.

The three column profiles the flag is based on, the shape of the noisy-column
population (which decides how arbitrary the cut is), and whether the flagged
columns come in the (2k-1, 2k) readout pairs the read noise is paired in.
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

with open(os.path.join(A.indir, 'badcol.json')) as fh:
    S = json.load(fh)
TAG = f"run {S['Run']}, {S['Die']}, {S['GainHalf']} gain"
RC  = np.array(S['RawCol'], dtype=int)
NP  = np.array(S['NoiseProfile'], dtype=float)
RP  = np.array(S['RespProfile'], dtype=float)
DP  = np.array(S['DcProfile'], dtype=float)
BN  = np.array(S['BadNoise'], dtype=bool)
BR  = np.array(S['BadResp'], dtype=bool)
BAD = BN | BR
NM, NS = float(S['NoiseMedian']), float(S['NoiseSigma'])
RM, RS = float(S['RespMedian']), float(S['RespSigma'])
DM, DS = float(S['DcMedian']), float(S['DcSigma'])
NCUT = NM + float(S['NoiseSigmaCut'])*NS
RCUT = RM - float(S['RespSigmaCut'])*RS
o = np.argsort(RC)

def savefig(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=130)
    plt.close(fig)
    print('wrote', os.path.join(OUT, name))

# ------------------------------------------------------------------ 1. the three profiles
fig, axs = plt.subplots(3, 1, figsize=(12.0, 9.6), sharex=True)
for ax, y, cut, lab, ylab, logy in (
        (axs[0], NP, NCUT, 'read noise (stage 1)', 'median read noise [ADU]', True),
        (axs[1], RP, RCUT, 'photo-response (stage 3)', 'median response [ADU/int]', False),
        (axs[2], DP, None, 'dark current (stage 2) -- reported, never masked', 'median dark current [ADU/s]', False)):
    ax.plot(RC[o], y[o], '-', lw=0.6, color='#4c72b0')
    ax.plot(RC[BAD], y[BAD], '.', ms=4, color='#c44e52',
            label=f'{BAD.sum()} flagged columns')
    if cut is not None:
        ax.axhline(cut, color='k', ls='--', lw=1.1, label=f'cut {cut:.4g}')
    ax.set_ylabel(ylab, fontsize=9)
    ax.set_title(lab, fontsize=10, loc='left')
    ax.grid(alpha=0.25)
    if logy:
        ax.set_yscale('log')
    else:
        lo, hi = np.nanpercentile(y, [0.3, 99.7])
        pad = 0.25*(hi - lo)
        ax.set_ylim(min(lo - pad, cut - pad if cut else lo - pad), hi + pad)
    ax.legend(fontsize=8.5, loc='upper right')
axs[2].set_xlabel('raw readout column')
fig.suptitle(f'Column profiles and the bad-column flag, {TAG}', fontsize=11)
savefig(fig, 'fig_badcol_profiles.png')

# ------------------------------------------------------------------ 2. is the cut sharp?
fig, axs = plt.subplots(1, 2, figsize=(12.6, 5.0))
ax = axs[0]
z = (NP - NM)/NS
ax.hist(z, bins=np.linspace(-6, 60, 200), histtype='stepfilled',
        color='#cccccc', edgecolor='#555555', lw=0.7)
ax.axvline(float(S['NoiseSigmaCut']), color='#c44e52', ls='--', lw=1.4,
           label=f"cut at {S['NoiseSigmaCut']:g} sigma")
ax.set_yscale('log')
ax.set_xlabel('read noise of the column, in robust sigmas above the median')
ax.set_ylabel('columns per bin')
ax.set_title('The noisy columns are a smooth tail, not a separate population', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=9)

ax = axs[1]
cs = np.array(S['CutScan']['Sigma'], dtype=float)
nb = np.array(S['CutScan']['Nbad'], dtype=float)
ax.plot(cs, nb, 'o-', lw=1.4, color='#4c72b0')
ax.plot([float(S['NoiseSigmaCut'])], [nb[cs == float(S['NoiseSigmaCut'])]], 'o',
        ms=10, mfc='none', mec='#c44e52', mew=2, label='the cut in use')
for x, y in zip(cs, nb):
    ax.annotate(f'{int(y)}', (x, y), textcoords='offset points', xytext=(5, 6), fontsize=8)
ax.set_xscale('log'); ax.set_yscale('log')
ax.set_xlabel('noise cut [robust sigma]')
ax.set_ylabel('columns flagged')
ax.set_title(f"How many columns the cut costs ({100*(1-float(S['GoodFraction'])):.2f} % of the pixels "
             'at the cut in use)', fontsize=9.5)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=9)
fig.suptitle(f'Where to cut, {TAG}', fontsize=11)
savefig(fig, 'fig_badcol_cut.png')

# ------------------------------------------------------------------ 3. pairing of the defects
fig, axs = plt.subplots(1, 2, figsize=(12.6, 5.0))
partner = RC + np.where(RC % 2 == 1, 1, -1)
pos = {c: i for i, c in enumerate(RC)}
has = np.array([c in pos for c in partner])
pidx = np.array([pos.get(c, 0) for c in partner])
both = has & BAD & BAD[pidx]
ax = axs[0]
ax.plot(NP[has], NP[pidx[has]], '.', ms=2.5, color='#999999', label='every column')
ax.plot(NP[both], NP[pidx[both]], '.', ms=4, color='#c44e52',
        label=f'both flagged ({both.sum()} of {BAD.sum()})')
lim = (0.9*np.nanmin(NP), 1.15*np.nanmax(NP))
ax.plot(lim, lim, 'k--', lw=0.9)
ax.set_xscale('log'); ax.set_yscale('log')
ax.set_xlim(lim); ax.set_ylim(lim)
ax.set_xlabel('read noise of the column [ADU]')
ax.set_ylabel('of its (2k-1, 2k) partner [ADU]')
ax.set_title('A noisy column almost always has a noisy partner', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=9, loc='upper left')

ax = axs[1]
worst = RC[np.argsort(-NP)][:6]
for c in sorted(worst):
    lo, hi = c - 6, c + 6
    sel = (RC >= lo) & (RC <= hi)
    ax.plot(RC[sel] - c, NP[sel], 'o-', ms=3.5, lw=1.0, label=f'around column {c}')
ax.set_yscale('log')
ax.set_xlabel('offset from the worst column of the group')
ax.set_ylabel('median read noise [ADU]')
ax.set_title('Zoom: the defect is two adjacent columns wide', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8)
fig.suptitle(f'Readout-pair structure of the bad columns, {TAG}', fontsize=11)
savefig(fig, 'fig_badcol_pairs.png')
