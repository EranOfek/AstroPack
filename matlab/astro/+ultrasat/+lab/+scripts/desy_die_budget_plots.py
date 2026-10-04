#!/usr/bin/env python3
"""Noise-budget figures for a single die, from desy_die_budget.m output.

Three charge thresholds are carried side by side, because the chain does not
determine which is right and the difference between them is most of the signal
range the test is about. Calibrated curves assume the fixed patterns are
removed by a flat field and a dark frame; raw curves are a single frame.
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_die/run32_W04_D07_high')
P.add_argument('--out', default=None)
P.add_argument('--mask', default='Unmasked', choices=['Unmasked', 'Masked'])
A = P.parse_args()
OUT = A.out or A.indir
os.makedirs(OUT, exist_ok=True)

with open(os.path.join(A.indir, 'budget.json')) as fh:
    B = json.load(fh)
TAG   = f"{B['Lot']} {B['Die']}, run {B['Run']}, {B['GainHalf']} gain"
M     = B[A.mask]
IN    = M['Inputs']
NAMES = list(np.atleast_1d(B['ThresholdNames']))
COL   = {'PTC': '#55a868', 'Dark': '#4c72b0', 'Light': '#c44e52'}
LBL   = {'PTC': 'shot noise (stage 5)', 'Dark': 'dark response (stage 2)',
         'Light': 'light response (stage 3)'}
Q     = np.array(M[NAMES[0]]['Q'], dtype=float)

def savefig(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=130)
    plt.close(fig)
    print('wrote', os.path.join(OUT, name))

# ------------------------------------------------------------------ 1. sigma_eff
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.2))
for ax, key, ttl in ((axs[0], 'SigmaEff_cal', 'Calibrated: fixed patterns removed'),
                     (axs[1], 'SigmaEff_raw', 'Raw: a single frame, patterns left in')):
    for n in NAMES:
        C = M[n]
        ax.plot(Q, np.array(C[key], dtype=float), '-', lw=1.8, color=COL[n],
                label=f"T = {float(C['Threshold_e']):.1f} e-, {LBL[n]}")
    ax.plot(Q, np.sqrt(Q), ':', color='#888888', lw=1.3, label='pure shot noise')
    ax.axhline(float(IN['RN_ADU'])/float(IN['GainADU']), color='k', ls='--', lw=1.0,
               label=f"read noise {float(IN['RN_ADU'])/float(IN['GainADU']):.2f} e-")
    ax.set_xscale('log'); ax.set_yscale('log')
    ax.set_xlabel('incident charge Q [e-]')
    ax.set_ylabel(r'$\sigma_{\rm eff}$ [e-]')
    ax.set_title(ttl, fontsize=10)
    ax.grid(alpha=0.25, which='both')
    ax.legend(fontsize=8, loc='upper left')
fig.suptitle(f'Effective noise per pixel, {TAG} ({A.mask.lower()})', fontsize=11)
savefig(fig, 'fig_budget_sigma.png')

# ------------------------------------------------------------------ 2. SNR
fig, ax = plt.subplots(figsize=(8.6, 5.8))
for n in NAMES:
    C = M[n]
    ax.plot(Q, np.array(C['SNR_cal'], dtype=float), '-', lw=1.9, color=COL[n],
            label=f"T = {float(C['Threshold_e']):.1f} e- ({LBL[n]})")
    ax.plot(Q, np.array(C['SNR_raw'], dtype=float), '--', lw=1.2, color=COL[n], alpha=0.75)
for s, ls in ((5, '-'), (3, ':')):
    ax.axhline(s, color='k', lw=1.0, ls=ls)
    ax.text(Q[0]*1.1, s*1.06, f'SNR {s}', fontsize=8)
for n in NAMES:
    for s in (5, 3):
        q = M[n].get(f'Qlim_cal_{s}')
        if q and np.isfinite(q):
            ax.plot([q], [s], 'o', ms=7, mfc='none', mec=COL[n], mew=1.8)
ax.set_xscale('log'); ax.set_yscale('log')
ax.set_xlim(Q[0], Q[-1]); ax.set_ylim(0.3, 60)
ax.set_xlabel('incident charge Q [e-]')
ax.set_ylabel('signal to noise')
ax.set_title(f'Signal to noise, {TAG}\nsolid = calibrated, dashed = a single raw frame; '
             'circles mark the limiting signal', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5, loc='lower right')
savefig(fig, 'fig_budget_snr.png')

# ------------------------------------------------------------------ 3. what dominates
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.0))
C = M['PTC']
T = C['Terms']
keys = [('RN', 'read noise', '#4c72b0'), ('Shot', 'signal shot', '#dd8452'),
        ('DarkShot', 'dark shot', '#55a868'), ('OffsetFPN', 'offset FPN', '#c44e52'),
        ('DSNU', 'DSNU', '#8172b3'), ('PRNU', 'PRNU', '#937860')]
vals = [np.array(T[k], dtype=float)*np.ones_like(Q) for k, _, _ in keys]
tot  = np.sum(vals, axis=0)
ax = axs[0]
ax.stackplot(Q, [100*v/tot for v in vals], labels=[l for _, l, _ in keys],
             colors=[c for _, _, c in keys], alpha=0.9)
ax.set_xscale('log')
ax.set_xlim(Q[0], Q[-1]); ax.set_ylim(0, 100)
ax.set_xlabel('incident charge Q [e-]')
ax.set_ylabel('share of the variance [%]')
ax.set_title('Uncalibrated budget, PTC threshold', fontsize=10)
ax.legend(fontsize=8, loc='upper right', ncol=2)

ax = axs[1]
for k, l, c in keys:
    ax.plot(Q, np.sqrt(np.array(T[k], dtype=float)*np.ones_like(Q)), '-', lw=1.6, color=c, label=l)
ax.plot(Q, np.array(C['SigmaEff_raw'], dtype=float), 'k-', lw=2.2, label='total (raw)')
ax.plot(Q, np.array(C['SigmaEff_cal'], dtype=float), 'k--', lw=1.6, label='total (calibrated)')
ax.set_xscale('log'); ax.set_yscale('log')
ax.set_xlim(Q[0], Q[-1]); ax.set_ylim(0.05, 60)
ax.set_xlabel('incident charge Q [e-]')
ax.set_ylabel('contribution to $\\sigma_{\\rm eff}$ [e-]')
ax.set_title('The same terms as noise, not variance', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8, loc='upper left', ncol=2)
fig.suptitle(f'Noise budget terms, {TAG} ({A.mask.lower()})', fontsize=11)
savefig(fig, 'fig_budget_terms.png')

# ------------------------------------------------------------------ 4. limiting signal
fig, ax = plt.subplots(figsize=(9.0, 5.4))
x = np.arange(len(NAMES))
w = 0.2
sets = [('Qlim_cal_5', 'SNR 5, calibrated', '#4c72b0'),
        ('Qlim_raw_5', 'SNR 5, raw', '#9fb8d6'),
        ('Qlim_cal_3', 'SNR 3, calibrated', '#c44e52'),
        ('Qlim_raw_3', 'SNR 3, raw', '#e0a3a3')]
for i, (k, l, c) in enumerate(sets):
    v = [float(M[n][k]) for n in NAMES]
    ax.bar(x + (i - 1.5)*w, v, w, color=c, label=l)
    for xi, vi in zip(x + (i - 1.5)*w, v):
        ax.annotate(f'{vi:.0f}', (xi, vi), ha='center', va='bottom', fontsize=7.5)
gl = [abs(float(M[n]['Qlim_cal_gmax']) - float(M[n]['Qlim_cal_gmin'])) for n in NAMES]
ax.errorbar(x - 1.5*w, [float(M[n]['Qlim_cal_5']) for n in NAMES], yerr=gl, fmt='none',
            ecolor='k', capsize=4, lw=1.2, label='gain window systematic')
ax.set_xticks(x)
ax.set_xticklabels([f"{n}\nT = {float(M[n]['Threshold_e']):.1f} e-" for n in NAMES])
ax.set_ylabel('limiting signal [e-]')
sp = [float(M[n]['Qlim_cal_5']) for n in NAMES]
ax.set_title(f'Smallest measurable signal, {TAG} ({A.mask.lower()})\n'
             f'the threshold route alone moves it from {min(sp):.0f} to {max(sp):.0f} e-, '
             'far more than the gain or the column mask', fontsize=10)
ax.grid(alpha=0.25, axis='y')
ax.legend(fontsize=8.5)
savefig(fig, 'fig_budget_limit.png')
