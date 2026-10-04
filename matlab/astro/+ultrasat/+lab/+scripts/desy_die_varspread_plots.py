#!/usr/bin/env python3
"""Per-step variance distributions against the identical-pixel null.

For every ladder step the distribution of the per-pixel temporal variance is
drawn against a simulated null in which every pixel has exactly the same true
variance, with the frames rounded to integers as the detector rounds them. The
null is wide on its own -- with 3 frames a variance has 2 degrees of freedom
and a 100 % spread even when the pixels are identical -- so the question is
only how much wider the measurement is.
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

with open(os.path.join(A.indir, 'varspread.json')) as fh:
    S = json.load(fh)
TAG = f"{S['Lot']} {S['Die']}, run {S['Run']}, {S['GainHalf']} gain"
ST  = S['Steps']
if isinstance(ST, dict):
    ST = [ST]

def savefig(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=130)
    plt.close(fig)
    print('wrote', os.path.join(OUT, name))

def panels(sel, title, fname, ncol=5):
    """cumulative distributions: immune to the quantisation comb, so the
    difference between the measurement and the null is actually visible"""
    n = len(sel)
    nrow = int(np.ceil(n/ncol))
    fig, axs = plt.subplots(nrow, ncol, figsize=(3.0*ncol, 2.5*nrow), squeeze=False)
    for ax in axs.ravel()[n:]:
        ax.axis('off')
    for i, E in enumerate(sel):
        ax = axs[i//ncol][i % ncol]
        H  = E['Hist']
        ed = np.array(H['Edges'], dtype=float)
        c  = 0.5*(ed[1:] + ed[:-1])
        meas = np.array(H['Unmasked'], dtype=float)
        null = np.array(H['Null'], dtype=float)
        cm = 100*np.cumsum(meas)/max(meas.sum(), 1)
        cn = 100*np.cumsum(null)/max(null.sum(), 1)
        ax.plot(c, cm, '-', lw=1.5, color='#333333', label='measured')
        ax.plot(c, cn, '--', lw=1.5, color='#dd8452', label='identical pixels')
        ax.set_xlim(0, 4)
        ax.set_ylim(0, 100)
        U = E['Unmasked']
        sig = float(U['Sigma'])
        txt = (f"{E['Type']} step {E['Step']}\n{float(E['Signal']):.0f} ADU\n"
               f"spread {100*float(U['RelIntr']):.0f} %" if sig > 5 else
               f"{E['Type']} step {E['Step']}\n{float(E['Signal']):.0f} ADU\n"
               f"< {100*float(U['RelIntrUL95']):.0f} %")
        if E.get('Saturated'):
            txt += '\nsaturated'
        ax.text(0.97, 0.06, txt, transform=ax.transAxes, ha='right', va='bottom', fontsize=7.5)
        ax.tick_params(labelsize=7)
        if i % ncol == 0:
            ax.set_ylabel('pixels below [%]', fontsize=8)
        if i//ncol == nrow-1:
            ax.set_xlabel('variance / median', fontsize=8)
        if i == 0:
            ax.legend(fontsize=6.5, loc='upper left')
        ax.grid(alpha=0.2)
    fig.suptitle(title, fontsize=11)
    savefig(fig, fname)

def comb(sel, fname):
    """the quantisation, and the fact that the null reproduces it"""
    fig, axs = plt.subplots(1, len(sel), figsize=(6.4*len(sel), 4.6), squeeze=False)
    for ax, E in zip(axs[0], sel):
        H  = E['Hist']
        ed = np.array(H['Edges'], dtype=float)
        c  = 0.5*(ed[1:] + ed[:-1])
        meas = np.array(H['Unmasked'], dtype=float)
        null = np.array(H['Null'], dtype=float)
        meas = meas/max(meas.sum(), 1)
        null = null/max(null.sum(), 1)
        ax.step(c, meas, where='mid', lw=1.1, color='#333333', label='measured')
        ax.step(c, null, where='mid', lw=1.3, color='#dd8452', ls='--',
                label='simulated, identical pixels, integer frames')
        ax.set_xlim(0, 2.2)
        ax.set_yscale('log')
        ax.set_xlabel('variance / median')
        ax.set_ylabel('fraction of pixels')
        ax.set_title(f"{E['Type']} step {E['Step']}, {float(E['Signal']):.0f} ADU, "
                     f"{int(E['Nframes'])} frames\nmedian variance {float(H['Median']):.2f} ADU$^2$",
                     fontsize=10)
        ax.grid(alpha=0.25)
        ax.legend(fontsize=8.5)
    fig.suptitle('A variance built from three integers can only take multiples of 1/18, so both '
                 'distributions are combs —\nthe null reproduces the comb only because its frames '
                 'are rounded the same way', fontsize=10.5)
    savefig(fig, fname)

ZE = [e for e in ST if e['Type'] == 'ZE']
DK = [e for e in ST if e['Type'] == 'D']
BR = [e for e in ST if e['Type'] == 'B']

panels(ZE + DK, f'Per-pixel variance against the identical-pixel null — bias and dark ladder, {TAG}',
       'fig_varspread_dark.png')
panels(BR[:20], f'Per-pixel variance against the identical-pixel null — bright ladder, {TAG}',
       'fig_varspread_light.png')
comb([e for e in (ZE + DK) if e['Step'] in (0, 5)][:2] or (ZE + DK)[:2], 'fig_varspread_comb.png')

# ------------------------------------------------------------------ trend
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.2))
ax = axs[0]
for lab, sel, col, mk in (('bias + dark ladder', ZE + DK, '#4c72b0', 'o'),
                          ('bright ladder', BR, '#c44e52', 's')):
    x, y, ul, xu, yu = [], [], [], [], []
    for e in sel:
        if e.get('Saturated'):
            continue
        sg = max(float(e['Signal']), 0.3)
        U  = e['Unmasked']
        if float(U['Sigma']) > 5:
            x.append(sg); y.append(100*float(U['RelIntr']))
        else:
            xu.append(sg); yu.append(100*float(U['RelIntrUL95']))
    ax.plot(x, y, mk + '-', ms=6, lw=1.3, color=col, label=lab)
    if xu:
        ax.errorbar(xu, yu, yerr=[np.array(yu)*0.35, np.zeros(len(yu))], fmt=mk, ms=5,
                    color=col, mfc='none', uplims=True, lw=1.0,
                    label=f'{lab}: not significant, 95 % upper limit')
ax.set_xscale('log'); ax.set_yscale('log')
ax.set_xlabel('mean signal of the step [ADU]')
ax.set_ylabel('pixel-to-pixel spread of the true variance [%]')
ax.set_title('How much the variance really differs between pixels', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8)

ax = axs[1]
for lab, sel, col, mk in (('bias + dark ladder', ZE + DK, '#4c72b0', 'o'),
                          ('bright ladder', BR, '#c44e52', 's')):
    sel = [e for e in sel if not e.get('Saturated')]
    sg  = [max(float(e['Signal']), 0.3) for e in sel]
    ax.plot(sg, [100*float(e['Unmasked']['Tail10']) for e in sel], mk + '-', ms=6, lw=1.3,
            color=col, label=f'{lab}, measured')
    ax.plot(sg, [100*float(e['Null']['Tail10']) for e in sel], mk + ':', ms=4, lw=1.1,
            color=col, mfc='none', label=f'{lab}, identical pixels')
ax.set_xscale('log'); ax.set_yscale('log')
ax.set_xlabel('mean signal of the step [ADU]')
ax.set_ylabel('pixels above 10 x the median variance [%]')
ax.set_title('The tail: hot pixels at zero signal, cosmic rays on the long darks', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8)
fig.suptitle(f'Pixel-to-pixel variation of the variance, {TAG}', fontsize=11)
savefig(fig, 'fig_varspread_trend.png')
