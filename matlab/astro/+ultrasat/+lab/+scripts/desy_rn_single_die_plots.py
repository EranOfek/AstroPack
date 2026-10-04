#!/usr/bin/env python3
"""Read-noise figures for a single die, from desy_rn_single_die.m output.

All pixels of the die, split by raw readout-column parity. The key comparison
on every distribution is the curve expected if every pixel had the SAME read
noise: with 5 bias frames a per-pixel sigma has 4 degrees of freedom and about
50 % sampling scatter, so most of the observed width is sampling, not real
pixel-to-pixel variation.
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_rn/run32_W04_D07_high')
P.add_argument('--out', default=None)
P.add_argument('--seed', type=int, default=11)
A = P.parse_args()
OUT = A.out or A.indir
os.makedirs(OUT, exist_ok=True)

with open(os.path.join(A.indir, 'stats.json')) as fh:
    S = json.load(fh)
NY, NX = [int(v) for v in S['Size']]
TAG = f"{S['Lot']} {S['Die']}, run {S['Run']}, {S['GainHalf']} gain"

def rd(name, dtype=np.float32, shape=(NY, NX)):
    a = np.fromfile(os.path.join(A.indir, name), dtype=dtype)
    return a.reshape(shape, order='F') if shape else a

RN   = rd('rn.bin')
BIAS = rd('bias.bin')
RAWC = rd('rawcol.bin', np.int32, None)

# parity: in the DESY orientation the readout column index runs along the rows
DIM = int(S['ReadoutDim'])
odd1d = (RAWC % 2) == 1
ODD = np.repeat(odd1d[:, None], NX, axis=1) if DIM == 1 else np.repeat(odd1d[None, :], NY, axis=0)

FIN = np.isfinite(RN) & (RN > 0)
SETS = (('all pixels',  FIN,          '#333333'),
        ('even columns', FIN & ~ODD,  '#c44e52'),
        ('odd columns',  FIN &  ODD,  '#4c72b0'))

def stat(k, field, default=np.nan):
    return float(S.get(k, {}).get(field, default))

MED = {'all pixels': stat('All', 'ReadNoiseMedian'),
       'even columns': stat('Even', 'ReadNoiseMedian'),
       'odd columns': stat('Odd', 'ReadNoiseMedian')}
RMS = stat('All', 'ReadNoiseRMS')
DOF = int(S['Dof'])

rng = np.random.default_rng(A.seed)
def null_sigma(n, sigma0, dof):
    """sigma of n identical pixels measured with dof degrees of freedom"""
    chi2 = (rng.standard_normal((n, dof))**2).sum(axis=1)
    return sigma0*np.sqrt(chi2/dof)

# ------------------------------------------------------------------ 1. distribution
# The bias frames are integers, so with Nf frames the sample variance can only
# take multiples of 1/(Nf*Dof) and sigma only the roots of those. Near the peak
# the allowed values are ~0.013 ADU apart AND carry very different
# multiplicities, so a histogram of sigma is intrinsically combed however the
# bins are chosen. The parity comparison is therefore made on the cumulative
# distribution, which is immune to binning.
QUANT = 1.0/(2.0*MED['all pixels']*S['Nframes']*DOF)
BW = 0.10
XHI = 6.0
bins = np.arange(0.0, XHI + BW, BW)
beyond = 100.0*np.mean(RN[FIN] > XHI)
fig, axs = plt.subplots(1, 2, figsize=(12.8, 5.2))

ax = axs[0]
ax.hist(RN[FIN], bins=bins, histtype='stepfilled', color='#cccccc',
        edgecolor='#555555', lw=0.7, label=f"all pixels ({FIN.sum()/1e6:.1f} M)")
for lab, sel, col in SETS[1:]:
    ax.hist(RN[sel], bins=bins, histtype='step', color=col, lw=1.3,
            label=f'{lab} (median {MED[lab]:.4f} ADU)')
nul = null_sigma(min(FIN.sum(), 4_000_000), RMS, DOF)
ax.hist(nul, bins=bins, weights=np.full(nul.size, FIN.sum()/nul.size),
        histtype='step', color='#dd8452', lw=1.8, ls='--',
        label=f'if every pixel had sigma = {RMS:.3f} ADU')
ax.axvline(MED['all pixels'], color='k', ls='--', lw=1.1,
           label=f"median {MED['all pixels']:.3f} ADU")
ax.set_yscale('log')
ax.set_xlim(0, XHI)
ax.set_xlabel('per-pixel read noise [ADU]')
ax.set_ylabel(f'pixels per {BW:.2f} ADU bin')
ax.set_title(f'Distribution over the whole die ({beyond:.1f} % beyond the axis)', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=7.5, loc='upper right')

ax = axs[1]
zlo, zhi = 1.2, 2.8
for lab, sel, col in SETS[1:]:
    v = np.sort(RN[sel])
    xs = np.linspace(zlo, zhi, 400)
    cdf = np.searchsorted(v, xs)/v.size
    ax.plot(xs, 100*cdf, '-', lw=1.8, color=col,
            label=f'{lab}: median {MED[lab]:.4f} ADU')
    ax.plot([MED[lab], MED[lab]], [0, 50], ':', lw=1.0, color=col)
ax.axhline(50, color='k', lw=1.0, ls='--')
d = MED['odd columns'] - MED['even columns']
rel = 100*d/MED['even columns'] if MED['even columns'] else np.nan
ax.annotate('', xy=(MED['odd columns'], 50), xytext=(MED['even columns'], 50),
            arrowprops=dict(arrowstyle='<->', color='k', lw=1.3))
ax.text(0.5*(MED['even columns']+MED['odd columns']), 53,
        f'{d:+.4f} ADU  ({rel:+.2f} %)', ha='center', fontsize=9)
ax.set_xlim(zlo, zhi)
ax.set_xlabel('per-pixel read noise [ADU]')
ax.set_ylabel('cumulative fraction of pixels [%]')
ax.set_title('Odd and even readout columns, cumulative (immune to the quantisation)',
             fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5, loc='lower right')
fig.suptitle(f'Read noise, {TAG} — {S["Nframes"]} bias frames, {DOF} dof per pixel; '
             f'the comb in the left panel is the {QUANT:.3f} ADU quantisation of sigma, '
             f'not structure in the detector', fontsize=10)
fig.tight_layout()
fig.savefig(os.path.join(OUT, 'fig_rn_distribution.png'), dpi=110)
plt.close(fig)

# ------------------------------------------------------------------ 2. map
def block_median(a, f):
    ny, nx = a.shape
    ny2, nx2 = (ny//f)*f, (nx//f)*f
    b = a[:ny2, :nx2].reshape(ny2//f, f, nx2//f, f)
    return np.nanmedian(b, axis=(1, 3))

f = max(1, int(round(max(NY, NX)/1200)))
img = block_median(np.where(FIN, RN, np.nan), f)
lo, hi2 = np.nanpercentile(img, [1, 99])
fig, ax = plt.subplots(figsize=(8.2, 7.2))
im = ax.imshow(img, origin='lower', cmap='viridis', norm=Normalize(lo, hi2),
               interpolation='nearest')
ax.set_xlabel(f'image column (binned {f}x)')
ax.set_ylabel(f'image row = readout column (binned {f}x)')
ax.set_title(f'Read-noise map, {TAG}\n(block median {f}x{f}; the odd/even alternation '
             f'runs along the vertical axis and is averaged out by the binning)', fontsize=9)
cb = fig.colorbar(im, ax=ax, fraction=0.046)
cb.set_label('read noise [ADU]')
fig.tight_layout()
fig.savefig(os.path.join(OUT, 'fig_rn_map.png'), dpi=110)
plt.close(fig)

# ------------------------------------------------------------------ 3. column profile
axis = 1 if DIM == 1 else 0
prof = np.nanmedian(np.where(FIN, RN, np.nan), axis=axis)
col = RAWC.astype(float)
fig, axs = plt.subplots(2, 1, figsize=(12.0, 6.6))
ax = axs[0]
ax.plot(col, prof, '-', lw=0.6, color='#333333')
for lab, c, ls in (('even columns', '#c44e52', '--'), ('odd columns', '#4c72b0', ':')):
    ax.axhline(MED[lab], color=c, ls=ls, lw=1.2, label=f'{lab} median {MED[lab]:.3f}')
hiy = np.nanpercentile(prof, 99.5)
nout = int(np.sum(prof > hiy))
ax.set_ylim(np.nanpercentile(prof, 0.3), hiy)
ax.text(0.99, 0.95, f'{nout} columns above the axis', transform=ax.transAxes,
        ha='right', va='top', fontsize=8, color='#666')
ax.set_xlabel('raw readout column')
ax.set_ylabel('median read noise [ADU]')
ax.set_title('Median read noise per readout column, whole die', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8)

ax = axs[1]
c0 = int(len(col)//2)
sl = slice(c0, c0+60)
ax.plot(col[sl], prof[sl], 'o-', ms=3.5, lw=1.0, color='#333333')
ev = ~odd1d[sl]
ax.plot(col[sl][ev], prof[sl][ev], 'o', ms=5, color='#c44e52', label='even')
ax.plot(col[sl][~ev], prof[sl][~ev], 'o', ms=5, color='#4c72b0', label='odd')
ax.set_xlabel('raw readout column')
ax.set_ylabel('median RN [ADU]')
ax.set_title('Zoom on 60 consecutive columns: the odd/even alternation', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8)
fig.tight_layout()
fig.savefig(os.path.join(OUT, 'fig_rn_column_profile.png'), dpi=110)
plt.close(fig)

# ------------------------------------------------------------------ 3b. column pairing
# The readout columns turn out to be organised in (odd, even) PAIRS whose noise
# amplitude is shared, which is a stronger statement than an odd/even offset.
oo = np.argsort(RAWC)
pp = prof[oo]
cc = RAWC[oo]
A1, B1 = pp[0::2], pp[1::2]                      # columns 2k-1 and 2k, the pair
n = min(A1.size, B1.size)
A1, B1 = A1[:n], B1[:n]
A2, B2 = pp[1::2][:-1], pp[2::2]                 # columns 2k and 2k+1, across pairs
n2 = min(A2.size, B2.size)
A2, B2 = A2[:n2], B2[:n2]

def rof(a, b):
    m = np.isfinite(a) & np.isfinite(b)
    return np.corrcoef(a[m], b[m])[0, 1], m.sum()

r1, n1 = rof(A1, B1)
r2, nn2 = rof(A2, B2)
lim = (np.nanpercentile(pp, 0.3), np.nanpercentile(pp, 99.7))
fig, axs = plt.subplots(1, 2, figsize=(11.6, 5.4), sharex=True, sharey=True)
for ax, (a, b, r, nn, ttl, xl, yl) in zip(axs, (
        (A1, B1, r1, n1, 'within a pair: columns 2k-1 and 2k',
         'median RN of column 2k-1 (odd) [ADU]', 'median RN of column 2k (even) [ADU]'),
        (A2, B2, r2, nn2, 'across pairs: columns 2k and 2k+1',
         'median RN of column 2k (even) [ADU]', 'median RN of column 2k+1 (odd) [ADU]'))):
    ax.plot(a, b, '.', ms=2.2, alpha=0.45, color='#4c72b0')
    ax.plot(lim, lim, '-', lw=1, color='#c44e52', label='1:1')
    ax.set_xscale('log'); ax.set_yscale('log')
    ax.set_xlim(*lim); ax.set_ylim(*lim)
    ax.set_xlabel(xl); ax.set_ylabel(yl)
    ax.set_title(f'{ttl}\nr = {r:.3f}  ({nn} pairs)', fontsize=10)
    ax.grid(alpha=0.25, which='both')
    ax.legend(fontsize=8, loc='upper left')
fig.suptitle(f'Read-noise amplitude is shared within each (odd, even) column pair, {TAG}',
             fontsize=10)
fig.tight_layout()
fig.savefig(os.path.join(OUT, 'fig_rn_column_pairing.png'), dpi=110)
plt.close(fig)
print(f'  column pairing: r(within pair) = {r1:.3f}, r(across pairs) = {r2:.3f}')

# ------------------------------------------------------------------ 4. bias
BFIN = np.isfinite(BIAS)
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.4))
bimg = block_median(np.where(BFIN, BIAS, np.nan), f)
blo, bhi = np.nanpercentile(bimg, [1, 99])
im = axs[0].imshow(bimg, origin='lower', cmap='magma', norm=Normalize(blo, bhi),
                   interpolation='nearest')
axs[0].set_xlabel(f'image column (binned {f}x)')
axs[0].set_ylabel(f'image row = readout column (binned {f}x)')
axs[0].set_title('Bias map', fontsize=10)
cb = fig.colorbar(im, ax=axs[0], fraction=0.046)
cb.set_label('bias [ADU]')
ax = axs[1]
blev = stat('All', 'BiasLevel')
# the bias is the mean of Nf integer frames, so it only takes multiples of
# 1/Nf ADU: bins must sit on that grid or the histogram combs
QB = 1.0/S['Nframes']
bb = np.arange(np.floor((blev - 8)/QB)*QB - QB/2, blev + 8 + QB, QB)
for lab, sel, col_ in SETS:
    ax.hist(BIAS[sel & BFIN], bins=bb, histtype='step', color=col_, lw=1.4,
            label=f"{lab}: median {stat(lab.split()[0].capitalize() if lab != 'all pixels' else 'All', 'BiasLevel'):.2f}, "
                  f"FPN {stat(lab.split()[0].capitalize() if lab != 'all pixels' else 'All', 'FixedPatternRMS'):.3f} ADU")
ax.set_yscale('log')
ax.set_xlabel('bias level [ADU]')
ax.set_ylabel('pixels per bin')
ax.set_title(f'Bias distribution ({QB:.2f} ADU bins = the quantisation of a '
             f'{S["Nframes"]}-frame mean); FPN has its own sampling noise removed', fontsize=9.5)
ax.grid(alpha=0.25)
ax.legend(fontsize=8)
fig.suptitle(f'Bias frame, {TAG}', fontsize=10)
fig.tight_layout()
fig.savefig(os.path.join(OUT, 'fig_bias.png'), dpi=110)
plt.close(fig)

# ------------------------------------------------------------------ 5. tail
fig, axs = plt.subplots(1, 2, figsize=(12.6, 5.0))
ax = axs[0]
m = MED['all pixels']
xs = np.linspace(0.5, 6, 240)
for lab, sel, col_ in SETS:
    v = np.sort(RN[sel])
    frac = 1.0 - np.searchsorted(v, xs*m)/v.size
    ax.plot(xs, np.maximum(frac, 1e-9), '-', lw=1.5, color=col_, label=lab)
nul = null_sigma(3_000_000, RMS, DOF)
nv = np.sort(nul)
ax.plot(xs, np.maximum(1.0 - np.searchsorted(nv, xs*m)/nv.size, 1e-9), '--', lw=1.5,
        color='#dd8452', label='identical pixels')
ax.set_yscale('log')
ax.set_xlabel('read noise / median')
ax.set_ylabel('fraction of pixels above')
ax.set_title('Tail of the read-noise distribution', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8)

ax = axs[1]
ks = [1.5, 2, 3, 5, 10]
wdt = 0.26
for i, (lab, sel, col_) in enumerate(SETS):
    v = np.sort(RN[sel])
    fr = [100*(1.0 - np.searchsorted(v, k*m)/v.size) for k in ks]
    ax.bar(np.arange(len(ks)) + (i-1)*wdt, fr, wdt, color=col_, label=lab)
nv = np.sort(null_sigma(3_000_000, RMS, DOF))
frn = [100*(1.0 - np.searchsorted(nv, k*m)/nv.size) for k in ks]
ax.plot(np.arange(len(ks)), frn, 'kx--', ms=8, lw=1.2, label='identical pixels')
ax.set_xticks(np.arange(len(ks)))
ax.set_xticklabels([f'>{k}x' for k in ks])
ax.set_yscale('log')
ax.set_ylabel('fraction of pixels [%]')
ax.set_title('Noisy-pixel fractions', fontsize=10)
ax.grid(alpha=0.25, axis='y', which='both')
ax.legend(fontsize=8)
fig.suptitle(f'Noisy pixels, {TAG}', fontsize=10)
fig.tight_layout()
fig.savefig(os.path.join(OUT, 'fig_rn_tail.png'), dpi=110)
plt.close(fig)

print(f'{TAG}: {FIN.sum()/1e6:.2f} M pixels')
for lab in ('all pixels', 'even columns', 'odd columns'):
    k = 'All' if lab == 'all pixels' else lab.split()[0].capitalize()
    print(f'  {lab:14s} median {MED[lab]:.4f}  rms {stat(k,"ReadNoiseRMS"):.4f}  '
          f'robust {stat(k,"ReadNoiseRobust"):.4f}  tail>2x {100*stat(k,"TailFrac"):.2f} %  '
          f'intrinsic spread {100*stat(k,"SpreadSigmaRel"):.1f} %')
print('6 figures ->', OUT)
