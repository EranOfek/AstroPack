#!/usr/bin/env python3
"""Dark-ladder figures for a single die, from desy_die_dark.m output.

Whole die, per pixel, split by raw readout-column parity. Two comparisons
recur on every distribution:
  * the curve expected if every pixel were identical and only the FIT noise
    spread the measurements -- what is left over is the real pixel-to-pixel
    variation (the deconvolution the stage reports as StdIntr);
  * odd against even readout columns, the known parity effect.
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_die/run32_W04_D07_high')
P.add_argument('--out', default=None)
P.add_argument('--bin', type=int, default=10, help='binning of the map figure')
P.add_argument('--seed', type=int, default=11)
A = P.parse_args()
OUT = A.out or A.indir
os.makedirs(OUT, exist_ok=True)

with open(os.path.join(A.indir, 'dark.json')) as fh:
    S = json.load(fh)
NY, NX = [int(v) for v in S['Size']]
TAG = f"{S['Lot']} {S['Die']}, run {S['Run']}, {S['GainHalf']} gain"
NSTEP = len(np.atleast_1d(S['FitSteps']))

def rd(name, dtype=np.float32, shape=(NY, NX)):
    a = np.fromfile(os.path.join(A.indir, name), dtype=dtype)
    return a.reshape(shape, order='F') if shape else a

DC    = rd('dc.bin')
TD    = rd('tdark.bin')
CHI2  = rd('chi2.bin')
RAWC  = rd('rawcol.bin', np.int32, None)
DIM   = int(S['ReadoutDim'])
odd1d = (RAWC % 2) == 1
ODD   = np.repeat(odd1d[:, None], NX, axis=1) if DIM == 1 else np.repeat(odd1d[None, :], NY, axis=0)

FIN  = np.isfinite(DC) & np.isfinite(TD)
SETS = (('all pixels',   FIN,         '#333333'),
        ('even columns', FIN & ~ODD,  '#c44e52'),
        ('odd columns',  FIN &  ODD,  '#4c72b0'))
F   = S['Fit']
PAT = S['Pattern']
rng = np.random.default_rng(A.seed)

def sp(key, par, field):
    return float(F[key][par][field])

LOC   = S.get('Local', {})
DCLOC = float(LOC.get('DC', {}).get('RelIntr', float('nan')))
TLOC  = float(LOC.get('T', {}).get('StdIntr', float('nan')))
BLK   = int(LOC.get('DC', {}).get('Block', 32))
DCMED = sp('All', 'SlopeSpread', 'Median')
DCINT = sp('All', 'SlopeSpread', 'StdIntr')
DCFIT = sp('All', 'SlopeSpread', 'StdFitRobust')
TMED  = -sp('All', 'InterceptSpread', 'Median')
TINT  = sp('All', 'InterceptSpread', 'StdIntr')
TFIT  = sp('All', 'InterceptSpread', 'StdFitRobust')

def savefig(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=130)
    plt.close(fig)
    print('wrote', os.path.join(OUT, name))

# ------------------------------------------------------------------ 1. map
B  = A.bin
ny, nx = (NY // B) * B, (NX // B) * B
Mb = np.nanmean(DC[:ny, :nx].reshape(ny // B, B, nx // B, B), axis=(1, 3))
lo, hi = np.nanpercentile(Mb, [1, 99])
fig, ax = plt.subplots(figsize=(7.8, 7.4))
im = ax.imshow(Mb, origin='lower', cmap='viridis', vmin=lo, vmax=hi,
               extent=[0, NX, 0, NY], interpolation='nearest')
fig.colorbar(im, ax=ax, label='dark current [ADU/s]', shrink=0.85)
ax.set_xlabel('image column')
ax.set_ylabel('image row  (readout column index runs along the rows)')
ax.set_title(f'Dark current per pixel, {TAG}\n{B}x{B} binned; median {DCMED:.4f} ADU/s. '
             f'Spread over the die {100*DCINT/DCMED:.1f} %, but that is this structure: '
             f'\npixel to pixel ({BLK}x{BLK} blocks detrended) it is only {100*DCLOC:.2f} %',
             fontsize=10)
savefig(fig, 'fig_dark_dc_map.png')

# ------------------------------------------------------------------ 2. DC distribution
fig, axs = plt.subplots(1, 2, figsize=(12.8, 5.2))
xlo, xhi = max(0.0, DCMED - 5*DCINT), DCMED + 6*DCINT
bins = np.linspace(xlo, xhi, 160)
ax = axs[0]
ax.hist(DC[FIN], bins=bins, histtype='stepfilled', color='#cccccc',
        edgecolor='#555555', lw=0.7, label=f'all pixels ({FIN.sum()/1e6:.1f} M)')
for lab, sel, col in SETS[1:]:
    ax.hist(DC[sel], bins=bins, histtype='step', color=col, lw=1.3,
            label=f"{lab} (median {sp(lab.split()[0].capitalize(), 'SlopeSpread', 'Median'):.4f})")
nul = DCMED + DCFIT*rng.standard_normal(min(FIN.sum(), 4_000_000))
ax.hist(nul, bins=bins, weights=np.full(nul.size, FIN.sum()/nul.size),
        histtype='step', color='#dd8452', lw=1.8, ls='--',
        label=f'fit noise alone ({DCFIT:.4f} ADU/s)')
ax.set_yscale('log')
ax.set_xlim(xlo, xhi)
ax.set_xlabel('per-pixel dark current [ADU/s]')
ax.set_ylabel('pixels per bin')
ax.set_title('Distribution over the whole die', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8, loc='upper right')

ax = axs[1]
for lab, sel, col in SETS[1:]:
    v = np.sort(DC[sel])
    xs = np.linspace(xlo, xhi, 400)
    ax.plot(xs, 100*np.searchsorted(v, xs)/v.size, '-', lw=1.8, color=col, label=lab)
ax.axhline(50, color='k', lw=1.0, ls='--')
de = sp('Odd', 'SlopeSpread', 'Median') - sp('Even', 'SlopeSpread', 'Median')
ax.set_xlim(xlo, xhi)
ax.set_xlabel('per-pixel dark current [ADU/s]')
ax.set_ylabel('cumulative fraction of pixels [%]')
ax.set_title(f'Odd and even readout columns: odd - even = {de:+.5f} ADU/s '
             f'({100*de/DCMED:+.2f} %)', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=9, loc='lower right')
fig.suptitle(f'Dark current, {TAG} — {NSTEP} steps, weighted per-pixel fit', fontsize=11)
savefig(fig, 'fig_dark_dc_distribution.png')

# ------------------------------------------------------------------ 3. threshold
fig, ax = plt.subplots(figsize=(7.6, 5.4))
xlo, xhi = TMED - 5*max(TINT, TFIT), TMED + 5*max(TINT, TFIT)
bins = np.linspace(xlo, xhi, 170)
ax.hist(TD[FIN], bins=bins, histtype='stepfilled', color='#cccccc',
        edgecolor='#555555', lw=0.7, label=f'all pixels ({FIN.sum()/1e6:.1f} M)')
for lab, sel, col in SETS[1:]:
    ax.hist(TD[sel], bins=bins, histtype='step', color=col, lw=1.3,
            label=f"{lab} (median {-sp(lab.split()[0].capitalize(), 'InterceptSpread', 'Median'):.2f} ADU)")
nul = TMED + TFIT*rng.standard_normal(min(FIN.sum(), 4_000_000))
ax.hist(nul, bins=bins, weights=np.full(nul.size, FIN.sum()/nul.size),
        histtype='step', color='#dd8452', lw=1.8, ls='--',
        label=f'fit noise alone ({TFIT:.2f} ADU)')
ax.set_yscale('log')
ax.set_xlim(xlo, xhi)
ax.set_xlabel('per-pixel dark threshold  T = -intercept [ADU]')
ax.set_ylabel('pixels per bin')
ax.set_title(f'Dark threshold, {TAG}\nobserved spread '
             f"{sp('All','InterceptSpread','StdRobust'):.2f} ADU, of which "
             f'{TFIT:.2f} is fit noise, leaving {TINT:.2f} ADU over the die '
             f'and {TLOC:.2f} ADU pixel to pixel', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5, loc='upper right')
savefig(fig, 'fig_dark_threshold.png')

# ------------------------------------------------------------------ 4. column profile and pairing
prof = np.full(RAWC.size, np.nan)
for i in range(RAWC.size):
    v = DC[i, :] if DIM == 1 else DC[:, i]
    prof[i] = np.nanmedian(v)
order = np.argsort(RAWC)
rc, pr = RAWC[order], prof[order]
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.0))
ax = axs[0]
ax.plot(rc, pr, '-', lw=0.6, color='#4c72b0')
ax.axhline(DCMED, color='k', ls='--', lw=1.0, label=f'median {DCMED:.4f} ADU/s')
ax.set_xlabel('raw readout column')
ax.set_ylabel('median dark current of the column [ADU/s]')
ax.set_ylim(np.nanpercentile(pr, 0.2), np.nanpercentile(pr, 99.8))
ax.set_title('Column profile', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5)

ax = axs[1]
# The profile carries a large smooth gradient (0.53 -> 0.25 ADU/s across the
# die), which correlates ANY two neighbouring columns at r ~ 1 whether or not
# they share a readout pair. The pairing question is about what is left after
# that trend: the residual to a running median over 11 columns, which spans
# both parities and so cannot absorb the pairing itself.
W = 11
trend = np.array([np.nanmedian(pr[max(0, i - W//2):i + W//2 + 1]) for i in range(pr.size)])
res = pr - trend
odd_i = np.where(rc % 2 == 1)[0]
pairs, cross = [], []
for i in odd_i:                                   # (2k-1, 2k) share a pair
    if i + 1 < rc.size and rc[i + 1] == rc[i] + 1:
        pairs.append((res[i], res[i + 1]))
    if i - 1 >= 0 and rc[i - 1] == rc[i] - 1:
        cross.append((res[i - 1], res[i]))
pairs, cross = np.array(pairs), np.array(cross)
def rho(d):
    d = d[np.all(np.isfinite(d), axis=1)]
    lim = np.nanpercentile(np.abs(d), 99.5)       # keep the dead columns out
    d = d[np.all(np.abs(d) < lim, axis=1)]
    return np.corrcoef(d[:, 0], d[:, 1])[0, 1] if d.shape[0] > 2 else np.nan
rp, rc2 = rho(pairs), rho(cross)
ax.plot(pairs[:, 0], pairs[:, 1], '.', ms=2.5, color='#c44e52',
        label=f'within pair (2k-1, 2k): r = {rp:+.3f}')
ax.plot(cross[:, 0], cross[:, 1], '.', ms=2.5, color='#4c72b0',
        label=f'across pairs (2k, 2k+1): r = {rc2:+.3f}')
lim = np.nanpercentile(np.abs(res), 99)
ax.set_xlim(-lim, lim);  ax.set_ylim(-lim, lim)
ax.set_xlabel('detrended dark current of the first column [ADU/s]')
ax.set_ylabel('of the second column [ADU/s]')
_rnp = os.path.join(OUT, 'rnplots.json')
if os.path.isfile(_rnp):
    with open(_rnp) as fh:
        _rn = json.load(fh)
    _ttl = f"Pairing test, trend removed: read noise gave {_rn['PairR']:+.3f} / {_rn['CrossR']:+.3f}"
else:
    _ttl = 'Pairing test, trend removed'
ax.set_title(_ttl, fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5)
fig.suptitle(f'Dark current per readout column, {TAG}', fontsize=11)
savefig(fig, 'fig_dark_column_profile.png')
with open(os.path.join(OUT, 'darkplots.json'), 'w') as fh:
    json.dump({'PairR': float(rp), 'CrossR': float(rc2)}, fh)

# ------------------------------------------------------------------ 5. per-step fixed pattern
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.0))
med = np.array(PAT['All']['Median'], dtype=float)
SHOW = med > 1.0                                   # the two lowest steps sit at ~0 ADU
ax = axs[0]
for key, lab, col, mk in (('StdObs', 'observed spread', '#333333', 'o'),
                          ('StdNoise', 'temporal noise of the mean', '#dd8452', 's'),
                          ('StdFixed', 'fixed pattern (difference)', '#c44e52', '^')):
    y = np.array(PAT['All'][key], dtype=float)
    ax.plot(med[SHOW], y[SHOW], mk + '-', ms=5, lw=1.2, color=col, label=lab)
a = float(PAT['All']['Additive'])
b = float(PAT['All']['Multiplicative'])
xs = np.linspace(1.0, med.max(), 200)
ax.plot(xs, np.hypot(a, b*xs), 'k--', lw=1.4,
        label=f'a + b*S fit: a = {a:.2f} ADU, b = {100*b:.2f} %')
ax.set_xscale('log'); ax.set_yscale('log')
ax.set_xlabel('median dark signal of the step [ADU]')
ax.set_ylabel('spread over the pixels [ADU]')
ax.set_title('Per-step spread and its decomposition', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5, loc='upper left')

ax = axs[1]
for key in ('All', 'Even', 'Odd'):
    if key not in PAT:
        continue
    m = np.array(PAT[key]['Median'], dtype=float)
    r = 100*np.array(PAT[key]['RelFixed'], dtype=float)
    ax.plot(m[SHOW], r[SHOW], 'o-', ms=4.5, lw=1.2, label=key.lower())
ax.axhline(100*b, color='k', ls='--', lw=1.2, label=f'asymptote {100*b:.2f} %')
ax.set_xscale('log')
ax.set_xlabel('median dark signal of the step [ADU]')
ax.set_ylabel('fixed pattern / signal [%]')
ax.set_ylim(0, min(60, 1.4*100*b + 25))
ax.set_title('DSNU as a fraction of the signal', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5)
fig.suptitle(f'Dark fixed pattern per step, {TAG}', fontsize=11)
savefig(fig, 'fig_dark_fixed_pattern.png')

# ------------------------------------------------------------------ 6. goodness of fit
dof = max(NSTEP - 2, 1)
fig, ax = plt.subplots(figsize=(7.4, 5.2))
bins = np.linspace(0, 6, 150)
ok = np.isfinite(CHI2)
ax.hist(CHI2[ok], bins=bins, histtype='stepfilled', color='#cccccc',
        edgecolor='#555555', lw=0.7, density=True, label=f'all pixels ({ok.sum()/1e6:.1f} M)')
for lab, sel, col in SETS[1:]:
    ax.hist(CHI2[sel & ok], bins=bins, histtype='step', color=col, lw=1.3,
            density=True, label=lab)
sim = (rng.standard_normal((2_000_000, dof))**2).sum(axis=1)/dof
ax.hist(sim, bins=bins, histtype='step', color='#dd8452', lw=1.8, ls='--',
        density=True, label=f'chi2/{dof} if the weights were exact')
ax.axvline(float(F['All']['MedianChi2Dof']), color='k', ls=':', lw=1.3,
           label=f"median {float(F['All']['MedianChi2Dof']):.2f}")
ax.set_xlabel('chi2 per degree of freedom of the per-pixel dark fit')
ax.set_ylabel('density')
ax.set_title(f'Goodness of fit, {TAG}\n{NSTEP} steps, {dof} dof: the median to compare '
             f'with is {np.median(sim):.2f}, not 1', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5)
savefig(fig, 'fig_dark_chi2.png')
