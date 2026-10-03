#!/usr/bin/env python3
"""Light-ladder figures for a single die, from desy_die_light.m output.

Whole die, per pixel, split by raw readout-column parity. The recurring
comparisons are the same as in stage 2: what the measurement would look like
if every pixel were identical and only the FIT noise spread it, and odd
against even readout columns. The light threshold is the extreme case -- its
fit noise is larger than everything else in the figure.
"""
import argparse, json, os
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_die/run32_W04_D07_high')
P.add_argument('--out', default=None)
P.add_argument('--bin', type=int, default=10)
P.add_argument('--seed', type=int, default=11)
A = P.parse_args()
OUT = A.out or A.indir
os.makedirs(OUT, exist_ok=True)

with open(os.path.join(A.indir, 'light.json')) as fh:
    S = json.load(fh)
DARK = None
dpath = os.path.join(A.indir, 'dark.json')
if os.path.isfile(dpath):
    with open(dpath) as fh:
        DARK = json.load(fh)

NY, NX = [int(v) for v in S['Size']]
TAG = f"{S['Lot']} {S['Die']}, run {S['Run']}, {S['GainHalf']} gain"
NSTEP = len(np.atleast_1d(S['FitSteps']))

def rd(name, dtype=np.float32, shape=(NY, NX)):
    a = np.fromfile(os.path.join(A.indir, name), dtype=dtype)
    return a.reshape(shape, order='F') if shape else a

R     = rd('resp.bin')
TL    = rd('tlight.bin')
CHI2  = rd('bchi2.bin')
RAWC  = rd('rawcol.bin', np.int32, None)
DIM   = int(S['ReadoutDim'])
odd1d = (RAWC % 2) == 1
ODD   = np.repeat(odd1d[:, None], NX, axis=1) if DIM == 1 else np.repeat(odd1d[None, :], NY, axis=0)

FIN  = np.isfinite(R) & np.isfinite(TL)
SETS = (('all pixels',   FIN,         '#333333'),
        ('even columns', FIN & ~ODD,  '#c44e52'),
        ('odd columns',  FIN &  ODD,  '#4c72b0'))
F, PAT, TH, LOC = S['Fit'], S['Pattern'], S['Threshold'], S['Local']
rng = np.random.default_rng(A.seed)

def sp(key, par, field):
    return float(F[key][par][field])

RMED  = sp('All', 'SlopeSpread', 'Median')
RINT  = sp('All', 'SlopeSpread', 'StdIntr')
RFIT  = sp('All', 'SlopeSpread', 'StdFitRobust')
RLOC  = float(LOC['Resp']['RelIntr'])
BLK   = int(LOC['Resp']['Block'])
PRNU  = float(S['PRNU']['Multiplicative'])
TMED, TOBS = float(TH['Median']), float(TH['StdRobust'])
TFIT, TINT = float(TH['StdFitRobust']), float(TH['StdIntr'])
TLOC  = float(LOC['T']['StdIntr'])

def savefig(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, name), dpi=130)
    plt.close(fig)
    print('wrote', os.path.join(OUT, name))

# ------------------------------------------------------------------ 1. response map
B = A.bin
ny, nx = (NY // B) * B, (NX // B) * B
Mb = np.nanmean(R[:ny, :nx].reshape(ny // B, B, nx // B, B), axis=(1, 3))
lo, hi = np.nanpercentile(Mb, [1, 99])
fig, ax = plt.subplots(figsize=(7.8, 7.4))
im = ax.imshow(Mb, origin='lower', cmap='viridis', vmin=lo, vmax=hi,
               extent=[0, NX, 0, NY], interpolation='nearest')
fig.colorbar(im, ax=ax, label='response [ADU per intensity unit]', shrink=0.85)
ax.set_xlabel('image column')
ax.set_ylabel('image row  (readout column index runs along the rows)')
ax.set_title(f'Photo-response per pixel, {TAG}\n{B}x{B} binned; median {RMED:.0f} ADU/int. '
             f'Spread over the die {100*RINT/RMED:.2f} %,\npixel to pixel '
             f'({BLK}x{BLK} blocks detrended) {100*RLOC:.2f} %', fontsize=10)
savefig(fig, 'fig_light_response_map.png')

# ------------------------------------------------------------------ 2. response distribution
fig, axs = plt.subplots(1, 2, figsize=(12.8, 5.2))
xlo, xhi = RMED - 5*RINT, RMED + 5*RINT
bins = np.linspace(xlo, xhi, 170)
ax = axs[0]
ax.hist(R[FIN], bins=bins, histtype='stepfilled', color='#cccccc',
        edgecolor='#555555', lw=0.7, label=f'all pixels ({FIN.sum()/1e6:.1f} M)')
for lab, sel, col in SETS[1:]:
    k = lab.split()[0].capitalize()
    ax.hist(R[sel], bins=bins, histtype='step', color=col, lw=1.3,
            label=f"{lab} (median {sp(k,'SlopeSpread','Median'):.0f})")
nul = RMED + RFIT*rng.standard_normal(min(FIN.sum(), 4_000_000))
ax.hist(nul, bins=bins, weights=np.full(nul.size, FIN.sum()/nul.size),
        histtype='step', color='#dd8452', lw=1.8, ls='--',
        label=f'fit noise alone ({100*RFIT/RMED:.2f} %)')
ax.set_yscale('log')
ax.set_xlim(xlo, xhi)
ax.set_xlabel('per-pixel response [ADU/int]')
ax.set_ylabel('pixels per bin')
ax.set_title('Distribution over the whole die', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8, loc='upper right')

ax = axs[1]
for lab, sel, col in SETS[1:]:
    v = np.sort(R[sel])
    xs = np.linspace(xlo, xhi, 400)
    ax.plot(xs, 100*np.searchsorted(v, xs)/v.size, '-', lw=1.8, color=col, label=lab)
ax.axhline(50, color='k', lw=1.0, ls='--')
de = sp('Odd', 'SlopeSpread', 'Median') - sp('Even', 'SlopeSpread', 'Median')
ax.set_xlim(xlo, xhi)
ax.set_xlabel('per-pixel response [ADU/int]')
ax.set_ylabel('cumulative fraction of pixels [%]')
ax.set_title(f'Odd and even readout columns: odd - even = {de:+.1f} ADU/int '
             f'({100*de/RMED:+.2f} %)', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=9, loc='lower right')
fig.suptitle(f'Photo-response, {TAG} — {NSTEP} steps, weighted per-pixel fit', fontsize=11)
savefig(fig, 'fig_light_response_distribution.png')

# ------------------------------------------------------------------ 3. light threshold
fig, ax = plt.subplots(figsize=(8.0, 5.6))
xlo, xhi = TMED - 4*TOBS, TMED + 4*TOBS
bins = np.linspace(xlo, xhi, 180)
ax.hist(TL[FIN], bins=bins, histtype='stepfilled', color='#cccccc',
        edgecolor='#555555', lw=0.7, label=f'light method, all pixels ({FIN.sum()/1e6:.1f} M)')
nul = TMED + TFIT*rng.standard_normal(min(FIN.sum(), 4_000_000))
ax.hist(nul, bins=bins, weights=np.full(nul.size, FIN.sum()/nul.size),
        histtype='step', color='#dd8452', lw=2.0, ls='--',
        label=f'fit noise alone ({TFIT:.1f} ADU)')
if DARK is not None:
    TD = rd('tdark.bin')
    ax.hist(TD[np.isfinite(TD)], bins=bins, histtype='step', color='#55a868', lw=1.5,
            label=f"dark method (median {-float(DARK['Fit']['All']['InterceptSpread']['Median']):.1f} ADU)")
ax.axvline(TMED, color='k', ls=':', lw=1.2, label=f'light median {TMED:.1f} ADU')
ax.set_yscale('log')
ax.set_xlim(xlo, xhi)
ax.set_xlabel('per-pixel charge threshold [ADU]')
ax.set_ylabel('pixels per bin')
ax.set_title(f'Charge threshold, {TAG}\nlight method: observed spread {TOBS:.1f} ADU is almost all '
             f'fit noise ({TFIT:.1f}), leaving {TINT:.1f} ADU\n(pixel to pixel {TLOC:.1f} ADU); '
             'the dark method is narrower because its lever arm is longer', fontsize=9.5)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5, loc='upper right')
savefig(fig, 'fig_light_threshold.png')

# ------------------------------------------------------------------ 4. PRNU per step
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.0))
med = np.array(PAT['All']['Median'], dtype=float)
SAT  = float(S.get('SatLevel', 15000.0))
SHOW = (med > 1.0) & (med < SAT)        # the saturated steps have no pattern left
ax = axs[0]
for key, lab, col, mk in (('StdObs', 'observed spread', '#333333', 'o'),
                          ('StdNoise', 'temporal noise of the mean', '#dd8452', 's'),
                          ('StdFixed', 'fixed pattern (difference)', '#c44e52', '^')):
    y = np.array(PAT['All'][key], dtype=float)
    ax.plot(med[SHOW], y[SHOW], mk + '-', ms=4.5, lw=1.1, color=col, label=lab)
a = float(S['PRNU']['Additive'])
xs = np.logspace(np.log10(max(med[SHOW].min(), 1)), np.log10(med.max()), 200)
ax.plot(xs, np.hypot(a, PRNU*xs), 'k--', lw=1.4,
        label=f'a + b*S fit: a = {a:.1f} ADU, b = {100*PRNU:.3f} %')
ax.set_xscale('log'); ax.set_yscale('log')
ax.set_xlabel('median bright signal of the step [ADU]')
ax.set_ylabel('spread over the pixels [ADU]')
ax.set_title(f'Per-step spread and its decomposition (below saturation, {SAT:.0f} ADU)',
             fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5, loc='upper left')

ax = axs[1]
for key in ('All', 'Even', 'Odd'):
    if key not in PAT:
        continue
    m = np.array(PAT[key]['Median'], dtype=float)
    r = 100*np.array(PAT[key]['RelFixed'], dtype=float)
    ax.plot(m[SHOW], r[SHOW], 'o-', ms=4, lw=1.1, label=key.lower())
ax.axhline(100*PRNU, color='k', ls='--', lw=1.2, label=f'PRNU {100*PRNU:.3f} % (whole die)')
ax.axhline(100*RLOC, color='#55a868', ls=':', lw=1.6,
           label=f'pixel to pixel {100*RLOC:.2f} %')
ax.set_xscale('log')
ax.set_xlabel('median bright signal of the step [ADU]')
ax.set_ylabel('fixed pattern / signal [%]')
ax.set_ylim(0, max(6.0, 1.5*100*PRNU))
ax.set_title('PRNU as a fraction of the signal', fontsize=10)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5)
fig.suptitle(f'Bright fixed pattern per step, {TAG} — {len(med)} steps', fontsize=11)
savefig(fig, 'fig_light_prnu.png')

# ------------------------------------------------------------------ 5. column profile and pairing
prof = np.array([np.nanmedian(R[i, :] if DIM == 1 else R[:, i]) for i in range(RAWC.size)])
order = np.argsort(RAWC)
rc, pr = RAWC[order], prof[order]
W = 11
trend = np.array([np.nanmedian(pr[max(0, i - W//2):i + W//2 + 1]) for i in range(pr.size)])
res = pr - trend
fig, axs = plt.subplots(1, 2, figsize=(13.0, 5.0))
ax = axs[0]
ax.plot(rc, pr, '-', lw=0.6, color='#4c72b0')
ax.axhline(RMED, color='k', ls='--', lw=1.0, label=f'median {RMED:.0f} ADU/int')
ax.set_ylim(np.nanpercentile(pr, 0.2), np.nanpercentile(pr, 99.8))
ax.set_xlabel('raw readout column')
ax.set_ylabel('median response of the column [ADU/int]')
ax.set_title('Column profile', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5)

ax = axs[1]
odd_i = np.where(rc % 2 == 1)[0]
pairs, cross = [], []
for i in odd_i:
    if i + 1 < rc.size and rc[i + 1] == rc[i] + 1:
        pairs.append((res[i], res[i + 1]))
    if i - 1 >= 0 and rc[i - 1] == rc[i] - 1:
        cross.append((res[i - 1], res[i]))
pairs, cross = np.array(pairs), np.array(cross)
def rho(d):
    d = d[np.all(np.isfinite(d), axis=1)]
    lim = np.nanpercentile(np.abs(d), 99.5)
    d = d[np.all(np.abs(d) < lim, axis=1)]
    return np.corrcoef(d[:, 0], d[:, 1])[0, 1] if d.shape[0] > 2 else np.nan
ax.plot(pairs[:, 0], pairs[:, 1], '.', ms=2.5, color='#c44e52',
        label=f'within pair (2k-1, 2k): r = {rho(pairs):+.3f}')
ax.plot(cross[:, 0], cross[:, 1], '.', ms=2.5, color='#4c72b0',
        label=f'across pairs (2k, 2k+1): r = {rho(cross):+.3f}')
lim = np.nanpercentile(np.abs(res), 99)
ax.set_xlim(-lim, lim);  ax.set_ylim(-lim, lim)
ax.set_xlabel('detrended response of the first column [ADU/int]')
ax.set_ylabel('of the second column [ADU/int]')
ax.set_title('Pairing test, trend removed: read noise gave +0.991 / -0.013', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5)
fig.suptitle(f'Photo-response per readout column, {TAG}', fontsize=11)
savefig(fig, 'fig_light_column_profile.png')

# ------------------------------------------------------------------ 6. goodness of fit
dof = max(NSTEP - 2, 1)
fig, ax = plt.subplots(figsize=(7.4, 5.2))
bins = np.linspace(0, 6, 150)
ok = np.isfinite(CHI2)
sim = (rng.standard_normal((2_000_000, dof))**2).sum(axis=1)/dof
ax.hist(CHI2[ok], bins=bins, histtype='stepfilled', color='#cccccc',
        edgecolor='#555555', lw=0.7, density=True, label=f'all pixels ({ok.sum()/1e6:.1f} M)')
for lab, sel, col in SETS[1:]:
    ax.hist(CHI2[sel & ok], bins=bins, histtype='step', color=col, lw=1.3,
            density=True, label=lab)
ax.hist(sim, bins=bins, histtype='step', color='#dd8452', lw=1.8, ls='--',
        density=True, label=f'chi2/{dof} if the weights were exact')
ax.axvline(float(F['All']['MedianChi2Dof']), color='k', ls=':', lw=1.3,
           label=f"median {float(F['All']['MedianChi2Dof']):.3f}")
ax.set_yscale('log')
ax.set_xlabel('chi2 per degree of freedom of the per-pixel response fit')
ax.set_ylabel('density')
ax.set_title(f'Goodness of fit, {TAG}\n{NSTEP} steps, {dof} dof: the median to compare with is '
             f'{np.median(sim):.3f}, not 1', fontsize=10)
ax.grid(alpha=0.25)
ax.legend(fontsize=8.5)
savefig(fig, 'fig_light_chi2.png')
