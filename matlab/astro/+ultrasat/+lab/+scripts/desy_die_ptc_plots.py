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

# ------------------------------------------------------------------ 5. intercept from the PTC
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
ax.set_ylabel('implied charge intercept [ADU]')
ax.set_title(f'What the shot noise says about the intercept, {TAG}\n'
             'a real threshold is the same at every step; the drift above the window is\n'
             'the PTC bending, so an intercept fitted there is not a threshold', fontsize=9.5)
ax.grid(alpha=0.25, which='both')
ax.legend(fontsize=8.5, loc='lower left')
savefig(fig, 'fig_ptc_threshold.png')

# ------------------------------------------------------------------ 6. is it straight once the
#                                                                        intercept is added back?
# The photon-transfer fit says Var - RN^2 = g*(S + T): a straight line whose
# intercept is a charge T the signal appears to be missing at zero. If that
# description were complete, then crediting the device with T would make the
# relation proportional, and (Var - RN^2)/(S + T) would be the SAME number -- the
# gain -- at every step. Plotting it is therefore a direct test of the model, and
# of where it stops working, which on these devices is at the bottom.
#   Why not (F - G)/G with F = G + intercept: F - G is the intercept, a constant,
#   so that ratio is intercept/G and falls like 1/signal on any device whatever,
#   straight or bent. It measures the SIZE of the correction, not its success.
#   Why normalised rather than the absolute T of panel 5: T = G/g - S is in ADU,
#   so an error in the global gain tilts the whole curve and is read as curvature.
#   Here a gain error shifts the curve bodily and the shape is the nonlinearity.
_PP = os.path.join(A.indir, 'ptc_points.json')
_MJ = os.path.join(A.indir, 'methods.json')
if os.path.isfile(_PP) and os.path.isfile(_MJ):
    with open(_PP) as fh:
        _pts = json.load(fh)['Points']
    with open(_MJ) as fh:
        _me = json.load(fh)
    _rn2 = float(_me['RN2'])              # the same RN^2 the routes subtracted
    _lad = [('B', 'd', 'bright ladder', '#c44e52'), ('D', 'c', 'dark ladder', '#4c72b0')]
    _lad = [t for t in _lad if t[1] in _me['Routes'] and
            any(q['Type'] == t[0] for q in _pts)]
    if _lad:
        # Fit the response model to each ladder, so the panel can carry the two
        # competing pictures at once: a constant deficit that leaves the noise
        # alone (the flat line) and a smooth response whose noise follows the
        # slope (the curve). The fit is written beside the other dumps and the
        # report and the campaign note read it rather than refitting.
        import desy_nonlin as _NL
        _FIT = {}
        for _ty, _rt, _name, _col in _lad:
            _q = sorted([p for p in _pts if p['Type'] == _ty],
                        key=lambda p: float(p['SignalMean']))
            if not _q:
                continue
            _mm = np.array([float(p['SignalMean']) for p in _q])
            _vv = np.array([float(p['VarMean']) for p in _q]) - _rn2
            _f = _NL.fit(_mm, _vv, float(_me['Routes'][_rt]['Gain']))
            if _f is not None:
                _f['Ladder'] = _name
                _FIT[_ty] = _f
        with open(os.path.join(OUT, 'nonlin_fit.json'), 'w') as _fh:
            json.dump({'Tag': _me.get('Tag', ''), 'Model': 'Q_out = Q*exp(-D/Q)',
                       'Ladders': _FIT}, _fh)
        _nr = 2 if _FIT else 1
        fig, axs = plt.subplots(_nr, len(_lad), figsize=(7.2*len(_lad), 5.4*_nr),
                                squeeze=False)
        fig.subplots_adjust(wspace=0.42)
        for _i, (_ty, _rt, _name, _col) in enumerate(_lad):
            ax = axs[0][_i]
            _ff = _FIT.get(_ty)
            _R  = _me['Routes'][_rt]
            _g  = float(_R['Gain'])
            _T  = float(_R['Threshold'])
            _w  = set(int(v) for v in np.atleast_1d(np.array(_me['Windows'][_rt][0], dtype=int)).tolist())
            _q  = sorted([p for p in _pts if p['Type'] == _ty], key=lambda p: float(p['SignalMean']))
            _m  = np.array([float(p['SignalMean']) for p in _q])
            _v  = np.array([float(p['VarMean']) for p in _q]) - _rn2
            _in = np.array([int(p['Step']) in _w for p in _q])
            # Two kinds of step carry no information here and are left out rather
            # than drawn: one whose mean signal does not reach the read noise, so
            # that it is not distinguishable from zero exposure at all, and one
            # below the line's own zero crossing, where the ratio changes sign.
            _ok = (_m > np.sqrt(_rn2)) & ((_m + _T) > 0.2*max(abs(_T), 1.0))
            _y  = np.full(_m.shape, np.nan)
            _y[_ok] = _v[_ok]/(_m[_ok] + _T)
            ax.axhline(_g, color='#55a868', ls='--', lw=1.5,
                       label=f'fitted gain {_g:.4f} ADU/e$^-$')
            if _in.any():
                ax.axvspan(_m[_in].min(), _m[_in].max(), color='#dd8452', alpha=0.15,
                           label='fit window')
            # The frame is held to -45..+12 % of the gain: that is the range in
            # which the departure can still be read off, and the lowest steps of
            # a ladder run far past it. They are not dropped -- a reader must see
            # that they exist and which way they go -- but clamped to the edge
            # with a marker that says the value is below the frame.
            _ylo, _yhi = _g*0.55, _g*1.12
            _lowf = _ok & np.isfinite(_y) & (_y < _ylo)
            _show = _ok & np.isfinite(_y) & ~_lowf
            ax.plot(_m[~_in & _show], _y[~_in & _show], 'o', ms=6, mfc='none', color=_col,
                    label='outside the window')
            ax.plot(_m[_in & _show], _y[_in & _show], 'o-', ms=7, lw=1.3, color=_col,
                    label='fitted steps')
            if _lowf.any():
                ax.plot(_m[_lowf], np.full(_lowf.sum(), _ylo), 'v', ms=7, mfc='none',
                        color=_col, clip_on=False,
                        label=f'below the frame ({_y[_lowf].min()/_g*100-100:+.0f} % at '
                              f'{_m[_lowf][np.argmin(_y[_lowf])]:.0f} ADU)')
            ax.set_xscale('log')
            ax.set_xlabel('mean signal of the step, S [ADU]')
            ax.set_ylabel('(Var - RN$^2$) / (S + T)   [ADU/e$^-$]')
            ax.set_ylim(_ylo, _yhi)
            _sec = ax.secondary_yaxis('right', functions=(lambda v, _g=_g: 100*(v/_g - 1),
                                                          lambda v, _g=_g: _g*(1 + v/100)))
            _sec.set_ylabel('departure from the fitted gain [%]')
            if _ff is not None and _ff['Rms'] <= 20.0:
                # what the response model predicts for the SAME ratio: if the
                # curve passes through the points, the fall at the bottom is
                # explained without a constant deficit
                _xs = np.logspace(np.log10(max(_m[_ok].min(), 0.5)),
                                  np.log10(_m[_ok].max()), 240)
                _ym = _NL.variance(_xs, _ff['g'], _ff['D'])/(_xs + _T)
                ax.plot(_xs, _ym, '-', lw=1.8, color='#8172b2', alpha=0.95,
                        label=f"model, D = {_ff['D']:.1f} e$^-$")
            ax.set_title(f'{_name}: T = {_T:.1f} ADU added back', fontsize=10)
            ax.grid(alpha=0.25, which='both')
            ax.legend(fontsize=8.0, loc='lower right')
            if _ff is None:
                continue
            _bad = _ff['Rms'] > 20.0
            # the same test for the model, in the same style: flat at one
            bx = axs[1][_i]
            _pm = _NL.variance(_m[_ok], _ff['g'], _ff['D'])
            _rat = _v[_ok]/_pm
            bx.axhline(1.0, color='#55a868', ls='--', lw=1.5,
                       label=f"model, D = {_ff['D']:.1f} e$^-$, g = {_ff['g']:.4f}")
            if _in[_ok].any():
                bx.axvspan(_m[_ok][_in[_ok]].min(), _m[_ok][_in[_ok]].max(),
                           color='#dd8452', alpha=0.15, label='fit window')
            bx.plot(_m[_ok][~_in[_ok]], _rat[~_in[_ok]], 'o', ms=6, mfc='none', color=_col,
                    label='outside the window')
            bx.plot(_m[_ok][_in[_ok]], _rat[_in[_ok]], 'o-', ms=7, lw=1.3, color=_col,
                    label='fitted steps')
            bx.set_xscale('log')
            bx.set_xlabel('mean signal of the step, S [ADU]')
            bx.set_ylabel('measured variance / model')
            bx.set_ylim(0.6, 1.4)
            _sec2 = bx.secondary_yaxis('right', functions=(lambda v: 100*(v - 1),
                                                           lambda v: 1 + v/100))
            _sec2.set_ylabel('departure from the model [%]')
            bx.set_title((f"{_name}: the model over {_ff['Lo']:.0f}-{_ff['Hi']:.0f} ADU, "
                          f"{_ff['Rms']:.1f} % rms")
                         + ('  -  THE MODEL DOES NOT DESCRIBE THIS LADDER' if _bad else ''),
                         fontsize=10, color=('#c44e52' if _bad else 'black'))
            bx.grid(alpha=0.25, which='both')
            bx.legend(fontsize=8.0, loc='lower right')
        _sub = ('\ntop: flat at the fitted gain = one constant intercept describes the ladder. '
                'bottom: flat at one = the response model does'
                if _FIT else
                '\nflat at the fitted gain = one constant intercept describes the ladder; '
                'a fall at the bottom = it does not')
        fig.suptitle(f'Add the missing charge and ask whether the line is straight, {TAG}'
                     + _sub, fontsize=9.5)
        fig.tight_layout(rect=(0, 0, 1, 0.93), w_pad=3.2)
        savefig(fig, 'fig_ptc_straight.png')
