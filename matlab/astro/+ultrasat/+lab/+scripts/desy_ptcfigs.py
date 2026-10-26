#!/usr/bin/env python3
"""The photon-transfer figure that every DESY report and comparison document shares.

One figure per analysed unit: both ladders, each with the straight line its routes
actually fitted and a second line over the whole 0-1000 ADU range, the residual to each
in per cent, and the two ways of compensating what is missing at the bottom - a constant
shift and the low-count model - drawn together.

It is a module because two documents draw it and the campaign has been bitten before by
the same quantity being computed twice and quoted differently. Everything it needs it
reads from that unit's own dumps.

Author : A.M. Krassilchtchikov (Oct 2026)
"""
import json
import os
import sys

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
try:
    import desy_nonlin as NL
except Exception as _e:                  # pragma: no cover - only if scipy is missing
    NL = None
    print(f'  ! desy_nonlin unavailable ({_e}); the model panels will be left empty')

SIGLO, SIGHI, FULLHI = 90.0, 1000.0, 1000.0
# A residual in per cent divides by the fitted line, so where that line passes close to
# zero the percentage says more about the line than about the measurement. Points whose
# fitted variance is below this are left out of the residual panels and counted instead.
MODFLOOR = 1.0                                   # [ADU^2]


def load(path):
    try:
        with open(path) as fh:
            return json.load(fh)
    except (OSError, ValueError):
        return None


def _route(ME, tag, key):
    r = ((ME or {}).get('Routes') or {}).get(tag)
    return None if not isinstance(r, dict) else r.get(key)


def ladder(dieroot, tag, role, ME):
    """the photon-transfer points of one ladder: signal, variance above the read noise."""
    P = load(os.path.join(dieroot, tag, 'ptc_points.json'))
    if not P or not ME or 'RN2' not in ME:
        return None
    rn2 = float(ME['RN2'])
    q = sorted((p for p in P['Points'] if p['Type'] == role), key=lambda p: p['SignalMean'])
    if not q:
        return None
    return (np.array([float(p['SignalMean']) for p in q]),
            np.array([float(p['VarMean']) for p in q]) - rn2,
            np.array([int(p['Step']) for p in q]))


def ptcfig(tag, dieroot, path):
    """both ladders, each with the window fit the routes use and a fit over the whole range.

    The window fit is not re-done here: it is the slope and intercept route c and route d
    already report, so the top panels show exactly the line the numbers in section 5 came
    from, over exactly the points that produced it. The second line is the same estimator
    - an unweighted least squares of the same two quantities - given every point up to
    1000 ADU instead.

    The top row is linear in the signal, so that a straight fit looks straight. The two
    residual rows are logarithmic in it, because that is where the two lines differ and a
    linear axis puts a third of the ladder inside one plotting symbol.
    """
    ME = load(os.path.join(dieroot, tag, 'methods.json'))
    NLF = ((load(os.path.join(dieroot, tag, 'nonlin_fit.json')) or {}).get('Ladders') or {})
    # the page is A4 landscape, so the figure is shaped to it: a nearly square figure
    # would be scaled down to fit the height and leave half the width empty
    fig, AX = plt.subplots(4, 2, figsize=(15.0, 9.7),
                           gridspec_kw=dict(height_ratios=[1.85, 0.92, 0.92, 1.30], hspace=0.36,
                                            wspace=0.15))
    note = []
    for col, (role, rt, ttl, sigkey) in enumerate((('D', 'c', 'dark ladder', 'dsig'),
                                                   ('B', 'd', 'bright ladder', 'lsig'))):
        L = ladder(dieroot, tag, role, ME)
        ax0, ax1, ax2, ax3 = AX[0][col], AX[1][col], AX[2][col], AX[3][col]
        if L is None:
            for ax in (ax0, ax1, ax2, ax3):
                ax.text(.5, .5, 'no ' + ttl, ha='center', va='center', transform=ax.transAxes,
                        fontsize=9, color='#888')
                ax.set_xticks([]); ax.set_yticks([])
            continue
        x, y, st = L
        gw, iw = _route(ME, rt, 'Slope'), _route(ME, rt, 'Intercept')
        # the window the route actually fitted, marked by its own points: the floor is
        # lowered where fewer than three steps clear 90 ADU, so the band is not always
        # the nominal one, and the signal it is defined on is the one plotted here
        wsteps = (_route(ME, rt, 'Windows') or [[]])[0]
        inw = np.isin(st, np.array(wsteps, dtype=int)) if len(wsteps) else np.zeros(x.shape, bool)
        wlo, whi = ((float(x[inw].min()), float(x[inw].max())) if inw.any() else (SIGLO, SIGHI))
        kf = (x >= 0) & (x <= FULLHI)
        gf, iff = (np.polyfit(x[kf], y[kf], 1) if kf.sum() > 1 else (np.nan, np.nan))
        wtxt = (f'{wlo:.0f}–{whi:.0f} ADU, {int(inw.sum())} points'
                + ('' if wlo >= SIGLO else '; floor lowered below ' + f'{SIGLO:.0f}'))

        xs = np.linspace(0, min(FULLHI, x.max()) * 1.02, 200)
        ax0.axvspan(wlo, whi, color='#eef4ff', zorder=0)
        ax0.plot(x, y, 'o', ms=3.4, color='#333', zorder=3, label='measured')
        if inw.any():
            ax0.plot(x[inw], y[inw], 'o', ms=7.5, mfc='none', mec='#c0392b', mew=1.0,
                     zorder=5, label=f'fitted by the window ({int(inw.sum())})')
        if gw is not None:
            ax0.plot(xs, float(gw) * xs + float(iw), '-', lw=1.5, color='#c0392b', zorder=4,
                     label=f'window fit: {float(gw):.3f}·M {float(iw):+.1f}')
        if np.isfinite(gf):
            ax0.plot(xs, gf * xs + iff, '--', lw=1.5, color='#2471a3', zorder=4,
                     label=f'0–{FULLHI:.0f} ADU fit: {gf:.3f}·M {iff:+.1f}')
        ax0.set_title(f'{ttl}:  window {wtxt}',
                      fontsize=9, loc='left')
        ax0.set_ylabel('Var − RN²  [ADU²]', fontsize=8.5)
        ax0.set_xlabel('mean signal M [ADU]', fontsize=8.5)
        ax0.legend(fontsize=7.0, loc='upper left', framealpha=.9)
        ax0.grid(alpha=.25)

        pos = x > 0
        for ax, g, i0, lab, cc in ((ax1, gw, iw, 'window fit', '#c0392b'),
                                   (ax2, gf, iff, f'0–{FULLHI:.0f} ADU fit', '#2471a3')):
            ax.axhline(0, color='#888', lw=.8)
            ax.axvspan(wlo, whi, color='#eef4ff', zorder=0)
            ax.grid(alpha=.25)
            ax.set_xscale('log')
            ax.set_ylabel(f'resid. vs\n{lab} [%]', fontsize=8)
            ax.set_xlabel('mean signal M [ADU]  (log)', fontsize=8)
            if g is None or not np.isfinite(g):
                continue
            mod = float(g) * x + float(i0)
            ok = pos & (mod > MODFLOOR)
            r = 100.0 * (y[ok] - mod[ok]) / mod[ok]
            ax.plot(x[ok], r, 'o', ms=3.2, color=cc)
            bad = pos & (mod <= MODFLOOR)
            if bad.any():
                note.append(f'the {lab} of the {ttl} passes within {MODFLOOR:.0f} ADU² of zero '
                            f'below {x[bad].max():.0f} ADU, so {int(bad.sum())} '
                            f'point{"s" if bad.sum() > 1 else ""} at the bottom '
                            f'{"are" if bad.sum() > 1 else "is"} left out of its residual panel')
            if r.size:
                lo, hi = float(np.min(r)), float(np.max(r))
                pad = max(3.0, .08 * (hi - lo))
                ax.set_ylim(max(lo - pad, -260), min(hi + pad, 260))
        # the fourth panel: the two ways of compensating what is missing at the bottom,
        # side by side. The constant shift is the straight line of the window fit, which is
        # the same thing as adding the intercept back to the signal, because the charge
        # intercept is the ADU intercept over the gain. The model is the one fitted to this
        # ladder in the unit's own report, read here rather than fitted again.
        ax3.axhline(0, color='#888', lw=.8)
        ax3.axvspan(wlo, whi, color='#eef4ff', zorder=0)
        ax3.grid(alpha=.25)
        ax3.set_xscale('log')
        ax3.set_ylabel('residual after\ncompensation [%]', fontsize=8)
        ax3.set_xlabel('mean signal M [ADU]  (log)', fontsize=8)
        ax3.axvline(100, color='#555', lw=.8, ls=':')
        rr = []
        if gw is not None:
            mod = float(gw) * x + float(iw)
            ok = pos & (mod > MODFLOOR)
            r = 100.0 * (y[ok] - mod[ok]) / mod[ok]
            rr.append(r)
            ax3.plot(x[ok], r, 'o', ms=3.2, color='#c0392b',
                     label=f'constant shift: {float(iw) / float(gw):+.1f} ADU of signal')
        ff = NLF.get(role)
        if NL is not None and ff:
            mod = NL.variance(np.maximum(x, 1e-6), float(ff['g']), float(ff['D']))
            ok = pos & np.isfinite(mod) & (mod > MODFLOOR)
            r = 100.0 * (y[ok] - mod[ok]) / mod[ok]
            rr.append(r)
            poor = float(ff.get('Rms', 0)) > 20.0
            ax3.plot(x[ok], r, 's', ms=3.2, mfc='none' if poor else '#196f3d',
                     mec='#196f3d', mew=1.0,
                     label=(f'model Q·exp(−D/Q), D = {float(ff["D"]):.1f} e⁻'
                            + (f' (fit rms {float(ff["Rms"]):.0f} %)' if poor else '')))
        ax3.legend(fontsize=6.8, loc='lower right', framealpha=.9)
        if rr:
            # a robust range: one point where the model predicts almost no variance can
            # carry a residual of several hundred per cent and flatten everything else.
            allr = np.concatenate(rr)
            lo, hi = (float(np.percentile(allr, 4)), float(np.percentile(allr, 96))
                      if allr.size > 6 else (float(allr.min()), float(allr.max())))
            if allr.size <= 6:
                lo, hi = float(allr.min()), float(allr.max())
            span = max(hi - lo, 12.0)
            lo, hi = lo - .18 * span, hi + .18 * span
            ax3.set_ylim(lo, hi)
            nout = int(((allr < lo) | (allr > hi)).sum())
            if nout:
                ax3.text(.015, .04, f'{nout} point{"s" if nout > 1 else ""} off scale',
                         transform=ax3.transAxes, fontsize=6.6, color='#777')

        if pos.any():                       # one signal range for all three residual panels
            _lo, _hi = float(x[pos].min()), float(x[pos].max())
            for ax in (ax1, ax2, ax3):
                ax.set_xlim(_lo / 1.6, _hi * 1.6)
        if (~pos).any():
            note.append(f'{int((~pos).sum())} step{"s" if (~pos).sum() > 1 else ""} of the '
                        f'{ttl} sit at or below zero signal and cannot be shown on a '
                        f'logarithmic axis')
    fig.savefig(path, dpi=125, bbox_inches='tight')
    plt.close(fig)
    return note


def datasheet(dieroot, tag):
    """the measured quantities of one unit, in electrons, as its own report quotes them.

    The gain that converts is the one the chain uses for every electron figure, and each
    error carries the quantity's own relative error and the gain's, which is what the
    per-unit report's section 11 does. Reading them here rather than in each document is
    what keeps the documents and the reports agreeing.
    """
    B = load(os.path.join(dieroot, tag, 'budget.json')) or {}
    ME = load(os.path.join(dieroot, tag, 'methods.json'))
    ER = load(os.path.join(dieroot, tag, 'errors.json')) or {}
    IN = ((B.get('Unmasked') or {}).get('Inputs')) or {}
    g = IN.get('GainADU')
    gg, gs, gy = (_route(ME, 'd', k) for k in ('Gain', 'GainStat', 'GainSyst'))
    grel = ((float(gs)**2 + float(gy or 0)**2)**0.5 / abs(float(gg))) if (gg and gs is not None) else 0.0

    def to_e(key, eadu):
        v = IN.get(key)
        if v is None or not g:
            return None, None
        rel = grel**2
        if eadu is not None and float(v):
            rel += (float(eadu) / abs(float(v)))**2
        out = float(v) / float(g)
        return out, (out * rel**0.5 if rel else None)

    st, sy = _route(ME, 'a', 'SlopeStat'), _route(ME, 'a', 'SlopeSyst')
    dce = None if st is None else (float(st)**2 + float(sy or 0)**2)**0.5
    dc, dcerr = to_e('DC_ADU', dce)
    rn, rnerr = to_e('RN_ADU', ((ER.get('ReadNoise') or {}).get('All')))
    out = dict(Tag=tag, DC_e=dc, DC_e_err=dcerr, RN_e=rn, RN_e_err=rnerr,
               GainADU=(float(g) if g else None))
    for rt in 'abcd':
        out[rt] = _route(ME, rt, 'Threshold_e')
        out[rt + '_err'] = _route(ME, rt, 'Threshold_e_err')
        out['g' + rt] = _route(ME, rt, 'Gain')
        _s, _y = _route(ME, rt, 'GainStat'), _route(ME, rt, 'GainSyst')
        out['g' + rt + '_err'] = (None if _s is None else
                                  (float(_s)**2 + float(_y or 0)**2)**0.5)
    return out
