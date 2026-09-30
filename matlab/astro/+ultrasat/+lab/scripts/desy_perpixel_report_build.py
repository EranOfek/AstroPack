#!/usr/bin/env python3
"""Build the individual-pixel setup-comparison report from desy_perpixel_run.m output.

Reads perpixel.json (scalars + SNR curves per die-run, split by readout-column
parity) and writes report.md, report.html and the figures next to it.
"""
import argparse, json, os, math
from collections import defaultdict
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

P = argparse.ArgumentParser()
P.add_argument('--indir',  default='/home/sasha/claude/desy_perpixel')
P.add_argument('--out',    default=None, help='output directory (default = indir)')
A = P.parse_args()
OUT = A.out or A.indir
os.makedirs(OUT, exist_ok=True)

with open(os.path.join(A.indir, 'perpixel.json')) as fh:
    D = json.load(fh)
FULL = D.get('Full') or []
ZERO = D.get('ZeroOnly') or []
if isinstance(FULL, dict):  FULL = [FULL]
if isinstance(ZERO, dict):  ZERO = [ZERO]

# ---------------------------------------------------------------- helpers
def g(d, *keys, default=np.nan):
    """nested get returning nan when anything is missing"""
    for k in keys:
        if not isinstance(d, dict) or k not in d:
            return default
        d = d[k]
    return default if d is None else d

def arr(x):
    a = np.atleast_1d(np.asarray(x, dtype=float))
    return a

def setup_label(e):
    s = f"TX {e['TX']:.1f}"
    if abs(float(e.get('RSTH', 3.0)) - 3.0) > 0.01:
        s += f" RST_H {e['RSTH']:.1f}"
    return s

SET_COLOR = {'AV': '#c44e52', 'aSpect': '#4c72b0'}
FLAV_MARK = {6: 'o', 2: 's'}

def style(e):
    return dict(color=SET_COLOR.get(e.get('Settings'), '#555555'),
                marker=FLAV_MARK.get(int(e.get('Flavour', 6)), '^'))

# ---------------------------------------------------------------- per-die rows
def row_full(e):
    """one flat record per die-run, parity 'All'"""
    r = dict(tag=e['Tag'], run=str(e['Run']), die=e['Die'], settings=e.get('Settings'),
             tx=float(e['TX']), rsth=float(e.get('RSTH', np.nan)),
             flavour=int(e.get('Flavour', 0)), passed=e.get('Pass'),
             gain=float(g(e, 'GainUsed')), expsen=float(g(e, 'ExpSen')),
             nbad=int(g(e, 'Bad', 'Nbad', default=0)))
    for pn in ('All', 'Even', 'Odd'):
        z = g(e, 'Zero', pn, default={})
        t = g(e, 'Threshold', pn, default={})
        pre = '' if pn == 'All' else pn.lower() + '_'
        r[pre+'bias']     = float(g(z, 'BiasLevel'))
        r[pre+'fpn']      = float(g(z, 'FixedPatternRMS'))
        r[pre+'rn']       = float(g(z, 'ReadNoiseMedian'))
        r[pre+'rn_rms']   = float(g(z, 'ReadNoiseRMS'))
        r[pre+'rn_tail']  = float(g(z, 'TailFrac'))
        r[pre+'rn_spread']= float(g(z, 'SpreadSigmaRel'))
        r[pre+'dc']       = float(g(t, 'DCSpread', 'Median'))
        r[pre+'dsnu']     = float(g(t, 'DCSpread', 'StdIntr'))
        r[pre+'dc_fit']   = float(g(t, 'DCSpread', 'StdFitRobust'))
        r[pre+'tdark']    = float(g(t, 'MedianDarkE'))
        r[pre+'tlight']   = float(g(t, 'MedianLightE'))
        r[pre+'tdark_adu']= float(g(t, 'DarkSpread', 'Median'))
        r[pre+'prnu']     = float(g(t, 'PRNU'))
        r[pre+'prnu_err'] = float(g(t, 'PRNU_Err'))
        r[pre+'offset_e'] = float(g(t, 'OffsetFPN_e'))
    gn = r['gain'] if np.isfinite(r['gain']) and r['gain'] > 0 else np.nan
    r['rn_e'] = r['rn'] / gn if np.isfinite(gn) else np.nan
    for meth in ('light', 'dark'):
        b = g(e, 'Budget', 'All', meth, default={})
        r['qlim_cal_'+meth] = float(g(b, 'Qlim_cal'))
        r['qlim_raw_'+meth] = float(g(b, 'Qlim_raw'))
    return r

def row_zero(e):
    r = dict(tag=e['Tag'], run=str(e['Run']), die=e['Die'], settings=e.get('Settings'),
             tx=float(e['TX']), rsth=float(e.get('RSTH', np.nan)),
             flavour=int(e.get('Flavour', 0)), nbad=int(g(e, 'Nbad', default=0)))
    for key, src in (('', 'Zero'), ('f5_', 'Zero5')):
        z = g(e, src, 'All', default={})
        r[key+'bias']      = float(g(z, 'BiasLevel'))
        r[key+'fpn']       = float(g(z, 'FixedPatternRMS'))
        r[key+'rn']        = float(g(z, 'ReadNoiseMedian'))
        r[key+'rn_tail']   = float(g(z, 'TailFrac'))
        r[key+'rn_spread'] = float(g(z, 'SpreadSigmaRel'))
        r[key+'nframes']   = float(g(z, 'Nframes'))
    return r

ROWS = [row_full(e) for e in FULL]
ZROWS = [row_zero(e) for e in ZERO]
BYTAG = {e['Tag']: e for e in FULL}

# ---------------------------------------------------------------- figures
def scan_plot(fname, ykeys, ylabel, title, rows=None, logy=False, ylim=None,
              parity=False, errkey=None):
    """value versus TX, one point per die-run, medians per setup overlaid"""
    rows = rows if rows is not None else ROWS
    fig, ax = plt.subplots(figsize=(8.2, 4.6))
    keys = ykeys if isinstance(ykeys, (list, tuple)) else [ykeys]
    seen = set()
    for r in rows:
        for ik, k in enumerate(keys):
            y = r.get(k, np.nan)
            if not np.isfinite(y):
                continue
            st = style(r)
            lbl = None
            tag = (r['settings'], st['marker'])
            if tag not in seen:
                seen.add(tag)
                lbl = f"{r['settings']}, W{'04' if r['flavour']==6 else '08'}"
            ax.plot(r['tx'] + 0.012*ik, y, st['marker'], color=st['color'], ms=5,
                    mfc=st['color'] if ik == 0 else 'none', alpha=0.85, label=lbl)
    # median per (tx, rsth, settings)
    grp = defaultdict(list)
    for r in rows:
        y = r.get(keys[0], np.nan)
        if np.isfinite(y):
            grp[(r['tx'], r['rsth'], r['settings'])].append(y)
    for (tx, rsth, sett), vals in sorted(grp.items()):
        m = np.median(vals)
        ax.plot(tx, m, '_', color='k', ms=22, mew=1.6, zorder=5)
        if abs(rsth - 3.0) > 0.01:
            ax.annotate('RST_H %.1f' % rsth, (tx, m), textcoords='offset points',
                        xytext=(0, 9), ha='center', fontsize=7, color='#666666')
    ax.set_xlabel('TX voltage [V]')
    ax.set_ylabel(ylabel)
    ax.set_title(title, fontsize=10)
    if logy:
        ax.set_yscale('log')
    if ylim:
        ax.set_ylim(*ylim)
    ax.grid(alpha=0.3)
    ax.legend(fontsize=7, ncol=2)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, fname), dpi=110)
    plt.close(fig)
    return fname

FIGS = {}
if ROWS:
    FIGS['rn'] = scan_plot('fig_rn_vs_tx.png', ['rn', 'even_rn', 'odd_rn'],
                           'median per-pixel read noise [ADU]',
                           'Read noise per pixel (filled = all pixels, open = even / odd readout columns)')
    FIGS['rn_spread'] = scan_plot('fig_rn_spread_vs_tx.png', ['rn_spread'],
                                  'intrinsic spread of sigma_RN  [fraction]',
                                  'Pixel-to-pixel non-uniformity of the read noise (chi2 sampling scatter removed)')
    FIGS['rn_tail'] = scan_plot('fig_rn_tail_vs_tx.png', ['rn_tail'],
                                'fraction of pixels with sigma > 2x median',
                                'Read-noise tail')
    FIGS['fpn'] = scan_plot('fig_fpn_vs_tx.png', ['fpn'],
                            'bias fixed pattern [ADU]',
                            'Fixed pattern of the bias frame (its own sampling noise removed)')
    FIGS['dc'] = scan_plot('fig_dc_vs_tx.png', ['dc'], 'dark current [ADU/s]',
                           'Dark current', logy=True)
    FIGS['dsnu'] = scan_plot('fig_dsnu_vs_tx.png', ['dsnu'], 'DSNU [ADU/s]',
                             'Dark-current non-uniformity (fit noise removed)', logy=True)
    FIGS['thr'] = scan_plot('fig_threshold_vs_tx.png', ['tlight', 'tdark'],
                            'charge threshold [e-]',
                            'Charge threshold: light method (filled) and dark method (open)')
    FIGS['prnu'] = scan_plot('fig_prnu_vs_tx.png', ['prnu'], 'PRNU [fraction]',
                             'Photo-response non-uniformity, from sigma_fixed^2 = a^2 + (b S)^2')
    FIGS['offset'] = scan_plot('fig_offset_vs_tx.png', ['offset_e'],
                               'additive offset pattern [e-]',
                               'Additive (offset) fixed pattern, the a of the same fit')
    FIGS['qlim'] = scan_plot('fig_qlim_vs_tx.png', ['qlim_cal_light', 'qlim_raw_light'],
                             'limiting signal at SNR = 5 [e-]',
                             'Smallest detectable signal, light-method threshold (filled = calibrated, open = raw)',
                             logy=True)

# pattern profile: fixed pattern versus signal, one curve per setup (median die)
if FULL:
    fig, axs = plt.subplots(1, 2, figsize=(11.5, 4.6))
    for e in FULL:
        if e['Die'] != 'W04_D07':
            continue
        for ax, key, lbl in ((axs[0], 'PatternB', 'bright ladder'),
                             (axs[1], 'PatternD', 'dark ladder')):
            pat = g(e, key, 'All', default={})
            med = arr(g(pat, 'Median', default=[]))
            rel = arr(g(pat, 'RelFixed', default=[]))
            ok = np.isfinite(med) & np.isfinite(rel) & (med > 5) & (med < 12000)
            if ok.sum() > 2:
                ax.plot(med[ok], 100*rel[ok], '-o', ms=3, lw=1,
                        color=SET_COLOR.get(e.get('Settings'), '#555'),
                        alpha=0.8, label=f"run {e['Run']} {setup_label(e)}")
    for ax, lbl in ((axs[0], 'bright ladder'), (axs[1], 'dark ladder')):
        ax.set_xscale('log'); ax.set_yscale('log')
        ax.set_xlabel('signal [ADU]'); ax.set_ylabel('fixed pattern [% of signal]')
        ax.set_title(f'W04_D07, {lbl}', fontsize=10)
        ax.grid(alpha=0.3, which='both')
        ax.legend(fontsize=6, ncol=2)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, 'fig_pattern_profile.png'), dpi=110)
    plt.close(fig)
    FIGS['pattern'] = 'fig_pattern_profile.png'

# SNR curves with the threshold dead zone
if FULL:
    fig, axs = plt.subplots(1, 2, figsize=(11.5, 4.8), sharey=True)
    for e in FULL:
        if e['Die'] != 'W04_D07':
            continue
        b = g(e, 'Budget', 'All', 'light', default={})
        Q = arr(g(b, 'Q', default=[]))
        if Q.size < 3:
            continue
        st = style(e)
        Te = float(g(b, 'Threshold_e', default=np.nan))
        for ax, key, lbl in ((axs[0], 'SNR_cal', 'fixed pattern calibrated out'),
                             (axs[1], 'SNR_raw', 'raw frame')):
            S = arr(g(b, key, default=[]))
            if S.size == Q.size:
                ax.plot(Q, S, '-', lw=1.3, color=st['color'], alpha=0.85,
                        label=f"run {e['Run']} {setup_label(e)}  T={Te:.0f} e-")
                if np.isfinite(Te) and Te > 0:
                    ax.axvspan(Q.min(), min(Te, Q.max()), color=st['color'], alpha=0.05)
    for ax, lbl in ((axs[0], 'fixed pattern calibrated out'), (axs[1], 'single raw frame')):
        ax.axhline(5, color='k', ls=':', lw=1)
        ax.set_xscale('log'); ax.set_yscale('log')
        ax.set_xlabel('incident charge Q [e-]')
        ax.set_title(lbl, fontsize=10)
        ax.grid(alpha=0.3, which='both')
        ax.legend(fontsize=6)
    axs[0].set_ylabel('SNR per pixel')
    fig.suptitle('Per-pixel SNR, 15 s exposure; shaded = below the charge threshold', fontsize=10)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT, 'fig_snr_curves.png'), dpi=110)
    plt.close(fig)
    FIGS['snr'] = 'fig_snr_curves.png'

# ---------------------------------------------------------------- tables
def fmt(v, p=3):
    if v is None or (isinstance(v, float) and not np.isfinite(v)):
        return '--'
    return f'{v:.{p}f}'

def table(rows, cols):
    head = '| ' + ' | '.join(c[0] for c in cols) + ' |'
    rule = '|' + '|'.join('---' for _ in cols) + '|'
    out = [head, rule]
    for r in rows:
        out.append('| ' + ' | '.join(c[1](r) for c in cols) + ' |')
    return '\n'.join(out)

ROWS.sort(key=lambda r: (r['tx'], -r['rsth'], r['settings'] or '', r['die']))
ZROWS.sort(key=lambda r: (r['tx'], r['die']))

MAIN_COLS = [
    ('run',        lambda r: r['run']),
    ('die',        lambda r: r['die']),
    ('set',        lambda r: r['settings'] or '--'),
    ('TX',         lambda r: fmt(r['tx'], 1)),
    ('RST_H',      lambda r: fmt(r['rsth'], 1)),
    ('bad col',    lambda r: str(r['nbad'])),
    ('bias',       lambda r: fmt(r['bias'], 1)),
    ('FPN',        lambda r: fmt(r['fpn'], 2)),
    ('RN [ADU]',   lambda r: fmt(r['rn'], 3)),
    ('RN [e-]',    lambda r: fmt(r['rn_e'], 2)),
    ('RN spread',  lambda r: fmt(100*r['rn_spread'], 1) + ' %'),
    ('tail',       lambda r: fmt(100*r['rn_tail'], 1) + ' %'),
    ('gain',       lambda r: fmt(r['gain'], 4)),
    ('DC [ADU/s]', lambda r: fmt(r['dc'], 4)),
    ('DSNU',       lambda r: fmt(r['dsnu'], 4)),
    ('T light',    lambda r: fmt(r['tlight'], 1)),
    ('T dark',     lambda r: fmt(r['tdark'], 1)),
    ('PRNU',       lambda r: fmt(100*r['prnu'], 3) + ' %'),
    ('a [e-]',     lambda r: fmt(r['offset_e'], 2)),
    ('Qlim cal',   lambda r: fmt(r['qlim_cal_light'], 1)),
    ('Qlim raw',   lambda r: fmt(r['qlim_raw_light'], 1)),
]

PARITY_COLS = [
    ('run',   lambda r: r['run']),
    ('die',   lambda r: r['die']),
    ('TX',    lambda r: fmt(r['tx'], 1)),
    ('RN even',  lambda r: fmt(r['even_rn'], 3)),
    ('RN odd',   lambda r: fmt(r['odd_rn'], 3)),
    ('FPN even', lambda r: fmt(r['even_fpn'], 2)),
    ('FPN odd',  lambda r: fmt(r['odd_fpn'], 2)),
    ('DC even',  lambda r: fmt(r['even_dc'], 4)),
    ('DC odd',   lambda r: fmt(r['odd_dc'], 4)),
    ('T light even', lambda r: fmt(r['even_tlight'], 1)),
    ('T light odd',  lambda r: fmt(r['odd_tlight'], 1)),
    ('T dark even',  lambda r: fmt(r['even_tdark'], 1)),
    ('T dark odd',   lambda r: fmt(r['odd_tdark'], 1)),
    ('PRNU even',    lambda r: fmt(100*r['even_prnu'], 3)),
    ('PRNU odd',     lambda r: fmt(100*r['odd_prnu'], 3)),
]

ZERO_COLS = [
    ('run',  lambda r: r['run']),
    ('die',  lambda r: r['die']),
    ('TX',   lambda r: fmt(r['tx'], 1)),
    ('N ZE', lambda r: fmt(r['nframes'], 0)),
    ('bias', lambda r: fmt(r['bias'], 1)),
    ('FPN',  lambda r: fmt(r['fpn'], 2)),
    ('RN',   lambda r: fmt(r['rn'], 3)),
    ('RN spread', lambda r: fmt(100*r['rn_spread'], 1) + ' %'),
    ('tail',      lambda r: fmt(100*r['rn_tail'], 1) + ' %'),
    ('RN (5 frames)', lambda r: fmt(r['f5_rn'], 3)),
    ('spread (5)',    lambda r: fmt(100*r['f5_rn_spread'], 1) + ' %'),
]

# ---------------------------------------------------------------- ranking
def setup_summary():
    grp = defaultdict(list)
    for r in ROWS:
        grp[(r['tx'], r['rsth'], r['settings'], r['run'])].append(r)
    out = []
    for (tx, rsth, sett, run), rs in sorted(grp.items()):
        def med(k):
            v = [x[k] for x in rs if np.isfinite(x.get(k, np.nan))]
            return np.median(v) if v else np.nan
        out.append(dict(run=run, tx=tx, rsth=rsth, settings=sett, ndie=len(rs),
                        rn=med('rn'), rn_e=med('rn_e'), fpn=med('fpn'),
                        dc=med('dc'), dsnu=med('dsnu'), tlight=med('tlight'),
                        tdark=med('tdark'), prnu=med('prnu'), offset=med('offset_e'),
                        qlim_cal=med('qlim_cal_light'), qlim_raw=med('qlim_raw_light')))
    return out

SETUPS = setup_summary()
SETUP_COLS = [
    ('run',    lambda r: r['run']),
    ('set',    lambda r: r['settings'] or '--'),
    ('TX',     lambda r: fmt(r['tx'], 1)),
    ('RST_H',  lambda r: fmt(r['rsth'], 1)),
    ('dies',   lambda r: str(r['ndie'])),
    ('RN [ADU]', lambda r: fmt(r['rn'], 3)),
    ('RN [e-]',  lambda r: fmt(r['rn_e'], 2)),
    ('FPN [ADU]',lambda r: fmt(r['fpn'], 2)),
    ('DC [ADU/s]', lambda r: fmt(r['dc'], 4)),
    ('DSNU',     lambda r: fmt(r['dsnu'], 4)),
    ('T light [e-]', lambda r: fmt(r['tlight'], 1)),
    ('T dark [e-]',  lambda r: fmt(r['tdark'], 1)),
    ('PRNU',     lambda r: fmt(100*r['prnu'], 3) + ' %'),
    ('a [e-]',   lambda r: fmt(r['offset'], 2)),
    ('Qlim cal [e-]', lambda r: fmt(r['qlim_cal'], 1)),
    ('Qlim raw [e-]', lambda r: fmt(r['qlim_raw'], 1)),
]

# ---------------------------------------------------------------- markdown
LIN = D.get('LinLimit')
MD = []
w = MD.append
w('# DESY wafer test TH02954 — individual-pixel comparison of the setups\n')
w(f'Lot TH02954, high-gain half, DESY region 100x100 pixels, {len(ROWS)} die-runs with '
  f'full ladders and {len(ZROWS)} ZE-only die-runs. Every statistic is computed per pixel '
  'and separately for the even and odd readout columns; the bad readout columns are masked.\n')

w('## 1. What is measured, and how\n')
w("""The chain follows the five steps of the measurement itself.

1. **Individual pixels, not superpixels.** All statistics are per pixel over the
   10 000 pixels of the region. A single pixel's temporal variance is measured from
   3 frames, i.e. 2 degrees of freedom: it is exponentially distributed with
   sd/mean = 1, so no single pixel's variance means anything and only the
   distribution over the region does. The ensemble is nevertheless well determined,
   to 1.4 % with 10^4 pixels.
2. **Odd and even readout columns.** In the DESY orientation the readout columns run
   along the image rows (the analysis image is the TIFF half transposed and rotated by
   180 degrees), counted from 1 at the first pixel column of the selected gain half, so
   odd = detector columns 1, 3, 5 ... The even columns are the higher-gain, higher-
   threshold parity.
3. **ZE frames first.** Per-frame common mode (clipped mean level, removed as a
   deviation so the absolute bias level survives), the bias fixed pattern with its own
   sampling noise mean(sigma^2)/N removed in quadrature, the per-pixel read noise, its
   tail, and the *intrinsic* pixel-to-pixel spread of the read noise. The last needs a
   deconvolution: with 5 frames the read-noise map has 4 degrees of freedom, so most of
   its apparent spread is chi2 sampling scatter.
4. **Per-pixel variance on the dark and the light ladders.** Per-pixel weighted fits of
   signal against exposure time and against intensity, per-pixel dark current,
   thresholds by both methods, and the fixed pattern of every ladder step.
5. **Comparison of the setups**, on a charge threshold / effective noise plane and
   through the resulting SNR curves.
""")

w('### 1.1 Two things that had to be got right\n')
w("""**The fit noise of every per-pixel parameter is removed.** The observed spread of a
fitted quantity over the pixels is (true spread) (+) (fit noise), and the fit noise
depends on the lever arm of the ladder, which differs from setup to setup: a run whose
dark ladder only reaches 150 ADU has a far noisier intercept than one reaching 3500 ADU.
Comparing raw spreads would therefore compare lever arms. Each parameter's variance is
computed analytically from the weighted fit and subtracted, and both numbers are
reported. Where the fit noise dominates, a 95 % upper limit is given instead of a value.

**The ladder points are weighted with their measured variance, not a modelled one.**
The shot noise of a ladder point follows the collected charge Q, not the measured signal
Q - T: the first T electrons are lost after they have fluctuated. This is the same fact
that makes the PTC intercept g*T rather than the read noise. A modelled weight
(RN^2 + gain*S) misses that term and over-weights the lowest dark steps, whose measured
signal can even be negative; on the synthetic test device it inflated the recovered
intercept spread by 31 %.
""")

w('### 1.2 The fit window\n')
w("""The fit window is the published one (FitRange 1000-2500 ADU with the 'auto'
step rule), because the dark ladder is non-linear at **both** ends. Fitting everything
below the 2.9 kADU linearity limit would pull in a low-signal knee: on run 31 W04_D07
the three lowest steps lie +54.8, +31.7 and +12.6 ADU above the straight line, and
including them moves the intercept from -139.8 to -112.2 ADU, changing the threshold by
20 % and turning the residuals into a systematic arc. The same selection as the
published reports also keeps the medians directly comparable with them: on W04_D07 the
per-pixel chain reproduces run 31 T_dark = 146.23 ADU against the published 146.42, and
DC = 6.1554 against 6.1593 ADU/s.
""")

w('### 1.3 Bad readout columns\n')
nbad = [r['nbad'] for r in ROWS]
if nbad:
    w(f"""Columns are flagged when their median read noise exceeds the profile median by
more than 5 robust sigmas (or 3x the median), and when their median light response falls
below the same limits. A plain ratio test finds nothing here: the column-to-column spread
of the read noise is about 10 %, so a column twice as noisy is a 30-sigma outlier but only
2x the median. Flagged columns per die-run: median {int(np.median(nbad))},
range {min(nbad)}-{max(nbad)}.
""")

w('## 2. Setup summary\n')
w('Median over the dies of each setup. TX in volts; RN in ADU and in electrons; '
  'thresholds by the light and dark methods in electrons; PRNU and the additive offset '
  'pattern *a* from the fit sigma_fixed^2 = a^2 + (b S)^2; Qlim = the smallest signal '
  'reaching SNR 5 per pixel in one 15 s frame, with and without the fixed patterns '
  'calibrated out.\n')
w(table(SETUPS, SETUP_COLS) + '\n')

for key, title, cap in (
        ('rn',        '## 3. Read noise', 'Median per-pixel read noise against TX.'),
        ('rn_spread', None, 'Intrinsic pixel-to-pixel spread of the read noise, chi2 sampling scatter removed.'),
        ('rn_tail',   None, 'Fraction of pixels noisier than twice the median.'),
        ('fpn',       '## 4. Bias fixed pattern', 'Fixed pattern of the bias frame.'),
        ('dc',        '## 5. Dark current', 'Median per-pixel dark current.'),
        ('dsnu',      None, 'Dark-current non-uniformity with the fit noise removed.'),
        ('thr',       '## 6. Charge thresholds', 'Thresholds by both methods, in electrons.'),
        ('prnu',      '## 7. Non-uniformity of the response', 'PRNU from the two-parameter pattern fit.'),
        ('offset',    None, 'Additive offset pattern from the same fit.'),
        ('pattern',   None, 'Fixed pattern against signal: the low-signal rise is the additive term, the floor is the PRNU.'),
        ('snr',       '## 8. Noise budget and SNR', 'Per-pixel SNR against incident charge; the shaded region is below the charge threshold, where no signal is collected at all.'),
        ('qlim',      None, 'Smallest detectable signal at SNR 5.')):
    if key not in FIGS:
        continue
    if title:
        w(title + '\n')
    w(f'![{cap}]({FIGS[key]})\n')
    w(f'*{cap}*\n')

w('## 9. Per-die results\n')
w(table(ROWS, MAIN_COLS) + '\n')
w('### 9.1 Odd and even readout columns\n')
w(table(ROWS, PARITY_COLS) + '\n')
if ZROWS:
    w('### 9.2 ZE-only runs (39, 39-2)\n')
    w('These runs have 10 ZE frames instead of 5, so their read-noise spread is far '
      'better constrained (9 degrees of freedom against 4). The last two columns repeat '
      'the analysis on the first 5 frames only, which is the apples-to-apples comparison '
      'with every other run.\n')
    w(table(ZROWS, ZERO_COLS) + '\n')

w('## 10. Caveats\n')
w("""- The per-pixel read noise comes from 5 ZE frames of integer ADU with sigma ~ 2 ADU,
  so the chi2 model behind the spread deconvolution is approximate at the 1 ADU
  quantisation scale. Runs 39 and 39-2 (10 frames) are the reference.
- The light-method threshold's pixel-to-pixel spread is **not** measurable from the
  published 3-step bright window: the per-pixel intercept carries about 53 ADU of fit
  noise, which is the whole of the spread quoted as StdLightADU in the earlier reports.
  Its median is unaffected. The offset fixed pattern in the noise budget therefore comes
  from the additive term of the bright-ladder pattern fit, which is well measured.
- The same applies to the PRNU from a response slope: three closely spaced intensity
  steps fix a pixel's slope to about 3 %, five times coarser than the pattern sought.
- The calibrated SNR curve assumes the fixed patterns are removed exactly. A master bias
  built from 5 ZE frames adds RN/sqrt(5), about 9.5 % on sigma_eff at low signal. It
  applies to every setup equally and does not reorder the comparison.
- Run 35 (5 dies on wafers W03, W07, W12) is excluded: its TX and RST_H are not recorded
  in any file we parse.
- The dark pattern of this detector is spatially clustered (about 34 effective
  independent pixels per 100), so averaging the dark signal over an area beats down more
  slowly than sqrt(N). The photo-response pattern is white.
""")

md = '\n'.join(MD)
with open(os.path.join(OUT, 'report.md'), 'w') as fh:
    fh.write(md)

# ---------------------------------------------------------------- html
md_src = md.replace('</script', '<\\/script')
HTML = """<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>TH02954 individual-pixel setup comparison</title>
<style>
body{max-width:1150px;margin:2rem auto;padding:0 1rem;font:15px/1.6 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{border-bottom:2px solid #ddd;padding-bottom:.3rem}
h2{margin-top:2.2rem;border-bottom:1px solid #eee}
table{border-collapse:collapse;font-size:11.5px;margin:1rem 0;display:block;overflow-x:auto}
th,td{border:1px solid #ddd;padding:2px 6px;text-align:right;white-space:nowrap}
th{background:#f5f5f5}
td:nth-child(-n+5),th:nth-child(-n+5){text-align:left}
img{max-width:100%;margin:.6rem 0;border:1px solid #eee}
em{color:#666;font-size:13px}
code{background:#f5f5f5;padding:1px 4px}
</style></head><body>
<div id="c"></div>
<script type="text/markdown" id="src">
__MD__
</script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/9.1.6/marked.min.js"></script>
<script>
document.getElementById('c').innerHTML =
  marked.parse(document.getElementById('src').textContent);
</script>
</body></html>"""
with open(os.path.join(OUT, 'report.html'), 'w') as fh:
    fh.write(HTML.replace('__MD__', md_src))

print(f'{len(ROWS)} die-runs, {len(ZROWS)} ZE-only, {len(FIGS)} figures -> {OUT}')
