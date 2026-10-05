#!/usr/bin/env python3
"""The WIS slide deck on the aSpect flavour-comparison measurements.

Thirteen slides, built to Yossi's outline. Every number is read from the chain's
json dumps and every figure from the per-die reports, so the deck cannot drift
from the analysis it describes: rebuild it and it re-reads.

usage: desy_slides_wis.py [--sig] [--out FILE]
  --sig   take the single-die figures and numbers from the signal-window run
          (the 100-1000 ADU rule) instead of the default windows
"""
import argparse, json, os, sys, datetime
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from desy_pptx import Deck, EMU, W, H

P = argparse.ArgumentParser()
P.add_argument('--root', default='/home/sasha/claude/desy_die')
P.add_argument('--rnroot', default='/home/sasha/claude/desy_rn')
P.add_argument('--summary', default='/home/sasha/claude/desy_die/summary')
P.add_argument('--summary-sig', default='/home/sasha/claude/desy_die/summary_sig16',
               help=('the signal-window summary over the SAME die-runs as --summary; the two panels '
                     'of slide 12 must cover the same dies to be a comparison at all'))
P.add_argument('--out', default='/home/sasha/DESY_reports/WIS_TH02954_aSpect_flavour_comparison.pptx')
A = P.parse_args()

DIE, RUN = 'W04_D07', '32'
OLD = os.path.join(A.root, f'run{RUN}_{DIE}_high')          # default windows
NEW = OLD + '_sig'                                           # 100-1000 ADU rule
OLD31 = os.path.join(A.root, f'run31_{DIE}_high')
NEW31 = OLD31 + '_sig'

def J(d, n):
    p = os.path.join(d, n)
    return json.load(open(p)) if os.path.isfile(p) else None

def thr(d):
    M = J(d, 'methods.json')
    return {k: (float(M['Routes'][k]['Threshold_e']),
                float(M['Routes'][k]['Threshold_e_err'])) for k in 'abcd'} if M else None

ME, DK, LT = J(OLD, 'methods.json'), J(OLD, 'dark.json'), J(OLD, 'light.json')
ZE = J(os.path.join(A.rnroot, f'run{RUN}_{DIE}_high'), 'stats.json')
PT, BU = J(OLD, 'ptc.json'), J(OLD, 'budget.json')
G = float(ME['Routes']['d']['Gain'])
Told, Tnew = thr(OLD), thr(NEW)
T31old, T31new = thr(OLD31), thr(NEW31)

FLAV = {'W04': 6, 'W08': 2}            # General/Lots_Cassette_table.xlsx
VOLT = {'31':   dict(RST_L='1.0', RST_SEL='3.8', SF='3.3', TX='3.3'),
        '32':   dict(RST_L='1.2', RST_SEL='3.5', SF='3.0', TX='3.3'),
        '38':   dict(RST_L='1.2', RST_SEL='3.5', SF='3.0', TX='3.5'),
        '38-2': dict(RST_L='1.2', RST_SEL='3.5', SF='3.0', TX='3.7')}
SETNAME = {'31': 'setting A', '32': 'aSpect', '38': 'aSpect', '38-2': 'aSpect'}
CTX = (f"TH02954 {DIE}  ·  wafer 04 = flavour {FLAV['W04']}  ·  run {RUN} ({SETNAME[RUN]} settings, "
       f"V$_TX$ {VOLT[RUN]['TX']} V)  ·  high-gain half  ·  whole die, 22.5 M pixels")
CTX = CTX.replace('$_TX$', 'DD_TX')

NAVY, GREY, RED = '1F3864', '5A5A5A', 'C44E52'
d = Deck()

def slide(title, context=None, kicker=None):
    d.add()
    d.rect(0, 0, W, int(0.95*EMU), NAVY)
    d.text(int(0.45*EMU), int(0.17*EMU), W-int(0.9*EMU), int(0.62*EMU), title, 2200, True, 'FFFFFF')
    y = int(1.05*EMU)
    if context:
        d.rect(0, int(0.95*EMU), W, int(0.38*EMU), 'EDF1F7')
        d.text(int(0.45*EMU), int(1.0*EMU), W-int(0.9*EMU), int(0.3*EMU), context, 1050, False, NAVY)
        y = int(1.45*EMU)
    if kicker:
        d.text(int(0.45*EMU), y, W-int(0.9*EMU), int(0.45*EMU), kicker, 1250, False, GREY)
        y += int(0.5*EMU)
    return y

def cap(x, y, w, t):
    d.text(x, y, w, int(0.42*EMU), t, 1000, False, GREY, align='ctr')

# ------------------------------------------------------------------ 1. title
d.add()
d.rect(0, 0, W, H, NAVY)
d.text(int(1.0*EMU), int(2.3*EMU), W-int(2.0*EMU), int(1.2*EMU),
       'WIS analysis of the flavour-comparison measurements at aSpect', 3600, True, 'FFFFFF')
d.text(int(1.0*EMU), int(3.7*EMU), W-int(2.0*EMU), int(0.8*EMU),
       'Lot TH02954  ·  wafers 04 (flavour 6) and 08 (flavour 2)  ·  runs 31, 32, 38, 38-2',
       1700, False, 'C7D3E8')
d.text(int(1.0*EMU), int(4.5*EMU), W-int(2.0*EMU), int(0.8*EMU),
       'Weizmann Institute of Science  ·  ' + datetime.date.today().strftime('%d %B %Y'),
       1400, False, '8FA6C8')

# ------------------------------------------------------------------ 2. data summary
slide('Data summary — what was measured',
      kicker='Measurements only; no results on this slide. Every run is the same 7 dies, '
             'of which 4 pass the final test and are analysed.')
rows = [['wafer / dies', 'flavour', 'run', 'settings  V_RST_L / V_RST_SEL / V_SF / V_TX',
         'n(ZE)', 'dark ladder', 'bright ladder']]
for run in ('31', '32', '38', '38-2'):
    v = VOLT[run]
    for waf, dies in (('W04', 'D05, D07'), ('W08', 'D02, D04')):
        rows.append([f'{waf} — {dies}', str(FLAV[waf]), run,
                     f"{v['RST_L']} / {v['RST_SEL']} / {v['SF']} / {v['TX']}  ({SETNAME[run]})",
                     '5', '9 × 3 fr, 15–600 s', '34 × 3 fr, 0.01–14 int'])
d.table(int(0.45*EMU), int(1.95*EMU), W-int(0.9*EMU), rows,
        colw=[int(2.0*EMU), int(0.9*EMU), int(0.8*EMU), int(4.5*EMU), int(0.7*EMU),
              int(2.1*EMU), int(2.4*EMU)], sz=950)
d.text(int(0.45*EMU), int(5.6*EMU), W-int(0.9*EMU), int(1.4*EMU), [
    'Settings are read from each run\'s PTC_Config.xlsx. Runs 32, 38 and 38-2 differ from each '
    'other in V_TX alone (3.3 / 3.5 / 3.7 V) — a controlled transfer-gate scan.',
    'Run 31 differs from them in V_RST_L, V_RST_SEL and V_SF as well, so it is not part of that scan.',
    'Flavour is a wafer property (General/Lots_Cassette_table.xlsx).   134 TIFF frames, 12.1 GB per die-run.'],
    1050, False, GREY)

# ------------------------------------------------------------------ 3. single-die opener
d.add()
d.rect(0, 0, W, H, 'EDF1F7')
d.rect(0, int(2.4*EMU), W, int(0.06*EMU), NAVY)
d.text(int(1.0*EMU), int(1.4*EMU), W-int(2.0*EMU), int(1.0*EMU),
       'Single-die analysis', 3600, True, NAVY)
d.text(int(1.0*EMU), int(2.7*EMU), W-int(2.0*EMU), int(1.6*EMU), [
    f'TH02954 {DIE} — wafer 04, flavour {FLAV["W04"]}',
    f'run {RUN}, aSpect settings, V_TX {VOLT[RUN]["TX"]} V, high-gain half'], 2000, False, '28406B')
d.text(int(1.0*EMU), int(4.6*EMU), W-int(2.0*EMU), int(1.2*EMU), [
    'Whole die, 4740 × 4742 = 22.5 M pixels, individual pixels throughout — nothing averaged into superpixels.',
    'The slides that follow all refer to this one die unless stated otherwise.'], 1300, False, GREY)

# ------------------------------------------------------------------ 4. read noise
y = slide('General behaviour — read noise', CTX)
d.picture(os.path.join(OLD, 'fig_rn_distribution.png'),
          int(0.5*EMU), y, int(8.3*EMU), int(4.6*EMU))
cap(int(0.5*EMU), y+int(4.6*EMU), int(8.3*EMU),
    'Per-pixel read noise over the 5 zero-exposure frames; dashed = the same measurement if every pixel were identical')
d.rect(int(9.1*EMU), y, int(3.75*EMU), int(3.3*EMU), 'F2F4F7')
d.text(int(9.35*EMU), y+int(0.2*EMU), int(3.3*EMU), int(3.0*EMU), [
    f"read noise (median)",
    f"   {float(ZE['All']['ReadNoiseMedian']):.3f} ADU = {float(ZE['All']['ReadNoiseMedian'])/G:.2f} e-",
    '',
    f"bias level  {float(ZE['All']['BiasLevel']):.2f} ADU",
    f"fixed pattern  {float(ZE['All']['FixedPatternRMS']):.2f} ADU",
    f"common mode  {float(ZE['CommonMode']['Std']):.4f} ADU",
    '',
    'With 5 frames each pixel’s sigma has 4 dof',
    '(~50 % scatter), so most of the width is',
    'the measurement, not the pixels.'], 1200, False, '28406B')

# ------------------------------------------------------------------ 5. dark current
y = slide('General behaviour — dark current', CTX,
          kicker='Left: where the dark current comes from. Right: how it varies over the die.')
d.picture(os.path.join(OLD, 'fig_dark_ladder_fit.png'),
          int(0.4*EMU), y, int(6.3*EMU), int(2.4*EMU))
cap(int(0.4*EMU), y+int(2.4*EMU), int(6.3*EMU),
    f"Dark ladder and its fit — steps {list(np.atleast_1d(np.array(DK['FitSteps'],dtype=int)))}, "
    f"slope = dark current, −intercept = dark-route threshold")
d.picture(os.path.join(OLD, 'fig_dark_dc_map.png'),
          int(7.0*EMU), y-int(0.1*EMU), int(5.9*EMU), int(4.5*EMU))
cap(int(7.0*EMU), y+int(4.4*EMU), int(5.9*EMU), 'Dark current per pixel')
d.text(int(0.4*EMU), y+int(2.95*EMU), int(6.3*EMU), int(1.9*EMU), [
    f"dark current  {float(DK['Fit']['All']['SlopeSpread']['Median']):.4f} ADU/s "
    f"= {float(DK['Fit']['All']['SlopeSpread']['Median'])/G:.4f} e-/s",
    f"DSNU, pixel to pixel  {100*float(DK['Local']['DC']['RelIntr']):.2f} % of the dark current",
    '',
    'The spread over the whole die is a 2:1 ramp along the readout',
    'direction, not pixel-to-pixel variation — the map shows why the',
    'two numbers must be quoted separately.'], 1150, False, '28406B')

# ------------------------------------------------------------------ 6. gain
y = slide('General behaviour — conversion gain', CTX,
          kicker='Left: where the gain comes from (new plot). Right: how much it varies between pixels.')
d.picture(os.path.join(OLD, 'fig_ptc_window.png'),
          int(0.4*EMU), y, int(6.4*EMU), int(2.5*EMU))
cap(int(0.4*EMU), y+int(2.5*EMU), int(6.4*EMU),
    'Bright photon-transfer curve: slope = gain, intercept = g·T (not the read noise)')
d.picture(os.path.join(OLD, 'fig_ptc_gain_null.png'),
          int(7.1*EMU), y-int(0.05*EMU), int(5.8*EMU), int(3.9*EMU))
cap(int(7.1*EMU), y+int(3.85*EMU), int(5.8*EMU), 'Gain fitted to every pixel, against the identical-pixel null')
d.text(int(0.4*EMU), y+int(3.05*EMU), int(6.4*EMU), int(1.8*EMU), [
    f"gain  {G:.4f} ± {float(np.hypot(float(ME['Routes']['d']['GainStat']), float(ME['Routes']['d']['GainSyst']))):.4f} ADU/e-",
    f"between readout columns  {100*float(PT['Column']['RelIntr']):.2f} %",
    f"pixel to pixel  not detected, < {100*float(PT['Unmasked']['All']['IntrFromMAD']):.0f} % per pixel",
    '',
    'A per-pixel variance from 3 frames has 2 dof, so the per-pixel',
    'distribution is wide by construction — it is compared with a',
    'simulated null, not with zero.'], 1150, False, '28406B')

# ------------------------------------------------------------------ 7. response routes
y = slide('Threshold, routes 1–2: extrapolate the response to zero', CTX,
          kicker='Both ladders: the fitted points, the line, and its extrapolation to zero signal. '
                 '−intercept is the threshold.')
d.picture(os.path.join(OLD, 'fig_dark_ladder_fit.png'), int(0.35*EMU), y, int(12.6*EMU), int(2.3*EMU))
cap(int(0.35*EMU), y+int(2.28*EMU), int(12.6*EMU), 'Dark ladder (signal against exposure time)')
d.picture(os.path.join(OLD, 'fig_light_ladder_fit.png'), int(0.35*EMU), y+int(2.75*EMU), int(12.6*EMU), int(2.3*EMU))
cap(int(0.35*EMU), y+int(5.03*EMU), int(12.6*EMU),
    f"Bright ladder (signal against intensity).   dark route {Told['a'][0]:.1f} ± {Told['a'][1]:.1f} e-,"
    f"   light route {Told['b'][0]:.1f} ± {Told['b'][1]:.1f} e-")

# ------------------------------------------------------------------ 8. response, per pixel
y = slide('Threshold, routes 1–2: pixel-to-pixel distributions', CTX,
          kicker='The same two routes, fitted to every pixel. The dashed curve is pure fit noise — '
                 'what is left over is the real pixel-to-pixel spread.')
d.picture(os.path.join(OLD, 'fig_dark_threshold.png'), int(0.4*EMU), y, int(6.2*EMU), int(4.0*EMU))
cap(int(0.4*EMU), y+int(4.0*EMU), int(6.2*EMU),
    f"Dark route: {float(DK['Local']['T']['StdIntr'])/G:.1f} e- pixel to pixel after deconvolution")
d.picture(os.path.join(OLD, 'fig_light_threshold.png'), int(6.8*EMU), y, int(6.2*EMU), int(4.0*EMU))
cap(int(6.8*EMU), y+int(4.0*EMU), int(6.2*EMU),
    f"Light route: {float(LT['Threshold']['StdIntr'])/G:.1f} e- pixel to pixel after deconvolution")

# ------------------------------------------------------------------ 9. PTC routes
y = slide('Threshold, routes 3–4: the shot noise, which extrapolates nothing', CTX,
          kicker='Variance − RN² against signal for both ladders. The intercept is g·T, so the threshold '
                 'is read from the noise rather than from a curved response.')
d.picture(os.path.join(OLD, 'fig_ptc_both.png'), int(0.5*EMU), y, int(12.3*EMU), int(4.0*EMU))
cap(int(0.5*EMU), y+int(4.0*EMU), int(12.3*EMU),
    f"dark-ladder PTC {Told['c'][0]:.1f} ± {Told['c'][1]:.1f} e-,   "
    f"bright-ladder PTC {Told['d'][0]:.1f} ± {Told['d'][1]:.1f} e-   "
    f"(the dark points sit below the bright line — the 'dark deficit')")

# ------------------------------------------------------------------ 10. PTC per pixel
y = slide('Threshold, routes 3–4: pixel-to-pixel distributions', CTX,
          kicker='A photon-transfer fit to every pixel, on each ladder separately.')
d.picture(os.path.join(OLD, 'fig_pp_params.png'), int(0.4*EMU), y, int(6.3*EMU), int(4.0*EMU))
cap(int(0.4*EMU), y+int(4.0*EMU), int(6.3*EMU), 'Per-pixel gain and intercept, both ladders, against the null')
d.picture(os.path.join(OLD, 'fig_pp_diff.png'), int(6.9*EMU), y, int(6.1*EMU), int(4.0*EMU))
cap(int(6.9*EMU), y+int(4.0*EMU), int(6.1*EMU), 'Difference of the two gains, pixel by pixel')

# ------------------------------------------------------------------ 11. the ~50 e- gap
y = slide('Where the ~50 e- difference came from — and what fixes it',
          f"TH02954 {DIE} · run 31 (setting A) · the run in which the gap was seen",
          kicker='The two response routes disagreed because they were fitted over different signal '
                 'ranges. Fitting both over 100–1000 ADU removes most of the gap.')
d.picture(os.path.join(OLD31, 'fig_dark_ladder_fit.png'), int(0.35*EMU), y, int(6.3*EMU), int(2.0*EMU))
d.picture(os.path.join(OLD31, 'fig_light_ladder_fit.png'), int(0.35*EMU), y+int(2.1*EMU), int(6.3*EMU), int(2.0*EMU))
cap(int(0.35*EMU), y+int(4.1*EMU), int(6.3*EMU), 'BEFORE — each ladder over its own best range')
d.picture(os.path.join(NEW31, 'fig_dark_ladder_fit.png'), int(6.85*EMU), y, int(6.3*EMU), int(2.0*EMU))
d.picture(os.path.join(NEW31, 'fig_light_ladder_fit.png'), int(6.85*EMU), y+int(2.1*EMU), int(6.3*EMU), int(2.0*EMU))
cap(int(6.85*EMU), y+int(4.1*EMU), int(6.3*EMU), 'AFTER — both ladders over 100–1000 ADU')
gap_o = abs(T31old['a'][0]-T31old['b'][0]); gap_n = abs(T31new['a'][0]-T31new['b'][0])
d.rect(int(0.35*EMU), y+int(4.55*EMU), int(12.8*EMU), int(0.62*EMU), 'F2F4F7')
d.text(int(0.55*EMU), y+int(4.62*EMU), int(12.5*EMU), int(0.5*EMU),
       f"dark route {T31old['a'][0]:.1f} → {T31new['a'][0]:.1f} e-     "
       f"light route {T31old['b'][0]:.1f} → {T31new['b'][0]:.1f} e-     "
       f"GAP {gap_o:.1f} → {gap_n:.1f} e-     "
       f"dark-ladder PTC {T31old['c'][0]:.1f} → {T31new['c'][0]:.1f} e-",
       1250, True, '28406B')

# ------------------------------------------------------------------ 12. comparison
def spread_of(suffix):
    '''median over die-runs of the max-min threshold across the four routes'''
    out = []
    for run in ('31', '32', '38', '38-2'):
        for die in ('W04_D05', 'W04_D07', 'W08_D02', 'W08_D04'):
            M = J(os.path.join(A.root, f'run{run}_{die}_high{suffix}'), 'methods.json')
            if M:
                v = [float(M['Routes'][k]['Threshold_e']) for k in 'abcd']
                out.append((f'{die} r{run}', max(v)-min(v)))
    return out

fold, fnew = os.path.join(A.summary, 'fig_sum_threshold.png'), \
             os.path.join(getattr(A, 'summary_sig'), 'fig_sum_threshold.png')
sold, snew = spread_of(''), spread_of('_sig')
both = os.path.isfile(fnew) and snew
y = slide('Comparison across dies, flavours and runs',
          kicker=('Four threshold routes on every die-run, before and after fitting both ladders over '
                  'the same 100-1000 ADU.' if both else
                  'Four threshold routes on every die-run. x axis: die, gain half and setup.'))
if both:
    d.picture(fold, int(0.35*EMU), y, int(6.3*EMU), int(3.5*EMU))
    cap(int(0.35*EMU), y+int(3.5*EMU), int(6.3*EMU), 'BEFORE — each ladder over its own best range')
    d.picture(fnew, int(6.85*EMU), y, int(6.3*EMU), int(3.5*EMU))
    cap(int(6.85*EMU), y+int(3.5*EMU), int(6.3*EMU), 'AFTER — both ladders over 100-1000 ADU (same 16 die-runs)')
    # The aggregate alone would mislead: the common window helps where the dark
    # ladder is long and hurts where it is short, and those are different runs.
    def med(sel):
        o = [v for k, v in sold if sel(k)]; n = [v for k, v in snew if sel(k)]
        nb = sum(1 for a, b in zip(o, n) if b < a)
        return float(np.median(o)), float(np.median(n)), nb, len(n)
    hi = med(lambda k: k.endswith('r31'))
    lo = med(lambda k: not k.endswith('r31'))
    d.rect(int(0.35*EMU), y+int(3.95*EMU), int(12.8*EMU), int(1.25*EMU), 'F2F4F7')
    d.text(int(0.6*EMU), y+int(4.02*EMU), int(12.3*EMU), int(1.1*EMU), [
        'Spread between the four routes, median:',
        f'   run 31, long dark ladder:  {hi[0]:.0f} -> {hi[1]:.0f} e-   (narrower on {hi[2]} of {hi[3]})',
        f'   runs 32 / 38 / 38-2, short dark ladder:  {lo[0]:.0f} -> {lo[1]:.0f} e-   '
        f'(narrower on only {lo[2]} of {lo[3]})'], 1150, True, '28406B')
    d.text(int(0.6*EMU), y+int(5.3*EMU), int(12.3*EMU), int(0.5*EMU),
           'A common window helps where the dark ladder is long enough to reach it, and hurts where it '
           'is not: on the low dark-current runs it forces the dark fit into 100-200 ADU, high on a '
           'convex ladder, which raises the dark-response threshold.   Run 35 adds 10 more dies on '
           'these settings (a second lot, 7 new wafers, all flavour 6); it has no default-window '
           'counterpart, so it is in the full cross-die summary rather than in this before/after.',
           1050, False, GREY)
else:
    d.picture(fold, int(0.5*EMU), y, int(12.3*EMU), int(4.3*EMU))
    cap(int(0.5*EMU), y+int(4.3*EMU), int(12.3*EMU),
        'Wafer 04 = flavour 6, wafer 08 = flavour 2. Runs 32 / 38 / 38-2 are V_TX 3.3 / 3.5 / 3.7 V.')

# ------------------------------------------------------------------ 13. conclusions
y = slide('Conclusions')
mx_o = max(Told[k][0] for k in 'abcd'); mx_n = max(Tnew[k][0] for k in 'abcd') if Tnew else float('nan')
d.text(int(0.6*EMU), y, W-int(1.2*EMU), int(4.6*EMU), [
    f'•  Flavour 6 on the aSpect setting (run 32): the charge threshold is below '
    f'{np.ceil(max(mx_o, mx_n)):.0f} e- on all four methods',
    f'    — {mx_o:.1f} e- on the per-ladder windows and {mx_n:.1f} e- when both ladders are fitted '
    f'over 100–1000 ADU.',
    '',
    f'•  The ~50 e- difference between the two response routes is understood: it was the two ladders '
    f'being fitted over',
    f'    different signal ranges. On run 31 the gap falls from {gap_o:.0f} e- to {gap_n:.0f} e- when '
    f'both use 100–1000 ADU.',
    '',
    f'•  Pixel-to-pixel variation of the threshold is of order {float(DK["Local"]["T"]["StdIntr"])/G:.0f}–'
    f'{float(LT["Threshold"]["StdIntr"])/G:.0f} e- on this die, after the fit noise is deconvolved',
    '    (it is larger on run 31, where the ladders are noisier).',
    '',
    '•  We recommend moving forward with wafer-level testing of all dies.'], 1500, False, '1A1A1A')
d.rect(int(0.6*EMU), int(6.25*EMU), W-int(1.2*EMU), int(0.75*EMU), 'FDF1F0')
d.text(int(0.8*EMU), int(6.33*EMU), W-int(1.6*EMU), int(0.6*EMU),
       f"To check before circulating: on the common 100–1000 ADU window the dark-response route gives "
       f"{Tnew['a'][0]:.1f} ± {Tnew['a'][1]:.1f} e-, so the \"≤ 20 e- in all methods\" wording holds "
       f"for the per-ladder windows but not for the common one.", 1050, False, RED)

d.save(A.out)
print('wrote', A.out, f'({len(d.slides)} slides)')
