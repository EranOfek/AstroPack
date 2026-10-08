#!/usr/bin/env python3
"""Report on one run / wafer / die / gain half, from the desy_die_* stage dumps.

Reads the six json files the chain writes and builds report.md / report.html
in the stage output directory, so the figure paths stay relative. The stage 1
figures live in their own directory and are copied in.
"""
import argparse, json, os, shutil
import numpy as np

P = argparse.ArgumentParser()
P.add_argument('--indir', default='/home/sasha/claude/desy_die/run32_W04_D07_high')
P.add_argument('--rndir', default='/home/sasha/claude/desy_rn/run32_W04_D07_high')
A = P.parse_args()
OUT = A.indir

def load(name, where=None):
    with open(os.path.join(where or OUT, name)) as fh:
        return json.load(fh)

Z  = load('stats.json', A.rndir)
D  = load('dark.json')
L  = load('light.json')
BC = load('badcol.json')
PT = load('ptc.json')
BU = load('budget.json')
ME = load('methods.json') if os.path.isfile(os.path.join(OUT, 'methods.json')) else None
VS = load('varspread.json') if os.path.isfile(os.path.join(OUT, 'varspread.json')) else None
LS = load('lowsignal.json') if os.path.isfile(os.path.join(OUT, 'lowsignal.json')) else None
DW = load('darkwindow.json') if os.path.isfile(os.path.join(OUT, 'darkwindow.json')) else None
FW = load('fitwindow.json') if os.path.isfile(os.path.join(OUT, 'fitwindow.json')) else None
PB = load('ptc_both.json')  if os.path.isfile(os.path.join(OUT, 'ptc_both.json'))  else None
PX = load('ptc_points.json') if os.path.isfile(os.path.join(OUT, 'ptc_points.json')) else None
RP = load('rnplots.json')   if os.path.isfile(os.path.join(OUT, 'rnplots.json'))   else None
DP = load('darkplots.json') if os.path.isfile(os.path.join(OUT, 'darkplots.json')) else None
CH = load('chain.json')     if os.path.isfile(os.path.join(OUT, 'chain.json'))     else None

def steps_of(S):
    st = S['Steps']
    return [st] if isinstance(st, dict) else st

def resid(S):
    '''per-step residual of the median signal to that ladder's own fit'''
    x  = np.atleast_1d(np.array(S['PatternX'], dtype=float))
    m  = np.atleast_1d(np.array(S['Pattern']['All']['Median'], dtype=float))
    n  = np.atleast_1d(np.array(S['PatternStep'], dtype=int))
    a  = float(S['Fit']['All']['SlopeSpread']['Median'])
    b  = float(S['Fit']['All']['InterceptSpread']['Median'])
    fs = set(np.atleast_1d(np.array(S['FitSteps'], dtype=int)).tolist())
    return n, x, m, m - (a*x + b), fs

def chi2ratio(S):
    return float(S['Fit']['All']['MedianChi2Dof'])/float(S['Chi2DofExpected'])

for f in os.listdir(A.rndir):
    if f.endswith('.png'):
        shutil.copyfile(os.path.join(A.rndir, f), os.path.join(OUT, f))

TAG  = f"{PT['Lot']} {PT['Die']}, run {PT['Run']}, {PT['GainHalf']}-gain half"
NY, NX = [int(v) for v in PT['Size']]
UM   = BU['Unmasked']
QLIM = [float(UM[k]['Qlim_cal_5']) for k in ('PTC', 'Dark', 'Light')]
IN   = UM['Inputs']
G    = float(IN['GainADU'])
TT   = float(BU['ExpTime'])

# ---- the dark current, with an error, and with the span of the window choice.
# Routes.a is the slope of the dark ladder and carries both errors already: the
# scatter between blocks of the die, and the spread over the windows it was
# refitted on. The datasheet table and section 4 quote the median pixel, a
# different estimator of the same slope, so those errors are carried across as
# relative ones rather than invented afresh.
# DCLO..DCHI is the honest span of the window choice: every contiguous window of
# three or more steps inside the linear range, refitted. Route a's systematic
# covers only the two to four variants of the chosen window, which on a curved
# ladder is much narrower, so both numbers are reported.
LINLIM = float(DW['LinLimit']) if (DW and 'LinLimit' in DW) else 2900.0   # DieLinLimit
DCA = float(ME['Routes']['a']['Slope'])     if ME else None
DCAS = float(ME['Routes']['a']['SlopeStat']) if ME else None
DCAY = float(ME['Routes']['a']['SlopeSyst']) if ME else None
RSB = float(ME['Routes']['b']['Slope'])     if ME else None
RSBS = float(ME['Routes']['b']['SlopeStat']) if ME else None
RSBY = float(ME['Routes']['b']['SlopeSyst']) if ME else None
def _dcspan():
    '''(lo, hi, nwindows) of the ensemble dark current over every contiguous
    window of >=3 steps within the linear range. The chi2-mode dump already
    holds that grid; in signal mode it is refitted here from the per-step means
    the window stage measured.'''
    if DW and DW.get('Scan'):
        v = [float(q['DC']) for q in DW['Scan']]
        return min(v), max(v), len(v)
    if FW:
        x = np.atleast_1d(np.array(FW['D']['X'], dtype=float))
        y = np.atleast_1d(np.array(FW['D']['SignalMean'], dtype=float))
        kmax = int(np.argmax(y > LINLIM)) if np.any(y > LINLIM) else y.size
        v = [np.polyfit(x[i:j+1], y[i:j+1], 1)[0]
             for i in range(kmax) for j in range(i+2, kmax)]
        if v:
            return float(min(v)), float(max(v)), len(v)
    return None, None, 0
DCLO, DCHI, DCNW = _dcspan()
def _darkgof():
    '''residuals of the ensemble dark ladder to its own straight line, against
    the error on each step mean. With 22.5 M pixels that error is ~0.002 ADU, so
    a curved ladder gives an enormous chi2 and the formal fit error on the slope
    is meaningless - which is why the error quoted is the block scatter.'''
    x  = np.atleast_1d(np.array(D['ExpTime'], dtype=float))
    y  = np.atleast_1d(np.array(D['StepMedian'], dtype=float))
    v  = np.atleast_1d(np.array(D['VarStep'], dtype=float))
    nf = np.atleast_1d(np.array(D['PatternNframes'], dtype=float))[
         np.atleast_1d(np.array(D['FitSteps'], dtype=int)) - 1]
    r  = y - np.polyval(np.polyfit(x, y, 1), x)
    se = np.sqrt(v/(NY*NX*nf))
    wt = 1.0/se**2                                  # the formal error on the slope
    xb = float(np.sum(wt*x)/np.sum(wt))             # of that same weighted fit
    sf = float(1.0/np.sqrt(np.sum(wt*(x - xb)**2)))
    return r, se, float(np.sum((r/se)**2)), max(x.size - 2, 1), sf
DGR, DGSE, DGCHI2, DGDOF, DGSEF = _darkgof()
MD   = []
def w(t):
    MD.append(t)
def fig(name, cap):
    w(f'![{cap}]({name})\n')
    w(f'*{cap}*\n')
def f3(x, n=3):
    return ('%.' + str(n) + 'f') % float(x)
def pm(val, ref, stat, syst, n=3):
    '''val with the relative errors of ref carried across. Used where the
    datasheet quotes one estimator of a slope (the median pixel) and section 11
    carries the errors on another (the mean over pixels): the errors are
    fractions of the slope, so they transfer, and inventing a second pair would
    be worse than saying which fit they came from.'''
    if ref is None:
        return f3(val, n)
    return (f"{f3(val, n)} &plusmn; {f3(abs(val)*stat/abs(ref), n)} "
            f"&plusmn; {f3(abs(val)*syst/abs(ref), n)}")
def nfr(S):
    return int(np.sum(np.atleast_1d(np.array(S['PatternNframes'], dtype=float))))
ND, NB = nfr(D), nfr(L)
# what a cached whole-die ladder of both ladders would cost in memory, as single
NFRAME_GB = f'{NY*NX*(ND+NB)*4/1e9:.0f}'
NFRAME = f"{int(CH['Nfiles'])}" if CH else f"{ND + NB + int(Z['Nframes'])}"
VOLUME = f" and {float(CH['Bytes'])/1e9:.0f} GB" if CH else ''
# How long the chain took on this die. The runner records it; without that record
# the table simply has no time column rather than an invented one.
STIME = {d['Name']: float(d['Seconds']) for d in CH['Stages']} if CH else {}
# Stage 1 depends on no fit window, so signal mode shares the default mode's run
# of it and does not record its time here. Take it from the default-mode chain
# record of the same die rather than printing a nan. This die's own times win.
_base = os.path.basename(OUT)
for _suf in ('_off', '_sig'):
    if _base.endswith(_suf):
        _base = _base[:-len(_suf)]
if _base != os.path.basename(OUT):
    _sib = os.path.join(os.path.dirname(OUT), _base, 'chain.json')
    if os.path.isfile(_sib):
        with open(_sib) as fh:
            for _d in json.load(fh)['Stages']:
                STIME.setdefault(_d['Name'], float(_d['Seconds']))
TOTMIN = (f"about {float(CH['TotalSeconds'])/60:.0f} minutes end to end"
          if CH else 'a quarter of an hour or so end to end')

# ================================================================= datasheet
w(f'# {PT["Lot"]} {PT["Die"]} — single-die characterisation, run {PT["Run"]}\n')
w(f'Whole die, {NY}x{NX} = {NY*NX/1e6:.1f} M pixels of the {PT["GainHalf"]}-gain half, '
  f'individual pixels throughout, every statistic also split by readout-column parity. '
  f'{len(STIME) if STIME else 11} stages, {TOTMIN}.\n')

EXPOFF = float(D.get('ExpTimeOffset', 0) or 0)
if EXPOFF != 0:
    w(f"""> **This report is in CHARGE-COLLECTING TIME.** Every dark exposure has had
> {EXPOFF:.4f} s subtracted from it, because the commanded exposure of this tester is
> `t_exp = RO_time + Reset_delay` with `RO_time` the full-die readout
> (`zDUT_ExpTimeOffset` in the configuration, confirmed by DESY as
> 2 x 4742 rows x 1.3 ms = 12.3292 s), so the charge-collecting interval is
> `t_exp - RO_time`. The sensor exposure of the bright frames is corrected the
> same way, which moves the light-route threshold as well. The dark current and
> the conversion gain are unchanged by construction -- a shift of the time axis
> cannot alter a slope -- so what differs from the uncorrected report is the two
> response-route thresholds, the dark-threshold fixed pattern, and everything the
> noise budget derives from them.
""")
    w('')
w('## The die in one table\n')
rows = [
 ('Bias level', f"{f3(Z['All']['BiasLevel'],2)} ADU", '5 zero-exposure frames, stage 1'),
 ('Read noise (median)', f"{f3(Z['All']['ReadNoiseMedian'],3)} ADU = {f3(float(IN['RN_ADU'])/G,3)} e-",
  'per-pixel std over the ZE frames'),
 ('Read noise, even / odd columns', f"{f3(Z['Even']['ReadNoiseMedian'],3)} / {f3(Z['Odd']['ReadNoiseMedian'],3)} ADU",
  f'odd columns are {100*(float(Z["Odd"]["ReadNoiseMedian"])/float(Z["Even"]["ReadNoiseMedian"])-1):.1f} % noisier'),
 ('Bias fixed pattern', f"{f3(Z['All']['FixedPatternRMS'],2)} ADU", 'spatial spread, sampling noise removed'),
 ('Common mode', f"{f3(Z['CommonMode']['Std'],4)} ADU", 'frame-to-frame, clipped mean'),
 ('Conversion gain', (f"**{float(ME['Routes']['d']['Gain']):.4f}** &plusmn; {float(ME['Routes']['d']['GainStat']):.4f} &plusmn; {float(ME['Routes']['d']['GainSyst']):.4f} ADU/e-" if ME else f"{f3(G,4)} ADU/e-"),
  'bright-ladder PTC, section 11 (stat, syst)'),
 ('Conversion gain, dark ladder', (f"{float(ME['Routes']['c']['Gain']):.4f} &plusmn; {float(ME['Routes']['c']['GainStat']):.4f} &plusmn; {float(ME['Routes']['c']['GainSyst']):.4f} ADU/e-" if ME else '&mdash;'),
  (f"**{100*(1-float(ME['Routes']['c']['Gain'])/float(ME['Routes']['d']['Gain'])):.0f} % lower than "
   'the bright one — section 9**' if ME else '&mdash;')),
 ('Dark current', f"{pm(float(IN['DC_ADU']), DCA, DCAS, DCAY, 4)} ADU/s = {f3(float(IN['DC_ADU'])/G,4)} e-/s",
  f'weighted per-pixel fit, {len(np.atleast_1d(D["FitSteps"]))} steps; errors section 11 (blocks, window)'),
 ('Photo-response', f"{pm(float(L['Fit']['All']['SlopeSpread']['Median']), RSB, RSBS, RSBY, 0)} ADU per intensity unit",
  f'{len(np.atleast_1d(L["FitSteps"]))} steps of the bright ladder; errors section 11'),
 ('PRNU, pixel to pixel', f"{f3(100*float(IN['PRNU']),2)} %", '32x32 detrended, fit noise removed'),
 ('DSNU, pixel to pixel', f"{f3(100*float(D['Local']['DC']['RelIntr']),2)} % of the dark current",
  'same method'),
 ('Offset fixed pattern', f"{f3(float(IN['SigmaTdark_ADU'])/G,2)} e-", 'dark-threshold map, detrended'),
 ('Gain spread between columns', f"{f3(100*float(PT['Column']['RelIntr']),2)} %",
  'null-calibrated, see section 7'),
 ('Gain spread pixel to pixel', f"not detected (&lt; {100*float(PT['Unmasked']['All']['IntrFromMAD']):.0f} % per pixel)",
  f"observed spread {float(PT['Unmasked']['All']['MADoverNull']):.3f} x the null"),
 ('Bad readout columns', f"{int(BC['Nbad'])} of {int(BC['Nrawcol'])} ({100*(1-float(BC['GoodFraction'])):.2f} % of pixels)",
  f"{int(BC['NbadInPairs'])} of them in complete pairs"),
 ('**Charge threshold**', (f"**{float(ME['Routes']['c']['Threshold_e']):.1f} ± {float(ME['Routes']['c']['Threshold_e_err']):.1f}** / "
   f"{float(ME['Routes']['d']['Threshold_e']):.1f} / {float(ME['Routes']['b']['Threshold_e']):.1f} / "
   f"{float(ME['Routes']['a']['Threshold_e']):.1f} e-") if ME else
  f"{f3(float(PT['Thresholds']['PTC_e']),1)} e-", '**four routes disagree — section 11**'),
 ('**Limiting signal, SNR 5**', f"**{f3(min(QLIM),0)}** to {f3(max(QLIM),0)} e-",
  'calibrated; the range is the threshold'),
]
w('| quantity | value | how it was measured |\n|---|---|---|\n'
  + '\n'.join(f'| {a} | {b} | {c} |' for a, b, c in rows) + '\n')
if ME is not None:
    _T4 = sorted(float(ME['Routes'][k]['Threshold_e']) for k in 'abcd')
    w(f"The electron columns above are converted with the gain the noise budget of section 10 used, "
      f"{f3(G,4)} ADU/e-. Section 11's value is {float(ME['Routes']['d']['Gain']):.4f}; the two "
      f"differ by {100*abs(G/float(ME['Routes']['d']['Gain'])-1):.1f} %, which is the estimator "
      f"choice explained there and not a disagreement about the device.\n")
    w(f'The one number this report cannot give as a single value is the **charge threshold**. '
      f'Four independent routes give {", ".join(f"{v:.1f}" for v in _T4[:-1])} and {_T4[-1]:.1f} e-, '
      f'and they are mutually inconsistent beyond their errors; section 11 sets them out with those '
      f'errors, says which one I would use and what would settle it. The noise budget of section 10 '
      f'was computed earlier from three of the four and puts the smallest measurable signal between '
      f'{f3(min(QLIM),0)} and {f3(max(QLIM),0)} e-; a fourth route only widens that range, which is '
      f'why nothing in it needs redoing.\n')
else:
    w(f'The one number this report cannot give as a single value is the **charge threshold**. '
      f'Three independent routes give {f3(float(PT["Thresholds"]["PTC_e"]),1)}, '
      f'{f3(float(PT["Thresholds"]["Dark_e"]),1)} and {f3(float(PT["Thresholds"]["Light_e"]),1)} e-, '
      f'which moves the smallest measurable signal from {f3(min(QLIM),0)} to '
      f'{f3(max(QLIM),0)} e-. Section 11 says which one I would use and why, '
      'and what would settle it.\n')

# ================================================================= method
w('## 1. What was done\n')
_ST = [('desy_rn_single_die',    '1 bias and read noise',    f'{int(Z["Nframes"])} ZE frames',
        'bias, fixed pattern, per-pixel read noise'),
       # the window stage has a different name and a different job in the two
       # modes: chi2 mode scans the dark ladder alone, signal mode measures both
       # ladders to put one signal window on them
       (('desy_die_fitwindow', 'desy_die_darkwindow'),
        '2a fit window' if FW else '2a dark fit window',
        (f'{int(sum(np.atleast_1d(np.array(FW["D"]["Nframes"], dtype=float))) + sum(np.atleast_1d(np.array(FW["B"]["Nframes"], dtype=float)))) } frames'
         if FW else f'{ND} frames'),
        'one signal window for both ladders' if FW else 'which dark steps are the straight part'),
       ('desy_die_dark',         '2 dark ladder',            f'{ND} frames',
        'dark current, dark-route threshold, DSNU'),
       ('desy_die_light',        '3 bright ladder',          f'{NB} frames',
        'response, PRNU, light-route threshold'),
       ('desy_die_badcol',       '4 bad columns',            'nothing',
        'the column mask'),
       ('desy_die_ptc',          '5 PTC',                    'the PTC window',
        'conversion gain, shot-noise threshold'),
       ('desy_die_budget',       '6 noise budget',           'nothing',
        'sigma_eff, SNR, limiting signal'),
       ('desy_die_varspread',    '7 variance distributions', f'{NFRAME} frames',
        'how much the variance differs between pixels'),
       ('desy_die_lowsignal',    '8 low-signal prediction',  'the low steps of both ladders',
        'is that variance explained, pixel by pixel'),
       ('desy_die_ptc_perpixel', '9 per-pixel PTC',          'both ladders',
        'a gain per pixel on each ladder'),
       ('desy_die_methods',      '10 four routes',           'nothing',
        'gain and threshold with errors')]
_hdr = '| stage | reads | time | what it settles |\n|---|---|---|---|'
if not STIME:
    _hdr = '| stage | reads | what it settles |\n|---|---|---|'
_rows = []
for _nm_, _lb, _rd, _wh in _ST:
    # a stage can be recorded under either of two names, and a stage this die
    # never ran says so rather than printing a nan
    _cand = (_nm_,) if isinstance(_nm_, str) else tuple(_nm_)
    _sec  = next((STIME[c] for c in _cand if c in STIME), None)
    if STIME:
        _rows.append(f'| {_lb} | {_rd} | ' +
                     (f'{_sec:.0f} s' if _sec is not None else 'not run') + f' | {_wh} |')
    else:
        _rows.append(f'| {_lb} | {_rd} | {_wh} |')
STAGE_TABLE = _hdr + '\n' + '\n'.join(_rows)

w(f"""One configuration, one die, nothing averaged into superpixels. The dataset is a
{len(np.atleast_1d(np.array(D['PatternStep'])))}-step dark and
{len(np.atleast_1d(np.array(L['PatternStep'])))}-step bright ladder with
{int(np.atleast_1d(np.array(D['PatternNframes']))[0])} frames each
plus {int(Z['Nframes'])} zero-exposure frames, {NFRAME} TIFF files{VOLUME}, read once per stage.

The whole die is processed in a **streamed** mode: each ladder step is read, reduced to a mean
and a variance map, accumulated into the running sums of a weighted per-pixel straight-line fit,
and discarded. Only the sums survive, so {NY*NX/1e6:.1f} M pixels cost a few GB rather than the
~{NFRAME_GB} GB a cached whole-die ladder would need, and every frame is read exactly once — the residual sum of
squares is expanded from the sums instead of being accumulated in a second pass.

{STAGE_TABLE}

Stages 4 and 6 read no frames at all: they consume the maps the earlier stages wrote. Each stage
validates the dumps it inherits against the dataset it was asked for, so a stale map cannot leak
into a later stage unnoticed.

Points are weighted by their **measured** variance, never by a model of the signal. The shot noise
of a ladder point follows the charge actually collected, not the signal recorded, and when a
threshold removes the first electrons after they have already fluctuated no model of the measured
signal reproduces that. The weighting is checked model-free at every stage by comparing the median
chi2 per degree of freedom with its expectation: __CHI2__.
""")

# ================================================================= stage 1
# The dark window is chosen per die, so the report says which window this die got
# and shows the whole scan: a later change of tolerance is then a re-reading of
# this table, not a rerun of the chain.
if DW is not None:
    _sc = DW['Scan'] if isinstance(DW['Scan'], list) else [DW['Scan']]
    _ch = set(np.atleast_1d(np.array(DW['Chosen'], dtype=int)).tolist())
    _lines = []
    for _e in sorted(_sc, key=lambda e: (-int(e['Nsteps']), -float(e['Lever']))):
        _st = np.atleast_1d(np.array(_e['Steps'], dtype=int)).tolist()
        _mk = ' **chosen**' if set(_st) == _ch else (' *eligible*' if _e.get('Anchored') else '')
        _lines.append(f"| [{' '.join(str(v) for v in _st)}]{_mk} | "
                      f"{np.atleast_1d(np.array(_e['Median'],dtype=float))[0]:.0f}-"
                      f"{np.atleast_1d(np.array(_e['Median'],dtype=float))[-1]:.0f} | "
                      f"{float(_e['Chi2Dof']):.4f} | {float(_e['Chi2Exp']):.4f} | "
                      f"{float(_e['Ratio']):.3f} | {float(_e['DC']):.4f} | {float(_e['Tdark']):.1f} |")
    _dcs = [float(e['DC']) for e in _sc]
    _tds = [float(e['Tdark']) for e in _sc]
    DRIFT_TEXT = (f"Across the {len(_sc)} candidate windows of this ladder the dark current moves "
                  f"{100*(max(_dcs)/min(_dcs)-1):.0f} % and the dark threshold by a factor "
                  f"{max(_tds)/min(_tds):.1f} ({min(_tds):.1f} to {max(_tds):.1f} ADU).")
    DARK_WINDOW_TEXT = f"""
The dark window is not a setting: it is chosen for this die by goodness of fit, because the
bias-board setups of this lot differ by up to a factor 22 in dark current and so put their dark
ladders in signal ranges that barely overlap. The window is widened **downward from the highest
step below the {float(DW['LinLimit']):.0f} ADU linearity limit**, and the widest one whose median
chi2/dof is within {100*(float(DW['Tol'])-1):.0f} % of its expectation is taken; ties go to the
longest lever arm.

The anchor at the top is what makes the criterion mean anything. A goodness of fit on its own
rewards the windows where the data constrain the line *least*: at the bottom of a dark ladder the
signal is a few ADU, the per-point variance is read-noise dominated and large, and curvature hides
inside it, so a low window can pass the tolerance that every informative window fails. The dark
current wanted here is the asymptotic slope, reached once the threshold has been overcome, which
is the top of the ladder by construction. Rows marked *eligible* below are the anchored ones; the
rest are scanned only to measure how far the answer moves with the window.

On this die the choice is **[{' '.join(str(v) for v in sorted(_ch))}]**
({np.atleast_1d(np.array(DW['ChosenMedian'],dtype=float))[0]:.0f}-{np.atleast_1d(np.array(DW['ChosenMedian'],dtype=float))[-1]:.0f} ADU),
at a ratio of {float(DW['ChosenRatio']):.3f}. The whole scan:

| window | medians [ADU] | chi2/dof | expected | ratio | DC [ADU/s] | T [ADU] |
|---|---|---|---|---|---|---|
{chr(10).join(_lines)}

The expectation is not 1: the median of a chi2/dof distribution with few degrees of freedom sits
below its mean ({float(_sc[0]['Chi2Exp']):.3f} for {int(_sc[0]['Nsteps'])-2} dof), and comparing a
median against 1 would reject every correct fit.
"""
else:
    DRIFT_TEXT = ('The dark current moves a few per cent between the widest and narrowest windows; '
                  'the dark threshold moves by a factor of a few.')
    DARK_WINDOW_TEXT = ''

w('## 2. Both ladders are curved, and that is why the windows are what they are\n')
_nd, _xd, _md, _rd, _fd = resid(D)
_nl, _xl, _ml, _rl, _fl = resid(L)
w(f"""Neither ladder is a straight line, and every threshold in this report is an extrapolation of one
of them to zero signal, so the curvature decides more than the fit window does.

The **dark ladder** is fitted on steps {sorted(_fd)} ({float(np.atleast_1d(np.array(D['ExpTime'],dtype=float))[0]):.0f}-{float(np.atleast_1d(np.array(D['ExpTime'],dtype=float))[-1]):.0f} s). Residuals of
every step to that fit, in ADU:

| step | {' | '.join(str(int(n)) for n in _nd)} |
|---|{'---|'*len(_nd)}
| exposure [s] | {' | '.join(f'{v:.0f}' for v in _xd)} |
| median signal [ADU] | {' | '.join(f'{v:.1f}' for v in _md)} |
| residual [ADU] | {' | '.join(f'{v:+.2f}' + ('*' if int(n) in _fd else '') for n, v in zip(_nd, _rd))} |

The **bright ladder** is fitted on steps {sorted(_fl)} ({float(_ml[0]):.0f}-{float(_ml[max(_fl)-1]):.0f} ADU):

| step | {' | '.join(str(int(n)) for n in _nl[:9])} |
|---|{'---|'*9}
| median signal [ADU] | {' | '.join(f'{v:.0f}' for v in _ml[:9])} |
| residual [ADU] | {' | '.join(f'{v:+.1f}' + ('*' if int(n) in _fl else '') for n, v in zip(_nl[:9], _rl[:9]))} |

{DARK_WINDOW_TEXT}
(* marks the fitted steps.)
__LADDERFIGS__
Both run the same way: the residuals are positive away from the fitted
range on the side of **lower** responsivity, which means the response per unit charge **rises with
signal**. On the bright ladder the local slope goes from about {(_ml[4]-_ml[0])/(_xl[4]-_xl[0]):.0f} ADU per intensity unit
between steps 1 and 5 to about {(_ml[6]-_ml[4])/(_xl[6]-_xl[4]):.0f} between steps 5 and 7, a rise of
{100*(((_ml[6]-_ml[4])/(_xl[6]-_xl[4]))/((_ml[4]-_ml[0])/(_xl[4]-_xl[0]))-1):.1f} %. The dark ladder behaves the same way against exposure time.

This is the single fact behind the threshold disagreement in section 11. A convex response has no
unique intercept: every window extrapolates its own local tangent to zero and gets a different
answer, and the slope is robust while the intercept is not. {DRIFT_TEXT}
""")

w('## 3. Bias and read noise\n')
w(f"""Median read noise **{f3(Z['All']['ReadNoiseMedian'],3)} ADU**
({f3(float(IN['RN_ADU'])/G,3)} e- at the measured gain), bias level
{f3(Z['All']['BiasLevel'],2)} ADU, bias fixed pattern {f3(Z['All']['FixedPatternRMS'],2)} ADU, and a
frame-to-frame common mode of only {f3(Z['CommonMode']['Std'],4)} ADU.

With 5 frames each pixel's sigma carries 4 degrees of freedom and so about 50 % sampling scatter:
most of the width of the observed distribution is the measurement, not real pixel-to-pixel
variation, which is why every distribution below is drawn against the curve expected if every
pixel were identical.
""")
_bd = Z.get('BiasDist')
if _bd:
    _q = np.atleast_1d(np.array(_bd['Quantiles'], dtype=float))
    w(f"""Four quite different numbers get called the error on that bias level, and they span three orders
of magnitude, so it is worth separating them once.

| what is being asked | value |
|---|---|
| error on **one pixel's** bias, RN/sqrt({int(Z['Nframes'])}) | {float(_bd['ErrOnePixel']):.2f} ADU |
| spread **across pixels**, sampling noise removed | {float(Z['All']['FixedPatternRMS']):.2f} ADU |
| error on the **die-level** value | **{float(_bd['ErrDieTotal']):.3f} ADU** |
| quantisation of a mean of {int(Z['Nframes'])} integers | {float(_bd['Quantisation']):.1f} ADU |

The second is not an error at all but real structure, which is why it is the one that enters a noise
budget. The third is the one to quote for the device, and its two parts are instructive: the pixel
statistics contribute only {float(_bd['ErrDieFromPixels']):.5f} ADU, while the frame-to-frame common
mode contributes {float(_bd['ErrDieFromCommonMode']):.5f} ADU and dominates. With
{float(Z['Npix'])/1e6:.1f} M pixels the spatial average is free; what limits the bias level is that
there are only {int(Z['Nframes'])} zero-exposure frames, so more pixels would not help and more
frames would.

The fourth matters for how the number is written. A mean of {int(Z['Nframes'])} integers can only land
on multiples of {float(_bd['Quantisation']):.1f} ADU, so the median of {float(_bd['Median']):.2f} is an
exact grid point and is not meaningful finer than that; the mean, {float(_bd['Mean']):.4f}, averages
over the grid and is. The distribution itself is far from Gaussian — MAD
{float(_bd['MAD']):.2f} ADU against a standard deviation of {float(_bd['Std']):.2f}, with percentiles
1/25/50/75/99 at {_q[0]:.1f} / {_q[1]:.0f} / {_q[2]:.0f} / {_q[3]:.0f} / {_q[4]:.1f} and
{100*float(_bd['TailFrac5MAD']):.2f} % of pixels beyond five MAD — so the core is about
{float(_bd['MAD']):.1f} ADU wide and the {float(Z['All']['FixedPatternRMS']):.1f} ADU fixed pattern is
carried by the tails and the large-scale structure.

This also settles the parity offset. By medians the two parities read
{float(Z['Even']['BiasLevel']):.2f} and {float(Z['Odd']['BiasLevel']):.2f}, a difference of exactly
{(float(Z['Odd']['BiasLevel'])-float(Z['Even']['BiasLevel']))*int(Z['Nframes']):.0f} quantisation
steps, which would be reason for suspicion. By means they read {float(Z['Even']['BiasMean']):.4f} and
{float(Z['Odd']['BiasMean']):.4f}, a difference of
{float(Z['Odd']['BiasMean'])-float(Z['Even']['BiasMean']):+.4f} ADU against a standard error of about
{float(_bd['Std'])/np.sqrt(float(Z['Npix'])/2):.4f} — real, and resolved to well under a per cent.
""")

fig('fig_rn_distribution.png', 'Read noise over the whole die. The dashed curve is what the same measurement would give if every pixel had the same noise. The parity comparison is made on the cumulative distribution, which is immune to the quantisation of a sigma built from integer frames.')
w(f"""Odd columns are {100*(float(Z['Odd']['ReadNoiseMedian'])/float(Z['Even']['ReadNoiseMedian'])-1):.1f} %
noisier than even ones. That effect is known, but the whole-die map shows it is not a parity
effect at all.
""")
fig('fig_rn_column_pairing.png',
    ('Readout columns 2k-1 and 2k share their noise amplitude: the median read noise of a column '
     f"correlates with its partner at r = {float(RP['PairR']):+.3f}, and with the next column across "
     f"the pair boundary at r = {float(RP['CrossR']):+.3f}. What is shared is the noise amplitude, "
     'not the samples.') if RP else
    ('Readout columns 2k-1 and 2k share their noise amplitude: the median read noise of a column '
     'correlates with its partner and not with the next column across the pair boundary.'))
w('The same pairing turns up again in the defects (section 6) and is absent from the dark current '
  '(section 4) — so whatever is shared sits in the readout chain, not in the pixel.\n')

# ================================================================= stage 2
w('## 4. Dark current\n')
dloc = D['Local']['DC']
w(f"""Dark current **{pm(float(IN['DC_ADU']), DCA, DCAS, DCAY, 4)} ADU/s** = {f3(float(IN['DC_ADU'])/G,4)} e-/s,
from a weighted fit of signal against exposure over the steps of section 2.
""")
if ME:
    w(f"""**That error is not the error of the fit, and it cannot be.** The ladder is averaged over
{NY*NX/1e6:.1f} M pixels, so each step mean is known to about {np.median(DGSE):.4f} ADU, while the
residuals of those means to their own straight line are
{', '.join('%+.2f' % q for q in DGR)} ADU: chi2 = {DGCHI2:.3g} on
{DGDOF} degree{'' if DGDOF == 1 else 's'} of freedom. The straight line is rejected outright — section 2
shows why, the ladder is curved — so the formal error such a fit hands back,
{DGSEF:.2g} ADU/s, is {DCAS/DGSEF:.0f} times smaller than the error quoted above and
describes nothing about this device. What is quoted instead is the scatter between the
{int(ME['NBlock'])**2} blocks of the die, and then the spread over the fit windows refitted; section 11
sets out both for all four routes.
""")
    if DCNW:
        w(f"""**And the window term is a lower bound.** The
&plusmn;{float(IN['DC_ADU'])*DCAY/DCA:.4f} ADU/s above is the spread over the
{len(ME['Routes']['a']['WindowValues'])} variants of the chosen window. Refitting this same ladder over
*every* contiguous window of three or more steps inside the {LINLIM:.0f} ADU linear range
({DCNW} windows) puts the slope anywhere between **{DCLO:.4f}** and **{DCHI:.4f} ADU/s** — a half-range
{0.5*(DCHI-DCLO)*DCA/(DCAY*float(IN['DC_ADU'])):.0f} times the systematic quoted above. That range is
where a straight line lands depending on which steps it is given, not an error bar around the value
above; the quoted systematic covers only the part of it the chosen window is exposed to, so this dark
current should not be quoted to better than its window.
""")
w(f"""The spread needs care, and this is the first of three places in this report where the obvious answer was wrong.
Over the whole die the dark current spreads **{100*float(D['Fit']['All']['SlopeSpread']['RelIntr']):.1f} %**
of its median — but the map shows why.
""")
fig('fig_dark_dc_map.png', 'Dark current per pixel. The spread over the die is a 2:1 ramp along the readout columns plus banding and a hot row, not pixel-to-pixel variation.')
w(f"""A noise budget asks what varies between *neighbouring* pixels, because a slow ramp is removed
by any flat field. Taking the residual to a 32x32 block median and removing the fit noise in
quadrature leaves **{100*float(dloc['RelIntr']):.2f} %**. On run 32 W04_D07, where the same
quantity was measured independently inside the DESY 100x100 analysis region, the two agreed
(5.62 % local against 5.72 % in the region) — that agreement is the check that the detrending
removes structure rather than signal. Every fixed-pattern number in this report is the local one.
""")
_is = D['Fit']['All']['InterceptSpread']
fig('fig_dark_threshold.png', f"Dark-route threshold, T = -intercept. The observed spread is "
    f"{float(_is['StdRobust']):.2f} ADU of which {float(_is['StdFitRobust']):.2f} is fit noise, leaving "
    f"{float(_is['StdIntr']):.2f} ADU over the die and {float(D['Local']['T']['StdIntr']):.2f} pixel to pixel. "
    f"Without that deconvolution the die would look "
    f"{100*(1-float(_is['StdIntr'])/float(_is['StdRobust'])):.0f} % less uniform than it is.")
_gr = BC.get('Gradient')
if _gr:
    _g3 = lambda k: np.atleast_1d(np.array(_gr[k], dtype=float))
    w(f"""The most prominent feature of that map is the bright band along one edge, and it is worth saying
what it is, because it is neither a patch nor an edge. The dark current rises **monotonically along
the readout-column direction** across the whole die, by a factor
{float(_gr['DCRatio']):.2f} from one end to the other; the band is simply the hot end of that ramp,
picked out by the colour scale. Taking the die in thirds of raw column, band 1 being the columns read
out first:

| | band 1 (first read) | band 2 | band 3 (last read) |
|---|---|---|---|
| raw column | {_g3('BandRawCol')[0]:.0f} | {_g3('BandRawCol')[1]:.0f} | {_g3('BandRawCol')[2]:.0f} |
| dark current [ADU/s] | **{_g3('DC')[0]:.4f}** | {_g3('DC')[1]:.4f} | **{_g3('DC')[2]:.4f}** |
| bias [ADU] | {_g3('Bias')[0]:.2f} | {_g3('Bias')[1]:.2f} | {_g3('Bias')[2]:.2f} |
| read noise [ADU] | {_g3('RN')[0]:.3f} | {_g3('RN')[1]:.3f} | {_g3('RN')[2]:.3f} |
| **photo-response [ADU/int]** | {_g3('Resp')[0]:.0f} | {_g3('Resp')[1]:.0f} | {_g3('Resp')[2]:.0f} |
| dark threshold [ADU] | {_g3('Tdark')[0]:.2f} | {_g3('Tdark')[1]:.2f} | {_g3('Tdark')[2]:.2f} |

Two things that rules out. **It is not amplifier glow**: glow accumulates during readout, which takes
the same time whatever the exposure, so it would be a constant offset. The excess here is
{float(_gr['DCDiff']):.4f} ADU/s of slope against a {float(_gr['TdarkDiff']):+.2f} ADU offset, so it
grows with exposure — and the two ends in fact cross at t = {float(_gr['CrossTime']):.0f} s, below
which the first-read columns are the *darker* of the two. **It is not a photodiode defect** either:
the photo-response across the same bands is flat to
{100*abs(float(_gr['RespRatio'])-1):.2f} %, so charge collection is normal and it is specifically the
leakage that is elevated.

What does change alongside the leakage is the operating point — the bias falls
{abs(float(_gr['BiasDiff'])):.1f} ADU and the read noise is
{100*(1-float(_gr['RNRatio'])):.0f} % lower at the hot end. A thermal gradient from the readout end
fits all of it: dark current roughly doubles per 7 to 10 K, so this ratio needs about 5 to 10 K, and
the output amplifier is where the power is dissipated. The headers carry a single set-point
(TESTTEMP = CHUCKTMP = -50) and no on-die sensor, so they can neither confirm nor refute a gradient of
that size. Two tests in data already taken would: the shape should repeat on every die if it is
thermal, where a process gradient would track position on the wafer, and the amplitude should scale
between runs at different chuck temperatures.
""")

fig('fig_dark_column_profile.png',
    ('Dark current per readout column. The profile carries the ramp, so the pairing test is made on '
     f"the residual to a running median: within a pair r = {float(DP['PairR']):+.3f}, across pairs "
     f"{float(DP['CrossR']):+.3f} — the same. The pairing of the read noise does not repeat in the "
     'leakage current.') if DP else
    ('Dark current per readout column. The profile carries the ramp, so the pairing test is made on '
     'the residual to a running median, and within a pair and across pairs it is the same: the '
     'pairing of the read noise does not repeat in the leakage current.'))

# ================================================================= stage 3
w('## 5. Response, PRNU and the light-route threshold\n')
lloc = L['Local']
w(f"""Photo-response {pm(float(L['Fit']['All']['SlopeSpread']['Median']), RSB, RSBS, RSBY, 0)} ADU
per intensity unit (block scatter, then fit window — section 11).
PRNU is deliberately **not** taken from the spread of that response: the published bright window
holds three closely spaced steps, which fixes a pixel's slope to about a per cent — far coarser
than the pattern being measured — so that spread is almost all fit noise. It is measured instead
from the spatial spread of each step's mean map with the temporal noise removed, over all
{len(np.atleast_1d(np.array(L['PatternStep'])))} steps, where one step already determines the pattern
from {NY*NX/1e6:.1f} M pixels.
""")
fig('fig_light_prnu.png', f"Per-step bright fixed pattern, measured over all "
    f"{len(np.atleast_1d(np.array(L['PatternStep'])))} steps and so independent "
    f"of the response fit window. The relative pattern plateaus at "
    f"{100*float(L['PRNU']['Multiplicative']):.2f} % at high signal; the pixel-to-pixel part of the fitted "
    f"response, after detrending, is {100*float(L['Local']['Resp']['RelIntr']):.2f} %.")
w(f"""The light-route threshold is {f3(float(L['Threshold']['Median']),2)} ADU, and its figure is the
clearest statement in the whole chain of why these deconvolutions are needed.
""")
_th = L['Threshold']
fig('fig_light_threshold.png', f"The light-route threshold distribution sits almost entirely on the "
    f"pure-fit-noise curve: {float(_th['StdRobust']):.1f} ADU observed, {float(_th['StdFitRobust']):.1f} of "
    f"fit noise, {float(_th['StdIntr']):.1f} left. The dark route is the narrow spike beside it. Without the "
    f"deconvolution one would report {float(_th['StdRobust']):.0f} ADU of threshold non-uniformity where "
    f"there is {float(_th['StdIntr']):.0f}.")

# ================================================================= stage 4
_brc = np.atleast_1d(np.array(BC['RawCol'], dtype=int))[
        np.atleast_1d(np.array(BC['BadResp'], dtype=bool))].tolist()
def _runs(v):
    # contiguous runs of column numbers, so a dead pair reads "3243-3244"
    out, i = [], 0
    while i < len(v):
        j = i
        while j+1 < len(v) and v[j+1] == v[j]+1:
            j += 1
        out.append(str(v[i]) if j == i else f'{v[i]}-{v[j]}')
        i = j+1
    return ', '.join(out)
_brc_txt = _runs(_brc) if _brc else 'none'
w('## 6. Bad readout columns\n')
w(f"""{int(BC['Nbad'])} of {int(BC['Nrawcol'])} readout columns are flagged, {100*(1-float(BC['GoodFraction'])):.2f} %
of the pixels — and **{int(BC['NbadInPairs'])} of them are complete (2k-1, 2k) pairs**. Only
{len(_brc)} fail on response, at raw columns {_brc_txt} — the blind first columns of this gain half
and the dead pairs.
""")
fig('fig_badcol_pairs.png', 'Every column lies on the 1:1 line against its readout partner, and the zoom shows each defect is exactly two adjacent columns wide. The correlation found in the read noise is visible here in the defects themselves.')
w(f"""One caveat the stage reports itself: the noisy columns are a smooth tail, not a separate
population — {int(np.atleast_1d(BC['CutScan']['Nbad'])[0])} columns at 3 sigma,
{int(np.atleast_1d(BC['CutScan']['Nbad'])[1])} at 5, {int(np.atleast_1d(BC['CutScan']['Nbad'])[3])} at 10.
The count is a choice of cut, not a number of defects, so every later number is reported with and
without the mask. It makes very little difference: the limiting signal moves by
{abs(float(UM['PTC']['Qlim_cal_5']) - float(BU['Masked']['PTC']['Qlim_cal_5'])):.1f} e-.
""")
fig('fig_badcol_cut.png', 'Where to cut. The flagged columns are the tail of a continuous distribution, so the 5-sigma line is a convention.')

# ================================================================= stage 5
w('## 7. Conversion gain, and an estimator that had to be calibrated\n')
nul = PT['Null']
U5  = PT['Unmasked']['All']
w(f"""Conversion gain **{f3(G,4)} ADU/e-** on stage 5's own convention, with a
{100*float(PT['GainSystematic']['Rel']):.1f} % systematic from the choice of fit window. The PTC slope is the gain and its intercept is *not* the
read noise: the shot noise follows the collected charge while the signal recorded is what is left
after the threshold, so the intercept is RN^2 + g*T.

The gain quoted in this section comes from stage 5, which forms its ensemble line by weighting
per-step **medians** — the third row of the estimator table in section 11. Section 11's own value,
{float(ME['Routes']['d']['Gain']):.4f}, comes from the unweighted mean-mean fit and is the one to
quote for the device; the two differ by
{100*abs(float(PT['GainEnsemble'])/float(ME['Routes']['d']['Gain'])-1):.1f} %, which is an estimator
difference and not a measurement. What this section is actually about is the shape of the per-pixel
distribution, and that is unaffected by the choice.

This stage is where the statistics of the chain change, and the first run of it looked broken: a
median gain 10 % below the ensemble, a negative intercept, and an "intrinsic" pixel-to-pixel spread
of zero at many hundreds of sigma. The cause is that a ladder point here is a per-pixel
**variance** from {int(np.atleast_1d(np.array(PT['Nframes']))[0])} frames —
{int(np.atleast_1d(np.array(PT['Nframes']))[0])-1} degrees of freedom, chi2 distributed, of order
100 % error, long tail — and not a Gaussian mean like every earlier stage. A null simulation in which every pixel is given exactly the same gain
reproduces all of it:

| | measured | null, identical pixels |
|---|---|---|
| median gain | {f3(U5['GainMedian'],4)} | {f3(nul['Median'],4)} |
| mean gain | {f3(U5['GainMean'],4)} | {f3(nul['Mean'],4)} (truth {f3(nul['Truth'],4)}) |
| intercept median | {f3(U5['OffsetMedian'],1)} | {f3(nul['OffsetMedian'],1)} |
| gain MAD | {f3(U5['GainMAD'],4)} | {f3(nul['MAD'],4)} |

So the **mean** is the gain, the median is {100*(1-float(nul['MedianBias'])):.1f} % low by construction, and the Gaussian
deconvolution used elsewhere simply does not apply here — it pairs a robust observed spread with
an analytic rms, and for a heavy tail the first is the smaller.
""")
fig('fig_ptc_gain_null.png', f"The measured per-pixel gain distribution and the null lie on top of "
    f"each other: the observed spread is {float(U5['MADoverNull']):.4f} times the null, so no "
    f"pixel-to-pixel gain variation is detected and a single pixel differs by at most "
    f"{100*float(U5['IntrFromMAD']):.0f} %.")
w(f"""Where the gain *is* measurable is after averaging. Per readout column ({int(PT['NpixPerCol'])} pixels each) the
real spread is **{100*float(PT['Column']['RelIntr']):.2f} %**, per 32x32 block
{100*float(PT['BlockGain']['RelIntr']):.2f} %, and the two parities differ by
{100*(float(PT['Column']['Odd']['Median'])/float(PT['Column']['Even']['Median'])-1):+.2f} %.
""")
fig('fig_ptc_gain_column.png', f"Gain per readout column. The observed histogram is only slightly "
    f"wider than the null, and the difference is the {100*float(PT['Column']['RelIntr']):.2f} % real "
    f"column-to-column variation.")

# ================================================================= stage 6
if VS is not None:
    w('## 8. How much does the variance itself differ between pixels?\n')
    _vs = steps_of(VS)
    _ze = [e for e in _vs if e['Type'] == 'ZE']
    _dk = [e for e in _vs if e['Type'] == 'D']
    _br = [e for e in _vs if e['Type'] == 'B' and not e.get('Saturated')]
    def _row(e):
        U = e['Unmasked']
        sig = float(U['Sigma'])
        val = f"{100*float(U['RelIntr']):.0f} %" if sig > 5 else f"< {100*float(U['RelIntrUL95']):.0f} %"
        return f"| {e['Type']} {e['Step']} | {float(e['Signal']):.0f} | {float(U['Shape']):.3f} | {float(e['Null']['Shape']):.3f} | {val} |"
    w(f"""Everything above is an average over pixels. This asks the distribution question directly: at each
step, how widely does the per-pixel variance itself vary, once the estimator's own width is taken out?

The estimator's width is not a detail. A variance from {int(_dk[0]['Nframes'])} frames carries
{int(_dk[0]['Dof'])} degrees of freedom, so even with perfectly identical pixels its spread is 100 % of
its mean. The comparison is therefore against a **simulated** null in which every pixel has exactly the
same true variance, with the frames rounded to integers as the detector rounds them -- a variance built
from three integers can only take multiples of 1/18, so both distributions are combs, and a continuous
chi2 null would differ from the data for a reason that has nothing to do with the pixels.

| step | signal [ADU] | measured width | null | spread of the true variance |
|---|---|---|---|---|
{chr(10).join(_row(e) for e in (_ze + _dk + _br[:4]))}

The trend is physics, not noise: **the variance inherits the non-uniformity of whatever dominates it.**
At zero signal that is read noise, which varies enormously from pixel to pixel
({100*float(_ze[0]['Unmasked']['RelIntr']):.0f} %). Down the dark ladder it becomes dark signal, and settles near
{100*float(_dk[-1]['Unmasked']['RelIntr']):.0f} % -- close to the {100*float(D['Fit']['All']['SlopeSpread']['RelIntr']):.0f} % by which the dark current itself varies over
the die. On the bright ladder shot noise takes over and nothing is detected: the only spread expected
there is the {100*float(L['Local']['Resp']['RelIntr']):.2f} % of the response, far below what this measurement can reach.
""")
    fig('fig_varspread_comb.png', 'The quantisation, and the reason the null has to be simulated with integer frames rather than drawn from a continuous chi2: the simulated comb falls on the measured one spike for spike.')
    fig('fig_varspread_trend.png', 'Left: the pixel-to-pixel spread of the true variance against signal, with arrows where the excess is not significant. Right: the tail above ten times the median -- hot pixels at zero signal, cosmic rays on the long darks, and nothing above the null on the bright steps.')

if LS is not None:
    w('## 9. Is that variance explained?\n')
    _ls = steps_of(LS)
    _lb = [e for e in _ls if e['Type'] == 'B']
    _ld = [e for e in _ls if e['Type'] == 'D']
    # a second, lower point on the dark ladder to contrast with its top: the
    # middle of whatever steps this setup put below the low-signal limit
    _ldm = _ld[len(_ld)//2] if _ld else None
    _lz = [e for e in _ls if e['Circular']]
    w(f"""Section 8 measured the spread; this asks what it is made of, by predicting **every pixel's** variance
from quantities measured elsewhere,

  sigma^2_pred,i = RN_i^2 + g*(S_i + T)

with RN_i the read-noise map of stage 1, S_i the pixel's own signal at that step, g the gain of stage 7
and T the charge threshold. The prediction is never compared with the data directly: it is first
*measured* the way the data were, sampled through its own chi2 with the frames rounded to integers, so
both sides go through identical processing. The bias frames are included as a wiring check, where the
prediction is circular by construction and must come out exact -- it does, to
{100*float(_lz[0]['ResidRel']):+.2f} %.

| | signal [ADU] | measured | predicted | difference |
|---|---|---|---|---|
{chr(10).join(f"| bright step {e['Step']} | {float(e['Signal']):.0f} | {float(e['MeanV']):.1f} | {float(e['MeanPred']):.1f} | **{100*float(e['ResidRel']):+.2f} %** |" for e in _lb)}
{chr(10).join(f"| dark step {e['Step']} | {float(e['Signal']):.0f} | {float(e['MeanV']):.1f} | {float(e['MeanPred']):.1f} | **{100*float(e['ResidRel']):+.2f} %** |" for e in _ld[-4:])}

**The bright ladder is fully explained** -- to {abs(100*float(_lb[-1]['ResidRel'])):.2f} % at {float(_lb[-1]['Signal']):.0f} ADU -- and so is its
pixel-to-pixel width. **The dark ladder is not.** Its variance runs
{abs(100*float(_ld[-1]['ResidRel'])):.1f} % below the prediction at the top of the ladder and
{abs(100*float(_ldm['ResidRel'])):.1f} % below at {float(_ldm['Signal']):.0f} ADU, while the widths still match. So it is the level, not
the uniformity, that is wrong: dark charge produces **less shot noise than photo-charge of the same
measured size**.
""")
    # the same comparison made against the fitted PTC line, with both corrections
    _G, _C = float(PT['GainEnsemble']), float(PT['OffsetEnsemble'])
    _vd = [e for e in steps_of(VS) if e['Type'] == 'D'] if VS is not None else []
    _vbr = ([float(e['Unmasked']['RelIntr']) for e in steps_of(VS)
             if e['Type'] == 'B' and float(e['Signal']) < 1000] if VS is not None else [0.0])
    # Both gaps come from ptc_both.json, which the figure itself wrote. They were
    # previously rebuilt here out of stage 7's SigmaNull, a different estimator
    # of the same thing: the caption then disagreed with its own figure by up to
    # a factor of four, and printed nan on a die where stage 7 had not been run.
    _dt  = 100*float(PB['DarkDeficitTop'])    if PB and 'DarkDeficitTop'    in PB else float('nan')
    _dt0 = 100*float(PB['DarkDeficitTopRaw']) if PB and 'DarkDeficitTopRaw' in PB else float('nan')
    _tops = f" ({float(PB['TopSignal']):.0f} ADU)" if PB and 'TopSignal' in PB else ''
    fig('fig_ptc_both.png', "Both ladders on one photon transfer curve, log-log. Shot noise should not "
        "know where the electrons came from, so if dark charge and photo-charge were the same thing the "
        "two ladders would lie on one line. The bright points do; the dark points run below"
        + (f", by {_dt:.1f} % at the top of the dark ladder{_tops}" if np.isfinite(_dt) else "")
        + ". The open symbols in the right panel are the same points without the median-to-mean "
          "correction described below"
        + (f", where the same deficit reads {_dt0:+.1f} %" if np.isfinite(_dt0) else "")
        + ". The bias point shows how much of the fitted intercept is read noise and "
          "how much is the threshold term.")
    PP = load('ptc_perpixel.json') if os.path.isfile(os.path.join(OUT, 'ptc_perpixel.json')) else None
    # how right-skewed each ladder's signal is at the top of the dark ladder,
    # measured rather than quoted: it is the whole reason the mixed pair biased
    # the two ladders differently, and it is a property of this die
    def _skew(ty):
        if PX is None:
            return None
        pp = [e for e in PX['Points'] if e.get('Type') == ty]
        if not pp:
            return None
        e = max(pp, key=lambda q: float(q['SignalMean']))
        return 100*(float(e['SignalMean'])/float(e['SignalMedian']) - 1)
    _skD, _skB = _skew('D'), _skew('B')
    _movep = (f"moved the answer from {_dt0:+.1f} % to {_dt:.1f} % at the top of the dark ladder"
              if np.isfinite(_dt) and np.isfinite(_dt0) else
              "moved the answer by more than the effect being measured")
    _skewp = ((f"The dark signal is right-skewed, its mean sitting {_skD:.1f} % above its median, "
               f"against {_skB:.1f} % on the bright ladder, ")
              if _skD is not None and _skB is not None else
              "The dark signal is right-skewed and the bright signal is not, ")
    w(f"""Getting that comparison right took two attempts, and the mistake is worth recording because it
{_movep}. The plotted variance was first a median over
pixels (a plain mean is destroyed by cosmic rays) times the chi2 median-to-mean factor, while the
signal stayed a plain median. That is not a point on any curve: Var = g*S + c holds **per pixel**, so
averaging over pixels needs E[Var] against E[S] — a mean on both axes, over the same pixels. {_skewp}so the
mixed pair biased the two ladders differently. Both axes are now means over one common set of pixels,
those outside the top 0.1 % of the variance; the open symbols in the ratio panel are the old pair.
""")
    if PP is not None:
        _dd = PP['Ladder']['Difference']
        _gD = PP['Ladder']['D']
        _gB = PP['Ladder']['B']
        w(f"""With that fixed, three independent measurements of the deficit agree:

| measurement | deficit |
|---|---|
| ladder means against the bright line | **{100*float(PB['DarkDeficit']):.1f} %** |
| per-pixel prediction (above) | **{abs(100*float(_ld[-1]['ResidRel'])):.1f} %** |
| per-pixel gain difference (below) | **{abs(100*float(_dd['MeanRel'])):.1f} %** |

The third is the one an ensemble cannot make. Fitting a PTC to **every pixel** on each ladder
separately gives each pixel two gains, and their difference says whether the deficit is something
every pixel does or something a subset carries. The ensemble gains are {float(_gD['GainEnsemble']):.4f}
on the dark ladder against {float(_gB['GainEnsemble']):.4f} on the bright; per pixel the mean
difference is {float(_dd['TrimMean']):+.4f} ADU/e- (trimmed; the plain mean reads
{float(_dd['Mean']):+.4f}, carried by the tails of two heavy-tailed estimators, and the median
{float(_dd['Median']):+.4f}, skewed — neither is the number to quote), and its spread is **{float(_dd['MADoverNull']):.3f}
times** the null for two independent identical pixels. The distribution is the null's, shifted
bodily: every pixel shows the deficit, and none of it is carried by a subpopulation.

Neither fitted parameter shows pixel-to-pixel structure. The gain's width is
{float(_gD['Slope']['MADoverNull']):.3f} (dark) and {float(_gB['Slope']['MADoverNull']):.3f} (bright)
times the null, the intercept's {float(_gD['Inter']['MADoverNull']):.3f} and
{float(_gB['Inter']['MADoverNull']):.3f}, and the slope-intercept anti-correlation on the bright
ladder is {float(_gB['Cov']['CorrMeasuredRobust']):+.3f} measured against
{float(_gB['Cov']['CorrNullRobust']):+.3f} for the null and {float(_gB['Cov']['CorrAnalytic']):+.3f}
predicted by the fit itself. The three agree, so that anti-correlation is the straight line's own and
not a property of the detector. (Robust correlations on a common central window; the plain ones are
dominated by the tails.)
""")
        _ie = _gD['InterExcess']
        _dc = _gD['ReadNoiseDecile']
        _cr = PP.get('Cross', {})
        _ex = np.sqrt(max(float(_gD['Inter']['MAD'])**2 - float(_gD['Inter']['NullMAD'])**2, 0))
        _rows = []
        for _k in (0, 4, 7, 9):
            _rows.append(f"| {_k+1} | {float(np.atleast_1d(_dc['RN2'])[_k]):.2f} | "
                         f"{float(np.atleast_1d(_dc['MADexcess'])[_k]):.2f} | "
                         f"{float(np.atleast_1d(_dc['Predicted'])[_k]):.2f} |")
        w(f"""One of those widths needed chasing, and the answer is a rule worth carrying. Fitting the
per-pixel PTC on Var - RN^2 instead of on the total variance — removing each pixel's own read noise
before the fit, as the ensemble routes of section 11 do — raises the dark intercept's width from
**{float(_gD['Inter']['MADoverNull']):.3f}** to **{float(_ie['MADoverNull']):.3f}** times its null,
and that looked at first like real pixel-to-pixel threshold structure.

It is not. Binned by the pixel's own read noise, the extra width appears only where the read noise is
large, and it tracks the sampling error of the very quantity being subtracted:

| read-noise decile | median RN^2 [ADU^2] | intercept width on Var - RN^2 | RN^2/sqrt({int(PP['DofZ'])}/2) |
|---|---|---|---|
{chr(10).join(_rows)}

In the quietest decile the measured width equals the null; by the noisiest it has nearly doubled.
RN^2_i is itself a variance measured from {int(PP['DofZ'])+1} frames, so it carries about
{100*np.sqrt(2/float(PP['DofZ'])):.0f} % error with a long tail, and because **one** value is
subtracted at **every** step of the ladder its error cannot go anywhere but the intercept. The slope
is untouched by it — a per-pixel constant cannot tilt a line — which is the signature that identifies
the cause.

What survives, once the fit uses the total variance, is small and independently confirmed. The
residual excess is {_ex:.2f} ADU^2 in quadrature, about
{_ex/float(_gD['GainEnsemble']):.1f} ADU of threshold variation. Separately, the two threshold maps
measured by routes that share no data beyond the frames — this PTC intercept and the dark response of
section 4 — correlate at r = {float(_cr.get('Corr', float('nan'))):+.4f} over
{float(_cr.get('Npix', 0))/1e6:.1f} M pixels, which inverts to a shared spread of
**{float(_cr.get('SharedSigmaT', float('nan'))):.2f} ADU**. Those two agree; the
{float(_ie['MADoverNull']):.2f} did not mean anything.

**A second methodological point, about the weights.** The variance of a ladder point is
Var[y] = 2*Vtot^2/Dof, which depends on the **step** and not on the pixel, so the right weight is one
scalar per step — which is what the fits above use. An earlier version instead weighted each point by
the ensemble model evaluated at that pixel's own signal, 1/(g*x+c)^2, which is a different estimator:
it lets a pixel's own brightness decide how much each of its points counts. Refitting that way costs
nothing once the maps are loaded, so the stage measures the difference:

| ladder | trimmed mean gain, per-step weights | with per-pixel model weights | change |
|---|---|---|---|
| dark | {float(_gD['Slope']['TrimMean']):.4f} | {float(_gD['ModelWeighted']['TrimMean']):.4f} | **{100*(float(_gD['ModelWeighted']['TrimMean'])/float(_gD['Slope']['TrimMean'])-1):+.1f} %** |
| bright | {float(_gB['Slope']['TrimMean']):.4f} | {float(_gB['ModelWeighted']['TrimMean']):.4f} | {100*(float(_gB['ModelWeighted']['TrimMean'])/float(_gB['Slope']['TrimMean'])-1):+.2f} % |

The asymmetry is the physics of the two ladders. The bright signal is uniform to about a per cent, so
a weight built from each pixel's own signal is nearly the same for every pixel and agrees with a
per-step scalar. The dark signal is not: the dark current varies about
{100*float(D['Fit']['All']['SlopeSpread']['RelIntr']):.0f} % across the die, so the per-pixel weight
varies with it and the two estimators part company. The per-pixel **dark** gain therefore carries a
weighting systematic of several per cent — larger than its own departure from the null — while the
ensemble routes of section 11 are free of it, since they weight per step by construction. It is one
more reason to read the per-pixel fits for their *distributions*, which is what they are for, and to
take the gain itself from the ensemble.

**Subtract the read noise in an ensemble fit, never in a per-pixel one.** An ensemble subtracts an
average and loses nothing — section 11 does exactly that and is better for it. A per-pixel fit
subtracts a noisy estimate and puts its error straight into the parameter being measured.
""")
        fig('fig_pp_params.png', "Per-pixel fit parameters against the identical-pixel null. The null is wide and skewed because a variance from three frames carries two degrees of freedom: a single pixel's gain is good only to tens of per cent and the median of the estimator sits 6 to 14 % below the truth.")
        fig('fig_pp_diff.png', 'Left: goodness of fit per pixel against the null. Right: the dark-minus-bright gain of the same pixel. The measured distribution lies on the null, displaced by the deficit, which is what says every pixel shares it.')
        fig('fig_pp_joint.png', 'Slope against intercept per pixel. The strong anti-correlation on the bright ladder is what any straight-line fit gives when its points sit away from x = 0, and it matches both the null and the analytic prediction; the dark ladder, whose points reach down to zero signal, shows none.')

    fig('fig_lowsig_level.png', 'Left: the level test above, against signal. Right: the same for the pixel-to-pixel width, which is explained on both ladders -- it is only the level of the dark one that fails.')

w('## 10. The noise budget\n')
w(f"""In electrons, per pixel, at the {TT:g} s exposure of the bright frames: read noise
{f3(float(IN['RN_ADU'])/G,3)} e-, dark signal {f3(float(IN['DC_ADU'])*TT/G,2)} e-, offset fixed
pattern {f3(float(IN['SigmaTdark_ADU'])/G,2)} e-, DSNU {f3(float(IN['SigmaDC_ADU'])*TT/G,2)} e- and
PRNU {100*float(IN['PRNU']):.2f} %.

Which of them matters depends entirely on where you look:

__TERMS__

__TERMSTEXT__
""")
fig('fig_budget_terms.png', 'The budget decomposed, uncalibrated, with the shot-noise threshold. Below the threshold there is no signal at all; above it the floor is the offset pattern, the dark shot noise and the read noise, in that order.')
fig('fig_budget_snr.png', 'Signal to noise against incident charge for the three threshold routes. Solid is calibrated, dashed a single raw frame; the circles mark where each curve reaches SNR 5 and 3.')

# ================================================================= threshold
if DW is not None:
    _sc2 = DW['Scan'] if isinstance(DW['Scan'], list) else [DW['Scan']]
    _td2 = [float(e['Tdark']) for e in _sc2]
    _wd = sorted(_sc2, key=lambda e: -int(e['Nsteps']))[0]
    _wn = sorted(_sc2, key=lambda e:  int(e['Nsteps']))[0]
    DARK_T_DRIFT = (f"the dark-route threshold moves from {min(_td2):.1f} to {max(_td2):.1f} ADU "
                    f"over the {len(_sc2)} candidate windows of section 2")
else:
    DARK_T_DRIFT = 'the dark-route threshold moves by a factor of a few across the possible windows'
w('## 11. The gain and the threshold: four routes, with errors\n')
TH = PT['Thresholds']
if ME is not None:
    _R = ME['Routes']
    _fsd = np.atleast_1d(np.array(D['FitSteps'], dtype=int)).tolist()
    _fsb = np.atleast_1d(np.array(L['FitSteps'], dtype=int)).tolist()
    _bmed = np.atleast_1d(np.array(L['Pattern']['All']['Median'], dtype=float))
    _nm = {'a': f"dark response (steps {_fsd[0]}-{_fsd[-1]})",
           'b': f"light response (below {_bmed[_fsb[-1]-1]:.0f} ADU)",
           'c': 'PTC, dark ladder', 'd': 'PTC, bright ladder'}
    # Every route has a slope, and it is a different quantity in each: the dark
    # current, the photo-response, and for the two photon-transfer routes the
    # conversion gain. An earlier version of this table headed the column "gain"
    # and so printed a dash for a) and b), which threw the dark current and the
    # photo-response, both with errors, out of the report.
    _su  = {'a': 'ADU/s', 'b': 'ADU/intensity', 'c': 'ADU/e-', 'd': 'ADU/e-'}
    _snd = {'a': 4, 'b': 1, 'c': 4, 'd': 4}
    def _g(k):
        # c) and d) quote the gain errors rather than the bare slope errors: on
        # those two the systematic also carries the choice of ensemble estimator,
        # discussed below the table, and dropping it would understate them.
        Q, n = _R[k], _snd[k]
        if Q.get('Gain') is not None and np.isfinite(float(Q['Gain'])):
            v, st, sy = float(Q['Gain']), float(Q['GainStat']), float(Q['GainSyst'])
        else:
            v, st, sy = float(Q['Slope']), float(Q['SlopeStat']), float(Q['SlopeSyst'])
        return f"**{v:.{n}f}** ± {st:.{n}f} ± {sy:.{n}f} {_su[k]}"
    def _t(k):
        Q = _R[k]
        return (f"{float(Q['Threshold']):.2f} ± {float(Q['ThresholdStat']):.2f} ± "
                f"{float(Q['ThresholdSyst']):.2f}")
    # four residuals spread over the constrained fit, however many steps the
    # dark ladder of this setup has inside the linear range
    _cr  = np.atleast_1d(np.array(_R['constrained']['Residual'], dtype=float))
    _conres = _cr[np.unique(np.linspace(0, _cr.size-1, min(4, _cr.size)).astype(int))]
    _gc, _gd = float(_R['c']['Gain']), float(_R['d']['Gain'])
    _ec = np.hypot(float(_R['c']['GainStat']), float(_R['c']['GainSyst']))
    _ed = np.hypot(float(_R['d']['GainStat']), float(_R['d']['GainSyst']))
    _ns = abs(_gd - _gc)/np.hypot(_ec, _ed)
    _ta, _tc = float(_R['a']['Threshold_e']), float(_R['c']['Threshold_e'])
    _ea, _ec2 = float(_R['a']['Threshold_e_err']), float(_R['c']['Threshold_e_err'])
    _nt = abs(_ta - _tc)/np.hypot(_ea, _ec2)
    w(f"""Four independent routes reach these two numbers, and putting them in one table with their
errors is the clearest statement of what this device does and does not have.

| route | slope, value ± stat ± syst | threshold [ADU] | threshold [e-] |
|---|---|---|---|
| a) {_nm['a']} | {_g('a')} | {_t('a')} | **{float(_R['a']['Threshold_e']):.1f} ± {float(_R['a']['Threshold_e_err']):.1f}** |
| b) {_nm['b']} | {_g('b')} | {_t('b')} | **{float(_R['b']['Threshold_e']):.1f} ± {float(_R['b']['Threshold_e_err']):.1f}** |
| c) {_nm['c']} | {_g('c')} | {_t('c')} | **{float(_R['c']['Threshold_e']):.1f} ± {float(_R['c']['Threshold_e_err']):.1f}** |
| d) {_nm['d']} | {_g('d')} | {_t('d')} | **{float(_R['d']['Threshold_e']):.1f} ± {float(_R['d']['Threshold_e_err']):.1f}** |

Errors are quoted statistical first, then systematic. The slope is a different quantity on each route:
a) is the dark current, b) the photo-response, c) and d) the conversion gain. **Only the two
photon-transfer routes measure a gain** — a response curve contains no noise, so it cannot — but all
four have a slope and all four give a threshold, which is why both columns are filled.

**What the errors are.** The statistical one is the scatter between
{int(ME['NBlock'])}x{int(ME['NBlock'])} = {int(ME['NBlock'])**2} independent blocks of the die, each
{int(ME['BlockSize'][0])}x{int(ME['BlockSize'][1])} pixels, not a formal error from the pixel count:
with {NY*NX/1e6:.1f} M pixels the latter reads 1e-5 and means nothing, while the block version carries the
spatial structure, which is what makes "the gain of this die" uncertain at all. The systematic is the
fit window, refitted over the variants of the chosen window — not over every window the ladder admits,
which is wider, and which section 4 quantifies for the dark current — and on a convex ladder it
dominates everywhere —
the dark threshold moves {float(_R['a']['ThresholdSyst']):.1f} ADU across windows against a block
error of {float(_R['a']['ThresholdStat']):.1f}, the light threshold
{float(_R['b']['ThresholdSyst']):.1f} against {float(_R['b']['ThresholdStat']):.1f}.

The two PTC routes carry a third term, folded into their systematic rather than hidden: the choice of
estimator. The nominal fit is unweighted, the mean variance against the mean signal, which is the
unbiased estimator of the ensemble relation. Weighting by 1/Var^2 instead, as the per-pixel stage
does, lets the low-signal points set the slope of a slightly curved PTC and gives
{float(_R['c']['GainWeighted']):.4f} and {float(_R['d']['GainWeighted']):.4f} rather than
{_gc:.4f} and {_gd:.4f}. Neither is wrong, so half the difference joins the error.

**The two gains differ by {100*abs(_gd-_gc)/_gd:.1f} %**, {_gc:.4f} against {_gd:.4f} with combined
errors of about {np.hypot(_ec,_ed):.4f} — a {_ns:.0f} sigma separation. That is the dark deficit of
section 9 again, now as a gain with an error bar on it.

**The four thresholds span {min(float(_R[k]['Threshold_e']) for k in 'abcd'):.1f} to
{max(float(_R[k]['Threshold_e']) for k in 'abcd'):.1f} e- and are mutually inconsistent.** Taking the
two best determined, c) at {_tc:.1f} ± {_ec2:.1f} and a) at {_ta:.1f} ± {_ea:.1f}, they sit
{_nt:.0f} sigma apart, and no choice of window brings them together. The threshold is not a quantity
this device has a single value of, and whichever route the noise budget adopts has to be carried as a
stated assumption rather than a measurement.

**The fit is on Var - RN^2, not on Var.** Each pixel's own read-noise variance is removed before
averaging rather than subtracted from the intercept afterwards. The term is small —
{float(ME['RN2']):.1f} of {np.atleast_1d(np.array(PT['StepVariance'],dtype=float))[0]:.0f} ADU^2
at the lowest bright step — but it makes the intercept mean one thing only, g*T, and it takes a
quantity that varies strongly from pixel to pixel out of a whole-die constant.

**What if the two ladders share a gain?** Forcing the bright value
{float(_R['d']['Gain']):.4f} on the dark points and fitting only the offset gives an offset of
{float(ME['Routes']['constrained']['Offset']):+.2f} ADU^2, so T = {float(ME['Routes']['constrained']['Threshold']):+.2f} ADU — and
it does not work. The residuals run {' '.join(f"{v:+.0f}" for v in _conres)} ADU^2
from the lowest step to the highest, a clean monotonic trend, and the residual rms is
{float(ME['Routes']['constrained']['ResidRMS']):.2f} ADU^2 against
{float(ME['Routes']['constrained']['FreeResidRMS']):.2f} when the gain is free — worse by a factor
{float(ME['Routes']['constrained']['ResidRMS'])/float(ME['Routes']['constrained']['FreeResidRMS']):.0f}. A
shared gain cannot be rescued by any offset: the two ladders differ in slope, not in intercept.

**Four numbers for one gain.** The same bright ladder yields different gains depending on how the
ensemble is formed, and the differences are larger than the statistical errors, so the convention has
to be stated rather than assumed:

| how the ensemble is formed | dark | bright |
|---|---|---|
| per-step means, unweighted — **used here and in section 9** | **{float(_R['c']['Gain']):.4f}** | **{float(_R['d']['Gain']):.4f}** |
| per-step means, weighted by 1/Var^2 | {float(_R['c']['GainWeighted']):.4f} | {float(_R['d']['GainWeighted']):.4f} |
| per-step medians, weighted (stage 5's convention, bright ladder only) | &mdash; | {float(PT['GainEnsemble']):.4f} |
| trimmed mean over pixels of the per-pixel fitted slope | {float(PP['Ladder']['D']['Slope']['TrimMean']):.4f} | {float(PP['Ladder']['B']['Slope']['TrimMean']):.4f} |

The first is the unbiased estimator of the ensemble relation and is what both this table and the
per-pixel section now use; earlier drafts of this report quoted the third in one section and the
first in another, which differ by {100*abs(float(PT['GainEnsemble'])/_gd-1):.1f} % on the bright
ladder for no physical reason.

One number in that table moved against what section 4 reports and both are right: the dark current
here is {float(_R['a']['Slope']):.4f} ± {float(_R['a']['SlopeStat']):.4f} ± {float(_R['a']['SlopeSyst']):.4f} ADU/s,
the **mean** over pixels, while section 4 quotes the **median pixel**. The dark-current distribution
is skewed by {100*(float(_R['a']['Slope'])/float(D['Fit']['All']['SlopeSpread']['Median'])-1):+.1f} %,
which is the whole of the difference.
""")

w(f"""The two response routes get their threshold by extrapolating a curve to zero signal, and section 2
showed that neither curve is straight: the responsivity of both ladders rises with signal, so each
window extrapolates its own local tangent and lands somewhere different. Measured on this die, {DARK_T_DRIFT}
and the light-route threshold moves {float(_R['b']['ThresholdSyst']):.1f} ADU across the four windows
tried, against a block-to-block error of {float(_R['b']['ThresholdStat']):.1f} ADU. Neither motion is
noise -- both are monotonic with the window -- and neither route can therefore claim its own number.

The shot-noise route extrapolates nothing. The variance measures the charge actually collected,
Q = (Var - RN^2)/g^2, against the signal recorded, S = g(Q - T), so T follows step by step — and a
real threshold must then come out the same at every step.

**How this table relates to the budget in section 10.** The budget was computed before this
comparison existed and uses three values taken straight from the stage summaries:
{f3(TH['PTC_e'],1)}, {f3(TH['Dark_e'],1)} and {f3(TH['Light_e'],1)} e-. Two of them match the table
within its errors — the dark response and the light response — and the third, labelled there simply
"shot noise", is the bright-ladder PTC, whose value shifts from {f3(TH['PTC_e'],1)} to
{float(_R['d']['Threshold_e']):.1f} e- when the unweighted mean-mean estimator replaces the weighted
one. The dark-ladder PTC, route c), has no counterpart in the budget at all. Nothing in section 10
needs redoing for that: its point was that the threshold choice moves the limiting signal by far
more than any other term, and a fourth route at {float(_R['c']['Threshold_e']):.1f} e- only widens
the range it already shows.
""")
_pst = np.atleast_1d(np.array(PT['PerStepThreshold']['ThresholdADU'], dtype=float))
_psm = np.atleast_1d(np.array(PT['PerStepThreshold']['Median'], dtype=float))
_psw = (_psm >= float(PT['GainRange'][0])) & (_psm <= float(PT['GainRange'][1]))
fig('fig_ptc_threshold.png',
    f"The threshold implied by the shot noise, step by step. Flat at "
    f"{np.nanmin(_pst[_psw]):.0f}-{np.nanmax(_pst[_psw]):.0f} ADU across the whole fit window, and "
    f"drifting only above it, where the PTC itself bends and an intercept fitted there would not be "
    f"a threshold at all.")
w(f"""**Which I would use.** A photon-transfer value — route d), {float(_R['d']['Threshold_e']):.1f} ±
{float(_R['d']['Threshold_e_err']):.1f} e-, measured on the ladder whose variance is fully explained
(section 9). It is the only kind of route that does
not extrapolate a curved response at all: it reads the threshold from the shot noise step by step,
and gets the same answer at every step of the window. Section 9 adds a second argument for it --
the bright ladder's variance is explained to better than a per cent with this threshold in the
prediction. On that value the die measures signals down to **{f3(float(UM['PTC']['Qlim_cal_5']),0)} e-**
at SNR 5 and {f3(float(UM['PTC']['Qlim_cal_3']),0)} e- at SNR 3, calibrated.

**What would confirm it.** Two measurements, neither expensive. First, a run whose bright ladder
reaches below {_bmed[0]:.0f} ADU: if the low-signal excess is a response non-linearity rather than
lost charge, the shot-noise threshold should stay flat there while the light-route value keeps
drifting. Second, the comparison across this lot's two bias-board setups, whose dark currents
differ by a factor 22: if the shot-noise route returns a large value on the high-dark-current run
too it is tracking something physical, and if it returns the same few ADU it is measuring a
property of the measurement rather than of the device. Both runs are mirrored locally and the
cross-die summary makes that comparison.
""")
fig('fig_budget_limit.png',
    f"The smallest measurable signal under each route. The threshold choice moves it by "
    f"{max(QLIM)-min(QLIM):.0f} e-; the gain systematic (error bars) by "
    f"{abs(float(UM['PTC']['Qlim_cal_gmin'])-float(UM['PTC']['Qlim_cal_gmax'])):.1f} e- and the "
    f"bad-column mask by {abs(float(UM['PTC']['Qlim_cal_5'])-float(BU['Masked']['PTC']['Qlim_cal_5'])):.1f} e-.")

# ================================================================= open
w('## 12. What this chain does not determine\n')
w(f"""**The charge threshold**, as above: a factor four, and the dominant uncertainty on every
number that matters. Everything else in this report is known to a few per cent.

**What the low-signal excess is.** Both ladders show it, and at a similar size in ADU: about +10 to
+15 ADU at the lowest steps of each. A common additive effect at low signal would explain both
knees and would also explain why the PTC, which is insensitive to an additive offset in the
response, gives the smallest threshold. That is a hypothesis, not a measurement.

**Where the column pairing lives.** Readout columns 2k-1 and 2k share their noise amplitude almost
perfectly (r = {float(RP['PairR']):+.3f} on this die) while their pixels stay independent, and the
defects come in the same pairs.
The dark current shows no pairing at all, which points at the readout chain rather than the pixel.
The test that would settle it is the same measurement on the low-gain half: if the pairing survives
the shared element is in the pixel, if it vanishes it is in the chain. That path was blocked by a
bug (the streamed reader ignored the gain-half setting) which is now fixed, so the test is
available.

**Whether any of this is particular to this die.** One die, one configuration, one temperature.
The chain exists to be pointed at the others.
""")

# ================================================================= appendix
w('## Appendix — how to rerun this\n')
w(f"""Every stage is a MATLAB script in the package `ultrasat.lab.scripts` and takes no arguments.
The dataset is selected by defining `DieSelect` (run, folder, die, gain half) in the workspace
before the stage runs; without it `desy_die_config.m` supplies its defaults. That config also holds
the bad-column cut, the PTC signal window, which threshold the budget prefers and the
goodness-of-fit tolerance of the dark window, each with the measurement behind the choice in a
comment.

```matlab
DieSelect = struct('Run','{PT["Run"]}', 'Folder','<dataset folder>', ...
                   'Die','{PT["Die"]}', 'Gain','{PT["GainHalf"]}');
ultrasat.lab.scripts.desy_rn_single_die      % stage 1  -> desy_rn/<tag>/
ultrasat.lab.scripts.desy_die_darkwindow     % stage 2a -> desy_die/<tag>/
ultrasat.lab.scripts.desy_die_dark           % stage 2
ultrasat.lab.scripts.desy_die_light          % stage 3
ultrasat.lab.scripts.desy_die_badcol         % stage 4
ultrasat.lab.scripts.desy_die_ptc            % stage 5
ultrasat.lab.scripts.desy_die_budget         % stage 6
ultrasat.lab.scripts.desy_die_varspread      % stage 7
ultrasat.lab.scripts.desy_die_lowsignal      % stage 8
ultrasat.lab.scripts.desy_die_ptc_perpixel   % stage 9
ultrasat.lab.scripts.desy_die_methods        % stage 10
ultrasat.lab.scripts.desy_die_ptc_export     % the PTC points, for the joint plot
```

or, for the whole chain including the figures and this PDF,
`desy_batch/run_die.sh {PT["Run"]} <dataset folder> {PT["Die"]} {PT["GainHalf"]}`.

Each stage writes binary maps (`single`, [Ny Nx], column-major) and a json summary, and the
figures come from the matching `*_plots.py` run by path. This report is built by
`desy_die_report_build.py --indir <die dir> --rndir <stage 1 dir>`, which reads only the json
files, so it regenerates from the dumps without touching the frames.

Note that the streamed mode needs an explicit step list: the `'auto'` rule resolves steps from a
cached region ladder, which the whole-die mode does not build. The bright list is the config's; the
dark list is what stage 2a measured, and every stage that inherits it checks that the scan it came
from belongs to this die and was made at the tolerance now set.
""")

# ---------------------------------------------------------------- derived substitutions
_cd = chi2ratio(D)
_cl = chi2ratio(L)
_chi = (f'{100*(_cd-1):+.1f} % on the dark ladder, {100*(_cl-1):+.1f} % on the bright')

_C  = UM['PTC']
_T  = _C['Terms']
_qs = [10, 30, 100, 300, 1000]
_rows = []
for _q in _qs:
    _i = int(np.argmin(np.abs(np.array(_C['Q'], dtype=float) - _q)))
    _v = {k: float(np.atleast_1d(np.array(_T[k], dtype=float)).repeat(len(_C['Q']))[_i])
          if np.size(_T[k]) == 1 else float(np.array(_T[k], dtype=float)[_i])
          for k in ('RN', 'Shot', 'DarkShot', 'OffsetFPN', 'DSNU', 'PRNU')}
    _tot = sum(_v.values())
    _rows.append((float(np.array(_C['Q'], dtype=float)[_i]), _v, _tot))
_terms = ('| Q [e-] | read noise | signal shot | dark shot | offset FPN |\n|---|---|---|---|---|\n'
          + '\n'.join(f"| {_q:.0f} | {100*_v['RN']/_tot:.0f} % | {100*_v['Shot']/_tot:.0f} % | "
                       f"{100*_v['DarkShot']/_tot:.0f} % | {100*_v['OffsetFPN']/_tot:.0f} % |"
                       for _q, _v, _tot in _rows))
_q0, _v0, _t0 = _rows[0]
_pq = [q for q, v, t in _rows if v['PRNU'] < 0.1*t]
_prnu_q = f'{max(_pq):.0f}' if _pq else 'none of the levels tabulated'
_dom = max(('read noise', _v0['RN']), ('signal shot', _v0['Shot']), ('dark shot', _v0['DarkShot']),
           ('offset fixed pattern', _v0['OffsetFPN']), key=lambda t: t[1])
_termstext = (f"At the faint end — exactly the regime this test is about — an uncalibrated frame is "
              f"dominated by the **{_dom[0]} and the dark shot noise**, with read noise only "
              f"{100*_v0['RN']/_t0:.0f} % of the variance at {_q0:.0f} e-. Both are removable: the pattern by "
              f"calibration, the dark signal by a shorter exposure. Read noise only becomes the thing "
              f"worth improving once they are gone, and PRNU stays below a tenth of the variance up "
              f"to {_prnu_q} e-.")

_lf = []
def _figtxt(name, cap):
    return f'![{cap}]({name})\n\n*{cap}*\n'
_lf.append(_figtxt('fig_dark_ladder_fit.png',
    'The dark-ladder fit. Filled symbols are the steps in the window, open symbols the steps left '
    'out; the middle panel is the extrapolation of that line to zero exposure, whose intercept is '
    'the dark-route threshold, with the error bar this report quotes for it. The right panel is the '
    'residual column of the table above.'))
_lf.append(_figtxt('fig_light_ladder_fit.png',
    'The bright-ladder fit, the same way. Its intercept is not yet the light-route threshold: the '
    'charge the dark current collected during the exposure has to be added, which a line against '
    'intensity cannot see.'))

md = '\n'.join(MD).replace('__LADDERFIGS__', '\n'.join(_lf)) \
                   .replace('__CHI2__', _chi).replace('__TERMS__', _terms) \
                   .replace('__TERMSTEXT__', _termstext) \

with open(os.path.join(OUT, 'report.md'), 'w') as fh:
    fh.write(md)

HTML = """<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>__TITLE__</title>
<style>
body{max-width:1100px;margin:2rem auto;padding:0 1rem;font:15px/1.6 -apple-system,Segoe UI,Roboto,sans-serif;color:#222}
h1{border-bottom:2px solid #ddd;padding-bottom:.3rem}
h2{margin-top:2.2rem;border-bottom:1px solid #eee}
table{border-collapse:collapse;font-size:13px;margin:1rem 0}
th,td{border:1px solid #ddd;padding:3px 8px;text-align:left;vertical-align:top}
th{background:#f5f5f5}
img{max-width:100%;margin:.6rem 0;border:1px solid #eee}
em{color:#666;font-size:13px}
table em,table strong{font-size:inherit;color:inherit}
code{background:#f5f5f5;padding:1px 4px}
pre{background:#f7f7f7;padding:.6rem;overflow-x:auto}
@page{size:A4 portrait;margin:12mm 10mm}
@media print{
  body{max-width:none;margin:0;padding:0;font-size:11.5px}
  table{display:table;width:100%;overflow:visible;font-size:10px;margin:.5rem 0}
  th,td{white-space:normal;overflow-wrap:anywhere;padding:2px 4px;line-height:1.2}
  img{page-break-inside:avoid;max-width:100%}
  h1,h2,h3{page-break-after:avoid}
  p,li{orphans:2;widows:2}
  pre{font-size:9px;page-break-inside:avoid}
}
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
    fh.write(HTML.replace('__MD__', md.replace('</script', '<\\/script'))
                 .replace('__TITLE__', f'{PT["Lot"]} {PT["Die"]} run {PT["Run"]}'))
print(f'report.md / report.html -> {OUT}')
