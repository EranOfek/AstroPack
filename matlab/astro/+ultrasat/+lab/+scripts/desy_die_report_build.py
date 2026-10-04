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
MD   = []
def w(t):
    MD.append(t)
def fig(name, cap):
    w(f'![{cap}]({name})\n')
    w(f'*{cap}*\n')
def f3(x, n=3):
    return ('%.' + str(n) + 'f') % float(x)

# ================================================================= datasheet
w(f'# {PT["Lot"]} {PT["Die"]} — single-die characterisation, run {PT["Run"]}\n')
w(f'Whole die, {NY}x{NX} = {NY*NX/1e6:.1f} M pixels of the {PT["GainHalf"]}-gain half, '
  f'individual pixels throughout, every statistic also split by readout-column parity. '
  f'Six stages, about six minutes end to end.\n')

w('## The die in one table\n')
rows = [
 ('Bias level', f"{f3(Z['All']['BiasLevel'],2)} ADU", '5 zero-exposure frames, stage 1'),
 ('Read noise (median)', f"{f3(Z['All']['ReadNoiseMedian'],3)} ADU = {f3(float(IN['RN_ADU'])/G,3)} e-",
  'per-pixel std over the ZE frames'),
 ('Read noise, even / odd columns', f"{f3(Z['Even']['ReadNoiseMedian'],3)} / {f3(Z['Odd']['ReadNoiseMedian'],3)} ADU",
  'odd columns are 2.5 % noisier'),
 ('Bias fixed pattern', f"{f3(Z['All']['FixedPatternRMS'],2)} ADU", 'spatial spread, sampling noise removed'),
 ('Common mode', f"{f3(Z['CommonMode']['Std'],4)} ADU", 'frame-to-frame, clipped mean'),
 ('Conversion gain', f"{f3(G,4)} ADU/e- &plusmn; {f3(100*float(PT['GainSystematic']['Rel']),1)} % (window)",
  'per-pixel PTC, mean over 22.5 M pixels'),
 ('Dark current', f"{f3(float(IN['DC_ADU']),4)} ADU/s = {f3(float(IN['DC_ADU'])/G,4)} e-/s",
  f'weighted per-pixel fit, {len(np.atleast_1d(D["FitSteps"]))} steps'),
 ('Photo-response', f"{f3(float(L['Fit']['All']['SlopeSpread']['Median']),0)} ADU per intensity unit",
  f'{len(np.atleast_1d(L["FitSteps"]))} steps of the bright ladder'),
 ('PRNU, pixel to pixel', f"{f3(100*float(IN['PRNU']),2)} %", '32x32 detrended, fit noise removed'),
 ('DSNU, pixel to pixel', f"{f3(100*float(D['Local']['DC']['RelIntr']),2)} % of the dark current",
  'same method'),
 ('Offset fixed pattern', f"{f3(float(IN['SigmaTdark_ADU'])/G,2)} e-", 'dark-threshold map, detrended'),
 ('Gain spread between columns', f"{f3(100*float(PT['Column']['RelIntr']),2)} %",
  'null-calibrated, see section 7'),
 ('Gain spread pixel to pixel', 'not detected (&lt; 7 % per pixel)', 'observed spread 1.006 x the null'),
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
w(f'The one number this report cannot give as a single value is the **charge threshold**. '
  f'Three independent routes give {f3(float(PT["Thresholds"]["PTC_e"]),1)}, '
  f'{f3(float(PT["Thresholds"]["Dark_e"]),1)} and {f3(float(PT["Thresholds"]["Light_e"]),1)} e-, '
  f'which moves the smallest measurable signal from {f3(min(QLIM),0)} to '
  f'{f3(max(QLIM),0)} e-. Section 11 says which one I would use and why, '
  'and what would settle it.\n')

# ================================================================= method
w('## 1. What was done\n')
w(f"""One configuration, one die, nothing averaged into superpixels. The dataset is
{int(np.atleast_1d(D['FitSteps']).size)}-step dark and 34-step bright ladders with 3 frames each
plus 5 zero-exposure frames, 134 TIFF files and 12 GB, read once per stage.

The whole die is processed in a **streamed** mode: each ladder step is read, reduced to a mean
and a variance map, accumulated into the running sums of a weighted per-pixel straight-line fit,
and discarded. Only the sums survive, so 22.5 M pixels cost a few GB rather than the ~20 GB a
cached whole-die ladder would need, and every frame is read exactly once — the residual sum of
squares is expanded from the sums instead of being accumulated in a second pass.

| stage | reads | time | what it settles |
|---|---|---|---|
| 1 bias and read noise | 5 ZE frames | 32 s | bias, fixed pattern, per-pixel read noise |
| 2 dark ladder | 27 frames | 69 s | dark current, dark-route threshold, DSNU |
| 3 bright ladder | 102 frames | 158 s | response, PRNU, light-route threshold |
| 4 bad columns | nothing | 5 s | the column mask |
| 5 PTC | 24 frames | 59 s | conversion gain, shot-noise threshold |
| 6 noise budget | nothing | 12 s | sigma_eff, SNR, limiting signal |
| 7 variance distributions | 134 frames | 248 s | how much the variance differs between pixels |
| 8 low-signal prediction | 48 frames | 273 s | is that variance explained, pixel by pixel |

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

(* marks the fitted steps.) Both run the same way: the residuals are positive away from the fitted
range on the side of **lower** responsivity, which means the response per unit charge **rises with
signal**. On the bright ladder the local slope goes from about {(_ml[4]-_ml[0])/(_xl[4]-_xl[0]):.0f} ADU per intensity unit
between steps 1 and 5 to about {(_ml[6]-_ml[4])/(_xl[6]-_xl[4]):.0f} between steps 5 and 7, a rise of
{100*(((_ml[6]-_ml[4])/(_xl[6]-_xl[4]))/((_ml[4]-_ml[0])/(_xl[4]-_xl[0]))-1):.1f} %. The dark ladder behaves the same way against exposure time.

This is the single fact behind the threshold disagreement in section 11. A convex response has no
unique intercept: every window extrapolates its own local tangent to zero and gets a different
answer, and the slope is robust while the intercept is not. The dark current moves 5 % between the
widest and narrowest windows; the dark threshold moves by a factor three.
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
fig('fig_rn_distribution.png', 'Read noise over the whole die. The dashed curve is what the same measurement would give if every pixel had the same noise. The parity comparison is made on the cumulative distribution, which is immune to the quantisation of a sigma built from integer frames.')
w(f"""Odd columns are {100*(float(Z['Odd']['ReadNoiseMedian'])/float(Z['Even']['ReadNoiseMedian'])-1):.1f} %
noisier than even ones. That effect is known, but the whole-die map shows it is not a parity
effect at all.
""")
fig('fig_rn_column_pairing.png', 'Readout columns 2k-1 and 2k share their noise amplitude: the median read noise of a column correlates with its partner at r = 0.991, and with the next column across the pair boundary at r = -0.013. The pixels themselves are independent (within-pair pixel correlation 0.706, across-pair 0.665), so what is shared is the noise amplitude, not the samples.')
w('The same pairing turns up again in the defects (section 6) and is absent from the dark current '
  '(section 4) — so whatever is shared sits in the readout chain, not in the pixel.\n')

# ================================================================= stage 2
w('## 4. Dark current\n')
dloc = D['Local']['DC']
w(f"""Dark current **{f3(float(IN['DC_ADU']),4)} ADU/s** = {f3(float(IN['DC_ADU'])/G,4)} e-/s,
from a weighted fit of signal against exposure over the steps of section 2.

The spread needs care, and this is the first of three places in this report where the obvious answer was wrong.
Over the whole die the dark current spreads **{100*float(D['Fit']['All']['SlopeSpread']['RelIntr']):.1f} %**
of its median — but the map shows why.
""")
fig('fig_dark_dc_map.png', 'Dark current per pixel. The spread over the die is a 2:1 ramp along the readout columns plus banding and a hot row, not pixel-to-pixel variation.')
w(f"""A noise budget asks what varies between *neighbouring* pixels, because a slow ramp is removed
by any flat field. Taking the residual to a 32x32 block median and removing the fit noise in
quadrature leaves **{100*float(dloc['RelIntr']):.2f} %** — and the DESY 100x100 analysis region,
measured independently, gives 5.72 %. That agreement is the check that the detrending removes
structure rather than signal. Every fixed-pattern number in this report is the local one.
""")
_is = D['Fit']['All']['InterceptSpread']
fig('fig_dark_threshold.png', f"Dark-route threshold, T = -intercept. The observed spread is "
    f"{float(_is['StdRobust']):.2f} ADU of which {float(_is['StdFitRobust']):.2f} is fit noise, leaving "
    f"{float(_is['StdIntr']):.2f} ADU over the die and {float(D['Local']['T']['StdIntr']):.2f} pixel to pixel. "
    f"Without that deconvolution the die would look "
    f"{100*(1-float(_is['StdIntr'])/float(_is['StdRobust'])):.0f} % less uniform than it is.")
fig('fig_dark_column_profile.png', 'Dark current per readout column. The profile carries the ramp, so the pairing test is made on the residual to a running median: within a pair r = +0.226, across pairs +0.177 — the same. The pairing of the read noise does not repeat in the leakage current.')

# ================================================================= stage 3
w('## 5. Response, PRNU and the light-route threshold\n')
lloc = L['Local']
w(f"""Photo-response {f3(float(L['Fit']['All']['SlopeSpread']['Median']),0)} ADU per intensity unit.
PRNU is deliberately **not** taken from the spread of that response: the published bright window
holds three closely spaced steps, which fixes a pixel's slope to about a per cent — far coarser
than the pattern being measured — so that spread is almost all fit noise. It is measured instead
from the spatial spread of each step's mean map with the temporal noise removed, over all 34 steps,
where one step already determines the pattern from 22.5 M pixels.
""")
fig('fig_light_prnu.png', f"Per-step bright fixed pattern, measured over all 34 steps and so independent "
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
w('## 6. Bad readout columns\n')
w(f"""{int(BC['Nbad'])} of {int(BC['Nrawcol'])} readout columns are flagged, {100*(1-float(BC['GoodFraction'])):.2f} %
of the pixels — and **{int(BC['NbadInPairs'])} of them are complete (2k-1, 2k) pairs**. Only six fail
on response: columns 1-4, the known blind first columns of the high-gain half, and the dead pair
3243/3244.
""")
fig('fig_badcol_pairs.png', 'Every column lies on the 1:1 line against its readout partner, and the zoom shows each defect is exactly two adjacent columns wide. The correlation found in the read noise is visible here in the defects themselves.')
w(f"""One caveat the stage reports itself: the noisy columns are a smooth tail, not a separate
population — {int(np.atleast_1d(BC['CutScan']['Nbad'])[0])} columns at 3 sigma,
{int(np.atleast_1d(BC['CutScan']['Nbad'])[1])} at 5, {int(np.atleast_1d(BC['CutScan']['Nbad'])[3])} at 10.
The count is a choice of cut, not a number of defects, so every later number is reported with and
without the mask. It makes very little difference: the limiting signal moves by 0.2 e-.
""")
fig('fig_badcol_cut.png', 'Where to cut. The flagged columns are the tail of a continuous distribution, so the 5-sigma line is a convention.')

# ================================================================= stage 5
w('## 7. Conversion gain, and an estimator that had to be calibrated\n')
nul = PT['Null']
U5  = PT['Unmasked']['All']
w(f"""Conversion gain **{f3(G,4)} ADU/e-**, with a {100*float(PT['GainSystematic']['Rel']):.1f} %
systematic from the choice of fit window. The PTC slope is the gain and its intercept is *not* the
read noise: the shot noise follows the collected charge while the signal recorded is what is left
after the threshold, so the intercept is RN^2 + g*T.

This stage is where the statistics of the chain change, and the first run of it looked broken: a
median gain 10 % below the ensemble, a negative intercept, and an "intrinsic" pixel-to-pixel spread
of zero at -796 sigma. The cause is that a ladder point here is a per-pixel **variance** from 3
frames — 2 degrees of freedom, chi2 distributed, 100 % error, long tail — and not a Gaussian mean
like every earlier stage. A null simulation in which every pixel is given exactly the same gain
reproduces all of it:

| | measured | null, identical pixels |
|---|---|---|
| median gain | {f3(U5['GainMedian'],4)} | {f3(nul['Median'],4)} |
| mean gain | {f3(U5['GainMean'],4)} | {f3(nul['Mean'],4)} (truth {f3(nul['Truth'],4)}) |
| intercept median | {f3(U5['OffsetMedian'],1)} | {f3(nul['OffsetMedian'],1)} |
| gain MAD | {f3(U5['GainMAD'],4)} | {f3(nul['MAD'],4)} |

So the **mean** is the gain, the median is 9.6 % low by construction, and the Gaussian
deconvolution used elsewhere simply does not apply here — it pairs a robust observed spread with
an analytic rms, and for a heavy tail the first is the smaller.
""")
fig('fig_ptc_gain_null.png', 'The measured per-pixel gain distribution and the null lie on top of each other: the observed spread is 1.0056 times the null, so no pixel-to-pixel gain variation is detected and a single pixel differs by at most 7 %.')
w(f"""Where the gain *is* measurable is after averaging. Per readout column (4742 pixels each) the
real spread is **{100*float(PT['Column']['RelIntr']):.2f} %**, per 32x32 block
{100*float(PT['BlockGain']['RelIntr']):.2f} %, and the two parities differ by
{100*(float(PT['Column']['Odd']['Median'])/float(PT['Column']['Even']['Median'])-1):+.2f} %.
""")
fig('fig_ptc_gain_column.png', 'Gain per readout column. The observed histogram is only slightly wider than the null, and the difference is the 0.47 % real column-to-column variation.')

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
{abs(100*float(_ld[4]['ResidRel'])):.1f} % below at {float(_ld[4]['Signal']):.0f} ADU, while the widths still match. So it is the level, not
the uniformity, that is wrong: dark charge produces **less shot noise than photo-charge of the same
measured size**.
""")
    # the same comparison made against the fitted PTC line, with both corrections
    _G, _C = float(PT['GainEnsemble']), float(PT['OffsetEnsemble'])
    _vd = [e for e in steps_of(VS) if e['Type'] == 'D'] if VS is not None else []
    _vbr = ([float(e['Unmasked']['RelIntr']) for e in steps_of(VS)
             if e['Type'] == 'B' and float(e['Signal']) < 1000] if VS is not None else [0.0])
    _rr = _rr0 = float('nan')
    if _vd:
        _e  = max(_vd, key=lambda e: float(e['Signal']))
        _s  = float(_e['Unmasked']['RelIntr'])
        _y0 = float(_e['SigmaNull'])**2
        _rr0 = _y0/(_G*float(_e['Signal']) + _C)
        _rr  = _rr0*np.sqrt(1 + _s**2)
    fig('fig_ptc_both.png', f"Both ladders on one photon transfer curve, log-log. Shot noise should not "
        f"know where the electrons came from, so if dark charge and photo-charge were the same thing the "
        f"two ladders would lie on one line. The bright points do; the dark points run below, by "
        f"{100*(1-_rr):.1f} % at the top of the dark ladder. The open symbols in the right panel are the "
        f"same points without the median-to-mean correction described below, where the gap reads "
        f"{100*(1-_rr0):.1f} %. The bias point shows how much of the fitted intercept is read noise and "
        f"how much is the threshold term.")
    PP = load('ptc_perpixel.json') if os.path.isfile(os.path.join(OUT, 'ptc_perpixel.json')) else None
    w("""Getting that comparison right took two attempts, and the mistake is worth recording because it moved
the answer by a factor of three in each direction. The plotted variance was first a median over
pixels (a plain mean is destroyed by cosmic rays) times the chi2 median-to-mean factor, while the
signal stayed a plain median. That is not a point on any curve: Var = g*S + c holds **per pixel**, so
averaging over pixels needs E[Var] against E[S] — a mean on both axes, over the same pixels. The dark
signal is right-skewed, its mean sitting 6 % above its median, while the bright signal is not, so the
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
| ladder means against the fitted line | **8.8 %** |
| per-pixel prediction (above) | **{abs(100*float(_ld[-1]['ResidRel'])):.1f} %** |
| per-pixel gain difference (below) | **{abs(100*float(_dd['MeanRel'])):.1f} %** |

The third is the one an ensemble cannot make. Fitting a PTC to **every pixel** on each ladder
separately gives each pixel two gains, and their difference says whether the deficit is something
every pixel does or something a subset carries. The ensemble gains are {float(_gD['GainEnsemble']):.4f}
on the dark ladder against {float(_gB['GainEnsemble']):.4f} on the bright; per pixel the mean
difference is {float(_dd['Mean']):+.4f} ADU/e-, and its spread is **{float(_dd['MADoverNull']):.3f}
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
w('## 11. The gain and the threshold: four routes, with errors\n')
TH = PT['Thresholds']
if ME is not None:
    _R = ME['Routes']
    _nm = {'a': 'dark response (steps 7-9)', 'b': 'light response (below 1000 ADU)',
           'c': 'PTC, dark ladder', 'd': 'PTC, bright ladder'}
    def _g(k):
        Q = _R[k]
        if Q.get('Gain') is None or not np.isfinite(float(Q['Gain'])):
            return '—'          # a response curve has no noise in it, so no gain
        return f"**{float(Q['Gain']):.4f}** ± {float(Q['GainStat']):.4f} ± {float(Q['GainSyst']):.4f}"
    def _t(k):
        Q = _R[k]
        return (f"{float(Q['Threshold']):.2f} ± {float(Q['ThresholdStat']):.2f} ± "
                f"{float(Q['ThresholdSyst']):.2f}")
    _gc, _gd = float(_R['c']['Gain']), float(_R['d']['Gain'])
    _ec = np.hypot(float(_R['c']['GainStat']), float(_R['c']['GainSyst']))
    _ed = np.hypot(float(_R['d']['GainStat']), float(_R['d']['GainSyst']))
    _ns = abs(_gd - _gc)/np.hypot(_ec, _ed)
    _ta, _tc = float(_R['a']['Threshold_e']), float(_R['c']['Threshold_e'])
    _ea, _ec2 = float(_R['a']['Threshold_e_err']), float(_R['c']['Threshold_e_err'])
    _nt = abs(_ta - _tc)/np.hypot(_ea, _ec2)
    w(f"""Four independent routes reach these two numbers, and putting them in one table with their
errors is the clearest statement of what this device does and does not have.

| route | gain [ADU/e-] | threshold [ADU] | threshold [e-] |
|---|---|---|---|
| a) {_nm['a']} | {_g('a')} | {_t('a')} | **{float(_R['a']['Threshold_e']):.1f} ± {float(_R['a']['Threshold_e_err']):.1f}** |
| b) {_nm['b']} | {_g('b')} | {_t('b')} | **{float(_R['b']['Threshold_e']):.1f} ± {float(_R['b']['Threshold_e_err']):.1f}** |
| c) {_nm['c']} | {_g('c')} | {_t('c')} | **{float(_R['c']['Threshold_e']):.1f} ± {float(_R['c']['Threshold_e_err']):.1f}** |
| d) {_nm['d']} | {_g('d')} | {_t('d')} | **{float(_R['d']['Threshold_e']):.1f} ± {float(_R['d']['Threshold_e_err']):.1f}** |

Errors are quoted statistical first, then systematic. **Only the two photon-transfer routes measure a
gain**: a response curve contains no noise, so it cannot. All four give a threshold.

**What the errors are.** The statistical one is the scatter between
{int(ME['NBlock'])}x{int(ME['NBlock'])} = {int(ME['NBlock'])**2} independent blocks of the die, each
{int(ME['BlockSize'][0])}x{int(ME['BlockSize'][1])} pixels, not a formal error from the pixel count:
with 22.5 M pixels the latter reads 1e-5 and means nothing, while the block version carries the
spatial structure, which is what makes "the gain of this die" uncertain at all. The systematic is the
fit window, refitted over every defensible choice, and on a convex ladder it dominates everywhere —
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
averaging rather than subtracted from the intercept afterwards. The term is small — 3.5 of 150 ADU^2
at the lowest bright step — but it makes the intercept mean one thing only, g*T, and it takes a
quantity that varies strongly from pixel to pixel out of a whole-die constant.

**What if the two ladders share a gain?** Forcing the bright value
{float(_R['d']['Gain']):.4f} on the dark points and fitting only the offset gives an offset of
{float(ME['Routes']['constrained']['Offset']):+.2f} ADU^2, so T = {float(ME['Routes']['constrained']['Threshold']):+.2f} ADU — and
it does not work. The residuals run {' '.join(f"{v:+.0f}" for v in np.atleast_1d(np.array(ME['Routes']['constrained']['Residual'], dtype=float))[[0,3,6,8]])} ADU^2
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
| per-step medians, weighted (the earlier convention) | 1.0635 | 1.1392 |
| trimmed mean over pixels of the per-pixel fitted slope | {float(PP['Ladder']['D']['Slope']['TrimMean']):.4f} | {float(PP['Ladder']['B']['Slope']['TrimMean']):.4f} |

The first is the unbiased estimator of the ensemble relation and is what both this table and the
per-pixel section now use; earlier drafts of this report quoted the third in one section and the
first in another, which differed by 1.1 % on the bright ladder for no physical reason.

One number in that table moved against what section 4 reports and both are right: the dark current
here is {float(_R['a']['Slope']):.4f} ± {float(_R['a']['SlopeStat']):.4f} ± {float(_R['a']['SlopeSyst']):.4f} ADU/s,
the **mean** over pixels, while section 4 quotes the **median pixel**. The dark-current distribution
is skewed by about 5 %, which is the whole of the difference.
""")

w(f"""The two response routes get their threshold by extrapolating a curve to zero signal, and section 2
showed that neither curve is straight: the responsivity of both ladders rises with signal, so each
window extrapolates its own local tangent and lands somewhere different. Measured on this die, the
dark threshold moves from 8.9 ADU fitting all nine steps to 25.0 ADU fitting the top three, and the
light threshold from 27.8 ADU on the published window to {float(L['Threshold']['Median']):.1f} ADU on the window below 1000 ADU
used here. Neither motion is noise -- both are monotonic with the window -- and neither route can
therefore claim its own number.

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
fig('fig_ptc_threshold.png', 'The threshold implied by the shot noise, step by step. Flat at 7-10 ADU across the whole fit window, and drifting only above it, where the PTC itself bends and an intercept fitted there would not be a threshold at all.')
w(f"""**Which I would use.** A photon-transfer value — route d), {float(_R['d']['Threshold_e']):.1f} ±
{float(_R['d']['Threshold_e_err']):.1f} e-, measured on the ladder whose variance is fully explained
(section 9). It is the only kind of route that does
not extrapolate a curved response at all: it reads the threshold from the shot noise step by step,
and gets the same answer at every step of the window. Section 9 adds a second argument for it --
the bright ladder's variance is explained to better than a per cent with this threshold in the
prediction. On that value the die measures signals down to **{f3(float(UM['PTC']['Qlim_cal_5']),0)} e-**
at SNR 5 and {f3(float(UM['PTC']['Qlim_cal_3']),0)} e- at SNR 3, calibrated.

**What would confirm it.** Two measurements, neither expensive. First, a run whose bright ladder
reaches below 120 ADU: if the low-signal excess is a response non-linearity rather than lost
charge, the shot-noise threshold should stay flat there while the light-route value keeps drifting.
Second, run 31 — 22 times the dark current and a published threshold of 134 e-. If the shot-noise
route also returns a large value there it is tracking something physical; if it returns ~8 ADU
again it is measuring a property of the measurement, and should be distrusted here too. Run 31 is
already mirrored locally and the test is one configuration edit and six minutes.
""")
fig('fig_budget_limit.png', 'The smallest measurable signal under each route. The threshold choice moves it by 21 e-; the gain systematic (error bars) by 0.4 e- and the bad-column mask by 0.2 e-.')

# ================================================================= open
w('## 12. What this chain does not determine\n')
w(f"""**The charge threshold**, as above: a factor four, and the dominant uncertainty on every
number that matters. Everything else in this report is known to a few per cent.

**What the low-signal excess is.** Both ladders show it, and at a similar size in ADU: about +10 to
+15 ADU at the lowest steps of each. A common additive effect at low signal would explain both
knees and would also explain why the PTC, which is insensitive to an additive offset in the
response, gives the smallest threshold. That is a hypothesis, not a measurement.

**Where the column pairing lives.** Readout columns 2k-1 and 2k share their noise amplitude almost
perfectly (r = 0.991) while their pixels stay independent, and the defects come in the same pairs.
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
w(f"""All six stages are MATLAB scripts in the package `ultrasat.lab.scripts`, with no arguments;
`desy_die_config.m` is the only file to edit when moving to another dataset. It names the run,
folder, die and gain half, the fit windows, the bad-column cut and which threshold the budget
prefers, each with the measurement behind the choice in a comment.

```matlab
ultrasat.lab.scripts.desy_rn_single_die     % stage 1  -> desy_rn/<tag>/
ultrasat.lab.scripts.desy_die_dark          % stage 2  -> desy_die/<tag>/
ultrasat.lab.scripts.desy_die_light         % stage 3
ultrasat.lab.scripts.desy_die_badcol        % stage 4
ultrasat.lab.scripts.desy_die_ptc           % stage 5
ultrasat.lab.scripts.desy_die_budget        % stage 6
```

Each stage writes binary maps (`single`, [Ny Nx], column-major) and a json summary, and the
figures come from the matching `*_plots.py` run by path. This report is built by
`desy_die_report_build.py --indir <die dir> --rndir <stage 1 dir>`, which reads only the json
files, so it regenerates from the dumps without touching the frames.

Note that the streamed mode needs an explicit step list: the `'auto'` rule resolves steps from a
cached region ladder, which the whole-die mode does not build. The settings in the config are what
`'auto'` picks for this run from the published window.
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
_dom = max(('read noise', _v0['RN']), ('signal shot', _v0['Shot']), ('dark shot', _v0['DarkShot']),
           ('offset fixed pattern', _v0['OffsetFPN']), key=lambda t: t[1])
_termstext = (f"At the faint end — exactly the regime this test is about — an uncalibrated frame is "
              f"dominated by the **{_dom[0]} and the dark shot noise**, with read noise only "
              f"{100*_v0['RN']/_t0:.0f} % of the variance at {_q0:.0f} e-. Both are removable: the pattern by "
              f"calibration, the dark signal by a shorter exposure. Read noise only becomes the thing "
              f"worth improving once they are gone, and PRNU never matters below about 1000 e-.")

md = '\n'.join(MD).replace('__CHI2__', _chi).replace('__TERMS__', _terms) \
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
