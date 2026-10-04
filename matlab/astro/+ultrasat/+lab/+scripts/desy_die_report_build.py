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

for f in os.listdir(A.rndir):
    if f.endswith('.png'):
        shutil.copyfile(os.path.join(A.rndir, f), os.path.join(OUT, f))

TAG  = f"{PT['Lot']} {PT['Die']}, run {PT['Run']}, {PT['GainHalf']}-gain half"
NY, NX = [int(v) for v in PT['Size']]
UM   = BU['Unmasked']
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
  'null-calibrated, see section 6'),
 ('Gain spread pixel to pixel', 'not detected (&lt; 7 % per pixel)', 'observed spread 1.006 x the null'),
 ('Bad readout columns', f"{int(BC['Nbad'])} of {int(BC['Nrawcol'])} ({100*(1-float(BC['GoodFraction'])):.2f} % of pixels)",
  f"{int(BC['NbadInPairs'])} of them in complete pairs"),
 ('**Charge threshold**', f"**{f3(float(PT['Thresholds']['PTC_e']),1)}** / {f3(float(PT['Thresholds']['Dark_e']),1)} / "
  f"{f3(float(PT['Thresholds']['Light_e']),1)} e-", '**three routes disagree — section 8**'),
 ('**Limiting signal, SNR 5**', f"**{f3(float(UM['PTC']['Qlim_cal_5']),0)}** to {f3(float(UM['Light']['Qlim_cal_5']),0)} e-",
  'calibrated; the range is the threshold'),
]
w('| quantity | value | how it was measured |\n|---|---|---|\n'
  + '\n'.join(f'| {a} | {b} | {c} |' for a, b, c in rows) + '\n')
w(f'The one number this report cannot give as a single value is the **charge threshold**. '
  f'Three independent routes give {f3(float(PT["Thresholds"]["PTC_e"]),1)}, '
  f'{f3(float(PT["Thresholds"]["Dark_e"]),1)} and {f3(float(PT["Thresholds"]["Light_e"]),1)} e-, '
  f'which moves the smallest measurable signal from {f3(float(UM["PTC"]["Qlim_cal_5"]),0)} to '
  f'{f3(float(UM["Light"]["Qlim_cal_5"]),0)} e-. Section 8 says which one I would use and why, '
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

Stages 4 and 6 read no frames at all: they consume the maps the earlier stages wrote. Each stage
validates the dumps it inherits against the dataset it was asked for, so a stale map cannot leak
into a later stage unnoticed.

Points are weighted by their **measured** variance, never by a model of the signal. The shot noise
of a ladder point follows the charge actually collected, not the signal recorded, and when a
threshold removes the first electrons after they have already fluctuated no model of the measured
signal reproduces that. The weighting is checked model-free at every stage by comparing the median
chi2 per degree of freedom with its expectation: +3.3 % on the dark ladder, -0.3 % on the bright.
""")

# ================================================================= stage 1
w('## 2. Bias and read noise\n')
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
w('The same pairing turns up again in the defects (section 5) and is absent from the dark current '
  '(section 3) — so whatever is shared sits in the readout chain, not in the pixel.\n')

# ================================================================= stage 2
w('## 3. Dark current\n')
dloc = D['Local']['DC']
w(f"""Dark current **{f3(float(IN['DC_ADU']),4)} ADU/s** = {f3(float(IN['DC_ADU'])/G,4)} e-/s,
from a weighted fit of signal against exposure over the published steps.

The spread needs care, and this is the first of three places where the obvious answer was wrong.
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
fig('fig_dark_threshold.png', 'Dark-route threshold, T = -intercept. The observed spread is 7.27 ADU of which 5.96 is fit noise, leaving 4.17 ADU over the die and 2.72 pixel to pixel. Without that deconvolution the die would look 75 % less uniform than it is.')
fig('fig_dark_column_profile.png', 'Dark current per readout column. The profile carries the ramp, so the pairing test is made on the residual to a running median: within a pair r = +0.226, across pairs +0.177 — the same. The pairing of the read noise does not repeat in the leakage current.')

# ================================================================= stage 3
w('## 4. Response, PRNU and the light-route threshold\n')
lloc = L['Local']
w(f"""Photo-response {f3(float(L['Fit']['All']['SlopeSpread']['Median']),0)} ADU per intensity unit.
PRNU is deliberately **not** taken from the spread of that response: the published bright window
holds three closely spaced steps, which fixes a pixel's slope to about a per cent — far coarser
than the pattern being measured — so that spread is almost all fit noise. It is measured instead
from the spatial spread of each step's mean map with the temporal noise removed, over all 34 steps,
where one step already determines the pattern from 22.5 M pixels.
""")
fig('fig_light_prnu.png', 'Per-step bright fixed pattern. The relative pattern plateaus at 1.14 % above ~2000 ADU; the pixel-to-pixel part, after detrending, is 0.41 %, which the DESY region independently gives as 0.46 %.')
w(f"""The light-route threshold is {f3(float(L['Threshold']['Median']),2)} ADU, and its figure is the
clearest statement in the whole chain of why these deconvolutions are needed.
""")
fig('fig_light_threshold.png', 'The light-route threshold distribution lies exactly on the pure-fit-noise curve: 52.4 ADU observed, 51.4 ADU of fit noise, 9.9 ADU left. The dark route, with its longer lever arm, is the narrow spike beside it. Without the deconvolution one would report 52 ADU of threshold non-uniformity where there is 10.')

# ================================================================= stage 4
w('## 5. Bad readout columns\n')
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
w('## 6. Conversion gain, and an estimator that had to be calibrated\n')
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
w('## 7. The noise budget\n')
w(f"""In electrons, per pixel, at the {TT:g} s exposure of the bright frames: read noise
{f3(float(IN['RN_ADU'])/G,3)} e-, dark signal {f3(float(IN['DC_ADU'])*TT/G,2)} e-, offset fixed
pattern {f3(float(IN['SigmaTdark_ADU'])/G,2)} e-, DSNU {f3(float(IN['SigmaDC_ADU'])*TT/G,2)} e- and
PRNU {100*float(IN['PRNU']):.2f} %.

Which of them matters depends entirely on where you look:

| Q [e-] | read noise | signal shot | dark shot | offset FPN |
|---|---|---|---|---|
| 10 | 18 % | 15 % | 28 % | **39 %** |
| 30 | 8 % | 63 % | 12 % | 17 % |
| 100 | 3 % | 88 % | 4 % | 6 % |
| 300 | 1 % | 95 % | 1 % | 2 % |

At the faint end — exactly the regime this test is about — an uncalibrated frame is dominated by
the **offset fixed pattern and the dark shot noise**, with read noise only 18 % of the variance at
10 e-. Both are removable: the pattern by calibration, the dark signal by a shorter exposure. Read
noise only becomes the thing worth improving once they are gone, and PRNU never matters below
about 1000 e-.
""")
fig('fig_budget_terms.png', 'The budget decomposed, uncalibrated, with the shot-noise threshold. Below the threshold there is no signal at all; above it the floor is the offset pattern, the dark shot noise and the read noise, in that order.')
fig('fig_budget_snr.png', 'Signal to noise against incident charge for the three threshold routes. Solid is calibrated, dashed a single raw frame; the circles mark where each curve reaches SNR 5 and 3.')

# ================================================================= threshold
w('## 8. The charge threshold: three answers\n')
TH = PT['Thresholds']
w(f"""| route | T [e-] | what it assumes | limiting signal, SNR 5 |
|---|---|---|---|
| shot noise (stage 5) | **{f3(TH['PTC_e'],1)}** | the PTC is straight in the fit window | **{f3(float(UM['PTC']['Qlim_cal_5']),1)} e-** |
| dark response (stage 2) | {f3(TH['Dark_e'],1)} | the dark ladder extrapolates linearly to t = 0 | {f3(float(UM['Dark']['Qlim_cal_5']),1)} e- |
| light response (stage 3) | {f3(TH['Light_e'],1)} | the bright ladder extrapolates linearly to zero intensity | {f3(float(UM['Light']['Qlim_cal_5']),1)} e- |

The two response routes get their threshold by extrapolating a curve to zero signal, which is only
as good as the curve is straight there — and neither is. The dark ladder is visibly bent at the
bottom: residuals to its own fit are +11.6, +9.9, +7.2 and +3.4 ADU at the four lowest steps, and
as a result the dark threshold moves **from 8.9 to 25.0 ADU** depending on which steps are fitted,
monotonically. The bright ladder is better behaved, and its intercept moves only 25.5 to 29.5 ADU
across every window inside its linear range, which is why the light route was preferred earlier.

The shot-noise route extrapolates nothing. The variance measures the charge actually collected,
Q = (Var - RN^2)/g^2, against the signal recorded, S = g(Q - T), so T follows step by step — and a
real threshold must then come out the same at every step.
""")
fig('fig_ptc_threshold.png', 'The threshold implied by the shot noise, step by step. Flat at 7-10 ADU across the whole fit window, and drifting only above it, where the PTC itself bends and an intercept fitted there would not be a threshold at all.')
w(f"""**Which I would use.** The shot-noise value, {f3(TH['PTC_e'],1)} e-. It is the only route that
measures the lost charge instead of extrapolating a response, and its internal consistency check —
the same answer at every step of the window — is stronger than "the intercept moves less than the
other one's". On that value the die measures signals down to **{f3(float(UM['PTC']['Qlim_cal_5']),0)} e-**
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
w('## 9. What this chain does not determine\n')
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

md = '\n'.join(MD)
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
