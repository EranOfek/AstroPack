#!/usr/bin/env python3
"""Build the DESY PTC reproduction report (Markdown + HTML + PDF) from
desy_ptc_report/results.json and the PNG figures written by desy_ptc_report_run.m."""
import json, html, os, subprocess, shutil, sys
from pathlib import Path

D = Path('/home/sasha/claude/desy_ptc_report')
R = json.load(open(D / 'results.json'))

def denull(o):
    if isinstance(o, dict):
        return {k: denull(v) for k, v in o.items()}
    if isinstance(o, list):
        return None if len(o) == 0 else [denull(v) for v in o]
    return o
R = denull(R)

def fix_light(s):
    # threshold(light) = DC*ExpSen - intercept (the driver ran before the sign fix; recompute from the saved medians)
    if 'MedianDCtermADU' not in s or s.get('BrightFit') is None:
        return
    g = s['GainUsed']
    s['MedianLightADU'] = s['MedianDCtermADU'] - s['BrightFit']['MedianIntercept']
    s['MedianLightE'] = s['MedianLightADU'] / g
    pt = {r['Quantity']: r for r in s.get('ParityTable', [])}
    if 'ThresholdLightADU' in pt and 'DarkSlope' in pt and 'BrightIntercept' in pt:
        for grp in ('Even', 'Odd'):
            pt['ThresholdLightADU'][grp] = pt['DarkSlope'][grp] * s['ExpSen'] - pt['BrightIntercept'][grp]
            pt['ThresholdLightE'][grp] = pt['ThresholdLightADU'][grp] / g
        for q in ('ThresholdLightADU', 'ThresholdLightE'):
            r = pt[q]; r['Diff'] = r['Even'] - r['Odd']; r['DiffOverSE'] = None
            r['RelDiff'] = 2 * r['Diff'] / (r['Even'] + r['Odd']) if (r['Even'] + r['Odd']) else None
for _s in R['Region'] + R.get('Parity400', []) + ([R['FullDie']] if R.get('FullDie') else []):
    fix_light(_s)
DECK = {  # UC-3400-TN175-05 numbers (median / std over 10000 pixels)
    ('31', 'W04_D07'): dict(DS=6.123, DSs=0.392, DI=-140.8, DIs=30.7, DR=13.73, DN='4/9', BS=1.356e4, BSs=445, BI=-2.341, BIs=59, BR=12.37, BN='3/10', ThD='−140 ± 30', ThL='−92.3 ± 60'),
    ('31', 'W08_D02'): dict(DS=7.589, DSs=0.488, DI=-193.5, DIs=34.8, DR=None,  DN='4/9', BS=1.297e4, BSs=436, BI=-25.37, BIs=58, BR=None,  BN='3/10', ThD='−193.5 ± 35', ThL='−140 ± 60'),
    ('32', 'W04_D07'): dict(DS=0.2933, DSs=0.0993, DI=-24.8, DIs=50.9, DR=1.4e-12, DN='2/9', BS=None, BSs=None, BI=None, BIs=None, BR=None, BN=None, ThD=None, ThL=None),
}
SLIDES = {('31', 'W04_D07'): '16, 17, 19, 20', ('31', 'W08_D02'): '29, 30', ('32', 'W04_D07'): '26, 27'}

def f(x, n=3):
    if x is None or (isinstance(x, float) and x != x):
        return '—'
    if isinstance(x, (list, tuple)):
        return '—'
    if isinstance(x, str):
        return x
    if abs(x) >= 1e4:
        return f'{x:.4g}'
    return f'{x:.{n}f}' if n else f'{x:.0f}'

def pct(x):
    return '—' if x is None or isinstance(x, (list, tuple)) else f'{100*x:.2f} %'

def pm(a, b, n=1):
    return f'{f(a, n)} ± {f(b, n)}'

def region(run, die):
    for s in R['Region']:
        if s['Run'] == run and s['Die'] == die:
            return s
    return None

L = []
w = L.append
w('# Reproduction of the DESY flavour-test PTC analysis with `ultrasat.lab.PTCAnalysis`')
w('')
w('Lot TH02954, runs 31 (AV settings) and 32 (aSpect settings), −50 °C, high-gain readout. Reference: DESY technical note UC-3400-TN175-05 “Flavors analysis — Prod 3” (A. Porelli, D. Kuesters, 2026-09-04). Analysis code: AstroPack `dev1`, `ultrasat.lab.readPTC` / `writeFITS` / `PTCAnalysis` (commits `d86daa9b4`, `ec8613631`). Data: `/bigdata3/projects/ultrasat/DESY/`.')
w('')
w('## 1. Data')
w('')
w('| Item | Value |')
w('|---|---|')
w('| Dies | TH02954 W04_D03, D05, D06, D07 (flavour 6) and W08_D02, D03, D04 (flavour 2); all with RBS mask |')
w('| Runs | 31 = `AV_Config` (VDD_SF 3.3 / RST_L 1.0 / RST_SEL 3.8 V), 32 = `aSpect_Config` (3.0 / 1.2 / 3.5 V) |')
w('| Frames per die | 5 ZE (zero exposure), 9 × 3 D (dark, 15–600 s), 34 × 3 B (bright, `Bright_Intensity` 1e-5 … 1.4e-2 at 15 s) |')
w('| TIFF | 9482 × 4742 uint16; each row = 2 counter columns (row number, row number + 32768) + 4740 low-gain + 4740 high-gain columns of the same pixels |')
w('| Analysis region | DESY 100 × 100 region: rows 1860:1960, cols 1360:1460 (0-based, DESY orientation) = `CCDSEC [1361 1460 1861 1960]` |')
w('')
w('## 2. Data reduction process')
w('')
w('1. **Read and split** (`ultrasat.lab.readPTC`, `Gain=\'high\'`): the two leading counter columns of every TIFF row are dropped (`MetaCols`), the high-gain half (columns 4743–9482, 1-based) is extracted and put into the DESY orientation, `rot90(Half.\', 2)` (transposed and rotated by 180°). This orientation was established empirically: it is the only one of the eight dihedral transforms whose region medians reproduce the deck (596 / 2800 / 3501 ADU vs 600 / 2790 / 3500 at 120 / 480 / 600 s). Sections are mapped back to the raw TIFF so only the region is read. The frames can be exported as FITS (`ultrasat.lab.writeFITS`, one file per gain half, full header).')
w('2. **Zero level, bias and read noise** (`subtractZero`, `zeroStats`): the 5 ZE frames are combined (mean) into a bias frame that is subtracted from every D and B frame (the deck’s “DARK − ZERO”, “BRIGHT − ZERO”). From the same frames: **bias level** = median of the bias frame (its spatial std = fixed-pattern amplitude) and **read noise** with three estimators — temporal (per-pixel std across the 5 frames, N−1: median and rms over the pixels), frame-difference std(F1 − F2)/√2 (free of fixed pattern), spatial std of a single frame (includes the fixed pattern) — in ADU, and in e⁻ with the measured gain; cross-checked against √offset of the temporal PTC line (Var = gain × mean + offset).')
w('3. **Step combination** (`combineSteps`): the 3 repeats of every step are averaged per pixel; the temporal variance per pixel, the frame-difference variance var(F1 − F2)/2 and the spatial variance of single frames are kept as three independent PTC estimators.')
w('4. **Per-pixel linear fits** (`fitResponse`): signal vs exposure time (D) and vs light intensity (B) for every pixel, using a subset of steps. The deck fits “the linear regime ~1 k–2.5 k ADU”, but its point counts (4/9 dark, 3/10 bright on run 31; 2/9 dark on run 32) are not reproduced by any fixed ADU window; the deck’s selections are therefore applied explicitly (`FitSteps`: dark steps 4–7 = 120–360 s for run 31, 8–9 = 480–600 s for run 32; bright: the default window `[1000 2500]` ADU gives exactly the deck’s three points, intensities 0.09 / 0.14 / 0.18). Slope, intercept and residual rms are summarised by the median and std over the pixels.')
w('5. **Gain** (`fitGain`): straight line through variance vs mean of the bright ladder for mean signals in `[300 2500]` ADU (below the 3–5 kADU variance dip and far from ADC saturation at 16383 ADU). The temporal estimator is used for the ADU → e⁻ conversion unless `GainADU` is set.')
w('6. **Threshold** (`threshold`): dark method −I_D; light method DC × 15 s − I_B; both in ADU and e⁻, positive = electrons lost. In detail:')
w('   The threshold is the charge a pixel must collect before its output starts to rise — charge that is lost (e.g. left under the transfer gate) and never appears in the signal. Both estimates come from the two per-pixel straight-line fits of item 4 on the zero-subtracted signal:')
w('   - *Dark ladder* (9 exposures in the dark, t = 15–600 s): S_dark(t) = DC · t + I_D. **DC** is the slope, the dark current in ADU/s; **I_D** is the intercept, the value of the line at t = 0. Without a threshold a zero-length exposure would give zero signal and I_D = 0; with lost charge the line reaches S = 0 only after some accumulation time, so I_D < 0 (e.g. −141 ADU for W04_D07, run 31).')
w('   - *Bright ladder* (34 illuminated exposures of fixed length t = ExpSen = 15 s, `PTC_ExpTime`, with the LED intensity as the variable): S_bright(int) = R · int + I_B, with **R** the response in ADU per intensity unit and **I_B** the intercept at zero intensity.')
w('   - **Dark method:** Threshold = −I_D. The intercept is the signal deficit at t = 0, so its negative is the charge lost (141 ADU in the example).')
w('   - **Light method:** Threshold = DC · t − I_B. A bright frame of zero intensity is not a zero-signal frame: during its 15 s the pixel also collects dark charge DC · t (6.14 ADU/s × 15 s ≈ 92 ADU, the “DC term”), which already fills part of the threshold, so I_B is the deficit *after* that dark charge was absorbed and the DC term is added back (92.1 − 0.2 ≈ 92 ADU in the example). DC is taken from the dark fit of the same pixel; the summary values use the median DC, as the deck does. The deck quotes the same quantity with the opposite sign as a “corrected intercept”: (−2.3) − 6.1 × 15 = −92.3.')
w('   - **Electrons:** both ADU values are divided by the conversion gain G [ADU/e⁻] of item 5 (130 e⁻ dark-method, 85 e⁻ light-method for the example). Positive = electrons lost; a negative light-method value means the bright line extrapolates above zero at zero intensity, i.e. charge is present even without light.')
w('   - The two methods probe different regimes: the dark method extrapolates a slow, linear accumulation (minutes) to t = 0 and relies on the dark current being linear from the start — under aSpect settings the dark ladder stays below 200 ADU, so this extrapolation is poorly constrained; the light method uses fast illumination at a fixed 15 s and subtracts the dark contribution, and repeats better between runs.')
w('7. **Odd / even columns** (`Parity=\'rawcol\'`): every pixel is tagged by the parity of its column in the stored TIFF half (= the readout column; these are rows in the DESY orientation), and all statistics are repeated for the two groups.')
w('8. **Full die** (`CCDSEC=[]`): the same fits are accumulated frame by frame over all 4740 × 4742 pixels (running sums of the masked regression; ~2 GB RAM) to produce the slope / intercept / rms maps of the deck’s slide 28.')
w('')
w('## 3. Bias level and read noise (zero-exposure frames)')
w('')
w('All dies, DESY region (10 000 pixels) and the 400 × 400 region (160 000 pixels); ADU unless stated. e⁻ values (median / rms) use the temporal PTC gain of the die measured in §4. “PTC √offset” is the square root of the intercept of the variance–mean line fitted to the bright ladder in 300–2500 ADU.')
w('')
w('| Run | Die | Region | Bias level | Bias spatial std | RN temporal (median / rms) | RN frame-diff | RN spatial | RN [e⁻] median / rms | PTC √offset | Bias even / odd | RN even / odd |')
w('|---|---|---|---|---|---|---|---|---|---|---|---|')
for z in R.get('Zero', []):
    reg = region(z['Run'], z['Die'])
    g = reg['GainUsed'] if reg else None
    rn_e = f'{z["ReadNoiseTemporal"]/g:.2f} / {z["ReadNoiseTemporalRMS"]/g:.2f}' if g else None
    off = reg.get('ReadNoiseFromOffset') if reg else None
    if off is None and reg and z['Region'] == 'desy':
        # driver ran before the field existed: recompute from the temporal PTC fit stored in the curve
        import math
        m, v = reg['PTCcurve']['Mean'], reg['PTCcurve']['VarTemporal']
        pts = [(a, b) for a, b in zip(m, v) if 300 <= a <= 2500 and b > 0]
        n = len(pts); sx = sum(a for a, _ in pts); sy = sum(b for _, b in pts); sxx = sum(a*a for a, _ in pts); sxy = sum(a*b for a, b in pts)
        slope = (n*sxy - sx*sy) / (n*sxx - sx*sx); inter = (sy - slope*sx) / n
        off = math.sqrt(max(inter, 0))
    w(f'| {z["Run"]} | {z["Die"]} | {z["Region"]} | {f(z["BiasLevel"],1)} | {f(z["BiasStd"],2)} | {f(z["ReadNoiseTemporal"],2)} / {f(z["ReadNoiseTemporalRMS"],2)} | {f(z["ReadNoiseDiff"],2)} | {f(z["ReadNoiseSpatial"],2)} | {rn_e or "—"} | {f(off,2) if z["Region"]=="desy" else "—"} | {f(z["Even"]["BiasLevel"],1)} / {f(z["Odd"]["BiasLevel"],1)} | {f(z["Even"]["ReadNoiseTemporalRMS"],2)} / {f(z["Odd"]["ReadNoiseTemporalRMS"],2)} |')
w('')
w('Findings: the **bias level** is 398 ± 0.5 ADU on all dies except W04_D06 and W08_D03 (394–395 ADU — the same two dies that show no dark current, §5), identical in runs 31 and 32 (it does not depend on the AV/aSpect biases), and 0.2–0.4 ADU higher on odd than on even columns. The **read noise of a typical pixel** is 1.9 ADU (median of the per-pixel temporal std; ≈1.75 e⁻), the same on all dies and for even and odd columns; the rms over pixels and the frame-difference estimate agree with each other at 2.2–3.1 ADU, i.e. a tail of noisier pixels adds ~50 % to the variance budget while the typical pixel stays at 1.9 ADU (no frame-to-frame common-mode term is needed to explain the difference). The fixed-pattern amplitude of the bias frame is 1.4–2.1 ADU in the DESY region; the 400 × 400 region of W04_D07 has a bias std of 12–13 ADU because it contains a structured feature (visible in the full-die maps), which also inflates its spatial estimate. The PTC √offset (≈8 ADU) is **not** a read-noise measurement: the variance–mean relation is curved at low signal (the deck’s low-signal excess above the gain = 1 line), so the straight line fitted in 300–2500 ADU extrapolates to an intercept far above the read-noise variance. The temporal medians are quantised (1.82 / 1.87 / 1.92 / 1.95) because only five integer-valued frames enter each pixel’s std.')
w('')
w('## 4. Comparison with the DESY deck')
w('')
w('Medians ± std over the 10 000 pixels of the DESY region; units ADU/s, ADU/int (int = `Bright_Intensity` × 1000, the deck’s `led_intensity_list`), ADU. “N” = points used / available.')
w('')
w('| Die (run, settings) | Quantity | DESY | This work |')
w('|---|---|---|---|')
for (run, die), dk in DECK.items():
    s = region(run, die)
    if s is None:
        continue
    name = f'{die} Fl.{s["Flavour"]} (run {run}, {s["Settings"]})'
    d, b = s['DarkFit'], s['BrightFit']
    w(f'| {name} | dark slope [ADU/s] | {pm(dk["DS"], dk["DSs"], 3)} | {pm(d["MedianSlope"], d["StdSlope"], 3)} |')
    w(f'| | dark intercept [ADU] | {pm(dk["DI"], dk["DIs"], 1)} | {pm(d["MedianIntercept"], d["StdIntercept"], 1)} |')
    w(f'| | dark residual rms [ADU] | {f(dk["DR"], 2)} | {f(d["MedianResidRMS"], 2)} |')
    w(f'| | dark points used | {dk["DN"]} | {d["NusedMode"]}/{d["Nsteps"]} |')
    if dk['BS'] is not None:
        w(f'| | bright slope [ADU/int] | {pm(dk["BS"], dk["BSs"], 0)} | {pm(b["MedianSlope"], b["StdSlope"], 0)} |')
        w(f'| | bright intercept [ADU] | {pm(dk["BI"], dk["BIs"], 1)} | {pm(b["MedianIntercept"], b["StdIntercept"], 1)} |')
        w(f'| | bright points used | {dk["BN"]} (of the first 10 steps) | {b["NusedMode"]}/{b["Nsteps"]} |')
        w(f'| | corrected intercept, dark method [e⁻] | {dk["ThD"]} (ADU ≈ e⁻) | {f(-s["MedianDarkE"],1)} ± {f(s["StdDarkE"],1)} at {f(s["GainUsed"],3)} ADU/e⁻; {f(-s["MedianDarkADU"]/1.05,1)} at 1.05 |')
        w(f'| | corrected intercept, light method [e⁻] | {dk["ThL"]} | {f(-s["MedianLightE"],1)} ± {f(s["StdLightE"],1)}; {f(-s["MedianLightADU"]/1.05,1)} at 1.05 |')
    w(f'| | PTC gain [ADU/e⁻] (temporal / diff / spatial) | ≈1.05 (deck, from an earlier Fl.1 measurement) | {f(s["GainTemporal"],3)} / {f(s["GainDiff"],3)} / {f(s["GainSpatial"],3)} |')
w('')
w('The slopes agree to better than 1 %, the intercepts to a few ADU (well inside the pixel-to-pixel std), and the numbers of fitted points are identical once the deck’s step selection is applied. The run-32 dark fit uses only two points, so its residual rms is zero by construction, as in the deck (1.4e-12). The deck converts ADU to e⁻ with a nominal 1.05 ADU/e⁻ (and in places treats ADU as e⁻); with the gain measured here on the same die the electron values are 4–8 % smaller.')
w('')
for (run, die), sl in SLIDES.items():
    tag = f'run{run}_{die}'
    w(f'### 4.{list(SLIDES).index((run, die)) + 1} {die}, run {run} — figures (deck slides {sl})')
    w('')
    w(f'![dark response]({tag}_resp_D.png)')
    w('')
    w(f'*Signal vs exposure time, median of 10 000 pixels with 16/84 % band, median fit and excluded points; blue/magenta: even/odd raw columns.*')
    w('')
    w(f'![dark histograms]({tag}_hist_D.png)')
    w('')
    if (run, die) != ('32', 'W04_D07'):
        w(f'![bright response]({tag}_resp_B.png)')
        w('')
        w(f'![bright histograms]({tag}_hist_B.png)')
        w('')
    w(f'![PTC]({tag}_ptc.png)')
    w('')
    w('*Photon transfer curve of the bright ladder with the three variance estimators; orange: variance = mean. The dip at 3–5 kADU and the collapse at ADC saturation (≈16 kADU) are in the data.*')
    w('')
    w(f'![PTC zoom]({tag}_ptc_zoom.png)')
    w('')

w('## 5. All dies of runs 31 and 32')
w('')
w('DESY region, same reduction. Dark: steps 4–7 (run 31) / 8–9 (run 32); bright: `[1000 2500]` window. Thresholds with the measured temporal gain.')
w('')
w('| Run | Die | Fl. | FT | Dark slope [ADU/s] | Dark intercept [ADU] | Bright slope [ADU/int] | Bright intercept [ADU] | Gain T/D/S [ADU/e⁻] | Thr. dark [e⁻] | Thr. light [e⁻] |')
w('|---|---|---|---|---|---|---|---|---|---|---|')
for s in R['Region']:
    d, b = s['DarkFit'], s['BrightFit']
    ft = 'pass' if s['Pass'] else f'bin {s["SoftBin"]}'
    w(f'| {s["Run"]} | {s["Die"]} | {s["Flavour"]} | {ft} | {pm(d["MedianSlope"], d["StdSlope"], 3)} | {pm(d["MedianIntercept"], d["StdIntercept"], 1)} | {pm(b["MedianSlope"], b["StdSlope"], 0)} | {pm(b["MedianIntercept"], b["StdIntercept"], 1)} | {f(s["GainTemporal"],3)} / {f(s["GainDiff"],3)} / {f(s["GainSpatial"],3)} | {f(s["MedianDarkE"],0)} ± {f(s["StdDarkE"],0)} | {f(s["MedianLightE"],0)} ± {f(s["StdLightE"],0)} |')
w('')
w('Threshold sign convention: positive = electrons lost before the output starts to rise (the deck quotes −threshold). “FT” is the functional-test outcome of the die (bin 2 = `VDD_GND_Shorts_VDD_SF_I` out of limits); the PTC frames exist for all dies.')
w('')
w('Observations: (i) the four FT-passing dies and W04_D03 have dark currents of 6.1–7.6 ADU/s under AV settings and 0.28–0.36 ADU/s under aSpect settings, with dark-method thresholds of 130–181 e⁻ (AV) — flavour 2 (W08) higher than flavour 6 (W04), as in the deck; (ii) **W04_D06 and W08_D03 show essentially no dark signal in high gain** (0.02 and 0.12 ADU/s under AV) and a bias level 4 ADU below the others (§3), while their bright response is normal — the dark ladder of these two bin-2 dies is not usable for a threshold; (iii) under aSpect settings the dark signal reaches only ~150 ADU at 600 s, so the two-point fits carry ±45 ADU pixel scatter and the thresholds are 20–40 e⁻ with large uncertainty, consistent with the deck’s slide 26 discussion.')
w('')

w('## 6. Odd versus even readout columns')
w('')
w('Pixels are split by the parity of their column in the stored TIFF half, counted from the first pixel column of the half (1-based, counters excluded). In the DESY orientation these are alternate rows. Table: medians over each group, difference (even − odd), the difference in units of the standard error of the median difference, and the relative difference. The spatial-variance gain is not meaningful on the 400 × 400 regions (the large-scale dark and illumination gradients dominate the spatial variance); use the temporal and frame-difference rows.')
w('')
w('Findings: the dark-current *slopes* of even and odd columns agree to 0.1–0.4 %, but the dark *intercepts* differ by 3.5–6.6 ADU (even columns more negative, 20–33 standard errors on 160 000 pixels), i.e. the even readout columns lose ~3–6 e⁻ more before responding; the bright intercepts show the same offset (4–7 ADU) and the temporal PTC gain of the even columns is 1.5–4.5 % higher (1.10–1.13 vs 1.05–1.08 ADU/e⁻). Under aSpect settings the pattern persists at smaller amplitude (1.6 ADU, 5 σ). The even columns also show a 0.2–0.4 % higher dark slope, so at 600 s they sit ~1 % above the odd ones — the alternating-row structure seen directly in the DESY-oriented 600 s dark frames (region means even 3931 vs odd 3887 ADU on W04_D07, bias included). Column parity is counted in the stored TIFF half, 1-based, after the two counter columns; with the earlier reader that did not strip the counters the two groups were labelled the other way round.')
w('')
def parity_block(s, title):
    w(f'### {title}')
    w('')
    w('| Quantity | Even | Odd | Even − Odd | Diff / SE | Relative |')
    w('|---|---|---|---|---|---|')
    for r in s['ParityTable']:
        w(f'| {r["Quantity"]} | {f(r["Even"],3)} | {f(r["Odd"],3)} | {f(r["Diff"],3)} | {f(r["DiffOverSE"],1)} | {pct(r["RelDiff"])} |')
    w('')
for s in R.get('Parity400', []):
    parity_block(s, f'{s["Die"]}, run {s["Run"]} — 400 × 400 region `CCDSEC {s["CCDSEC"]}` ({s["DarkFit"]["Npix"]} pixels)')
    tag = f'run{s["Run"]}_{s["Die"]}'
    w(f'![parity response]({tag}_400_resp_D_parity.png)')
    w('')
    w(f'![parity PTC]({tag}_400_ptc_parity.png)')
    w('')
w('### Summary over the DESY region, all dies')
w('')
w('| Run | Die | Dark slope even / odd [ADU/s] | Δ/SE | Δ rel. | Dark intercept even / odd [ADU] | Δ/SE | Bright slope Δ rel. | Gain (temporal) even / odd |')
w('|---|---|---|---|---|---|---|---|---|')
for s in R['Region']:
    pt = {r['Quantity']: r for r in s['ParityTable']}
    ds, di, bs, g = pt['DarkSlope'], pt['DarkIntercept'], pt['BrightSlope'], pt['Gain_temporal']
    w(f'| {s["Run"]} | {s["Die"]} | {f(ds["Even"],3)} / {f(ds["Odd"],3)} | {f(ds["DiffOverSE"],1)} | {pct(ds["RelDiff"])} | {f(di["Even"],1)} / {f(di["Odd"],1)} | {f(di["DiffOverSE"],1)} | {pct(bs["RelDiff"])} | {f(g["Even"],3)} / {f(g["Odd"],3)} |')
w('')

if R.get('FullDie'):
    s = R['FullDie']
    w('## 7. Full die, W04_D07 run 31 (streamed mode)')
    w('')
    w(f'All 4740 × 4742 pixels, same step selection, {s["Seconds"]:.0f} s wall time. Fraction of pixels with a valid dark fit: {100*s["DarkFit"]["FracValid"]:.2f} %.')
    w('')
    w('| Quantity | 1 % | 16 % | median | 84 % | 99 % |')
    w('|---|---|---|---|---|---|')
    for q, lab in [('DarkFit', 'dark slope [ADU/s]'), ('BrightFit', 'bright slope [ADU/int]')]:
        p = s[q]['SlopeP']
        w(f'| {lab} | ' + ' | '.join(f(v, 3 if q == "DarkFit" else 0) for v in p) + ' |')
        p = s[q]['InterceptP']
        w(f'| {lab.split()[0]} intercept [ADU] | ' + ' | '.join(f(v, 1) for v in p) + ' |')
    w('')
    w(f'Full-die medians: dark slope {f(s["DarkFit"]["MedianSlope"],3)} ADU/s, intercept {f(s["DarkFit"]["MedianIntercept"],1)} ADU (deck region: {f(region("31","W04_D07")["DarkFit"]["MedianSlope"],3)} / {f(region("31","W04_D07")["DarkFit"]["MedianIntercept"],1)}); PTC gain over the whole die {f(s["GainTemporal"],3)} / {f(s["GainDiff"],3)} / {f(s["GainSpatial"],3)} ADU/e⁻.')
    w('')
    w('![maps D](run31_W04_D07_full_maps_D.png)')
    w('')
    w('*Per-pixel dark slope, intercept and residual rms over the full die (deck slide 28).*')
    w('')
    w('![maps B](run31_W04_D07_full_maps_B.png)')
    w('')
    w('![hist D full](run31_W04_D07_full_hist_D.png)')
    w('')
    w('![slope profile](run31_W04_D07_full_slope_profile.png)')
    w('')
    w('*Row profile of the dark-slope map (DESY rows 1800–2000 = alternate raw columns): the row-to-row sawtooth of ≈0.02 ADU/s is the even/odd column difference (even 0.3 % higher, table below) riding on the large-scale gradient; the odd-column markers are too small to be resolved at this scale.*')
    w('')
    w('![full PTC parity](run31_W04_D07_full_ptc_parity.png)')
    w('')
    parity_block(s, 'Odd / even columns over the full die')

w('## 8. Caveats')
w('')
w('- The deck’s fit-point selection rule is not documented; it was reproduced by explicit step lists. A fixed 1 k–2.5 k ADU window reproduces the bright fits but not the dark ones.')
w('- The PTC gain depends on the fitted range because the variance–mean relation is not a straight line (dip at 3–5 kADU); `[300 2500]` ADU is used throughout. The deck’s 1.05 ADU/e⁻ is a rough figure from an earlier flavour-1 measurement.')
w('- `Bright_Intensity` in the configuration is 1000 × the deck’s “int”; its physical unit is not stated in any file.')
w('- `DATE-OBS` in the exported FITS is the file time, and on the `/bigdata3` copy that is the copy time (2026-09-14); the original write times survive only in the `/Data1/DESY` mirror.')
w('- Three of the seven dies failed the functional test on `VDD_GND_Shorts_VDD_SF_I`; their PTC results are reported without judgement of usability.')
w('- Read noise from five frames: the per-pixel temporal estimate has ~35 % statistical error per pixel and is quantised; the medians/rms over 10⁴–10⁵ pixels are stable to ~1 %, but a dedicated zero-exposure series (e.g. the `FT_ZE` runs 39) would give a cleaner per-pixel read-noise map.')
w('')
w('## 9. Reproducing')
w('')
w('```matlab')
w("P = ultrasat.lab.PTCAnalysis('/bigdata3/projects/ultrasat/DESY/LOT_TH02954_31_FT_PTCint_-50_2026-08-25/LOT_TH02954_W04_D07', ...")
w("        'FitSteps',struct('D',4:7,'B',[]), 'Parity','rawcol');")
w('P.run;  S = P.summary;  T = P.parityTable;')
w("P.ZeroStats                                     % bias level and read noise from the ZE frames")
w("P.plotResponse('D','Parity',true);  P.plotHistograms('D');  P.plotPTC;")
w("P.writeFITS('/data/fits');                       % one FITS per gain half")
w("F = ultrasat.lab.PTCAnalysis(P.DeviceDir, 'CCDSEC',[], 'FitSteps',P.FitSteps);  F.run;  F.plotMaps('D');")
w('```')
w('')
w('Driver of this report: `~/claude/desy_ptc_report_run.m`; builder: `~/claude/desy_ptc_report_build.py`; unit tests: `ultrasat.lab.unitTest`, `ultrasat.lab.PTCAnalysis.unitTest`.')

md = '\n'.join(L)
(D / 'report.md').write_text(md)

# ---- HTML (marked.js) with the report stylesheet; images referenced relatively
css = open('/home/sasha/claude/DESY_PTCint_runs_31_32_report.html').read().split('<style>')[1].split('</style>')[0]
md_src = md.replace('</script', '<\\/script')
page = f'''<title>DESY PTC Reproduction</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans+Condensed:wght@500;600&family=IBM+Plex+Sans:ital,wght@0,400;0,600;1,400&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>{css}
h1{{font-size:1.8rem;padding-bottom:.8rem;border-bottom:2px solid var(--ink);}}
main{{max-width:96ch;}} p{{max-width:88ch;}} li{{max-width:88ch;}}
table{{font-size:.78rem;}} img{{max-width:100%;height:auto;display:block;margin:.6rem 0;}}
p>em{{color:var(--muted);font-size:.85rem;}}
@media print{{ table,.tw{{break-inside:auto;page-break-inside:auto;}} img{{max-height:170mm;break-inside:avoid;}} }}
</style>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/12.0.2/marked.min.js"></script>
<main id="out"></main>
<script type="text/markdown" id="src">{md_src}</script>
<script>
document.getElementById('out').innerHTML = marked.parse(document.getElementById('src').textContent, {{gfm:true}});
document.querySelectorAll('table').forEach(t => {{ const d=document.createElement('div'); d.className='tw'; t.replaceWith(d); d.appendChild(t); }});
</script>'''
(D / 'report.html').write_text(page)

# ---- PDF via headless chromium from a home-dir staging copy (snap chromium cannot see /tmp)
stage = D / '.print'
stage.mkdir(exist_ok=True)
full = '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">' + page + '</html>'
(stage / 'report.html').write_text(full)
for png in D.glob('*.png'):
    shutil.copy(png, stage / png.name)
pdf = D / 'DESY_PTC_reproduction_report.pdf'
subprocess.run(['chromium', '--headless=new', '--no-sandbox', '--disable-gpu', '--virtual-time-budget=20000',
                '--no-pdf-header-footer', f'--print-to-pdf={pdf}', f'file://{stage}/report.html'],
               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=300)
shutil.rmtree(stage)
print('written', D / 'report.md', D / 'report.html', pdf, pdf.stat().st_size if pdf.exists() else 'NO PDF')
