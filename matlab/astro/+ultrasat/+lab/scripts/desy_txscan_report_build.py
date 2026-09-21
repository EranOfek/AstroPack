#!/usr/bin/env python3
"""Build the TX-scan / settings report (Markdown + HTML + PDF) from desy_txscan_report/results.json."""
import json, html, subprocess, shutil, math
from pathlib import Path
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

D = Path('/home/sasha/claude/desy_txscan_report')
R = json.load(open(D / 'results.json'))
# entries re-processed after the auto-rule fix (runs 33, 34, 36-2) replace the originals
for Pf in ('results_patch.json', 'results_patch31.json'):
    if (D / Pf).is_file():
        Patch = json.load(open(D / Pf))['Region']
        Keys = {(S['Run'], S['Die']) for S in Patch}
        R['Region'] = [S for S in R['Region'] if (S['Run'], S['Die']) not in Keys] + Patch

def denull(o):
    if isinstance(o, dict):
        return {k: denull(v) for k, v in o.items()}
    if isinstance(o, list):
        return None if len(o) == 0 else [denull(v) for v in o]
    return o
R = denull(R)
REG = R['Region'] or []
ZERO = R['Zero'] or []

def f(x, n=3):
    if x is None or isinstance(x, (list, tuple)) or (isinstance(x, float) and x != x):
        return '—'
    if isinstance(x, str):
        return x
    if abs(x) >= 1e4:
        return f'{x:.4g}'
    return f'{x:.{n}f}' if n else f'{x:.0f}'

def pm(a, b, n=1):
    return f'{f(a, n)} ± {f(b, n)}'

RUNS = [('31','AV',3.3,3.0), ('32','aSpect',3.3,3.0), ('33','AV',3.0,2.7), ('34','aSpect',3.0,2.7), ('36','aSpect',3.3,3.0),
        ('36-2','aSpect',3.9,2.7), ('36-3','aSpect',3.9,3.0), ('38','aSpect',3.5,3.0), ('38-2','aSpect',3.7,3.0), ('40','aSpect',3.3,3.0)]
RUNORDER = [r[0] for r in RUNS]
def runkey(s):
    return RUNORDER.index(s['Run']) if s['Run'] in RUNORDER else 99
REG.sort(key=lambda s: (runkey(s), s['Die']))
DIES = sorted({s['Die'] for s in REG})
def get(run, die):
    for s in REG:
        if s['Run'] == run and s['Die'] == die:
            return s
    return None

# ---------------- figures: TX scan at aSpect settings, RST_H 3.0 ------------------
scan_runs = [r for r in RUNS if r[1] == 'aSpect' and r[3] == 3.0]      # 32, 36, 36-3, 38, 38-2, 40
colors = {'W04_D03':'#1f77b4','W04_D05':'#2ca02c','W04_D06':'#9467bd','W04_D07':'#17becf','W08_D02':'#d62728','W08_D03':'#ff7f0e','W08_D04':'#8c564b','W04_D04':'#7f7f7f','W08_D01':'#bcbd22','W08_D05':'#e377c2'}
def scan_plot(quant, ylabel, fname, err=None, log=False):
    fig, ax = plt.subplots(figsize=(8, 5))
    for die in DIES:
        xs, ys, es = [], [], []
        for run, _, tx, _ in scan_runs:
            s = get(run, die)
            if s is None:
                continue
            v = quant(s)
            if v is None:
                continue
            xs.append(tx + (0.004 if run in ('36','40') else -0.004 if run == '32' else 0)); ys.append(v); es.append(err(s) if err else 0)
        if xs:
            Order = sorted(range(len(xs)), key=lambda i: xs[i])
            xs, ys, es = [xs[i] for i in Order], [ys[i] for i in Order], [es[i] for i in Order]
            ls = '-' if die.startswith('W04') else '--'
            ax.errorbar(xs, ys, yerr=es if err else None, fmt='o' + ls, color=colors.get(die, 'k'), label=f'{die} (Fl.{6 if die.startswith("W04") else 2})', capsize=2, markersize=4)
    ax.set_xlabel('VDD_TX [V]  (aSpect biases, RST_H 3.0 V)'); ax.set_ylabel(ylabel)
    if log: ax.set_yscale('log')
    ax.grid(True, alpha=.4); ax.legend(fontsize=7, ncol=2)
    fig.tight_layout(); fig.savefig(D / fname, dpi=100); plt.close(fig)

scan_plot(lambda s: s['DarkFit']['MedianSlope'], 'dark current [ADU/s]', 'scan_dark_slope.png', err=lambda s: s['DarkFit']['StdSlope'])
scan_plot(lambda s: s['MedianDarkE'], 'threshold, dark method [e⁻] (positive = lost)', 'scan_thr_dark.png', err=lambda s: s['StdDarkE'])
scan_plot(lambda s: s['MedianLightE'], 'threshold, light method [e⁻]', 'scan_thr_light.png', err=lambda s: s['StdLightE'])
scan_plot(lambda s: s['BrightFit']['MedianSlope'], 'bright response [ADU/int]', 'scan_bright_slope.png', err=lambda s: s['BrightFit']['StdSlope'])
scan_plot(lambda s: s['GainTemporal'], 'PTC gain, temporal [ADU/e⁻]', 'scan_gain.png')
scan_plot(lambda s: s['ReadNoiseTemporalRMS'], 'read noise, rms of per-pixel std [ADU]', 'scan_rn.png')
scan_plot(lambda s: s['BiasLevel'], 'bias level [ADU]', 'scan_bias.png')
# dark ladders of W04_D07 for every run
fig, ax = plt.subplots(figsize=(8, 5))
for run, sett, tx, rsth in RUNS:
    s = get(run, 'W04_D07')
    if s:
        ax.plot(s['DarkLadder']['X'], s['DarkLadder']['Median'], 'o-', ms=3, label=f'run {run}: {sett}, TX {tx}, RST_H {rsth}')
ax.set_xlabel('exposure time [s]'); ax.set_ylabel('median dark signal − zero [ADU]'); ax.set_yscale('symlog', linthresh=10); ax.grid(True, alpha=.4); ax.legend(fontsize=7)
ax.set_title('W04_D07 (Fl.6): dark ladders of all runs')
fig.tight_layout(); fig.savefig(D / 'ladders_W04_D07.png', dpi=100); plt.close(fig)
fig, ax = plt.subplots(figsize=(8, 5))
for run, sett, tx, rsth in RUNS:
    s = get(run, 'W04_D07')
    if s:
        c = s['PTCcurve']; ax.plot(c['Mean'], c['VarTemporal'], '.-', ms=3, label=f'run {run}: {sett}, TX {tx}, RST_H {rsth}')
ax.plot([0, 3000], [0, 3000], '-', color='orange', label='gain = 1')
ax.set_xlim(0, 3000); ax.set_ylim(0, 3600); ax.set_xlabel('mean signal [ADU]'); ax.set_ylabel('temporal variance [ADU²]'); ax.grid(True, alpha=.4); ax.legend(fontsize=7)
ax.set_title('W04_D07 (Fl.6): low-signal PTC of all runs')
fig.tight_layout(); fig.savefig(D / 'ptc_W04_D07.png', dpi=100); plt.close(fig)

# ---------------- markdown ------------------
L = []; w = L.append
w('# Lot TH02954: bias-setting and transfer-gate (TX) scan runs 33–40')
w('')
w('Same reduction as the runs 31/32 report (`ultrasat.lab.PTCAnalysis`, DESY 100 × 100 region, high gain, raw-column parity), applied to every full-measurement PTCint run of the flavour-test dies after the two flavour-settings runs, with a uniform automatic fit-step rule. Runs 31 and 32 are re-processed with the same rule for reference.')
w('')
w('## Summary')
w('')
w('1. **Scope.** Same reduction as the runs 31/32 report (`ultrasat.lab.PTCAnalysis`, high gain, DESY 100 × 100 region, odd/even column split), applied to all PTCint runs 31–40 with one automatic fit-step rule; the ZE-only runs 39 / 39-2 add bias and read-noise points at TX 3.6 / 3.8 V.')
w('2. **What was varied.** Two bias sets (AV vs aSpect), the reset-high voltage RST_H (3.0 vs 2.7 V) and the transfer-gate voltage VDD_TX (3.0–3.9 V); run 40 adds three new dies.')
w('3. **Repeatability.** Identical settings six days apart (runs 32 vs 36): dark current +10 %, bright response −2 %, light-method threshold ≤ 1 e⁻ (2.3 e⁻ on one die), dark-method threshold 0.3–2 e⁻ (4.8 e⁻ on W04_D03); bias and read noise identical.')
w('4. **RST_H 2.7 V.** The bias jumps from 398 to 830–1055 ADU with a ≈ 100 ADU fixed pattern; read noise 3–4 ADU rms (15 ADU combined with TX 3.9 V). The conversion gain rises 1.08 → 1.21 ADU/e⁻; in electrons the dark current, bright response and thresholds are unchanged.')
w('5. **TX scan at RST_H 3.0 V.** Nothing changes up to 3.5 V. From 3.6 V the zero level collapses (396 → 370 → 260 → 172 ADU at 3.6 / 3.7 / 3.8 / 3.9 V), the read noise grows 2.7 → 6.9 ADU rms and the bias pattern to 20–30 ADU. The dark current rises ≈ 25 % only at 3.9 V; the dark-method threshold falls from ≈ 20 to ≈ 10 e⁻.')
w('6. **Bright response.** In ADU it drops 6–13 % with TX, but in electrons it is flat within ±7 % — the drop is a conversion-gain change. The bright intercept turns positive above 3.5 V (light-method threshold ≈ 0, then negative): charge is already present at zero exposure.')
w('7. **Dies.** The same ranking in every run: W08_D02 has the largest dark current and thresholds, W04_D07 the smallest; W04_D06, W08_D03 and the new W08_D05 show no dark current in any run.')
w('8. **Odd / even columns.** At the reference settings the small parity offsets of the runs 31/32 report persist (even columns: more negative dark intercept, higher gain). Off the reference settings parity becomes the dominant structure — bias split by 13–35 ADU (RST_H 2.7 V) or 27–55 ADU (TX ≥ 3.7 V), gain split by ≈ 10 % at TX 3.5 / 3.7 V.')
w('9. **Method note.** The first version of the automatic fit-step rule fitted through the saturation knee for the RST_H 2.7 V runs; it was fixed (nearest-step top-up, §2) and those runs re-processed before the numbers above were drawn.')
w('')
w('## 1. Runs and their setup differences')
w('')
w('All runs use the same measurement design (5 ZE, 9 × 3 dark 15–600 s, 34 × 3 bright at 15 s), chuck −50 °C, VDDA/VDDD 3.3 V, VDD_SEN 3.8 V, ADC_ADJ 6, COL_ADJ 31 (ADC_CAP re-searched per die). Only the bias set and the die set change (`PTC_Config.xlsx`, confirmed by the `Result.txt` readbacks):')
w('')
w('| Run | Date | Settings (SF / RST_L / RST_SEL) | VDD_TX | VDD_RST_H | Dies |')
w('|---|---|---|---|---|---|')
dates = {'31':'08-25','32':'08-27','33':'08-28','34':'08-30','36':'09-02','36-2':'09-03','36-3':'09-05','38':'09-11','38-2':'09-13','40':'09-15'}
for run, sett, tx, rsth in RUNS:
    dies = sorted({s['Die'] for s in REG if s['Run'] == run})
    sv = '3.3 / 1.0 / 3.8' if sett == 'AV' else '3.0 / 1.2 / 3.5'
    w(f'| {run} | 2026-{dates[run]} | {sett} ({sv}) | **{tx}** | **{rsth}** | {len(dies)}: {", ".join(dies)} |')
w('| 39 / 39-2 | 09-14 | aSpect | 3.6 / 3.8 | 3.0 | 7 (zero-exposure frames only, 10 per die) |')
w('| 35 | 08-31 | aSpect | 3.3 | 3.0 | other Fl.6 dies (W03_D01, W07_D06, W12_D01–03); not analysed here |')
w('')
w('After the four flavour-settings runs the campaign is a **transfer-gate voltage scan at the aSpect biases**: TX = 3.3 (32, 36, 40), 3.5 (38), 3.7 (38-2), 3.9 (36-3), with 36-2 lowering RST_H to 2.7 V at TX 3.9, and the ZE-only runs 39/39-2 at TX 3.6/3.8. Runs 33/34 repeat the AV/aSpect pair with TX 3.0 V and RST_H 2.7 V. Run 40 adds three new FT-passing dies (W04_D04, W08_D01, W08_D05) at the reference aSpect settings.')
w('')
w('## 2. Fit-step rule')
w('')
w('Per die and ladder: use the unsaturated steps whose median signal lies in 1000–2500 ADU; if the window is empty (dark ladders of the aSpect-type runs, which stay below 260 ADU), use the steps whose median is ≥ 15 % of the highest step median; in either case fewer than three steps are topped up with the steps nearest to the window. This gives 3/9 dark points for AV-type runs (the deck used 4) and 5/9 for aSpect-type runs (the deck used 2); bright ladders get the three steps 5–7, or 4–6 where step 7 lies just above 2500 ADU (runs 33, 34, 36-2 on the W04 dies, whose bright response is 10–15 % higher). The first version of the rule replaced a partially filled window by the ≥ 15 % set, which for those bright ladders fitted one line through the saturation knee (slope ≈ 3000 instead of ≈ 14 000); for the AV-type dark ladders it likewise added the two highest, slightly non-linear steps (steps 4–9 instead of 5–7); runs 31, 33, 34 and 36-2 (28 die-runs) were re-processed with the corrected rule. The steps used are listed in the tables.')
w('')
w('## 3. Results per run and die (DESY region, medians ± std over 10 000 pixels)')
w('')
w('| Run | Set. | TX | RST_H | Die | Fl. | FT | Dark steps | Dark slope [ADU/s] | Dark intercept [ADU] | Bright slope [ADU/int] | Bright int. [ADU] | Gain T/D [ADU/e⁻] | Thr. dark [e⁻] | Thr. light [e⁻] | Bias | RN rms |')
w('|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|')
for s in REG:
    d, b = s['DarkFit'], s['BrightFit']
    ft = 'pass' if s['Pass'] else f'bin {s["SoftBin"]}'
    steps = d['FitSteps']; steps = f'{steps[0]}–{steps[-1]}' if isinstance(steps, list) else str(steps)
    w(f'| {s["Run"]} | {s["Settings"]} | {s["TX"]} | {s["RSTH"]} | {s["Die"]} | {s["Flavour"]} | {ft} | {steps} | {pm(d["MedianSlope"], d["StdSlope"], 3)} | {pm(d["MedianIntercept"], d["StdIntercept"], 1)} | {pm(b["MedianSlope"], b["StdSlope"], 0)} | {pm(b["MedianIntercept"], b["StdIntercept"], 1)} | {f(s["GainTemporal"],3)} / {f(s["GainDiff"],3)} | {f(s["MedianDarkE"],0)} ± {f(s["StdDarkE"],0)} | {f(s["MedianLightE"],0)} ± {f(s["StdLightE"],0)} | {f(s["BiasLevel"],1)} | {f(s["ReadNoiseTemporalRMS"],2)} |')
w('')
w('## 4. TX scan at the aSpect biases (RST_H 3.0 V)')
w('')
for fn, cap in [('scan_dark_slope.png', 'Dark current vs TX. Bars: pixel-to-pixel std.'),
                ('scan_thr_dark.png', 'Threshold (dark method, −intercept / gain) vs TX.'),
                ('scan_thr_light.png', 'Threshold (light method) vs TX.'),
                ('scan_bright_slope.png', 'Bright response (ADU per intensity unit) vs TX.'),
                ('scan_gain.png', 'Temporal PTC gain vs TX.'),
                ('scan_rn.png', 'Read noise (rms of per-pixel std over the 5 ZE frames) vs TX.'),
                ('scan_bias.png', 'Bias level vs TX.'),
                ('ladders_W04_D07.png', 'W04_D07: median dark ladders of all runs (symlog scale).'),
                ('ptc_W04_D07.png', 'W04_D07: low-signal photon transfer curves of all runs.')]:
    w(f'![{fn}]({fn})'); w(''); w(f'*{cap}*'); w('')

w('## 5. Bias level and read noise, all runs including the ZE-only runs 39 / 39-2')
w('')
w('| Run | Set. | TX | RST_H | Die | N ZE | Bias | Bias std | RN median / rms | RN frame-diff | Bias even / odd | RN rms even / odd |')
w('|---|---|---|---|---|---|---|---|---|---|---|---|')
ZALL = sorted(ZERO, key=lambda z: (RUNORDER.index(z['Run']) if z['Run'] in RUNORDER else 50 + (z['Run'] == '39-2'), z['Die']))
for z in ZALL:
    w(f'| {z["Run"]} | {z["Settings"]} | {z["TX"]} | {z["RSTH"]} | {z["Die"]} | {z["Nframes"]} | {f(z["BiasLevel"],1)} | {f(z["BiasStd"],2)} | {f(z["ReadNoiseTemporal"],2)} / {f(z["ReadNoiseTemporalRMS"],2)} | {f(z["ReadNoiseDiff"],2)} | {f(z["Even"]["BiasLevel"],1)} / {f(z["Odd"]["BiasLevel"],1)} | {f(z["Even"]["ReadNoiseTemporalRMS"],2)} / {f(z["Odd"]["ReadNoiseTemporalRMS"],2)} |')
w('')
w('## 6. Odd / even raw columns per run (DESY region)')
w('')
w('| Run | Die | Dark slope even / odd | Δ rel. | Dark intercept even / odd [ADU] | Δ/SE | Gain (temporal) even / odd | Bias even / odd |')
w('|---|---|---|---|---|---|---|---|')
for s in REG:
    pt = {r['Quantity']: r for r in s['ParityTable']}
    ds, di, g, bz = pt['DarkSlope'], pt['DarkIntercept'], pt['Gain_temporal'], pt.get('BiasLevel')
    rel = '—' if ds['RelDiff'] is None else f'{100*ds["RelDiff"]:.2f} %'
    w(f'| {s["Run"]} | {s["Die"]} | {f(ds["Even"],3)} / {f(ds["Odd"],3)} | {rel} | {f(di["Even"],1)} / {f(di["Odd"],1)} | {f(di["DiffOverSE"],1)} | {f(g["Even"],3)} / {f(g["Odd"],3)} | {f(bz["Even"],1) if bz else "—"} / {f(bz["Odd"],1) if bz else "—"} |')
w('')
w('## 7. Figures for the reference dies')
w('')
for die in ('W04_D07', 'W08_D02'):
    for run, sett, tx, rsth in RUNS:
        tag = f'run{run}_{die}'
        if (D / f'{tag}_resp_D.png').exists():
            w(f'### {die}, run {run} ({sett}, TX {tx}, RST_H {rsth})'); w('')
            w(f'![{tag} dark]({tag}_resp_D.png)'); w('')
            w(f'![{tag} ptc]({tag}_ptc_zoom.png)'); w('')
w('## 8. Notes')
w('')
w('**Findings** (medians over the five dies with dark current — W04_D03, W04_D05, W04_D07, W08_D02, W08_D04 — unless stated; the two no-dark-current dies W04_D06 / W08_D03 and the run-40 dies are listed in the tables only).')
w('')
w('- **Repeatability.** Runs 32 and 36 (identical aSpect settings, TX 3.3 / RST_H 3.0, six days apart) differ by +10 % in dark current (0.302 → 0.335 ADU/s), −2 % in bright response (12 970 → 12 730 ADU per intensity unit); the light-method threshold repeats to ≤ 1 e⁻ on four of the five dies (2.3 e⁻ on W04_D05), the dark-method threshold — an extrapolation of the 0.3 ADU/s aSpect dark ladder to t = 0 — to 0.3–2 e⁻ (4.8 e⁻ on W04_D03); bias and read noise are identical. Differences smaller than this between runs are not significant.')
w('- **RST_H 2.7 V (runs 33, 34, 36-2).** Lowering the reset-high voltage changes the zero level completely: the bias rises from 398 to 830–1055 ADU (die-dependent), with a fixed pattern of ≈100 ADU rms inside the 100 × 100 region (vs 1.6 ADU at RST_H 3.0), and the read noise rises to 3.1–3.8 ADU rms (TX 3.0) and to ≈15 ADU (TX 3.9, run 36-2). The dark current and the dark thresholds are unchanged within repeatability (AV: 6.7–8.3 vs 6.1–7.6 ADU/s in run 31 — the +8 % is of the same size as the 32 → 36 drift; aSpect: 0.30–0.37 vs 0.27–0.34 ADU/s), while the bright response in ADU is 10–15 % higher (14 100–15 000 vs 12 750–13 650 ADU per intensity unit). This is entirely a change of the conversion gain: the temporal PTC gain is 1.17–1.21 ADU/e⁻ at RST_H 2.7 V against 1.08–1.09 at 3.0 V, and the bright response in electrons (slope / gain) is the same within 1–4 % (11 700–12 300 vs 11 800–12 400 e⁻ per unit); the dark current in electrons is likewise unchanged (5.7 vs 5.9 e⁻/s AV, 0.27 vs 0.27 e⁻/s aSpect). The light-method thresholds at TX 3.0 are the same as at 3.3 V (32 vs 31 e⁻ aSpect, 97 vs 92 e⁻ AV). The zero-exposure medians are stable frame to frame (±0.5 ADU), so the large read noise is per-pixel temporal noise, not drift.')
w('- **TX scan at RST_H 3.0 V (32/36 → 38 → 38-2 → 36-3, with the ZE-only runs 39 / 39-2).** Up to TX 3.5 V nothing changes: bias 397–398 ADU, read noise 1.9 ADU (median) / 2.4 ADU (rms), dark current 0.29–0.34 ADU/s, dark threshold 18–23 e⁻. From TX 3.6 V upwards the zero level collapses and the noise grows monotonically: bias 396 → 370 → 260 → 172 ADU and read-noise rms 2.7 → 4.7 → 6.1 → 6.9 ADU at TX 3.6 / 3.7 / 3.8 / 3.9 V, with the bias fixed pattern growing from 1.7 to 20–30 ADU. The dark current is flat up to 3.7 V and rises by ≈25 % at 3.9 V (0.38 ADU/s), while the dark intercept shrinks (−20 → −16 → −11 ADU), i.e. the dark-method threshold drops from ≈20 e⁻ to ≈10 e⁻ at 3.9 V. The bright response in ADU drops by 6–13 % (12 970 → 12 220 → 11 300 → 11 570 ADU per unit), but so does the temporal PTC gain (1.08–1.09 → 0.96 → 0.94 → 1.04 ADU/e⁻), and in electrons the response is flat within ±7 % with no monotone trend (11 800 → 12 700 → 12 200 → 11 100 e⁻ per unit) — the ADU decline is a conversion-gain change, not a loss of signal. The gain values at 3.5 and 3.7 V are averages over the two column parities, which differ by 10 % there (§6), so the ±7 % scatter in electrons is within the uncertainty of the gain itself. What does change is the bright intercept, which turns positive (−30 → +4 → +33 → +24 ADU): the light-method threshold goes from +31 e⁻ to ≈0 at 3.5 V and to negative values (−30 / −18 e⁻) at 3.7 / 3.9 V, i.e. the bright ladder no longer extrapolates through a positive threshold — the signature of charge already present at zero exposure, consistent with the collapsing bias level and the growing bias pattern and noise.')
w('- **Dies.** The die-to-die pattern is the same in every run: W08_D02 has the largest dark current and the largest thresholds (27 e⁻ dark, 50 e⁻ light at aSpect), W04_D07 the smallest; W04_D06 and W08_D03 show no dark current in any run (their dark fits use the ≥ 15 % fallback on a ladder of ≤ 20 ADU and their dark thresholds are fits of noise); the run-40 dies W04_D04 and W08_D01 behave like the other Fl.6 / Fl.2 dies, W08_D05 like W04_D06 / W08_D03 (no dark current).')
w('- **Odd / even columns.** At the reference settings (TX 3.3 / RST_H 3.0: runs 32, 36, 40) the pattern of the runs 31/32 report persists: even raw columns have the more negative dark intercept (by 1.4–2.5 ADU under aSpect settings) and the bias is 0.2–0.4 ADU higher on the odd columns, with gain differences of a few per cent in either direction. Outside the reference settings the parity split becomes the dominant structure: at RST_H 2.7 V the odd columns sit 13–35 ADU higher in bias (TX 3.0) and the AV-settings dark intercepts split by 5–15 ADU (2–13 ADU in run 31); at TX ≥ 3.7 V (RST_H 3.0) the even columns sit 27–55 ADU higher in bias, the dark intercepts split by 5–11 ADU at 3.7 V, and the temporal PTC gain splits by parity — even 1.00–1.04 vs odd 0.90–0.92 ADU/e⁻ at TX 3.5 V, the other way round (0.87–0.91 vs 0.93–0.97) at 3.7 V — so the whole-region gain values of §3/§4 at TX ≥ 3.5 V are averages over two populations.')
w('- The bright-ladder step selection of the first version of the auto rule (see §2) had produced slopes of ≈ 3000 for the RST_H 2.7 V runs; runs 31, 33, 34 and 36-2 (tables and the §7 figures) were re-processed with the corrected rule.')
w('- Reproduce: `P = ultrasat.lab.PTCAnalysis(Dev, \'FitSteps\',struct(\'D\',\'auto\',\'B\',\'auto\'), \'Parity\',\'rawcol\'); P.run; S = P.summary;` — driver `~/claude/desy_txscan_run.m`, builder `~/claude/desy_txscan_report_build.py`.')

md = '\n'.join(L)
(D / 'report.md').write_text(md)
css = open('/home/sasha/claude/DESY_PTCint_runs_31_32_report.html').read().split('<style>')[1].split('</style>')[0]
page = f'''<title>TH02954 TX Scan</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans+Condensed:wght@500;600&family=IBM+Plex+Sans:ital,wght@0,400;0,600;1,400&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>{css}
h1{{font-size:1.8rem;padding-bottom:.8rem;border-bottom:2px solid var(--ink);}}
main{{max-width:110ch;}} p{{max-width:90ch;}} li{{max-width:90ch;}}
table{{font-size:.72rem;}} img{{max-width:100%;height:auto;display:block;margin:.6rem 0;}}
p>em{{color:var(--muted);font-size:.85rem;}}
@media print{{ @page{{size:A4 landscape;margin:12mm;}} table,.tw{{break-inside:auto;page-break-inside:auto;}} img{{max-height:150mm;break-inside:avoid;}} }}
</style>
<script src="https://cdnjs.cloudflare.com/ajax/libs/marked/12.0.2/marked.min.js"></script>
<main id="out"></main>
<script type="text/markdown" id="src">{html.escape(md)}</script>
<script>
document.getElementById('out').innerHTML = marked.parse(document.getElementById('src').textContent, {{gfm:true}});
document.querySelectorAll('table').forEach(t => {{ const d=document.createElement('div'); d.className='tw'; t.replaceWith(d); d.appendChild(t); }});
</script>'''
(D / 'report.html').write_text(page)
stage = D / '.print'; stage.mkdir(exist_ok=True)
(stage / 'report.html').write_text('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">' + page + '</html>')
for png in D.glob('*.png'):
    shutil.copy(png, stage / png.name)
pdf = D / 'DESY_TH02954_TXscan_report.pdf'
subprocess.run(['chromium', '--headless=new', '--no-sandbox', '--disable-gpu', '--virtual-time-budget=20000', '--no-pdf-header-footer',
                f'--print-to-pdf={pdf}', f'file://{stage}/report.html'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=300)
shutil.rmtree(stage)
print('written', pdf, pdf.stat().st_size if pdf.exists() else 'NO PDF')
