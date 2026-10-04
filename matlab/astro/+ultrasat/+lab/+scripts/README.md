# DESY wafer-test (PTCint) analysis scripts

Drivers and report builders used for the lot TH02954 flavour-test analysis with
`ultrasat.lab.PTCAnalysis` / `ultrasat.lab.writeFITS` (September 2026). They are
plain scripts with hard-coded paths (data under `/Data1/DESY`, falling back to the
`/bigdata3/projects/ultrasat/DESY` share; outputs under `~/claude/desy_*`, FITS
under `/Data1/DESY_FITS`); edit the `Root` / `OutDir` / `D` lines at the top to
run them elsewhere.

This folder is the MATLAB package **`ultrasat.lab.scripts`**, so the drivers are
reachable by name once AstroPack is on the path:

```matlab
ultrasat.lab.scripts.desy_rn_single_die          % no arguments, settings at the top of the file
ultrasat.lab.scripts.desy_perpixel_run
```

They are scripts rather than functions, so they run in the caller's workspace
and leave their variables there — convenient when something fails half way and
you want to inspect `P`. R2020b resolves package-qualified scripts; note that
`exist('ultrasat.lab.scripts.x','file')` returns 0 for them even though `which`
finds them, so test with `which`, not `exist`.

The `.py` and `.sh` files live in the same folder and are run by path as usual;
MATLAB ignores them.

| Script | Purpose |
|---|---|
| `desy_ptc_report_run.m` | Runs 31 (AV) and 32 (aSpect), 7 dies: DESY 100x100 region with the deck's fit steps (`FitSteps` D 4:7 / 8:9), raw-column parity, 400x400 parity regions for the three deck dies, streamed full die W04_D07 run 31; figures + `results.json`. |
| `desy_ptc_report_zero.m` | Bias level and read noise from the ZE frames (both regions, parity split), appended to `results.json` as `Zero`. |
| `desy_ptc_report_build.py` | Builds `report.md` / `report.html` / `DESY_PTC_reproduction_report.pdf` from `results.json` (markdown via marked.js from cdnjs, PDF via headless chromium). |
| `desy_txscan_run.m` | Runs 31–40 (settings and VDD_TX scan) with the `'auto'` fit-step rule and parity; ZE statistics for all runs incl. the ZE-only runs 39 / 39-2; figures for W04_D07 and W08_D02; `results.json`. |
| `desy_txscan_report_build.py` | Builds `DESY_TH02954_TXscan_report.pdf` (matplotlib scan plots vs TX, tables, findings). Merges `results_patch*.json` if present (die-runs re-processed separately). |
| `desy_fits_export.m` | Exports runs 31 and 32 (7 dies, both gains, DESY orientation) as FITS to `/Data1/DESY_FITS/<run>/<die>/`. |
| `desy_ptc_shape_extract.m` | Region PTC of both ladders (dark and light) with robust per-step variance statistics (mean, 1 %-clipped mean, median/ln2) and the ZE read noise, for the three deck die-runs; writes `dark_light2.json`. |
| `desy_ptc_shape_plots.py` | (Var − RN²)/(Mean·Gain) panels for Gain = 1.02…1.10, light and dark ladders (report §8.3). |
| `desy_ptc_perpixel_extract.m` | Per-pixel means and temporal variances of both ladders + per-pixel ZE noise (binary dumps + `meta.json`), same three die-runs. |
| `desy_ptc_perpixel_plots.py` | Single-pixel version of the same panels, each pixel with its own mean, variance and read noise (report §8.4); the scatter-across-pixels figure of §8.5 is built from the same dumps. |
| `desy_var_mean_plots.py` | Variance vs mean of the individual pixels, one figure per regime (light / dark) per die-run, from the same dumps: per-pixel density, per-step median/ln2 and mean, fitted line and the (Var − RN²)/(g·Mean) ratio. `--tag`, `--ladder`, `--fit-range`, `--gain`. |
| `desy_regenerate_all.sh` | Sequential chain of the above (drivers, zero stats, TX scan, FITS export) with a log. |

## Single-die stage chain

Step-by-step tools for ONE run / wafer / die / gain half, each stage writing its
own binary maps and `stats.json` so the stages are independent and rerunnable.
The whole die is processed in the **streamed** mode (`CCDSEC` empty): the
per-pixel methods read one ladder step at a time and keep only the running sums,
so 22.5 M pixels cost ~3 GB rather than the ~20 GB a cached whole-die ladder
would need, and every frame is read exactly once.

| Stage | Script | What it measures |
|---|---|---|
| 1 | `desy_rn_single_die.m` + `_plots.py` | bias, fixed pattern, per-pixel read noise and its intrinsic spread, common mode, column parity (ZE frames only) |
| 2 | `desy_die_dark.m` + `desy_die_dark_plots.py` | per-pixel dark current and dark threshold from the weighted ladder fit, their spreads with the fit noise deconvolved, DSNU per step |
| 3 | `desy_die_light.m` + `desy_die_light_plots.py` | per-pixel photo-response, PRNU from the per-step fixed pattern, light-method threshold with the dark current of stage 2 and both fit variances propagated |
| 4 | `desy_die_badcol.m` + `desy_die_badcol_plots.py` | bad readout columns from the stage 1-3 maps (no frames read): excess read noise, dead response, the mask stages 5-6 use |
| 5 | `desy_die_ptc.m` + `desy_die_ptc_plots.py` | per-pixel PTC gain, the gain per readout column and per block, and what the shot noise says about the charge threshold |
| 6 | `desy_die_budget.m` + `desy_die_budget_plots.py` | sigma_eff and SNR in electrons, the limiting signal, and which noise term dominates where -- all three charge thresholds carried side by side |
| 7 | `desy_die_varspread.m` + `desy_die_varspread_plots.py` | the distribution of the per-pixel variance at every ladder step against a simulated identical-pixel null: how much the variance really differs between pixels, and the tail |

`desy_die_config.m` holds the run / die / gain / fit-step settings; every stage
runs it first, so it is the only file to edit when moving to another dataset.
Note that the streamed mode needs an explicit step list (`FitSteps`): the
`'auto'` rule resolves the steps from the cached region ladder, which full mode
does not build. The defaults in the config are what `'auto'` picks for run 32
from the published window: dark steps 5-9 (there is none inside 1000-2500 ADU,
so the rule falls back to the steps above 15 % of the top), bright steps 5-7
(1173, 1842, 2374 ADU).

Every stage reports two spreads, and they are not interchangeable. The spread
over the **whole die** is a total non-uniformity, dominated on this device by
large-scale structure -- a 2:1 dark-current ramp along the readout columns, and
illumination/response patches. The **local** spread (residual to a 32x32 block
median, fit noise removed) is the pixel-to-pixel term a noise budget needs: on
run 32 W04_D07 it is 5.64 % against 20.0 % for the dark current, and 0.41 %
against 1.14 % for the response, the local values agreeing with what the
region-based reports measured in the DESY 100x100 window. The spreads are
computed by `ultrasat.lab.PTCAnalysis.localSpread`.

`DieThreshold` records which charge threshold the budget takes, and the choice
is evidence-based: on run 32 W04_D07 the dark-method threshold moves from 8.9
to 25.0 ADU as the fit window is raised from all 9 steps to the top 3, because
the dark ladder is bent at the bottom (residuals +11.6, +9.9, +7.2, +3.4 ADU
at steps 1-4) and this run's whole dark ladder sits inside that knee, whereas
the light-method intercept moves only 25.5 to 29.5 ADU over every window
inside the linear range of the bright ladder. Hence `'light'`.

Reports: runs 31/32 reproduction (deck UC-3400-TN175-05) and the TX-scan report;
the analysis conventions (TIFF = counter columns + low-gain + high-gain halves,
DESY orientation `rot90(Half.',2)`, region `CCDSEC [1361 1460 1861 1960]`, deck
intensity = config x 1000) are documented in `ultrasat.lab.readPTC` and
`ultrasat.lab.PTCAnalysis`.

Stage 5 does not use the same statistics as stages 2-4, and the difference is
not cosmetic. Its ladder points are per-pixel VARIANCES from 3 frames, so each
carries 2 degrees of freedom and is chi2 distributed with a 100 % error and a
long tail, not a Gaussian mean. The fitted slope is then unbiased in the mean
but its median is 10 % low, the Gaussian deconvolution of `paramSpread` does
not apply (it pairs a robust observed spread with an analytic rms and returns
a meaningless zero), and one pixel's gain is good only to ~70 %. The stage
therefore quotes the MEAN as the gain, compares the observed spread with a
null simulation in which every pixel has exactly the same gain, and measures
the gain where it is measurable -- per readout column (4742 pixels, ~1 %) and
per 32x32 block (~2 %). Every one of those effects was first seen as an
anomaly in the output and then reproduced to three digits by the null.

`desy_die_report_build.py` builds the report of one die from the six json
dumps alone (`--indir` the stage output directory, `--rndir` the stage 1 one):
a one-page datasheet, a section per stage with the figures that carry each
conclusion, the three places the first answer was wrong, the threshold
question, what the chain does not determine, and how to rerun it. The PDF is
rendered from `report.html` with headless chromium, as for the other reports.

Stage 6 reads no frames either, and it deliberately refuses to choose a charge
threshold. The three routes give 7.7 e- (shot noise, stage 5), 17.1 e- (dark
response, stage 2) and 29.0 e- (light response, stage 3), and on run 32
W04_D07 that alone moves the limiting signal at SNR 5 from 38 to 60 e-, against
0.4 e- for the gain's window systematic and 0.2 e- for the bad-column mask. All
the fixed-pattern terms it uses are the LOCAL (block-detrended) spreads, since
a budget asks what varies between neighbouring pixels, not across the die.

Stage 7 needs three things that are easy to get wrong. The null is **simulated
with integer frames**, because a variance from three integers can only take
multiples of 1/18 and both distributions are combs -- a continuous chi2 null
would differ from the data for a reason that has nothing to do with the
pixels, and the figure shows the simulated comb falling on the measured one.
The width is **trimmed** (top 0.1 %, the same rule applied to the null),
because a cosmic ray in one of three frames puts a pixel's variance at 10^7
ADU^2 and on the long darks the untrimmed sd of V is 2700 times the chi2
expectation, almost all of it from 0.001 % of the pixels; a median-based width
will not do either, since the MAD of a quantised variance ties exactly with
the null. And the significance counts **the null's own simulation error**,
which dominates: the data has 22.5 M pixels against the null's 10^6, so above
a few hundred ADU the upper limits are what mean something.

On run 32 W04_D07 the pixel-to-pixel spread of the true variance falls from
120 % at zero signal (where the variance is read noise, which varies a great
deal) through 27 % at the top of the dark ladder (where it is dark signal, and
the dark current itself spreads 20 % over the die) to below 6 % everywhere on
the bright ladder, where shot noise dominates and the only expected spread is
the 0.4 % PRNU.
