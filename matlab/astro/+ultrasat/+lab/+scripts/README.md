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
| 4-6 | — | bad columns, PTC gain, noise budget (not written yet) |

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
region-based reports measured in the DESY 100x100 window.

Reports: runs 31/32 reproduction (deck UC-3400-TN175-05) and the TX-scan report;
the analysis conventions (TIFF = counter columns + low-gain + high-gain halves,
DESY orientation `rot90(Half.',2)`, region `CCDSEC [1361 1460 1861 1960]`, deck
intensity = config x 1000) are documented in `ultrasat.lab.readPTC` and
`ultrasat.lab.PTCAnalysis`.
