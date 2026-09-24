# DESY wafer-test (PTCint) analysis scripts

Drivers and report builders used for the lot TH02954 flavour-test analysis with
`ultrasat.lab.PTCAnalysis` / `ultrasat.lab.writeFITS` (September 2026). They are
plain scripts with hard-coded paths (data under `/bigdata3/projects/ultrasat/DESY`,
outputs under `~/claude/desy_*_report`, FITS under `/Data1/DESY_FITS`); edit the
`Root` / `OutDir` / `D` lines to run them elsewhere. This folder is not a MATLAB
package: run the `.m` files with `run('<path>/<script>.m')`.

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

Reports: runs 31/32 reproduction (deck UC-3400-TN175-05) and the TX-scan report;
the analysis conventions (TIFF = counter columns + low-gain + high-gain halves,
DESY orientation `rot90(Half.',2)`, region `CCDSEC [1361 1460 1861 1960]`, deck
intensity = config x 1000) are documented in `ultrasat.lab.readPTC` and
`ultrasat.lab.PTCAnalysis`.
