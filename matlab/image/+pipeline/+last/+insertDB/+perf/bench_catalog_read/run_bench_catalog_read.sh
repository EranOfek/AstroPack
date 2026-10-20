#!/usr/bin/env bash
# run_bench_catalog_read.sh -- driver for the MATLAB + Python catalog-read benchmarks.
#
# usage: run_bench_catalog_read.sh --visit VISIT_DIR [--states warm,evict,cold] [--cold-pool PROC_DIR ...]
#                                  [--reps N] [--lang matlab|python|both] [--csv-prefix PATH]
# Env  : MATLAB_BIN, ASTROPACK_ROOT (default: derived from this file's location), PYTHON_BIN (python3)
set -euo pipefail

BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ASTROPACK_ROOT="${ASTROPACK_ROOT:-$(cd "${BENCH_DIR}/../../../../../.." && pwd)}"
MATLAB_BIN="${MATLAB_BIN:-/usr/local/MATLAB/R2025b/bin/matlab}"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export ASTROPACK_PATH="${ASTROPACK_PATH:-${ASTROPACK_ROOT}}"

VISIT=""; STATES="warm,evict"; REPS=3; LANG_SEL="both"; POOLS=()
PREFIX="${BENCH_DIR}/bench_$(date +%Y%m%d_%H%M%S)"
PY_METHODS="raw,numpy,fitsio,astropy"; PY_DRIVERS="serial,threads,prefetch"; PY_THREADS="16,64"
ML_CONFIGS="raw,astrocatalog,mexloop,matlab,matlab_proc:16,batch:1,batch:4,batch:16,batch:64,batch_nopf:16"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --visit) VISIT="${2:?}"; shift 2 ;;
    --states) STATES="${2:?}"; shift 2 ;;
    --reps) REPS="${2:?}"; shift 2 ;;
    --lang) LANG_SEL="${2:?}"; shift 2 ;;
    --cold-pool) POOLS+=("${2:?}"); shift 2 ;;
    --csv-prefix) PREFIX="${2:?}"; shift 2 ;;
    --ml-configs) ML_CONFIGS="${2:?}"; shift 2 ;;
    --py-methods) PY_METHODS="${2:?}"; shift 2 ;;
    -h|--help) sed -n '2,6p' "$0"; exit 0 ;;
    *) echo "unknown arg $1" >&2; exit 2 ;;
  esac
done
[[ -d "${VISIT}" ]] || { echo "--visit VISIT_DIR required (a proc/<time>v0 folder)" >&2; exit 2; }
[[ "${STATES}" != *cold* || ${#POOLS[@]} -gt 0 ]] || { echo "state cold needs --cold-pool" >&2; exit 2; }

run_python() {
  local pools=(); [[ ${#POOLS[@]} -gt 0 ]] && pools=(--cold-pool "${POOLS[@]}")
  "${PYTHON_BIN}" -I "${BENCH_DIR}/bench_catalog_read.py" --visit "${VISIT}" --states "${STATES}" --reps "${REPS}" \
    --methods "${PY_METHODS}" --drivers "${PY_DRIVERS}" --threads "${PY_THREADS}" "${pools[@]}" --csv "${PREFIX}_py.csv"
}

run_matlab() {
  # build the mex once if missing / stale
  local cfgs states pools=""
  cfgs="{'${ML_CONFIGS//,/\',\'}'}"
  states="{'${STATES//,/\',\'}'}"
  if [[ ${#POOLS[@]} -gt 0 ]]; then pools=",'ColdPool',{'$(IFS="','"; echo "${POOLS[*]}")'}"; pools="${pools//\' \'/\',\'}"; fi
  local build="if ~isfile(fullfile(B,['read_catalog_batch.' mexext])), cd(B); mex('-R2018a','CXXFLAGS=\$CXXFLAGS -O3 -std=c++17 -pthread','LDFLAGS=\$LDFLAGS -pthread','read_catalog_batch.cpp'); end;"
  local cmd="B='${BENCH_DIR}'; addpath(B); ${build} bench_catalog_read('Visit','${VISIT}','States',${states},'Reps',${REPS},'Configs',${cfgs}${pools},'Csv','${PREFIX}_ml.csv');"
  ( cd "${ASTROPACK_ROOT}" && "${MATLAB_BIN}" -nodisplay -nosplash -sd matlab/startup -batch "${cmd}" )
}

case "${LANG_SEL}" in
  python) run_python ;;
  matlab) run_matlab ;;
  both) run_matlab; run_python ;;
  *) echo "bad --lang" >&2; exit 2 ;;
esac
echo "summary: python3 ${BENCH_DIR}/summarize_bench.py ${PREFIX}_*.csv"
