#!/usr/bin/env bash
# Test driver: measure proc-catalog ingest overhead only (insertArchiveCatalogs2DB_perf).
# No ClickHouse insert, no .status updates, IgnoreStatus=true (process every visit in scope).
# Not for cron — use run_insert_archive.sh for production.
#
# Phase 1: one telescope, one night (known data: LAST.01.10.04 / 2026-01-28).
# Phase 2: all LAST.01.*.* telescopes, full month 2026/01.
#
# Perf log: ~/matlab/log/last/insertArchive/insertDB_perf.log
set -u

# --- Configuration ---
# MATLAB_BIN — MATLAB executable
# WORKDIR    — MATLAB cwd when the job starts
# RUN_PHASE1 — 1 (default) run single-night smoke; 0 skip
# RUN_PHASE2 — 1 (default) run full-month all telescopes; 0 skip
MATLAB_BIN="${MATLAB_BIN:-/usr/local/MATLAB/R2020b/bin/matlab}"
WORKDIR="${WORKDIR:-$(pwd)}"
RUN_PHASE1="${RUN_PHASE1:-1}"
RUN_PHASE2="${RUN_PHASE2:-1}"

ROOT="/bigdata2/projects/last/data/"

# Known visit example: .../LAST.01.10.04/2026/01/28/proc/194924v0 (~480 proc Cat FITS)
PROC_ONE_NIGHT="LAST.01.10.04/2026/01/28/proc/*"
PROC_ALL_MONTH="LAST.01.*.*/2026/01/*/proc/*"

# Always measure-only (never allow writes from this script)
MEASURE_ARGS="'SkipInsert',true,'UpdateStatus',false,'IgnoreStatus',true"

run_proc_measure() {
  local Label="$1"
  local ProcTemplate="$2"
  local BatchCmd
  BatchCmd="pipeline.last.insertDB.insertArchiveCatalogs2DB_perf('${ROOT}','ProcDirTemplate','${ProcTemplate}',${MEASURE_ARGS})"

  echo "=== ${Label} start $(date -Iseconds) ==="
  echo "=== RootDir=${ROOT} ProcDirTemplate=${ProcTemplate} ==="
  echo "=== SkipInsert UpdateStatus=false IgnoreStatus=true ==="

  if ( cd "${WORKDIR}" && "${MATLAB_BIN}" -nodisplay -nosplash -batch "${BatchCmd}" ); then
    echo "=== ${Label} done $(date -Iseconds) ==="
    return 0
  else
    echo "=== ${Label} FAILED $(date -Iseconds) ===" >&2
    return 1
  fi
}

OverallFail=0

# --- Phase 1: one telescope, one night ---
if [[ "${RUN_PHASE1}" == "1" ]]; then
  run_proc_measure "phase1 one telescope one night (LAST.01.10.04 2026/01/28)" "${PROC_ONE_NIGHT}" || OverallFail=1
else
  echo "=== phase 1 skipped (RUN_PHASE1=0) ==="
fi

# --- Phase 2: all telescopes, January 2026 ---
if [[ "${RUN_PHASE2}" == "1" ]]; then
  run_proc_measure "phase2 all telescopes month 2026/01" "${PROC_ALL_MONTH}" || OverallFail=1
else
  echo "=== phase 2 skipped (RUN_PHASE2=0) ==="
fi

exit "${OverallFail}"
