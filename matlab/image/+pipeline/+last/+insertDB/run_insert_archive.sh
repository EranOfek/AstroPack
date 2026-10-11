#!/usr/bin/env bash
# Sequential LAST archive insert (perf clones). One cron entry replaces four staggered jobs.
#
# DEFAULT: single pass (four jobs once), measure only — no ClickHouse, no .status on archive.
# Opt in: ALLOW_WRITES=1 (or DUMMY=0) for writes; SECOND_PASS=1 + DELAY_HOURS for a second pass.
set -u

# --- Configuration (override via environment) ---
# MATLAB_BIN    — MATLAB executable (default R2020b; point at R2025b to compare)
# ALLOW_WRITES=0 — default; passes SkipInsert, UpdateStatus=false, IgnoreStatus=true
# ALLOW_WRITES=1 — production-like: insert into ClickHouse and update .status (via *_perf)
# DUMMY=0        — legacy alias for ALLOW_WRITES=1 (DUMMY=1 is default measure-only)
# SECOND_PASS=0 — default: one pass only (four jobs, then exit)
# SECOND_PASS=1  — optional: sleep DELAY_HOURS, then run the same four jobs again
# WORKDIR       — MATLAB cwd (failure sidecar *_dir.txt may still append here when visits fail)
MATLAB_BIN="${MATLAB_BIN:-/usr/local/MATLAB/R2020b/bin/matlab}"
DUMMY="${DUMMY:-1}"
ALLOW_WRITES="${ALLOW_WRITES:-0}"
SECOND_PASS="${SECOND_PASS:-0}"
DELAY_HOURS="${DELAY_HOURS:-}"
WORKDIR="${WORKDIR:-$(pwd)}"

# --- Paths passed to MATLAB (edit month segment in PROC_TEMPLATE as needed) ---
ROOT="/bigdata2/projects/last/data/"
PROC_TEMPLATE="LAST.01.01.01/2026/01/*/proc/*"

# --- Validate optional second pass ---
if [[ "${SECOND_PASS}" == "1" && -z "${DELAY_HOURS}" ]]; then
  echo "SECOND_PASS=1 requires DELAY_HOURS to be set" >&2
  exit 2
fi

# --- Measure-only vs writes (default: no DB, no archive .status changes) ---
if [[ "${DUMMY}" == "0" ]]; then
  ALLOW_WRITES=1
fi

MATLAB_NV_ARGS=""
if [[ "${ALLOW_WRITES}" == "1" ]]; then
  echo "=== ALLOW_WRITES=1: ClickHouse insert and .status updates ENABLED ==="
else
  MATLAB_NV_ARGS="'SkipInsert',true,'UpdateStatus',false,'IgnoreStatus',true"
  echo "=== measure-only (default): SkipInsert, UpdateStatus=false, IgnoreStatus=true ==="
fi

if [[ "${SECOND_PASS}" == "1" ]]; then
  echo "=== two-pass mode: pass 1 now, pass 2 after ${DELAY_HOURS} hour(s) ==="
else
  echo "=== single pass (default): four jobs once, then exit ==="
fi

# --- Run four ingest jobs in order (never parallel) ---
run_pass() {
  local PassNum="$1"
  local Fail=0
  # Order: proc catalogs → coadd catalogs → asteroids → visit images
  local Jobs=(
    "pipeline.last.insertDB.insertArchiveCatalogs2DB_perf('${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'${MATLAB_NV_ARGS:+,${MATLAB_NV_ARGS}})"
    "pipeline.last.insertDB.insertArchiveCoaddCatalogs2DB_perf('${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'${MATLAB_NV_ARGS:+,${MATLAB_NV_ARGS}})"
    "pipeline.last.insertDB.insertArchiveAsteroids2DB_perf('${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'${MATLAB_NV_ARGS:+,${MATLAB_NV_ARGS}})"
    "pipeline.last.insertDB.insertArchiveImages2DB_perf('${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'${MATLAB_NV_ARGS:+,${MATLAB_NV_ARGS}})"
  )
  local Names=(
    insertArchiveCatalogs2DB_perf
    insertArchiveCoaddCatalogs2DB_perf
    insertArchiveAsteroids2DB_perf
    insertArchiveImages2DB_perf
  )
  local I
  for I in "${!Jobs[@]}"; do
    echo "=== pass ${PassNum} job $((I + 1))/4 ${Names[$I]} start $(date -Iseconds) ==="
    if ! ( cd "${WORKDIR}" && "${MATLAB_BIN}" -nodisplay -nosplash -batch "${Jobs[$I]}" ); then
      echo "=== pass ${PassNum} job ${Names[$I]} FAILED $(date -Iseconds) ===" >&2
      Fail=1
    else
      echo "=== pass ${PassNum} job ${Names[$I]} done $(date -Iseconds) ==="
    fi
  done
  return "${Fail}"
}

# --- Single pass (default): four jobs, then exit unless SECOND_PASS=1 ---
OverallFail=0
run_pass 1 || OverallFail=1

# --- Optional second pass (SECOND_PASS=1 only) ---
if [[ "${SECOND_PASS}" == "1" ]]; then
  DelaySec=$((DELAY_HOURS * 3600))
  echo "=== sleeping ${DELAY_HOURS} hours (${DelaySec}s) before pass 2 $(date -Iseconds) ==="
  sleep "${DelaySec}"
  run_pass 2 || OverallFail=1
fi

exit "${OverallFail}"
