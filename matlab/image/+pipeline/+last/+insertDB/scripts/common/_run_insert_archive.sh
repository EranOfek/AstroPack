#!/usr/bin/env bash
# Core: four *_perf jobs in sequence. Host env must be set by run_*_{wrx80,euclid}.sh wrapper.
set -u

INSERTDB_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=insertdb_run_matlab.sh
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"

DUMMY="${DUMMY:-1}"
ALLOW_WRITES="${ALLOW_WRITES:-0}"
SECOND_PASS="${SECOND_PASS:-0}"
DELAY_HOURS="${DELAY_HOURS:-}"

ROOT="${INSERTDB_ARCHIVE_ROOT:-/bigdata2/projects/last/data/}"
PROC_TEMPLATE="${PROC_TEMPLATE:-LAST.01.01.01/2026/01/*/proc/*}"

if [[ "${SECOND_PASS}" == "1" && -z "${DELAY_HOURS}" ]]; then
  echo "SECOND_PASS=1 requires DELAY_HOURS to be set" >&2
  exit 2
fi

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

echo "=== INSERTDB_RUN_HOST=${INSERTDB_RUN_HOST:-unknown} ROOT=${ROOT} WORKDIR=${WORKDIR} ==="

run_pass() {
  local PassNum="$1"
  local Fail=0
  local Jobs=(
    "pipeline.last.insertDB.perf.insertArchiveCatalogs2DB_perf('${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'${MATLAB_NV_ARGS:+,${MATLAB_NV_ARGS}})"
    "pipeline.last.insertDB.perf.insertArchiveCoaddCatalogs2DB_perf('${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'${MATLAB_NV_ARGS:+,${MATLAB_NV_ARGS}})"
    "pipeline.last.insertDB.perf.insertArchiveAsteroids2DB_perf('${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'${MATLAB_NV_ARGS:+,${MATLAB_NV_ARGS}})"
    "pipeline.last.insertDB.perf.insertArchiveImages2DB_perf('${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'${MATLAB_NV_ARGS:+,${MATLAB_NV_ARGS}})"
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
    if ! insertdb_run_matlab_batch "${Jobs[$I]}"; then
      echo "=== pass ${PassNum} job ${Names[$I]} FAILED $(date -Iseconds) ===" >&2
      Fail=1
    else
      echo "=== pass ${PassNum} job ${Names[$I]} done $(date -Iseconds) ==="
    fi
  done
  return "${Fail}"
}

OverallFail=0
run_pass 1 || OverallFail=1

if [[ "${SECOND_PASS}" == "1" ]]; then
  DelaySec=$((DELAY_HOURS * 3600))
  echo "=== sleeping ${DELAY_HOURS} hours (${DelaySec}s) before pass 2 $(date -Iseconds) ==="
  sleep "${DelaySec}"
  run_pass 2 || OverallFail=1
fi

exit "${OverallFail}"
