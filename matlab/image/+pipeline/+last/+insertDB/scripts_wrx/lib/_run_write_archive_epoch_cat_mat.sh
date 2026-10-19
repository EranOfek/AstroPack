#!/usr/bin/env bash
# Core: batch epoch proc Cat/Head MAT under {proc}/_mat via writeArchiveEpochCatMat (no DB).
set -u

INSERTDB_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=insertdb_run_matlab.sh
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"

ROOT="${INSERTDB_ARCHIVE_ROOT:-/bigdata2/projects/last/data/}"
PROC_TEMPLATE="${PROC_TEMPLATE:-LAST.*/20*/*/*/proc/*}"
OVERWRITE="${OVERWRITE:-0}"
VERIFY_FIRST_N="${VERIFY_FIRST_N:-50}"
VERIFY_STRICT="${VERIFY_STRICT:-0}"

BUILD_ARGS="'RootDir','${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'"
BUILD_ARGS="${BUILD_ARGS},'VerifyFirstN',${VERIFY_FIRST_N}"
if [[ "${OVERWRITE}" == "1" ]]; then
  BUILD_ARGS="${BUILD_ARGS},'OverWrite',true"
fi
if [[ "${VERIFY_STRICT}" == "1" ]]; then
  BUILD_ARGS="${BUILD_ARGS},'VerifyStrict',true"
fi

BatchCmd="pipeline.last.insertDB.writeArchiveEpochCatMat(${BUILD_ARGS})"

echo "=== write archive epoch cat mat start $(date -Iseconds) ==="
echo "=== INSERTDB_RUN_HOST=${INSERTDB_RUN_HOST:-unknown} RootDir=${ROOT} ProcDirTemplate=${PROC_TEMPLATE} ==="
echo "=== OVERWRITE=${OVERWRITE} VERIFY_FIRST_N=${VERIFY_FIRST_N} VERIFY_STRICT=${VERIFY_STRICT} ==="
echo "=== Logs: ~/matlab/log/last/insertArchive/insertDB_epoch_mat_build*.log ==="

if insertdb_run_matlab_batch "${BatchCmd}"; then
  echo "=== write archive epoch cat mat done $(date -Iseconds) ==="
  exit 0
fi
echo "=== write archive epoch cat mat FAILED $(date -Iseconds) ===" >&2
exit 1
