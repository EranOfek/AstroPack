#!/usr/bin/env bash
# Core: batch epoch proc Cat/Head MAT in visit dirs via writeArchiveProcCatMat (no DB).
set -u

INSERTDB_PKG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INSERTDB_COMMON_DIR="${INSERTDB_PKG_DIR}/scripts/common"
# shellcheck source=../scripts/common/insertdb_run_matlab.sh
source "${INSERTDB_COMMON_DIR}/insertdb_run_matlab.sh"

ROOT="${INSERTDB_ARCHIVE_ROOT:-/bigdata2/projects/last/data/}"
PROC_TEMPLATE="${PROC_TEMPLATE:-LAST.*/20*/*/*/proc/*}"
OVERWRITE="${OVERWRITE:-0}"
VERIFY_FIRST_N="${VERIFY_FIRST_N:-50}"
VERIFY_STRICT="${VERIFY_STRICT:-0}"
VISIT_IDLE_SEC="${VISIT_IDLE_SEC:-10}"

BUILD_ARGS="'RootDir','${ROOT}','ProcDirTemplate','${PROC_TEMPLATE}'"
BUILD_ARGS="${BUILD_ARGS},'VerifyFirstN',${VERIFY_FIRST_N}"
BUILD_ARGS="${BUILD_ARGS},'VisitIdleSec',${VISIT_IDLE_SEC}"
if [[ "${OVERWRITE}" == "1" ]]; then
  BUILD_ARGS="${BUILD_ARGS},'OverWrite',true"
fi
if [[ "${VERIFY_STRICT}" == "1" ]]; then
  BUILD_ARGS="${BUILD_ARGS},'VerifyStrict',true"
fi

BatchCmd="pipeline.last.insertDB.create_proc_mat.writeArchiveProcCatMat(${BUILD_ARGS})"

echo "=== write archive proc cat mat start $(date -Iseconds) ==="
echo "=== INSERTDB_RUN_HOST=${INSERTDB_RUN_HOST:-unknown} RootDir=${ROOT} ProcDirTemplate=${PROC_TEMPLATE} ==="
echo "=== OVERWRITE=${OVERWRITE} VERIFY_FIRST_N=${VERIFY_FIRST_N} VERIFY_STRICT=${VERIFY_STRICT} VISIT_IDLE_SEC=${VISIT_IDLE_SEC} ==="
echo "=== Logs: ~/matlab/log/last/insertArchive/insertDB_proc_mat_build*.log ==="

if insertdb_run_matlab_batch "${BatchCmd}"; then
  echo "=== write archive proc cat mat done $(date -Iseconds) ==="
  exit 0
fi
echo "=== write archive proc cat mat FAILED $(date -Iseconds) ===" >&2
exit 1
