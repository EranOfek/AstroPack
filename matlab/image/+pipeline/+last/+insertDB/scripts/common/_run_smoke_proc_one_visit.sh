#!/usr/bin/env bash
# Core: one proc visit, measure-only (SkipInsert, no .status). Host env from wrapper.
set -u

INSERTDB_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=insertdb_run_matlab.sh
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"

ROOT="${INSERTDB_ARCHIVE_ROOT:-/bigdata2/projects/last/data/}"
SMOKE_VISIT_DIR="${SMOKE_VISIT_DIR:-${ROOT}LAST.01.01.01/2026/01/06/proc/000224v0}"

MEASURE_ARGS="'SkipInsert',true,'UpdateStatus',false,'IgnoreStatus',true,'DbName','last_perf','DbTable','proc_src_perf','DbPassKey','last','ColMapTableName','proc_src'"
BatchCmd="pipeline.last.insertDB.perf.insertArchiveCatalogs2DB_perf('${ROOT}','ProcDirList',{'${SMOKE_VISIT_DIR}'},${MEASURE_ARGS})"

echo "=== smoke proc one visit start $(date -Iseconds) ==="
echo "=== INSERTDB_RUN_HOST=${INSERTDB_RUN_HOST:-unknown} visit=${SMOKE_VISIT_DIR} ==="
echo "=== SkipInsert, no .status; expect ~480 proc FITS ==="

if insertdb_run_matlab_batch "${BatchCmd}"; then
  echo "=== smoke proc one visit done $(date -Iseconds) ==="
  exit 0
fi
echo "=== smoke proc one visit FAILED $(date -Iseconds) ===" >&2
exit 1
