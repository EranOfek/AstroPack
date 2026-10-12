#!/usr/bin/env bash
# Test driver: all four *_perf jobs with inserts into last_perf only.
# UpdateStatus=false, IgnoreStatus=true. Not for cron.
set -u

MATLAB_BIN="${MATLAB_BIN:-/usr/local/MATLAB/R2020b/bin/matlab}"
WORKDIR="${WORKDIR:-$(pwd)}"
TEST_SCOPE="${TEST_SCOPE:-visit}"
ROOT="/bigdata2/projects/last/data/"
VISIT_DIR="${VISIT_DIR:-${ROOT}LAST.01.10.04/2026/01/28/proc/194924v0}"
PROC_ONE_NIGHT="LAST.01.10.04/2026/01/28/proc/*"
PROC_ALL_MONTH="LAST.01.*.*/2026/01/*/proc/*"

COMMON_NV="'SkipInsert',false,'UpdateStatus',false,'IgnoreStatus',true,'DbName','last_perf','DbPassKey','last'"

case "${TEST_SCOPE}" in
  visit)
    SCOPE_NV="'ProcDirList',{'${VISIT_DIR}'}" ;;
  night)
    SCOPE_NV="'ProcDirTemplate','${PROC_ONE_NIGHT}'" ;;
  month)
    SCOPE_NV="'ProcDirTemplate','${PROC_ALL_MONTH}'" ;;
  *)
    echo "TEST_SCOPE must be visit|night|month (got ${TEST_SCOPE})" >&2
    exit 2 ;;
esac

run_job() {
  local Label="$1" Fn="$2" DbTable="$3" ColMap="$4"
  local BatchCmd="pipeline.last.insertDB.${Fn}('${ROOT}',${SCOPE_NV},${COMMON_NV},'DbTable','${DbTable}','ColMapTableName','${ColMap}')"
  echo "=== ${Label} → last_perf.${DbTable} start $(date -Iseconds) ==="
  if ( cd "${WORKDIR}" && "${MATLAB_BIN}" -nodisplay -nosplash -batch "${BatchCmd}" ); then
    echo "=== ${Label} done $(date -Iseconds) ==="
    return 0
  fi
  echo "=== ${Label} FAILED $(date -Iseconds) ===" >&2
  return 1
}

OverallFail=0
run_job "1/4 proc catalogs" insertArchiveCatalogs2DB_perf proc_src_perf proc_src || OverallFail=1
run_job "2/4 coadd catalogs" insertArchiveCoaddCatalogs2DB_perf visit_src_perf visit_src || OverallFail=1
run_job "3/4 asteroids" insertArchiveAsteroids2DB_perf visit_asteroids_perf visit_asteroids || OverallFail=1
run_job "4/4 visit images" insertArchiveImages2DB_perf visit_images_perf visit_images || OverallFail=1
exit "${OverallFail}"
