#!/usr/bin/env bash
# Core: proc-catalog insert into last_perf.proc_src_perf (same phases as measure overhead).
# Host env from wrapper. DbName/DbTable are hard-coded; no env override.
set -u

INSERTDB_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=insertdb_run_matlab.sh
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"

RUN_PHASE1="${RUN_PHASE1:-1}"
RUN_PHASE2="${RUN_PHASE2:-1}"

ROOT="${INSERTDB_ARCHIVE_ROOT:-/bigdata2/projects/last/data/}"
PROC_ONE_NIGHT="${PROC_ONE_NIGHT:-LAST.01.10.04/2026/01/28/proc/*}"
PROC_ALL_MONTH="${PROC_ALL_MONTH:-LAST.01.*.*/2026/01/*/proc/*}"

INSERT_ARGS="'SkipInsert',false,'UpdateStatus',false,'IgnoreStatus',true,'DbName','last_perf','DbTable','proc_src_perf','DbPassKey','last','ColMapTableName','proc_src'"

MEASURE_CATALOG_FILE_INVENTORY="${MEASURE_CATALOG_FILE_INVENTORY:-1}"
MEASURE_CATALOG_RAW_FILE_READ="${MEASURE_CATALOG_RAW_FILE_READ:-1}"
MEASURE_CATALOG_ASTRO_PERF="${MEASURE_CATALOG_ASTRO_PERF:-0}"
MEASURE_SKIP_DB_INSERT="${MEASURE_SKIP_DB_INSERT:-0}"
if [[ "${MEASURE_CATALOG_FILE_INVENTORY}" == "1" ]]; then
  INSERT_ARGS="${INSERT_ARGS},'CatalogFileInventory',true"
fi
if [[ "${MEASURE_CATALOG_RAW_FILE_READ}" == "1" ]]; then
  INSERT_ARGS="${INSERT_ARGS},'CatalogRawFileRead',true"
fi
if [[ "${MEASURE_CATALOG_ASTRO_PERF}" == "1" ]]; then
  INSERT_ARGS="${INSERT_ARGS},'CatalogAstroPerf',true"
fi
if [[ "${MEASURE_SKIP_DB_INSERT}" == "1" ]]; then
  INSERT_ARGS="${INSERT_ARGS},'SkipDbInsert',true"
fi
USE_ZIP_CATALOGS="${USE_ZIP_CATALOGS:-0}"
if [[ "${USE_ZIP_CATALOGS}" == "1" ]]; then
  INSERT_ARGS="${INSERT_ARGS},'UseZipCatalogs',true"
fi
if [[ -n "${ZIP_DIR:-}" ]]; then
  INSERT_ARGS="${INSERT_ARGS},'ZipDir','${ZIP_DIR}'"
fi
if [[ -n "${EXTRACT_ROOT:-}" ]]; then
  INSERT_ARGS="${INSERT_ARGS},'ExtractRoot','${EXTRACT_ROOT}'"
fi

EPOCH_CAT_MAT="${EPOCH_CAT_MAT:-none}"
EPOCH_CAT_MAT_OVERWRITE="${EPOCH_CAT_MAT_OVERWRITE:-0}"
case "${EPOCH_CAT_MAT}" in
  none|write|load)
    INSERT_ARGS="${INSERT_ARGS},'EpochCatMat','${EPOCH_CAT_MAT}'"
    ;;
  *)
    echo "FATAL: EPOCH_CAT_MAT must be none, write, or load (got ${EPOCH_CAT_MAT})" >&2
    exit 2
    ;;
esac
if [[ "${EPOCH_CAT_MAT_OVERWRITE}" == "1" ]]; then
  INSERT_ARGS="${INSERT_ARGS},'EpochCatMatOverWrite',true"
fi

assert_last_perf_target() {
  if [[ "${INSERT_ARGS}" != *"'DbName','last_perf'"* ]]; then
    echo "FATAL: INSERT_ARGS must use DbName last_perf" >&2
    exit 2
  fi
  if [[ "${INSERT_ARGS}" != *"'DbTable','proc_src_perf'"* ]]; then
    echo "FATAL: INSERT_ARGS must use DbTable proc_src_perf" >&2
    exit 2
  fi
  if [[ "${INSERT_ARGS}" == *"'DbName','last'"* ]]; then
    echo "FATAL: INSERT_ARGS must not target production DbName last" >&2
    exit 2
  fi
}

assert_last_perf_target

run_proc_insert() {
  local Label="$1"
  local ProcTemplate="$2"
  local BatchCmd
  BatchCmd="pipeline.last.insertDB.insertArchiveCatalogs2DB_perf('${ROOT}','ProcDirTemplate','${ProcTemplate}',${INSERT_ARGS})"

  echo "=== ${Label} start $(date -Iseconds) ==="
  echo "=== INSERTDB_RUN_HOST=${INSERTDB_RUN_HOST:-unknown} RootDir=${ROOT} ProcDirTemplate=${ProcTemplate} ==="
  echo "=== ClickHouse INSERT target: last_perf.proc_src_perf (NOT last.proc_src) ==="
  echo "=== CatalogFileInventory=${MEASURE_CATALOG_FILE_INVENTORY} CatalogRawFileRead=${MEASURE_CATALOG_RAW_FILE_READ} CatalogAstroPerf=${MEASURE_CATALOG_ASTRO_PERF} SkipDbInsert=${MEASURE_SKIP_DB_INSERT} UseZipCatalogs=${USE_ZIP_CATALOGS} ZipDir=${ZIP_DIR:-} ExtractRoot=${EXTRACT_ROOT:-} EPOCH_CAT_MAT=${EPOCH_CAT_MAT} EPOCH_CAT_MAT_OVERWRITE=${EPOCH_CAT_MAT_OVERWRITE} ==="
  echo "=== SkipInsert=false, UpdateStatus=false, IgnoreStatus=true ==="

  if insertdb_run_matlab_batch "${BatchCmd}"; then
    echo "=== ${Label} done $(date -Iseconds) ==="
    return 0
  fi
  echo "=== ${Label} FAILED $(date -Iseconds) ===" >&2
  return 1
}

OverallFail=0

if [[ "${RUN_PHASE1}" == "1" ]]; then
  run_proc_insert "phase1 one telescope one night (LAST.01.10.04 2026/01/28)" "${PROC_ONE_NIGHT}" || OverallFail=1
else
  echo "=== phase 1 skipped (RUN_PHASE1=0) ==="
fi

if [[ "${RUN_PHASE2}" == "1" ]]; then
  run_proc_insert "phase2 all telescopes month 2026/01" "${PROC_ALL_MONTH}" || OverallFail=1
else
  echo "=== phase 2 skipped (RUN_PHASE2=0) ==="
fi

exit "${OverallFail}"
