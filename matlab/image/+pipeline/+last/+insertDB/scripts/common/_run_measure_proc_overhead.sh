#!/usr/bin/env bash
# Core: proc-catalog overhead (SkipInsert). Host env from wrapper.
set -u

INSERTDB_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=insertdb_run_matlab.sh
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"

RUN_PHASE1="${RUN_PHASE1:-1}"
RUN_PHASE2="${RUN_PHASE2:-1}"

ROOT="${INSERTDB_ARCHIVE_ROOT:-/bigdata2/projects/last/data/}"
PROC_ONE_NIGHT="${PROC_ONE_NIGHT:-LAST.01.10.04/2026/01/28/proc/*}"
PROC_ALL_MONTH="${PROC_ALL_MONTH:-LAST.01.*.*/2026/01/*/proc/*}"

MEASURE_CATALOG_FILE_INVENTORY="${MEASURE_CATALOG_FILE_INVENTORY:-0}"
MEASURE_CATALOG_RAW_FILE_READ="${MEASURE_CATALOG_RAW_FILE_READ:-0}"
MEASURE_CATALOG_ASTRO_PERF="${MEASURE_CATALOG_ASTRO_PERF:-1}"

MEASURE_ARGS="'SkipInsert',true,'UpdateStatus',false,'IgnoreStatus',true,'DbName','last_perf','DbTable','proc_src_perf','DbPassKey','last','ColMapTableName','proc_src'"
if [[ "${MEASURE_CATALOG_FILE_INVENTORY}" == "1" ]]; then
  MEASURE_ARGS="${MEASURE_ARGS},'CatalogFileInventory',true"
fi
if [[ "${MEASURE_CATALOG_RAW_FILE_READ}" == "1" ]]; then
  MEASURE_ARGS="${MEASURE_ARGS},'CatalogRawFileRead',true"
fi
if [[ "${MEASURE_CATALOG_ASTRO_PERF}" == "1" ]]; then
  MEASURE_ARGS="${MEASURE_ARGS},'CatalogAstroPerf',true"
fi

run_proc_measure() {
  local Label="$1"
  local ProcTemplate="$2"
  local BatchCmd
  BatchCmd="pipeline.last.insertDB.perf.insertArchiveCatalogs2DB_perf('${ROOT}','ProcDirTemplate','${ProcTemplate}',${MEASURE_ARGS})"

  echo "=== ${Label} start $(date -Iseconds) ==="
  echo "=== INSERTDB_RUN_HOST=${INSERTDB_RUN_HOST:-unknown} RootDir=${ROOT} ProcDirTemplate=${ProcTemplate} ==="
  echo "=== CatalogFileInventory=${MEASURE_CATALOG_FILE_INVENTORY} CatalogRawFileRead=${MEASURE_CATALOG_RAW_FILE_READ} CatalogAstroPerf=${MEASURE_CATALOG_ASTRO_PERF} ==="
  echo "=== ClickHouse target: last_perf.proc_src_perf (SkipInsert, no .status) ==="

  if insertdb_run_matlab_batch "${BatchCmd}"; then
    echo "=== ${Label} done $(date -Iseconds) ==="
    return 0
  fi
  echo "=== ${Label} FAILED $(date -Iseconds) ===" >&2
  return 1
}

OverallFail=0

if [[ "${RUN_PHASE1}" == "1" ]]; then
  run_proc_measure "phase1 one telescope one night (LAST.01.10.04 2026/01/28)" "${PROC_ONE_NIGHT}" || OverallFail=1
else
  echo "=== phase 1 skipped (RUN_PHASE1=0) ==="
fi

if [[ "${RUN_PHASE2}" == "1" ]]; then
  run_proc_measure "phase2 all telescopes month 2026/01" "${PROC_ALL_MONTH}" || OverallFail=1
else
  echo "=== phase 2 skipped (RUN_PHASE2=0) ==="
fi

exit "${OverallFail}"
