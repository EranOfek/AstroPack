#!/usr/bin/env bash
# Core: write visit-level _Cat.mat + _Hdr.mat from proc Cat FITS (no DB).
# Host env from wrapper.
set -u

INSERTDB_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=insertdb_run_matlab.sh
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"

ROOT="${INSERTDB_ARCHIVE_ROOT:-/bigdata2/projects/last/data/}"
PROC_ONE_NIGHT="${PROC_ONE_NIGHT:-LAST.01.01.01/2026/06/01/proc/*}"

OVERWRITE="${OVERWRITE:-0}"
USE_ZIP_CATALOGS="${USE_ZIP_CATALOGS:-0}"
IGNORE_STATUS="${IGNORE_STATUS:-1}"

WRITE_ARGS="'IgnoreStatus',${IGNORE_STATUS}"
if [[ "${OVERWRITE}" == "1" ]]; then
  WRITE_ARGS="${WRITE_ARGS},'OverWrite',true"
fi
if [[ "${USE_ZIP_CATALOGS}" == "1" ]]; then
  WRITE_ARGS="${WRITE_ARGS},'UseZipCatalogs',true"
fi
if [[ -n "${ZIP_DIR:-}" ]]; then
  WRITE_ARGS="${WRITE_ARGS},'ZipDir','${ZIP_DIR}'"
fi
if [[ -n "${EXTRACT_ROOT:-}" ]]; then
  WRITE_ARGS="${WRITE_ARGS},'ExtractRoot','${EXTRACT_ROOT}'"
fi

BatchCmd="pipeline.last.insertDB.visitMat.writeVisitCatalogs2Mat_perf('${ROOT}','ProcDirTemplate','${PROC_ONE_NIGHT}',${WRITE_ARGS})"

echo "=== write visit catalogs to mat start $(date -Iseconds) ==="
echo "=== INSERTDB_RUN_HOST=${INSERTDB_RUN_HOST:-unknown} RootDir=${ROOT} ProcDirTemplate=${PROC_ONE_NIGHT} ==="
echo "=== OVERWRITE=${OVERWRITE} USE_ZIP_CATALOGS=${USE_ZIP_CATALOGS} IGNORE_STATUS=${IGNORE_STATUS} ==="
echo "=== Output: {proc}/_mat/{visitId}_Cat.mat and {visitId}_Hdr.mat ==="

if insertdb_run_matlab_batch "${BatchCmd}"; then
  echo "=== write visit catalogs to mat done $(date -Iseconds) ==="
  exit 0
fi
echo "=== write visit catalogs to mat FAILED $(date -Iseconds) ===" >&2
exit 1
