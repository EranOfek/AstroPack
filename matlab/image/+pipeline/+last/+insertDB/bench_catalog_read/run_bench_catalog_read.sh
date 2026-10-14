#!/usr/bin/env bash
# run_bench_catalog_read.sh -- host-agnostic driver for catalog read benchmarks
set -euo pipefail

BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSERTDB_PKG_DIR="$(cd "${BENCH_DIR}/.." && pwd)"

PARENT=""
THREADS="1"
MODE="all"
MAX_VISITS=1
TEMPLATE="LAST*proc_Cat_1.fits*"
LANG="both"
LOG_FILE=""

usage() {
  cat <<'EOF'
Usage: run_bench_catalog_read.sh PARENT [options]

  PARENT          proc/ directory whose children are *v0 visit folders

Options:
  --threads LIST  Comma-separated worker counts (default: 1)
  --mode MODE     raw | astro | header | all (default: all)
  --max-visits N  Number of visit folders (default: 1)
  --template PAT  Cat FITS glob inside each visit (default: LAST*proc_Cat_1.fits*)
  --lang LANG     matlab | python | both (default: both)
  --log-file PATH Shared log path (optional; each runner may still append)

Requires for MATLAB: MATLAB_BIN, WORKDIR, MATLAB_STARTUP_DIR (source insertdb_env_*.sh).
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --threads)
      THREADS="${2:?}"
      shift 2
      ;;
    --mode)
      MODE="${2:?}"
      shift 2
      ;;
    --max-visits)
      MAX_VISITS="${2:?}"
      shift 2
      ;;
    --template)
      TEMPLATE="${2:?}"
      shift 2
      ;;
    --lang)
      LANG="${2:?}"
      shift 2
      ;;
    --log-file)
      LOG_FILE="${2:?}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    -*)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
    *)
      if [[ -z "${PARENT}" ]]; then
        PARENT="$1"
      else
        echo "Unexpected argument: $1" >&2
        exit 2
      fi
      shift
      ;;
  esac
done

if [[ -z "${PARENT}" ]]; then
  echo "PARENT (proc/ directory) is required" >&2
  usage >&2
  exit 2
fi

if [[ ! -d "${PARENT}" ]]; then
  echo "PARENT not found: ${PARENT}" >&2
  exit 2
fi

THREADS_MATLAB="[$(echo "${THREADS}" | tr ',' ' ')]"
MATLAB_TEMPLATE="${TEMPLATE//\'/\'\'}"
MATLAB_PARENT="${PARENT//\'/\'\'}"

if [[ -z "${LOG_FILE}" ]]; then
  LOG_FILE="${BENCH_DIR}/bench_catalog_read_$(date +%Y%m%d_%H%M%S).log"
fi
export BENCH_LOG_FILE="${LOG_FILE}"

echo "bench_catalog_read START $(date -Iseconds) PARENT=${PARENT} threads=${THREADS} mode=${MODE} lang=${LANG}" | tee -a "${LOG_FILE}"

run_matlab() {
  local LibDir="${INSERTDB_PKG_DIR}/scripts_wrx/lib"
  if [[ ! -f "${LibDir}/insertdb_run_matlab.sh" ]]; then
    echo "MATLAB runner lib not found: ${LibDir}/insertdb_run_matlab.sh" >&2
    return 2
  fi
  # shellcheck source=/dev/null
  source "${LibDir}/insertdb_run_matlab.sh"
  local BatchCmd
  BatchCmd="addpath('${BENCH_DIR}'); bench_catalog_read('Parent','${MATLAB_PARENT}','Threads',${THREADS_MATLAB},'Mode','${MODE}','MaxVisits',${MAX_VISITS},'Template','${MATLAB_TEMPLATE}','LogFile','${LOG_FILE//\'/\'\'}');"
  insertdb_run_matlab_batch "${BatchCmd}"
}

run_python() {
  local PyBin="${PYTHON_BIN:-python3}"
  if ! command -v "${PyBin}" >/dev/null 2>&1; then
    echo "Python not found (${PyBin})" >&2
    return 2
  fi
  "${PyBin}" "${BENCH_DIR}/bench_catalog_read.py" "${PARENT}" \
    --threads "${THREADS}" \
    --mode "${MODE}" \
    --max-visits "${MAX_VISITS}" \
    --template "${TEMPLATE}" \
    --log-file "${LOG_FILE}"
}

Rc=0
case "${LANG}" in
  matlab)
    run_matlab || Rc=$?
    ;;
  python)
    run_python || Rc=$?
    ;;
  both)
    run_matlab || Rc=$?
    run_python || Rc=$?
    ;;
  *)
    echo "Invalid --lang: ${LANG}" >&2
    exit 2
    ;;
esac

echo "bench_catalog_read END $(date -Iseconds) log=${LOG_FILE}" | tee -a "${LOG_FILE}"
exit "${Rc}"
