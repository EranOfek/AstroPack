#!/usr/bin/env bash
# run_bench_unzip.sh -- host-agnostic driver for unzip-to-tmpfs benchmarks
set -euo pipefail

BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSERTDB_PKG_DIR="$(cd "${BENCH_DIR}/.." && pwd)"

ZIP_DIR=""
ZIP_FILE=""
DEST_ROOT="/dev/shm"
METHODS="unzip"
ITERS=1
WARMUP=0
FAIL_FAST=0
LOG_FILE=""

usage() {
  cat <<'EOF'
Usage: run_bench_unzip.sh ZIP_DIR [options]

  ZIP_DIR         Directory containing *.zip (e.g. proc/_zip)

Options:
  --zip-file PATH   Single zip instead of directory
  --dest-root PATH  Extract root (default: /dev/shm)
  --methods LIST    Comma list or all: unzip,bsdtar,7z,python,matlab (default: unzip)
  --iters N         Timed iterations per zip (default: 1)
  --warmup N        Warmup cycles per zip (default: 0)
  --fail-fast       Stop on first error
  --log-file PATH   Shared log path (optional)

Python runs unzip, bsdtar, 7z, python. MATLAB method uses bench_unzip.m.
Requires for MATLAB: source insertdb_env_*.sh (MATLAB_BIN, WORKDIR, MATLAB_STARTUP_DIR).
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --zip-file)
      ZIP_FILE="${2:?}"
      shift 2
      ;;
    --dest-root)
      DEST_ROOT="${2:?}"
      shift 2
      ;;
    --methods)
      METHODS="${2:?}"
      shift 2
      ;;
    --iters)
      ITERS="${2:?}"
      shift 2
      ;;
    --warmup)
      WARMUP="${2:?}"
      shift 2
      ;;
    --fail-fast)
      FAIL_FAST=1
      shift
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
      if [[ -z "${ZIP_DIR}" ]]; then
        ZIP_DIR="$1"
      else
        echo "Unexpected argument: $1" >&2
        exit 2
      fi
      shift
      ;;
  esac
done

if [[ -z "${ZIP_DIR}" && -z "${ZIP_FILE}" ]]; then
  echo "ZIP_DIR or --zip-file is required" >&2
  usage >&2
  exit 2
fi

if [[ -n "${ZIP_DIR}" && ! -d "${ZIP_DIR}" ]]; then
  echo "ZIP_DIR not found: ${ZIP_DIR}" >&2
  exit 2
fi

if [[ ! -d "${DEST_ROOT}" ]]; then
  echo "dest-root not found: ${DEST_ROOT}" >&2
  exit 2
fi

if [[ -z "${LOG_FILE}" ]]; then
  LOG_FILE="${BENCH_DIR}/bench_unzip_$(date +%Y%m%d_%H%M%S).log"
fi
export BENCH_LOG_FILE="${LOG_FILE}"

echo "bench_unzip START $(date -Iseconds) zipDir=${ZIP_DIR} methods=${METHODS} dest=${DEST_ROOT}" | tee -a "${LOG_FILE}"

want_matlab=0
want_python=0
IFS=',' read -r -a _METHOD_ARR <<< "${METHODS}"
for m in "${_METHOD_ARR[@]}"; do
  m="$(echo "${m}" | tr -d ' ')"
  if [[ "${m}" == "matlab" ]]; then
    want_matlab=1
  elif [[ "${m}" == "all" ]]; then
    want_python=1
    want_matlab=1
  else
    want_python=1
  fi
done
if [[ "${METHODS}" == "all" ]]; then
  want_python=1
  want_matlab=1
fi

PY_METHODS="${METHODS}"
if [[ "${want_matlab}" -eq 1 ]]; then
  PY_METHODS="$(echo "${METHODS}" | tr ',' '\n' | grep -v '^matlab$' | grep -v '^$' | paste -sd, - || true)"
  if [[ "${METHODS}" == "all" ]]; then
    PY_METHODS="all"
  fi
fi

run_python() {
  local PyBin="${PYTHON_BIN:-python3}"
  if ! command -v "${PyBin}" >/dev/null 2>&1; then
    echo "Python not found (${PyBin})" >&2
    return 2
  fi
  local -a PyArgs=()
  if [[ -n "${ZIP_FILE}" ]]; then
    PyArgs+=(--zip-file "${ZIP_FILE}")
  fi
  if [[ -n "${ZIP_DIR}" ]]; then
    PyArgs+=("${ZIP_DIR}")
  fi
  if [[ -z "${PY_METHODS}" ]]; then
    return 0
  fi
  PyArgs+=(--dest-root "${DEST_ROOT}" --methods "${PY_METHODS}" --iters "${ITERS}" --warmup "${WARMUP}" --log-file "${LOG_FILE}")
  if [[ "${FAIL_FAST}" -eq 1 ]]; then
    PyArgs+=(--fail-fast)
  fi
  "${PyBin}" "${BENCH_DIR}/bench_unzip.py" "${PyArgs[@]}"
}

run_matlab() {
  local LibDir="${INSERTDB_PKG_DIR}/scripts/common"
  if [[ ! -f "${LibDir}/insertdb_run_matlab.sh" ]]; then
    echo "MATLAB runner lib not found: ${LibDir}/insertdb_run_matlab.sh" >&2
    return 2
  fi
  # shellcheck source=/dev/null
  source "${LibDir}/insertdb_run_matlab.sh"
  local BatchCmd="addpath('${BENCH_DIR}'); "
  if [[ -n "${ZIP_FILE}" ]]; then
    BatchCmd+="bench_unzip('ZipFile','${ZIP_FILE//\'/\'\'}',"
  else
    BatchCmd+="bench_unzip('ZipDir','${ZIP_DIR//\'/\'\'}',"
  fi
  BatchCmd+="'DestRoot','${DEST_ROOT//\'/\'\'}','Niter',${ITERS},'Warmup',${WARMUP},'LogFile','${LOG_FILE//\'/\'\'}');"
  insertdb_run_matlab_batch "${BatchCmd}"
}

Rc=0
if [[ "${want_python}" -eq 1 ]]; then
  run_python || Rc=$?
fi
if [[ "${want_matlab}" -eq 1 ]]; then
  run_matlab || Rc=$?
fi

echo "bench_unzip END $(date -Iseconds) log=${LOG_FILE}" | tee -a "${LOG_FILE}"
exit "${Rc}"
