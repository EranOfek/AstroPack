#!/usr/bin/env bash
# run_debug_insert_continuous.sh — continuous MATLAB insert (parallel to Python).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ -z "${MATLAB_ROOT:-}" ]]; then
  if [[ "${OSTYPE:-}" == msys* ]] || [[ "${OSTYPE:-}" == cygwin* ]]; then
    MATLAB_ROOT="c:/Matlab/R2025b"
  else
    MATLAB_ROOT="/usr/local/MATLAB/R2025b"
  fi
fi

export US_BASE_URL="${US_BASE_URL:-http://127.0.0.1:8151}"
export US_INTERVAL_SEC="${US_INTERVAL_SEC:-60}"
export US_MAX_ROUNDS="${US_MAX_ROUNDS:-0}"
export US_ROWS="${US_ROWS:-1000}"

MATLAB_BIN="${MATLAB_ROOT}/bin/matlab"
if [[ ! -x "$MATLAB_BIN" ]]; then
  echo "ERROR: MATLAB not found at $MATLAB_BIN" >&2
  echo "Set MATLAB_ROOT to your install (e.g. c:/Matlab/R2025b or /usr/local/MATLAB/R2025b)" >&2
  exit 1
fi

ADDPATH=""
if [[ -n "${ASTROPACK_PATH:-}" ]]; then
  UTIL_PATH="${ASTROPACK_PATH}/matlab/util"
  ADDPATH="addpath('${UTIL_PATH}'); "
fi

echo "MATLAB_ROOT=$MATLAB_ROOT"
echo "US_BASE_URL=$US_BASE_URL"
[[ -n "${ASTROPACK_PATH:-}" ]] && echo "ASTROPACK_PATH=$ASTROPACK_PATH"
echo "US_INTERVAL_SEC=$US_INTERVAL_SEC US_ROWS=$US_ROWS US_MAX_ROUNDS=$US_MAX_ROUNDS"

exec "$MATLAB_BIN" -batch \
  "${ADDPATH}db.sources.debug.debug_insert_continuous('IntervalSec', ${US_INTERVAL_SEC}, 'Rows', ${US_ROWS}, 'MaxRounds', ${US_MAX_ROUNDS});"
