#!/usr/bin/env bash
set -u
HOST_SCRIPTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../scripts_euclid" && pwd)"
INSERTDB_PKG_DIR="$(cd "${HOST_SCRIPTS_DIR}/.." && pwd)"
INSERTDB_LIB_DIR="${INSERTDB_PKG_DIR}/scripts_wrx/lib"
BENCH_DIR="${INSERTDB_PKG_DIR}/bench_unzip"
export INSERTDB_PKG_DIR BENCH_DIR

# shellcheck source=/dev/null
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"
# shellcheck source=/dev/null
source "${HOST_SCRIPTS_DIR}/insertdb_env_euclid.sh"

exec "${BENCH_DIR}/run_bench_unzip.sh" "$@"
