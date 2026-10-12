#!/usr/bin/env bash
set -u
HOST_SCRIPTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSERTDB_PKG_DIR="$(cd "${HOST_SCRIPTS_DIR}/.." && pwd)"
INSERTDB_LIB_DIR="${INSERTDB_PKG_DIR}/scripts_wrx/lib"
export INSERTDB_PKG_DIR
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"
source "${HOST_SCRIPTS_DIR}/insertdb_env_euclid.sh"
exec "${INSERTDB_LIB_DIR}/_run_test_last_perf_insert.sh" "$@"
