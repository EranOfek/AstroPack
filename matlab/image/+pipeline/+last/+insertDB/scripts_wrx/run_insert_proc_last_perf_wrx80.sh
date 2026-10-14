#!/usr/bin/env bash
set -u
HOST_SCRIPTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSERTDB_PKG_DIR="$(cd "${HOST_SCRIPTS_DIR}/.." && pwd)"
INSERTDB_LIB_DIR="${HOST_SCRIPTS_DIR}/lib"
export INSERTDB_PKG_DIR
MEASURE_ENV="${HOST_SCRIPTS_DIR}/measure_proc_overhead.env"
if [[ -f "${MEASURE_ENV}" ]]; then
  set -a
  # shellcheck source=/dev/null
  source "${MEASURE_ENV}"
  set +a
fi
# shellcheck source=lib/insertdb_run_matlab.sh
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"
# shellcheck source=insertdb_env_wrx80.sh
source "${HOST_SCRIPTS_DIR}/insertdb_env_wrx80.sh"
exec "${INSERTDB_LIB_DIR}/_run_insert_proc_last_perf.sh" "$@"
