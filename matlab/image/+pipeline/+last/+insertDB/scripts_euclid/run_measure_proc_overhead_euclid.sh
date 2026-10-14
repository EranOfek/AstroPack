#!/usr/bin/env bash
set -u
HOST_SCRIPTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSERTDB_PKG_DIR="$(cd "${HOST_SCRIPTS_DIR}/.." && pwd)"
INSERTDB_LIB_DIR="${INSERTDB_PKG_DIR}/scripts_wrx/lib"
export INSERTDB_PKG_DIR
# Optional: copy measure_proc_overhead.env.example -> measure_proc_overhead.env (not in git)
MEASURE_ENV="${HOST_SCRIPTS_DIR}/measure_proc_overhead.env"
if [[ -f "${MEASURE_ENV}" ]]; then
  set -a
  # shellcheck source=/dev/null
  source "${MEASURE_ENV}"
  set +a
fi
source "${INSERTDB_LIB_DIR}/insertdb_run_matlab.sh"
source "${HOST_SCRIPTS_DIR}/insertdb_env_euclid.sh"
exec "${INSERTDB_LIB_DIR}/_run_measure_proc_overhead.sh" "$@"
