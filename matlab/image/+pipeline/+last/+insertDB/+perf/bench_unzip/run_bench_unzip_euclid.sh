#!/usr/bin/env bash
set -u
BENCH_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSERTDB_PKG_DIR="$(cd "${BENCH_DIR}/../.." && pwd)"
INSERTDB_COMMON_DIR="${INSERTDB_PKG_DIR}/scripts/common"
INSERTDB_EUCLID_DIR="${INSERTDB_PKG_DIR}/scripts/euclid"
export INSERTDB_PKG_DIR BENCH_DIR

# shellcheck source=../../scripts/common/insertdb_run_matlab.sh
source "${INSERTDB_COMMON_DIR}/insertdb_run_matlab.sh"
# shellcheck source=../../scripts/euclid/insertdb_env_euclid.sh
source "${INSERTDB_EUCLID_DIR}/insertdb_env_euclid.sh"

exec "${BENCH_DIR}/run_bench_unzip.sh" "$@"
