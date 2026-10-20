#!/usr/bin/env bash
set -u
HOST_SCRIPTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSERTDB_PKG_DIR="$(cd "${HOST_SCRIPTS_DIR}/.." && pwd)"
INSERTDB_COMMON_DIR="${INSERTDB_PKG_DIR}/scripts/common"
INSERTDB_WRX_DIR="${INSERTDB_PKG_DIR}/scripts/wrx"
export INSERTDB_PKG_DIR
# shellcheck source=../scripts/common/insertdb_run_matlab.sh
source "${INSERTDB_COMMON_DIR}/insertdb_run_matlab.sh"
# shellcheck source=../scripts/wrx/insertdb_env_wrx80.sh
source "${INSERTDB_WRX_DIR}/insertdb_env_wrx80.sh"
exec "${HOST_SCRIPTS_DIR}/_run_write_archive_proc_cat_mat.sh" "$@"
