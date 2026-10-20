#!/usr/bin/env bash
set -u
HOST_SCRIPTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSERTDB_PKG_DIR="$(cd "${HOST_SCRIPTS_DIR}/../.." && pwd)"
INSERTDB_COMMON_DIR="$(cd "${HOST_SCRIPTS_DIR}/../common" && pwd)"
export INSERTDB_PKG_DIR
# shellcheck source=../common/insertdb_run_matlab.sh
source "${INSERTDB_COMMON_DIR}/insertdb_run_matlab.sh"
# shellcheck source=insertdb_env_wrx80.sh
source "${HOST_SCRIPTS_DIR}/insertdb_env_wrx80.sh"
exec "${INSERTDB_COMMON_DIR}/_run_insert_archive.sh" "$@"
