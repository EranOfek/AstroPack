#!/usr/bin/env bash
echo "run_test_last_perf_insert.sh: use scripts_wrx/ or scripts_euclid/ host wrapper" >&2
LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "${LIB_DIR}/../run_test_last_perf_insert_wrx80.sh" "$@"
