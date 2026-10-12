#!/usr/bin/env bash
echo "run_measure_proc_overhead.sh: use scripts_wrx/ or scripts_euclid/ host wrapper" >&2
LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "${LIB_DIR}/../run_measure_proc_overhead_wrx80.sh" "$@"
