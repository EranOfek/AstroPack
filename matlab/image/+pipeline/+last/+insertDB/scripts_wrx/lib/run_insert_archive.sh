#!/usr/bin/env bash
echo "run_insert_archive.sh: use scripts_wrx/run_insert_archive_wrx80.sh or scripts_euclid/run_insert_archive_euclid.sh" >&2
LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "${LIB_DIR}/../run_insert_archive_wrx80.sh" "$@"
