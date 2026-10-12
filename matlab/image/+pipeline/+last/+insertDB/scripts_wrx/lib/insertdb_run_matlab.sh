# insertdb_run_matlab.sh -- sourced by _run_*.sh cores (do not execute directly).
# Requires: MATLAB_BIN, WORKDIR, MATLAB_STARTUP_DIR

insertdb_run_matlab_batch() {
  local BatchCmd="$1"
  if [[ -z "${MATLAB_BIN:-}" || -z "${WORKDIR:-}" || -z "${MATLAB_STARTUP_DIR:-}" ]]; then
    echo "insertdb_run_matlab_batch: MATLAB_BIN, WORKDIR, MATLAB_STARTUP_DIR must be set" >&2
    return 2
  fi
  if [[ ! -d "${MATLAB_STARTUP_DIR}" ]]; then
    echo "insertdb_run_matlab_batch: MATLAB_STARTUP_DIR not found: ${MATLAB_STARTUP_DIR}" >&2
    return 2
  fi
  ( cd "${WORKDIR}" && "${MATLAB_BIN}" -nodisplay -nosplash -sd "${MATLAB_STARTUP_DIR}" -batch "${BatchCmd}" )
}

insertdb_derive_astropack_root() {
  local InsertDbPkgDir="$1"
  if [[ -n "${ASTROPACK_ROOT:-}" ]]; then
    return 0
  fi
  ASTROPACK_ROOT="$(cd "${InsertDbPkgDir}/../../../../.." && pwd)"
}
