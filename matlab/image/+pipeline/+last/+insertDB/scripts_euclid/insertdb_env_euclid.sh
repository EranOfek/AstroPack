# insertdb_env_euclid.sh -- euclid (local /bigdata2 + ClickHouse). Source from run_*_euclid.sh wrappers.
# Cold WRX compare: override at invoke time, e.g. RUN_PHASE2=0 PROC_ONE_NIGHT='LAST.01.10.04/2026/02/15/proc/*'

insertdb_derive_astropack_root "${INSERTDB_PKG_DIR:?INSERTDB_PKG_DIR not set}"

: "${MATLAB_BIN:=/usr/local/MATLAB/R2025b/bin/matlab}"
: "${INSERTDB_ARCHIVE_ROOT:=/bigdata2/projects/last/data/}"
: "${ASTROPACK_ROOT:=${HOME}/matlab/AstroPack}"
: "${MATLAB_STARTUP_DIR:=${ASTROPACK_ROOT}/matlab/startup}"
: "${WORKDIR:=${ASTROPACK_ROOT}}"

export MATLAB_BIN INSERTDB_ARCHIVE_ROOT ASTROPACK_ROOT MATLAB_STARTUP_DIR WORKDIR
export INSERTDB_RUN_HOST=euclid
