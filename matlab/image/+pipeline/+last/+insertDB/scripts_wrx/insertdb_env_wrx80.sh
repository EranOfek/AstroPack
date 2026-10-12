# insertdb_env_wrx80.sh -- WRX80 (NFS mount to /bigdata2). Source from run_*_wrx80.sh wrappers.

insertdb_derive_astropack_root "${INSERTDB_PKG_DIR:?INSERTDB_PKG_DIR not set}"

: "${MATLAB_BIN:=${HOME}/Soft/M2025b/bin/matlab}"
: "${INSERTDB_ARCHIVE_ROOT:=/bigdata2/projects/last/data/}"
: "${ASTROPACK_ROOT:=${HOME}/matlab/AstroPack}"
: "${MATLAB_STARTUP_DIR:=${ASTROPACK_ROOT}/matlab/startup}"
: "${WORKDIR:=${ASTROPACK_ROOT}}"

export MATLAB_BIN INSERTDB_ARCHIVE_ROOT ASTROPACK_ROOT MATLAB_STARTUP_DIR WORKDIR
export INSERTDB_RUN_HOST=wrx80
