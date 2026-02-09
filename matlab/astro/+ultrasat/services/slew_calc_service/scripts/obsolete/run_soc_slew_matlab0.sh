#!/bin/bash
#
# ============================================================
#  ULTRASAT SOC - Slew Service Launcher (MATLAB)
#  ------------------------------------------------------------
#  This script starts the MATLAB process:
#     soc_slew_matlab.m
#
#  The AstroPack root is determined from the environment
#  variable:
#     $ASTROPACK_PATH
#
#  Example:
#     export ASTROPACK_PATH=/home/soc/dev/AstroPack.git
#     ./run_soc_slew_matlab.sh
#
#  The script runs MATLAB in non-GUI mode and writes logs
#  to ./log/soc_slew_matlab_<timestamp>.log
# ============================================================

# --- Verify environment variable ---
if [ -z "$ASTROPACK_PATH" ]; then
  echo "Error: ASTROPACK_PATH environment variable is not set."
  echo "Please run: export ASTROPACK_PATH=/home/soc/dev/AstroPack.git"
  exit 1
fi

# --- Define service paths ---
SERVICE_PATH="$ASTROPACK_PATH/matlab/astro/+ultrasat/services/slew_service"
SERVICE_FILE="soc_slew_matlab.m"
LOG_DIR="./log"

# --- Create log directory if missing ---
mkdir -p "$LOG_DIR"

# --- Generate timestamped log file ---
TIMESTAMP=$(date +"%Y_%m_%d_%H_%M_%S")
LOG_FILE="$LOG_DIR/soc_slew_matlab_$TIMESTAMP.log"

# --- Start message ---
echo "============================================================"
echo " ULTRASAT SOC - MATLAB Slew Service"
echo "------------------------------------------------------------"
echo " AstroPack path : $ASTROPACK_PATH"
echo " Service file   : $SERVICE_PATH/$SERVICE_FILE"
echo " Log file       : $LOG_FILE"
echo "============================================================"

# --- Run MATLAB in no-GUI mode ---
matlab -nodisplay -nosplash -r "try, run('$SERVICE_PATH/$SERVICE_FILE'); catch ME, disp(getReport(ME)), end, exit" | tee "$LOG_FILE"

# --- Exit message ---
echo "MATLAB Slew Service finished. Log saved to: $LOG_FILE"
