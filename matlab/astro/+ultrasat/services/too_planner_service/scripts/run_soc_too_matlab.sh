#!/bin/bash
#
# ============================================================
#      ULTRASAT SOC - TooPlanner Service Launcher (MATLAB)
#  ------------------------------------------------------------
#  Supports:
#    - Single-run mode
#    - Auto-restart (loop) mode
#    - Optional watchdog monitor process
#
#  Example:
#     export ASTROPACK_PATH=/home/soc/dev/AstroPack.git
#     export SOC_PATH=/home/soc/soc
#     ./run_soc_too_matlab.sh --loop --watchdog
#
#
#  Author  : Chen Tishler
#  Created : 05/11/2025
#  Updated : 29/01/2026
# ============================================================

# ============================================================
#                    INSTALLATION & USAGE
# ============================================================
#
#  1.  Make the script executable:
#       chmod +x run_soc_too_matlab.sh
#
#  2.  (Optional but recommended)
#       Create a symbolic link from your SOC operational folder
#       to simplify service control:
#
#         mkdir -p $SOC_PATH/too
#         ln -sf $ASTROPACK_PATH/matlab/astro/+ultrasat/services/too_service/run_soc_too_matlab.sh \
#                $SOC_PATH/too/too.sh
#
#       Then you can simply start it from anywhere using:
#         $SOC_PATH/too/too.sh --loop --watchdog
#
#  3.  Environment setup before running:
#         export ASTROPACK_PATH=/home/soc/dev/AstroPack.git
#         export SOC_PATH=/home/soc/soc
#
#  4.  Example runs:
#         ./run_soc_too_matlab.sh
#         ./run_soc_too_matlab.sh --watchdog
#         ./run_soc_too_matlab.sh --loop --watchdog
#
# ============================================================


# --- CONFIGURATION ----------------------------------------------------------

SERVICE_NAME="soc_too_matlab"
SERVICE_FILE="soc_too_matlab.m"
WATCHDOG_FILE="${SERVICE_NAME}_watchdog.txt"
WATCHDOG_SCRIPT="watchdog_monitor.py"
WATCHDOG_TIMEOUT=120
WATCHDOG_GRACE=60

# --- ARGUMENT PARSING -------------------------------------------------------

USE_LOOP=false
USE_WATCHDOG=false

for arg in "$@"; do
  case $arg in
    --loop) USE_LOOP=true ;;
    --watchdog) USE_WATCHDOG=true ;;
  esac
done

# --- ENVIRONMENT CHECK ------------------------------------------------------

if [ -z "$ASTROPACK_PATH" ]; then
  echo "Error: ASTROPACK_PATH is not set."
  echo "Example: export ASTROPACK_PATH=/home/soc/dev/AstroPack.git"
  exit 1
fi

if [ -z "$SOC_PATH" ]; then
  echo "Error: SOC_PATH is not set."
  echo "Example: export SOC_PATH=/home/soc/soc"
  exit 1
fi

# --- PATHS ------------------------------------------------------------------

SERVICE_PATH="$ASTROPACK_PATH/matlab/astro/+ultrasat/services/too_service"
MATLAB_HOME="$ASTROPACK_PATH/matlab"
LOG_DIR="$SOC_PATH/too/log"

mkdir -p "$LOG_DIR"

# --- FUNCTION: Run MATLAB once ----------------------------------------------

run_matlab_service() {
  TIMESTAMP=$(date +"%Y_%m_%d_%H_%M_%S")
  LOG_FILE="$LOG_DIR/${SERVICE_NAME}_${TIMESTAMP}.log"

  echo "============================================================"
  echo " ULTRASAT SOC - MATLAB TooPlanner Service"
  echo "------------------------------------------------------------"
  echo " AstroPack Path : $ASTROPACK_PATH"
  echo " MATLAB Home    : $MATLAB_HOME"
  echo " Service File   : $SERVICE_PATH/$SERVICE_FILE"
  echo " Log File       : $LOG_FILE"
  echo "============================================================"

  # MATLAB command:
  #  - Set home folder
  #  - Add service folder to MATLAB path
  #  - Run the service script
  matlab -nodisplay -nosplash -r "cd('$MATLAB_HOME'); addpath('$SERVICE_PATH'); try, run('$SERVICE_PATH/$SERVICE_FILE'); catch ME, disp(getReport(ME)), end, exit" \
    | tee "$LOG_FILE"
}

# --- FUNCTION: Start watchdog -----------------------------------------------

start_watchdog() {
  echo "Starting watchdog monitor..."
  nohup python3 "$WATCHDOG_SCRIPT" "$WATCHDOG_FILE" "$WATCHDOG_TIMEOUT" "$WATCHDOG_GRACE" \
    > "$LOG_DIR/watchdog_${SERVICE_NAME}_$(date +%Y_%m_%d_%H_%M_%S).log" 2>&1 &
  echo "Watchdog PID: $!"
}

# --- MAIN LOOP --------------------------------------------------------------

if [ "$USE_WATCHDOG" = true ]; then
  start_watchdog
fi

if [ "$USE_LOOP" = true ]; then
  echo "Entering loop mode... service will auto-restart if stopped."
  while true; do
    run_matlab_service
    echo "MATLAB process exited, restarting in 5 seconds..."
    sleep 5
  done
else
  run_matlab_service
fi

echo "MATLAB TooPlanner Service finished."
