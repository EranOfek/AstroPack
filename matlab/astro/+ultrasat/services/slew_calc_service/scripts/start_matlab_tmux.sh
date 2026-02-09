#!/bin/bash
# ============================================================
#  Start MATLAB Slew Service in tmux session
# ============================================================

SESSION="soc_slew_matlab"
SCRIPT_PATH="$ASTROPACK_PATH/matlab/astro/+ultrasat/services/slew_calc_service/run_soc_slew_calc_matlab.sh"

# Ensure environment
if [ -z "$ASTROPACK_PATH" ]; then
  echo "ASTROPACK_PATH not set (e.g., export ASTROPACK_PATH=/home/soc/dev/AstroPack.git)"
  exit 1
fi

if [ -z "$SOC_PATH" ]; then
  echo "SOC_PATH not set (e.g., export SOC_PATH=/home/soc/soc)"
  exit 1
fi

tmux has-session -t "$SESSION" 2>/dev/null
if [ $? != 0 ]; then
  echo "Starting MATLAB service in tmux session [$SESSION]"
  tmux new-session -d -s "$SESSION" "$SCRIPT_PATH --watchdog --loop"
else
  echo "Session [$SESSION] already running."
fi
