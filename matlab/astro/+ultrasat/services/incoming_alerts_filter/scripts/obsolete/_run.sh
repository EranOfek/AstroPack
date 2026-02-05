#!/bin/bash

# Start Watchdog process to monitor the MATLAB process
python watchdog_monitor.py soc_slew_matlab_watchdog.txt &

# Run the process in loop, if killed by watchdog or max-run-time, it will be restarted
while true; do
    ./run_soc_slew_matlab.sh
done
