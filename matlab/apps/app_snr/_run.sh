#!/bin/bash

# Start Watchdog process to monitor the MATLAB process
python soc_snr_py_watchdog.py &

# Run the process in loop, if killed by watchdog or max-run-time, it will be restarted
while true; do
    ./soc_snr_matlab
done
