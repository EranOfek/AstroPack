
rem Start Watchdog process to monitor the MATLAB process
start python watchdog_monitor.py snr_matlab_watchdog.txt

rem Run the process in loop, if killed by watchdog or max-run-time, it will restarted
:run
soc_snr_matlab.exe
timeout /t 5
goto run
