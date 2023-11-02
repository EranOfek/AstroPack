rem pause

set ASTROPACK_PATH=C:\Ultrasat\AstroPack.git
set ASTROPACK_DATA_PATH=C:\AstroPack\Data
set ASTROPACK_CONFIG_PATH=C:\AstroPack\Config
set ULTRASAT_PACK=C:\Ultrasat\Ultrasat.git


mkdir %ASTROPACK_PATH%
mkdir %ASTROPACK_DATA_PATH%
mkdir %ASTROPACK_CONFIG_PATH%
mkdir %ULTRASAT_PACK%

rem Start Watchdog process to monitor the MATLAB process
start python soc_snr_py_watchdog.py

rem Run the process in loop, if killed by watchdog or max-run-time, it will restarted
:run
soc_snr_matlab.exe
goto run

