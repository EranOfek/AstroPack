@echo off
setlocal EnableExtensions
cd /d "%~dp0"

rem Continuous MATLAB insert — parallel to Python debug_insert_continuous.
rem Requires API + manager on US_BASE_URL (default http://127.0.0.1:8151).

if not defined MATLAB_ROOT set "MATLAB_ROOT=c:\Matlab\R2025b"
if not defined US_BASE_URL set "US_BASE_URL=http://127.0.0.1:8151"
if not defined US_INTERVAL_SEC set "US_INTERVAL_SEC=60"
if not defined US_MAX_ROUNDS set "US_MAX_ROUNDS=0"
if not defined US_ROWS set "US_ROWS=1000"

set "MATLAB_EXE=%MATLAB_ROOT%\bin\matlab.exe"
if not exist "%MATLAB_EXE%" (
  echo ERROR: MATLAB not found at %MATLAB_EXE%
  echo Set MATLAB_ROOT to your R2025b install, e.g. c:\Matlab\R2025b
  exit /b 1
)

echo MATLAB_ROOT=%MATLAB_ROOT%
echo US_BASE_URL=%US_BASE_URL%
if defined ASTROPACK_PATH echo ASTROPACK_PATH=%ASTROPACK_PATH%
echo US_INTERVAL_SEC=%US_INTERVAL_SEC% US_ROWS=%US_ROWS% US_MAX_ROUNDS=%US_MAX_ROUNDS%
echo Starting db.sources.debug.debug_insert_continuous...

set "UTIL_PATH="
if defined ASTROPACK_PATH set "UTIL_PATH=addpath(''%ASTROPACK_PATH%\matlab\util'');"

"%MATLAB_EXE%" -batch "%UTIL_PATH% db.sources.debug.debug_insert_continuous('IntervalSec', %US_INTERVAL_SEC%, 'Rows', %US_ROWS%, 'MaxRounds', %US_MAX_ROUNDS%);"
exit /b %ERRORLEVEL%
