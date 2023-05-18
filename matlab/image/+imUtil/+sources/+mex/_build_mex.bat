rem Compile all MEX files in folder
rem NOTE: You may need to type 'clear all' in MATLAB to release the current compiled binary file.

set Options=
rem set Options=COMPFLAGS="$COMPFLAGS /openmp"
rem Do we need options '-lut' ???

call mex mex_findLocalMaxAboveThreshold_single.cpp %Options%
call mex mex_findLocalMaxAboveThreshold_double.cpp %Options%

:exit
pause
