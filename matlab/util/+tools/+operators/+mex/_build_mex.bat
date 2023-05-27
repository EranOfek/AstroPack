rem Compile all MEX files in folder
rem NOTE: You may need to type 'clear all' in MATLAB to release the current compiled binary file.

rem call mex -R2020b mex_bitor_array32_threads.c mf_threads.c -lut
rem >mex  mex_bitsetFlag32.cpp  COMPFLAGS="$COMPFLAGS /openmp"

set Options=COMPFLAGS="$COMPFLAGS /openmp"
set OptionsAVX2=COMPFLAGS="$COMPFLAGS /openmp /arch:AVX2"
set OptionsAVX512=COMPFLAGS="$COMPFLAGS /openmp /arch:AVX512"

call mex mex_timesDouble.cpp %OptionsAVX%
call mex mex_timesDouble_avx2.cpp %OptionsAVX2%
call mex mex_timesDouble_avx512.cpp %OptionsAVX512%

call mex mex_times8.cpp %Options%
call mex mex_times16.cpp %Options%
call mex mex_times32.cpp %Options%
call mex mex_times64.cpp %Options%
call mex mex_timesSingle.cpp %Options%

:exit
pause
