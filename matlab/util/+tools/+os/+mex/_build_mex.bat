rem Compile all MEX files in folder
rem NOTE: You may need to type 'clear all' in MATLAB to release the current compiled binary file.

rem call mex -R2020b mex_bitor_array32_threads.c mf_threads.c -lut
rem >mex  mex_bitsetFlag32.cpp  COMPFLAGS="$COMPFLAGS /openmp"

set Options=COMPFLAGS="$COMPFLAGS /openmp"
rem /arch:AVX512

call mex is_avx512_supported.cpp %Options%


:exit
pause
