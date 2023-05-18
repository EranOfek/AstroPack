rem Compile all MEX files in folder
rem NOTE: You may need to type 'clear all' in MATLAB to release the current compiled binary file.

rem call mex -R2020b mex_bitor_array32_threads.c mf_threads.c -lut
rem >mex  mex_bitsetFlag32.cpp  COMPFLAGS="$COMPFLAGS /openmp"

set Options=COMPFLAGS="$COMPFLAGS /openmp"
rem Do we need options '-lut' ???

rem goto x3

call mex mex_bitor_array8.cpp %Options%
call mex mex_bitor_array16.cpp %Options%
call mex mex_bitor_array32.cpp %Options%
call mex mex_bitor_array64.cpp %Options%

call mex mex_bitand_array8.cpp %Options%
call mex mex_bitand_array16.cpp %Options%
call mex mex_bitand_array32.cpp %Options%
call mex mex_bitand_array64.cpp %Options%

call mex mex_countVal16.cpp %Options%
call mex mex_countVal32.cpp %Options%
call mex mex_countValSingle.cpp %Options%
call mex mex_countValDouble.cpp %Options%

call mex mex_bitsetFlag8.cpp %Options%
call mex mex_bitsetFlag16.cpp %Options%
call mex mex_bitsetFlag32.cpp %Options%
call mex mex_bitsetFlag64.cpp %Options%

call mex mex_insertInFlag16.cpp %Options%
call mex mex_insertInFlag32.cpp %Options%
call mex mex_insertInFlagSingle.cpp %Options%
call mex mex_insertInFlagDouble.cpp %Options%

call mex mex_insertInInd16.cpp %Options%
call mex mex_insertInInd32.cpp %Options%
call mex mex_insertInIndSingle.cpp %Options%
call mex mex_insertInIndDouble.cpp %Options%

:exit
pause
