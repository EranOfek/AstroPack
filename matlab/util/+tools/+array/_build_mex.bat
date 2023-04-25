rem Compile all MEX files in folder
rem NOTE: You may need to type 'clear all' in MATLAB to 
rem release the current compiled binary file

rem call mex -R2020b mex_bitor_array32_threads.c mf_threads.c -lut


rem >mex  mex_bitsetFlag32.cpp  COMPFLAGS="$COMPFLAGS /openmp"


rem goto exit

rem goto x3


set Options=COMPFLAGS="$COMPFLAGS /openmp"
rem -lut ???


call mex mex_bitor_array8.cpp %Options%
call mex mex_bitor_array16.cpp %Options%
call mex mex_bitor_array32.cpp %Options%
call mex mex_bitor_array64.cpp %Options%

call mex mex_bitand_array8.cpp %Options%
call mex mex_bitand_array16.cpp %Options%
call mex mex_bitand_array32.cpp %Options%
call mex mex_bitand_array64.cpp %Options%

:x2

call mex mex_countVal16.cpp %Options%
call mex mex_countVal32.cpp %Options%
call mex mex_countValSingle.cpp %Options%
call mex mex_countValDouble.cpp %Options%
rem goto exit

:x3
call mex -v mex_bitsetFlag32.cpp
rem call mex mex_bitsetFlag32.c %Options%
rem goto exit


call mex mex_bitsetFlag8.cpp %Options%
call mex mex_bitsetFlag16.cpp %Options%
call mex mex_bitsetFlag32.cpp %Options%
call mex mex_bitsetFlag64.cpp %Options%
goto exit

call mex mex_insertInFlag16.cpp %Options%
call mex mex_insertInFlag32.cpp %Options%
call mex mex_insertInFlagSingle.cpp %Options%
call mex mex_insertInFlagDouble.cpp %Options%
goto exit

call mex mex_insertInInd16.cpp %Options%
call mex mex_insertInInd32.cpp %Options%
call mex mex_insertInIndSingle.cpp %Options%
call mex mex_insertInIndDouble.cpp %Options%

:exit
pause
