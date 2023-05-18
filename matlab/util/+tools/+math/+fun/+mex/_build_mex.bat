rem Compile all MEX files in folder

set Options=COMPFLAGS="$COMPFLAGS /openmp"
rem -lut ???

call mex mex_sincosSingle.cpp %Options%
call mex mex_sincosDouble.cpp %Options%

goto exit
call mex mex_logFlagSingle.cpp %Options%
call mex mex_logFlagDouble.cpp %Options%

call mex mex_sinFlagSingle.cpp %Options%
call mex mex_sinFlagDouble.cpp %Options%

call mex mex_cosFlagSingle.cpp %Options%
call mex mex_cosFlagDouble.cpp %Options%

call mex mex_tanFlagSingle.cpp %Options%
call mex mex_tanFlagDouble.cpp %Options%

:exit
pause
