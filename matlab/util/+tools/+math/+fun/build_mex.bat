rem Compile all MEX files in folder

call mex mex_sincosSingle.c -lut
call mex mex_sincosDouble.c -lut

call mex mex_logFlagSingle.c -lut
call mex mex_logFlagDouble.c -lut

call mex mex_sinFlagSingle.c -lut
call mex mex_sinFlagDouble.c -lut

call mex mex_cosFlagSingle.c -lut
call mex mex_cosFlagDouble.c -lut

call mex mex_tanFlagSingle.c -lut
call mex mex_tanFlagDouble.c -lut

:exit
pause
