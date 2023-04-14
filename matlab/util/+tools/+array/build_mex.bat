rem Compile all MEX files in folder

rem call mex -R2020b mex_bitor_array32_threads.c mf_threads.c -lut
rem goto exit

goto x2

call mex mex_bitor_array8.c -lut
call mex mex_bitor_array16.c -lut
call mex mex_bitor_array32.c -lut
call mex mex_bitor_array64.c -lut

call mex mex_bitand_array8.c -lut
call mex mex_bitand_array16.c -lut
call mex mex_bitand_array32.c -lut
call mex mex_bitand_array64.c -lut

:x2
call mex mex_insertInFlag16.c -lut
call mex mex_insertInFlag32.c -lut
call mex mex_insertInFlagSingle.c -lut
call mex mex_insertInFlagDouble.c -lut

call mex mex_insertInInd16.c -lut
call mex mex_insertInInd32.c -lut
call mex mex_insertInIndSingle.c -lut
call mex mex_insertInIndDouble.c -lut

call mex mex_countVal16.c -lut
call mex mex_countVal32.c -lut
call mex mex_countValSingle.c -lut
call mex mex_countValDouble.c -lut

:exit
pause
