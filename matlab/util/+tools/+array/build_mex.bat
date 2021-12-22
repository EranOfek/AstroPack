
rem call mex -R2020b mex_bitor_array32_threads.c mf_threads.c -lut
rem goto exit

call mex mex_bitor_array8.c -lut
call mex mex_bitor_array16.c -lut
call mex mex_bitor_array32.c -lut
call mex mex_bitor_array64.c -lut

call mex mex_bitand_array8.c -lut
call mex mex_bitand_array16.c -lut
call mex mex_bitand_array32.c -lut
call mex mex_bitand_array64.c -lut

:exit
pause
