call mex mex_fits_write_image_thread.cpp -lut 
rem -lstdc++ -pthread
goto exit

call mex mex_fits_write_image.cpp -lut
call mex mex_fits_table_write_image_header.cpp -lut

:exit
pause
