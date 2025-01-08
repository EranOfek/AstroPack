call mex ang2pix_nested.cpp -lut
call mex ang2pix_ring.cpp -lut

call mex pix2ang_nested.cpp -lut
call mex pix2ang_ring.cpp -lut

rem -lstdc++ -pthread

goto exit


:exit
pause
