rem Compile all MEX files in folder
rem NOTE: You may need to type 'clear all' in MATLAB to release the current compiled binary file.

call mex mex_xxhash.cpp
call mex mex_xxhashFile.cpp

:exit
pause
