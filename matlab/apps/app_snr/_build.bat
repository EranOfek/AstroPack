rem _build.bat - Compile MATLAB script to standalone EXE file (Windows).
rem
rem See: https://www.mathworks.com/help/compiler/create-and-install-a-standalone-application-from-matlab-code.html
rem See: https://www.mathworks.com/help/compiler/mcc.html
rem See: https://www.mathworks.com/help/compiler/isdeployed.html

pause

mcc -m soc_app_snr.m
rem applicationCompiler

zip a -r

pause
