rem _build.bat - Compile MATLAB script to standalone EXE file (Windows).
rem
rem See: https://www.mathworks.com/help/compiler/create-and-install-a-standalone-application-from-matlab-code.html
rem See: https://www.mathworks.com/help/compiler/mcc.html
rem See: https://www.mathworks.com/help/compiler/isdeployed.html
rem
rem Need startup-2022-12-15-Chen-Windows-FileMap.m
rem 
rem Upload to this folder, then download it on our AWS server:
rem
rem   https://drive.google.com/drive/folders/1i7jYXhYx07OlSBHhcG5VvfDsdOEmnq9O?usp=share_link
rem

pause

call mcc -m soc_app_snr.m
rem applicationCompiler

rem zip a -r

pause
