@rem _build.bat - Compile MATLAB script to standalone EXE file (Windows).
@rem
@rem ------------------------------------------------------------------------
@rem
@rem See: https://www.mathworks.com/help/compiler/create-and-install-a-standalone-application-from-matlab-code.html
@rem See: https://www.mathworks.com/help/compiler/mcc.html
@rem See: https://www.mathworks.com/help/compiler/isdeployed.html
@rem
@rem IMPORTANT: Need startup-2022-12-15-Chen-Windows-FileMap.m
@rem 
@rem ------------------------------------------------------------------------
@@rem 
@rem Upload soc_app_snr.exe to this shared folder on Google Drive, 
@rem then download it on our AWS server to c:/soc/snr/aws_matlab
@rem
@rem   https://drive.google.com/drive/folders/1i7jYXhYx07OlSBHhcG5VvfDsdOEmnq9O?usp=share_link
@rem
@rem On our AWS server, login with: ultrasatdev@gmail.com
@rem

pause

call mcc -m soc_snr_matlab.m

rem applicationCompiler
rem zip a -r

pause

copy soc_snr_matlab.exe %SOC_PATH%\snr\snr_matlab\
