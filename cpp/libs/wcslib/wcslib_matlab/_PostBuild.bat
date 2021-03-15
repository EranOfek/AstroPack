
set cpp=D:\Ultrasat\wcs\wcslib\cfitsio\wcslib\wrapper
set bin=D:\Ultrasat\wcs\wcslib\cfitsio\Build\Debug
set matlab=D:\Ultrasat\wcs\wcslib\wcs_matlab
set pkg=wcslibPkg
set definepkg=define%pkg%

pause

copy %matlab%\%pkg%\%pkg%Interface.dll %matlab%
del %matlab%\%pkg%\%pkg%Interface.dll

pause

