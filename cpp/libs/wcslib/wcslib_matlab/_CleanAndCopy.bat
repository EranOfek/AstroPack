set cpp=..\cfitsio\wcslib\wcslib_class
set bin=..\cfitsio\Build\Debug
set matlab=.
set pkg=wcslibPkg
set definepkg=define%pkg%
pause

del %matlab%\%pkg%\%pkg%Interface.dll
del %matlab%\cfitsio.dll
del %matlab%\cfitsio.lib
del %matlab%\%definepkg%.m
del %matlab%\%definepkg%.mlx
del %matlab%\%pkg%Data.xml


rem copy %cpp%\wcslib_wrapper.h %matlab%\
copy %cpp%\wcslib_class_matlab.h %matlab%\
copy %bin%\cfitsio.dll %matlab%\
copy %bin%\cfitsio.lib %matlab%\
pause
