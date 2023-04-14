pause
set src=sincos
set dest=tanFlag

copy %src%.m %dest%.m
rem copy mex_%src%_include.c mex_%dest%_include.c
copy mex_%src%Single.m mex_%dest%Single.c
copy mex_%src%Double.c mex_%dest%Double.c

pause
