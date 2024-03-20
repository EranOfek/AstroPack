# FITS format info


2880 bytes data block

header Big endian
36 lines of 80 char
last line END
after that spaces till the block end

data - in 2880 Bytes blocks
BITPIX specifies the bits per pix
order, NAXIS1, NAXIS2, ...
after data end, block is filled with zeros.

key name byte 1 to 8
left justified
space filled
upper case

value indicator: bytes 9 10 "= " - if exist then there is a vlaue, unless HISTORY/COMMENT, or blank keywords

value/comment 11 to 80
if not "= " then 9 to 80 is a comment

value format in section 4.2
or null (spaces)

'/' comment after value - a space is recomended.

use fixed value 
strings in ' '  ''-for '
in fixed value ' is in byte 11

logical T or F in Byte 30

integer - right justified in bytes 11 to 30

real -  right justified in bytes 11 to 30

