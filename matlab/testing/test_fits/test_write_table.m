%
% 
% Index   Extension                        Type         Dimension           
% =====   ==============================   ==========   ====================
% 1       Primary                          Image        0                   
% 2       my-table                         Binary       4 cols X 4 rows     
%
% Header 1:
%   SIMPLE  =                    T / file does conform to FITS standard
%   BITPIX  =                   16 / number of bits per data pixel
%   NAXIS   =                    0 / number of data axes
%   EXTEND  =                    T / FITS dataset may contain extensions
%   COMMENT   FITS (Flexible Image Transport System) format is defined in 'Astronomy
%   COMMENT   and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H
%   END
%
%
% Header 2:
%   XTENSION= 'BINTABLE'           / binary table extension
%   BITPIX  =                    8 / 8-bit bytes
%   NAXIS   =                    2 / 2-dimensional binary table
%   NAXIS1  =                   19 / width of table in bytes
%   NAXIS2  =                    4 / number of rows in table
%   PCOUNT  =                   72 / size of special data area
%   GCOUNT  =                    1 / one data group (required keyword)
%   TFIELDS =                    4 / number of fields in each row
%   TTYPE1  = 'Col1    '           / label for field   1
%   TFORM1  = '2L      '           / data format of field: 1-byte LOGICAL
%   TTYPE2  = 'Col2    '           / label for field   2
%   TFORM2  = '3X      '           / data format of field: BIT
%   TUNIT2  = 'kg/m^3  '           / physical unit of field
%   TTYPE3  = 'Col3    '           / label for field   3
%   TFORM3  = '1D      '           / data format of field: 8-byte DOUBLE
%   TUNIT3  = 'candela '           / physical unit of field
%   TTYPE4  = 'Col4    '           / label for field   4
%   TFORM4  = '1PC(4)  '           / data format of field: variable length array
%   TUNIT4  = 'parsec  '           / physical unit of field
%   EXTNAME = 'my-table'           / name of this binary table extension
%   END


import matlab.io.*
fptr = fits.createFile('myfile.fits');

ttype = {'Col1','Col2','Col3','Col4'};
tform = {'2L','3X','1D','1PC'};
tunit = {'','kg/m^3','candela','parsec'};  
fits.createTbl(fptr,'binary',0,ttype,tform,tunit,'my-table');

fits.writeCol(fptr,1,1,[false false; true false]);
fits.writeCol(fptr,2,1,int8([0 1 1; 1 1 1; 1 1 1; 1 0 1]));
fits.writeCol(fptr,3,1,[1; 2; 3; 4]);

data = cell(4,1);
data{1} = single(1);
data{2} = single(1+2j);
data{3} = single([1j 2 3+j]);
data{4} = single([1 2+3j 3 4]);
fits.writeCol(fptr,4,1,data);

fits.closeFile(fptr);
fitsdisp('myfile.fits','index',2,'mode','full');

