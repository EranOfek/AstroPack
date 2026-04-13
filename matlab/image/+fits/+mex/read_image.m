% Read an image from a FITS, FITS.fz, FITS.gz or FITS.bz2 file
% A special case of INT16 and BZERO = 32768 employed for UINT16 values is treated as well
% Compiled with: 
% mex read_image.cpp /usr/lib/x86_64-linux-gnu/libcfitsio.a /home/sasha/ExternalLib/bzip2-1.0.8/libbz2.a -lz -lcurl -lm
% after: sudo apt install libcfitsio-dev and:
% wget https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz 
% tar xzf bzip2-1.0.8.tar.gz && cd bzip2-1.0.8 
% make CFLAGS="-fPIC -O2" libbz2.a
% 
% Input  : - file name (*fits or *fits.fz)
%          - HDU number 
%          NB: if the number is 0, the function will scan all the HDUs and read the first image found  
%          - CCDSEC (optional)
% Output : - a matrix (image)
%          - a header (optional)
%          - the hdu number where the image was found (optional)
% Author : A.M. Krassilchtchikov (2026 Mar) 
% Example: FN = '~/LAST.01.01.01_20250708.014436.528_clear_1718.c_020_001_001_sci_raw_Image_1.fits.fz';
%          Image = fits.mex.read_image(FN,0);
%          [Image, Header] = fits.mex.read_image(FN,0);
%          [Image, Header] = fits.mex.read_image(FN,1,[2100 2200 1700 1900]);
