% Read the header of a FITS, FITS.fz, FITS.gz or FITS.bz2 file
% Compiled with:
% mex CXX=g++-9 read_header.cpp /usr/lib/x86_64-linux-gnu/libcfitsio.a /home/sasha/ExternalLib/bzip2-1.0.8/libbz2.a -lz -lcurl -lm
% after: sudo apt install libcfitsio-dev and:
% wget https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz
% tar xzf bzip2-1.0.8.tar.gz && cd bzip2-1.0.8
% make CFLAGS="-fPIC -O2" libbz2.a
%
% Input  : - file name (*fits, *fits.bz2, *fits.gz or *fits.fz)
%          - HDU number
%            NB: if the number is 0 or negative, HDU 1 (primary) is used
% Output : - a header: Nx3 cell array {keyword, value, comment}
%            values are typed: double, logical, or char
% Author : A.M. Krassilchtchikov (2026 May)
% Example: FN = '~/LAST.01.04.01_20251112.161132.830_clear_LHS1140_017_001_024_sci_proc_Cat_1.fits.bz2'
%          Header = io.fits.mex.read_header(FN, 3);

