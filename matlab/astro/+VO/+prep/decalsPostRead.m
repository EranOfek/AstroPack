function Mat = decalsPostRead(T)
% Convert DECaLS DR10 sweep table to numeric matrix
% Package: VO.prep
% Description: PostReadFun for VO.prep.buildHTMfromFiles.
%              Selects columns from DECaLS sweep catalog and
%              encodes the TYPE string column as integer:
%              PSF=1, REX=2, EXP=3, DEV=4, SER=5, DUP=6.
%
% Input  : - T: MATLAB table from FITS.readTable1 with OutClass=[].
% Output : - Mat: numeric matrix [Nsrc x 23] with columns:
%              1  RA          (deg)
%              2  DEC         (deg)
%              3  RA_IVAR     (1/deg^2)
%              4  DEC_IVAR    (1/deg^2)
%              5  Type        (integer code)
%              6  FLUX_G      (nanomaggy)
%              7  FLUX_R      (nanomaggy)
%              8  FLUX_I      (nanomaggy)
%              9  FLUX_Z      (nanomaggy)
%              10 FLUX_W1     (nanomaggy)
%              11 FLUX_W2     (nanomaggy)
%              12 FLUX_W3     (nanomaggy)
%              13 FLUX_W4     (nanomaggy)
%              14 FLUX_IVAR_G (1/nanomaggy^2)
%              15 FLUX_IVAR_R (1/nanomaggy^2)
%              16 FLUX_IVAR_I (1/nanomaggy^2)
%              17 FLUX_IVAR_Z (1/nanomaggy^2)
%              18 FLUX_IVAR_W1(1/nanomaggy^2)
%              19 FLUX_IVAR_W2(1/nanomaggy^2)
%              20 FLUX_IVAR_W3(1/nanomaggy^2)
%              21 FLUX_IVAR_W4(1/nanomaggy^2)
%              22 MASKBITS    (bitmask)
%              23 SHAPE_R     (arcsec)
% Author : Dana + Claude (Mar 2026)

    TypeStr = string(T.TYPE);
    TypeNum = zeros(height(T), 1);
    TypeNum(TypeStr == "PSF") = 1;
    TypeNum(TypeStr == "REX") = 2;
    TypeNum(TypeStr == "EXP") = 3;
    TypeNum(TypeStr == "DEV") = 4;
    TypeNum(TypeStr == "SER") = 5;
    TypeNum(TypeStr == "DUP") = 6;

    Mat = [T.RA, T.DEC, T.RA_IVAR, T.DEC_IVAR, TypeNum, ...
           T.FLUX_G, T.FLUX_R, T.FLUX_I, T.FLUX_Z, ...
           T.FLUX_W1, T.FLUX_W2, T.FLUX_W3, T.FLUX_W4, ...
           T.FLUX_IVAR_G, T.FLUX_IVAR_R, T.FLUX_IVAR_I, T.FLUX_IVAR_Z, ...
           T.FLUX_IVAR_W1, T.FLUX_IVAR_W2, T.FLUX_IVAR_W3, T.FLUX_IVAR_W4, ...
           double(T.MASKBITS), T.SHAPE_R];
end
