function Mat = decalsPostRead(T)
% Convert DECaLS DR10 sweep table to numeric matrix
% Package: VO.prep
% Description: PostReadFun for VO.prep.buildHTMfromFiles.
%              Selects columns from DECaLS sweep catalog and
%              encodes the TYPE string column as integer:
%              PSF=1, REX=2, EXP=3, DEV=4, SER=5, DUP=6.
%              Uses column indices because FITS.readTable1 with
%              OutClass=[] loses column names for mixed-type tables.
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
%
% Column index map (from DECaLS DR10 sweep FITS):
%   5  TYPE           12 FLUX_G         20 FLUX_IVAR_G    93  SHAPE_R
%   6  RA             13 FLUX_R         21 FLUX_IVAR_R   127  MASKBITS
%   7  DEC            14 FLUX_I         22 FLUX_IVAR_I
%   8  RA_IVAR        15 FLUX_Z         23 FLUX_IVAR_Z
%   9  DEC_IVAR       16 FLUX_W1        24 FLUX_IVAR_W1
%                     17 FLUX_W2        25 FLUX_IVAR_W2
%                     18 FLUX_W3        26 FLUX_IVAR_W3
%                     19 FLUX_W4        27 FLUX_IVAR_W4

    % Encode TYPE (column 5, char) as integer
    TypeStr = string(T{:, 5});
    TypeNum = zeros(height(T), 1);
    TypeNum(TypeStr == "PSF") = 1;
    TypeNum(TypeStr == "REX") = 2;
    TypeNum(TypeStr == "EXP") = 3;
    TypeNum(TypeStr == "DEV") = 4;
    TypeNum(TypeStr == "SER") = 5;
    TypeNum(TypeStr == "DUP") = 6;

    % Extract numeric columns by index
    Mat = [double(T{:, 6}),  ...  % RA
           double(T{:, 7}),  ...  % DEC
           double(T{:, 8}),  ...  % RA_IVAR
           double(T{:, 9}),  ...  % DEC_IVAR
           TypeNum,           ...  % Type (encoded)
           double(T{:, 12}), ...  % FLUX_G
           double(T{:, 13}), ...  % FLUX_R
           double(T{:, 14}), ...  % FLUX_I
           double(T{:, 15}), ...  % FLUX_Z
           double(T{:, 16}), ...  % FLUX_W1
           double(T{:, 17}), ...  % FLUX_W2
           double(T{:, 18}), ...  % FLUX_W3
           double(T{:, 19}), ...  % FLUX_W4
           double(T{:, 20}), ...  % FLUX_IVAR_G
           double(T{:, 21}), ...  % FLUX_IVAR_R
           double(T{:, 22}), ...  % FLUX_IVAR_I
           double(T{:, 23}), ...  % FLUX_IVAR_Z
           double(T{:, 24}), ...  % FLUX_IVAR_W1
           double(T{:, 25}), ...  % FLUX_IVAR_W2
           double(T{:, 26}), ...  % FLUX_IVAR_W3
           double(T{:, 27}), ...  % FLUX_IVAR_W4
           double(T{:, 127}), ... % MASKBITS
           double(T{:, 93})];     % SHAPE_R
end
