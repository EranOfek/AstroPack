function Mat = gladePostRead(FileName)
% Convert GLADE+ catalog text file to numeric matrix
% Package: VO.prep
% Description: PostReadFun for VO.prep.buildHTMfromFiles.
%              Reads the GLADE+ galaxy catalog (space-delimited text,
%              23M rows, 40 columns) using fast textscan. Drops string
%              name columns (3-7), encodes object type (col 8) as
%              integer, keeps all other numeric columns.
%              Missing values ("null") are read as NaN.
%
%              Object type encoding:
%              G=1, Q=2, GQ=3, C=4, GC=5
%
% Input  : - FileName: path to GLADE+ text file.
% Output : - Mat: numeric matrix [Nsrc x 35] with columns:
%              1  RA               (deg)
%              2  Dec              (deg)
%              3  GLADE_no         (integer)
%              4  PGC              (integer)
%              5  ObjType          (integer code)
%              6  Bmag             (mag)
%              7  Bmag_err         (mag)
%              8  Bmag_flag        (0=GWGC/HyperLEDA, 1=from B-J)
%              9  AbsBmag          (mag)
%              10 Jmag             (mag)
%              11 Jmag_err         (mag)
%              12 Hmag             (mag)
%              13 Hmag_err         (mag)
%              14 Kmag             (mag)
%              15 Kmag_err         (mag)
%              16 W1mag            (mag)
%              17 W1mag_err        (mag)
%              18 W2mag            (mag)
%              19 W2mag_err        (mag)
%              20 BJ_flag          (0/1)
%              21 BJmag            (mag, B-J apparent)
%              22 BJmag_err        (mag)
%              23 z_helio          (redshift)
%              24 z_cmb            (redshift)
%              25 z_flag           (0=spec,1=phot,2=from dL,3=from group)
%              26 v_err            (km/s)
%              27 z_err            (redshift)
%              28 d_L              (Mpc)
%              29 d_L_err          (Mpc)
%              30 dist_flag        (1=CMB,2=pecvel,3=from dL)
%              31 Mstar            (log10 M*/Msun)
%              32 Mstar_err        (log10 M*/Msun)
%              33 Mstar_flag       (0=from W1, 1=from B)
%              34 merger_rate      (yr^-1)
%              35 merger_rate_err  (yr^-1)
% Author : Dana + Claude (Mar 2026)
%
% GLADE+ column map (40 columns in text file):
%   1  GLADE_no      9  RA           17 Hmag_err    25 BJ_flag    33 d_L
%   2  PGC          10  Dec          18 Kmag        26 BJmag      34 d_L_err
%   3  GWGC_name*   11  Bmag         19 Kmag_err    27 BJmag_err  35 dist_flag
%   4  HyperLEDA*   12  Bmag_err     20 W1mag       28 z_helio    36 Mstar
%   5  2MASS_name*  13  Bmag_flag    21 W1mag_err   29 z_cmb      37 Mstar_err
%   6  WISExSCOS*   14  AbsBmag      22 W2mag       30 z_flag     38 Mstar_flag
%   7  SDSS_name*   15  Jmag         23 W2mag_err   31 v_err      39 merger_rate
%   8  ObjType*     16  Jmag_err     24 W2mag_err   32 z_err      40 merger_rate_err
%   (* = string columns)

    % Format: 2 numeric, 5 strings (skip), 1 string (type), 32 numeric
    % "null" is treated as NaN via TreatAsEmpty
    fmt = ['%f %f ' ...                      % cols 1-2: GLADE_no, PGC
           '%*s %*s %*s %*s %*s ' ...        % cols 3-7: names (skip)
           '%s ' ...                         % col 8: ObjType
           repmat('%f ', 1, 32)];            % cols 9-40: numeric

    FID = fopen(FileName, 'r');
    C = textscan(FID, fmt, 'TreatAsEmpty', {'null'});
    fclose(FID);

    % Encode object type as integer
    TypeStr = string(C{3});
    TypeNum = zeros(numel(TypeStr), 1);
    TypeNum(TypeStr == "G")  = 1;
    TypeNum(TypeStr == "Q")  = 2;
    TypeNum(TypeStr == "GQ") = 3;
    TypeNum(TypeStr == "C")  = 4;
    TypeNum(TypeStr == "GC") = 5;

    % Build output matrix: RA, Dec first, then rest
    % C{1}=GLADE_no, C{2}=PGC, C{3}=ObjType(str), C{4}=RA, C{5}=Dec, ...
    % C{4..35} map to input cols 9-40
    Mat = [C{4},   ...  % RA  (input col 9)
           C{5},   ...  % Dec (input col 10)
           C{1},   ...  % GLADE_no
           C{2},   ...  % PGC
           TypeNum, ...  % ObjType (encoded)
           C{6},   ...  % Bmag
           C{7},   ...  % Bmag_err
           C{8},   ...  % Bmag_flag
           C{9},   ...  % AbsBmag
           C{10},  ...  % Jmag
           C{11},  ...  % Jmag_err
           C{12},  ...  % Hmag
           C{13},  ...  % Hmag_err
           C{14},  ...  % Kmag
           C{15},  ...  % Kmag_err
           C{16},  ...  % W1mag
           C{17},  ...  % W1mag_err
           C{18},  ...  % W2mag
           C{19},  ...  % W2mag_err
           C{20},  ...  % BJ_flag
           C{21},  ...  % BJmag
           C{22},  ...  % BJmag_err
           C{23},  ...  % z_helio
           C{24},  ...  % z_cmb
           C{25},  ...  % z_flag
           C{26},  ...  % v_err
           C{27},  ...  % z_err
           C{28},  ...  % d_L
           C{29},  ...  % d_L_err
           C{30},  ...  % dist_flag
           C{31},  ...  % Mstar
           C{32},  ...  % Mstar_err
           C{33},  ...  % Mstar_flag
           C{34},  ...  % merger_rate
           C{35}];      % merger_rate_err
end
