function Mat = tepcatPostRead(FileName)
% Convert TEPCat allinfo CSV to numeric matrix
% Package: VO.prep
% Description: PostReadFun for VO.prep.buildHTMfromFiles.
%              Reads the TEPCat transiting exoplanet catalog CSV.
%              Drops the System name column, encodes Type as integer,
%              converts RA/Dec from degrees to radians, and replaces
%              -999 and -1 with NaN.
%
%              Type encoding: TEP=1, KTEP=2, BD=3, CBP=4
%
% Input  : - FileName: path to TEPCat CSV file.
% Output : - Mat: numeric matrix [Nsrc x 55] with columns:
%              1  RA              (rad)
%              2  Dec             (rad)
%              3  Type            (integer code)
%              4  Vmag            (mag)
%              5  Kmag            (mag)
%              6  TransitDur      (days)
%              7  TransitDepth    (percent)
%              8  T0              (HJD/BJD)
%              9  T0_err          (days)
%              10 Period          (days)
%              11 Period_err      (days)
%              12 Teff            (K)
%              13 Teff_errup      (K)
%              14 Teff_errdn      (K)
%              15 FeH             (dex)
%              16 FeH_errup       (dex)
%              17 FeH_errdn       (dex)
%              18 M_A             (Msun)
%              19 M_A_errup       (Msun)
%              20 M_A_errdn       (Msun)
%              21 R_A             (Rsun)
%              22 R_A_errup       (Rsun)
%              23 R_A_errdn       (Rsun)
%              24 loggA           (cgs)
%              25 loggA_errup     (cgs)
%              26 loggA_errdn     (cgs)
%              27 rho_A           (rho_sun)
%              28 rho_A_errup     (rho_sun)
%              29 rho_A_errdn     (rho_sun)
%              30 e               (-)
%              31 e_errup         (-)
%              32 e_errdn         (-)
%              33 a               (AU)
%              34 a_errup         (AU)
%              35 a_errdn         (AU)
%              36 M_b             (Mjup)
%              37 M_b_errup       (Mjup)
%              38 M_b_errdn       (Mjup)
%              39 R_b             (Rjup)
%              40 R_b_errup       (Rjup)
%              41 R_b_errdn       (Rjup)
%              42 g_b             (m/s^2)
%              43 g_b_errup       (m/s^2)
%              44 g_b_errdn       (m/s^2)
%              45 rho_b           (rho_jup)
%              46 rho_b_errup     (rho_jup)
%              47 rho_b_errdn     (rho_jup)
%              48 Teq             (K)
%              49 Teq_errup       (K)
%              50 Teq_errdn       (K)
%              51 Lambda          (deg)
%              52 Lambda_errup    (deg)
%              53 Lambda_errdn    (deg)
%              54 Psi             (deg)
%              55 Psi_errup       (deg)
%              56 Psi_errdn       (deg)
% Author : Dana + Claude (Mar 2026)
%
% CSV columns (57):
%   1  System*    9  T0         17 erru(FeH)  25 loggA      33 errdn(e)   41 errup(R_b) 49 Teq       57 err(Psi)
%   2  Type*     10  T0err      18 errd(FeH)  26 errup      34 a(AU)      42 errdn(R_b) 50 err(Teq)
%   3  RA(deg)   11  Period     19 M_A        27 errdn      35 errup      43 g_b        51 err(Teq)
%   4  Dec(deg)  12  Perioderr  20 errup      28 rho_A      36 errdn      44 errup      52 Lambda
%   5  Vmag      13  Teff       21 errdn      29 errup      37 M_b        45 errdn      53 err
%   6  Kmag      14  err        22 R_A        30 errdn      38 errup      46 rho_b      54 err
%   7  length    15  err        23 errup      31 e          39 errdn      47 errup      55 Psi
%   8  depth     16  [Fe/H]     24 errdn      32 errup      40 R_b        48 errdn      56 err
%   (* = string columns)

    RAD = 180 ./ pi;

    % Format: 1 string (skip), 1 string (type), 55 numeric
    fmt = ['%*s %s ' repmat('%f ', 1, 55)];

    FID = fopen(FileName, 'r');
    % Skip header line
    fgetl(FID);
    C = textscan(FID, fmt, 'Delimiter', ',', 'TreatAsEmpty', {'null', ''});
    fclose(FID);

    % Encode Type as integer
    TypeStr = string(C{1});
    TypeNum = zeros(numel(TypeStr), 1);
    TypeNum(TypeStr == "TEP")  = 1;
    TypeNum(TypeStr == "KTEP") = 2;
    TypeNum(TypeStr == "BD")   = 3;
    TypeNum(TypeStr == "CBP")  = 4;

    % Build output matrix: RA, Dec (convert deg→rad), then Type, then rest
    % C{1}=Type(str), C{2}=RA(deg), C{3}=Dec(deg), C{4..56}=numeric cols 5-57
    Mat = [C{2} ./ RAD, ...  % RA (rad)
           C{3} ./ RAD, ...  % Dec (rad)
           TypeNum,      ...  % Type (encoded)
           C{4},         ...  % Vmag
           C{5},         ...  % Kmag
           C{6},         ...  % TransitDur
           C{7},         ...  % TransitDepth
           C{8},         ...  % T0
           C{9},         ...  % T0_err
           C{10},        ...  % Period
           C{11},        ...  % Period_err
           C{12},        ...  % Teff
           C{13},        ...  % Teff_errup
           C{14},        ...  % Teff_errdn
           C{15},        ...  % FeH
           C{16},        ...  % FeH_errup
           C{17},        ...  % FeH_errdn
           C{18},        ...  % M_A
           C{19},        ...  % M_A_errup
           C{20},        ...  % M_A_errdn
           C{21},        ...  % R_A
           C{22},        ...  % R_A_errup
           C{23},        ...  % R_A_errdn
           C{24},        ...  % loggA
           C{25},        ...  % loggA_errup
           C{26},        ...  % loggA_errdn
           C{27},        ...  % rho_A
           C{28},        ...  % rho_A_errup
           C{29},        ...  % rho_A_errdn
           C{30},        ...  % e
           C{31},        ...  % e_errup
           C{32},        ...  % e_errdn
           C{33},        ...  % a
           C{34},        ...  % a_errup
           C{35},        ...  % a_errdn
           C{36},        ...  % M_b
           C{37},        ...  % M_b_errup
           C{38},        ...  % M_b_errdn
           C{39},        ...  % R_b
           C{40},        ...  % R_b_errup
           C{41},        ...  % R_b_errdn
           C{42},        ...  % g_b
           C{43},        ...  % g_b_errup
           C{44},        ...  % g_b_errdn
           C{45},        ...  % rho_b
           C{46},        ...  % rho_b_errup
           C{47},        ...  % rho_b_errdn
           C{48},        ...  % Teq
           C{49},        ...  % Teq_errup
           C{50},        ...  % Teq_errdn
           C{51},        ...  % Lambda
           C{52},        ...  % Lambda_errup
           C{53},        ...  % Lambda_errdn
           C{54},        ...  % Psi
           C{55},        ...  % Psi_errup
           C{56}];            % Psi_errdn

    % Replace -999 and -1 with NaN (skip RA, Dec columns)
    Mat(:, 3:end) = changem_nan(Mat(:, 3:end));
end

function M = changem_nan(M)
    M(M == -999) = NaN;
    M(M == -1)   = NaN;
end
