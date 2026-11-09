function [Cat, Info] = buildPowerLawCatalog(Args)
    % The source catalog of a DUT set from a power law of GALEX NUV counts: random uniform positions in a cone, U for one SED
    %   The arguments are the fields of Cfg.Catalog of ultrasat.dut.config
    %   with 'CatType' 'PowerLaw':
    %   C = ultrasat.simconfig.toArgs(Cfg.Catalog); ultrasat.dut.buildPowerLawCatalog(C{:}).
    %   1. The number of sources is a Poisson draw with the mean
    %      Area * integral of dN/dm = 10^(Slope*NUV + Intercept) [deg^-2 mag^-1]
    %      over NUVRange, Area the cone of Radius around (RA0, Dec0).
    %   2. Each source: a position random uniform on the sphere inside the
    %      cone (the same probability per unit area, no clustering) and an
    %      NUV from the power law (continuous, by the inverse cumulative
    %      distribution).
    %   3. U (AB, the r = 0 filter) = NUV + (U - NUV) of the SED, by
    %      ultrasat.dut.sedColors; the sources with U <= MaxU are kept.
    %   The draws do not depend on the SED, so catalogs of different SEDs
    %   with the same Seed have the same sources (positions and NUV); the
    %   one with the more negative U - NUV keeps more of them.
    %   Columns: as ultrasat.dut.buildSourceCatalog, so the same downstream
    %   code reads both: RA, Dec [deg], U [AB mag], USource (5: power law,
    %   U from NUV with the set's SED), G, BP, RP, Plx, ErrPlx (NaN), NUV
    %   [AB mag], e_NUV, FUV, e_FUV, MatchDist, Nblend (NaN), SynthParent (0),
    %   InGap (0).
    %   Writes OutFile (a FITS binary table).
    % Input  : * ...,key,val,...
    %            'CatType' - must be 'PowerLaw' (the field that selects this
    %                   builder in ultrasat.dut.simulateSet).
    %            'RA0', 'Dec0' - [deg] the cone center. Default is 254, 64.
    %            'Radius' - [deg] the cone radius. Default is 15.
    %            'MaxU' - Default is 22.5.
    %            'SED' - one SED for all sources: 'BB<Teff>' or a Pickles
    %                   template name. Default is 'G2V'.
    %            'Slope' - of log10 dN/dm. Default is 0.312.
    %            'Intercept' - of log10 dN/dm, with dN/dm per deg^2 per mag.
    %                   Default is -3.950 (with Slope 0.312: the GALEX AIS
    %                   counts of the N3 field, NUV 15-20.5, Nov 2026).
    %            'NUVRange' - [mag] the NUV range of the law. Default is [13 26].
    %            'GABminusVega' - G(AB) - G(Vega), for sedColors. Default is 0.1136.
    %            'Seed' - Default is 1.
    %            'OutFile' - '' (default): nothing is written.
    %            'Header' - N x 3 cell {key, value, comment} added to the table
    %                   header (e.g. ultrasat.simconfig.headerRows). Default is {}.
    %            'Verbose' - Default is true.
    % Output : - Cat: a table of the sources (the columns above).
    %          - Info: the expected and drawn counts, U - NUV and the files.
    % Author : D. Kovaleva (Nov 2026)
    % Example: Cfg = ultrasat.dut.config('CatType', 'PowerLaw', 'ConfigN', 4);
    %          C = ultrasat.simconfig.toArgs(Cfg.Catalog);
    %          [Cat, Info] = ultrasat.dut.buildPowerLawCatalog(C{:});
    %          [Cat, Info] = ultrasat.dut.buildPowerLawCatalog('CatType','PowerLaw','Radius',1,'Verbose',false);

    arguments
        Args.CatType (1,:) char {mustBeMember(Args.CatType, {'PowerLaw'})} = 'PowerLaw'
        Args.RA0 (1,1) double = 254
        Args.Dec0 (1,1) double = 64
        Args.Radius (1,1) double {mustBePositive} = 15
        Args.MaxU (1,1) double = 22.5
        Args.SED (1,:) char = 'G2V'
        Args.Slope (1,1) double {mustBePositive} = 0.312
        Args.Intercept (1,1) double = -3.950
        Args.NUVRange (1,2) double = [13 26]
        Args.GABminusVega (1,1) double = 0.1136
        Args.Seed (1,1) double = 1
        Args.OutFile char = ''
        Args.Header cell = {}
        Args.Verbose (1,1) logical = true
    end
    Say = @(varargin) Args.Verbose && fprintf([varargin{1} '\n'], varargin{2:end});
    if Args.NUVRange(2) <= Args.NUVRange(1)
        error('dut:buildPowerLawCatalog:NUVRange', 'NUVRange must be increasing, got %s', mat2str(Args.NUVRange));
    end
    RngSaved   = rng;
    rng(Args.Seed);
    RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    T0 = tic;

    % 1. the number of sources
    Area  = 2*pi*(1 - cosd(Args.Radius)) * (180/pi)^2;                 % [deg^2]
    A     = Args.Slope * log(10);
    E1    = 10.^(Args.Slope .* Args.NUVRange);
    Nexp  = Area * 10^Args.Intercept * (E1(2) - E1(1)) / A;
    N     = poissrnd(Nexp);

    % 2. positions in the cone and NUV, by the inverse cumulative distributions
    CosT  = 1 - rand(N, 1) .* (1 - cosd(Args.Radius));
    SinT  = sqrt(1 - CosT.^2);
    Phi   = 2*pi*rand(N, 1);
    SinD  = sind(Args.Dec0) .* CosT + cosd(Args.Dec0) .* SinT .* cos(Phi);
    Dec   = asind(SinD);
    RA    = mod(Args.RA0 + atan2d(sin(Phi) .* SinT .* cosd(Args.Dec0), CosT - sind(Args.Dec0) .* SinD), 360);
    NUV   = log10(E1(1) + rand(N, 1) .* (E1(2) - E1(1))) ./ Args.Slope;

    % 3. U of the SED and the cut
    [~, UmNUV] = ultrasat.dut.sedColors(Args.SED, Args.GABminusVega);
    U    = NUV + UmNUV;
    Keep = U <= Args.MaxU;
    Nk   = sum(Keep);

    Cat0 = nan(Nk, 17);
    Cat0(:,1)  = RA(Keep);
    Cat0(:,2)  = Dec(Keep);
    Cat0(:,3)  = U(Keep);
    Cat0(:,4)  = 5;
    Cat0(:,10) = NUV(Keep);
    Cat0(:,16) = 0;
    Cat0(:,17) = 0;
    Names = {'RA','Dec','U','USource','G','BP','RP','Plx','ErrPlx','NUV','e_NUV','FUV','e_FUV', ...
             'MatchDist','Nblend','SynthParent','InGap'};
    Units = {'deg','deg','mag','','mag','mag','mag','mas','mas','mag','mag','mag','mag','arcsec','','',''};
    Cat = array2table(Cat0, 'VariableNames', Names);
    Cat.Properties.VariableUnits = Units;

    Info = struct('N', Nk, 'Ndrawn', N, 'Nexpected', Nexp, 'Area', Area, 'UmNUV', UmNUV, ...
                  'Time', toc(T0), 'Files', {{}});
    Say('power law 10^(%.3f NUV %+.3f), NUV %g-%g, %.0f deg^2: %d drawn (%.0f expected); SED %s, U - NUV = %.3f; U <= %.2f: %d sources (%.0f s)', ...
        Args.Slope, Args.Intercept, Args.NUVRange, Area, N, Nexp, Args.SED, UmNUV, Args.MaxU, Nk, Info.Time);

    if ~isempty(Args.OutFile)
        H = { ...
            'CATTYPE',  'PowerLaw',     'random uniform positions, NUV from a power law'; ...
            'SED',      Args.SED,       'the SED of all sources'; ...
            'UMNUV',    UmNUV,          'U - NUV of the SED'; ...
            'UFILTER',  'ULTRASAT r=0', 'U: AB, the ULTRASAT filter at the field center'; ...
            'MAXU',     Args.MaxU,      'U cut'; ...
            'CONE_RA',  Args.RA0,       '[deg] cone center'; ...
            'CONE_DEC', Args.Dec0,      '[deg] cone center'; ...
            'CONE_R',   Args.Radius,    '[deg] cone radius'; ...
            'LAWSLOPE', Args.Slope,     'log10 dN/dm = LAWSLOPE*NUV + LAWINTC'; ...
            'LAWINTC',  Args.Intercept, 'dN/dm per deg^2 per mag'; ...
            'LAWNUV1',  Args.NUVRange(1), '[mag] NUV range of the law, bright end'; ...
            'LAWNUV2',  Args.NUVRange(2), '[mag] NUV range of the law, faint end'; ...
            'NEXPECT',  Nexp,           'expected number of sources over the NUV range'; ...
            'NDRAWN',   int32(N),       'drawn number of sources over the NUV range'; ...
            'USRC5',    'power_law',    'USource 5: power-law source, U from NUV with SED'; ...
            'NSRC',     int32(Nk),      'number of sources'};
        H = [H; Args.Header];
        ultrasat.dut.writeTable(Cat, Args.OutFile, H, 'SOURCES');
        Info.Files = {Args.OutFile};
        Say('written: %s', Args.OutFile);
    end
end
