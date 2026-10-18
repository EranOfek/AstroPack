function [SimImage, Src] = simulateGaiaField(Args)
    % Simulate an ULTRASAT tile image of a sky field from Gaia DR3 stars (Pickles templates from the Gaia CMD)
    %   1. Gaia DR3 stars in the tile: catsHTM cone search of GAIADR3 around
    %      the tile center (or a user catalog), cut to the tile FOV with the
    %      same local WCS as ultrasat.usim builds from RA0, Dec0, Tile and
    %      PlaneRotation.
    %   2. E(B-V): SFD98 (astro.extinction.sky_ebv) unless given, scaled with
    %      distance inside astro.stars.bprp2teff.
    %   3. Pickles template per star from Gaia BP-RP, G and parallax
    %      (astro.stars.gaia2picklesClass: luminosity class from PARSEC
    %      isochrone boundaries in the Gaia CMD).
    %   4. Dereddened G in AB: G0 = G(Vega) + GABminusVega - A_G, with A_G
    %      computed on the star's own template with
    %      astro.extinction.extinction, the same law usim applies, so that usim
    %      reddens G0 back to the observed G.
    %   5. Predicted U (r=0 filter, reddened) from the template; stars with
    %      U > MaxU are dropped.
    %   6. ultrasat.usim with the AstroSpec array, G0 in GAIA/G and per-star E(B-V).
    % Input  : * ...,key,val,...
    %            'RA0', 'Dec0' - [deg] telescope aimpoint (the [0 0] pixel of
    %                   the 4 tiles), as in ultrasat.usim. Default is the
    %                   Kepler field, 291, 44.5.
    %            'Tile'          - 'A', 'B', 'C' or 'D'. Default is 'B'.
    %            'PlaneRotation' - [deg] as in ultrasat.usim. Default is 0.
    %            'Exposure'      - [number duration(s)]. Default is [1 300].
    %            'Cat' - [] (default) queries the GAIADR3 catsHTM catalog;
    %                   or a table with columns RA, Dec [deg], phot_g_mean_mag,
    %                   bp_rp, Plx, ErrPlx [mas] (GAIADR3 column names).
    %            'CatName' - catsHTM catalog name. Default is 'GAIADR3'.
    %            'Ebv' - [] (default): SFD98 at each star; or a scalar or a
    %                   vector of total line-of-sight E(B-V) per star.
    %            'MaxU' - drop stars with predicted U (AB, r=0) above this.
    %                   Default is 25.
    %            'NoColor' - stars without BP-RP: 'median' (default) gives
    %                   them the field median observed BP-RP [PLACEHOLDER];
    %                   'drop' removes them.
    %            'GABminusVega' - G(AB) - G(Vega) of Gaia DR3. Default is
    %                   0.1136 (zero points 25.8010 and 25.6874, Riello et al.
    %                   2021). The synthetic value from astro.spec.synthetic_phot
    %                   is 0.023 mag larger: there Vega has 0 mag, in the Gaia
    %                   Vega system 0.023 mag.
    %            'MH' - [M/H] of the luminosity-class boundaries. Default is 0.
    %            'BprpArgs' - cell of key,val for astro.stars.bprp2teff. Default is {}.
    %            'UsimArgs' - cell of further key,val for ultrasat.usim
    %                   (e.g. {'OutDir','/tmp','OutName','Kepler_B'}). Default is {}.
    %            'PicklesDir' - Pickles library directory. Default is from Installer.
    %            'Simulate' - false stops after the catalog (SimImage = []).
    %                   Default is true.
    %            'Verbose' - Default is true.
    % Output : - The ultrasat.usim output (AstroImage), or [] if Simulate=false.
    %          - Table of the simulated stars: RA, Dec, G, BP_RP, NoColor,
    %            Plx, ErrPlx, EbvTot, Ebv (applied), BP_RP0, M_G0, LumClass,
    %            Template, G0_AB, U_pred, X, Y. Stars of the FOV with
    %            U_pred > MaxU are not included.
    % Author : D. Kovaleva (Oct 2026)
    % Example: [Sim, Src] = ultrasat.simulateGaiaField('Tile','B','MaxU',24,'UsimArgs',{'OutDir','/tmp'});
    %          [~, Src] = ultrasat.simulateGaiaField('Simulate',false);   % catalog only

    arguments
        Args.RA0              = 291;
        Args.Dec0             = 44.5;
        Args.Tile             = 'B';
        Args.PlaneRotation    = 0;
        Args.Exposure         = [1 300];
        Args.Cat              = [];
        Args.CatName          = 'GAIADR3';
        Args.Ebv              = [];
        Args.MaxU             = 25;
        Args.NoColor          = 'median';
        Args.GABminusVega     = 0.1136;
        Args.MH               = 0;
        Args.BprpArgs cell    = {};
        Args.UsimArgs cell    = {};
        Args.PicklesDir       = [];
        Args.Simulate logical = true;
        Args.Verbose logical  = true;
    end
    I = Installer;
    if isempty(Args.PicklesDir)
        Args.PicklesDir = I.getDataDir('PicklesStellarSpec');
    end
    Say = @(varargin) Args.Verbose && fprintf([varargin{1} '\n'], varargin{2:end});

    % 1. the tile WCS (as in ultrasat.usim), its center and the stars in it
    [WCS, NX, NY] = tileWCS(Args.RA0, Args.Dec0, Args.Tile, Args.PlaneRotation);
    [CRA, CDec] = WCS.xy2sky((NX+1)/2, (NY+1)/2);
    [KRA, KDec] = WCS.xy2sky([0.5 NX+0.5 0.5 NX+0.5], [0.5 0.5 NY+0.5 NY+0.5]);
    Radius = rad2deg(max(celestial.coo.sphere_dist(CRA, CDec, KRA(:), KDec(:), 'deg'))) * 1.01; % [deg]
    if isempty(Args.Cat)
        Say('Cone search of %s: center %.4f %+.4f, radius %.2f deg', Args.CatName, CRA, CDec, Radius);
        [M, ColC] = catsHTM.cone_search(Args.CatName, deg2rad(CRA), deg2rad(CDec), Radius*3600);
        T = array2table(M, 'VariableNames', ColC);
        T.RA  = rad2deg(T.RA);
        T.Dec = rad2deg(T.Dec);
    else
        T = Args.Cat;
    end
    T = T(:, {'RA', 'Dec', 'phot_g_mean_mag', 'bp_rp', 'Plx', 'ErrPlx'});
    [X, Y] = WCS.sky2xy(T.RA, T.Dec);
    In = X > 0.1 & Y > 0.1 & X < NX & Y < NY & ~isnan(T.phot_g_mean_mag);  % the usim FOV test
    Src = table(T.RA(In), T.Dec(In), T.phot_g_mean_mag(In), T.bp_rp(In), T.Plx(In), T.ErrPlx(In), X(In), Y(In), ...
                'VariableNames', {'RA', 'Dec', 'G', 'BP_RP', 'Plx', 'ErrPlx', 'X', 'Y'});
    Say('%d stars with G in the tile %s FOV (of %d in the cone)', height(Src), Args.Tile, height(T));

    % 2. E(B-V) and stars without color
    if isempty(Args.Ebv)
        Src.EbvTot = astro.extinction.sky_ebv(deg2rad(Src.RA), deg2rad(Src.Dec));
    else
        E = Args.Ebv(:);
        if numel(E) > 1
            E = E(In);
        end
        Src.EbvTot = E .* ones(height(Src), 1);
    end
    Src.NoColor = isnan(Src.BP_RP);
    switch lower(Args.NoColor)
        case 'median'
            Src.BP_RP(Src.NoColor) = median(Src.BP_RP, 'omitnan');
        case 'drop'
            Src = Src(~Src.NoColor, :);
        otherwise
            error('simulateGaiaField: NoColor must be ''median'' or ''drop''');
    end
    Say('%d stars without BP-RP (NoColor=%s)', sum(Src.NoColor), Args.NoColor);

    % 3. Pickles template from the CMD
    Gal = celestial.coo.coco([Src.RA Src.Dec], 'j2000.0', 'g', 'd', 'd');
    [Name, R] = astro.stars.gaia2picklesClass(Src.BP_RP, 'G', Src.G, 'Plx', Src.Plx, 'PlxErr', Src.ErrPlx, ...
                                               'Ebv', Src.EbvTot, 'GalLat', Gal(:,2), 'MH', Args.MH, ...
                                               'BprpArgs', Args.BprpArgs, 'PicklesDir', Args.PicklesDir);
    Src.Ebv      = R.Ebv;
    Src.BP_RP0   = R.BP_RP0;
    Src.M_G0     = R.M_G0;
    Src.LumClass = R.LumClass;
    Src.Template = Name;

    % 4.-5. per template: A_G and U - G0 on an E(B-V) grid
    io.files.load1(fullfile(I.getDataDir('ULTRASAT_Properties'), 'P90_UP_test_60_ZP_Var_Cern_21.mat'), 'UP');
    [Un, ~, J] = unique(Name);
    EG = (0:0.01:max(0.01, max(Src.Ebv)+0.01))';
    AG = NaN(numel(Un), numel(EG));
    UG = AG;
    SpecData = cell(numel(Un), 1);
    for It = 1:numel(Un)
        SP = io.files.load2(fullfile(Args.PicklesDir, "uk" + Un(It) + ".mat"));
        SpecData{It} = SP(:,1:2);
        Red = SP(:,2) .* 10.^(-0.4 .* astro.extinction.extinction(EG', SP(:,1)./1e4));  % [Nwave nE]
        GR  = astro.spec.synthetic_phot([SP(:,1) Red], 'GAIA', 'G', 'AB');
        G00 = GR(1);                                                                     % EG(1) = 0
        AG(It,:) = GR - G00;
        UG(It,:) = astro.spec.synthetic_phot([SP(:,1) Red], UP.U_AstFilt(1), 'R1', 'AB') - G00;
    end
    Src.G0_AB  = NaN(height(Src), 1);
    Src.U_pred = NaN(height(Src), 1);
    for It = 1:numel(Un)
        S = J == It;
        Src.G0_AB(S)  = Src.G(S) + Args.GABminusVega - interp1(EG, AG(It,:)', Src.Ebv(S));
        Src.U_pred(S) = Src.G0_AB(S) + interp1(EG, UG(It,:)', Src.Ebv(S));
    end
    Keep = Src.U_pred <= Args.MaxU;
    Say('%d stars with U_pred <= %.1f (of %d)', sum(Keep), Args.MaxU, height(Src));
    Src = Src(Keep, :);
    J   = J(Keep);

    % 6. usim with the AstroSpec array
    SimImage = [];
    if Args.Simulate
        Obj = AstroSpec(SpecData);
        for It = 1:numel(Un)
            Obj(It).ObjName = char(Un(It));
        end
        SimImage = ultrasat.usim('Cat', [Src.RA Src.Dec], 'SkyCat', true, 'Mag', Src.G0_AB, ...
                                 'FiltFam', {'GAIA'}, 'Filt', {'G'}, 'Spec', Obj(J), 'Ebv', Src.Ebv, ...
                                 'RA0', Args.RA0, 'Dec0', Args.Dec0, 'Tile', Args.Tile, ...
                                 'PlaneRotation', Args.PlaneRotation, 'Exposure', Args.Exposure, Args.UsimArgs{:});
    end
end

function [WCS, NX, NY] = tileWCS(RA0, Dec0, Tile, PlaneRotation)
    % the local TAN WCS of an ULTRASAT tile, as built in ultrasat.usim (keep in sync)
    NX = 4738;
    NY = 4738;
    PixSizeDeg = (9.5e-3 / 360) * 180/pi;
    Ngap = ceil(2.4 / 9.5e-3);
    switch Tile
        case 'A'
            CRPIX = [NX+Ngap/2, -Ngap/2];
        case 'B'
            CRPIX = [-Ngap/2, -Ngap/2];
        case 'C'
            CRPIX = [-Ngap/2, NY+Ngap/2];
        case 'D'
            CRPIX = [NX+Ngap/2, NY+Ngap/2];
        otherwise
            error('simulateGaiaField: invalid tile name %s', Tile);
    end
    WCS = AstroWCS();
    WCS.ProjType  = 'TAN';
    WCS.ProjClass = 'ZENITHAL';
    WCS.CooName   = {'RA'  'DEC'};
    WCS.CTYPE     = {'RA---TAN', 'DEC--TAN'};
    WCS.CUNIT     = {'deg', 'deg'};
    WCS.CD(1,1)   = PixSizeDeg;
    WCS.CD(2,2)   = PixSizeDeg;
    WCS.CRVAL(1)  = RA0;
    WCS.CRVAL(2)  = Dec0;
    WCS.CRPIX(1)  = CRPIX(1);
    WCS.CRPIX(2)  = CRPIX(2);
    WCS.populate_projMeta;
    A = deg2rad(PlaneRotation);
    WCS.CD = [cos(A), -sin(A); sin(A), cos(A)] * WCS.CD;
end
