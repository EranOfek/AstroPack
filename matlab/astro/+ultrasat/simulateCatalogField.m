function [SimImage, Src, Info] = simulateCatalogField(Args)
    % Simulate an ULTRASAT tile image of a sky field from a source catalog (default Gaia DR3; Pickles templates from the Gaia CMD)
    %   1. The stars in the tile: a catsHTM cone search of Catalog around the
    %      tile center (default GAIADR3), or a user table; the columns are
    %      found by ColNames; cut to the tile FOV with the same local WCS as
    %      ultrasat.usim builds from RA0, Dec0, Tile and PlaneRotation.
    %      The catalog must give a Gaia-like G magnitude, BP-RP color and
    %      parallax (the spectrum is chosen from them); a catalog without
    %      color or parallax (ColNames field '') can be used with one
    %      Template for all stars.
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
    %   6. ultrasat.usim with the AstroSpec array, G0 in GAIA/G, per-star
    %      E(B-V) and per-star PSF rotation angle.
    % PSF rotation: the lab PSF database is oriented along one radial
    %   direction; usim turns it by one angle per tile, which points the PSF
    %   flare away from the optical axis at the tile center. With
    %   PerStarRotation, each star gets RotAng + phi - phi_c, where phi is its
    %   azimuth around the tile inner corner (the point usim measures the PSF
    %   radius from) and phi_c that of the tile center, so the flare points
    %   along the star's own radial direction (up to 45 deg different at the
    %   tile corners).
    % Input  : * ...,key,val,...
    %            'RA0', 'Dec0' - [deg] telescope aimpoint (the [0 0] pixel of
    %                   the 4 tiles), as in ultrasat.usim. Default is the
    %                   Kepler field, 291, 44.5.
    %            'Tile'          - 'A', 'B', 'C' or 'D'. Default is 'B'.
    %            'PlaneRotation' - [deg] as in ultrasat.usim. Default is 0.
    %            'Exposure'      - [number duration(s)]. Default is [1 300].
    %            'RotAng' - [deg] PSF rotation offset, as in ultrasat.usim.
    %                   Default is 0.
    %            'PerStarRotation' - true (default) rotates each star's PSF
    %                   to its own radial direction (see above); false uses
    %                   usim's single angle per tile.
    %            'Catalog' - the source catalog: the name of a catsHTM
    %                   catalog (cone search around the tile; RA, Dec in rad
    %                   as in catsHTM), or a table (RA, Dec in deg). Default
    %                   is 'GAIADR3'.
    %            'ColNames' - structure: the catalog column of each quantity,
    %                   .RA, .Dec, .Mag (G, Vega), .Color (BP-RP), .Plx and
    %                   .ErrPlx [mas]; '' for a missing column (NaN). Default
    %                   is the GAIADR3 names: RA, Dec, phot_g_mean_mag, bp_rp,
    %                   Plx, ErrPlx.
    %            'Ebv' - [] (default): the EbvMap value at each star; or a
    %                   scalar or a vector of total line-of-sight E(B-V) per star.
    %            'EbvMap' - the 2-D E(B-V) map, as 'Map' of
    %                   astro.extinction.sky_ebv: 'SFD98' (default), 'CSFD23'
    %                   (Chang et al. 2023) or 'G24' (Gontcharov et al. 2024).
    %            'EbvMapArgs' - cell of further key,val for
    %                   astro.extinction.sky_ebv (e.g. {'Recalibrate',false}).
    %                   Default is {}.
    %            'ExtLaw' - the extinction law, as 'Model' of
    %                   astro.extinction.extinction: 'CCM89' (default) or 'F99'.
    %            'Rv' - R_V of the extinction law. Default is 3.08.
    %                   Both are passed to usim, so the wrapper and usim use
    %                   the same law. F99 is coded for R_V = 3.1 only.
    %            'MaxU' - drop stars with predicted U (AB, r=0) above this.
    %                   Default is 25.
    %            'MaxG' - drop stars with Gaia G (Vega, observed) above this,
    %                   before anything else is computed. Default is Inf.
    %            'Template' - [] (default): each star's Pickles template from
    %                   the Gaia CMD; or one template for all stars: a Pickles
    %                   name (e.g. "k2v", the file uk<name>.mat), or a Teff [K],
    %                   for which astro.stars.tlogg2picklesClass picks the
    %                   template with TemplateLogg. Each star keeps its own
    %                   G and E(B-V); only the spectral shape is common.
    %            'TemplateLogg' - log g used with a numeric Template.
    %                   Default is 4.5.
    %            'NoColor' - stars without BP-RP: 'median' (default) gives
    %                   them the field median observed BP-RP [PLACEHOLDER];
    %                   'drop' removes them. If no star has a color, a
    %                   Template is required.
    %            'GABminusVega' - G(AB) - G(Vega) of Gaia DR3. Default is
    %                   0.1136 (zero points 25.8010 and 25.6874, Riello et al.
    %                   2021). The synthetic value from astro.spec.synthetic_phot
    %                   is 0.023 mag larger: there Vega has 0 mag, in the Gaia
    %                   Vega system 0.023 mag.
    %            'MH' - [M/H] of the luminosity-class boundaries. Default is 0.
    %            'MagChange' - [] (default); or a table RA, Dec [deg], dMag: the
    %                   stars of the catalog at exactly these positions are
    %                   made dMag fainter (negative: brighter) in all bands;
    %                   applied after the MaxU cut.
    %            'ExtraSrc' - [] (default); or a table of added point sources
    %                   (e.g. transients) with X, Y [tile pixels] or RA, Dec
    %                   [deg], U [observed AB mag in the r=0 ULTRASAT filter,
    %                   as U_pred], and the spectrum: Teff [K] of a blackbody,
    %                   and/or Template, a Pickles name (e.g. "g2v"; a row
    %                   with Template "" or missing gets the blackbody). They
    %                   get no extinction (U is the observed magnitude) and
    %                   are not cut by MaxG or MaxU; sources outside the
    %                   tile are dropped.
    %            'ExtraWave' - [A] wavelength grid of the ExtraSrc blackbody
    %                   spectra; [] (default) is the grid of the Pickles
    %                   templates (usim needs one grid for all spectra).
    %            'BprpArgs' - cell of key,val for astro.stars.bprp2teff. Default is {}.
    %            'UsimArgs' - cell of further key,val for ultrasat.usim
    %                   (e.g. {'OutDir','/tmp','OutName','Kepler_B'}). Default is {}.
    %            'PicklesDir' - Pickles library directory. Default is from Installer.
    %            'GeometryOnly' - true returns only the tile geometry (third
    %                   output); SimImage and Src are empty. Default is false.
    %            'Simulate' - false stops after the catalog (SimImage = []).
    %                   Default is true.
    %            'Verbose' - Default is true.
    % Output : - The ultrasat.usim output (AstroImage), or [] if Simulate=false.
    %          - Table of the simulated stars: RA, Dec, G, BP_RP, NoColor,
    %            Plx, ErrPlx, EbvTot, Ebv (applied), BP_RP0, M_G0, LumClass,
    %            Template, G0_AB, U_pred, X, Y, RotAng (the angle passed to
    %            usim, before its per-tile offset). Stars of the FOV with
    %            U_pred > MaxU are not included. ExtraSrc rows come last, with
    %            Template the Pickles name or "bb<Teff>", and NaN in the Gaia
    %            columns.
    %          - Structure with the tile geometry: .WCS (AstroWCS), .NX, .NY.
    % Author : D. Kovaleva (Oct 2026)
    % Example: [Sim, Src] = ultrasat.simulateCatalogField('Tile','B','MaxU',24,'UsimArgs',{'OutDir','/tmp'});
    %          [~, Src] = ultrasat.simulateCatalogField('Simulate',false);   % catalog only
    %          % a user table with other column names
    %          CN = struct('RA','ra','Dec','dec','Mag','g','Color','bprp','Plx','parallax','ErrPlx','parallax_error');
    %          [~, Src] = ultrasat.simulateCatalogField('Catalog',T,'ColNames',CN,'Simulate',false);

    arguments
        Args.RA0              = 291;
        Args.Dec0             = 44.5;
        Args.Tile             = 'B';
        Args.PlaneRotation    = 0;
        Args.Exposure         = [1 300];
        Args.RotAng           = 0;
        Args.PerStarRotation logical = true;
        Args.Catalog          = 'GAIADR3';
        Args.ColNames struct  = struct('RA', 'RA', 'Dec', 'Dec', 'Mag', 'phot_g_mean_mag', 'Color', 'bp_rp', ...
                                       'Plx', 'Plx', 'ErrPlx', 'ErrPlx');
        Args.Ebv              = [];
        Args.EbvMap           = 'SFD98';
        Args.EbvMapArgs cell  = {};
        Args.ExtLaw           = 'CCM89';
        Args.Rv               = 3.08;
        Args.MaxU             = 25;
        Args.MaxG             = Inf;
        Args.Template         = [];
        Args.TemplateLogg     = 4.5;
        Args.NoColor          = 'median';
        Args.GABminusVega     = 0.1136;
        Args.MH               = 0;
        Args.MagChange        = [];
        Args.ExtraSrc         = [];
        Args.ExtraWave        = [];
        Args.BprpArgs cell    = {};
        Args.UsimArgs cell    = {};
        Args.PicklesDir       = [];
        Args.GeometryOnly logical = false;
        Args.Simulate logical = true;
        Args.Verbose logical  = true;
    end
    I = Installer;
    if isempty(Args.PicklesDir)
        Args.PicklesDir = I.getDataDir('PicklesStellarSpec');
    end
    Say = @(varargin) Args.Verbose && fprintf([varargin{1} '\n'], varargin{2:end});
    TemplateName = forcedTemplate(Args.Template, Args.TemplateLogg, Args.PicklesDir);
    if strcmpi(Args.ExtLaw, 'F99') && Args.Rv ~= 3.1
        error('simulateCatalogField: the F99 extinction law is coded for R_V = 3.1 only, Rv = %g given', Args.Rv);
    end

    % 1. the tile WCS (as in ultrasat.usim), its center and the stars in it
    [WCS, NX, NY] = tileWCS(Args.RA0, Args.Dec0, Args.Tile, Args.PlaneRotation);
    Info = struct('WCS', WCS, 'NX', NX, 'NY', NY);
    SimImage = [];
    Src      = table();
    if Args.GeometryOnly
        return
    end
    [CRA, CDec] = WCS.xy2sky((NX+1)/2, (NY+1)/2);
    [KRA, KDec] = WCS.xy2sky([0.5 NX+0.5 0.5 NX+0.5], [0.5 0.5 NY+0.5 NY+0.5]);
    Radius = rad2deg(max(celestial.coo.sphere_dist(CRA, CDec, KRA(:), KDec(:), 'deg'))) * 1.01; % [deg]
    C = Args.ColNames;
    if ischar(Args.Catalog) || isstring(Args.Catalog)   % a catsHTM catalog: RA, Dec in rad
        Say('Cone search of %s: center %.4f %+.4f, radius %.2f deg', Args.Catalog, CRA, CDec, Radius);
        [M, ColC] = catsHTM.cone_search(char(Args.Catalog), deg2rad(CRA), deg2rad(CDec), Radius*3600);
        T0 = array2table(M, 'VariableNames', ColC);
        T0.(C.RA)  = rad2deg(T0.(C.RA));
        T0.(C.Dec) = rad2deg(T0.(C.Dec));
    else
        T0 = Args.Catalog;
    end
    T = table();   % the internal (GAIADR3) names
    Names = {'RA', 'Dec', 'Mag', 'Color', 'Plx', 'ErrPlx'; 'RA', 'Dec', 'phot_g_mean_mag', 'bp_rp', 'Plx', 'ErrPlx'};
    for K = 1:size(Names, 2)
        Col = '';
        if isfield(C, Names{1,K})
            Col = C.(Names{1,K});
        end
        if isempty(Col)
            T.(Names{2,K}) = NaN(height(T0), 1);
        else
            T.(Names{2,K}) = double(T0.(Col));
        end
    end
    [X, Y] = WCS.sky2xy(T.RA, T.Dec);
    In = X > 0.1 & Y > 0.1 & X < NX & Y < NY & ~isnan(T.phot_g_mean_mag);  % the usim FOV test
    NinFOV = sum(In);
    In = In & T.phot_g_mean_mag <= Args.MaxG;
    Src = table(T.RA(In), T.Dec(In), T.phot_g_mean_mag(In), T.bp_rp(In), T.Plx(In), T.ErrPlx(In), X(In), Y(In), ...
                'VariableNames', {'RA', 'Dec', 'G', 'BP_RP', 'Plx', 'ErrPlx', 'X', 'Y'});
    Say('%d stars with G in the tile %s FOV (of %d in the cone), %d with G <= %.2f', NinFOV, Args.Tile, ...
        height(T), height(Src), Args.MaxG);

    % 2. E(B-V) and stars without color
    if isempty(Args.Ebv)
        Src.EbvTot = astro.extinction.sky_ebv(deg2rad(Src.RA), deg2rad(Src.Dec), 'eq', true, ...
                                              'Map', Args.EbvMap, Args.EbvMapArgs{:});
        Src.EbvTot = Src.EbvTot(:);
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
            if height(Src) > 0 && all(Src.NoColor)
                if strlength(TemplateName) == 0
                    error('simulateCatalogField: no star has a color; give one Template for all stars');
                end
                Src.BP_RP(:) = 0;   % used only for the CMD class, which the Template replaces
            else
                Src.BP_RP(Src.NoColor) = median(Src.BP_RP, 'omitnan');
            end
        case 'drop'
            Src = Src(~Src.NoColor, :);
        otherwise
            error('simulateCatalogField: NoColor must be ''median'' or ''drop''');
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
    if strlength(TemplateName) > 0
        Name(:) = TemplateName;
        Src.BP_RP0(Src.NoColor) = NaN;   % the color-based quantities of stars without a color
        Src.M_G0(Src.NoColor)   = NaN;
        Say('one template for all stars: %s', TemplateName);
    end
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
        Red = SP(:,2) .* 10.^(-0.4 .* astro.extinction.extinction(EG', SP(:,1)./1e4, [], Args.Rv, ...
                                                       'Model', Args.ExtLaw));               % [Nwave nE]
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

    % brightness changes of chosen stars
    if ~isempty(Args.MagChange)
        [Is, Loc] = ismember([Src.RA Src.Dec], [Args.MagChange.RA Args.MagChange.Dec], 'rows');
        Src.G0_AB(Is)  = Src.G0_AB(Is)  + Args.MagChange.dMag(Loc(Is));
        Src.U_pred(Is) = Src.U_pred(Is) + Args.MagChange.dMag(Loc(Is));
        Say('MagChange: %d of %d stars found', sum(Is), height(Args.MagChange));
    end

    % added sources: blackbody spectra normalized to the observed U, no extinction
    if ~isempty(Args.ExtraSrc)
        E = Args.ExtraSrc;
        if ismember('RA', E.Properties.VariableNames)
            [EX, EY] = WCS.sky2xy(E.RA, E.Dec);
            ERA = E.RA; EDec = E.Dec;
        else
            [ERA, EDec] = WCS.xy2sky(E.X, E.Y);
            EX = E.X; EY = E.Y;
        end
        InT = EX > 0.1 & EY > 0.1 & EX < NX & EY < NY;   % the usim FOV test
        E = E(InT, :); ERA = ERA(InT); EDec = EDec(InT); EX = EX(InT); EY = EY(InT);
        Wave = Args.ExtraWave;
        if isempty(Wave)   % the template grid: of the stars' spectra, or of any Pickles file
            if isempty(SpecData)
                Lst = dir(fullfile(Args.PicklesDir, 'uk*.mat'));
                SPw = io.files.load2(fullfile(Lst(1).folder, Lst(1).name));
            else
                SPw = SpecData{1};
            end
            Wave = SPw(:,1);
        end
        if ismember('Template', E.Properties.VariableNames)
            TName = lower(string(E.Template(:)));
        else
            TName = strings(height(E), 1);
        end
        IsBB = TName == "" | ismissing(TName);
        if any(IsBB)
            TName(IsBB) = "bb" + string(round(E.Teff(IsBB)));
        end
        [UnT, ~, JT] = unique(TName);
        UGt = NaN(numel(UnT), 1);
        for It = 1:numel(UnT)
            if startsWith(UnT(It), "bb")
                Tk = E.Teff(find(JT == It, 1));
                SP = [Wave(:), reshape(astro.spec.black_body(Tk, Wave(:)), [], 1)];
            else   % a Pickles template, on the common grid
                SP0 = io.files.load2(fullfile(Args.PicklesDir, "uk" + UnT(It) + ".mat"));
                SP  = [Wave(:), interp1(SP0(:,1), SP0(:,2), Wave(:), 'linear', 0)];
            end
            SpecData{end+1, 1} = SP; %#ok<AGROW>
            UGt(It) = astro.spec.synthetic_phot(SP, UP.U_AstFilt(1), 'R1', 'AB') - ...
                      astro.spec.synthetic_phot(SP, 'GAIA', 'G', 'AB');
        end
        Add = blankRows(Src, height(E));
        Add.RA = ERA(:); Add.Dec = EDec(:); Add.X = EX(:); Add.Y = EY(:);
        Add.EbvTot(:) = 0; Add.Ebv(:) = 0; Add.NoColor(:) = false;
        Add.Template = TName;
        Add.U_pred   = E.U(:);
        Add.G0_AB    = E.U(:) - UGt(JT);
        J   = [J; numel(Un) + JT];
        Un  = [Un; UnT];
        Src = [Src; Add];
        Say('%d added sources', height(E));
    end

    % per-star PSF rotation: azimuth around the tile inner corner, relative to the tile center
    Src.RotAng = Args.RotAng .* ones(height(Src), 1);
    if Args.PerStarRotation
        [X0, Y0] = innerCorner(Args.Tile, NX, NY);
        PhiC = atan2d((NY+1)/2 - Y0, (NX+1)/2 - X0);
        Src.RotAng = Src.RotAng + atan2d(Src.Y - Y0, Src.X - X0) - PhiC;
    end

    % 6. usim with the AstroSpec array
    if Args.Simulate
        Obj = AstroSpec(SpecData);
        for It = 1:numel(Un)
            Obj(It).ObjName = char(Un(It));
        end
        SimImage = ultrasat.usim('Cat', [Src.RA Src.Dec], 'SkyCat', true, 'Mag', Src.G0_AB, ...
                                 'FiltFam', {'GAIA'}, 'Filt', {'G'}, 'Spec', Obj(J), 'Ebv', Src.Ebv, ...
                                 'RA0', Args.RA0, 'Dec0', Args.Dec0, 'Tile', Args.Tile, ...
                                 'PlaneRotation', Args.PlaneRotation, 'Exposure', Args.Exposure, ...
                                 'RotAng', Src.RotAng, 'ExtLaw', Args.ExtLaw, 'Rv', Args.Rv, ...
                                 Args.UsimArgs{:});
    end
end

function Add = blankRows(Src, N)
    % N rows with the variables of Src: NaN, false, "" or empty
    Add = table();
    for V = string(Src.Properties.VariableNames)
        C = Src.(V);
        if islogical(C)
            Add.(V) = false(N, 1);
        elseif isnumeric(C)
            Add.(V) = NaN(N, 1);
        elseif isstring(C)
            Add.(V) = strings(N, 1);
        elseif iscategorical(C)
            Add.(V) = categorical(strings(N, 1));
        else
            Add.(V) = repmat({''}, N, 1);
        end
    end
end

function Name = forcedTemplate(Template, Logg, PicklesDir)
    % the Pickles template name forced on all stars, or "" for the CMD choice
    Name = "";
    if ~isempty(Template)
        if isnumeric(Template)
            R    = astro.stars.tlogg2picklesClass(Template, Logg);
            Name = string(R.class) + string(R.lumclass);
        else
            Name = lower(string(Template));
        end
        if ~isfile(fullfile(PicklesDir, "uk" + Name + ".mat"))
            error('simulateCatalogField: no Pickles template %s in %s', Name, PicklesDir);
        end
    end
end

function [X0, Y0] = innerCorner(Tile, NX, NY)
    % the tile corner that usim measures the PSF radius from (keep in sync)
    switch Tile
        case 'A'
            X0 = NX + 0.5; Y0 = 0.5;
        case 'B'
            X0 = 0.5;      Y0 = 0.5;
        case 'C'
            X0 = 0.5;      Y0 = NY + 0.5;
        case 'D'
            X0 = NX + 0.5; Y0 = NY + 0.5;
        otherwise
            error('simulateCatalogField: invalid tile name %s', Tile);
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
            error('simulateCatalogField: invalid tile name %s', Tile);
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
