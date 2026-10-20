function [Name, Res, Spec] = gaia2picklesClass(BP_RP, Args)
    % Pickles template per star from Gaia BP-RP, G and parallax (luminosity class from the CMD)
    %   1. Dereddened color BP_RP0, absolute G M_G0 and the parallax quality
    %      come from astro.stars.bprp2teff (same reddening model and arguments).
    %   2. dM = M_G,MS(BP_RP0) - M_G0 is the height above the EEM main sequence.
    %   3. Luminosity class from dM against PARSEC-isochrone boundaries at the
    %      star's color (astro.stars.buildLumClassBoundaries):
    %        V   - dM < V_IV, or no usable parallax;
    %        IV  - V_IV <= dM < IV_III;
    %        III - dM >= IV_III;
    %        I   - dM >= V_IV and M_G0 brighter than the faintest evolved
    %              massive (Mini >= 8 Msun) isochrone star at that color.
    %      Class II is not assigned.
    %   4. Spectral type: the solar-abundance Pickles template of that class
    %      with the nearest synthetic BP-RP (Vega) to BP_RP0.
    %   astro.stars.tlogg2picklesClass is not used.
    % Input  : - Observed Gaia BP-RP [mag] (vector).
    %          * ...,key,val,...
    %            'G', 'Plx', 'PlxErr', 'Ebv', 'GalLat' - as in astro.stars.bprp2teff.
    %            'MH'  - [M/H] of the boundary grid. Default is 0 (solar).
    %                   Other grids in the file (-1.0, -0.5, +0.25) are
    %                   available but not validated [PLACEHOLDER].
    %            'BoundaryFile' - output of astro.stars.buildLumClassBoundaries.
    %                   Default is <AstroPack data path>/stars/PARSEC/lumclass_boundaries_parsec.mat.
    %            'BprpArgs' - cell of further key,val arguments for
    %                   astro.stars.bprp2teff (e.g. {'MinPlxSN',10}). Default is {}.
    %            'PicklesDir' - Pickles library directory (only for the third
    %                   output). Default is from Installer.
    % Output : - String array of Pickles template names (e.g. "g2v", "k0iii");
    %            "" where BP-RP is NaN.
    %          - Structure with per-star columns: .LumClass, .DeltaM, .V_IV,
    %            .IV_III, .Teff, .Logg (from bprp2teff), and the bprp2teff
    %            structure fields (.BP_RP0, .Ebv, .M_G0, .M_G_MS, .PlxOK, ...).
    %          - AstroSpec handle array, one element per star, for
    %            ultrasat.usim('Spec',...). Elements of the same template share
    %            one object. Requires all BP-RP to be finite.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Name = astro.stars.gaia2picklesClass([0.82 1.2], 'G',[12 10], 'Plx',[10 1], 'PlxErr',[0.02 0.02])
    %          [Name,Res,Spec] = astro.stars.gaia2picklesClass(BP_RP, 'G',G, 'Plx',Plx, 'PlxErr',PlxErr, 'Ebv',Ebv, 'GalLat',B);

    arguments
        BP_RP
        Args.G                    = [];
        Args.Plx                  = [];
        Args.PlxErr               = [];
        Args.Ebv                  = 0;
        Args.GalLat               = [];
        Args.MH                   = 0;
        Args.BoundaryFile         = fullfile(tools.os.getAstroPackDataPath, 'stars', 'PARSEC', 'lumclass_boundaries_parsec.mat');
        Args.BprpArgs cell        = {};
        Args.PicklesDir           = [];
    end
    persistent Cache
    if isempty(Cache) || ~strcmp(Cache.File, Args.BoundaryFile)
        if ~isfile(Args.BoundaryFile)
            error('gaia2picklesClass: boundary file %s not found; run astro.stars.buildLumClassBoundaries', Args.BoundaryFile);
        end
        Cache      = load(Args.BoundaryFile);
        Cache.File = Args.BoundaryFile;
    end
    IG = find(abs([Cache.Grid.MH] - Args.MH) < 1e-6, 1);
    if isempty(IG)
        error('gaia2picklesClass: no boundary grid for [M/H]=%g (available: %s)', Args.MH, mat2str([Cache.Grid.MH]));
    end
    Grid = Cache.Grid(IG);
    Tmpl = Cache.Templates;

    [Teff, Logg, Res] = astro.stars.bprp2teff(BP_RP, 'G', Args.G, 'Plx', Args.Plx, 'PlxErr', Args.PlxErr, ...
                                              'Ebv', Args.Ebv, 'GalLat', Args.GalLat, Args.BprpArgs{:});
    C   = min(max(Res.BP_RP0, Grid.Color(1)), Grid.Color(end));  % boundaries are flat beyond the grid
    VIV   = interp1(Grid.Color, Grid.V_IV,         C, 'linear');
    IVIII = interp1(Grid.Color, Grid.IV_III,       C, 'linear');
    SupMG = interp1(Grid.Color, Grid.SupergiantMG, C, 'linear');
    DM    = Res.M_G_MS - Res.M_G0;                                % NaN without a usable parallax

    N   = numel(C);
    Lum = repmat("v", N, 1);
    Lum(Res.PlxOK & DM >= VIV)                     = "iv";
    Lum(Res.PlxOK & DM >= IVIII)                   = "iii";
    Lum(Res.PlxOK & DM >= VIV & Res.M_G0 < SupMG)  = "i";

    % nearest synthetic color among the templates of the star's class
    Name = strings(N, 1);
    for L = ["v", "iv", "iii", "i"]
        Sel = Lum == L & ~isnan(Res.BP_RP0);
        if any(Sel)
            T = Tmpl(Tmpl.LumClass == L, :);
            [Cs, Iu] = unique(T.BP_RP);
            Idx = interp1(Cs, Iu, Res.BP_RP0(Sel), 'nearest', 'extrap');
            Name(Sel) = T.Name(Idx);
        end
    end

    Res.LumClass = Lum;
    Res.DeltaM   = DM;
    Res.V_IV     = VIV;
    Res.IV_III   = IVIII;
    Res.Teff     = Teff;
    Res.Logg     = Logg;

    if nargout > 2
        if any(Name == "")
            error('astro:stars:gaia2picklesClass:NaNColor', ...
                  'gaia2picklesClass: %d stars have no BP-RP; remove them before building spectra', sum(Name == ""));
        end
        if isempty(Args.PicklesDir)
            I = Installer;
            Args.PicklesDir = I.getDataDir('PicklesStellarSpec');
        end
        [Un, ~, J] = unique(Name);
        Obj = AstroSpec(arrayfun(@(U) io.files.load2(fullfile(Args.PicklesDir, "uk" + U + ".mat")), Un, 'UniformOutput', false));
        for I = 1:numel(Un)
            Obj(I).ObjName = char(Un(I));
        end
        Spec = Obj(J);
    end
end
