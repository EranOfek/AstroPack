function B = buildLumClassBoundaries(Args)
    % Build luminosity-class boundaries in the Gaia CMD from PARSEC isochrones (for gaia2picklesClass)
    %   For each [M/H] grid, the PARSEC isochrone points are binned in (BP-RP),
    %   and in each bin the height above the EEM main sequence,
    %   dM = M_G,MS(BP-RP) - M_G, is measured for the evolutionary phases
    %   (label 1 = core H burning, 2 = subgiant branch, 3 = red giant branch,
    %   4-6 = core He burning). Only isochrones of age >= 10^MinLogAge yr and
    %   initial masses < MaxMini are used for the V/IV/III boundaries (field
    %   population); evolved stars with Mini >= MaxMini set the supergiant limit.
    %   Boundaries (per color bin, then smoothed with a 3-bin running median):
    %     V/IV   = max( min(top of MS, bottom of SGB), BinaryFloor );
    %              in bins without a subgiant branch below the giants
    %              (red colors) there is no class IV: V/IV = IV/III.
    %     IV/III = min(bottom of RGB, bottom of core He burning), not below V/IV;
    %              NoGiant (99) where neither exists (blue colors).
    %   SupergiantMG = M_G of the faintest evolved (label >= 2) star with
    %   Mini >= MaxMini, per bin (gaps filled from the nearest bin).
    %   Also tabulates the synthetic Gaia BP-RP (Vega) of the solar-abundance
    %   Pickles templates, per luminosity class.
    % Input  : * ...,key,val,...
    %            'IsoDir'  - directory of the PARSEC files
    %                   parsec_v1.2S_gaiaEDR3_MH<+-x.xx>.dat. Default is
    %                   <AstroPack data path>/stars/PARSEC.
    %            'OutFile' - output .mat file; '' does not save. Default is
    %                   <IsoDir>/lumclass_boundaries_parsec.mat.
    %            'ColorEdges'  - BP-RP bin edges. Default is -0.4:0.1:3.5.
    %            'MinLogAge'   - minimal log10(age/yr). Default is 8.
    %            'MaxMini'     - maximal initial mass [Msun] of V/IV/III stars. Default is 8.
    %            'BinaryFloor' - minimal V/IV height [mag] (equal-mass binaries are
    %                   0.75 mag above the MS). Default is 0.75.
    %            'PicklesDir'  - Pickles library directory. Default is from Installer.
    % Output : - Structure with .Grid(i) per [M/H] (.MH, .Color, .V_IV, .IV_III,
    %            .SupergiantMG, and the raw per-bin statistics), .Templates
    %            (table Name, Class, LumClass, BP_RP) and .Settings.
    % Author : D. Kovaleva (Oct 2026)
    % Example: B = astro.stars.buildLumClassBoundaries;

    arguments
        Args.IsoDir           = fullfile(tools.os.getAstroPackDataPath, 'stars', 'PARSEC');
        Args.OutFile          = [];
        Args.ColorEdges       = -0.4:0.1:3.5;
        Args.MinLogAge        = 8;
        Args.MaxMini          = 8;
        Args.BinaryFloor      = 0.75;
        Args.PicklesDir       = [];
    end
    if isempty(Args.OutFile)
        Args.OutFile = fullfile(Args.IsoDir, 'lumclass_boundaries_parsec.mat');
    end
    if isempty(Args.PicklesDir)
        I = Installer;
        Args.PicklesDir = I.getDataDir('PicklesStellarSpec');
    end
    NoGiant = 99;

    Edge = Args.ColorEdges(:);
    Ctr  = (Edge(1:end-1) + Edge(2:end)) ./ 2;
    Nb   = numel(Ctr);
    [~, ~, R] = astro.stars.bprp2teff(Ctr);
    MS_G = R.M_G_MS;

    Files = dir(fullfile(Args.IsoDir, 'parsec_v1.2S_gaiaEDR3_MH*.dat'));
    if isempty(Files)
        error('buildLumClassBoundaries: no PARSEC files in %s', Args.IsoDir);
    end
    for If = 1:numel(Files)
        MH  = sscanf(erase(Files(If).name, 'parsec_v1.2S_gaiaEDR3_MH'), '%f');
        T   = readmatrix(fullfile(Files(If).folder, Files(If).name), 'FileType', 'text', 'CommentStyle', '#');
        LogAge = T(:,3);
        Mini   = T(:,4);
        Label  = T(:,10);
        G      = T(:,end-2);
        Color  = T(:,end-1) - T(:,end);
        Bin    = discretize(Color, Edge);
        Field  = Mini < Args.MaxMini & LogAge >= Args.MinLogAge & ~isnan(Bin);
        DM     = NaN(size(G));
        DM(~isnan(Bin)) = MS_G(Bin(~isnan(Bin))) - G(~isnan(Bin));

        Stat = @(Sel, Fun) accumarray(Bin(Sel), DM(Sel), [Nb 1], Fun, NaN);
        MStop   = Stat(Field & Label == 1, @max);
        SGBbot  = Stat(Field & Label == 2, @min);
        RGBbot  = Stat(Field & Label == 3, @min);
        CHeBbot = Stat(Field & Label >= 4 & Label <= 6, @min);
        Sup     = ~isnan(Bin) & Mini >= Args.MaxMini & Label >= 2;
        SupMG   = accumarray(Bin(Sup), G(Sup), [Nb 1], @max, NaN);

        Giant  = min(RGBbot, CHeBbot);                           % min() ignores NaN
        HasSGB = SGBbot < Giant | (~isnan(SGBbot) & isnan(Giant)); % a subgiant branch below the giants
        MSIV   = MStop;
        MSIV(HasSGB) = min(MStop(HasSGB), SGBbot(HasSGB));
        VIV   = movmedian(MSIV, 3, 'omitnan');
        VIV   = max(VIV, Args.BinaryFloor);                      % NaN -> floor
        IVIII = movmedian(Giant, 3, 'omitnan');
        IVIII(isnan(IVIII)) = NoGiant;
        IVIII = max(IVIII, VIV);
        VIV(~HasSGB) = IVIII(~HasSGB);                           % no class IV without a subgiant branch

        Grid(If).MH           = MH; %#ok<AGROW>
        Grid(If).Color        = Ctr;
        Grid(If).V_IV         = VIV;
        Grid(If).IV_III       = IVIII;
        Grid(If).SupergiantMG = fillmissing(movmedian(SupMG, 3, 'omitnan'), 'nearest'); % faintest evolved massive star per bin
        Grid(If).MStop        = MStop;
        Grid(If).SGBbottom    = SGBbot;
        Grid(If).RGBbottom    = RGBbot;
        Grid(If).CHeBbottom   = CHeBbot;
        Grid(If).File         = Files(If).name;
    end
    [~, Is] = sort([Grid.MH]);
    B.Grid = Grid(Is);

    % synthetic BP-RP of the solar-abundance Pickles templates
    List = dir(fullfile(Args.PicklesDir, 'uk*.mat'));
    Name = erase(string({List.name}'), [".mat", "uk"]);
    Tok  = regexp(Name, '^([obafgkm][0-9]*)(i|ii|iii|iv|v)$', 'tokens', 'once');
    Ok   = ~cellfun(@isempty, Tok);
    Name = Name(Ok);
    Tok  = vertcat(Tok{Ok});
    BP_RP = NaN(numel(Name), 1);
    for I = 1:numel(Name)
        SP = io.files.load2(fullfile(Args.PicklesDir, "uk" + Name(I) + ".mat"));
        BP_RP(I) = astro.spec.synthetic_phot(SP(:,1:2), 'GAIA', 'BP', 'Vega') - ...
                   astro.spec.synthetic_phot(SP(:,1:2), 'GAIA', 'RP', 'Vega');
    end
    B.Templates = table(Name, string(Tok(:,1)), string(Tok(:,2)), BP_RP, ...
                        'VariableNames', {'Name', 'Class', 'LumClass', 'BP_RP'});

    B.Settings = rmfield(Args, 'PicklesDir');
    B.Settings.NoGiant = NoGiant;
    B.Settings.Built   = string(datetime('now', 'Format', 'yyyy-MM-dd HH:mm'));
    B.Settings.Note    = "dM = M_G,MS(EEM) - M_G; V/IV = max(min(MS top, SGB bottom), BinaryFloor); " + ...
                         "IV/III = min(RGB bottom, CHeB bottom), NoGiant if none; PARSEC v1.2S, Gaia EDR3, Vega";
    if strlength(string(Args.OutFile)) > 0
        save(Args.OutFile, '-struct', 'B');
    end
end
