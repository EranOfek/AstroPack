function Result = plotPhotScatter(MS, Args)
    % Plot per-source RMS vs median for one or more catalog quantities
    % Description: For each named quantity Q, collects per source the
    %              median of a reference x-axis column and the std of Q
    %              across epochs, then plots Std vs Median with optional
    %              binned trend lines.
    %
    %              Quantities are organised in 'QuantityGroups'; each group
    %              becomes one figure with one panel per quantity in the
    %              group. The default 3-group layout is:
    %                  Group 1  : RA, Dec               (astrometry)
    %                  Group 2  : MAG_PSF, MAG_APER_3   (relative photometry)
    %                  Group 3  : MAG_AB_PSF,           (calibrated photometry —
    %                             MAG_AB_APER_3          empty for non-coadd MS,
    %                                                    silently skipped when
    %                                                    SkipEmptyGroups=true)
    %
    %              Two quantity types:
    %                * Magnitude quantities (anything not in
    %                  AngularQuantities). x = y = quantity (the classic
    %                  "mag vs mag-RMS" plot).
    %                * Angular quantities (default {'RA','Dec'}). x =
    %                  median(RefMag) (default 'MAG_APER_3'); y =
    %                  std(quantity) computed on the raw tabulated values
    %                  (no per-row transformation), then rescaled by 3600
    %                  to display in arcsec. The underlying RA/Dec table
    %                  values are NOT modified; only the derived std is
    %                  rescaled for the y axis.
    %                  Additionally for Q=='RA', the per-source std is
    %                  multiplied by cos(median Dec) to express it as a
    %                  projected angular separation on the sky (rather
    %                  than raw RA-coordinate spread).
    %                  The faint cut ('BackgroundMag') is applied on RefMag.
    %
    % Input  : - MS, in one of three shapes:
    %              1) MatchedSources array (one element per crop), as
    %                 returned by pipeline.last.load.loadVisit. No
    %                 'Modes' arg needed — the array is treated as a
    %                 single anonymous mode.
    %              2) Cell array of MatchedSources (rare).
    %              3) Struct with mode-named cell fields, MS.(mode){crop}
    %                 = MatchedSources (from matchPhotEpochs or
    %                 pipeline.last.load.loadMergedMat). 'Modes' selects
    %                 which fields to plot.
    %          * ...,key,val,...
    %            'Modes'        - Cell of MS sub-modes (loadMergedMat path
    %                             only). Default {'percrop'}.
    %            'QuantityGroups' - Cell of cells of column names. Each
    %                             inner cell is one figure (1+ panels).
    %                             Default = the 3-group preset described
    %                             above. Supersedes 'Quantities'.
    %            'Quantities'   - Flat cell of column names. When set
    %                             (without QuantityGroups), each quantity
    %                             becomes its own one-panel figure. Default {}.
    %            'MagFields'    - Legacy alias for 'Quantities'. Default {}.
    %            'AngularQuantities' - Names that should use the angular
    %                             treatment (x = RefMag, std rescaled to
    %                             arcsec). Default {'RA','Dec'}.
    %            'RefMag'       - Reference mag column for angular x-axis
    %                             AND for the faint-cut filter on angular
    %                             quantities. Default 'MAG_APER_3'.
    %            'LayoutMode'   - 'PerQuantity' (default) | 'Combined'.
    %                             'PerQuantity' opens one figure per group.
    %                             'Combined' opens a single figure with
    %                             one subplot per quantity (groups ignored).
    %            'SkipEmptyGroups' - Drop groups whose every quantity has
    %                             no finite data (so empty AB-mag panels
    %                             never appear when MS lacks AB columns).
    %                             Default true.
    %            'CropsToAnalyze' - Crop indices. Default [] (all).
    %            'OverlayTrend' - 'median' | 'mean' | 'none'. Default 'median'.
    %            'TrendBinWidth' - Bin width [mag]. Default 0.5.
    %            'MinEpochs'    - Min non-NaN epochs per source. Default 5.
    %            'FilterFlags'  - FLAGS bit names to NaN out per epoch.
    %                             Default {'Saturated','NearEdge','NaN'}.
    %            'BackgroundMag' - Faint cut applied on the reference-mag
    %                              column (= the quantity itself for mag
    %                              quantities, RefMag for angular).
    %                              Default 22.
    %            'ColorByCrop'  - Color sources by crop ID. Default false.
    %            'CentralEdge'  - Distinguish central vs edge crops by
    %                             shade. Default false.
    %            'TileOrder'    - For CentralEdge: 'colmajor'|'rowmajor'.
    %                             Default 'rowmajor'.
    % Output : - Result struct with fields:
    %            .Quantities    - 1xK cell of quantity names actually used
    %                             (after empty-group pruning).
    %            .Groups        - cell of cells of names actually rendered.
    %            .LayoutMode    - layout used.
    %            .RefMag        - reference mag used for angular quantities.
    %            .PerQuantity   - 1xK struct array, each entry:
    %                .Quantity      - name (char)
    %                .IsAngular     - logical
    %                .RefMag        - x-axis column name actually used
    %                .CropsAnalyzed - 1xNmodes cell of crop index vectors
    %                .PerCrop       - {Imode}{Iic}.Med .Std
    %                .AllMed,.AllStd - 1xNmodes cell, pooled across crops
    % Author : D. Kovaleva (Mar 2026; angular support Apr 2026; group layout Apr 2026)
    % Example: % --- loadVisit users (typical) ----------------------------
    %          % Load only the MatchedSources of a visit (skip images/coadd):
    %          VisitDir = '/archimedes/LASTunitTest/2025/04/26/proc/002712v0';
    %          [~, ~, MS] = pipeline.last.load.loadVisit(VisitDir, ...
    %              'TempName_IndivIm', [], 'TempName_Coadd', []);
    %
    %          % Default 3-group layout: figures for {RA,Dec},
    %          % {MAG_PSF,MAG_APER_3}, and {MAG_AB_PSF,MAG_AB_APER_3}.
    %          % AB group is auto-skipped when MS has no AB columns.
    %          R = pipeline.last.quality.photCalib.plotPhotScatter(MS);
    %
    %          % Custom grouping — e.g. one figure per quantity:
    %          R = pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
    %                  'QuantityGroups', { ...
    %                      {'MAG_PSF'}, {'MAG_APER_3'}, ...
    %                      {'RA'}, {'Dec'}});
    %
    %          % Or via the flat 'Quantities' shortcut (one figure each):
    %          R = pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
    %                  'Quantities', {'MAG_PSF','MAG_APER_3'});
    %
    %          % All groups in one combined figure (groups flattened):
    %          R = pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
    %                  'LayoutMode', 'Combined');
    %
    %          % Restrict to two crops (e.g. crop 1 and crop 10) and color
    %          % sources by crop:
    %          R = pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
    %                  'CropsToAnalyze', [1 10], 'ColorByCrop', true);
    %
    %          % Tighten the source selection: at least 10 epochs after
    %          % flag filtering, faint cut at MAG_APER_3 = 20:
    %          R = pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
    %                  'MinEpochs', 10, 'BackgroundMag', 20);
    %
    %          % Use MAG_PSF (not MAG_APER_3) as RA/Dec x-axis reference:
    %          R = pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
    %                  'RefMag', 'MAG_PSF');
    %
    %          % Force the AB group to render (even if MS lacks AB columns):
    %          R = pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
    %                  'SkipEmptyGroups', false);
    %
    %          % --- loadMergedMat users (legacy, mode-keyed struct) ------
    %          MS = pipeline.last.load.loadMergedMat('MergedMatDir', VisitDir);
    %          pipeline.last.quality.photCalib.plotPhotScatter(MS, ...
    %                  'Modes', {'percrop'});

    arguments
        MS
        Args.Modes                  cell                = {'percrop'}
        Args.Quantities             cell                = {}
        Args.MagFields              cell                = {}
        Args.QuantityGroups         cell                = {}
        Args.AngularQuantities      cell                = {'RA', 'Dec'}
        Args.RefMag                 (1,:) char          = 'MAG_APER_3'
        Args.LayoutMode             (1,:) char ...
            {mustBeMember(Args.LayoutMode, {'PerQuantity','Combined'})} = 'PerQuantity'
        Args.SkipEmptyGroups        logical             = true
        Args.CropsToAnalyze                             = []
        Args.OverlayTrend                               = 'median'
        Args.TrendBinWidth                              = 0.5
        Args.MinEpochs                                  = 2
        Args.FilterFlags            cell                = {'Saturated', 'NearEdge', 'NaN'}
        Args.BackgroundMag                              = 22
        Args.ColorByCrop            logical             = false
        Args.CentralEdge            logical             = false
        Args.TileOrder                                  = 'rowmajor'
    end

    % --- Normalize MS input ---------------------------------------------
    %   1) MatchedSources array (loadVisit) → wrap as single-mode struct
    %   2) Cell of MatchedSources              → same
    %   3) Struct with mode fields (loadMergedMat) → use as-is
    if isa(MS, 'MatchedSources')
        MS = struct('msdata', {num2cell(MS(:).')});
        Args.Modes = {'msdata'};
    elseif iscell(MS)
        MS = struct('msdata', {MS(:).'});
        Args.Modes = {'msdata'};
    elseif ~isstruct(MS)
        error('plotPhotScatter:BadInput', ...
            'MS must be a MatchedSources array, a cell of MatchedSources, or a mode-keyed struct.');
    end

    % --- Resolve quantity groups ----------------------------------------
    %   Priority: explicit QuantityGroups > Quantities flat list > default 3-group
    %   layout (astrometry, relative photometry, AB photometry).
    if ~isempty(Args.QuantityGroups)
        Groups = Args.QuantityGroups;
    elseif ~isempty(Args.Quantities)
        Groups = cellfun(@(q){{q}}, Args.Quantities);  % one group per quantity
    elseif ~isempty(Args.MagFields)
        Groups = cellfun(@(q){{q}}, Args.MagFields);
    else
        Groups = { ...
            {'RA', 'Dec'}, ...
            {'MAG_PSF', 'MAG_APER_3'}, ...
            {'MAG_AB_PSF', 'MAG_AB_APER_3'} ...
        };
    end
    % Flat de-duplicated quantity list across groups.
    Quantities = unique([Groups{:}], 'stable');

    Nmodes = numel(Args.Modes);

    % --- Central crops for CentralEdge mode -----------------------------
    switch lower(Args.TileOrder)
        case 'colmajor', CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor', CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise,        CentralCrops = [];
    end

    % --- Extract data once per quantity ---------------------------------
    DataByQ = struct();
    for Iq = 1:numel(Quantities)
        FName = matlab.lang.makeValidName(Quantities{Iq});
        DataByQ.(FName) = extractQuantity(MS, Quantities{Iq}, Args);
    end

    % --- Drop groups with no data (when SkipEmptyGroups=true) ------------
    if Args.SkipEmptyGroups
        Keep = false(1, numel(Groups));
        for Ig = 1:numel(Groups)
            for Iq = 1:numel(Groups{Ig})
                FName = matlab.lang.makeValidName(Groups{Ig}{Iq});
                D = DataByQ.(FName);
                for Im = 1:numel(D.AllMed)
                    if ~isempty(D.AllMed{Im}); Keep(Ig) = true; break; end
                end
                if Keep(Ig); break; end
            end
        end
        Groups = Groups(Keep);
    end

    % --- Render ----------------------------------------------------------
    if strcmpi(Args.LayoutMode, 'Combined')
        renderCombined(Quantities, DataByQ, Args, CentralCrops);
    else
        for Ig = 1:numel(Groups)
            renderGroup(Groups{Ig}, DataByQ, Args, CentralCrops, Nmodes);
        end
    end

    % --- Pack output -----------------------------------------------------
    PerQuantity = repmat(struct('Quantity','', 'IsAngular',false, ...
        'RefMag','', 'CropsAnalyzed',{cell(1,Nmodes)}, ...
        'PerCrop',{cell(1,Nmodes)}, ...
        'AllMed',{cell(1,Nmodes)}, 'AllStd',{cell(1,Nmodes)}), 1, numel(Quantities));
    for Iq = 1:numel(Quantities)
        FName = matlab.lang.makeValidName(Quantities{Iq});
        D = DataByQ.(FName);
        PerQuantity(Iq).Quantity      = D.Quantity;
        PerQuantity(Iq).IsAngular     = D.IsAngular;
        PerQuantity(Iq).RefMag        = D.RefMag;
        PerQuantity(Iq).CropsAnalyzed = D.CropsAnalyzed;
        PerQuantity(Iq).PerCrop       = D.PerCrop;
        PerQuantity(Iq).AllMed        = D.AllMed;
        PerQuantity(Iq).AllStd        = D.AllStd;
    end

    Result.Quantities  = Quantities;
    Result.LayoutMode  = Args.LayoutMode;
    Result.RefMag      = Args.RefMag;
    Result.Groups      = Groups;
    Result.PerQuantity = PerQuantity;
end

% =========================================================================
function renderGroup(QNames, DataByQ, Args, CentralCrops, Nmodes)
    % Render one figure for a group of quantities.
    %   One panel per quantity (per mode). No relative-vs-calibrated split.
    QNames = QNames(:).';   % row cell
    NPanelsPerMode = numel(QNames);
    Npanels = NPanelsPerMode * Nmodes;

    figure('Name', sprintf('Mag vs Std — %s', strjoin(QNames, ' / ')), ...
           'Position', [50, 50, 400*Npanels, 500]);

    for Im = 1:Nmodes
        for Iqp = 1:numel(QNames)
            Q = QNames{Iqp};
            FName = matlab.lang.makeValidName(Q);
            D = DataByQ.(FName);
            PanelIdx = (Im - 1) * NPanelsPerMode + Iqp;
            Ax = subplot(1, Npanels, PanelIdx); hold(Ax, 'on');
            if D.IsAngular
                drawAngularPanel(Ax, Q, DataByQ, Im, Args, CentralCrops);
            else
                drawMagPanel(Ax, Q, DataByQ, Im, Args, CentralCrops);
                title(Ax, Q, 'Interpreter', 'none');
            end
        end
    end
    sgtitle(sprintf('Epoch-to-epoch scatter: %s', strjoin(QNames, ' / ')), ...
        'FontSize', 11, 'Interpreter', 'none');
    for Isub = 1:Npanels
        Ax = subplot(1, Npanels, Isub);
        Ax.Position(4) = Ax.Position(4) * 0.88;
    end
end

% =========================================================================
function D = extractQuantity(MS, Q, Args)
    % Extract per-(mode, crop) (Med, Std) data for a single quantity.
    %   For angular quantities (RA/Dec), x = median(RefMag); for mag
    %   quantities, x = y = quantity. Std is computed on raw tabulated
    %   values; for angular quantities it is then rescaled by 3600 (deg
    %   → arcsec) for display.
    IsAng = ismember(Q, Args.AngularQuantities);
    if IsAng
        RefMag = Args.RefMag;
    else
        RefMag = Q;
    end

    Modes = Args.Modes;
    Nmodes = numel(Modes);

    D.Quantity      = Q;
    D.IsAngular     = IsAng;
    D.RefMag        = RefMag;
    D.CropsAnalyzed = cell(1, Nmodes);
    D.PerCrop       = cell(1, Nmodes);
    D.AllMed        = cell(1, Nmodes);
    D.AllStd        = cell(1, Nmodes);

    for Im = 1:Nmodes
        Mode = Modes{Im};
        if ~isfield(MS, Mode); continue; end
        MSchunks = MS.(Mode);

        if isempty(Args.CropsToAnalyze)
            CropsToUse = 1:numel(MSchunks);
        else
            CropsToUse = Args.CropsToAnalyze;
        end
        D.CropsAnalyzed{Im} = CropsToUse;

        PerCropM = cell(numel(CropsToUse), 1);
        AllM = []; AllS = [];

        for Iic = 1:numel(CropsToUse)
            Ic = CropsToUse(Iic);
            if Ic > numel(MSchunks) || isempty(MSchunks{Ic}); continue; end
            MSobj = MSchunks{Ic};

            BadEpochMask = buildBadEpochMask(MSobj, RefMag, Args);

            % For RA: scale std by cos(median Dec) per source (projected
            % angular separation on sky, not raw RA-coordinate spread).
            ScaleCosCol = '';
            if IsAng && strcmpi(Q, 'RA')
                ScaleCosCol = 'Dec';
            end

            S = collectMedStd(MSobj, Q, RefMag, BadEpochMask, Args, ScaleCosCol);
            if ~isempty(S)
                if IsAng
                    S.Std = S.Std * 3600;   % deg → arcsec for display
                end
                PerCropM{Iic} = S;
                AllM = [AllM, S.Med]; %#ok<AGROW>
                AllS = [AllS, S.Std]; %#ok<AGROW>
            end
        end

        D.PerCrop{Im} = PerCropM;
        D.AllMed{Im}  = AllM;
        D.AllStd{Im}  = AllS;
    end
end

% =========================================================================
function Mask = buildBadEpochMask(MSobj, SizeRefCol, Args)
    % Build per-epoch bad-source mask [Nepochs x Nsrc] from FLAGS bits.
    if isfield(MSobj.Data, SizeRefCol)
        Mask = false(size(MSobj.Data.(SizeRefCol)));
    elseif isfield(MSobj.Data, 'FLAGS')
        Mask = false(size(MSobj.Data.FLAGS));
    else
        F = fieldnames(MSobj.Data);
        Mask = false(size(MSobj.Data.(F{1})));
    end
    if ~isempty(Args.FilterFlags) && isfield(MSobj.Data, 'FLAGS')
        FlagMat = MSobj.Data.FLAGS;
        FlagMat(isnan(FlagMat)) = 0;
        try
            BD = BitDictionary;
            for Ifl = 1:numel(Args.FilterFlags)
                [~, ~, BitDec] = BD.name2bit(Args.FilterFlags{Ifl});
                Mask = Mask | (bitand(uint32(FlagMat), uint32(BitDec)) > 0);
            end
        catch
        end
    end
end

% =========================================================================
function S = collectMedStd(MSobj, YCol, XCol, BadEpochMask, Args, ScaleCosCol)
    % Per-source median(XCol) and std(YCol) over epochs, with NaN/flag/faint cuts.
    %   Faint cut is applied on XCol (the reference mag), so for angular
    %   quantities (XCol=RefMag) faint sources on RefMag are dropped from
    %   YCol's statistics too. Tabulated YCol values are never transformed.
    %
    %   Optional 6th argument ScaleCosCol (char): name of a column whose
    %   per-source median is taken in degrees; the resulting std is then
    %   multiplied by cos(median ScaleCosCol) for each source. Used for the
    %   RA -> projected angular separation correction (ScaleCosCol = 'Dec').
    if nargin < 6, ScaleCosCol = ''; end
    S = [];
    if ~isfield(MSobj.Data, YCol); return; end
    if ~isfield(MSobj.Data, XCol); return; end

    Y = MSobj.Data.(YCol);
    X = MSobj.Data.(XCol);

    Y(BadEpochMask) = NaN;
    X(BadEpochMask) = NaN;

    BadX = false(size(X));
    if isfinite(Args.BackgroundMag)
        BadX = X > Args.BackgroundMag;
        Y(BadX) = NaN;
        X(BadX) = NaN;
    end

    HaveScale = ~isempty(ScaleCosCol) && isfield(MSobj.Data, ScaleCosCol);
    if HaveScale
        Z = MSobj.Data.(ScaleCosCol);
        Z(BadEpochMask) = NaN;
        Z(BadX)         = NaN;
    end

    if Args.MinEpochs > 0
        Good = sum(~isnan(Y), 1) >= Args.MinEpochs;
        Y = Y(:, Good);
        X = X(:, Good);
        if HaveScale; Z = Z(:, Good); end
    end

    S.Med = median(X, 1, 'omitnan');
    S.Std = std(Y, 0, 1, 'omitnan');
    if HaveScale
        MedZ = median(Z, 1, 'omitnan');
        S.Std = S.Std .* cosd(MedZ);
    end
end

% =========================================================================
function renderCombined(Quantities, DataByQ, Args, CentralCrops)
    % One figure with one subplot per quantity (single mode only).
    if numel(Args.Modes) > 1
        warning('plotPhotScatter:CombinedSingleMode', ...
            'LayoutMode=''Combined'' uses the first mode only.');
    end
    N = numel(Quantities);
    Ncols = ceil(sqrt(N));
    Nrows = ceil(N / Ncols);
    figure('Name', sprintf('Mag vs Std — combined (%d quantities)', N), ...
           'Position', [50, 50, 380*Ncols, 350*Nrows]);
    for Iq = 1:N
        Ax = subplot(Nrows, Ncols, Iq); hold(Ax, 'on');
        FName = matlab.lang.makeValidName(Quantities{Iq});
        D = DataByQ.(FName);
        if D.IsAngular
            drawAngularPanel(Ax, Quantities{Iq}, DataByQ, 1, Args, CentralCrops);
        else
            drawMagPanel(Ax, Quantities{Iq}, DataByQ, 1, Args, CentralCrops);
            title(Ax, Quantities{Iq}, 'Interpreter', 'none');
        end
    end
    sgtitle('Epoch-to-epoch scatter (combined)', 'FontSize', 11, ...
        'Interpreter', 'none');
end

% =========================================================================
function drawMagPanel(Ax, QName, DataByQ, Im, Args, CentralCrops)
    % Draw one mag-quantity panel onto axes Ax.
    FName = matlab.lang.makeValidName(QName);
    D = DataByQ.(FName);
    CropsToUse = D.CropsAnalyzed{Im};
    if isempty(CropsToUse); return; end

    CropCmap = lines(numel(CropsToUse));
    plotCropDots(Ax, CropsToUse, D.PerCrop{Im}, Args, CropCmap, ...
        CentralCrops, [0.2 0.4 0.8], [0.6 0.75 0.95]);
    if Args.ColorByCrop
        addCropLegend(Ax, CropsToUse, D.PerCrop{Im}, CropCmap);
    end
    if ~strcmp(Args.OverlayTrend, 'none') && ~isempty(D.AllMed{Im})
        TrendFun = str2func(['nan' Args.OverlayTrend]);
        drawTrend(Ax, D.AllMed{Im}, D.AllStd{Im}, ...
            Args.TrendBinWidth, TrendFun, '-', [0 0 0], 2);
    end
    set(Ax, 'YScale', 'log'); box(Ax, 'on'); grid(Ax, 'on');
    xlabel(Ax, 'Median Magnitude', 'Interpreter', 'none');
    ylabel(Ax, 'Std [mag]', 'Interpreter', 'none');
    xlim(Ax, [9 22]); ylim(Ax, [1e-3 10]);
end

% =========================================================================
function drawAngularPanel(Ax, QName, DataByQ, Im, Args, CentralCrops)
    FName = matlab.lang.makeValidName(QName);
    D = DataByQ.(FName);
    CropsToUse = D.CropsAnalyzed{Im};
    if isempty(CropsToUse); return; end

    CropCmap = lines(numel(CropsToUse));
    plotCropDots(Ax, CropsToUse, D.PerCrop{Im}, Args, CropCmap, ...
        CentralCrops, [0.2 0.4 0.8], [0.6 0.75 0.95]);
    if Args.ColorByCrop
        addCropLegend(Ax, CropsToUse, D.PerCrop{Im}, CropCmap);
    end
    if ~strcmp(Args.OverlayTrend, 'none') && ~isempty(D.AllMed{Im})
        TrendFun = str2func(['nan' Args.OverlayTrend]);
        drawTrend(Ax, D.AllMed{Im}, D.AllStd{Im}, ...
            Args.TrendBinWidth, TrendFun, '-', [0 0 0], 2);
    end
    set(Ax, 'YScale', 'log'); box(Ax, 'on'); grid(Ax, 'on');
    xlabel(Ax, sprintf('Median %s', D.RefMag), 'Interpreter', 'none');
    ylabel(Ax, 'Std [arcsec]', 'Interpreter', 'none');
    xlim(Ax, [9 22]);
    title(Ax, QName, 'Interpreter', 'none');
end

% =========================================================================
function drawTrend(Ax, MedAll, StdAll, BinWidth, TrendFun, Style, Color, LineWidth)
    R = timeSeries.bin.binningFast([MedAll(:), StdAll(:)], ...
        BinWidth, [9 22], {'MidBin', @numel, TrendFun});
    Valid = R(:,2) >= 5;
    plot(Ax, R(Valid,1), R(Valid,3), Style, 'Color', Color, 'LineWidth', LineWidth);
end

% =========================================================================
function plotCropDots(Ax, CropsToUse, PerCropData, Args, CropCmap, CentralCrops, CentralColor, EdgeColor)
    for Iic = 1:numel(CropsToUse)
        if Iic > numel(PerCropData) || isempty(PerCropData{Iic}); continue; end
        Ic = CropsToUse(Iic);
        Med = PerCropData{Iic}.Med;
        Std = PerCropData{Iic}.Std;

        if Args.ColorByCrop
            plot(Ax, Med, Std, '.', 'Color', CropCmap(Iic,:), 'MarkerSize', 3);
        elseif Args.CentralEdge
            if ismember(Ic, CentralCrops)
                plot(Ax, Med, Std, '.', 'Color', CentralColor, 'MarkerSize', 4);
            else
                plot(Ax, Med, Std, '.', 'Color', EdgeColor, 'MarkerSize', 2);
            end
        else
            plot(Ax, Med, Std, '.', 'Color', CentralColor, 'MarkerSize', 3);
        end
    end
end

% =========================================================================
function addCropLegend(Ax, CropsToUse, PerCropData, CropCmap)
    LegHandles = []; LegLabels = {};
    for Iic = 1:numel(CropsToUse)
        if Iic > numel(PerCropData) || isempty(PerCropData{Iic}); continue; end
        Ic = CropsToUse(Iic);
        H = plot(Ax, NaN, NaN, '.', 'Color', CropCmap(Iic,:), 'MarkerSize', 10);
        LegHandles = [LegHandles, H]; %#ok<AGROW>
        LegLabels{end+1} = sprintf('%d', Ic); %#ok<AGROW>
    end
    if ~isempty(LegHandles)
        legend(Ax, LegHandles, LegLabels, 'Location', 'southwest', ...
            'FontSize', 6, 'NumColumns', 4, 'Interpreter', 'none');
    end
end
