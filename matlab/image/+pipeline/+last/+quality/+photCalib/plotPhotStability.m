function Result = plotPhotStability(MS, Args)
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
    %              Three quantity types:
    %                * Magnitude quantities (anything not in
    %                  AngularQuantities or RefMagXQuantities). x = y =
    %                  quantity (the classic "mag vs mag-RMS" plot).
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
    %                * RefMagX quantities (default {}; opt-in via
    %                  'RefMagXQuantities'). Like angular: x =
    %                  median(RefMag). Unlike angular: y = std(quantity)
    %                  as-is, with NO arcsec rescale and NO cos(Dec)
    %                  projection. Use this when std(Q) is plotted as a
    %                  function of source brightness but Q itself isn't a
    %                  sky angle (e.g. pixel coordinates X1/Y1, FWHM,
    %                  shape parameters).
    %
    % Input  : - MS - a MatchedSources array (one element per crop, e.g.
    %            from matchEpochs or loadMergedMat) or a cell of
    %            MatchedSources.
    %          * ...,key,val,...
    %            'QuantityGroups' - Cell of cells of column names. Each
    %                             inner cell is one figure (1+ panels).
    %                             Default = the 3-group preset described
    %                             above. Supersedes 'Quantities'.
    %            'Quantities'   - Flat cell of column names. When set
    %                             (without QuantityGroups), each quantity
    %                             becomes its own one-panel figure. Default {}.
    %            'AngularQuantities' - Names that should use the angular
    %                             treatment (x = RefMag, std rescaled to
    %                             arcsec, RA additionally scaled by
    %                             cos(Dec)). Default {'RA','Dec'}.
    %            'RefMagXQuantities' - Names that should be plotted as
    %                             std(Q) vs median(RefMag) WITHOUT the
    %                             arcsec / cos(Dec) rescale applied to
    %                             angular quantities. Use for non-angular
    %                             quantities whose stability you want to
    %                             see as a function of source brightness
    %                             (pixel positions X1/Y1, FWHM, etc.).
    %                             Default {}.
    %            'RefMag'       - Reference mag column for the x-axis of
    %                             angular and RefMagX quantities, AND for
    %                             the faint-cut filter on angular
    %                             quantities. Default 'MAG_APER_3'.
    %            'FluxAsMag'    - Quantity names treated as fluxes and
    %                             converted to a magnitude before the
    %                             Std/median reduction:
    %                               MAG = -2.5*log10(Flux) + ZP
    %                             where ZP is read per crop from the 'Coadd'
    %                             image header (keyword 'ZPKey'). Lets a
    %                             flux column be plotted as a magnitude.
    %                             Default {'FLUX_PSF','FLUX_APER_3'}.
    %            'Coadd'        - 1xNcrop AstroImage array (the visit coadd)
    %                             supplying the per-crop zero-point for
    %                             FluxAsMag. Required when a plotted
    %                             quantity is in FluxAsMag. Default [].
    %            'ZPKey'        - Header keyword read from 'Coadd' for the
    %                             zero-point. Default 'PH_ZP'.
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
    %            'MinSN'        - Per-epoch S/N cut: (epoch,source) entries
    %                             with MS.Data.SN <= MinSN are NaN'd before
    %                             the Std/median reduction. 0 disables;
    %                             silently skipped if the MS has no SN
    %                             field. Default 5.
    %            'ColorByCrop'  - Color sources by crop ID. Default false.
    %            'CentralEdge'  - Distinguish central vs edge crops by
    %                             shade. Default false.
    %            'TileOrder'    - For CentralEdge: 'colmajor'|'rowmajor'.
    %                             Default 'rowmajor'.
    %            'SplitField'   - Per-source flag field (e.g. 'FORCED').
    %                             When non-empty, sources are split into two
    %                             populations (flag == 1 vs not), overplotted
    %                             in SplitColors, each with its own binned
    %                             trend; the per-crop dot colouring is
    %                             bypassed. A source is "flagged" when the
    %                             field is 1 in >= half its finite epochs.
    %                             Default '' (off).
    %            'SplitColors'  - 2x3 RGB [flagged; other]. Default
    %                             [0.85 0.33 0.10; 0.00 0.45 0.74].
    %            'SplitLabels'  - {flagged, other} legend labels. Default
    %                             {'Forced','Other'}.
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
    % Author : D. Kovaleva (Mar 2026)
    % Example: % --- loadVisit ----------------------------
    %          % Load only the MatchedSources of a visit (skip images/coadd):
    %          VisitDir = '/archimedes/LASTunitTest/2025/04/26/proc/002712v0';
    %          [~, ~, MS] = pipeline.last.load.loadVisit(VisitDir, ...
    %              'TempName_IndivIm', [], 'TempName_Coadd', []);
    %
    %          % Default 3-group layout: figures for {RA,Dec},
    %          % {MAG_PSF,MAG_APER_3}, and {MAG_AB_PSF,MAG_AB_APER_3}.
    %          % AB group is auto-skipped when MS has no AB columns.
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS);
    %
    %          % Custom grouping — e.g. one figure per quantity:
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %                  'QuantityGroups', { ...
    %                      {'MAG_PSF'}, {'MAG_APER_3'}, ...
    %                      {'RA'}, {'Dec'}});
    %
    %          % Or via the flat 'Quantities' shortcut (one figure each):
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %                  'Quantities', {'MAG_PSF','MAG_APER_3'});
    %
    %          % All groups in one combined figure (groups flattened):
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %                  'LayoutMode', 'Combined');
    %
    %          % Restrict to two crops (e.g. crop 1 and crop 10) and color
    %          % sources by crop:
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %                  'CropsToAnalyze', [1 10], 'ColorByCrop', true);
    %
    %          % Tighten the source selection: at least 10 epochs after
    %          % flag filtering, faint cut at MAG_APER_3 = 20:
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %                  'MinEpochs', 10, 'BackgroundMag', 20);
    %
    %          % Use MAG_PSF (not MAG_APER_3) as RA/Dec x-axis reference:
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %                  'RefMag', 'MAG_PSF');
    %
    %          % Force the AB group to render (even if MS lacks AB columns):
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %                  'SkipEmptyGroups', false);
    %
    %          % Plot std(X1), std(Y1) vs median MAG_APER_3 (no arcsec
    %          % rescale, no cos(Dec) projection -- pixel-coordinate
    %          % stability as a function of brightness):
    %          R = pipeline.last.quality.photCalib.plotPhotStability(MS, ...
    %                  'QuantityGroups', {{'X1','Y1'}}, ...
    %                  'RefMagXQuantities', {'X1','Y1'});
    %

    arguments
        MS
        Args.Quantities             cell                = {}
        Args.QuantityGroups         cell                = {}
        Args.AngularQuantities      cell                = {'RA', 'Dec'}
        Args.RefMagXQuantities      cell                = {}     % quantities plotted as Std(Q) vs median(RefMag); like AngularQuantities but without the *3600 arcsec rescale and cos(Dec) projection
        Args.RefMag                 (1,:) char          = 'MAG_APER_3'
        Args.FluxAsMag              cell                = {'FLUX_PSF','FLUX_APER_3'}
        Args.Coadd                                      = []
        Args.ZPKey                  (1,:) char          = 'PH_ZP'
        Args.LayoutMode             (1,:) char ...
            {mustBeMember(Args.LayoutMode, {'PerQuantity','Combined'})} = 'PerQuantity'
        Args.SkipEmptyGroups        logical             = true
        Args.CropsToAnalyze                             = []
        Args.OverlayTrend                               = 'median'
        Args.TrendBinWidth                              = 0.5
        Args.MinEpochs                                  = 5
        Args.FilterFlags            cell                = {'Saturated', 'NearEdge', 'NaN'}
        Args.BackgroundMag                              = 22
        Args.MinSN                                      = 5
        Args.ColorByCrop            logical             = false
        Args.CentralEdge            logical             = false
        Args.TileOrder                                  = 'rowmajor'
        Args.CentralColor           (1,3) double        = [0.2 0.4 0.8]
        Args.EdgeColor              (1,3) double        = [0.95 0.75 0.25]
        Args.MarkerSize             (1,1) double        = 3
        Args.ShowTrend              logical             = true
        Args.ShowDots               logical             = true
        Args.ShowStdBand            logical             = false
        Args.LinkAxes               (1,:) char ...
            {mustBeMember(Args.LinkAxes, {'xy','x','y','off'})} = 'y'
        Args.ShowLegend             logical             = true
        Args.SplitField             (1,:) char          = ''
        Args.SplitColors            (2,3) double        = [0.85 0.33 0.10; 0.00 0.45 0.74]
        Args.SplitLabels            cell                = {'Forced','Other'}
    end

    % --- Normalize MS input ---------------------------------------------
    %   Accept a MatchedSources array (one element per crop, e.g. from
    %   matchEpochs) or a cell of such. Internally wrap as a single-mode
    %   struct so the per-mode-loop machinery below stays unchanged.
    if isa(MS, 'MatchedSources')
        MS = struct('msdata', {num2cell(MS(:).')});
    elseif iscell(MS)
        MS = struct('msdata', {MS(:).'});
    else
        error('plotPhotStability:BadInput', ...
            'MS must be a MatchedSources array or a cell of MatchedSources.');
    end
    Args.Modes = {'msdata'};   % internal placeholder; not user-controllable

    % --- Resolve quantity groups ----------------------------------------
    %   Priority: explicit QuantityGroups > Quantities flat list > default 3-group
    %   layout (astrometry, relative photometry, AB photometry).
    if ~isempty(Args.QuantityGroups)
        Groups = Args.QuantityGroups;
    elseif ~isempty(Args.Quantities)
        Groups = cellfun(@(q){{q}}, Args.Quantities);  % one group per quantity
    else
        Groups = { ...
            {'RA', 'Dec'}, ...
            {'MAG_PSF', 'MAG_APER_3'}, ...
            {'MAG_AB_PSF', 'MAG_AB_APER_3'} ...
        };
    end
    % Flat de-duplicated quantity list across groups.
    Quantities = unique([Groups{:}], 'stable');

    % FluxAsMag converts flux -> mag and needs a per-crop zero-point.
    FluxQ = Quantities(ismember(Quantities, Args.FluxAsMag));
    if ~isempty(FluxQ) && isempty(Args.Coadd)
        error('plotPhotStability:NoCoadd', ...
            ['Quantity %s is in FluxAsMag (flux->mag needs a zero-point) ', ...
             'but no Coadd was supplied. Pass ''Coadd'' (a 1xNcrop ', ...
             'AstroImage array) or drop it from FluxAsMag.'], ...
            strjoin(FluxQ, ', '));
    end

    Nmodes = numel(Args.Modes);

    % --- Central crops for CentralEdge mode -----------------------------
    CentralCrops = centralCrops(Args.TileOrder);

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
    PerQuantity = repmat(struct('Quantity','', 'IsAngular',false, 'IsRefMagX',false, ...
        'RefMag','', 'CropsAnalyzed',{cell(1,Nmodes)}, ...
        'PerCrop',{cell(1,Nmodes)}, ...
        'AllMed',{cell(1,Nmodes)}, 'AllStd',{cell(1,Nmodes)}), 1, numel(Quantities));
    for Iq = 1:numel(Quantities)
        FName = matlab.lang.makeValidName(Quantities{Iq});
        D = DataByQ.(FName);
        PerQuantity(Iq).Quantity      = D.Quantity;
        PerQuantity(Iq).IsAngular     = D.IsAngular;
        PerQuantity(Iq).IsRefMagX     = D.IsRefMagX;
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

    AxArr = gobjects(1, Npanels);
    for Im = 1:Nmodes
        for Iqp = 1:numel(QNames)
            Q = QNames{Iqp};
            FName = matlab.lang.makeValidName(Q);
            D = DataByQ.(FName);
            PanelIdx = (Im - 1) * NPanelsPerMode + Iqp;
            Ax = subplot(1, Npanels, PanelIdx); hold(Ax, 'on');
            if D.IsAngular || D.IsRefMagX
                drawAngularPanel(Ax, Q, DataByQ, Im, Args, CentralCrops);
            else
                drawMagPanel(Ax, Q, DataByQ, Im, Args, CentralCrops);
                title(Ax, Q, 'Interpreter', 'none');
            end
            AxArr(PanelIdx) = Ax;
        end
    end
    sgtitle(sprintf('Epoch-to-epoch scatter: %s', strjoin(QNames, ' / ')), ...
        'FontSize', 11, 'Interpreter', 'none');
    for Isub = 1:Npanels
        Ax = subplot(1, Npanels, Isub);
        Ax.Position(4) = Ax.Position(4) * 0.88;
    end

    % Align scales within this group (coupled-panel comparison is
    % only meaningful when both panels share the same range).
    if ~strcmpi(Args.LinkAxes, 'off') && Npanels >= 2
        linkaxes(AxArr, Args.LinkAxes);
    end
end

% =========================================================================
function D = extractQuantity(MS, Q, Args)
    % Extract per-(mode, crop) (Med, Std) data for a single quantity.
    %   For angular quantities (RA/Dec), x = median(RefMag); for mag
    %   quantities, x = y = quantity. Std is computed on raw tabulated
    %   values; for angular quantities it is then rescaled by 3600 (deg
    %   → arcsec) for display.
    IsAng     = ismember(Q, Args.AngularQuantities);
    IsRefMagX = ismember(Q, Args.RefMagXQuantities);
    % Both IsAngular and IsRefMagX put median(RefMag) on the X axis;
    % only IsAngular triggers the *3600 / cos(Dec) Y rescale below.
    if IsAng || IsRefMagX
        RefMag = Args.RefMag;
    else
        RefMag = Q;
    end

    Modes = Args.Modes;
    Nmodes = numel(Modes);

    D.Quantity      = Q;
    D.IsAngular     = IsAng;
    D.IsRefMagX     = IsRefMagX;
    D.RefMag        = RefMag;
    D.CropsAnalyzed = cell(1, Nmodes);
    D.PerCrop       = cell(1, Nmodes);
    D.AllMed        = cell(1, Nmodes);
    D.AllStd        = cell(1, Nmodes);
    D.AllGroup      = cell(1, Nmodes);

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
        AllM = []; AllS = []; AllG = [];

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

            % Per-crop zero-point for FluxAsMag (from the coadd header).
            ZPcrop = cropZP(Args, Ic);

            S = collectMedStd(MSobj, Q, RefMag, BadEpochMask, Args, ...
                ScaleCosCol, Args.SplitField, ZPcrop);
            if ~isempty(S)
                if IsAng
                    S.Std = S.Std * 3600;   % deg → arcsec for display
                end
                PerCropM{Iic} = S;
                AllM = [AllM, S.Med];   %#ok<AGROW>
                AllS = [AllS, S.Std];   %#ok<AGROW>
                AllG = [AllG, S.Group]; %#ok<AGROW>
            end
        end

        D.PerCrop{Im}  = PerCropM;
        D.AllMed{Im}   = AllM;
        D.AllStd{Im}   = AllS;
        D.AllGroup{Im} = AllG;
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
function ZP = cropZP(Args, Ic)
    % Per-crop zero-point read from the Coadd image header (NaN if absent).
    ZP = NaN;
    if isempty(Args.Coadd) || Ic > numel(Args.Coadd); return; end
    try
        V = Args.Coadd(Ic).HeaderData.getVal(Args.ZPKey);
        if isnumeric(V) && isscalar(V) && isfinite(V); ZP = V; end
    catch
        % header / keyword unavailable - leave ZP = NaN
    end
end

% =========================================================================
function S = collectMedStd(MSobj, YCol, XCol, BadEpochMask, Args, ScaleCosCol, SplitCol, ZP)
    % Per-source median(XCol) and std(YCol) over epochs, with NaN/flag/faint cuts.
    %   Faint cut is applied on XCol (the reference mag), so for angular
    %   quantities (XCol=RefMag) faint sources on RefMag are dropped from
    %   YCol's statistics too.
    %
    %   ScaleCosCol (char, optional): column whose per-source median (deg)
    %   scales the std by its cosine - the RA projected-separation
    %   correction (ScaleCosCol = 'Dec').
    %
    %   SplitCol (char, optional): per-(epoch,source) flag column. When
    %   given and present, S.Group is a per-source logical (true where the
    %   flag is 1 in >= half the source's finite epochs), aligned with
    %   S.Med / S.Std. Empty otherwise.
    %
    %   ZP (numeric, optional): per-crop zero-point added when YCol/XCol is
    %   in Args.FluxAsMag, i.e. MAG = -2.5*log10(Flux) + ZP.
    if nargin < 6, ScaleCosCol = ''; end
    if nargin < 7, SplitCol    = ''; end
    if nargin < 8, ZP          = NaN; end
    S = [];
    if ~isfield(MSobj.Data, YCol); return; end
    if ~isfield(MSobj.Data, XCol); return; end

    Y = MSobj.Data.(YCol);
    X = MSobj.Data.(XCol);

    % Flux columns requested as magnitudes: MAG = -2.5*log10(Flux) + ZP,
    % converted per epoch before any reduction (the zero-point makes Std a
    % calibrated-magnitude scatter, and the faint cut act in real mag).
    if ismember(YCol, Args.FluxAsMag)
        Y(Y <= 0) = NaN;
        Y = -2.5 * log10(Y) + ZP;
    end
    if ismember(XCol, Args.FluxAsMag)
        X(X <= 0) = NaN;
        X = -2.5 * log10(X) + ZP;
    end

    Y(BadEpochMask) = NaN;
    X(BadEpochMask) = NaN;

    % Per-epoch S/N cut: drop (epoch,source) entries with SN <= MinSN.
    if Args.MinSN > 0 && isfield(MSobj.Data, 'SN')
        BadSN = ~(MSobj.Data.SN > Args.MinSN);
        Y(BadSN) = NaN;
        X(BadSN) = NaN;
    end

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

    % Per-source split classification (over all epochs, pre-filter).
    HaveSplit = ~isempty(SplitCol) && isfield(MSobj.Data, SplitCol);
    if HaveSplit
        Grp = mean(MSobj.Data.(SplitCol) == 1, 1, 'omitnan') >= 0.5;
    end

    if Args.MinEpochs > 0
        Good = sum(~isnan(Y), 1) >= Args.MinEpochs;
        Y = Y(:, Good);
        X = X(:, Good);
        if HaveScale; Z = Z(:, Good); end
        if HaveSplit; Grp = Grp(Good); end
    end

    S.Med = median(X, 1, 'omitnan');
    S.Std = std(Y, 0, 1, 'omitnan');
    if HaveScale
        MedZ = median(Z, 1, 'omitnan');
        S.Std = S.Std .* cosd(MedZ);
    end
    if HaveSplit
        S.Group = logical(Grp);
    else
        S.Group = [];
    end
end

% =========================================================================
function renderCombined(Quantities, DataByQ, Args, CentralCrops)
    % One figure with one subplot per quantity (single mode only).
    if numel(Args.Modes) > 1
        warning('plotPhotStability:CombinedSingleMode', ...
            'LayoutMode=''Combined'' uses the first mode only.');
    end
    N = numel(Quantities);
    Ncols = ceil(sqrt(N));
    Nrows = ceil(N / Ncols);
    figure('Name', sprintf('Mag vs Std — combined (%d quantities)', N), ...
           'Position', [50, 50, 380*Ncols, 350*Nrows]);
    AxArr = gobjects(1, N);
    for Iq = 1:N
        Ax = subplot(Nrows, Ncols, Iq); hold(Ax, 'on');
        AxArr(Iq) = Ax;
        FName = matlab.lang.makeValidName(Quantities{Iq});
        D = DataByQ.(FName);
        if D.IsAngular || D.IsRefMagX
            drawAngularPanel(Ax, Quantities{Iq}, DataByQ, 1, Args, CentralCrops);
        else
            drawMagPanel(Ax, Quantities{Iq}, DataByQ, 1, Args, CentralCrops);
            title(Ax, Quantities{Iq}, 'Interpreter', 'none');
        end
    end
    sgtitle('Epoch-to-epoch scatter (combined)', 'FontSize', 11, ...
        'Interpreter', 'none');
    if ~strcmpi(Args.LinkAxes, 'off') && N >= 2
        linkaxes(AxArr, Args.LinkAxes);
    end
end

% =========================================================================
function drawMagPanel(Ax, QName, DataByQ, Im, Args, CentralCrops)
    % Draw one mag-quantity panel onto axes Ax.
    FName = matlab.lang.makeValidName(QName);
    D = DataByQ.(FName);
    CropsToUse = D.CropsAnalyzed{Im};
    if isempty(CropsToUse); return; end

    if ~isempty(Args.SplitField)
        drawSplitPanel(Ax, D, Im, Args);
    else
        CropCmap = lines(numel(CropsToUse));
        if Args.ShowDots
            plotCropDots(Ax, CropsToUse, D.PerCrop{Im}, Args, CropCmap, ...
                CentralCrops, Args.CentralColor, Args.EdgeColor);
            if Args.ColorByCrop && Args.ShowLegend
                addCropLegend(Ax, CropsToUse, D.PerCrop{Im}, CropCmap);
            end
        end
        if Args.ShowTrend && ~strcmp(Args.OverlayTrend, 'none') && ~isempty(D.AllMed{Im})
            TrendFun = str2func(['nan' Args.OverlayTrend]);
            drawTrend(Ax, D.AllMed{Im}, D.AllStd{Im}, ...
                Args.TrendBinWidth, TrendFun, '-', [0 0 0], 2, Args.ShowStdBand);
        end
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

    if ~isempty(Args.SplitField)
        drawSplitPanel(Ax, D, Im, Args);
    else
        CropCmap = lines(numel(CropsToUse));
        if Args.ShowDots
            plotCropDots(Ax, CropsToUse, D.PerCrop{Im}, Args, CropCmap, ...
                CentralCrops, Args.CentralColor, Args.EdgeColor);
            if Args.ColorByCrop && Args.ShowLegend
                addCropLegend(Ax, CropsToUse, D.PerCrop{Im}, CropCmap);
            end
        end
        if Args.ShowTrend && ~strcmp(Args.OverlayTrend, 'none') && ~isempty(D.AllMed{Im})
            TrendFun = str2func(['nan' Args.OverlayTrend]);
            drawTrend(Ax, D.AllMed{Im}, D.AllStd{Im}, ...
                Args.TrendBinWidth, TrendFun, '-', [0 0 0], 2, Args.ShowStdBand);
        end
    end
    set(Ax, 'YScale', 'log'); box(Ax, 'on'); grid(Ax, 'on');
    xlabel(Ax, sprintf('Median %s', D.RefMag), 'Interpreter', 'none');
    if isfield(D, 'IsRefMagX') && D.IsRefMagX
        ylabel(Ax, sprintf('Std %s', QName), 'Interpreter', 'none');
    else
        ylabel(Ax, 'Std [arcsec]', 'Interpreter', 'none');
    end
    xlim(Ax, [9 22]);
    title(Ax, QName, 'Interpreter', 'none');
end

% =========================================================================
function drawSplitPanel(Ax, D, Im, Args)
    % Overplot two source populations split by Args.SplitField, each with
    % its own binned-median trend. Used in place of the per-crop dots when
    % SplitField is set.
    Med = D.AllMed{Im};
    Std = D.AllStd{Im};
    Grp = D.AllGroup{Im};                 % logical, true = flagged
    if numel(Grp) ~= numel(Med)
        % SplitField absent from MS.Data - degrade to one population.
        warning('plotPhotStability:NoSplitField', ...
            'SplitField "%s" not found in MS.Data; drawing one group.', ...
            Args.SplitField);
        Grp = false(size(Med));
    end
    Grp = logical(Grp);
    Fin = isfinite(Med) & isfinite(Std);   % N counts the plotted (finite) medians

    ColF = Args.SplitColors(1,:);
    ColO = Args.SplitColors(2,:);

    % "other" drawn first (behind), flagged on top
    plot(Ax, Med(~Grp), Std(~Grp), '.', 'Color', [ColO 0.5], ...
        'MarkerSize', Args.MarkerSize, ...
        'DisplayName', sprintf('%s (N=%d)', Args.SplitLabels{2}, nnz(~Grp & Fin)));
    plot(Ax, Med(Grp),  Std(Grp),  '.', 'Color', [ColF 0.5], ...
        'MarkerSize', Args.MarkerSize, ...
        'DisplayName', sprintf('%s (N=%d)', Args.SplitLabels{1}, nnz(Grp & Fin)));

    if Args.ShowTrend && ~strcmp(Args.OverlayTrend, 'none')
        To = binnedTrend(Med(~Grp), Std(~Grp), 'BinWidth', Args.TrendBinWidth, ...
            'Range', [9 22], 'Stat', Args.OverlayTrend, 'MinCount', 5);
        if ~isempty(To.X)
            plot(Ax, To.X, To.Val, '-', 'Color', ColO*0.7, 'LineWidth', 2.5, ...
                'DisplayName', sprintf('%s median', Args.SplitLabels{2}));
        end
        Tf = binnedTrend(Med(Grp), Std(Grp), 'BinWidth', Args.TrendBinWidth, ...
            'Range', [9 22], 'Stat', Args.OverlayTrend, 'MinCount', 5);
        if ~isempty(Tf.X)
            plot(Ax, Tf.X, Tf.Val, '-', 'Color', ColF*0.7, 'LineWidth', 2.5, ...
                'DisplayName', sprintf('%s median', Args.SplitLabels{1}));
        end
    end
    if Args.ShowLegend
        legend(Ax, 'Location', 'best');
    end
end

% =========================================================================
function drawTrend(Ax, MedAll, StdAll, BinWidth, TrendFun, Style, Color, LineWidth, ShowStdBand)
    % R columns: MidBin, count, trend value, per-bin std
    R = timeSeries.bin.binningFast([MedAll(:), StdAll(:)], ...
        BinWidth, [9 22], {'MidBin', @numel, TrendFun, @nanstd});
    Valid = R(:,2) >= 5;
    if ShowStdBand && any(Valid)
        X  = R(Valid, 1);
        T  = R(Valid, 3);
        Sb = R(Valid, 4);
        % Shaded +-std envelope around the trend (clamped above 0 because
        % the y axis is log-scaled).
        Lo = max(T - Sb, eps);
        Hi = T + Sb;
        fill([X; flipud(X)], [Lo; flipud(Hi)], Color, ...
            'EdgeColor', 'none', 'FaceAlpha', 0.18, ...
            'HandleVisibility', 'off');
    end
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
            plot(Ax, Med, Std, '.', 'Color', CropCmap(Iic,:), 'MarkerSize', Args.MarkerSize);
        elseif Args.CentralEdge
            if ismember(Ic, CentralCrops)
                plot(Ax, Med, Std, '.', 'Color', CentralColor, 'MarkerSize', Args.MarkerSize);
            else
                plot(Ax, Med, Std, '.', 'Color', EdgeColor, 'MarkerSize', Args.MarkerSize);
            end
        else
            plot(Ax, Med, Std, '.', 'Color', CentralColor, 'MarkerSize', Args.MarkerSize);
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
