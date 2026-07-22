function Report = batchOverlapSources(BaseDir, Args)
    % Bulk overlap-source comparison across many LAST coadd visits
    % Description: For each coadd visit under BaseDir, loads the crop
    %              AstroImages, per-crop-calibrates them via
    %              imProc.calib.fitPhotCalibTrans, then runs
    %              pipeline.last.quality.overlapSources to compare
    %              magnitudes at the 24-crop-overlap interfaces. Aggregates
    %              the per-visit results into (a) per-interface stats
    %              across visits and (b) a global pool of per-source
    %              MAG_APER_3 differences.
    %
    %              Missing crops are handled gracefully: visits with
    %              fewer than NCrops loaded files are padded with dummy
    %              AstroImages carrying a placeholder ORIGSEC so
    %              overlapSources's ORIGSEC lookup does not throw. Every
    %              interface touching a missing crop naturally returns
    %              "no overlap sources" and shows up as NaN in the
    %              per-visit R; cross-visit aggregation uses omitnan.
    %
    % Input  : - BaseDir - root path holding coadd files.
    %          * ...,key,val,...
    %            'FilePattern'    - dir() glob for the discovery product.
    %                               Default 'LAST*_sci_coadd_Image_1.fits'.
    %            'Recursive'      - Walk sub-tree. Default true.
    %            'FieldId'        - Substring match _<FieldId>_ in basename.
    %                               Default '' (no filter).
    %            'Filter'         - Substring match _<Filter>_  in basename.
    %                               Default ''.
    %            'MaxVisits'      - Cap. Default Inf.
    %            'NCrops'         - Expected crop count per visit
    %                               (LAST tiling = 24). Default 24.
    %            'OptSeqName'     - Recipe forwarded to fitPhotCalibTrans.
    %                               Default 'LAST_Joint_2Iter_AtmosFirst_Split3'.
    %            'CalibArgsExtra' - Extra key-value pairs prepended to the
    %                               CalibArgs cell (see fitPhotCalibTrans).
    %                               Default {}.
    %            'MagCol'         - Catalog column fed to overlapSources'
    %                               'Prop'. Default 'MAG_APER_3'.
    %            'MagRange'       - Single mag window for overlapSources.
    %                               Used when MagBins and MagLimits are
    %                               both empty. Default [12 14].
    %            'MagBins'        - Multiple non-overlapping (or overlapping)
    %                               mag windows for slicing statistics. Cell
    %                               of 1x2 doubles OR Nbin x 2 matrix. Each
    %                               entry [lo hi] runs overlapSources once
    %                               with that MagRange. Results end up in
    %                               Report.PerMagBin(b). Default {} (skip).
    %            'MagLimits'      - Vector of upper mag cutoffs. Each L runs
    %                               overlapSources with MagRange = [-Inf L],
    %                               i.e. cumulative "all sources brighter
    %                               than L". Results end up in
    %                               Report.PerMagLimit(l). Default [].
    %            'MatchRadius'    - Match radius [arcsec]. Default 1.
    %            'CroppingScheme' - 'new' | 'old'. Default 'new'.
    %            'FilterBad'      - Enable overlapSources's FLAGS filter.
    %                               Default false (safer when some crops
    %                               lack FLAGS).
    %            'OutFile'        - When non-empty, save Report to this
    %                               .mat file at the end (via `save`).
    %                               Default '' (no save).
    %            'Verbose'        - Per-visit progress printout. Default true.
    %            'InnerVerbose'   - Verbose flag passed to fitPhotCalibTrans
    %                               (surfaces calibrate's diagnostics).
    %                               Default false.
    % Output : - Report struct with:
    %            .Args             - echo of the batch args
    %            .PerVisit(1..NV)  - per-visit record. Fields:
    %                .VisitStem, .NCrops, .CropIds (1xNCrops present crop
    %                numbers), .Files (1xNCrops cellstr of full paths, in
    %                the same order as CropIds), .R (overlapSources
    %                output), .Elapsed [s], .ErrorMessage ('' on success)
    %            .PerInterface(1..38) - cross-visit aggregate per interface.
    %                Fields: .NVisits, .NDiff_total, .MedianOfMedians,
    %                .MeanOfMedians, .StdOfMedians, .MedianOfMeans,
    %                .MedianOfStds, .AllDiffs (pooled per-source vector for
    %                this interface across visits), and pool stats
    %                .Pool_Median, .Pool_Mean, .Pool_Std, .Pool_RobustStd
    %                (Pool_RobustStd = 1.4826*MAD, a robust std estimator - the
    %                scaled median-absolute-deviation, consistent with sigma
    %                under normality; NOT the raw unscaled MAD).
    %            .PerSourcePair     - global pool across ALL interfaces AND
    %                ALL visits (from Args.MagRange). Fields: .AllDiffs,
    %                .N, .Median, .Mean, .Std, .MAD.
    %            .PerMagBin(1..Nbin)   - only when Args.MagBins non-empty.
    %                One mini-Report per bin. Fields:
    %                    .MagRange       [lo hi]
    %                    .PerVisit(iv).R the overlapSources output at this bin
    %                    .PerInterface(k) cross-visit aggregate at this bin
    %                    .PerSourcePair   global pool at this bin
    %            .PerMagLimit(1..Nlim) - only when Args.MagLimits non-empty.
    %                Same schema as PerMagBin, but each .MagRange = [-Inf L]
    %                for the cumulative "MAG < L" pool. Additional field
    %                    .MagLimit    scalar L
    % Author : D. Kovaleva (Jul 2026)
    % See also: imProc.calib.fitPhotCalibTrans,
    %           pipeline.last.quality.overlapSources,
    %           pipeline.last.quality.photCalib.batchPhotCalibTrans.
    % Example:
    %   R = pipeline.last.quality.photCalib.batchOverlapSources( ...
    %           '/euclid/last/data/LAST.01.05.03/2025/06', ...
    %           'Filter', 'clear', 'FieldId', '1716.c', ...
    %           'MaxVisits', 10, ...
    %           'OutFile', '/home/dana/overlap_1716c_2iterAF.mat');

    arguments
        BaseDir                     (1,:) char
        Args.FilePattern            (1,:) char = 'LAST*_sci_coadd_Image_1.fits'
        Args.Recursive                    logical = true
        Args.FieldId                (1,:) char = ''
        Args.Filter                 (1,:) char = ''
        Args.MaxVisits              (1,1) double = Inf
        Args.NCrops                 (1,1) double {mustBePositive, mustBeInteger} = 24
        Args.OptSeqName             (1,:) char = 'LAST_Joint_2Iter_AtmosFirst_Split3'
        Args.CalibArgsExtra               cell = {}
        Args.MagCol                 (1,:) char = 'MAG_APER_3'
        Args.MagRange               (1,2) double = [12 17]
        Args.MagBins                              = {}   % cell of [lo hi] or [Nbin x 2] matrix; each entry is a mag window (lo < MAG < hi)
        Args.MagLimits                            = []   % vector of upper cutoffs; each L defines the cumulative bin MAG < L
        Args.MatchRadius            (1,1) double = 1
        Args.CroppingScheme         (1,:) char = 'new'
        Args.FilterBad                    logical = false
        Args.OutFile                (1,:) char = ''
        Args.Verbose                      logical = true
        Args.InnerVerbose                 logical = false
    end

    % ---- Normalise multi-view args (early, so shape errors fire fast) --
    Bins   = i_normaliseBins(Args.MagBins);         % 1xNbin cell of [lo hi]
    Limits = Args.MagLimits(:).';                   % 1xNlim vector of upper cutoffs

    % ---- Discovery ------------------------------------------------------
    if Args.Verbose
        fprintf('batchOverlapSources: discovering under %s (pattern "%s") ...\n', ...
                BaseDir, Args.FilePattern);
        T0 = tic;
    end
    Visits = i_discoverVisits(BaseDir, Args.FilePattern, Args.Recursive, ...
                              Args.FieldId, Args.Filter);
    if Args.Verbose
        fprintf('  discovery: %d visit group(s) in %.1f s\n', numel(Visits), toc(T0));
    end
    if isempty(Visits)
        error('pipeline:last:quality:photCalib:batchOverlapSources:NoVisits', ...
              'No visits matched pattern %s under %s', Args.FilePattern, BaseDir);
    end
    if isfinite(Args.MaxVisits) && Args.MaxVisits < numel(Visits)
        Visits = Visits(1:Args.MaxVisits);
    end
    NV = numel(Visits);
    if Args.Verbose
        fprintf('batchOverlapSources: processing %d visit(s)\n', NV);
    end

    % Multi-view runner: per-visit R cached separately for the default
    % Args.MagRange, each bin, each limit. All views share the SAME
    % calibrated Padded (calibration runs once per visit).
    NBin  = numel(Bins);
    NLim  = numel(Limits);
    PerVisit         = repmat(i_visitRowTemplate(), 1, NV);
    PerVisitBinR     = cell(NBin, NV);              % {b, iv} -> R
    PerVisitLimitR   = cell(NLim, NV);              % {l, iv} -> R

    % ---- Per-visit loop -------------------------------------------------
    for IV = 1:NV
        Vis = Visits(IV);
        NPresent = numel(Vis.Files);
        Row = i_visitRowTemplate();
        Row.VisitStem = Vis.Stem;
        Row.NCrops    = NPresent;
        Row.CropIds   = Vis.CropIds;
        Row.Files     = Vis.Files;

        if Args.Verbose
            fprintf('[%d/%d] %s   %d/%d crops present\n', ...
                IV, NV, Vis.Stem, NPresent, Args.NCrops);
        end

        TV = tic;
        try
            % Load only present crops.
            AI = AstroImage.readProducts(Vis.Files, 'ExtraOutProduct', "Cat");

            % Per-crop calibration (once per visit).
            CA = [{'OptSeqName', Args.OptSeqName}, Args.CalibArgsExtra];
            [ResultCalib, ~] = imProc.calib.fitPhotCalibTrans(AI, ...
                    'CreateNewObj', true, ...
                    'CalibArgs',    CA, ...
                    'Verbose',      Args.InnerVerbose);

            % Pad to NCrops slots so overlapSources can iterate over the
            % full LASToverlaps table; missing slots produce "no overlap".
            Padded = i_padToNCrops(ResultCalib, Vis.CropIds, Args.NCrops);

            % Default single-range overlap.
            Row.R = i_runOverlap(Padded, Args.MagRange, Args);

            % Per-bin overlaps (cheap: overlapSources reruns matching only).
            for B = 1:NBin
                PerVisitBinR{B, IV} = i_runOverlap(Padded, Bins{B}, Args);
            end
            % Per-limit overlaps (cumulative MAG < L).
            for L = 1:NLim
                PerVisitLimitR{L, IV} = i_runOverlap(Padded, [-Inf, Limits(L)], Args);
            end
        catch ME
            Row.ErrorMessage = ME.message;
            if Args.Verbose
                fprintf('  FAILED: %s\n', ME.message);
            end
        end
        Row.Elapsed = toc(TV);
        PerVisit(IV) = Row;
    end

    % ---- Aggregation ----------------------------------------------------
    Report.Args         = Args;
    Report.PerVisit     = PerVisit;
    Report.PerInterface  = i_aggregatePerInterface(PerVisit, Args.MagCol);
    Report.PerSourcePair = i_aggregatePerSourcePair(PerVisit, Args.MagCol);

    % Per-bin sub-Reports.
    if NBin > 0
        Report.PerMagBin = i_buildViewSummary(Bins, PerVisitBinR, PerVisit, ...
                                              Args.MagCol, 'range');
    end
    % Per-limit sub-Reports (each entry also carries the scalar MagLimit).
    if NLim > 0
        LimRanges = arrayfun(@(L) [-Inf, L], Limits, 'UniformOutput', false);
        PerLim = i_buildViewSummary(LimRanges, PerVisitLimitR, PerVisit, ...
                                    Args.MagCol, 'limit');
        for L = 1:NLim
            PerLim(L).MagLimit = Limits(L);
        end
        Report.PerMagLimit = PerLim;
    end

    if Args.Verbose
        NOK = sum(cellfun(@isempty, {PerVisit.ErrorMessage}));
        fprintf('batchOverlapSources: %d/%d visits succeeded\n', NOK, NV);
        if ~isempty(Report.PerSourcePair.AllDiffs)
            P = Report.PerSourcePair;
            fprintf('  pool: N=%d  median=%+.5f  mean=%+.5f  std=%.5f  MAD=%.5f (mag)\n', ...
                P.N, P.Median, P.Mean, P.Std, P.MAD);
        end
    end

    % ---- Optional save --------------------------------------------------
    if ~isempty(Args.OutFile)
        [d, ~, ~] = fileparts(Args.OutFile);
        if ~isempty(d) && ~exist(d, 'dir'); mkdir(d); end
        save(Args.OutFile, 'Report', '-v7.3');
        if Args.Verbose
            fprintf('  Report saved to %s\n', Args.OutFile);
        end
    end
end


% ==== Local helpers =====================================================

function Row = i_visitRowTemplate()
    Row = struct( ...
        'VisitStem',    '', ...
        'NCrops',       0,  ...
        'CropIds',      [], ...
        'Files',        {{}}, ...
        'R',            [], ...
        'Elapsed',      NaN,...
        'ErrorMessage', '');
end


function Visits = i_discoverVisits(BaseDir, Pattern, Recursive, FieldId, Filter)
    % Glob files matching Pattern under BaseDir; group by visit stem via the
    % LAST filename convention '<stem>_<crop>_sci_coadd_<Cat|Image>_<N>.fits'.
    % Returns 1xNvisit struct with .Stem, .Files (sorted by crop id), .CropIds.
    if Recursive
        D = dir(fullfile(BaseDir, '**', Pattern));
    else
        D = dir(fullfile(BaseDir, Pattern));
    end
    D = D(~[D.isdir]);
    Visits = struct('Stem', {}, 'Files', {}, 'CropIds', {});
    if isempty(D); return; end

    Files = fullfile({D.folder}, {D.name});
    Names = {D.name};

    if ~isempty(FieldId)
        Keep = contains(Names, ['_' FieldId '_']);
        Files = Files(Keep); Names = Names(Keep);
    end
    if ~isempty(Filter)
        Keep = contains(Names, ['_' Filter '_']);
        Files = Files(Keep); Names = Names(Keep);
    end
    if isempty(Files); return; end

    NF = numel(Files);
    Stems   = cell(1, NF);
    CropIdx = nan(1, NF);
    for K = 1:NF
        [~, FName, ~] = fileparts(Names{K});
        Tok = regexp(FName, '^(.+?)_(\d+)_sci_coadd_(?:Image|Cat)_\d+$', 'tokens', 'once');
        if isempty(Tok)
            Stems{K}   = FName;
            CropIdx(K) = NaN;
        else
            Stems{K}   = Tok{1};
            CropIdx(K) = str2double(Tok{2});
        end
    end

    [Us, ~, IUs] = unique(Stems);
    Visits = repmat(struct('Stem','','Files',{{}},'CropIds',[]), 1, numel(Us));
    for K = 1:numel(Us)
        Sel = find(IUs == K);
        [SortedCrops, Order] = sort(CropIdx(Sel));
        Visits(K).Stem    = Us{K};
        Visits(K).Files   = Files(Sel(Order));
        Visits(K).CropIds = SortedCrops(:).';
    end
end


function Padded = i_padToNCrops(Present, CropIds, NCrops)
    % Expand a length-NPresent AstroImage array to a length-NCrops array,
    % filling every missing crop position with a dummy AstroImage.
    %
    % Each dummy carries:
    %   .HeaderData with a placeholder ORIGSEC so overlapSources' header
    %                lookup does not throw;
    %   .CatData    an AstroCatalog inheriting the FULL column schema of a
    %                real crop (0 rows). That way overlapSources with
    %                FilterBad=true still finds a FLAGS column on the dummy
    %                — the filter runs on a 0-row vector (no-op), the pair
    %                yields no matches (no overlap anyway), and the "real
    %                has FLAGS / dummy does not" mismatch never happens.
    %
    % No-op fast path when every slot is already present in canonical order.
    if numel(Present) == NCrops && isequal(CropIds(:).', 1:NCrops)
        Padded = Present;
        return;
    end

    % Find a schema donor: the first present crop with a populated CatData.
    SchemaCat = [];
    for K = 1:numel(Present)
        C = Present(K).CatData;
        if ~isempty(C) && ~isempty(C.Catalog) && ~isempty(C.ColNames)
            SchemaCat = C;
            break;
        end
    end

    Padded   = AstroImage([1, NCrops]);
    DummyKey = {'ORIGSEC', '[1 100 1 100]', ''};
    for K = 1:NCrops
        Idx = find(CropIds == K, 1);
        if isempty(Idx)
            A = AstroImage();
            A.HeaderData = AstroHeader();
            A.HeaderData.insertKey(DummyKey);
            if ~isempty(SchemaCat)
                D = SchemaCat.copy;
                Ncol = size(SchemaCat.Catalog, 2);
                D.Catalog = zeros(0, Ncol);   % 0 rows, same Ncol/ColNames/ColUnits
                A.CatData = D;
            end
            Padded(K) = A;
        else
            Padded(K) = Present(Idx);
        end
    end
end


function AggIface = i_aggregatePerInterface(PerVisit, MagCol)
    % For each of the N_interface interfaces (typically 38), aggregate the
    % per-visit MedianDiff/MeanDiff/StdDiff into cross-visit summary stats,
    % and also pool every per-source diff into one vector for pool stats.
    NIface = 0;
    for IV = 1:numel(PerVisit)
        R = PerVisit(IV).R;
        if isstruct(R) && isfield(R, MagCol) ...
                && isfield(R.(MagCol), 'MedianDiff')
            NIface = numel(R.(MagCol).MedianDiff);
            break;
        end
    end
    if NIface == 0
        AggIface = i_ifaceRowTemplate();
        AggIface = AggIface([]);
        return;
    end

    AggIface = repmat(i_ifaceRowTemplate(), 1, NIface);
    for K = 1:NIface
        Meds = [];
        Means = [];
        Stds  = [];
        AllD  = [];
        for IV = 1:numel(PerVisit)
            R = PerVisit(IV).R;
            if ~(isstruct(R) && isfield(R, MagCol)); continue; end
            M = R.(MagCol);
            % Guard against any per-visit inconsistency where the diff arrays
            % are not all the same length (e.g. older data from before the
            % overlapSources no-overlap-branch fix).
            if K > numel(M.MedianDiff) || K > numel(M.MeanDiff) || K > numel(M.StdDiff)
                continue;
            end
            Meds(end+1)  = M.MedianDiff(K);  %#ok<AGROW>
            Means(end+1) = M.MeanDiff(K);    %#ok<AGROW>
            Stds(end+1)  = M.StdDiff(K);     %#ok<AGROW>
            if isfield(M, 'Diff') && K <= numel(M.Diff) && ~isempty(M.Diff{K})
                AllD = [AllD; M.Diff{K}(:)]; %#ok<AGROW>
            end
        end
        AggIface(K).NVisits         = sum(isfinite(Meds));
        AggIface(K).NDiff_total     = numel(AllD);
        AggIface(K).MedianOfMedians = median(Meds, 'omitnan');
        AggIface(K).MeanOfMedians   = mean(Meds,   'omitnan');
        AggIface(K).StdOfMedians    = std(Meds,    'omitnan');
        AggIface(K).MedianOfMeans   = median(Means,'omitnan');
        AggIface(K).MedianOfStds    = median(Stds, 'omitnan');
        AggIface(K).AllDiffs        = AllD;
        if ~isempty(AllD)
            Med = median(AllD, 'omitnan');
            AggIface(K).Pool_Median = Med;
            AggIface(K).Pool_Mean   = mean(AllD,   'omitnan');
            AggIface(K).Pool_Std    = std(AllD,    'omitnan');
            AggIface(K).Pool_RobustStd = 1.4826 * median(abs(AllD - Med), 'omitnan');   % scaled MAD (robust std)
        end
    end
end


function AggPool = i_aggregatePerSourcePair(PerVisit, MagCol)
    % Pool every per-source per-visit diff across ALL interfaces AND ALL
    % visits into one vector, then compute robust + non-robust stats.
    AllD = [];
    for IV = 1:numel(PerVisit)
        R = PerVisit(IV).R;
        if ~(isstruct(R) && isfield(R, MagCol) && isfield(R.(MagCol), 'Diff'))
            continue;
        end
        DCell = R.(MagCol).Diff;
        for K = 1:numel(DCell)
            if ~isempty(DCell{K})
                AllD = [AllD; DCell{K}(:)]; %#ok<AGROW>
            end
        end
    end
    AggPool = struct( ...
        'AllDiffs', AllD, ...
        'N',        numel(AllD), ...
        'Median',   NaN, ...
        'Mean',     NaN, ...
        'Std',      NaN, ...
        'MAD',      NaN);
    if ~isempty(AllD)
        Med = median(AllD, 'omitnan');
        AggPool.Median = Med;
        AggPool.Mean   = mean(AllD, 'omitnan');
        AggPool.Std    = std(AllD,  'omitnan');
        AggPool.MAD    = 1.4826 * median(abs(AllD - Med), 'omitnan');
    end
end


function Row = i_ifaceRowTemplate()
    Row = struct( ...
        'NVisits',         0,  ...
        'NDiff_total',     0,  ...
        'MedianOfMedians', NaN, ...
        'MeanOfMedians',   NaN, ...
        'StdOfMedians',    NaN, ...
        'MedianOfMeans',   NaN, ...
        'MedianOfStds',    NaN, ...
        'AllDiffs',        [], ...
        'Pool_Median',     NaN, ...
        'Pool_Mean',       NaN, ...
        'Pool_Std',        NaN, ...
        'Pool_RobustStd',  NaN);
end


function R = i_runOverlap(Padded, MagRange, Args)
    % One overlapSources call with the given MagRange. Returns [] on throw.
    try
        R = pipeline.last.quality.overlapSources(Padded, ...
                'Prop',           {Args.MagCol}, ...
                'MagRange',       MagRange, ...
                'MatchRadius',    Args.MatchRadius, ...
                'CroppingScheme', Args.CroppingScheme, ...
                'FilterBad',      Args.FilterBad, ...
                'Verbose',        false, ...
                'Plot',           false);
    catch
        R = [];
    end
end


function Bins = i_normaliseBins(In)
    % Accept a cell of 1x2, or an Nx2 numeric matrix, or empty. Return a
    % 1xNbin cell of 1x2 double.
    if isempty(In)
        Bins = {};
    elseif iscell(In)
        Bins = cellfun(@(x) reshape(double(x), 1, 2), In(:).', 'UniformOutput', false);
    elseif isnumeric(In)
        if size(In, 2) ~= 2
            error('pipeline:last:quality:photCalib:batchOverlapSources:BadMagBins', ...
                  'MagBins matrix must be N x 2');
        end
        Bins = arrayfun(@(k) In(k,:), 1:size(In,1), 'UniformOutput', false);
    else
        error('pipeline:last:quality:photCalib:batchOverlapSources:BadMagBins', ...
              'MagBins must be a cell of 1x2 or an N x 2 numeric matrix');
    end
end


function View = i_buildViewSummary(Ranges, RCell, PerVisit, MagCol, Kind)
    % Build a struct array of per-view mini-Reports from a NViews x NVisits
    % cell of overlapSources outputs. Each entry carries the view's
    % MagRange, per-visit R, per-interface aggregate, and pooled per-source
    % aggregate. Ignores view/visit combinations where R is empty (throw
    % during that overlap call).
    NView = numel(Ranges);
    View = repmat(struct('Kind', Kind, 'MagRange', [NaN NaN], ...
                         'PerVisit', struct('R', {}), ...
                         'PerInterface', [], ...
                         'PerSourcePair', struct()), 1, NView);
    for V = 1:NView
        View(V).Kind     = Kind;
        View(V).MagRange = Ranges{V};

        % Wrap per-visit R for this view in the same PerVisit(iv).R shape
        % the aggregation helpers already expect.
        VPerVisit = repmat(struct('R', [], 'ErrorMessage', ''), 1, numel(PerVisit));
        for IV = 1:numel(PerVisit)
            VPerVisit(IV).R = RCell{V, IV};
            if isempty(RCell{V, IV})
                VPerVisit(IV).ErrorMessage = 'view overlap failed';
            end
        end
        View(V).PerVisit      = VPerVisit;
        View(V).PerInterface  = i_aggregatePerInterface(VPerVisit, MagCol);
        View(V).PerSourcePair = i_aggregatePerSourcePair(VPerVisit, MagCol);
    end
end
