function Result = plotPhotHistogram(DataPath, Args)
    % Histogram of any photometric-calibration quantity over many visit dirs.
    % Description: Discovers visit subdirectories under DataPath (one level
    %              or recursively), runs PhotCalibTrans calibration on each
    %              via batchPhotCalib, pools the resulting PC objects, and
    %              plots a histogram of one named quantity via
    %              plotPhotParamHist. Mirrors the (DataPath + VisitPattern)
    %              interface used by pipeline.last.quality.masks.maskBitStatistics.
    %              Any name accepted by resolvePCParam can be plotted, e.g.
    %              'Chi2_DOF', 'NCalib', 'NCalibRetention', 'TauAod500', 'PWV_cm',
    %              'AirMass', 'MedResidual', 'StdResidual'.
    %
    % Input  : - DataPath char/string. Parent directory containing one or
    %            more visit subdirectories.
    %          * ...,key,val,...
    %            'VisitPattern' - Glob for visit subdirectory names.
    %                             Default '*v[0-9]*' (e.g. 002712v0, 012334v1).
    %            'Recursive'    - If true, search for visit dirs at any depth
    %                             under DataPath; if false, only direct
    %                             children. Default true.
    %            'FileType'     - 'coadd', 'proc', or a cell of those names
    %                             to overlay (e.g. {'coadd','proc'}). When a
    %                             cell is given, batchPhotCalib runs once per
    %                             FileType and the resulting PC arrays are
    %                             pooled into a multi-set histogram (one
    %                             label per FileType). Default 'coadd'.
    %            'CalibArgs'    - Cell of NV pairs forwarded to fitPhotCalibTrans.
    %                             Default {}.
    %            'PCFile'       - If non-empty, load a previously cached batch
    %                             from this .mat instead of re-running
    %                             batchPhotCalib. The cached variable 'R' may
    %                             be either a single batch struct (single
    %                             FileType) or a cell of batch structs
    %                             (multi-FileType cache). Default ''.
    %            'CacheFile'    - If non-empty, save the freshly computed batch
    %                             struct(s) to this .mat. For a single FileType
    %                             the file is written as-is; for a cell
    %                             FileType the FileType name is appended to
    %                             the file stem (e.g. 'foo.mat' becomes
    %                             'foo_coadd.mat' and 'foo_proc.mat'), so each
    %                             batch lands in its own file. Ignored when
    %                             PCFile is set. Default ''.
    %            'Param'        - Quantity name. Default 'Chi2_DOF'. See
    %                             resolvePCParam.
    %            'Bins'         - Number of bins or vector of edges. Default 30.
    %            'XLim'         - x-limits [min max]. Default [] (auto).
    %            'LogX'         - Log-transformed x axis. Default false.
    %            'CropsToAnalyze' - Crop indices to include. Default [] (all).
    %            'Stats'        - Overlay summary stats. Default true.
    %            'Color'        - Bar color. Default [0.30 0.55 0.90].
    %            'Verbose'      - Print progress. Default false.
    % Output : - Result struct returned by plotPhotParamHist (.Values,
    %            .CropIDs, .EpochIDs, .SetIDs, .Stats, .Param, ...), plus:
    %            .Batch       - struct (single FileType) or 1xK cell of
    %                           structs (one per FileType) as returned by
    %                           batchPhotCalib. Each has .PC, .VisitName,
    %                           .Success, .NTotal, etc.
    %            .DataPath    - DataPath echoed (char).
    %            .VisitPattern, .Recursive - echoed inputs.
    %            .FileType    - 1xK cell of FileType names actually used
    %                           (after dropping any that produced no PCs).
    % Author : D. Kovaleva (Apr 2026)
    % Example: % Only coadd images (default):
    %          DP = '/archimedes/LASTunitTest/2025/04/26';
    %          R  = pipeline.last.quality.photCalib.plotPhotHistogram(DP, ...
    %                  'FileType', 'coadd', 'Param', 'Chi2_DOF', 'LogX', true);
    %
    %          % Only proc images:
    %          R  = pipeline.last.quality.photCalib.plotPhotHistogram(DP, ...
    %                  'FileType', 'proc', 'Param', 'Chi2_DOF');
    %
    %          % Overlay coadd vs proc as a multi-set histogram:
    %          R  = pipeline.last.quality.photCalib.plotPhotHistogram(DP, ...
    %                  'FileType', {'coadd','proc'}, 'Param', 'Chi2_DOF');
    %
    %          % Whole year, only v0 visits, cached for reruns:
    %          DP = '/archimedes/LASTunitTest/2025';
    %          R  = pipeline.last.quality.photCalib.plotPhotHistogram(DP, ...
    %                  'VisitPattern', '*v0', ...
    %                  'CacheFile', '~/results/2025_v0_batch.mat', ...
    %                  'Param', 'NCalib');
    %
    %          % Replot from cache without recomputing:
    %          R  = pipeline.last.quality.photCalib.plotPhotHistogram('', ...
    %                  'PCFile', '~/results/2025_v0_batch.mat', ...
    %                  'Param', 'TauAod500');

    arguments
        DataPath                              {mustBeText} = ''
        Args.VisitPattern    {mustBeText}     = '*v[0-9]*'
        Args.Recursive       logical          = true
        Args.FileType                          = 'coadd'   % char or cell of {'coadd','proc'}
        Args.CalibArgs       cell             = {}
        Args.PCFile          {mustBeText}     = ''
        Args.CacheFile       {mustBeText}     = ''
        Args.Param           char             = 'Chi2_DOF'
        Args.Bins                              = 30
        Args.XLim                              = []
        Args.LogX            logical          = false
        Args.CropsToAnalyze                    = []
        Args.Stats           logical          = true
        Args.Color                             = [0.30 0.55 0.90]
        Args.Verbose         logical          = false
    end

    % --- Normalize FileType to a cell of names --------------------------
    if iscell(Args.FileType)
        FileTypes = cellfun(@char, Args.FileType, 'UniformOutput', false);
    else
        FileTypes = {char(Args.FileType)};
    end
    Valid = {'coadd', 'proc'};
    Bad = ~ismember(FileTypes, Valid);
    if any(Bad)
        error('plotPhotHistogram:BadFileType', ...
            'FileType must be ''coadd'', ''proc'', or a cell of those. Got: %s', ...
            strjoin(FileTypes(Bad), ', '));
    end
    NTypes = numel(FileTypes);

    % --- Obtain PC array(s) ---------------------------------------------
    if ~isempty(Args.PCFile)
        if Args.Verbose
            fprintf('Loading cached batch from %s\n', Args.PCFile);
        end
        S = load(char(Args.PCFile), 'R');
        if ~isfield(S, 'R')
            error('plotPhotHistogram:BadCache', ...
                'File %s does not contain an R struct.', Args.PCFile);
        end
        % Cache may be a single batch struct (single FileType) or a cell of
        % batch structs (multi FileType).
        if iscell(S.R)
            Batches = S.R(:)';
        else
            Batches = {S.R};
        end
        if numel(Batches) ~= NTypes && NTypes > 1
            warning('plotPhotHistogram:CacheMismatch', ...
                'Cache has %d batches but FileType requests %d. Using cache as-is.', ...
                numel(Batches), NTypes);
            NTypes = numel(Batches);
            FileTypes = arrayfun(@(k) sprintf('set%d', k), 1:NTypes, ...
                'UniformOutput', false);
        end
    else
        if isempty(DataPath)
            error('plotPhotHistogram:NoInput', ...
                'Provide either DataPath or PCFile.');
        end
        Batches = cell(1, NTypes);
        for It = 1:NTypes
            % When saving multi-FileType caches, suffix the file name
            % per FileType so each batch lands in its own file.
            CacheOut = '';
            if ~isempty(Args.CacheFile)
                if NTypes == 1
                    CacheOut = Args.CacheFile;
                else
                    [Pdir, Pname, Pext] = fileparts(char(Args.CacheFile));
                    CacheOut = fullfile(Pdir, sprintf('%s_%s%s', ...
                        Pname, FileTypes{It}, Pext));
                end
            end
            Batches{It} = pipeline.last.quality.photCalib.batchPhotCalib( ...
                char(DataPath), ...
                'VisitGlob',  Args.VisitPattern, ...
                'Recursive',  Args.Recursive, ...
                'FileType',   FileTypes{It}, ...
                'CalibArgs',  Args.CalibArgs, ...
                'OutFile',    CacheOut, ...
                'Verbose',    Args.Verbose);
        end
    end

    % Sanity: drop empty batches and align FileTypes accordingly
    KeepB = cellfun(@(b) ~isempty(b.PC), Batches);
    if ~any(KeepB)
        error('plotPhotHistogram:EmptyPC', ...
            'No PhotCalibTrans objects collected from %s', DataPath);
    end
    Batches   = Batches(KeepB);
    FileTypes = FileTypes(KeepB);

    % --- Plot histogram of the requested parameter ----------------------
    if numel(Batches) == 1
        PCInput = Batches{1}.PC;
        Labels  = {};
    else
        PCInput = cellfun(@(b) b.PC, Batches, 'UniformOutput', false);
        Labels  = FileTypes;
    end

    Result = pipeline.last.quality.photCalib.plotPhotParamHist(PCInput, ...
        'Param',          Args.Param, ...
        'Bins',           Args.Bins, ...
        'XLim',           Args.XLim, ...
        'LogX',           Args.LogX, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'Stats',          Args.Stats, ...
        'Color',          Args.Color, ...
        'Labels',         Labels, ...
        'Verbose',        Args.Verbose);

    if numel(Batches) == 1
        Result.Batch = Batches{1};
    else
        Result.Batch = Batches;
    end
    Result.DataPath     = char(DataPath);
    Result.VisitPattern = Args.VisitPattern;
    Result.Recursive    = Args.Recursive;
    Result.FileType     = FileTypes;
end
