function Nsrc = buildHTMfromTopCat(TableName, Args)
% Build an HTM catalog in HDF5 format by downloading in segments via TAP
% Package: VO.prep
% Description: Build an HTM catalog from large online sources by downloading
%              data in HTM-cell-sized segments via TAP protocol. This function
%              iterates over HTM cells, downloads sources for each cell via
%              cone/polygon query, filters them to the exact HTM triangle,
%              and saves directly to the corresponding HDF5 location.
%              This enables processing of catalogs too large to fit in memory.
% Input  : - TableName: TAP table name (e.g., 'gaiaedr3.gaia_source' or
%            '"II/349/ps1"' for VizieR catalogs with special characters).
%          * ...,key,val,...
%            'TapUrl'      - TAP service URL. If empty, uses TapName to resolve
%                            or defaults to Gaia TAP. Default is [].
%            'TapName'     - TAP service name (e.g., 'ESA Gaia Archive',
%                            'VizieR TAP (all VizieR catalogs)'). Used to resolve TapUrl via
%                            VO.TopCat.searchTapList. Default is [].
%            'CatName'     - Output catalog name. HDF5 files will be
%                            named <CatName>_htm_NNNNNN.hdf5. If empty,
%                            derived from TableName. Default is ''.
%            'Columns'     - Columns to SELECT. String or cell array.
%                            Default is '*' (all columns).
%            'WhereClause' - Additional filtering conditions for ADQL query,
%                            appended with AND to the spatial constraint.
%                            Do not include WHERE keyword.
%                            E.g., 'phot_g_mean_mag < 15' or
%                            'parallax > 10 AND bp_rp > 0.5'.
%                            Default is '' (no additional filter).
%            'ColRA'       - RA column name in output (after AS alias). Default is 'ra'.
%            'ColDec'      - Dec column name in output (after AS alias). Default is 'dec'.
%            'ColRASrc'    - RA column name in source table for spatial query.
%                            Use when output has alias (e.g., 'm.ramean' when
%                            SELECT has 'm.ramean AS ra'). If empty, uses ColRA.
%            'ColDecSrc'   - Dec column name in source table for spatial query.
%                            If empty, uses ColDec.
%            'ColRAOut'    - RA column index in output matrix. Default is 1.
%            'ColDecOut'   - Dec column index in output matrix. Default is 2.
%            'OutUnits'    - Output coordinate units: 'rad'|'deg'.
%                            Default is 'rad'.
%            'TapUnits'    - Coordinate units returned by TAP: 'rad'|'deg'.
%                            Default is 'deg'.
%            'HTM_Level'   - HTM level or 'auto' for automatic selection
%                            based on catalog size. Default is 7.
%            'AutoLevelMaxSrc' - Max sources per HTM segment for auto-level
%                            selection. Default is 1e6.
%            'AutoLevelRange' - [min, max] HTM level range for auto-selection.
%                            Default is [4, 10].
%            'NfilesInHDF' - Number of HTM cells per HDF5 file. Default is 100.
%            'IndStep'     - Index sampling step for HDF5 (for fast lookups).
%                            Default is 30.
%            'DecRange'    - [min, max] Dec range to process in radians.
%                            Default is [-pi/2, pi/2].
%            'RARange'     - [min, max] RA range to process in radians.
%                            Default is [0, 2*pi].
%            'RadiusFactor'- Cone search radius = RadiusFactor * HTM_side_length.
%                            Must be >1 to ensure cone fully covers triangle.
%                            Default is 1.5.
%            'QueryType'   - Spatial query type: 'cone'|'polygon'.
%                            Cone is universally supported; polygon is more
%                            efficient but not all TAP services support it.
%                            Default is 'cone'.
%            'TimeoutSec'  - Query timeout in seconds. Default is 600.
%            'MaxRetries'  - Max retries per cell on query failure. Default is 3.
%            'RetryPauseSec' - Pause between retries in seconds. Default is 5.
%            'Resume'      - If true, skip HTM cells that already exist in
%                            output files. Enables resuming interrupted downloads.
%                            Default is true.
%            'Verbose'     - Print progress information. Default is true.
%            'ColCell'     - Cell array of column names for output metadata.
%                            If empty, auto-detected from first query. Default is {}.
%            'ColUnits'    - Cell array of column units for output metadata.
%                            Default is {}.
%            'QueryMethod' - TAP query method: 'java'|'http'.
%                            'java' uses STILTS (faster), 'http' uses native HTTP.
%                            TopCat auto-falls back to 'http' if STILTS unavailable.
%                            Default is 'java'.
%            'HTM'         - Pre-built HTM structure from celestial.htm.htm_build.
%                            If empty, built internally. Default is [].
%            'LevelHTM'    - Pre-built LevelHTM structure. Default is [].
%            'SaveInd'     - Save HTM index file at the end. Default is true.
%            'LocalDir'    - Local directory for writing HDF5 files.
%                            Default is '~/tmp'.
%            'TargetDir'   - Remote directory where HDF5 files will be copied
%                            via NFS after writing locally. If empty, files
%                            are not copied. Default is '/euclid/catsHTM/NewCats/'.
%            'NullValue'   - Value to replace with NaN in output data.
%                            E.g., -999 for PS1 catalogs. Default is [] (no replacement).
%            'ComputedColumns' - Cell array defining columns to compute from
%                            existing columns. Each row: {NewName, Col1, Col2, Op}
%                            where Op is 'minus', 'plus', 'times', 'divide'.
%                            Computed after NullValue replacement.
%                            E.g., {'g_delta_psf_kron','gmeanpsfmag','gmeankronmag','minus'}
%                            Default is {} (no computed columns).
%            'DropColumns' - Cell array of column names to drop after computing.
%                            E.g., {'gmeankronmag','rmeankronmag'} to remove
%                            intermediate columns used only for computation.
%                            Default is {} (keep all columns).
%            'NumWorkers'  - Number of parallel workers for batch downloading.
%                            0 = sequential processing (default, original behavior).
%                            >0 = parallel download with N workers, sequential write.
%                            Requires Parallel Computing Toolbox.
%            'BatchSize'   - Number of HTM cells per batch when NumWorkers > 0.
%                            Downloads happen in parallel, writes are sequential.
%                            Default is 50.
% Output : - Nsrc: Nx2 matrix of [HTM_Index, Nsrc] with source counts per cell.
% Author : Dana Kovaleva (Dec 2025)
% Example: % Download Gaia DR3 bright stars with polygon query (ESA supports polygon)
%{
    Nsrc = VO.prep.buildHTMfromTopCat('gaiaedr3.gaia_source', ...
              'TapName', 'ESA Gaia Archive', ...
              'CatName', 'GAIA_DR3_Bright', ...
              'Columns', 'source_id, ra, dec, phot_g_mean_mag, bp_rp', ...
              'WhereClause', 'phot_g_mean_mag < 15', ...
              'QueryType', 'polygon');

    % Select columns by name using cell array
    Nsrc = VO.prep.buildHTMfromTopCat('gaiaedr3.gaia_source', ...
              'TapName', 'ESA Gaia Archive', ...
              'CatName', 'GAIA_DR3_Parallax', ...
              'Columns', {'source_id', 'ra', 'dec', 'parallax', 'pmra', 'pmdec'});

    % Download VizieR catalog (VizieR does not support polygon, uses cone)
    Nsrc = VO.prep.buildHTMfromTopCat('"II/349/ps1"', ...
              'TapName', 'VizieR TAP (all VizieR catalogs)', ...
              'CatName', 'PS1_DR1');

    % Resume interrupted download
    Nsrc = VO.prep.buildHTMfromTopCat('gaiaedr3.gaia_source', ...
              'TapName', 'ESA Gaia Archive', ...
              'CatName', 'GAIA_DR3_Full', ...
              'Resume', true);

    % Use pre-built HTM structure
    [HTM, LevelHTM] = celestial.htm.htm_build(7);
    Nsrc = VO.prep.buildHTMfromTopCat('gaiaedr3.gaia_source', ...
              'CatName', 'GAIA_Bright', ...
              'HTM', HTM, 'LevelHTM', LevelHTM);

    % PS1DR2 with NullValue replacement and computed columns
    Nsrc = VO.prep.buildHTMfromTopCat( ...
              'dbo.meanobjectview AS m JOIN dbo.stackobjectview AS s ON m.objid = s.objid JOIN dbo.forcedmeanobject AS f ON m.objid = f.objid', ...
              'TapUrl', 'https://mast.stsci.edu/vo-tap/api/v0.1/ps1dr2', ...
              'CatName', 'PS1DR2', ...
              'ColRA', 'ra', 'ColDec', 'dec', ...
              'Columns', ['m.ramean AS ra, m.decmean AS dec, m.rameanerr AS raerr, ' ...
                          'm.gmeanpsfmag, m.gmeankronmag, s.gpsflikelihood'], ...
              'NullValue', -999, ...
              'ComputedColumns', {'g_delta_psf_kron', 'gmeanpsfmag', 'gmeankronmag', 'minus'}, ...
              'DropColumns', {'gmeankronmag'});
%}

    arguments
        TableName                           % TAP table name
        Args.TapUrl           = []          % TAP service URL (or use TapName)
        Args.TapName          = []          % TAP service name (e.g., 'VizieR TAP (all VizieR catalogs)')
        Args.CatName          = ''          % Output catalog base name
        Args.Columns          = '*'         % Columns to SELECT (string or cell array)
        Args.WhereClause      = ''          % Additional WHERE conditions
        Args.ColRA            = 'ra'        % RA column name in output (after alias)
        Args.ColDec           = 'dec'       % Dec column name in output (after alias)
        Args.ColRASrc         = ''          % RA column in source table for spatial query
        Args.ColDecSrc        = ''          % Dec column in source table for spatial query
        Args.ColRAOut         = 1           % RA column index in output
        Args.ColDecOut        = 2           % Dec column index in output
        Args.OutUnits         = 'rad'       % Output coordinate units: 'rad'|'deg'
        Args.TapUnits         = 'deg'       % TAP input coordinate units: 'rad'|'deg'
        Args.HTM_Level        = 7           % HTM level (or 'auto' for automatic)
        Args.AutoLevelMaxSrc  = 1e3         % Max sources per cell for auto-level
        Args.AutoLevelRange   = [4, 10]    % [min, max] HTM level range for auto-selection
        Args.NfilesInHDF      = 100        % HTM cells per HDF5 file (matches catsHTM default)
        Args.IndStep          = 30          % Index sampling step for HDF5
        Args.DecRange         = [-pi/2, pi/2]   % Dec range to process [rad]
        Args.RARange          = [0, 2*pi]       % RA range to process [rad]
        Args.RadiusFactor     = 1.5         % Cone radius = RadiusFactor * HTM_radius
        Args.QueryType        = 'cone'      % 'cone'|'polygon'
        Args.TimeoutSec       = 600         % Query timeout in seconds
        Args.MaxRetries       = 3           % Max retries per cell on failure
        Args.RetryPauseSec    = 5           % Pause between retries in seconds
        Args.Resume           = true        % Skip existing HTM cells
        Args.Verbose          = true        % Print progress
        Args.ColCell          = {}          % Column names for output (auto-detect if empty)
        Args.ColUnits         = {}          % Column units for output
        Args.QueryMethod      = 'java'      % 'java'|'http' (TopCat handles fallback)
        Args.HTM              = []          % Pre-built HTM structure 
        Args.LevelHTM         = []          % Pre-built LevelHTM structure 
        Args.SaveInd          = true        % Save index HDF file at the end
        Args.LocalDir         = '/home/dana/tmp'      % Local directory for writing HDF5 files
        Args.TargetDir        = '/euclid/catsHTM/NewCats/'  % Remote directory for copying files
        Args.NullValue        = []          % Value to replace with NaN (e.g., -999)
        Args.ComputedColumns  = {}          % {NewName, Col1, Col2, Op} for computed columns
        Args.DropColumns      = {}          % Column names to drop after computing
        Args.NumWorkers       = 0           % 0=sequential, >0=parallel with N workers
        Args.BatchSize        = 50          % HTM cells per batch (for parallel mode)
    end

    RAD = constant.RAD;

    %----------------------------------------------------------------------
    % 1. INITIALIZATION
    %----------------------------------------------------------------------

    % Create TopCat object for TAP queries
    Tap = VO.TopCat;

    % Resolve TAP URL
    if isempty(Args.TapUrl)
        if ~isempty(Args.TapName)
            Args.TapUrl = VO.TopCat.searchTapList(Args.TapName);
            if isempty(Args.TapUrl)
                error('TAP service "%s" not found in TapList', Args.TapName);
            end
        else
            % Use default (Gaia)
            Args.TapUrl = Tap.TapUrl;
        end
    end

    % Generate catalog name from table name if not provided
    if isempty(Args.CatName)
        Args.CatName = regexprep(TableName, '[^a-zA-Z0-9_]', '_');
        Args.CatName = regexprep(Args.CatName, '_+', '_');
        Args.CatName = regexprep(Args.CatName, '^_|_$', '');
    end

    % Set source column names for spatial query (default to output names)
    if isempty(Args.ColRASrc)
        Args.ColRASrc = Args.ColRA;
    end
    if isempty(Args.ColDecSrc)
        Args.ColDecSrc = Args.ColDec;
    end

    % Format columns for query
    if iscell(Args.Columns)
        ColumnsStr = strjoin(Args.Columns, ', ');
    else
        ColumnsStr = char(Args.Columns);
    end

    if Args.Verbose
        fprintf('=== buildHTMfromTopCat ===\n');
        fprintf('Table: %s\n', TableName);
        fprintf('TAP URL: %s\n', Args.TapUrl);
        fprintf('Output: %s\n', Args.CatName);
        fprintf('Query method: %s\n', Args.QueryMethod);
    end

    %----------------------------------------------------------------------
    % 2. DETECT COLUMNS AND SAVE COLCELL FILE
    %----------------------------------------------------------------------

    if isempty(Args.ColCell)
        if Args.Verbose
            fprintf('Querying TAP for column names...\n');
        end

        % Run sample query to get data
        SampleQuery = sprintf('SELECT TOP 1 %s FROM %s', ColumnsStr, TableName);

        % Use STILTS directly to save CSV and preserve original column names
        OriginalColNames = {};
        if strcmp(ColumnsStr, '*')
            try
                % Run STILTS directly to get CSV with original column names
                TempCsvFile = fullfile(Args.LocalDir, 'temp_colnames.csv');
                JarPath = VO.TopCat.getStiltsJarPath();
                TapUrlClean = char(Args.TapUrl);
                if endsWith(TapUrlClean, '/'), TapUrlClean = TapUrlClean(1:end-1); end

                % Escape ADQL for shell
                AdqlEsc = VO.TopCat.escapeForShellDoubleQuotes(SampleQuery);

                % Build STILTS command
                cmd = sprintf('java -Xmx1g -jar "%s" tapquery tapurl="%s" language=ADQL adql="%s" omode=out ofmt=csv out="%s" sync=true 2>&1', ...
                    JarPath, TapUrlClean, AdqlEsc, TempCsvFile);

                [status, ~] = system(cmd);

                if status == 0 && isfile(TempCsvFile)
                    % Read original column names from CSV header
                    fid = fopen(TempCsvFile, 'r');
                    headerLine = fgetl(fid);
                    fclose(fid);

                    % Parse CSV header properly (handle quoted fields that may contain commas)
                    OriginalColNames = parseCSVHeader(headerLine);

                    if Args.Verbose
                        fprintf('Retrieved %d original column names from CSV header.\n', numel(OriginalColNames));
                    end
                    delete(TempCsvFile);
                end
            catch ME
                if Args.Verbose
                    fprintf('Could not read original column names from CSV: %s\n', ME.message);
                end
            end
        end

        % Run sample query to get data and MATLAB-sanitized column names
        T = Tap.query(SampleQuery, 'TapUrl', Args.TapUrl, 'TimeoutSec', 60, ...
                      'Method', Args.QueryMethod, 'WorkDir', Args.LocalDir);
        if isempty(T) || ~istable(T)
            error('VO:buildHTMfromTopCat:NoColumns', ...
                'Could not query column names from TAP service');
        end

        % Get column info with filtering details so we can apply same to original names
        [~, Args.ColCell, numericMask, reorderIdx] = tableToMatrixWithInfo(T, Args.ColRA, Args.ColDec, Args.TapUnits);
        if Args.Verbose
            fprintf('Detected %d columns (MATLAB names): %s\n', numel(Args.ColCell), strjoin(Args.ColCell, ', '));
        end

        % If user specified '*', rebuild ColumnsStr using original TAP column names
        % to ensure consistent columns across all HTM cell queries
        if strcmp(ColumnsStr, '*')
            if ~isempty(OriginalColNames) && numel(OriginalColNames) == numel(numericMask)
                % Apply same filtering (numeric only) and reordering to original names
                OriginalColNamesFiltered = OriginalColNames(numericMask);
                OriginalColNamesReordered = OriginalColNamesFiltered(reorderIdx);

                % Filter out VizieR internal columns (starting with '_') as they cannot be queried
                VizierInternalMask = cellfun(@(x) ~isempty(x) && x(1) == '_', OriginalColNamesReordered);
                if any(VizierInternalMask)
                    if Args.Verbose
                        fprintf('Excluding %d VizieR internal columns (starting with _).\n', sum(VizierInternalMask));
                    end
                    OriginalColNamesReordered = OriginalColNamesReordered(~VizierInternalMask);
                    Args.ColCell = Args.ColCell(~VizierInternalMask);
                end

                % Use original names from CSV header (quoted for safety)
                QuotedCols = cellfun(@(x) ['"' x '"'], OriginalColNamesReordered, 'UniformOutput', false);
                ColumnsStr = strjoin(QuotedCols, ', ');
                if Args.Verbose
                    fprintf('Using %d original TAP column names for queries.\n', numel(OriginalColNamesReordered));
                end
            else
                % Fallback: use MATLAB-sanitized names (may fail for special chars)
                QuotedCols = cellfun(@(x) ['"' x '"'], Args.ColCell, 'UniformOutput', false);
                ColumnsStr = strjoin(QuotedCols, ', ');
                if Args.Verbose
                    fprintf('Warning: Using MATLAB-sanitized column names (may fail for columns with special characters).\n');
                end
            end
        end
    end

    % Update ColCell to reflect computed columns and dropped columns
    Args.ColCell = applyColCellPostProcessing(Args.ColCell, Args.ComputedColumns, Args.DropColumns);
    if Args.Verbose && (~isempty(Args.ComputedColumns) || ~isempty(Args.DropColumns))
        fprintf('Final output columns (%d): %s\n', numel(Args.ColCell), strjoin(Args.ColCell, ', '));
    end

    % Save ColCell file immediately
    ColCellPath = fullfile(Args.LocalDir, Args.CatName);
    HDF5.save_cat_colcell(ColCellPath, Args.ColCell, Args.ColUnits);
    if Args.Verbose
        fprintf('Saved column metadata: %s_htmColCell.mat\n', ColCellPath);
    end

    %----------------------------------------------------------------------
    % 3. AUTO-LEVEL SELECTION (if requested)
    %----------------------------------------------------------------------

    if ischar(Args.HTM_Level) && strcmpi(Args.HTM_Level, 'auto')
        Args.HTM_Level = autoSelectLevel(Tap, TableName, Args.TapUrl, ...
                                          Args.AutoLevelMaxSrc, Args.AutoLevelRange, ...
                                          Args.WhereClause, Args.TimeoutSec, Args.Verbose);
    end

    %----------------------------------------------------------------------
    % 3. BUILD HTM STRUCTURE
    %----------------------------------------------------------------------

    if ~isempty(Args.HTM) && ~isempty(Args.LevelHTM)
        HTM = Args.HTM;
        LevelHTM = Args.LevelHTM;
    else
        if Args.Verbose
            fprintf('Building HTM structure at level %d...\n', Args.HTM_Level);
        end
        [HTM, LevelHTM] = celestial.htm.htm_build(Args.HTM_Level);
    end

    % Get list of HTM indices at target level
    % htm_build(N) builds levels 0 to N-1, so LevelHTM(N) gives level N-1
    ListIndexHTM = LevelHTM(Args.HTM_Level).ptr;
    Nhtm = numel(ListIndexHTM);

    % HTM cell side length in radians
    HTMSideRad = LevelHTM(Args.HTM_Level).side;
    SearchRadiusDeg = Args.RadiusFactor * HTMSideRad * RAD;

    if Args.Verbose
        fprintf('Number of HTM cells: %d\n', Nhtm);
        fprintf('HTM side length: %.4f deg\n', HTMSideRad * RAD);
        fprintf('Search radius: %.4f deg\n', SearchRadiusDeg);
        fprintf('Query type: %s\n', Args.QueryType);
    end

    % Initialize source count matrix
    Nsrc = zeros(Nhtm, 2);

    StartTime = tic;
    ProcessedCount = 0;
    SkippedCount = 0;
    FailedCells = [];
    CurrentHDFFile = '';  % Track current HDF5 file for remote copy

    %----------------------------------------------------------------------
    % 4. PRE-FILTER CELLS (for both sequential and parallel modes)
    %----------------------------------------------------------------------

    if Args.Verbose
        fprintf('\nFiltering HTM cells by Dec/RA range...\n');
    end

    CellsToProcess = [];
    CellIndices = [];  % Maps position in CellsToProcess to position in Nsrc

    for Ihtm = 1:Nhtm
        IndHTM = ListIndexHTM(Ihtm);
        MeanRA  = mean(HTM(IndHTM).coo(:,1));
        MeanDec = mean(HTM(IndHTM).coo(:,2));

        OutsideRange = MeanRA < Args.RARange(1) || MeanRA >= Args.RARange(2) || ...
                       MeanDec < Args.DecRange(1) || MeanDec >= Args.DecRange(2);
        AlreadyExists = Args.Resume && (checkHTMExists(Args.CatName, IndHTM, Args.NfilesInHDF, Args.LocalDir) || ...
                        (~isempty(Args.TargetDir) && checkHTMExists(Args.CatName, IndHTM, Args.NfilesInHDF, Args.TargetDir)));

        if OutsideRange
            Nsrc(Ihtm, :) = [IndHTM, 0];
        elseif AlreadyExists
            NsrcExisting = getHTMSourceCount(Args.CatName, IndHTM, Args.NfilesInHDF, Args.LocalDir);
            if NsrcExisting == 0 && ~isempty(Args.TargetDir)
                NsrcExisting = getHTMSourceCount(Args.CatName, IndHTM, Args.NfilesInHDF, Args.TargetDir);
            end
            Nsrc(Ihtm, :) = [IndHTM, NsrcExisting];
            SkippedCount = SkippedCount + 1;
        else
            CellsToProcess = [CellsToProcess, IndHTM]; %#ok<AGROW>
            CellIndices = [CellIndices, Ihtm]; %#ok<AGROW>
        end

        % Progress for filtering (every 100000 cells)
        if Args.Verbose && mod(Ihtm, 100000) == 0
            fprintf('  Filtered %d/%d cells...\n', Ihtm, Nhtm);
        end
    end

    NumCellsToProcess = numel(CellsToProcess);
    if Args.Verbose
        fprintf('Cells to process: %d (skipped %d existing/out-of-range)\n', ...
                NumCellsToProcess, Nhtm - NumCellsToProcess);
    end

    %----------------------------------------------------------------------
    % 5. MAIN LOOP: Process filtered cells
    %----------------------------------------------------------------------

    if NumCellsToProcess == 0
        if Args.Verbose
            fprintf('No cells to process.\n');
        end
    elseif Args.NumWorkers > 0
        %------------------------------------------------------------------
        % PARALLEL BATCH PROCESSING
        %------------------------------------------------------------------

        if Args.Verbose
            fprintf('\nStarting parallel processing: %d workers, batch size %d\n', ...
                    Args.NumWorkers, Args.BatchSize);
        end

        % Extract HTM coordinates for cells to process (avoid passing full HTM to workers)
        HTMCoo = cell(NumCellsToProcess, 1);
        for iCell = 1:NumCellsToProcess
            HTMCoo{iCell} = HTM(CellsToProcess(iCell)).coo;
        end

        % Start parallel pool if needed
        try
            pool = gcp('nocreate');
            if isempty(pool)
                pool = parpool(Args.NumWorkers);
            elseif pool.NumWorkers ~= Args.NumWorkers
                delete(pool);
                pool = parpool(Args.NumWorkers);
            end
        catch ME
            warning('Could not start parallel pool: %s. Falling back to sequential.', char(ME.message));
            Args.NumWorkers = 0;
        end

        if Args.NumWorkers > 0
            % Process in batches
            NumBatches = ceil(NumCellsToProcess / Args.BatchSize);

            for iBatch = 1:NumBatches
                batchStart = (iBatch - 1) * Args.BatchSize + 1;
                batchEnd = min(iBatch * Args.BatchSize, NumCellsToProcess);
                batchCells = CellsToProcess(batchStart:batchEnd);
                batchIndices = CellIndices(batchStart:batchEnd);
                batchHTMCoo = HTMCoo(batchStart:batchEnd);
                batchSize = numel(batchCells);

                if Args.Verbose
                    fprintf('Batch %d/%d: downloading cells %d-%d...\n', ...
                            iBatch, NumBatches, batchStart, batchEnd);
                end

                % Parallel download phase - pass only necessary data
                batchResults = cell(batchSize, 1);
                batchFailed = false(batchSize, 1);

                parfor iCell = 1:batchSize
                    IndHTM = batchCells(iCell);
                    cellCoo = batchHTMCoo{iCell};
                    [Data, queryFailed] = downloadHTMCellLight( ...
                        TableName, ColumnsStr, IndHTM, cellCoo, RAD, SearchRadiusDeg, Args);
                    batchResults{iCell} = Data;
                    batchFailed(iCell) = queryFailed;
                end

                % Sequential write phase
                for iCell = 1:batchSize
                    IndHTM = batchCells(iCell);
                    Ihtm = batchIndices(iCell);
                    Data = batchResults{iCell};

                    if batchFailed(iCell)
                        FailedCells = [FailedCells, IndHTM]; %#ok<AGROW>
                        Nsrc(Ihtm, :) = [IndHTM, 0];
                    else
                        NsrcCell = writeHTMCell(Data, IndHTM, Args);
                        Nsrc(Ihtm, :) = [IndHTM, NsrcCell];
                        ProcessedCount = ProcessedCount + 1;
                    end

                    % Handle file copying to remote
                    [ThisFileName, ~] = HDF5.get_file_var_from_htmid(Args.CatName, IndHTM, Args.NfilesInHDF);
                    if ~isempty(CurrentHDFFile) && ~strcmp(ThisFileName, CurrentHDFFile) && ~isempty(Args.TargetDir)
                        FullPath = fullfile(Args.LocalDir, CurrentHDFFile);
                        if isfile(FullPath)
                            tools.os.copyFileOverNFS({FullPath}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
                            if Args.Verbose
                                fprintf('  Copied completed file: %s\n', CurrentHDFFile);
                            end
                        end
                    end
                    CurrentHDFFile = ThisFileName;
                end

                % Clear batch data to free memory
                clear batchResults batchHTMCoo;

                % Print batch progress
                if Args.Verbose
                    batchFailed_count = sum(batchFailed);
                    Elapsed = toc(StartTime);
                    Rate = ProcessedCount / Elapsed;
                    Remaining = NumCellsToProcess - batchEnd;
                    ETA = Remaining / max(Rate, 0.001);
                    fprintf('[%d/%d] Processed %d cells (%.1f cells/min, ETA: %.1f min)\n', ...
                            batchEnd, NumCellsToProcess, ProcessedCount, Rate * 60, ETA / 60);
                    if batchFailed_count > 0
                        fprintf('  Batch had %d failed cells\n', batchFailed_count);
                    end
                end
            end
        end
    end

    if Args.NumWorkers == 0 && NumCellsToProcess > 0
        %------------------------------------------------------------------
        % SEQUENTIAL PROCESSING
        %------------------------------------------------------------------

        if Args.Verbose
            fprintf('\nStarting sequential processing...\n');
        end

        for iProc = 1:NumCellsToProcess
            IndHTM = CellsToProcess(iProc);
            Ihtm = CellIndices(iProc);

            % Check if we've moved to a new HDF5 file - if so, copy the completed one
            [ThisFileName, ~] = HDF5.get_file_var_from_htmid(Args.CatName, IndHTM, Args.NfilesInHDF);
            if ~isempty(CurrentHDFFile) && ~strcmp(ThisFileName, CurrentHDFFile) && ~isempty(Args.TargetDir)
                FullPath = fullfile(Args.LocalDir, CurrentHDFFile);
                if isfile(FullPath)
                    tools.os.copyFileOverNFS({FullPath}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
                    if Args.Verbose
                        fprintf('  Copied completed file: %s\n', CurrentHDFFile);
                    end
                end
            end
            CurrentHDFFile = ThisFileName;

            % Process this HTM cell
            [NsrcCell, ~, QueryFailed] = processHTMCell( ...
                Tap, TableName, ColumnsStr, IndHTM, HTM, RAD, SearchRadiusDeg, Args);

            if QueryFailed
                FailedCells = [FailedCells, IndHTM]; %#ok<AGROW>
                Nsrc(Ihtm, :) = [IndHTM, 0];
            else
                Nsrc(Ihtm, :) = [IndHTM, NsrcCell];
                ProcessedCount = ProcessedCount + 1;

                if Args.Verbose && (mod(ProcessedCount, 10) == 0 || ProcessedCount == 1)
                    printProgress(iProc, NumCellsToProcess, IndHTM, NsrcCell, StartTime, 0);
                end
            end
        end
    end

    % Copy the last HDF5 file to remote (if any processing was done)
    if ~isempty(CurrentHDFFile) && ~isempty(Args.TargetDir)
        FullPath = fullfile(Args.LocalDir, CurrentHDFFile);
        if isfile(FullPath)
            tools.os.copyFileOverNFS({FullPath}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
            if Args.Verbose
                fprintf('  Copied completed file: %s\n', CurrentHDFFile);
            end
        end
    end

    %----------------------------------------------------------------------
    % 6. FINALIZATION
    %----------------------------------------------------------------------

    if Args.Verbose
        fprintf('\n=== Finalization ===\n');
        fprintf('Processed: %d cells\n', ProcessedCount);
        fprintf('Skipped (existing): %d cells\n', SkippedCount);
        fprintf('Failed: %d cells\n', numel(FailedCells));
    end

    % Save HTM index file
    if Args.SaveInd
        if Args.Verbose
            fprintf('Saving HTM index...\n');
        end

        % Delete old index file if exists
        IndFileName = fullfile(Args.LocalDir, sprintf('%s_htm.hdf5', Args.CatName));
        if isfile(IndFileName)
            delete(IndFileName);
        end

        % Save HTM index using tracked Nsrc
        HDF5.save_htm_ind(HTM, IndFileName, sprintf('%s_HTM', Args.CatName), {}, Nsrc);

        if Args.Verbose
            fprintf('Saved HTM index: %s\n', IndFileName);
        end

        % Copy index file to remote
        if ~isempty(Args.TargetDir)
            tools.os.copyFileOverNFS({IndFileName}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true); 
        end

        % Copy ColCell file to remote
        ColCellFileName = fullfile(Args.LocalDir, sprintf('%s_htmColCell.mat', Args.CatName));
        if ~isempty(Args.TargetDir) && isfile(ColCellFileName)
            tools.os.copyFileOverNFS({ColCellFileName}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
        end
    end

    % Report completion
    ElapsedMin = toc(StartTime) / 60;
    if Args.Verbose
        fprintf('\n=== Completed ===\n');
        fprintf('Total time: %.1f minutes\n', ElapsedMin);
        fprintf('Total sources: %d\n', sum(Nsrc(:, 2)));
    end

    % Warn about failed cells
    if ~isempty(FailedCells)
        warning('%d cells failed. Re-run with Resume=true to retry.', numel(FailedCells));
    end
end


%==========================================================================
% HELPER FUNCTIONS
%==========================================================================

function Query = constructSpatialQuery(TableName, Columns, ColRA, ColDec, ...
                                        HTMCooDeg, CenterRA, CenterDec, RadiusDeg, ...
                                        QueryType, WhereClause)
    % Construct ADQL query with spatial constraint
    % Input  : - TableName: TAP table name (required)
    %          - Columns: columns to SELECT (required)
    %          - ColRA: RA column name in source table (required)
    %          - ColDec: Dec column name in source table (required)
    %          - HTMCooDeg: 3x2 matrix of [Long, Lat] in degrees (polygon only)
    %          - CenterRA: cone center RA in degrees (cone only)
    %          - CenterDec: cone center Dec in degrees (cone only)
    %          - RadiusDeg: cone radius in degrees (cone only)
    %          - QueryType: 'cone'|'polygon' (required)
    %          - WhereClause: additional WHERE conditions (can be empty)

    switch lower(QueryType)
        case 'cone'
            SpatialClause = sprintf('CIRCLE(''ICRS'',%.8f,%.8f,%.8f)', ...
                                    CenterRA, CenterDec, RadiusDeg);
        case 'polygon'
            % HTM triangle vertices (3 points)
            SpatialClause = sprintf('POLYGON(''ICRS'',%.8f,%.8f,%.8f,%.8f,%.8f,%.8f)', ...
                                    HTMCooDeg(1,1), HTMCooDeg(1,2), ...
                                    HTMCooDeg(2,1), HTMCooDeg(2,2), ...
                                    HTMCooDeg(3,1), HTMCooDeg(3,2));
        otherwise
            error('Unknown QueryType: %s. Use ''cone'' or ''polygon''.', QueryType);
    end

    if isempty(WhereClause)
        Query = sprintf('SELECT %s FROM %s WHERE 1=CONTAINS(POINT(''ICRS'',%s,%s),%s)', ...
                        Columns, TableName, ColRA, ColDec, SpatialClause);
    else
        Query = sprintf('SELECT %s FROM %s WHERE 1=CONTAINS(POINT(''ICRS'',%s,%s),%s) AND (%s)', ...
                        Columns, TableName, ColRA, ColDec, SpatialClause, WhereClause);
    end
end


function Exists = checkHTMExists(CatName, IndHTM, NfilesInHDF, LocalDir)
    % Check if an HTM cell already exists in HDF5 files
    [FileName, DataName] = HDF5.get_file_var_from_htmid(CatName, IndHTM, NfilesInHDF);
    FileName = fullfile(LocalDir, FileName);
    Exists = false;
    if isfile(FileName)
        try
            Info = h5info(FileName);
            Exists = any(strcmp({Info.Datasets.Name}, DataName));
        catch
            % File exists but can't be read - treat as not existing
        end
    end
end


function Nsrc = getHTMSourceCount(CatName, IndHTM, NfilesInHDF, LocalDir)
    % Get source count from an existing HTM cell in HDF5 file
    [FileName, DataName] = HDF5.get_file_var_from_htmid(CatName, IndHTM, NfilesInHDF);
    FileName = fullfile(LocalDir, FileName);
    Nsrc = 0;
    if isfile(FileName)
        try
            Info = h5info(FileName, ['/' DataName]);
            Nsrc = Info.Dataspace.Size(1);  % Number of rows = number of sources
        catch
            % Could not read - return 0
        end
    end
end


function [Data, ColNames] = tableToMatrix(T, ColRA, ColDec, TapUnits)
    % Convert MATLAB table to numeric matrix with RA/Dec in columns 1,2
    % Input  : - T: MATLAB table from TAP query
    %          - ColRA: RA column name
    %          - ColDec: Dec column name
    %          - TapUnits: coordinate units from TAP ('rad'|'deg')
    % Output : - Data: numeric matrix with RA/Dec in radians (columns 1,2)
    %          - ColNames: cell array of column names

    [Data, ColNames, ~, ~] = tableToMatrixWithInfo(T, ColRA, ColDec, TapUnits);
end


function [Data, ColNames, numericMask, reorderIdx] = tableToMatrixWithInfo(T, ColRA, ColDec, TapUnits)
    % Convert MATLAB table to numeric matrix with RA/Dec in columns 1,2
    % Also returns filtering info to apply same transformation to original column names
    % Input  : - T: MATLAB table from TAP query
    %          - ColRA: RA column name
    %          - ColDec: Dec column name
    %          - TapUnits: coordinate units from TAP ('rad'|'deg')
    % Output : - Data: numeric matrix with RA/Dec in radians (columns 1,2)
    %          - ColNames: cell array of column names (filtered and reordered)
    %          - numericMask: logical mask of which original columns are numeric
    %          - reorderIdx: indices showing how filtered columns were reordered

    ColNames = T.Properties.VariableNames;

    % Find RA/Dec columns (case-insensitive)
    idxRA = find(strcmpi(ColNames, ColRA), 1);
    idxDec = find(strcmpi(ColNames, ColDec), 1);

    if isempty(idxRA) || isempty(idxDec)
        error('Could not find RA column "%s" or Dec column "%s" in table', ColRA, ColDec);
    end

    % Keep only numeric columns (drop strings, cells, etc.)
    numericMask = varfun(@isnumeric, T, 'OutputFormat', 'uniform');
    if ~numericMask(idxRA) || ~numericMask(idxDec)
        error('RA column "%s" or Dec column "%s" is not numeric', ColRA, ColDec);
    end
    T = T(:, numericMask);
    ColNames = ColNames(numericMask);

    % Update RA/Dec indices after filtering
    idxRA = find(strcmpi(ColNames, ColRA), 1);
    idxDec = find(strcmpi(ColNames, ColDec), 1);

    % Convert table to matrix
    Data = table2array(T);

    % Reorder so RA=col1, Dec=col2
    if idxRA ~= 1 || idxDec ~= 2
        otherCols = setdiff(1:width(T), [idxRA, idxDec]);
        reorderIdx = [idxRA, idxDec, otherCols];
        Data = Data(:, reorderIdx);
        ColNames = ColNames(reorderIdx);
    else
        reorderIdx = 1:width(T);
    end

    % Convert to radians (always output radians for in_polysphere)
    if strcmpi(TapUnits, 'deg')
        Data(:, 1:2) = Data(:, 1:2) / constant.RAD;
    end
end


function T = queryWithRetry(Tap, Query, MaxRetries, RetryPauseSec, TapUrl, TimeoutSec, QueryMethod, WorkDir)
    % Execute TAP query with retry logic
    for attempt = 1:MaxRetries
        try
            T = Tap.query(Query, 'TapUrl', TapUrl, 'TimeoutSec', TimeoutSec, 'Method', QueryMethod, 'WorkDir', WorkDir);
            return;
        catch ME
            if attempt < MaxRetries
                fprintf('  Query failed (attempt %d/%d): %s\n', attempt, MaxRetries, ME.message);
                pause(RetryPauseSec);
            else
                rethrow(ME);
            end
        end
    end
end


function [NsrcCell, ColNames, QueryFailed] = processHTMCell(Tap, TableName, ColumnsStr, IndHTM, HTM, RAD, SearchRadiusDeg, Args)
    % Process a single HTM cell: query, filter, and save
    %
    % Output:
    %   NsrcCell    - Number of sources saved (0 if empty or failed)
    %   ColNames    - Column names from query (empty if failed)
    %   QueryFailed - true if query failed after retries

    NsrcCell = 0;
    ColNames = {};
    QueryFailed = false;

    % Get cell center in degrees
    MeanRA  = mean(HTM(IndHTM).coo(:,1));
    MeanDec = mean(HTM(IndHTM).coo(:,2));
    CenterRADeg  = MeanRA * RAD;
    CenterDecDeg = MeanDec * RAD;

    % Get HTM vertices in degrees for polygon query
    HTMCooDeg = HTM(IndHTM).coo * RAD;

    % Construct query (use source column names for spatial constraint)
    Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRASrc, Args.ColDecSrc, ...
                                   HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                   SearchRadiusDeg, Args.QueryType, Args.WhereClause);

    % Execute query with retry logic
    try
        T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                           Args.TapUrl, Args.TimeoutSec, Args.QueryMethod, Args.LocalDir);
    catch ME
        warning('VO:buildHTMfromTopCat:QueryFailed', ...
            'HTM %d: Query failed after %d retries: %s', IndHTM, Args.MaxRetries, char(ME.message));
        QueryFailed = true;
        return;
    end

    % Handle empty result
    if isempty(T) || height(T) == 0
        return;
    end

    % Convert table to matrix (radians for in_polysphere filtering)
    [Data, ColNames] = tableToMatrix(T, Args.ColRA, Args.ColDec, Args.TapUnits);

    % Filter sources to keep only those inside HTM triangle (for cone queries)
    if strcmpi(Args.QueryType, 'cone')
        CooRad = Data(:, [Args.ColRAOut, Args.ColDecOut]);
        Flag = celestial.htm.in_polysphere(CooRad, HTM(IndHTM).coo, 2);
        Data = Data(Flag, :);
    end

    % Apply post-processing: NullValue replacement, computed columns, drop columns
    [Data, ColNames] = applyPostProcessing(Data, ColNames, Args.NullValue, ...
                                            Args.ComputedColumns, Args.DropColumns);

    % Count sources
    NsrcCell = size(Data, 1);

    % Convert to output units and save to HDF5
    if NsrcCell > 0
        if strcmpi(Args.OutUnits, 'deg')
            DataOut = Data;
            DataOut(:, 1) = Data(:, 1) * RAD;  % RA rad->deg
            DataOut(:, 2) = Data(:, 2) * RAD;  % Dec rad->deg
        else
            DataOut = Data;  % Already in radians
        end
        [FileName, DataName] = HDF5.get_file_var_from_htmid(Args.CatName, IndHTM, Args.NfilesInHDF);
        FileName = fullfile(Args.LocalDir, FileName);
        HDF5.save_cat(FileName, DataName, DataOut, Args.ColDecOut, Args.IndStep);
    end
end


function printProgress(Ihtm, Nhtm, IndHTM, NsrcCell, StartTime, SkippedCount)
    % Print progress with ETA
    Elapsed = toc(StartTime);
    Processed = Ihtm - SkippedCount;
    if Processed > 0
        Rate = Processed / Elapsed;
        Remaining = Nhtm - Ihtm;
        ETA = Remaining / Rate;
        fprintf('[%d/%d] HTM %d: %d sources (%.1f cells/min, ETA: %.1f min)\n', ...
                Ihtm, Nhtm, IndHTM, NsrcCell, Rate * 60, ETA / 60);
    else
        fprintf('[%d/%d] HTM %d: %d sources\n', Ihtm, Nhtm, IndHTM, NsrcCell);
    end
end


function ColNames = parseCSVHeader(headerLine)
    % Parse CSV header line, properly handling quoted fields
    % Input  : - headerLine: string containing CSV header
    % Output : - ColNames: cell array of column names

    ColNames = {};
    headerLine = char(headerLine);
    pos = 1;
    len = length(headerLine);

    while pos <= len
        if headerLine(pos) == '"'
            % Quoted field - find matching close quote
            pos = pos + 1;  % skip opening quote
            fieldStart = pos;
            while pos <= len
                if headerLine(pos) == '"'
                    if pos < len && headerLine(pos+1) == '"'
                        % Escaped quote - skip both
                        pos = pos + 2;
                    else
                        % End of quoted field
                        break;
                    end
                else
                    pos = pos + 1;
                end
            end
            field = headerLine(fieldStart:pos-1);
            % Unescape double quotes
            field = strrep(field, '""', '"');
            ColNames{end+1} = field; %#ok<AGROW>
            pos = pos + 1;  % skip closing quote
            % Skip comma if present
            if pos <= len && headerLine(pos) == ','
                pos = pos + 1;
            end
        else
            % Unquoted field - find comma or end
            fieldStart = pos;
            while pos <= len && headerLine(pos) ~= ','
                pos = pos + 1;
            end
            field = strtrim(headerLine(fieldStart:pos-1));
            ColNames{end+1} = field; %#ok<AGROW>
            % Skip comma
            if pos <= len && headerLine(pos) == ','
                pos = pos + 1;
            end
        end
    end
end


function Level = autoSelectLevel(Tap, TableName, TapUrl, MaxSrcPerCell, LevelRange, WhereClause, TimeoutSec, Verbose)
    % Automatically select HTM level based on estimated catalog size

    if Verbose
        fprintf('Auto-selecting HTM level...\n');
    end

    % Query total count
    if isempty(WhereClause)
        Q = sprintf('SELECT COUNT(*) as cnt FROM %s', TableName);
    else
        Q = sprintf('SELECT COUNT(*) as cnt FROM %s WHERE %s', TableName, WhereClause);
    end

    try
        T = Tap.query(Q, 'TapUrl', TapUrl, 'TimeoutSec', TimeoutSec);

        % Handle different result formats
        if istable(T)
            if ismember('cnt', T.Properties.VariableNames)
                TotalSrc = T.cnt(1);
            else
                TotalSrc = T{1, 1};
            end
        else
            TotalSrc = T(1);
        end
    catch ME
        warning('VO:buildHTMfromTopCat:CountQueryFailed', ...
            'Could not query catalog size: %s. Using default level 7.', char(ME.message));
        Level = 7;
        return;
    end

    if Verbose
        fprintf('Total sources in catalog: %.2e\n', TotalSrc);
    end

    % Find appropriate level within specified range
    % htm_build(Level) creates levels 0 to Level-1, so level Level-1 has 8*4^(Level-1) cells
    for Level = LevelRange(1):LevelRange(2)
        Ncells = 8 * 4^(Level - 1);
        SrcPerCell = TotalSrc / Ncells;
        if SrcPerCell < MaxSrcPerCell
            break;
        end
    end

    if Verbose
        fprintf('Auto-selected HTM level %d (~%.0f sources/cell, %d cells)\n', ...
                Level, SrcPerCell, Ncells);
    end
end


function [Data, ColNames] = applyPostProcessing(Data, ColNames, NullValue, ComputedColumns, DropColumns)
    % Apply post-processing to data matrix: NullValue->NaN, computed columns, drop columns
    % Input  : - Data: numeric matrix
    %          - ColNames: cell array of column names
    %          - NullValue: value to replace with NaN (can be empty)
    %          - ComputedColumns: cell array of {NewName, Col1, Col2, Op} or Nx4 cell
    %          - DropColumns: cell array of column names to drop
    % Output : - Data: processed matrix with computed columns appended
    %          - ColNames: updated column names

    if isempty(Data)
        return;
    end

    % 1. Replace NullValue with NaN
    if ~isempty(NullValue)
        Data(Data == NullValue) = NaN;
    end

    % 2. Compute new columns
    if ~isempty(ComputedColumns)
        % Handle single row: {'name', 'col1', 'col2', 'op'}
        if ~iscell(ComputedColumns{1})
            ComputedColumns = {ComputedColumns};
        end

        for iComp = 1:numel(ComputedColumns)
            compDef = ComputedColumns{iComp};
            newName = compDef{1};
            col1Name = compDef{2};
            col2Name = compDef{3};
            op = compDef{4};

            % Find column indices
            idx1 = find(strcmpi(ColNames, col1Name), 1);
            idx2 = find(strcmpi(ColNames, col2Name), 1);

            if isempty(idx1)
                warning('ComputedColumns: column "%s" not found, skipping', col1Name);
                continue;
            end
            if isempty(idx2)
                warning('ComputedColumns: column "%s" not found, skipping', col2Name);
                continue;
            end

            % Compute new column
            switch lower(op)
                case 'minus'
                    newCol = Data(:, idx1) - Data(:, idx2);
                case 'plus'
                    newCol = Data(:, idx1) + Data(:, idx2);
                case 'times'
                    newCol = Data(:, idx1) .* Data(:, idx2);
                case 'divide'
                    newCol = Data(:, idx1) ./ Data(:, idx2);
                otherwise
                    warning('ComputedColumns: unknown operation "%s", skipping', op);
                    continue;
            end

            % Append new column
            Data = [Data, newCol]; %#ok<AGROW>
            ColNames = [ColNames, {newName}]; %#ok<AGROW>
        end
    end

    % 3. Drop columns
    if ~isempty(DropColumns)
        if ischar(DropColumns)
            DropColumns = {DropColumns};
        end

        keepMask = true(1, numel(ColNames));
        for iDrop = 1:numel(DropColumns)
            idx = find(strcmpi(ColNames, DropColumns{iDrop}), 1);
            if ~isempty(idx)
                keepMask(idx) = false;
            end
        end

        Data = Data(:, keepMask);
        ColNames = ColNames(keepMask);
    end
end


function ColNames = applyColCellPostProcessing(ColNames, ComputedColumns, DropColumns)
    % Update column names to reflect computed and dropped columns (for ColCell file)
    % Input  : - ColNames: cell array of column names
    %          - ComputedColumns: cell array of {NewName, Col1, Col2, Op} or Nx4 cell
    %          - DropColumns: cell array of column names to drop
    % Output : - ColNames: updated column names

    % 1. Add computed column names
    if ~isempty(ComputedColumns)
        % Handle single row: {'name', 'col1', 'col2', 'op'}
        if ~iscell(ComputedColumns{1})
            ComputedColumns = {ComputedColumns};
        end

        for iComp = 1:numel(ComputedColumns)
            compDef = ComputedColumns{iComp};
            newName = compDef{1};
            ColNames = [ColNames, {newName}]; %#ok<AGROW>
        end
    end

    % 2. Drop columns
    if ~isempty(DropColumns)
        if ischar(DropColumns)
            DropColumns = {DropColumns};
        end

        keepMask = true(1, numel(ColNames));
        for iDrop = 1:numel(DropColumns)
            idx = find(strcmpi(ColNames, DropColumns{iDrop}), 1);
            if ~isempty(idx)
                keepMask(idx) = false;
            end
        end

        ColNames = ColNames(keepMask);
    end
end


function [Data, QueryFailed] = downloadHTMCell(TableName, ColumnsStr, IndHTM, HTM, RAD, SearchRadiusDeg, Args)
    % Download and process a single HTM cell without writing to file
    % Used for parallel batch processing
    % Output : - Data: processed numeric matrix ready for HDF5 (empty if failed/no sources)
    %          - QueryFailed: true if query failed after retries

    Data = [];
    QueryFailed = false;

    % Create TopCat object for TAP queries (each worker needs its own)
    Tap = VO.TopCat;

    % Get cell center in degrees
    MeanRA  = mean(HTM(IndHTM).coo(:,1));
    MeanDec = mean(HTM(IndHTM).coo(:,2));
    CenterRADeg  = MeanRA * RAD;
    CenterDecDeg = MeanDec * RAD;

    % Get HTM vertices in degrees for polygon query
    HTMCooDeg = HTM(IndHTM).coo * RAD;

    % Construct query (use source column names for spatial constraint)
    Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRASrc, Args.ColDecSrc, ...
                                   HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                   SearchRadiusDeg, Args.QueryType, Args.WhereClause);

    % Execute query with retry logic
    try
        T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                           Args.TapUrl, Args.TimeoutSec, Args.QueryMethod, Args.LocalDir);
    catch ME
        warning('VO:buildHTMfromTopCat:QueryFailed', ...
            'HTM %d: Query failed after %d retries: %s', IndHTM, Args.MaxRetries, char(ME.message));
        QueryFailed = true;
        return;
    end

    % Handle empty result
    if isempty(T) || height(T) == 0
        return;
    end

    % Convert table to matrix (radians for in_polysphere filtering)
    [Data, ColNames] = tableToMatrix(T, Args.ColRA, Args.ColDec, Args.TapUnits);

    % Filter sources to keep only those inside HTM triangle (for cone queries)
    if strcmpi(Args.QueryType, 'cone')
        CooRad = Data(:, [Args.ColRAOut, Args.ColDecOut]);
        Flag = celestial.htm.in_polysphere(CooRad, HTM(IndHTM).coo, 2);
        Data = Data(Flag, :);
    end

    % Apply post-processing: NullValue replacement, computed columns, drop columns
    [Data, ~] = applyPostProcessing(Data, ColNames, Args.NullValue, ...
                                     Args.ComputedColumns, Args.DropColumns);

    % Convert to output units
    if ~isempty(Data) && strcmpi(Args.OutUnits, 'deg')
        Data(:, 1) = Data(:, 1) * RAD;  % RA rad->deg
        Data(:, 2) = Data(:, 2) * RAD;  % Dec rad->deg
    end
end


function NsrcCell = writeHTMCell(Data, IndHTM, Args)
    % Write processed data to HDF5 file
    % Used for sequential write phase in parallel batch processing
    % Output : - NsrcCell: number of sources written

    NsrcCell = size(Data, 1);

    if NsrcCell > 0
        [FileName, DataName] = HDF5.get_file_var_from_htmid(Args.CatName, IndHTM, Args.NfilesInHDF);
        FileName = fullfile(Args.LocalDir, FileName);
        HDF5.save_cat(FileName, DataName, Data, Args.ColDecOut, Args.IndStep);
    end
end


function [Data, QueryFailed] = downloadHTMCellLight(TableName, ColumnsStr, IndHTM, cellCoo, RAD, SearchRadiusDeg, Args)
    % Lightweight version of downloadHTMCell for parallel processing
    % Takes only cell coordinates instead of full HTM structure to reduce memory
    % Input  : - TableName, ColumnsStr: query parameters
    %          - IndHTM: HTM cell index (for error messages)
    %          - cellCoo: 3x2 matrix of cell vertices [RA, Dec] in radians
    %          - RAD: degrees per radian constant
    %          - SearchRadiusDeg: cone search radius in degrees
    %          - Args: argument structure
    % Output : - Data: processed numeric matrix ready for HDF5 (empty if failed/no sources)
    %          - QueryFailed: true if query failed after retries

    Data = [];
    QueryFailed = false;

    % Create TopCat object for TAP queries (each worker needs its own)
    Tap = VO.TopCat;

    % Get cell center in degrees
    MeanRA  = mean(cellCoo(:,1));
    MeanDec = mean(cellCoo(:,2));
    CenterRADeg  = MeanRA * RAD;
    CenterDecDeg = MeanDec * RAD;

    % Get HTM vertices in degrees for polygon query
    HTMCooDeg = cellCoo * RAD;

    % Construct query (use source column names for spatial constraint)
    Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRASrc, Args.ColDecSrc, ...
                                   HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                   SearchRadiusDeg, Args.QueryType, Args.WhereClause);

    % Execute query with retry logic
    try
        T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                           Args.TapUrl, Args.TimeoutSec, Args.QueryMethod, Args.LocalDir);
    catch ME
        warning('VO:buildHTMfromTopCat:QueryFailed', ...
            'HTM %d: Query failed after %d retries: %s', IndHTM, Args.MaxRetries, char(ME.message));
        QueryFailed = true;
        return;
    end

    % Handle empty result
    if isempty(T) || height(T) == 0
        return;
    end

    % Convert table to matrix (radians for in_polysphere filtering)
    [Data, ColNames] = tableToMatrix(T, Args.ColRA, Args.ColDec, Args.TapUnits);

    % Filter sources to keep only those inside HTM triangle (for cone queries)
    if strcmpi(Args.QueryType, 'cone')
        CooRad = Data(:, [Args.ColRAOut, Args.ColDecOut]);
        Flag = celestial.htm.in_polysphere(CooRad, cellCoo, 2);
        Data = Data(Flag, :);
    end

    % Apply post-processing: NullValue replacement, computed columns, drop columns
    [Data, ~] = applyPostProcessing(Data, ColNames, Args.NullValue, ...
                                     Args.ComputedColumns, Args.DropColumns);

    % Convert to output units
    if ~isempty(Data) && strcmpi(Args.OutUnits, 'deg')
        Data(:, 1) = Data(:, 1) * RAD;  % RA rad->deg
        Data(:, 2) = Data(:, 2) * RAD;  % Dec rad->deg
    end
end
