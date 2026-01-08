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
%                            'VizieR TAP'). Used to resolve TapUrl via
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
%            'ColRA'       - RA column name in source table. Default is 'ra'.
%            'ColDec'      - Dec column name in source table. Default is 'dec'.
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

    % Download VizieR catalog (VizieR does not support polygon, uses cone)
    Nsrc = VO.prep.buildHTMfromTopCat('"II/349/ps1"', ...
              'TapName', 'VizieR TAP', ...
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
%}

    arguments
        TableName                           % TAP table name
        Args.TapUrl           = []          % TAP service URL (or use TapName)
        Args.TapName          = []          % TAP service name (e.g., 'VizieR TAP')
        Args.CatName          = ''          % Output catalog base name
        Args.Columns          = '*'         % Columns to SELECT (string or cell array)
        Args.WhereClause      = ''          % Additional WHERE conditions
        Args.ColRA            = 'ra'        % RA column name in source table
        Args.ColDec           = 'dec'       % Dec column name in source table
        Args.ColRAOut         = 1           % RA column index in output
        Args.ColDecOut        = 2           % Dec column index in output
        Args.OutUnits         = 'rad'       % Output coordinate units: 'rad'|'deg'
        Args.TapUnits         = 'deg'       % TAP input coordinate units: 'rad'|'deg'
        Args.HTM_Level        = 7           % HTM level (or 'auto' for automatic)
        Args.AutoLevelMaxSrc  = 1e6         % Max sources per cell for auto-level
        Args.AutoLevelRange   = [4, 10]    % [min, max] HTM level range for auto-selection
        Args.NfilesInHDF      = 100         % HTM cells per HDF5 file
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
    % 2. AUTO-LEVEL SELECTION (if requested)
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

    % Track column names (will be set from first successful query)
    ColNamesDetected = {};

    StartTime = tic;
    ProcessedCount = 0;
    SkippedCount = 0;
    FailedCells = [];

    %----------------------------------------------------------------------
    % 4. MAIN LOOP: Process each HTM cell
    %----------------------------------------------------------------------

    if Args.Verbose
        fprintf('\nStarting HTM cell processing...\n');
    end

    for Ihtm = 1:Nhtm
        IndHTM = ListIndexHTM(Ihtm);

        % Get HTM cell center
        MeanRA  = mean(HTM(IndHTM).coo(:,1));   % radians
        MeanDec = mean(HTM(IndHTM).coo(:,2));   % radians

        % Determine cell status
        OutsideRange = MeanRA < Args.RARange(1) || MeanRA >= Args.RARange(2) || ...
                       MeanDec < Args.DecRange(1) || MeanDec >= Args.DecRange(2);
        AlreadyExists = Args.Resume && checkHTMExists(Args.CatName, IndHTM, Args.NfilesInHDF, Args.LocalDir);

        if OutsideRange
            % Skip: outside requested RA/Dec range
            Nsrc(Ihtm, :) = [IndHTM, 0];

        elseif AlreadyExists
            % Skip: already processed (resume mode)
            SkippedCount = SkippedCount + 1;
            if Args.Verbose && mod(SkippedCount, 100) == 0
                fprintf('  Skipped %d existing cells...\n', SkippedCount);
            end

        else
            % Process this HTM cell
            [NsrcCell, ColNames, QueryFailed] = processHTMCell( ...
                Tap, TableName, ColumnsStr, IndHTM, HTM, RAD, SearchRadiusDeg, Args);

            % Update tracking variables
            if QueryFailed
                FailedCells = [FailedCells, IndHTM]; %#ok<AGROW>
                Nsrc(Ihtm, :) = [IndHTM, 0];
            else
                Nsrc(Ihtm, :) = [IndHTM, NsrcCell];
                ProcessedCount = ProcessedCount + 1;

                % Store column names from first successful query
                if isempty(ColNamesDetected) && ~isempty(ColNames)
                    ColNamesDetected = ColNames;
                end

                % Print progress
                if Args.Verbose && (mod(ProcessedCount, 10) == 0 || ProcessedCount == 1)
                    printProgress(Ihtm, Nhtm, IndHTM, NsrcCell, StartTime, SkippedCount);
                end
            end
        end
    end

    %----------------------------------------------------------------------
    % 5. FINALIZATION
    %----------------------------------------------------------------------

    if Args.Verbose
        fprintf('\n=== Finalization ===\n');
        fprintf('Processed: %d cells\n', ProcessedCount);
        fprintf('Skipped (existing): %d cells\n', SkippedCount);
        fprintf('Failed: %d cells\n', numel(FailedCells));
    end

    % Save index file and column metadata
    if Args.SaveInd
        if Args.Verbose
            fprintf('Saving HTM index and column metadata...\n');
        end

        % Prepare column cell
        if isempty(Args.ColCell) && ~isempty(ColNamesDetected)
            Args.ColCell = ColNamesDetected;
        end

        % Delete old index file if exists
        IndFileName = fullfile(Args.LocalDir, sprintf('%s_htm.hdf5', Args.CatName));
        if isfile(IndFileName)
            delete(IndFileName);
        end

        % Save HTM index using tracked Nsrc
        HDF5.save_htm_ind(HTM, IndFileName, [], {}, Nsrc);

        % Copy index file to remote directory if specified
        if ~isempty(Args.TargetDir)
            tools.os.copyFileOverNFS({IndFileName}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
        end

        % Save column metadata
        if ~isempty(Args.ColCell)
            ColCellPath = fullfile(Args.LocalDir, Args.CatName);
            HDF5.save_cat_colcell(ColCellPath, Args.ColCell, Args.ColUnits);
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


function [Data, ColNames] = tableToMatrix(T, ColRA, ColDec, TapUnits)
    % Convert MATLAB table to numeric matrix with RA/Dec in columns 1,2
    % Input  : - T: MATLAB table from TAP query
    %          - ColRA: RA column name
    %          - ColDec: Dec column name
    %          - TapUnits: coordinate units from TAP ('rad'|'deg')
    % Output : - Data: numeric matrix with RA/Dec in radians (columns 1,2)
    %          - ColNames: cell array of column names

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
        Data = Data(:, [idxRA, idxDec, otherCols]);
        ColNames = ColNames([idxRA, idxDec, otherCols]);
    end

    % Convert to radians (always output radians for in_polysphere)
    if strcmpi(TapUnits, 'deg')
        Data(:, 1:2) = Data(:, 1:2) / constant.RAD;
    end
end


function T = queryWithRetry(Tap, Query, MaxRetries, RetryPauseSec, TapUrl, TimeoutSec, QueryMethod)
    % Execute TAP query with retry logic
    for attempt = 1:MaxRetries
        try
            T = Tap.query(Query, 'TapUrl', TapUrl, 'TimeoutSec', TimeoutSec, 'Method', QueryMethod, 'WorkDir', '/home/dana/tmp/');
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

    % Construct query
    Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRA, Args.ColDec, ...
                                   HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                   SearchRadiusDeg, Args.QueryType, Args.WhereClause);

    % Execute query with retry logic
    try
        T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                           Args.TapUrl, Args.TimeoutSec, Args.QueryMethod);
    catch ME
        warning('VO:buildHTMfromTopCat:QueryFailed', ...
            'HTM %d: Query failed after %d retries: %s', IndHTM, Args.MaxRetries, ME.message);
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

        % Copy to remote directory if specified
        if ~isempty(Args.TargetDir)
            tools.os.copyFileOverNFS({FileName}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
        end
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
            'Could not query catalog size: %s. Using default level 7.', ME.message);
        Level = 7;
        return;
    end

    if Verbose
        fprintf('Total sources in catalog: %.2e\n', TotalSrc);
    end

    % Find appropriate level within specified range
    for Level = LevelRange(1):LevelRange(2)
        Ncells = 8 * 4^Level;
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
