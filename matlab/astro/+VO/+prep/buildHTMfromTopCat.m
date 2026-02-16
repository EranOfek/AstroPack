function Nsrc = buildHTMfromTopCat(TableName, Args)
% Build an HTM catalog in HDF5 format by downloading in segments via TAP
% Package: VO.prep
% Description: Build an HTM catalog from large online sources by downloading
%              data in HTM-cell-sized segments via TAP protocol. Supports
%              two query strategies selected automatically:
%              1) Distribute-down: queries at a coarser HTM level, then
%                 distributes sources into finer output cells via
%                 in_polysphere. Used when QueryLevel <= HTM_Level.
%              2) Aggregate-up: queries at a finer HTM level (required when
%                 the TAP cone-search radius limit demands smaller cells
%                 than the desired output resolution), then aggregates
%                 results into coarser output cells with deduplication.
%                 Activated automatically when MaxConeRadiusDeg requires
%                 QueryLevel > HTM_Level.
%              Parallel mode uses parfeval+fetchNext for straggler-free
%              processing (no batch synchronization).
% Input  :   - TableName: TAP table name (e.g., 'gaiaedr3.gaia_source' or
%                         '"II/349/ps1"' for VizieR catalogs with special characters).
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
%            'HTM_Level'   - Output HTM level or 'auto' for automatic
%                            selection based on catalog size. This is
%                            always the output resolution; QueryLevel can
%                            independently exceed it when MaxConeRadiusDeg
%                            requires finer queries (aggregate-up mode).
%                            Default is 7.
%            'AutoLevelMaxSrc' - Max sources per HTM segment for auto-level
%                            selection. Default is 1e6.
%            'AutoLevelRange' - [min, max] HTM level range for auto-selection.
%                            Default is [4, 10].
%            'NcatInFile' - Number of HTM cells per HDF5 file. Default is 100.
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
%            'NumWorkers'  - Number of parallel workers for downloading.
%                            0 = sequential processing (default, original behavior).
%                            >0 = parallel download via parfeval+fetchNext.
%                            Requires Parallel Computing Toolbox.
%            'QueryLevel'  - HTM level at which TAP queries are issued.
%                            'auto' selects the coarsest level where max
%                            centroid-to-vertex distance fits MaxConeRadiusDeg.
%                            Can exceed HTM_Level (triggers aggregate-up mode).
%                            Can also be an integer level index. Default is 'auto'.
%            'MaxConeRadiusDeg' - Maximum cone search radius in degrees
%                            (TAP service limit). Default is 0.25.
% Output :   - Nx2 matrix of [HTM_Index, Nsrc] with source counts per cell.
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
              'TapUrl', 'https://tapvizier.cds.unistra.fr/TAPVizieR/tap', ...
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

  
   % PS1DR2 real usecase with parallel downloading and NullValue replacement and computed columns
    TableName = [ ...
      'dbo.meanobjectview AS m ' ...
      'LEFT OUTER JOIN dbo.forcedmeanobject AS f ON m.objid = f.objid'];
    Columns = strjoin({ ...
    'm.ramean AS RA', 'm.decmean AS Dec', 'm.rameanerr AS raerr', 'm.decmeanerr AS decerr', ...
    'm.objinfoflag', 'm.qualityflag', 'm.epochmean', 'm.posmeanchisq', ...
    'm.gqfperfect', 'm.gmeanpsfmag', 'm.gmeanpsfmagerr', 'm.gmeanpsfmagstd', ...
    'm.gmeanpsfmagnpt', 'm.gmeanpsfmagmin', 'm.gmeanpsfmagmax', 'm.gmeankronmag', ...
    'm.rqfperfect', 'm.rmeanpsfmag', 'm.rmeanpsfmagerr', 'm.rmeanpsfmagstd', ...
    'm.rmeanpsfmagnpt', 'm.rmeanpsfmagmin', 'm.rmeanpsfmagmax', 'm.rmeankronmag', ...
    'm.iqfperfect', 'm.imeanpsfmag', 'm.imeanpsfmagerr', 'm.imeanpsfmagstd', ...
    'm.imeanpsfmagnpt', 'm.imeanpsfmagmin', 'm.imeanpsfmagmax', 'm.imeankronmag', ...
    'm.zqfperfect', 'm.zmeanpsfmag', 'm.zmeanpsfmagerr', 'm.zmeanpsfmagstd', ...
    'm.zmeanpsfmagnpt', 'm.zmeanpsfmagmin', 'm.zmeanpsfmagmax', ...
    'm.yqfperfect', 'm.ymeanpsfmag', 'm.ymeanpsfmagerr', 'm.ymeanpsfmagstd', ...
    'm.ymeanpsfmagnpt', 'm.ymeanpsfmagmin', 'm.ymeanpsfmagmax', ...
    'f.gfmeanmagr5', 'f.gfmeanmagr5err', 'f.rfmeanmagr5', 'f.rfmeanmagr5err', ...
    'f.ifmeanmagr5', 'f.ifmeanmagr5err', 'f.zfmeanmagr5', 'f.zfmeanmagr5err', ...
    'f.yfmeanmagr5', 'f.yfmeanmagr5err',}, ', ');
    CompCols = { ...
    {'g_delta_psf_kron', 'gmeanpsfmag', 'gmeankronmag', 'minus'}; ...
    {'r_delta_psf_kron', 'rmeanpsfmag', 'rmeankronmag', 'minus'}; ...
    {'i_delta_psf_kron', 'imeanpsfmag', 'imeankronmag', 'minus'}};
    DropCols = {'gmeankronmag', 'rmeankronmag', 'imeankronmag'};
    Nsrc = VO.prep.buildHTMfromTopCat(TableName, ...
    'TapUrl', 'https://mast.stsci.edu/vo-tap/api/v0.1/ps1dr2',...
    'CatName', 'PS1DR2',...
    'HTM_Level', 9,...
    'MaxConeRadiusDeg', 0.25,...
    'ColRA', 'RA', 'ColDec', 'Dec',...
    'ColRASrc', 'm.ramean', 'ColDecSrc', 'm.decmean',...
    'Columns', Columns,...
    'NullValue', -999,...
    'ComputedColumns', CompCols,...
    'DropColumns', DropCols,...
    'RARange', [0, 1] .* pi/180, 'DecRange', [0, 1] .* pi/180,...
    'WhereClause', 'm.ndetections > 2', ...
    'TargetDir', '/euclid/catsHTM/NewCats/PS1DR2/',...
    'LocalDir', '~/tmp/',...
    'NumWorkers', 4,...
    'Verbose', true);
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
        Args.NcatInFile      = 100        % HTM cells per HDF5 file (matches catsHTM default)
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
        Args.QueryLevel       = 'auto'           % Query HTM level ('auto' or integer)
        Args.MaxConeRadiusDeg = 2.0         % Maximum cone search radius, deg (TAP service limit)
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

    % Remove underscores from CatName for catsHTM compatibility
    % (catsHTM splits filenames by '_' to derive variable names)
    Args.CatName = strrep(Args.CatName, '_', '');

    % Append CatName as subdirectory to TargetDir so files go to
    % e.g., /euclid/catsHTM/NewCats/GALEXAIS/ (no underscores in path)
    % Skip if TargetDir already ends with CatName.
    if ~isempty(Args.TargetDir)
        CleanDir = regexprep(Args.TargetDir, '[/\\]+$', '');
        [~, LastDir] = fileparts(CleanDir);
        if ~strcmp(LastDir, Args.CatName)
            Args.TargetDir = fullfile(Args.TargetDir, Args.CatName);
        end
    end

    % Set source column names for spatial query (default to output names)
    if isempty(Args.ColRASrc)
        Args.ColRASrc = Args.ColRA;
    end
    if isempty(Args.ColDecSrc)
        Args.ColDecSrc = Args.ColDec;
    end

    % Auto-detect source column names from aliases in Columns spec
    % e.g., 'RAJ2000 AS RA' with ColRA='ra' → ColRASrc='RAJ2000'
    if iscell(Args.Columns)
        for iCol = 1:numel(Args.Columns)
            Tokens = regexpi(Args.Columns{iCol}, '^\s*(.+?)\s+AS\s+(\S+)\s*$', 'tokens');
            if ~isempty(Tokens)
                SrcName = Tokens{1}{1};
                AliasName = Tokens{1}{2};
                if strcmpi(AliasName, Args.ColRASrc)
                    Args.ColRASrc = SrcName;
                end
                if strcmpi(AliasName, Args.ColDecSrc)
                    Args.ColDecSrc = SrcName;
                end
            end
        end
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
                Cmd = sprintf('java -Xmx1g -jar "%s" tapquery tapurl="%s" language=ADQL adql="%s" omode=out ofmt=csv out="%s" sync=true 2>&1', ...
                    JarPath, TapUrlClean, AdqlEsc, TempCsvFile);

                [Status, ~] = system(Cmd);

                if Status == 0 && isfile(TempCsvFile)
                    % Read original column names from CSV header
                    Fid = fopen(TempCsvFile, 'r');
                    HeaderLine = fgetl(Fid);
                    fclose(Fid);

                    % Parse CSV header properly (handle quoted fields that may contain commas)
                    OriginalColNames = parseCSVHeader(HeaderLine);

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
        [~, Args.ColCell, NumericMask, ReorderIdx] = tableToMatrixWithInfo(T, Args.ColRA, Args.ColDec, Args.TapUnits);
        % Force RA/Dec names to match user-specified ColRA/ColDec (MATLAB may change case)
        Args.ColCell{1} = Args.ColRA;
        Args.ColCell{2} = Args.ColDec;
        if Args.Verbose
            fprintf('Detected %d columns (MATLAB names): %s\n', numel(Args.ColCell), strjoin(Args.ColCell, ', '));
        end

        % Auto-detect column units from TAP VOTable metadata
        if isempty(Args.ColUnits)
            TapColUnits = extractUnitsFromVOTable(SampleQuery, Args.TapUrl, ...
                                                   Args.LocalDir, Args.Verbose);
            if ~isempty(TapColUnits) && numel(TapColUnits) == numel(NumericMask)
                % Apply same filtering (numeric only) and reordering as column names
                FilteredUnits = TapColUnits(NumericMask);
                Args.ColUnits = FilteredUnits(ReorderIdx);
            else
                % Fallback: empty string units with correct length
                Args.ColUnits = repmat({''}, 1, numel(Args.ColCell));
            end
        end

        % If user specified '*', rebuild ColumnsStr using original TAP column names
        % to ensure consistent columns across all HTM cell queries
        if strcmp(ColumnsStr, '*')
            if ~isempty(OriginalColNames) && numel(OriginalColNames) == numel(NumericMask)
                % Apply same filtering (numeric only) and reordering to original names
                OriginalColNamesFiltered = OriginalColNames(NumericMask);
                OriginalColNamesReordered = OriginalColNamesFiltered(ReorderIdx);

                % Filter out VizieR internal columns (starting with '_') as they cannot be queried
                VizierInternalMask = cellfun(@(x) ~isempty(x) && x(1) == '_', OriginalColNamesReordered);
                if any(VizierInternalMask)
                    if Args.Verbose
                        fprintf('Excluding %d VizieR internal columns (starting with _).\n', sum(VizierInternalMask));
                    end
                    OriginalColNamesReordered = OriginalColNamesReordered(~VizierInternalMask);
                    Args.ColCell = Args.ColCell(~VizierInternalMask);
                    if numel(Args.ColUnits) == numel(VizierInternalMask)
                        Args.ColUnits = Args.ColUnits(~VizierInternalMask);
                    end
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

    % Override RA/Dec units to match output coordinate system (radians)
    if ~isempty(Args.ColUnits)
        RaIdx = find(strcmpi(Args.ColCell, Args.ColRA), 1);
        if isempty(RaIdx) && isnumeric(Args.ColRAOut), RaIdx = Args.ColRAOut; end
        DecIdx = find(strcmpi(Args.ColCell, Args.ColDec), 1);
        if isempty(DecIdx) && isnumeric(Args.ColDecOut), DecIdx = Args.ColDecOut; end
        if ~isempty(RaIdx) && RaIdx <= numel(Args.ColUnits)
            Args.ColUnits{RaIdx} = Args.OutUnits;
        end
        if ~isempty(DecIdx) && DecIdx <= numel(Args.ColUnits)
            Args.ColUnits{DecIdx} = Args.OutUnits;
        end
    end

    % Update ColCell to reflect computed columns and dropped columns
    % Save pre-processed ColCell for ColUnits post-processing
    PreProcColCell = Args.ColCell;
    Args.ColCell = applyColCellPostProcessing(Args.ColCell, Args.ComputedColumns, Args.DropColumns);

    % Update ColUnits to match (add computed column units, remove dropped column units)
    if ~isempty(Args.ColUnits)
        Args.ColUnits = applyColUnitsPostProcessing(Args.ColUnits, Args.ComputedColumns, Args.DropColumns, PreProcColCell);
    end

    if Args.Verbose && (~isempty(Args.ComputedColumns) || ~isempty(Args.DropColumns))
        fprintf('Final output columns (%d): %s\n', numel(Args.ColCell), strjoin(Args.ColCell, ', '));
    end
    if Args.Verbose && ~isempty(Args.ColUnits)
        fprintf('Column units: %s\n', strjoin(Args.ColUnits, ', '));
    end

    % Save ColCell file immediately
    ColCellPath = fullfile(Args.LocalDir, Args.CatName);
    HDF5.save_cat_colcell(ColCellPath, Args.ColCell, Args.ColUnits);
    if Args.Verbose
        fprintf('Saved column metadata: %s_htmColCell.mat\n', ColCellPath);
    end

    %----------------------------------------------------------------------
    % 3. AUTO-LEVEL SELECTION AND HTM BUILD
    %----------------------------------------------------------------------

    if ischar(Args.HTM_Level) && strcmpi(Args.HTM_Level, 'auto')
        Args.HTM_Level = autoSelectLevel(Tap, TableName, Args.TapUrl, ...
                                          Args.AutoLevelMaxSrc, Args.AutoLevelRange, ...
                                          Args.WhereClause, Args.TimeoutSec, Args.Verbose);
    end

    % Determine if partial HTM build is beneficial
    % For non-full-sky requests at high HTM levels, building the full tree
    % is very slow (millions of cells). Instead, build to a moderate depth
    % and expand only the in-range cells on-the-fly.
    IsFullSky = (Args.DecRange(1) <= -pi/2 + 0.01) && (Args.DecRange(2) >= pi/2 - 0.01) && ...
                (Args.RARange(1) <= 0.01) && (Args.RARange(2) >= 2*pi - 0.01);
    % Compute minimum build level to contain the query level.
    % When QueryLevel is explicit, use it directly — MaxConeRadiusDeg only
    % influences the build depth when QueryLevel is 'auto'.
    if ischar(Args.QueryLevel) && strcmpi(Args.QueryLevel, 'auto')
        % At LevelHTM index L (depth L-1), side = 90/2^(L-1) deg.
        % Max centroid-to-vertex distance <= side, so need side*1.05 <= MaxConeRadiusDeg.
        MinBuildForQuery = ceil(1 + log2(1.05 * 90 / Args.MaxConeRadiusDeg));
    else
        MinBuildForQuery = Args.QueryLevel;
    end

    % Aggregate-up mode: query level exceeds output level (HTM_Level)
    % Needed when TAP radius limit forces queries at a finer level than the
    % desired output resolution. Multiple query cells are aggregated into
    % each coarser output cell.
    AggregateUp = (MinBuildForQuery > Args.HTM_Level) && isempty(Args.HTM);

    UsePartialBuild = ~AggregateUp && ~IsFullSky && isempty(Args.HTM) && Args.HTM_Level > MinBuildForQuery;

    if ~isempty(Args.HTM) && ~isempty(Args.LevelHTM)
        HTM = Args.HTM;
        LevelHTM = Args.LevelHTM;
    elseif AggregateUp
        BuildLevel = MinBuildForQuery;
        if Args.Verbose
            fprintf('Aggregate-up mode: building HTM to level %d (output at level %d, query at level %d)\n', ...
                    BuildLevel, Args.HTM_Level, MinBuildForQuery);
        end
        [HTM, LevelHTM] = celestial.htm.htm_build(BuildLevel);
    elseif UsePartialBuild
        BuildLevel = min(Args.HTM_Level, MinBuildForQuery);
        if Args.Verbose
            fprintf('Regional request: building HTM to level %d (instead of %d)\n', BuildLevel, Args.HTM_Level);
        end
        [HTM, LevelHTM] = celestial.htm.htm_build(BuildLevel);
    else
        if Args.Verbose
            fprintf('Building HTM structure at level %d...\n', Args.HTM_Level);
        end
        [HTM, LevelHTM] = celestial.htm.htm_build(Args.HTM_Level);
    end

    % Get list of fine-level HTM indices
    if UsePartialBuild
        % Compute analytically (avoids building the full tree)
        FineDepthLevel = Args.HTM_Level - 1;
        FineStartIdx = 1 + round(8 * (4^FineDepthLevel - 1) / 3);
        FineCount = 8 * 4^FineDepthLevel;
        FineListHTM = FineStartIdx:(FineStartIdx + FineCount - 1);
        Nfine = FineCount;
        FineHTMSideRad = (pi/2) / 2^FineDepthLevel;
    else
        FineListHTM = LevelHTM(Args.HTM_Level).ptr;
        Nfine = numel(FineListHTM);
        FineHTMSideRad = LevelHTM(Args.HTM_Level).side;
    end

    if Args.Verbose
        fprintf('Number of fine-level HTM cells: %d\n', Nfine);
        if UsePartialBuild
            fprintf('Fine HTM side length: %.4f deg \n', FineHTMSideRad * RAD);
        else
            fprintf('Fine HTM side length: %.4f deg\n', LevelHTM(Args.HTM_Level).side * RAD);
        end
    end

    %----------------------------------------------------------------------
    % 4. QUERY LEVEL SELECTION + DESCENDANT MAPPING
    %----------------------------------------------------------------------

    % Select the coarsest HTM level where cells fit within MaxConeRadiusDeg
    if AggregateUp
        % Aggregate-up: query level is MinBuildForQuery (already computed from MaxConeRadiusDeg)
        QueryLevelIdx = MinBuildForQuery;
    elseif ischar(Args.QueryLevel) && strcmpi(Args.QueryLevel, 'auto')
        QueryLevelIdx = selectQueryLevel(HTM, LevelHTM, Args.HTM_Level, Args.MaxConeRadiusDeg, RAD, Args.Verbose);
    else
        QueryLevelIdx = Args.QueryLevel;
        % Warn if explicit query level cells may exceed TAP cone radius limit
        ApproxSideDeg = 90 / 2^(QueryLevelIdx - 1);
        if Args.Verbose && ApproxSideDeg * 1.05 > Args.MaxConeRadiusDeg
            fprintf('Warning: Query level %d cells (side ~%.3f deg) exceed MaxConeRadiusDeg (%.3f deg).\n', ...
                    QueryLevelIdx, ApproxSideDeg, Args.MaxConeRadiusDeg);
            fprintf('  TAP queries may fail if the service enforces this radius limit.\n');
        end
    end

    % Safety: if selectQueryLevel chose a level beyond HTM_Level but
    % AggregateUp was not set (e.g., user provided a deep pre-built HTM),
    % enable aggregate-up to avoid distribute-down with query finer than output.
    if QueryLevelIdx > Args.HTM_Level && ~AggregateUp
        AggregateUp = true;
        UsePartialBuild = false;
        if Args.Verbose
            fprintf('Query level %d exceeds output level %d: enabling aggregate-up mode\n', ...
                    QueryLevelIdx, Args.HTM_Level);
        end
    end

    if QueryLevelIdx <= numel(LevelHTM)
        QueryListHTM = LevelHTM(QueryLevelIdx).ptr;
    else
        % Partial build: compute query cell list analytically
        QueryDepthLevel = QueryLevelIdx - 1;
        QueryStartIdx = 1 + round(8 * (4^QueryDepthLevel - 1) / 3);
        QueryCount = 8 * 4^QueryDepthLevel;
        QueryListHTM = QueryStartIdx:(QueryStartIdx + QueryCount - 1);
    end
    Nquery = numel(QueryListHTM);

    if Args.Verbose
        fprintf('Query level: %d (LevelHTM index), %d query cells\n', QueryLevelIdx, Nquery);
%        fprintf('Max cone radius limit: %.4f deg\n', Args.MaxConeRadiusDeg);
    end

    % Build mapping between query cells and output cells
    OutputToQueryMap = {};  % Only used in aggregate-up mode
    OutputToQueryCoo = {};  % Pre-computed query descendant coordinates (partial build)
    QueryToFineMap = cell(Nquery, 1);
    QueryToFineCoo = {};
    if AggregateUp
        % Aggregate-up: for each output cell, find its query-level descendants
        OutputToQueryMap = cell(Nfine, 1);
        if QueryLevelIdx <= numel(LevelHTM)
            % Full tree available — use tree traversal
            for iF = 1:Nfine
                OutputToQueryMap{iF} = getHTMDescendants(HTM, FineListHTM(iF));
            end
        else
            % Partial tree — expand analytically (avoids building deep tree)
            OutputToQueryCoo = cell(Nfine, 1);
            OutputDepthLevel = Args.HTM_Level - 1;
            QueryDepthLevel = QueryLevelIdx - 1;
            for iF = 1:Nfine
                FIdx = FineListHTM(iF);
                [QIndices, QCoos] = expandToFineLevel(HTM(FIdx).cosd, OutputDepthLevel, FIdx, QueryDepthLevel);
                OutputToQueryMap{iF} = QIndices;
                OutputToQueryCoo{iF} = QCoos;
            end
        end
        if Args.Verbose
            NDescPerOutput = 4^(QueryLevelIdx - Args.HTM_Level);
            fprintf('Aggregate-up: %d query descendants per output cell\n', NDescPerOutput);
        end
    elseif UsePartialBuild
        % On-the-fly expansion: compute fine descendants from query cells
        % Only expand in-range cells (skip out-of-range for speed)
        QueryToFineCoo = cell(Nquery, 1);
        QueryDepthLevel = QueryLevelIdx - 1;  % LevelHTM index -> depth level
        FineDepthLevel = Args.HTM_Level - 1;
        for iQ = 1:Nquery
            Idx = QueryListHTM(iQ);
            Coo = HTM(Idx).coo;
            [MeanRA, MeanDec] = cellCentroid(Coo);
            InRange = MeanRA >= Args.RARange(1) && MeanRA < Args.RARange(2) && ...
                      MeanDec >= Args.DecRange(1) && MeanDec < Args.DecRange(2);
            if InRange
                [Fi, Fc] = expandToFineLevel(HTM(Idx).cosd, QueryDepthLevel, Idx, FineDepthLevel);
                QueryToFineMap{iQ} = Fi;
                QueryToFineCoo{iQ} = Fc;
            else
                QueryToFineMap{iQ} = [];
                QueryToFineCoo{iQ} = {};
            end
        end
        if Args.Verbose
            NExpanded = sum(~cellfun(@isempty, QueryToFineMap));
            fprintf('Expanded %d/%d query cells to fine level (on-the-fly)\n', NExpanded, Nquery);
        end
    else
        % Full tree: use getHTMDescendants (tree is fully built)
        for iQ = 1:Nquery
            QueryToFineMap{iQ} = getHTMDescendants(HTM, QueryListHTM(iQ));
        end
    end

    % Build reverse mapping: fine HTM index -> position in Nsrc output
    MaxHTMIdx = max(FineListHTM);
    FineIdxToNsrcPos = zeros(MaxHTMIdx, 1, 'int32');
    FineIdxToNsrcPos(FineListHTM) = int32(1:Nfine);

    % Initialize source count matrix (pre-fill HTM indices, counts = 0)
    Nsrc = [FineListHTM(:), zeros(Nfine, 1)];

    StartTime = tic;
    ProcessedQueryCells = 0;
    ProcessedFineCells = 0;
    SkippedCount = 0;
    FailedCells = [];
    CurrentHDFFile = '';  % Track current HDF5 file for remote copy

    %----------------------------------------------------------------------
    % 5. PRE-FILTER QUERY CELLS (Dec/RA range + resume)
    %----------------------------------------------------------------------

    % Aggregate-up mode: pre-filter output cells directly
    OutputCellsToProcess = [];   % Output-level HTM indices (aggregate-up only)
    OutputCellIndices = [];      % Position in FineListHTM (aggregate-up only)
    NumOutputToProcess = 0;

    % Distribute-down mode: pre-filter query cells
    QueryCellsToProcess = [];    % Query-level HTM indices to download
    QueryCellIndices = [];       % Position in QueryListHTM (1..Nquery)
    SkippedFineFromResume = {};  % Fine descendants already done (per query cell)

    if AggregateUp
        if Args.Verbose
            fprintf('\nFiltering output cells by Dec/RA range and resume status...\n');
        end

        for iF = 1:Nfine
            FIdx = FineListHTM(iF);

            % Check if output cell center is within Dec/RA range
            Coo = HTM(FIdx).coo;
            [MeanRA, MeanDec] = cellCentroid(Coo);

            if MeanRA < Args.RARange(1) || MeanRA >= Args.RARange(2) || ...
               MeanDec < Args.DecRange(1) || MeanDec >= Args.DecRange(2)
                continue;
            end

            % Resume check: skip output cells that already exist
            if Args.Resume
                CellExists = checkHTMExists(Args.CatName, FIdx, Args.NcatInFile, Args.LocalDir) || ...
                         (~isempty(Args.TargetDir) && checkHTMExists(Args.CatName, FIdx, Args.NcatInFile, Args.TargetDir));
                if CellExists
                    Pos = FineIdxToNsrcPos(FIdx);
                    NsrcExisting = getHTMSourceCount(Args.CatName, FIdx, Args.NcatInFile, Args.LocalDir);
                    if NsrcExisting == 0 && ~isempty(Args.TargetDir)
                        NsrcExisting = getHTMSourceCount(Args.CatName, FIdx, Args.NcatInFile, Args.TargetDir);
                    end
                    Nsrc(Pos, :) = [FIdx, NsrcExisting];
                    SkippedCount = SkippedCount + 1;
                    continue;
                end
            end

            OutputCellsToProcess = [OutputCellsToProcess, FIdx]; %#ok<AGROW>
            OutputCellIndices = [OutputCellIndices, iF]; %#ok<AGROW>

            if Args.Verbose && mod(iF, 10000) == 0
                fprintf('  Filtered %d/%d output cells...\n', iF, Nfine);
            end
        end

        NumOutputToProcess = numel(OutputCellsToProcess);
        NumQueryToProcess = 0;  % Skip distribute-down loops

        if Args.Verbose
            fprintf('Output cells to process: %d / %d (skipped %d from resume)\n', ...
                    NumOutputToProcess, Nfine, SkippedCount);
        end
    else
        if Args.Verbose
            fprintf('\nFiltering query-level cells by Dec/RA range and resume status...\n');
        end

        for iQ = 1:Nquery
            IndQ = QueryListHTM(iQ);
            FineDescendants = QueryToFineMap{iQ};

            % Check if query cell center is within Dec/RA range
            [MeanRA, MeanDec] = cellCentroid(HTM(IndQ).coo);

            OutsideRange = MeanRA < Args.RARange(1) || MeanRA >= Args.RARange(2) || ...
                           MeanDec < Args.DecRange(1) || MeanDec >= Args.DecRange(2);

            if OutsideRange
                % Nsrc already initialized to [idx, 0] for all cells
                continue;
            end

            % Check resume status of fine descendants
            if Args.Resume
                AllExist = true;
                ExistMask = false(numel(FineDescendants), 1);
                for iF = 1:numel(FineDescendants)
                    FIdx = FineDescendants(iF);
                    Exists = checkHTMExists(Args.CatName, FIdx, Args.NcatInFile, Args.LocalDir) || ...
                             (~isempty(Args.TargetDir) && checkHTMExists(Args.CatName, FIdx, Args.NcatInFile, Args.TargetDir));
                    ExistMask(iF) = Exists;
                    if ~Exists
                        AllExist = false;
                    end
                end

                if AllExist
                    % All fine descendants exist - skip this query cell entirely
                    for iF = 1:numel(FineDescendants)
                        FIdx = FineDescendants(iF);
                        Pos = FineIdxToNsrcPos(FIdx);
                        NsrcExisting = getHTMSourceCount(Args.CatName, FIdx, Args.NcatInFile, Args.LocalDir);
                        if NsrcExisting == 0 && ~isempty(Args.TargetDir)
                            NsrcExisting = getHTMSourceCount(Args.CatName, FIdx, Args.NcatInFile, Args.TargetDir);
                        end
                        Nsrc(Pos, :) = [FIdx, NsrcExisting];
                    end
                    SkippedCount = SkippedCount + numel(FineDescendants);
                    continue;
                end

                % Some exist, some don't - we need to re-download but can skip writing existing ones
                % Record which fine cells already exist for this query cell
                SkippedFine = FineDescendants(ExistMask);
                for iF = 1:numel(SkippedFine)
                    FIdx = SkippedFine(iF);
                    Pos = FineIdxToNsrcPos(FIdx);
                    NsrcExisting = getHTMSourceCount(Args.CatName, FIdx, Args.NcatInFile, Args.LocalDir);
                    if NsrcExisting == 0 && ~isempty(Args.TargetDir)
                        NsrcExisting = getHTMSourceCount(Args.CatName, FIdx, Args.NcatInFile, Args.TargetDir);
                    end
                    Nsrc(Pos, :) = [FIdx, NsrcExisting];
                    SkippedCount = SkippedCount + 1;
                end
            else
                SkippedFine = [];
            end

            QueryCellsToProcess = [QueryCellsToProcess, IndQ]; %#ok<AGROW>
            QueryCellIndices = [QueryCellIndices, iQ]; %#ok<AGROW>
            SkippedFineFromResume{end+1} = SkippedFine; %#ok<AGROW>

            % Progress for filtering (every 10000 query cells)
            if Args.Verbose && mod(iQ, 10000) == 0
                fprintf('  Filtered %d/%d query cells...\n', iQ, Nquery);
            end
        end

        NumQueryToProcess = numel(QueryCellsToProcess);
        if Args.Verbose
            fprintf('Query cells to process: %d / %d (skipped %d fine cells from resume)\n', ...
                    NumQueryToProcess, Nquery, SkippedCount);
        end
    end

    %----------------------------------------------------------------------
    % 5b. FILE COMPLETION MAP (for incremental NFS copy in parallel mode)
    %----------------------------------------------------------------------
    % Pre-count how many cells will be written to each HDF5 file.
    % In parallel mode, results arrive in arbitrary order, so we track
    % remaining cells per file and copy as soon as a file is complete.
    % File completion map for incremental NFS copy (sequential mode only).
    % Parallel mode uses batch merge + copy instead.
    FileRemainingCells = containers.Map('KeyType', 'char', 'ValueType', 'int32');
    if Args.NumWorkers <= 1
        if AggregateUp && NumOutputToProcess > 0
            for iProc = 1:NumOutputToProcess
                [Fn, ~] = HDF5.get_file_var_from_htmid(Args.CatName, OutputCellsToProcess(iProc), Args.NcatInFile);
                if FileRemainingCells.isKey(Fn)
                    FileRemainingCells(Fn) = FileRemainingCells(Fn) + int32(1);
                else
                    FileRemainingCells(Fn) = int32(1);
                end
            end
        elseif NumQueryToProcess > 0
            for iProc = 1:NumQueryToProcess
                iQ = QueryCellIndices(iProc);
                FineDesc = QueryToFineMap{iQ};
                Skipped = SkippedFineFromResume{iProc};
                for iF = 1:numel(FineDesc)
                    if ~ismember(FineDesc(iF), Skipped)
                        [Fn, ~] = HDF5.get_file_var_from_htmid(Args.CatName, FineDesc(iF), Args.NcatInFile);
                        if FileRemainingCells.isKey(Fn)
                            FileRemainingCells(Fn) = FileRemainingCells(Fn) + int32(1);
                        else
                            FileRemainingCells(Fn) = int32(1);
                        end
                    end
                end
            end
        end
    end

    %----------------------------------------------------------------------
    % 6. MAIN LOOP
    %----------------------------------------------------------------------

    if AggregateUp && NumOutputToProcess > 0
        %==================================================================
        % AGGREGATE-UP MODE: download query descendants, aggregate into
        % coarser output cells
        %==================================================================

        if Args.NumWorkers > 1
            %--------------------------------------------------------------
            % AGGREGATE-UP PARALLEL PROCESSING
            %--------------------------------------------------------------

            if Args.Verbose
                fprintf('\nStarting aggregate-up parallel processing with %d workers\n', Args.NumWorkers);
            end

            % Start parallel pool if needed
            try
                Pool = gcp('nocreate');
                if isempty(Pool)
                    Pool = parpool(Args.NumWorkers);
                else
                    % Cancel stale futures from previous interrupted runs
                    if ~isempty(Pool.FevalQueue.QueuedFutures)
                        cancel(Pool.FevalQueue.QueuedFutures);
                    end
                    if ~isempty(Pool.FevalQueue.RunningFutures)
                        cancel(Pool.FevalQueue.RunningFutures);
                    end
                    if Pool.NumWorkers ~= Args.NumWorkers
                        delete(Pool);
                        Pool = parpool(Args.NumWorkers);
                    end
                end
            catch ME
                warning('Could not start parallel pool: %s. Falling back to sequential.', char(ME.message));
                Args.NumWorkers = 0;
            end

            if Args.NumWorkers > 0
                % Extract coordinates for workers (avoid passing full HTM)
                OutputCellCoos = cell(NumOutputToProcess, 1);
                QueryDescCooSets = cell(NumOutputToProcess, 1);
                for iProc = 1:NumOutputToProcess
                    iF = OutputCellIndices(iProc);
                    FIdx = OutputCellsToProcess(iProc);
                    OutputCellCoos{iProc} = HTM(FIdx).coo;
                    QDescs = OutputToQueryMap{iF};
                    if ~isempty(OutputToQueryCoo)
                        % Partial build: use precomputed coordinates
                        QueryDescCooSets{iProc} = OutputToQueryCoo{iF};
                    else
                        QCoos = cell(numel(QDescs), 1);
                        for j = 1:numel(QDescs)
                            QCoos{j} = HTM(QDescs(j)).coo;
                        end
                        QueryDescCooSets{iProc} = QCoos;
                    end
                end

                % Submit one parfeval per output cell (workers write HDF5 to temp dirs)
                Futures(NumOutputToProcess) = parallel.FevalFuture;
                for iProc = 1:NumOutputToProcess
                    Futures(iProc) = parfeval(Pool, @downloadAggregateCellWithWrite, 2, ...
                        TableName, ColumnsStr, OutputCellCoos{iProc}, ...
                        QueryDescCooSets{iProc}, OutputCellsToProcess(iProc), RAD, Args);
                end

                if Args.Verbose
                    fprintf('Submitted %d parfeval futures (aggregate-up, workers write HDF5).\n', NumOutputToProcess);
                end

                % Process results as they complete (workers already wrote HDF5)
                for iDone = 1:NumOutputToProcess
                    try
                        [CompletedIdx, NsrcCell, QueryFailed] = fetchNext(Futures);
                    catch ME
                        warning('VO:buildHTMfromTopCat:fetchNext', 'fetchNext error: %s', ME.message);
                        continue;
                    end

                    FIdx = OutputCellsToProcess(CompletedIdx);
                    Pos = FineIdxToNsrcPos(FIdx);

                    if QueryFailed
                        FailedCells = [FailedCells, FIdx]; %#ok<AGROW>
                        Nsrc(Pos, :) = [FIdx, 0];
                    else
                        Nsrc(Pos, :) = [FIdx, NsrcCell];
                        ProcessedFineCells = ProcessedFineCells + 1;
                    end

                    ProcessedQueryCells = ProcessedQueryCells + 1;

                    if Args.Verbose && (mod(ProcessedQueryCells, 10) == 0 || ProcessedQueryCells == 1 || ProcessedQueryCells == NumOutputToProcess)
                        Elapsed = toc(StartTime);
                        Rate = ProcessedQueryCells / Elapsed;
                        Remaining = NumOutputToProcess - ProcessedQueryCells;
                        ETA = Remaining / max(Rate, 0.001);
                        fprintf('[%d/%d output cells] %d cells written (%.1f cells/min, ETA: %.1f min)\n', ...
                                ProcessedQueryCells, NumOutputToProcess, ProcessedFineCells, Rate * 60, ETA / 60);
                    end
                end

                cancel(Futures);

                % Merge worker temp files into grouped HDF5
                mergeWorkerTempFiles(Args.LocalDir, Args.CatName, Args.NcatInFile, ...
                    Args.ColDecOut, Args.IndStep, Args.Verbose);

                % Batch NFS copy all grouped files
                if ~isempty(Args.TargetDir)
                    HdfPattern = fullfile(Args.LocalDir, sprintf('%s_htm_*.hdf5', Args.CatName));
                    HdfFiles = dir(HdfPattern);
                    if ~isempty(HdfFiles) && Args.Verbose
                        fprintf('\nCopying %d HDF5 files to remote directory...\n', numel(HdfFiles));
                    end
                    for iFile = 1:numel(HdfFiles)
                        FullPath = fullfile(Args.LocalDir, HdfFiles(iFile).name);
                        tools.os.copyFileOverNFS({FullPath}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
                        if Args.Verbose
                            fprintf('  Copied: %s\n', HdfFiles(iFile).name);
                        end
                    end
                end

                cleanupWorkerDirs(Args.LocalDir);
            end
        end

        if Args.NumWorkers == 0 && NumOutputToProcess > 0
            %--------------------------------------------------------------
            % AGGREGATE-UP SEQUENTIAL PROCESSING
            %--------------------------------------------------------------

            if Args.Verbose
                fprintf('\nStarting aggregate-up sequential processing...\n');
            end

            for iProc = 1:NumOutputToProcess
                FIdx = OutputCellsToProcess(iProc);
                iF = OutputCellIndices(iProc);
                OutputCoo = HTM(FIdx).coo;
                QDescs = OutputToQueryMap{iF};

                % Download all query descendants for this output cell
                AllData = [];
                AnyFailed = false;
                for j = 1:numel(QDescs)
                    QIdx = QDescs(j);
                    if ~isempty(OutputToQueryCoo)
                        QCoo = OutputToQueryCoo{iF}{j};
                    else
                        QCoo = HTM(QIdx).coo;
                    end
                    SearchRadiusDeg = computeCellSearchRadius(QCoo, RAD);

                    [D, QFailed] = downloadQueryConeSeq(Tap, TableName, ColumnsStr, ...
                        QIdx, QCoo, RAD, SearchRadiusDeg, Args);

                    if QFailed
                        AnyFailed = true;
                    elseif ~isempty(D)
                        AllData = [AllData; D]; %#ok<AGROW>
                    end
                end

                Pos = FineIdxToNsrcPos(FIdx);

                if AnyFailed && isempty(AllData)
                    FailedCells = [FailedCells, FIdx]; %#ok<AGROW>
                    Nsrc(Pos, :) = [FIdx, 0];
                else
                    % Filter to output cell boundaries and deduplicate by coordinates
                    if ~isempty(AllData)
                        CooRad = AllData(:, [Args.ColRAOut, Args.ColDecOut]);
                        Flag = celestial.htm.in_polysphere(CooRad, OutputCoo, 2);
                        AllData = AllData(Flag, :);
                        [~, uniqueIdx] = unique(AllData(:, [Args.ColRAOut, Args.ColDecOut]), 'rows', 'first');
                        if numel(uniqueIdx) < size(AllData, 1)
                            Nremoved = size(AllData, 1) - numel(uniqueIdx);
                            AllData = AllData(sort(uniqueIdx), :);
                            if Args.Verbose
                                fprintf('    Output cell %d: removed %d duplicate rows\n', FIdx, Nremoved);
                            end
                        end
                    end

                    % Check if we've moved to a new HDF5 file - copy completed one
                    [ThisFileName, ~] = HDF5.get_file_var_from_htmid(Args.CatName, FIdx, Args.NcatInFile);
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

                    NsrcCell = writeOutputCellDirect(AllData, FIdx, Args, RAD);
                    Nsrc(Pos, :) = [FIdx, NsrcCell];
                    ProcessedFineCells = ProcessedFineCells + 1;
                end

                ProcessedQueryCells = ProcessedQueryCells + 1;

                if Args.Verbose && (mod(ProcessedQueryCells, 10) == 0 || ProcessedQueryCells == 1)
                    Elapsed = toc(StartTime);
                    Rate = ProcessedQueryCells / Elapsed;
                    Remaining = NumOutputToProcess - ProcessedQueryCells;
                    ETA = Remaining / max(Rate, 0.001);
                    fprintf('[%d/%d output cells] %d cells written (%.1f cells/min, ETA: %.1f min)\n', ...
                            ProcessedQueryCells, NumOutputToProcess, ProcessedFineCells, Rate * 60, ETA / 60);
                end
            end

            % Copy the last HDF5 file to remote
            if ~isempty(CurrentHDFFile) && ~isempty(Args.TargetDir)
                FullPath = fullfile(Args.LocalDir, CurrentHDFFile);
                if isfile(FullPath)
                    tools.os.copyFileOverNFS({FullPath}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
                    if Args.Verbose
                        fprintf('  Copied completed file: %s\n', CurrentHDFFile);
                    end
                end
            end
        end

    elseif NumQueryToProcess == 0 && NumOutputToProcess == 0
        %==================================================================
        % NO CELLS TO PROCESS
        %==================================================================
        if Args.Verbose
            fprintf('No cells to process.\n');
        end

    elseif NumQueryToProcess > 0
        %==================================================================
        % DISTRIBUTE-DOWN MODE: download at query level, distribute to
        % finer output cells
        %==================================================================

        if Args.NumWorkers > 1
            %--------------------------------------------------------------
            % DISTRIBUTE-DOWN PARALLEL PROCESSING: parfeval + fetchNext
            %--------------------------------------------------------------

            if Args.Verbose
                fprintf('\nStarting parallel processing with %d workers (parfeval + fetchNext)\n', Args.NumWorkers);
             end

            % Start parallel pool if needed
            try
                Pool = gcp('nocreate');
                if isempty(Pool)
                    Pool = parpool(Args.NumWorkers);
                else
                    % Cancel stale futures from previous interrupted runs
                    if ~isempty(Pool.FevalQueue.QueuedFutures)
                        cancel(Pool.FevalQueue.QueuedFutures);
                    end
                    if ~isempty(Pool.FevalQueue.RunningFutures)
                        cancel(Pool.FevalQueue.RunningFutures);
                    end
                    if Pool.NumWorkers ~= Args.NumWorkers
                        delete(Pool);
                        Pool = parpool(Args.NumWorkers);
                    end
                end
            catch ME
                warning('Could not start parallel pool: %s. Falling back to sequential.', char(ME.message));
                Args.NumWorkers = 0;
            end

            if Args.NumWorkers > 0
                % Extract data needed by workers (avoid passing full HTM)
                QueryCellCoo = cell(NumQueryToProcess, 1);
                QuerySearchRadii = zeros(NumQueryToProcess, 1);
                WorkerFineIndices = cell(NumQueryToProcess, 1);
                WorkerFineCoos = cell(NumQueryToProcess, 1);
                for iProc = 1:NumQueryToProcess
                    IndQ = QueryCellsToProcess(iProc);
                    iQ = QueryCellIndices(iProc);
                    QueryCellCoo{iProc} = HTM(IndQ).coo;
                    QuerySearchRadii(iProc) = computeCellSearchRadius(HTM(IndQ).coo, RAD);
                    FineDesc = QueryToFineMap{iQ};
                    WorkerFineIndices{iProc} = FineDesc;
                    Coos = cell(numel(FineDesc), 1);
                    for iF = 1:numel(FineDesc)
                        if ~isempty(QueryToFineCoo) && ~isempty(QueryToFineCoo{iQ})
                            Coos{iF} = QueryToFineCoo{iQ}{iF};
                        else
                            Coos{iF} = HTM(FineDesc(iF)).coo;
                        end
                    end
                    WorkerFineCoos{iProc} = Coos;
                end

                % Submit ALL query cells (workers download, distribute, and write HDF5)
                Futures(NumQueryToProcess) = parallel.FevalFuture;
                for iProc = 1:NumQueryToProcess
                    Futures(iProc) = parfeval(Pool, @downloadQueryConeWithWrite, 2, ...
                        TableName, ColumnsStr, QueryCellsToProcess(iProc), ...
                        QueryCellCoo{iProc}, RAD, QuerySearchRadii(iProc), ...
                        WorkerFineIndices{iProc}, WorkerFineCoos{iProc}, ...
                        SkippedFineFromResume{iProc}, Args);
                end

                if Args.Verbose
                    fprintf('Submitted %d parfeval futures (workers write HDF5).\n', NumQueryToProcess);
                end

                % Process results as they complete (workers already wrote HDF5)
                for iDone = 1:NumQueryToProcess
                    try
                        [CompletedIdx, NsrcResults, QueryFailed] = fetchNext(Futures);
                    catch ME
                        warning('VO:buildHTMfromTopCat:fetchNext', 'fetchNext error: %s', ME.message);
                        continue;
                    end

                    IndQ = QueryCellsToProcess(CompletedIdx);

                    if QueryFailed
                        FailedCells = [FailedCells, IndQ]; %#ok<AGROW>
                        iQ = QueryCellIndices(CompletedIdx);
                        FineDescendants = QueryToFineMap{iQ};
                        SkippedFine = SkippedFineFromResume{CompletedIdx};
                        for iF = 1:numel(FineDescendants)
                            FIdx = FineDescendants(iF);
                            if ~ismember(FIdx, SkippedFine)
                                Pos = FineIdxToNsrcPos(FIdx);
                                Nsrc(Pos, :) = [FIdx, 0];
                            end
                        end
                    else
                        % Update Nsrc from worker results
                        SkippedFine = SkippedFineFromResume{CompletedIdx};
                        for iR = 1:size(NsrcResults, 1)
                            FIdx = NsrcResults(iR, 1);
                            if ismember(FIdx, SkippedFine)
                                continue;
                            end
                            NsrcCell = NsrcResults(iR, 2);
                            Pos = FineIdxToNsrcPos(FIdx);
                            Nsrc(Pos, :) = [FIdx, NsrcCell];
                            ProcessedFineCells = ProcessedFineCells + 1;
                        end
                    end

                    ProcessedQueryCells = ProcessedQueryCells + 1;

                    % Progress reporting
                    if Args.Verbose && (mod(ProcessedQueryCells, 10) == 0 || ProcessedQueryCells == 1 || ProcessedQueryCells == NumQueryToProcess)
                        Elapsed = toc(StartTime);
                        Rate = ProcessedQueryCells / Elapsed;
                        Remaining = NumQueryToProcess - ProcessedQueryCells;
                        ETA = Remaining / max(Rate, 0.001);
                        fprintf('[%d/%d query cells] %d fine cells written (%.1f q/min, ETA: %.1f min)\n', ...
                                ProcessedQueryCells, NumQueryToProcess, ProcessedFineCells, Rate * 60, ETA / 60);
                    end
                end

                % Cancel any remaining futures (shouldn't be any)
                cancel(Futures);

                % Merge worker temp files into grouped HDF5
                mergeWorkerTempFiles(Args.LocalDir, Args.CatName, Args.NcatInFile, ...
                    Args.ColDecOut, Args.IndStep, Args.Verbose);

                % Batch NFS copy all grouped files
                if ~isempty(Args.TargetDir)
                    HdfPattern = fullfile(Args.LocalDir, sprintf('%s_htm_*.hdf5', Args.CatName));
                    HdfFiles = dir(HdfPattern);
                    if ~isempty(HdfFiles) && Args.Verbose
                        fprintf('\nCopying %d HDF5 files to remote directory...\n', numel(HdfFiles));
                    end
                    for iFile = 1:numel(HdfFiles)
                        FullPath = fullfile(Args.LocalDir, HdfFiles(iFile).name);
                        tools.os.copyFileOverNFS({FullPath}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
                        if Args.Verbose
                            fprintf('  Copied: %s\n', HdfFiles(iFile).name);
                        end
                    end
                end

                % Cleanup per-worker temp directories
                cleanupWorkerDirs(Args.LocalDir);
            end
        end

        if Args.NumWorkers == 0 && NumQueryToProcess > 0
            %--------------------------------------------------------------
            % DISTRIBUTE-DOWN SEQUENTIAL PROCESSING
            %--------------------------------------------------------------

            if Args.Verbose
                fprintf('\nStarting sequential processing...\n');
            end

            for iProc = 1:NumQueryToProcess
                IndQ = QueryCellsToProcess(iProc);
                iQ = QueryCellIndices(iProc);
                FineDescendants = QueryToFineMap{iQ};
                SkippedFine = SkippedFineFromResume{iProc};
                SearchRadiusDeg = computeCellSearchRadius(HTM(IndQ).coo, RAD);

                % Download data for this query cell
                [Data, QueryFailed] = downloadQueryConeSeq(Tap, TableName, ColumnsStr, ...
                    IndQ, HTM(IndQ).coo, RAD, SearchRadiusDeg, Args);

                if QueryFailed
                    FailedCells = [FailedCells, IndQ]; %#ok<AGROW>
                    for iF = 1:numel(FineDescendants)
                        FIdx = FineDescendants(iF);
                        if ~ismember(FIdx, SkippedFine)
                            Pos = FineIdxToNsrcPos(FIdx);
                            Nsrc(Pos, :) = [FIdx, 0];
                        end
                    end
                else
                    % Distribute data to fine-level cells
                    for iF = 1:numel(FineDescendants)
                        FIdx = FineDescendants(iF);

                        % Skip fine cells that already exist from resume
                        if ismember(FIdx, SkippedFine)
                            continue;
                        end

                        % Check if we've moved to a new HDF5 file - copy completed one
                        [ThisFileName, ~] = HDF5.get_file_var_from_htmid(Args.CatName, FIdx, Args.NcatInFile);
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

                        Pos = FineIdxToNsrcPos(FIdx);
                        if ~isempty(QueryToFineCoo)
                            FineCoo = QueryToFineCoo{iQ}{iF};
                        else
                            FineCoo = HTM(FIdx).coo;
                        end
                        NsrcCell = writeFineCellFromQuery(Data, FIdx, FineCoo, Args, RAD);
                        Nsrc(Pos, :) = [FIdx, NsrcCell];
                        ProcessedFineCells = ProcessedFineCells + 1;
                    end
                end

                ProcessedQueryCells = ProcessedQueryCells + 1;

                if Args.Verbose && (mod(ProcessedQueryCells, 10) == 0 || ProcessedQueryCells == 1)
                    Elapsed = toc(StartTime);
                    Rate = ProcessedQueryCells / Elapsed;
                    Remaining = NumQueryToProcess - ProcessedQueryCells;
                    ETA = Remaining / max(Rate, 0.001);
                    fprintf('[%d/%d query cells] %d fine cells written (%.1f q/min, ETA: %.1f min)\n', ...
                            ProcessedQueryCells, NumQueryToProcess, ProcessedFineCells, Rate * 60, ETA / 60);
                end
            end

            % Copy the last HDF5 file to remote (sequential mode)
            if ~isempty(CurrentHDFFile) && ~isempty(Args.TargetDir)
                FullPath = fullfile(Args.LocalDir, CurrentHDFFile);
                if isfile(FullPath)
                    tools.os.copyFileOverNFS({FullPath}, Args.TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
                    if Args.Verbose
                        fprintf('  Copied completed file: %s\n', CurrentHDFFile);
                    end
                end
            end
        end
    end

    %----------------------------------------------------------------------
    % 7. FINALIZATION
    %----------------------------------------------------------------------

    if Args.Verbose
        fprintf('\n=== Finalization ===\n');
        fprintf('Query cells processed: %d\n', ProcessedQueryCells);
        fprintf('Fine cells written: %d\n', ProcessedFineCells);
        fprintf('Skipped (existing): %d fine cells\n', SkippedCount);
        fprintf('Failed: %d query cells\n', numel(FailedCells));
    end

    % Save HTM index file (fast analytical method, no struct build needed)
    if Args.SaveInd
        if Args.Verbose
            fprintf('Saving HTM index (fast analytical, level %d)...\n', Args.HTM_Level);
        end

        % Delete old index file if exists
        IndFileName = fullfile(Args.LocalDir, sprintf('%s_htm.hdf5', Args.CatName));
        if isfile(IndFileName)
            delete(IndFileName);
        end

        % Save HTM index analytically (avoids building full HTM struct)
        VO.prep.saveHTMIndexFast(Args.HTM_Level, IndFileName, sprintf('%s_HTM', Args.CatName), {}, Nsrc);

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
        warning('%d query cells failed. Re-run with Resume=true to retry.', numel(FailedCells));
    end
end


%==========================================================================
% HELPER FUNCTIONS
%==========================================================================

function QueryLevelIdx = selectQueryLevel(HTM, LevelHTM, ~, MaxConeRadiusDeg, RAD, Verbose)
    % Select coarsest query level where all cells fit within MaxConeRadiusDeg
    % Iterates from coarsest (L=1) to finest available level in LevelHTM.
    % For each level, computes the max centroid-to-vertex angular distance.
    % First level where max_distance * 1.05 <= MaxConeRadiusDeg is selected.
    % Result can exceed HTM_Level (triggers aggregate-up mode).

    MaxConeRadiusRad = MaxConeRadiusDeg / RAD;

    % Cap at available levels (partial build may have fewer)
    % Note: MaxAvailLevel can exceed HTM_Level in aggregate-up mode
    MaxAvailLevel = numel(LevelHTM);
    QueryLevelIdx = MaxAvailLevel;  % default: deepest available level

    for L = 1:MaxAvailLevel
        Ptrs = LevelHTM(L).ptr;
        MaxDist = 0;

        % Sample cells to compute max centroid-to-vertex distance
        % For efficiency, sample up to 200 cells (enough to catch polar extremes)
        if numel(Ptrs) <= 200
            SamplePtrs = Ptrs;
        else
            SamplePtrs = Ptrs(round(linspace(1, numel(Ptrs), 200)));
        end

        for iP = 1:numel(SamplePtrs)
            Idx = SamplePtrs(iP);
            Coo = HTM(Idx).coo;  % 3x2 [Long, Lat] in radians
            [CenterRA, CenterDec] = cellCentroid(Coo);

            % Angular distance from centroid to each vertex
            for iV = 1:3
                D = celestial.coo.sphere_dist_fast(CenterRA, CenterDec, Coo(iV,1), Coo(iV,2));
                if D > MaxDist
                    MaxDist = D;
                end
            end
        end

        % Apply 1.05 safety margin
        SearchRadius = MaxDist * 1.05;

        if Verbose
            fprintf('  Level %d: %d cells, max radius = %.4f deg (limit %.4f deg)\n', ...
                    L, numel(Ptrs), SearchRadius * RAD, MaxConeRadiusDeg);
        end

        if SearchRadius <= MaxConeRadiusRad
            QueryLevelIdx = L;
            break;
        end
    end

    if Verbose
        fprintf('  Selected query level: %d\n', QueryLevelIdx);
    end
end


function Descendants = getHTMDescendants(HTM, Idx)
    % Recursively find all leaf-level descendants of HTM cell Idx
    % Returns vector of leaf indices (cells with no sons)

    if isempty(HTM(Idx).son)
        % This is a leaf node
        Descendants = Idx;
    else
        % Recurse into sons
        Descendants = [];
        for iSon = 1:numel(HTM(Idx).son)
            Descendants = [Descendants, getHTMDescendants(HTM, HTM(Idx).son(iSon))]; %#ok<AGROW>
        end
    end
end


function [FineIndices, FineCoos] = expandToFineLevel(ParentCosd, ParentDepthLevel, ParentFullIdx, TargetDepthLevel)
    % Recursively subdivide an HTM cell to the target depth level
    % Computes correct full-tree indices and cell coordinates without
    % building the full HTM structure. Uses the same subdivision geometry
    % as celestial.htm.htm_build_son.
    % Input  : - ParentCosd: 3x3 cosine direction matrix of parent cell
    %          - ParentDepthLevel: depth level of parent (0-indexed)
    %          - ParentFullIdx: full-tree index of parent
    %          - TargetDepthLevel: target fine depth level
    % Output : - FineIndices: 1xN vector of full-tree indices at target level
    %          - FineCoos: 1xN cell array of 3x2 [Long,Lat] coordinate matrices

    if ParentDepthLevel == TargetDepthLevel
        % This IS the target level - return this cell
        [Long, Lat] = celestial.coo.cosined2coo(ParentCosd(:,1), ParentCosd(:,2), ParentCosd(:,3));
        FineIndices = ParentFullIdx;
        FineCoos = {[Long, Lat]};
        return;
    end

    % Compute midpoints on great circles (same as htm_build_son)
    V = ParentCosd;
    Cen = celestial.htm.gc_mid_section([V(1,:);V(2,:);V(3,:)], [V(2,:);V(3,:);V(1,:)]);

    % 4 children's cosine directions (same order as htm_build_son)
    ChildCosd = cell(4,1);
    ChildCosd{1} = [V(1,:); Cen(1,:); Cen(3,:)];
    ChildCosd{2} = [V(2,:); Cen(2,:); Cen(1,:)];
    ChildCosd{3} = [V(3,:); Cen(3,:); Cen(2,:)];
    ChildCosd{4} = [Cen(1,:); Cen(2,:); Cen(3,:)];

    % Compute children's full-tree indices analytically
    % startIdx(k) = 1 + 8*(4^k - 1)/3 for depth k
    ChildDepth = ParentDepthLevel + 1;
    ChildStartIdx = 1 + round(8 * (4^ChildDepth - 1) / 3);
    ParentStartIdx = 1 + round(8 * (4^ParentDepthLevel - 1) / 3);
    ParentPos = ParentFullIdx - ParentStartIdx;  % 0-indexed position in level
    ChildBaseIdx = ChildStartIdx + ParentPos * 4;

    % Recurse into children
    FineIndices = [];
    FineCoos = {};
    for s = 1:4
        ChildIdx = ChildBaseIdx + (s - 1);
        [Fi, Fc] = expandToFineLevel(ChildCosd{s}, ChildDepth, ChildIdx, TargetDepthLevel);
        FineIndices = [FineIndices, Fi]; %#ok<AGROW>
        FineCoos = [FineCoos, Fc]; %#ok<AGROW>
    end
end


function SearchRadiusDeg = computeCellSearchRadius(CellCoo, RAD)
    % Compute search radius for a query cell: max centroid-to-vertex distance * 1.05
    % Input  : - CellCoo: 3x2 [Long, Lat] in radians
    %          - RAD: degrees per radian
    % Output : - SearchRadiusDeg: cone search radius in degrees

    [CenterRA, CenterDec] = cellCentroid(CellCoo);

    MaxDist = 0;
    for iV = 1:3
        D = celestial.coo.sphere_dist_fast(CenterRA, CenterDec, CellCoo(iV,1), CellCoo(iV,2));
        if D > MaxDist
            MaxDist = D;
        end
    end

    SearchRadiusDeg = MaxDist * 1.05 * RAD;
end


function [Data, QueryFailed] = downloadQueryCone(TableName, ColumnsStr, IndHTM, CellCoo, RAD, SearchRadiusDeg, Args)
    % Parallel worker function: download cone query data and return raw matrix in radians
    % Creates its own Tap object and per-worker temp directory.
    % Output : - Data: numeric matrix with RA/Dec in radians (cols 1,2), or empty
    %          - QueryFailed: true if query failed after retries

    Data = [];
    QueryFailed = false;

    % Per-worker temp directory to avoid filesystem contention
    try
        W = getCurrentWorker();
        WorkerPid = W.ProcessId;
    catch
        WorkerPid = feature('getpid');
    end
    WorkDir = fullfile(Args.LocalDir, sprintf('tap_w%d', WorkerPid));
    if ~isfolder(WorkDir)
        mkdir(WorkDir);
    end

    % Create TopCat object (each worker needs its own)
    Tap = VO.TopCat;

    % Compute query center in degrees (via cosine direction centroid)
    [CenRA, CenDec] = cellCentroid(CellCoo);
    CenterRADeg  = CenRA * RAD;
    CenterDecDeg = CenDec * RAD;
    HTMCooDeg = CellCoo * RAD;

    % Construct query
    Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRASrc, Args.ColDecSrc, ...
                                   HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                   SearchRadiusDeg, Args.QueryType, Args.WhereClause);

    % Execute query with retry logic
    try
        T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                           Args.TapUrl, Args.TimeoutSec, Args.QueryMethod, WorkDir);
    catch ME
        warning('VO:buildHTMfromTopCat:QueryFailed', ...
            'Query cell %d: Query failed after %d retries: %s', IndHTM, Args.MaxRetries, char(ME.message));
        QueryFailed = true;
        return;
    end

    % Handle empty result
    if isempty(T) || height(T) == 0
        return;
    end

    % Convert table to matrix (RA/Dec in radians, cols 1,2)
    [Data, ColNames] = tableToMatrix(T, Args.ColRA, Args.ColDec, Args.TapUnits);

    % Apply post-processing: NullValue replacement, computed columns, drop columns
    [Data, ~] = applyPostProcessing(Data, ColNames, Args.NullValue, ...
                                     Args.ComputedColumns, Args.DropColumns);
end


function [Data, QueryFailed] = downloadQueryConeSeq(Tap, TableName, ColumnsStr, IndHTM, CellCoo, RAD, SearchRadiusDeg, Args)
    % Sequential version of downloadQueryCone: reuses existing Tap object
    % Output : - Data: numeric matrix with RA/Dec in radians (cols 1,2), or empty
    %          - QueryFailed: true if query failed after retries

    Data = [];
    QueryFailed = false;

    % Compute query center in degrees (via cosine direction centroid)
    [CenRA, CenDec] = cellCentroid(CellCoo);
    CenterRADeg  = CenRA * RAD;
    CenterDecDeg = CenDec * RAD;
    HTMCooDeg = CellCoo * RAD;

    % Construct query
    Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRASrc, Args.ColDecSrc, ...
                                   HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                   SearchRadiusDeg, Args.QueryType, Args.WhereClause);

    % Execute query with retry logic
    try
        T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                           Args.TapUrl, Args.TimeoutSec, Args.QueryMethod, Args.LocalDir);
    catch ME
        warning('VO:buildHTMfromTopCat:QueryFailed', ...
            'Query cell %d: Query failed after %d retries: %s', IndHTM, Args.MaxRetries, char(ME.message));
        QueryFailed = true;
        return;
    end

    % Handle empty result
    if isempty(T) || height(T) == 0
        return;
    end

    % Convert table to matrix (RA/Dec in radians, cols 1,2)
    [Data, ColNames] = tableToMatrix(T, Args.ColRA, Args.ColDec, Args.TapUnits);

    % Apply post-processing: NullValue replacement, computed columns, drop columns
    [Data, ~] = applyPostProcessing(Data, ColNames, Args.NullValue, ...
                                     Args.ComputedColumns, Args.DropColumns);
end


function NsrcCell = writeFineCellFromQuery(Data, FineIdx, FineCoo, Args, RAD)
    % Filter raw query data to a single fine HTM cell and write to HDF5
    % Input  : - Data: numeric matrix from query (RA/Dec in radians, cols 1,2)
    %          - FineIdx: fine-level HTM index for HDF5 naming
    %          - FineCoo: 3x2 [Long, Lat] of fine cell vertices (radians)
    %          - Args: argument structure
    %          - RAD: degrees per radian
    % Output : - NsrcCell: number of sources written

    NsrcCell = 0;

    if isempty(Data)
        return;
    end

    % Filter sources to keep only those inside this fine HTM triangle
    CooRad = Data(:, [Args.ColRAOut, Args.ColDecOut]);
    Flag = celestial.htm.in_polysphere(CooRad, FineCoo, 2);
    FineData = Data(Flag, :);

    % Deduplicate by coordinates — same (RA,Dec) = same source
    if size(FineData, 1) > 1
        NbeforeDedup = size(FineData, 1);
        [~, uniqueIdx] = unique(FineData(:, [Args.ColRAOut, Args.ColDecOut]), 'rows', 'first');
        FineData = FineData(sort(uniqueIdx), :);
        Nremoved = NbeforeDedup - size(FineData, 1);
        if Nremoved > 0 && Args.Verbose
            fprintf('    Cell %d: removed %d duplicate rows\n', FineIdx, Nremoved);
        end
    end

    NsrcCell = size(FineData, 1);

    if NsrcCell > 0
        % Convert to output units
        if strcmpi(Args.OutUnits, 'deg')
            FineData(:, 1) = FineData(:, 1) * RAD;  % RA rad->deg
            FineData(:, 2) = FineData(:, 2) * RAD;  % Dec rad->deg
        end

        [FileName, DataName] = HDF5.get_file_var_from_htmid(Args.CatName, FineIdx, Args.NcatInFile);
        FileName = fullfile(Args.LocalDir, FileName);
        HDF5.save_cat(FileName, DataName, FineData, Args.ColDecOut, Args.IndStep);
    end
end


function [Data, QueryFailed] = downloadAggregateCell(TableName, ColumnsStr, OutputCoo, QueryDescCoos, RAD, Args)
    % Parallel worker for aggregate-up: download all query descendants for
    % one output cell, concatenate, filter to output cell, deduplicate.
    % Input  : - TableName: TAP table name
    %          - ColumnsStr: columns for SELECT
    %          - OutputCoo: 3x2 [Long, Lat] of output cell (radians)
    %          - QueryDescCoos: cell array of 3x2 [Long, Lat] for each query descendant
    %          - RAD: degrees per radian
    %          - Args: argument structure
    % Output : - Data: deduplicated numeric matrix filtered to output cell, or empty
    %          - QueryFailed: true if all queries failed

    Data = [];
    QueryFailed = false;

    % Per-worker temp directory
    try
        W = getCurrentWorker();
        WorkerPid = W.ProcessId;
    catch
        WorkerPid = feature('getpid');
    end
    WorkDir = fullfile(Args.LocalDir, sprintf('tap_w%d', WorkerPid));
    if ~isfolder(WorkDir)
        mkdir(WorkDir);
    end

    Tap = VO.TopCat;

    AllData = [];
    AnyFailed = false;

    for j = 1:numel(QueryDescCoos)
        QCoo = QueryDescCoos{j};
        SearchRadiusDeg = computeCellSearchRadius(QCoo, RAD);
        [CenRA, CenDec] = cellCentroid(QCoo);
        CenterRADeg  = CenRA * RAD;
        CenterDecDeg = CenDec * RAD;
        HTMCooDeg = QCoo * RAD;

        Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRASrc, Args.ColDecSrc, ...
                                       HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                       SearchRadiusDeg, Args.QueryType, Args.WhereClause);

        try
            T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                               Args.TapUrl, Args.TimeoutSec, Args.QueryMethod, WorkDir);
        catch
            AnyFailed = true;
            continue;
        end

        if ~isempty(T) && height(T) > 0
            [D, ColNames] = tableToMatrix(T, Args.ColRA, Args.ColDec, Args.TapUnits);
            [D, ~] = applyPostProcessing(D, ColNames, Args.NullValue, ...
                                          Args.ComputedColumns, Args.DropColumns);
            AllData = [AllData; D]; %#ok<AGROW>
        end
    end

    QueryFailed = AnyFailed && isempty(AllData);

    % Filter to output cell boundaries and deduplicate by coordinates
    if ~isempty(AllData)
        CooRad = AllData(:, [Args.ColRAOut, Args.ColDecOut]);
        Flag = celestial.htm.in_polysphere(CooRad, OutputCoo, 2);
        AllData = AllData(Flag, :);
        [~, uniqueIdx] = unique(AllData(:, [Args.ColRAOut, Args.ColDecOut]), 'rows', 'first');
        if numel(uniqueIdx) < size(AllData, 1)
            AllData = AllData(sort(uniqueIdx), :);
        end
    end

    Data = AllData;
end


function NsrcCell = writeOutputCellDirect(Data, CellIdx, Args, RAD)
    % Write pre-filtered aggregated data directly to HDF5
    % Input  : - Data: numeric matrix (already filtered and deduplicated)
    %          - CellIdx: output-level HTM index for HDF5 naming
    %          - Args: argument structure
    %          - RAD: degrees per radian
    % Output : - NsrcCell: number of sources written

    NsrcCell = 0;

    if isempty(Data)
        return;
    end

    NsrcCell = size(Data, 1);

    if NsrcCell > 0
        % Convert to output units
        if strcmpi(Args.OutUnits, 'deg')
            Data(:, 1) = Data(:, 1) * RAD;
            Data(:, 2) = Data(:, 2) * RAD;
        end

        [FileName, DataName] = HDF5.get_file_var_from_htmid(Args.CatName, CellIdx, Args.NcatInFile);
        FileName = fullfile(Args.LocalDir, FileName);
        HDF5.save_cat(FileName, DataName, Data, Args.ColDecOut, Args.IndStep);
    end
end


function copyIfFileComplete(FIdx, CatName, NcatInFile, LocalDir, TargetDir, Verbose, FileRemainingCells)
    % Decrement remaining-cell counter for the HDF5 file containing FIdx.
    % When counter reaches zero, all expected cells have been processed
    % and the file is copied to TargetDir (with local deletion).
    % FileRemainingCells is a containers.Map (handle class) — modified in place.

    if isempty(TargetDir)
        return;
    end

    [Fn, ~] = HDF5.get_file_var_from_htmid(CatName, FIdx, NcatInFile);
    if FileRemainingCells.isKey(Fn)
        FileRemainingCells(Fn) = FileRemainingCells(Fn) - int32(1);
        if FileRemainingCells(Fn) <= 0
            FullPath = fullfile(LocalDir, Fn);
            if isfile(FullPath)
                tools.os.copyFileOverNFS({FullPath}, TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
                if Verbose
                    fprintf('  Copied completed file: %s\n', Fn);
                end
            end
        end
    end
end


function cleanupWorkerDirs(LocalDir)
    % Remove per-worker temp directories (tap_w*), but only if empty
    % Non-empty dirs indicate incomplete merges and should be preserved
    D = dir(fullfile(LocalDir, 'tap_w*'));
    for i = 1:numel(D)
        if D(i).isdir
            DirPath = fullfile(LocalDir, D(i).name);
            Contents = dir(DirPath);
            % dir() always returns '.' and '..' — check for additional entries
            if numel(Contents) <= 2
                try
                    rmdir(DirPath);
                catch
                    % Ignore cleanup errors
                end
            else
                warning('VO:buildHTMfromTopCat:WorkerDirNotEmpty', ...
                    'Worker dir not empty (incomplete merge?): %s', DirPath);
            end
        end
    end
end


function NsrcCell = writeOutputCellToTemp(Data, CellIdx, Args, RAD, WorkDir)
    % Write aggregated data to per-worker grouped file in worker directory
    % Uses same grouped filename convention as final output

    NsrcCell = 0;
    if isempty(Data)
        return;
    end

    NsrcCell = size(Data, 1);
    if NsrcCell > 0
        if strcmpi(Args.OutUnits, 'deg')
            Data(:, 1) = Data(:, 1) * RAD;
            Data(:, 2) = Data(:, 2) * RAD;
        end
        [FileName, DataName] = HDF5.get_file_var_from_htmid(Args.CatName, CellIdx, Args.NcatInFile);
        FileName = fullfile(WorkDir, FileName);
        HDF5.save_cat(FileName, DataName, Data, Args.ColDecOut, Args.IndStep);
    end
end


function NsrcResults = writeFineCellsToTemp(Data, FineIndices, FineCoos, SkippedFine, Args, RAD, WorkDir)
    % Distribute query data to fine cells and write each to temp file
    % Returns Nx2 matrix [FineIdx, NsrcCell]

    NsrcResults = zeros(numel(FineIndices), 2);
    NsrcResults(:, 1) = FineIndices(:);

    if isempty(Data)
        return;
    end

    for iF = 1:numel(FineIndices)
        FIdx = FineIndices(iF);
        if ismember(FIdx, SkippedFine)
            continue;
        end

        FineCoo = FineCoos{iF};
        CooRad = Data(:, [Args.ColRAOut, Args.ColDecOut]);
        Flag = celestial.htm.in_polysphere(CooRad, FineCoo, 2);
        FineData = Data(Flag, :);

        if size(FineData, 1) > 1
            [~, uniqueIdx] = unique(FineData(:, [Args.ColRAOut, Args.ColDecOut]), 'rows', 'first');
            FineData = FineData(sort(uniqueIdx), :);
        end

        NsrcCell = size(FineData, 1);
        NsrcResults(iF, 2) = NsrcCell;

        if NsrcCell > 0
            if strcmpi(Args.OutUnits, 'deg')
                FineData(:, 1) = FineData(:, 1) * RAD;
                FineData(:, 2) = FineData(:, 2) * RAD;
            end
            [FileName, DataName] = HDF5.get_file_var_from_htmid(Args.CatName, FIdx, Args.NcatInFile);
            FileName = fullfile(WorkDir, FileName);
            HDF5.save_cat(FileName, DataName, FineData, Args.ColDecOut, Args.IndStep);
        end
    end
end


function mergeWorkerTempFiles(LocalDir, CatName, NcatInFile, ColDecOut, IndStep, Verbose)
    % Merge per-worker grouped HDF5 files into final grouped files in LocalDir
    % Workers write to tap_w{PID}/CatName_htm_NNNNNN.hdf5 (same naming as final).
    % When only one worker contributed to a file and no pre-existing destination
    % exists, the file is simply moved (fast path, no HDF5 re-processing).
    % Otherwise datasets are read and re-written to the destination.

    D = dir(fullfile(LocalDir, 'tap_w*'));
    WorkerDirs = {};
    for i = 1:numel(D)
        if D(i).isdir
            WorkerDirs{end+1} = fullfile(LocalDir, D(i).name); %#ok<AGROW>
        end
    end

    if isempty(WorkerDirs)
        return;
    end

    if Verbose
        fprintf('\nMerging worker temp files into grouped HDF5...\n');
    end

    % Collect all grouped HDF5 files across workers
    % Key: filename, Value: cell array of source paths
    FileMap = containers.Map('KeyType', 'char', 'ValueType', 'any');
    Pattern = sprintf('%s_htm_*.hdf5', CatName);
    for iDir = 1:numel(WorkerDirs)
        HdfFiles = dir(fullfile(WorkerDirs{iDir}, Pattern));
        for iFile = 1:numel(HdfFiles)
            Fn = HdfFiles(iFile).name;
            SrcPath = fullfile(WorkerDirs{iDir}, Fn);
            if FileMap.isKey(Fn)
                FileMap(Fn) = [FileMap(Fn), {SrcPath}];
            else
                FileMap(Fn) = {SrcPath};
            end
        end
    end

    FileNames = FileMap.keys;
    MovedCount = 0;
    MergedDsCount = 0;
    FailedDsCount = 0;

    for iF = 1:numel(FileNames)
        Fn = FileNames{iF};
        SrcPaths = FileMap(Fn);
        DstPath = fullfile(LocalDir, Fn);

        if numel(SrcPaths) == 1 && ~isfile(DstPath)
            % Fast path: single worker source, no pre-existing file — just move
            movefile(SrcPaths{1}, DstPath);
            MovedCount = MovedCount + 1;
        else
            % Merge datasets from multiple workers (or into existing file)
            for iSrc = 1:numel(SrcPaths)
                AllMerged = true;
                try
                    Info = h5info(SrcPaths{iSrc});
                    for iDs = 1:numel(Info.Datasets)
                        DsName = Info.Datasets(iDs).Name;
                        % Skip index datasets — save_cat recreates them
                        if endsWith(DsName, '_Ind')
                            continue;
                        end
                        try
                            DsData = h5read(SrcPaths{iSrc}, ['/' DsName]);
                            HDF5.save_cat(DstPath, DsName, DsData, ColDecOut, IndStep);
                            MergedDsCount = MergedDsCount + 1;
                        catch ME2
                            AllMerged = false;
                            FailedDsCount = FailedDsCount + 1;
                            warning('VO:buildHTMfromTopCat:MergeDs', ...
                                'Failed to merge dataset %s from %s: %s', DsName, SrcPaths{iSrc}, ME2.message);
                        end
                    end
                catch ME
                    AllMerged = false;
                    warning('VO:buildHTMfromTopCat:MergeFile', ...
                        'Could not read source file %s: %s', SrcPaths{iSrc}, ME.message);
                end
                if AllMerged
                    delete(SrcPaths{iSrc});
                else
                    warning('VO:buildHTMfromTopCat:MergeIncomplete', ...
                        'Source file NOT deleted (merge incomplete): %s', SrcPaths{iSrc});
                end
            end
        end
    end

    if Verbose
        fprintf('Merged temp files: %d moved, %d datasets merged', MovedCount, MergedDsCount);
        if FailedDsCount > 0
            fprintf(', %d FAILED', FailedDsCount);
        end
        fprintf('.\n');
    end
    if FailedDsCount > 0
        warning('VO:buildHTMfromTopCat:MergeSummary', ...
            '%d datasets failed to merge. Check worker dirs in %s for preserved source files.', ...
            FailedDsCount, LocalDir);
    end
end


function [NsrcCell, QueryFailed] = downloadAggregateCellWithWrite(TableName, ColumnsStr, OutputCoo, QueryDescCoos, CellIdx, RAD, Args)
    % Parallel worker: download, aggregate, filter, dedup, write to temp dir
    % Returns scalar count instead of large data matrix

    NsrcCell = 0;
    QueryFailed = false;

    try
        W = getCurrentWorker();
        WorkerPid = W.ProcessId;
    catch
        WorkerPid = feature('getpid');
    end
    WorkDir = fullfile(Args.LocalDir, sprintf('tap_w%d', WorkerPid));
    if ~isfolder(WorkDir)
        mkdir(WorkDir);
    end

    Tap = VO.TopCat;
    AllData = [];
    AnyFailed = false;

    for j = 1:numel(QueryDescCoos)
        QCoo = QueryDescCoos{j};
        SearchRadiusDeg = computeCellSearchRadius(QCoo, RAD);
        [CenRA, CenDec] = cellCentroid(QCoo);
        CenterRADeg  = CenRA * RAD;
        CenterDecDeg = CenDec * RAD;
        HTMCooDeg = QCoo * RAD;

        Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRASrc, Args.ColDecSrc, ...
                                       HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                       SearchRadiusDeg, Args.QueryType, Args.WhereClause);
        try
            T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                               Args.TapUrl, Args.TimeoutSec, Args.QueryMethod, WorkDir);
        catch
            AnyFailed = true;
            continue;
        end

        if ~isempty(T) && height(T) > 0
            [D, ColNames] = tableToMatrix(T, Args.ColRA, Args.ColDec, Args.TapUnits);
            [D, ~] = applyPostProcessing(D, ColNames, Args.NullValue, ...
                                          Args.ComputedColumns, Args.DropColumns);
            AllData = [AllData; D]; %#ok<AGROW>
        end
    end

    QueryFailed = AnyFailed && isempty(AllData);

    if ~isempty(AllData)
        CooRad = AllData(:, [Args.ColRAOut, Args.ColDecOut]);
        Flag = celestial.htm.in_polysphere(CooRad, OutputCoo, 2);
        AllData = AllData(Flag, :);
        [~, uniqueIdx] = unique(AllData(:, [Args.ColRAOut, Args.ColDecOut]), 'rows', 'first');
        if numel(uniqueIdx) < size(AllData, 1)
            AllData = AllData(sort(uniqueIdx), :);
        end
    end

    NsrcCell = writeOutputCellToTemp(AllData, CellIdx, Args, RAD, WorkDir);
end


function [NsrcResults, QueryFailed] = downloadQueryConeWithWrite(TableName, ColumnsStr, IndHTM, CellCoo, RAD, SearchRadiusDeg, FineIndices, FineCoos, SkippedFine, Args)
    % Parallel worker: download query cone, distribute to fine cells, write to temp
    % Returns small Nx2 matrix [FineIdx, NsrcCell] instead of large data matrix

    NsrcResults = zeros(numel(FineIndices), 2);
    NsrcResults(:, 1) = FineIndices(:);
    QueryFailed = false;

    try
        W = getCurrentWorker();
        WorkerPid = W.ProcessId;
    catch
        WorkerPid = feature('getpid');
    end
    WorkDir = fullfile(Args.LocalDir, sprintf('tap_w%d', WorkerPid));
    if ~isfolder(WorkDir)
        mkdir(WorkDir);
    end

    Tap = VO.TopCat;

    [CenRA, CenDec] = cellCentroid(CellCoo);
    CenterRADeg  = CenRA * RAD;
    CenterDecDeg = CenDec * RAD;
    HTMCooDeg = CellCoo * RAD;

    Query = constructSpatialQuery(TableName, ColumnsStr, Args.ColRASrc, Args.ColDecSrc, ...
                                   HTMCooDeg, CenterRADeg, CenterDecDeg, ...
                                   SearchRadiusDeg, Args.QueryType, Args.WhereClause);
    try
        T = queryWithRetry(Tap, Query, Args.MaxRetries, Args.RetryPauseSec, ...
                           Args.TapUrl, Args.TimeoutSec, Args.QueryMethod, WorkDir);
    catch ME
        warning('VO:buildHTMfromTopCat:QueryFailed', ...
            'Query cell %d: Query failed after %d retries: %s', IndHTM, Args.MaxRetries, char(ME.message));
        QueryFailed = true;
        return;
    end

    if isempty(T) || height(T) == 0
        return;
    end

    [Data, ColNames] = tableToMatrix(T, Args.ColRA, Args.ColDec, Args.TapUnits);
    [Data, ~] = applyPostProcessing(Data, ColNames, Args.NullValue, ...
                                     Args.ComputedColumns, Args.DropColumns);

    NsrcResults = writeFineCellsToTemp(Data, FineIndices, FineCoos, SkippedFine, Args, RAD, WorkDir);
end


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


function Exists = checkHTMExists(CatName, IndHTM, NcatInFile, LocalDir)
    % Check if an HTM cell already exists in HDF5 files
    [FileName, DataName] = HDF5.get_file_var_from_htmid(CatName, IndHTM, NcatInFile);
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


function Nsrc = getHTMSourceCount(CatName, IndHTM, NcatInFile, LocalDir)
    % Get source count from an existing HTM cell in HDF5 file
    [FileName, DataName] = HDF5.get_file_var_from_htmid(CatName, IndHTM, NcatInFile);
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


function [Data, ColNames, NumericMask, ReorderIdx] = tableToMatrixWithInfo(T, ColRA, ColDec, TapUnits)
    % Convert MATLAB table to numeric matrix with RA/Dec in columns 1,2
    % Also returns filtering info to apply same transformation to original column names
    % Input  : - T: MATLAB table from TAP query
    %          - ColRA: RA column name
    %          - ColDec: Dec column name
    %          - TapUnits: coordinate units from TAP ('rad'|'deg')
    % Output : - Data: numeric matrix with RA/Dec in radians (columns 1,2)
    %          - ColNames: cell array of column names (filtered and reordered)
    %          - NumericMask: logical mask of which original columns are numeric
    %          - ReorderIdx: indices showing how filtered columns were reordered

    ColNames = T.Properties.VariableNames;

    % Find RA/Dec columns (case-insensitive)
    IdxRA = find(strcmpi(ColNames, ColRA), 1);
    IdxDec = find(strcmpi(ColNames, ColDec), 1);

    if isempty(IdxRA) || isempty(IdxDec)
        error('Could not find RA column "%s" or Dec column "%s" in table', ColRA, ColDec);
    end

    % Keep only numeric columns (drop strings, cells, etc.)
    NumericMask = varfun(@isnumeric, T, 'OutputFormat', 'uniform');
    if ~NumericMask(IdxRA) || ~NumericMask(IdxDec)
        error('RA column "%s" or Dec column "%s" is not numeric', ColRA, ColDec);
    end
    T = T(:, NumericMask);
    ColNames = ColNames(NumericMask);

    % Update RA/Dec indices after filtering
    IdxRA = find(strcmpi(ColNames, ColRA), 1);
    IdxDec = find(strcmpi(ColNames, ColDec), 1);

    % Convert table to matrix
    Data = table2array(T);

    % Reorder so RA=col1, Dec=col2
    if IdxRA ~= 1 || IdxDec ~= 2
        OtherCols = setdiff(1:width(T), [IdxRA, IdxDec]);
        ReorderIdx = [IdxRA, IdxDec, OtherCols];
        Data = Data(:, ReorderIdx);
        ColNames = ColNames(ReorderIdx);
    else
        ReorderIdx = 1:width(T);
    end

    % Convert to radians (always output radians for in_polysphere)
    if strcmpi(TapUnits, 'deg')
        Data(:, 1:2) = Data(:, 1:2) / constant.RAD;
    end
end


function T = queryWithRetry(Tap, Query, MaxRetries, RetryPauseSec, TapUrl, TimeoutSec, QueryMethod, WorkDir)
    % Execute TAP query with retry logic
    for Attempt = 1:MaxRetries
        try
            T = Tap.query(Query, 'TapUrl', TapUrl, 'TimeoutSec', TimeoutSec, 'Method', QueryMethod, 'WorkDir', WorkDir);
            return;
        catch ME
            if Attempt < MaxRetries
                fprintf('  Query failed (attempt %d/%d): %s\n', Attempt, MaxRetries, ME.message);
                pause(RetryPauseSec);
            else
                rethrow(ME);
            end
        end
    end
end


function ColNames = parseCSVHeader(HeaderLine)
    % Parse CSV header line, properly handling quoted fields
    % Input  : - HeaderLine: string containing CSV header
    % Output : - ColNames: cell array of column names

    ColNames = {};
    HeaderLine = char(HeaderLine);
    Pos = 1;
    Len = length(HeaderLine);

    while Pos <= Len
        if HeaderLine(Pos) == '"'
            % Quoted field - find matching close quote
            Pos = Pos + 1;  % skip opening quote
            FieldStart = Pos;
            while Pos <= Len
                if HeaderLine(Pos) == '"'
                    if Pos < Len && HeaderLine(Pos+1) == '"'
                        % Escaped quote - skip both
                        Pos = Pos + 2;
                    else
                        % End of quoted field
                        break;
                    end
                else
                    Pos = Pos + 1;
                end
            end
            Field = HeaderLine(FieldStart:Pos-1);
            % Unescape double quotes
            Field = strrep(Field, '""', '"');
            ColNames{end+1} = Field; %#ok<AGROW>
            Pos = Pos + 1;  % skip closing quote
            % Skip comma if present
            if Pos <= Len && HeaderLine(Pos) == ','
                Pos = Pos + 1;
            end
        else
            % Unquoted field - find comma or end
            FieldStart = Pos;
            while Pos <= Len && HeaderLine(Pos) ~= ','
                Pos = Pos + 1;
            end
            Field = strtrim(HeaderLine(FieldStart:Pos-1));
            ColNames{end+1} = Field; %#ok<AGROW>
            % Skip comma
            if Pos <= Len && HeaderLine(Pos) == ','
                Pos = Pos + 1;
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
            CompDef = ComputedColumns{iComp};
            NewName = CompDef{1};
            Col1Name = CompDef{2};
            Col2Name = CompDef{3};
            Op = CompDef{4};

            % Find column indices
            Idx1 = find(strcmpi(ColNames, Col1Name), 1);
            Idx2 = find(strcmpi(ColNames, Col2Name), 1);

            if isempty(Idx1)
                warning('ComputedColumns: column "%s" not found, skipping', Col1Name);
                continue;
            end
            if isempty(Idx2)
                warning('ComputedColumns: column "%s" not found, skipping', Col2Name);
                continue;
            end

            % Compute new column
            switch lower(Op)
                case 'minus'
                    NewCol = Data(:, Idx1) - Data(:, Idx2);
                case 'plus'
                    NewCol = Data(:, Idx1) + Data(:, Idx2);
                case 'times'
                    NewCol = Data(:, Idx1) .* Data(:, Idx2);
                case 'divide'
                    NewCol = Data(:, Idx1) ./ Data(:, Idx2);
                otherwise
                    warning('ComputedColumns: unknown operation "%s", skipping', Op);
                    continue;
            end

            % Append new column
            Data = [Data, NewCol]; %#ok<AGROW>
            ColNames = [ColNames, {NewName}]; %#ok<AGROW>
        end
    end

    % 3. Drop columns
    if ~isempty(DropColumns)
        if ischar(DropColumns)
            DropColumns = {DropColumns};
        end

        KeepMask = true(1, numel(ColNames));
        for iDrop = 1:numel(DropColumns)
            Idx = find(strcmpi(ColNames, DropColumns{iDrop}), 1);
            if ~isempty(Idx)
                KeepMask(Idx) = false;
            end
        end

        Data = Data(:, KeepMask);
        ColNames = ColNames(KeepMask);
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
            CompDef = ComputedColumns{iComp};
            NewName = CompDef{1};
            ColNames = [ColNames, {NewName}]; %#ok<AGROW>
        end
    end

    % 2. Drop columns
    if ~isempty(DropColumns)
        if ischar(DropColumns)
            DropColumns = {DropColumns};
        end

        KeepMask = true(1, numel(ColNames));
        for iDrop = 1:numel(DropColumns)
            Idx = find(strcmpi(ColNames, DropColumns{iDrop}), 1);
            if ~isempty(Idx)
                KeepMask(Idx) = false;
            end
        end

        ColNames = ColNames(KeepMask);
    end
end


function ColUnits = applyColUnitsPostProcessing(ColUnits, ComputedColumns, DropColumns, ColNames)
    % Update column units to reflect computed and dropped columns
    % Mirrors applyColCellPostProcessing but operates on units array
    % Input  : - ColUnits: cell array of unit strings (same length as ColNames)
    %          - ComputedColumns: cell array of {NewName, Col1, Col2, Op} or Nx4 cell
    %          - DropColumns: cell array of column names to drop
    %          - ColNames: cell array of column names BEFORE post-processing
    % Output : - ColUnits: updated unit strings

    if isempty(ColUnits)
        return;
    end

    % 1. Add units for computed columns (inherit from first source column)
    if ~isempty(ComputedColumns)
        if ~iscell(ComputedColumns{1})
            ComputedColumns = {ComputedColumns};
        end

        for iComp = 1:numel(ComputedColumns)
            CompDef = ComputedColumns{iComp};
            Col1Name = CompDef{2};
            % Inherit unit from first source column
            Idx1 = find(strcmpi(ColNames, Col1Name), 1);
            if ~isempty(Idx1) && Idx1 <= numel(ColUnits)
                CompUnit = ColUnits{Idx1};
            else
                CompUnit = '';
            end
            ColUnits = [ColUnits, {CompUnit}]; %#ok<AGROW>
            ColNames = [ColNames, CompDef(1)]; %#ok<AGROW>  % keep in sync
        end
    end

    % 2. Drop columns (by name, matching against ColNames which is kept in sync)
    if ~isempty(DropColumns)
        if ischar(DropColumns)
            DropColumns = {DropColumns};
        end

        KeepMask = true(1, numel(ColUnits));
        for iDrop = 1:numel(DropColumns)
            Idx = find(strcmpi(ColNames, DropColumns{iDrop}), 1);
            if ~isempty(Idx)
                KeepMask(Idx) = false;
            end
        end

        ColUnits = ColUnits(KeepMask);
    end
end


function ColUnits = extractUnitsFromVOTable(SampleQuery, TapUrl, LocalDir, Verbose)
    % Extract column units from TAP service by querying VOTable metadata
    % Runs STILTS with VOTable output for the sample query, then parses
    % <FIELD unit="..."> attributes from the XML.
    % Input  : - SampleQuery: ADQL query string (e.g., 'SELECT TOP 1 ...')
    %          - TapUrl: TAP service URL
    %          - LocalDir: local directory for temp files
    %          - Verbose: print progress
    % Output : - ColUnits: cell array of unit strings (one per FIELD in VOTable)
    %            Empty {} if extraction fails.

    ColUnits = {};
    try
        TempVotFile = fullfile(LocalDir, 'temp_units.vot');
        JarPath = VO.TopCat.getStiltsJarPath();
        TapUrlClean = char(TapUrl);
        if endsWith(TapUrlClean, '/'), TapUrlClean = TapUrlClean(1:end-1); end
        AdqlEsc = VO.TopCat.escapeForShellDoubleQuotes(SampleQuery);

        Cmd = sprintf('java -Xmx1g -jar "%s" tapquery tapurl="%s" language=ADQL adql="%s" omode=out ofmt=votable out="%s" sync=true 2>&1', ...
            JarPath, TapUrlClean, AdqlEsc, TempVotFile);
        [Status, ~] = system(Cmd);

        if Status == 0 && isfile(TempVotFile)
            VotContent = fileread(TempVotFile);
            ColUnits = parseVOTableFieldUnits(VotContent);
            delete(TempVotFile);
            if Verbose
                fprintf('Extracted %d column units from VOTable metadata.\n', numel(ColUnits));
            end
        else
            if Verbose
                fprintf('Warning: Could not retrieve VOTable metadata for units.\n');
            end
        end
    catch ME
        if Verbose
            fprintf('Could not extract column units from VOTable: %s\n', ME.message);
        end
    end
end


function ColUnits = parseVOTableFieldUnits(VotContent)
    % Parse FIELD elements from VOTable XML to extract unit attributes
    % Input  : - VotContent: string containing VOTable XML content
    % Output : - ColUnits: cell array of unit strings in column order.
    %            Fields without a unit attribute get empty string ''.

    ColUnits = {};

    % Match all <FIELD ...> elements (self-closing or with body)
    FieldPattern = '<FIELD\s[^>]*?/?>';
    Fields = regexp(VotContent, FieldPattern, 'match');

    for iF = 1:numel(Fields)
        FieldTag = Fields{iF};
        % Extract unit attribute value if present
        UnitMatch = regexp(FieldTag, '\sunit="([^"]*)"', 'tokens');
        if ~isempty(UnitMatch)
            ColUnits{end+1} = UnitMatch{1}{1}; %#ok<AGROW>
        else
            ColUnits{end+1} = ''; %#ok<AGROW>
        end
    end
end


function [CenLong, CenLat] = cellCentroid(Coo)
    % Compute the centroid of an HTM cell in a wraparound-safe way
    % by averaging in cosine direction (3D Cartesian) space.
    % Input  : - Coo: 3x2 matrix [Long, Lat] in radians
    % Output : - CenLong: centroid longitude in radians [0, 2*pi]
    %          - CenLat:  centroid latitude in radians [-pi/2, pi/2]

    [CD1, CD2, CD3] = celestial.coo.coo2cosined(Coo(:,1), Coo(:,2));
    [CenLong, CenLat] = celestial.coo.cosined2coo(mean(CD1), mean(CD2), mean(CD3));
end
