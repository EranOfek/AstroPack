function buildHTMfromFiles(Args)
% Build an HTM catalog from local or remote data files
% Package: VO.prep
% Description: Processes a collection of FITS or text files into an
%              HTM-structured HDF5 catalog. Supports downloading from
%              remote URLs with caching and resume.
%
%              Two processing modes are available (see ProcessMode):
%              - 'band' (default): accumulates all files per Dec band.
%                Good for small/medium catalogs.
%              - 'perfile': processes one sweep file at a time with
%                margin from neighbors. For very large catalogs that
%                would OOM in band mode (e.g., DECaLS with ~2 billion
%                sources). Requires sweep-style filenames with RA/Dec.
%
%              The function works in five steps:
%              1. Scrape file list from URL or local directory
%              2. Save column metadata (ColCell) upfront
%              3. Process sources into HTM cells via PostReadFun +
%                 VO.prep.build_htm_catalog. Completed grouped HDF5
%                 files are copied to TargetDir incrementally.
%              4. Build index with VO.prep.getNsrcFast +
%                 celestial.htm.saveHTMIndexFast
%              5. Copy remaining files and index to TargetDir
%
%              When Resume is true, completed regions (all HTM cells
%              exist in HDF5) are skipped entirely without reading
%              source files. Downloaded files not needed by future
%              processing are cleaned up (CleanCache).
%
% Input  : * ...,key,val,...
%            --- Source files ---
%            'SourceURL'    - URL of directory listing to scrape.
%            'SourceDir'    - Local directory with FITS files.
%                             One of SourceURL or SourceDir is required.
%            'FilePattern'  - Glob pattern for files. Default: '*.fits'
%                             Supports FITS and text files (.txt,.csv,.tsv,.dat).
%                             Text files are read with readtable; "null" treated
%                             as missing (NaN). For large text files spanning
%                             the full sky, the data is cached in memory.
%            --- Column selection ---
%            'Columns'      - Cell array of source column names to keep.
%                             Default: {} (all columns).
%            'ColNames'     - Cell array of output column names for HTM.
%                             Default: {} (use source names).
%            'ColUnits'     - Cell array of output column units.
%            'ColRA'        - RA column index in output matrix. Default: 1.
%            'ColDec'       - Dec column index in output matrix. Default: 2.
%            'CoorUnits'    - Input coordinate units 'deg'|'rad'. Default: 'deg'
%            'PostReadFun'  - Function handle: Mat = fun(Input).
%                             If provided, overrides Columns selection.
%                             For FITS files, Input is a table from readTable1.
%                             For text files, Input is the filename (string),
%                             allowing fast I/O via textscan.
%                             Must return a numeric matrix with RA in ColRA
%                             and Dec in ColDec (in original CoorUnits).
%            --- HTM ---
%            'CatName'      - Output catalog base name.
%            'HTM_Level'    - HTM level. Default: 9.
%            'NcatInFile'   - HTM cells per HDF5 file. Default: 100.
%            'IndStep'      - Index step for HDF5.save_cat. Default: 30.
%            --- Directories ---
%            'LocalDir'     - Local directory for writing HDF5 files.
%                             Default: pwd.
%            'TargetDir'    - Remote directory for final HDF5 files.
%                             Files are copied via tools.os.copyFileOverNFS
%                             after processing and deleted from LocalDir.
%                             Default: '' (no copy, keep in LocalDir).
%            'DownloadDir'  - Temp directory for downloaded files.
%                             Default: tempdir.
%            --- Processing ---
%            'ProcessMode'  - Processing mode. Default: 'band'.
%                             'band'   : accumulate all files per Dec band.
%                                        Good for small/medium catalogs.
%                             'perfile': process one file at a time with
%                                        margin from neighbors. For very
%                                        large catalogs (DECaLS, etc.) that
%                                        would OOM in band mode.
%            'ParseRangeFun'- Optional function handle for custom file
%                             range parsing:
%                             [DecLo,DecHi,RALo,RAHi] = fun(filepath).
%                             Returns ranges in degrees.
%                             If empty (default):
%                               - for *.hdf5/*.h5 files, reads MinDec,
%                                 MaxDec, MinRA, MaxRA attributes from
%                                 the '/data' dataset (in radians)
%                               - otherwise uses sweep-style filename parser.
%            'DecBandWidth' - Dec band width [deg] for 'band' mode. Default: 5.
%            'Resume'       - Skip existing HTM cells. Default: true.
%                             In band mode, completed bands are skipped
%                             entirely (no file reading).
%                             In perfile mode, completed file regions
%                             are skipped.
%            'CleanCache'   - Delete cached downloads after each Dec
%                             band (including skipped bands). Default: true.
%                             Only deletes files not needed by future bands.
%            'Verbose'      - Print progress. Default: true.
%
% Output : null
% Author : Dana Kovaleva (Mar 2026)
% Example:
%   % DECaLS DR10 — band mode (small/medium catalogs):
%   VO.prep.buildHTMfromFiles(...
%       'SourceURL', 'https://portal.nersc.gov/cfs/cosmo/data/legacysurvey/dr10/south/sweep/10.1/', ...
%       'ProcessMode', 'band', ...
%       'PostReadFun', @VO.prep.decalsPostRead, ...
%       'ColNames', {'RA','Dec','RA_IVAR','DEC_IVAR','Type', ...
%           'Flux_g','Flux_r','Flux_i','Flux_z', ...
%           'Flux_W1','Flux_W2','Flux_W3','Flux_W4', ...
%           'FluxIvar_g','FluxIvar_r','FluxIvar_i','FluxIvar_z', ...
%           'FluxIvar_W1','FluxIvar_W2','FluxIvar_W3','FluxIvar_W4', ...
%           'MaskBits','ShapeR'}, ...
%       'CatName', 'DECaLS10', 'HTM_Level', 9, ...
%       'LocalDir', '/home/dana/tmp/DECaLS10/htm/', ...
%       'TargetDir', '/euclid/catsHTM/NewCats/DECaLS10/', ...
%       'DownloadDir', '/home/dana/tmp/DECaLS10/');
%
%   % DECaLS DR10 — pre-converted HDF5, per-file mode (recommended):
%   % Step 1: Download sweep FITS and convert to HDF5 (one-time)
%   %   VO.prep.decalsSweepToHdf5( ...
%   %       'https://portal.nersc.gov/cfs/cosmo/data/legacysurvey/dr10/south/sweep/10.1/', ...
%   %       '/home/dana/tmp/DECaLS10/fits/', ...
%   %       '/home/dana/tmp/DECaLS10/hdf5/');
%   % Step 2: Build HTM catalog
%   VO.prep.buildHTMfromFiles(...
%       'SourceDir', '/home/dana/tmp/DECaLS10/hdf5/', ...
%       'FilePattern', '*.hdf5', ...
%       'ProcessMode', 'perfile', ...
%       'PostReadFun',   @(F) h5read(F, '/data'), ...
%       'CoorUnits', 'rad', ...
%       'ColNames', {'RA','Dec','RA_IVAR','DEC_IVAR','Type', ...
%           'Flux_g','Flux_r','Flux_i','Flux_z', ...
%           'Flux_W1','Flux_W2','Flux_W3','Flux_W4', ...
%           'FluxIvar_g','FluxIvar_r','FluxIvar_i','FluxIvar_z', ...
%           'FluxIvar_W1','FluxIvar_W2','FluxIvar_W3','FluxIvar_W4', ...
%           'MaskBits','ShapeR'}, ...
%       'CatName', 'DECaLS10', 'HTM_Level', 9, ...
%       'LocalDir',  '/home/dana/tmp/DECaLS10/htm/', ...
%       'TargetDir', '/euclid/catsHTM/NewCats/DECaLS10/');
%
%   % PS1DR2 — pre-converted HDF5, per-file mode:
%   % Step 1: Convert CSV to HDF5 (one-time)
%   %   VO.prep.ps1dr2CsvToHdf5('~/tmp/PS1DR2/csv/', '~/tmp/PS1DR2/hdf5/');
%   % Step 2: Build HTM catalog
%   VO.prep.buildHTMfromFiles(...
%       'SourceDir', '~/tmp/PS1DR2/hdf5/', ...
%       'FilePattern', '*.hdf5', ...
%       'ProcessMode', 'perfile', ...
%       'PostReadFun',   @(F) h5read(F, '/data'), ...
%       'CoorUnits', 'rad', ...
%       'ColNames', {'RA','Dec','raerr','decerr', ...
%           'objinfoflag','qualityflag','epochmean','posmeanchisq', ...
%           'gqfperfect','gmeanpsfmag','gmeanpsfmagerr','gmeanpsfmagstd', ...
%           'gmeanpsfmagnpt','gmeanpsfmagmin','gmeanpsfmagmax','g_delta_psf_kron', ...
%           'rqfperfect','rmeanpsfmag','rmeanpsfmagerr','rmeanpsfmagstd', ...
%           'rmeanpsfmagnpt','rmeanpsfmagmin','rmeanpsfmagmax','r_delta_psf_kron', ...
%           'iqfperfect','imeanpsfmag','imeanpsfmagerr','imeanpsfmagstd', ...
%           'imeanpsfmagnpt','imeanpsfmagmin','imeanpsfmagmax','i_delta_psf_kron', ...
%           'zqfperfect','zmeanpsfmag','zmeanpsfmagerr','zmeanpsfmagstd', ...
%           'zmeanpsfmagnpt','zmeanpsfmagmin','zmeanpsfmagmax', ...
%           'yqfperfect','ymeanpsfmag','ymeanpsfmagerr','ymeanpsfmagstd', ...
%           'ymeanpsfmagnpt','ymeanpsfmagmin','ymeanpsfmagmax', ...
%           'gfmeanmagr5','gfmeanmagr5err','rfmeanmagr5','rfmeanmagr5err', ...
%           'ifmeanmagr5','ifmeanmagr5err','zfmeanmagr5','zfmeanmagr5err', ...
%           'yfmeanmagr5','yfmeanmagr5err'}, ...
%       'CatName', 'PS1DR2', 'HTM_Level', 9, ...
%       'LocalDir', '~/tmp/PS1DR2/htm/', ...
%       'TargetDir', '/euclid/catsHTM/NewCats/PS1DR2/');

    arguments
        Args.SourceURL    string = ""
        Args.SourceDir    string = ""
        Args.FilePattern  string = "*.fits"
        Args.Columns      cell = {}
        Args.ColNames     cell = {}
        Args.ColUnits     cell = {}
        Args.ColRA        double = 1
        Args.ColDec       double = 2
        Args.CoorUnits    string = "deg"
        Args.PostReadFun  = []
        Args.CatName      string = ""
        Args.HTM_Level    double = 9
        Args.NcatInFile   double = 100
        Args.IndStep      double = 30
        Args.LocalDir     string = string(pwd)
        Args.TargetDir    string = ""
        Args.DownloadDir  string = string(tempdir)
        Args.ProcessMode  string = "band"   % 'band' or 'perfile'
        Args.ParseRangeFun = []            % function handle: [DecLo,DecHi,RALo,RAHi] = fun(basename)
        Args.DecBandWidth double = 5
        Args.Resume       logical = true
        Args.CleanCache   logical = true
        Args.Verbose      logical = true
    end

    LocalDir    = char(Args.LocalDir);
    TargetDir   = char(Args.TargetDir);
    DownloadDir = char(Args.DownloadDir);
    CatName     = char(Args.CatName);
    RAD         = 180 / pi;

    % Note: CleanCache is independent of Resume. cleanDownloadCache only
    % deletes files not needed by future bands/files, so files for the
    % current (possibly incomplete) processing are always kept.

    if ~exist(LocalDir, 'dir'),    mkdir(LocalDir); end
    if ~exist(DownloadDir, 'dir'), mkdir(DownloadDir); end
    if ~isempty(TargetDir) && ~exist(TargetDir, 'dir')
        mkdir(TargetDir);
    end

    %------------------------------------------------------------------
    % Step 1: Get file list
    %------------------------------------------------------------------
    if strlength(Args.SourceURL) > 0
        AllFiles = scrapeFileList(char(Args.SourceURL), char(Args.FilePattern));
        IsRemote = true;
    elseif strlength(Args.SourceDir) > 0
        D = dir(fullfile(char(Args.SourceDir), char(Args.FilePattern)));
        AllFiles = fullfile({D.folder}, {D.name});
        IsRemote = false;
    else
        error('VO:prep:buildHTMfromFiles', 'Specify SourceURL or SourceDir');
    end

    Nfiles = numel(AllFiles);
    if Args.Verbose
        fprintf('Found %d files\n', Nfiles);
    end

    %------------------------------------------------------------------
    % Step 2: Determine RA/Dec range for each file
    %------------------------------------------------------------------
    FileDecRanges = nan(Nfiles, 2);
    FileRARanges  = nan(Nfiles, 2);
    for Ifile = 1:Nfiles
        [~, ~, Ext] = fileparts(AllFiles{Ifile});
        if ~isempty(Args.ParseRangeFun)
            % User-provided parser
            [DecLo, DecHi, RALo, RAHi] = Args.ParseRangeFun(AllFiles{Ifile});
        elseif strcmpi(Ext, '.hdf5') || strcmpi(Ext, '.h5')
            % HDF5 file: read MinDec/MaxDec/MinRA/MaxRA attributes
            % from the '/data' dataset (stored in radians by converters
            % such as decalsSweepToHdf5 or ps1dr2CsvToHdf5).
            [DecLo, DecHi, RALo, RAHi] = readHdf5RangeAttrs(AllFiles{Ifile});
        else
            error('VO:prep:buildHTMfromFiles', ...
                ['Cannot determine RA/Dec range for file %s.\n' ...
                 'Either use HDF5 files with range attributes, or pass ' ...
                 'a ParseRangeFun argument.'], AllFiles{Ifile});
        end
        FileDecRanges(Ifile, :) = [DecLo, DecHi];
        FileRARanges(Ifile, :)  = [RALo, RAHi];
    end

    if any(isnan(FileDecRanges(:)))
        error('VO:prep:buildHTMfromFiles', ...
            'Dec range could not be determined for one or more files.');
    end
    if any(isnan(FileRARanges(:)))
        FileRARanges(:, 1) = 0;
        FileRARanges(:, 2) = 360;
    end

    %------------------------------------------------------------------
    % Step 3: Build HTM by Dec bands
    %------------------------------------------------------------------
    [HTM, LevelHTM] = celestial.htm.htm_build(Args.HTM_Level);
    RadiusHTM = (sqrt(2) * 90 / (2^(Args.HTM_Level - 1))) / RAD;
    MarginDeg = RadiusHTM * RAD * 1.5;

    ListIndexHTM = LevelHTM(Args.HTM_Level).ptr;
    Nhtm = numel(ListIndexHTM);

    % Accumulate Nsrc across build_htm_catalog calls
    % NaN = not yet processed; 0 = processed but empty; >0 = has sources
    Nsrc = [ListIndexHTM(:), nan(Nhtm, 1)];

    DecEdges = -90 : Args.DecBandWidth : 90;
    Nbands = numel(DecEdges) - 1;

    OrigDir = pwd;
    cd(LocalDir);

    TotalTic = tic;

    % Save ColCell file upfront so it's available during processing
    HDF5.save_cat_colcell(CatName, Args.ColNames, Args.ColUnits);
    if Args.Verbose
        fprintf('Saved column metadata: %s_htmColCell.mat\n', CatName);
    end

    % Precompute max MeanDec per grouped HDF5 file for incremental NFS copy.
    % A grouped file is safe to copy when its MaxDec < SafeDecRad
    % (= min Dec of remaining unprocessed files - margin).
    if ~isempty(TargetDir)
        [HdfFileMaxDec, HdfFileNames] = precomputeFileMaxDec( ...
            HTM, ListIndexHTM, CatName, Args.NcatInFile);
        CopiedFiles = false(numel(HdfFileNames), 1);

        % Copy ColCell file immediately
        ColCellFile = fullfile(LocalDir, sprintf('%s_htmColCell.mat', CatName));
        if isfile(ColCellFile)
            tools.os.copyFileOverNFS({ColCellFile}, TargetDir, ...
                'RemoteUser', 'euclid', 'RemoveOrigin', true);
            if Args.Verbose
                fprintf('Copied: %s_htmColCell.mat\n', CatName);
            end
        end
    end

    % Load completion log for Resume: tracks which source files have been
    % fully processed. More reliable than checking HTM cells, because
    % empty cells leave no HDF5 dataset.
    CompletionLog = fullfile(LocalDir, sprintf('%s_completed.mat', CatName));
    if Args.Resume && isfile(CompletionLog)
        Tmp = load(CompletionLog, 'CompletedFiles');
        CompletedFiles = Tmp.CompletedFiles;
        if Args.Verbose
            fprintf('Loaded completion log: %d files already processed\n', ...
                numel(CompletedFiles));
        end
    else
        CompletedFiles = {};
    end

    try
    if strcmpi(Args.ProcessMode, 'perfile')
        %==================================================================
        % PER-FILE MODE: process one sweep file at a time with neighbors
        %==================================================================
        if ~isempty(TargetDir)
            [Nsrc, CompletedFiles] = processPerFile(Nsrc, CompletedFiles, CompletionLog, ...
                AllFiles, Nfiles, FileDecRanges, FileRARanges, ...
                IsRemote, DownloadDir, MarginDeg, RAD, HTM, LevelHTM, ...
                ListIndexHTM, CatName, LocalDir, TargetDir, ...
                HdfFileMaxDec, HdfFileNames, CopiedFiles, Args);
        else
            [Nsrc, CompletedFiles] = processPerFile(Nsrc, CompletedFiles, CompletionLog, ...
                AllFiles, Nfiles, FileDecRanges, FileRARanges, ...
                IsRemote, DownloadDir, MarginDeg, RAD, HTM, LevelHTM, ...
                ListIndexHTM, CatName, LocalDir, '', ...
                [], {}, [], Args);
        end

    else
        %==============================================================
        % BAND MODE: accumulate all files per Dec band (original behavior)
        %==============================================================

        % File data cache: avoid re-reading files that span multiple bands.
        % FileCache{Idx} holds the full numeric matrix (with RA/Dec in radians)
        % for file Idx. Cleared when the file is no longer needed.
        FileCache = cell(Nfiles, 1);

        for Iband = 1:Nbands
            DecLoDeg = DecEdges(Iband);
            DecHiDeg = DecEdges(Iband + 1);
            DecLoRad = DecLoDeg / RAD;
            DecHiRad = DecHiDeg / RAD;

            % Find files overlapping this band (with margin)
            OverlapIdx = find( ...
                FileDecRanges(:,2) > (DecLoDeg - MarginDeg) & ...
                FileDecRanges(:,1) < (DecHiDeg + MarginDeg));

            if isempty(OverlapIdx)
                if Args.Verbose
                    fprintf('[Band %d/%d] Dec [%+.0f, %+.0f]: no files, skipping\n', ...
                        Iband, Nbands, DecLoDeg, DecHiDeg);
                end
                continue;
            end

            if Args.Verbose
                fprintf('\n[Band %d/%d] Dec [%+.0f, %+.0f] deg: %d files\n', ...
                    Iband, Nbands, DecLoDeg, DecHiDeg, numel(OverlapIdx));
            end

            % When resuming, check if all source files for this band were
            % already processed in a previous run
            if Args.Resume
                BandFileNames = AllFiles(OverlapIdx);
                AllDone = all(ismember(BandFileNames, CompletedFiles));
                if AllDone
                    if Args.Verbose
                        fprintf('  All files processed, skipping band\n');
                    end
                    % Fill Nsrc for skipped cells from existing HDF5 files
                    Nsrc = fillNsrcFromHDF5(Nsrc, HTM, ListIndexHTM, ...
                        DecLoRad, DecHiRad, CatName, Args.NcatInFile, TargetDir);
                    % Clean files not needed by future bands
                    if IsRemote && Args.CleanCache
                        cleanDownloadCache(AllFiles, OverlapIdx, FileDecRanges, ...
                            DownloadDir, Iband, Nbands, DecEdges, MarginDeg);
                    end
                    continue;
                end
            end

            BandTic = tic;

            % Load and accumulate data for this band
            AllData = [];
            MarginRad = MarginDeg / RAD;

            for K = 1:numel(OverlapIdx)
                Idx = OverlapIdx(K);
                [~, Bn, Ext] = fileparts(AllFiles{Idx});

                % Use cached data if available
                if ~isempty(FileCache{Idx})
                    Mat = FileCache{Idx};
                    if Args.Verbose
                        fprintf('  [%d/%d] Using cached %s\n', ...
                            K, numel(OverlapIdx), Bn);
                    end
                else
                    % Download if remote
                    if IsRemote
                        LocalFile = fullfile(DownloadDir, [Bn Ext]);
                        if ~exist(LocalFile, 'file')
                            if Args.Verbose
                                fprintf('  [%d/%d] Downloading %s ...\n', ...
                                    K, numel(OverlapIdx), Bn);
                            end
                            [Status, ~] = system(sprintf( ...
                                'wget -q -c -O "%s" "%s"', LocalFile, AllFiles{Idx}));
                            if Status ~= 0
                                fprintf('  WARNING: download failed for %s\n', Bn);
                                continue;
                            end
                        else
                            if Args.Verbose
                                fprintf('  [%d/%d] Using downloaded %s\n', ...
                                    K, numel(OverlapIdx), Bn);
                            end
                        end
                    else
                        LocalFile = AllFiles{Idx};
                    end

                    % Read table (FITS or text)
                    if Args.Verbose
                        fprintf('  Reading %s ...\n', Bn);
                    end
                    [~, ~, Fext] = fileparts(LocalFile);
                    IsFits = strcmpi(Fext, '.fits') || strcmpi(Fext, '.fit');

                    % Select columns / transform (with retry on corrupt files)
                    ReadOK = false;
                    for Iattempt = 1:2
                        try
                            if ~isempty(Args.PostReadFun)
                                % For FITS, pass table to PostReadFun.
                                % For all other formats (text, HDF5, etc.),
                                % pass filename — PostReadFun handles I/O.
                                if ~IsFits
                                    Mat = Args.PostReadFun(LocalFile);
                                else
                                    T = FITS.readTable1(LocalFile, 'OutClass', []);
                                    Mat = Args.PostReadFun(T);
                                    clear T;
                                end
                            else
                                if IsFits
                                    T = FITS.readTable1(LocalFile, 'OutClass', []);
                                else
                                    T = readtable(LocalFile, 'FileType', 'text', ...
                                        'TreatAsMissing', {'null', 'NA', 'N/A', ''});
                                end
                                if ~isempty(Args.Columns)
                                    T = T(:, Args.Columns);
                                end
                                Mat = table2array(T);
                                clear T;
                            end
                            ReadOK = true;
                            break;
                        catch ME
                            if Iattempt == 1 && IsRemote
                                fprintf('  WARNING: read failed (%s), deleting and re-downloading %s\n', ...
                                    ME.message, Bn);
                                delete(LocalFile);
                                [Status, ~] = system(sprintf( ...
                                    'wget -q -O "%s" "%s"', LocalFile, AllFiles{Idx}));
                                if Status ~= 0
                                    fprintf('  WARNING: re-download failed for %s, skipping\n', Bn);
                                    break;
                                end
                            else
                                fprintf('  WARNING: read failed for %s (%s), skipping\n', ...
                                    Bn, ME.message);
                            end
                        end
                    end
                    if ~ReadOK
                        continue;
                    end

                    % Convert coordinates to radians
                    if strcmpi(Args.CoorUnits, 'deg')
                        Mat(:, Args.ColRA)  = Mat(:, Args.ColRA)  .* (pi / 180);
                        Mat(:, Args.ColDec) = Mat(:, Args.ColDec) .* (pi / 180);
                    end

                    % Cache if file spans multiple bands
                    NbandsForFile = sum( ...
                        FileDecRanges(Idx,2) > (DecEdges(1:end-1)' - MarginDeg) & ...
                        FileDecRanges(Idx,1) < (DecEdges(2:end)' + MarginDeg));
                    if NbandsForFile > 1
                        FileCache{Idx} = Mat;
                    end
                end

                % Filter to band with margin
                InBand = Mat(:, Args.ColDec) >= (DecLoRad - MarginRad) & ...
                         Mat(:, Args.ColDec) <= (DecHiRad + MarginRad);
                Mat = Mat(InBand, :);

                AllData = [AllData; Mat]; %#ok<AGROW>

                if Args.Verbose
                    fprintf('    %d sources (filtered to band)\n', size(Mat, 1));
                end
                clear Mat;
            end

            % Evict cache entries no longer needed after this band
            if Iband < Nbands
                NextBandLoDeg = DecEdges(Iband + 1);
                NextBandHiDeg = DecEdges(end);
                for Idx = 1:Nfiles
                    if ~isempty(FileCache{Idx}) && ...
                       (FileDecRanges(Idx,2) <= (NextBandLoDeg - MarginDeg) || ...
                        FileDecRanges(Idx,1) >= (NextBandHiDeg + MarginDeg))
                        FileCache{Idx} = [];
                    end
                end
            else
                FileCache = cell(Nfiles, 1);
            end

            if isempty(AllData)
                continue;
            end

            if Args.Verbose
                fprintf('  Total sources in band: %d\n', size(AllData, 1));
                fprintf('  Building HTM cells ...\n');
            end

            % Build HTM for this Dec band
            BandNsrc = VO.prep.build_htm_catalog(AllData, ...
                'CatName', CatName, ...
                'HTM_Level', Args.HTM_Level, ...
                'ColRA', Args.ColRA, ...
                'ColDec', Args.ColDec, ...
                'ColCell', Args.ColNames, ...
                'ColUnits', Args.ColUnits, ...
                'DecRange', [DecLoRad, DecHiRad], ...
                'HTM', HTM, ...
                'LevelHTM', LevelHTM, ...
                'NfilesInHDF', Args.NcatInFile, ...
                'IndStep', Args.IndStep, ...
                'SaveInd', false, ...
                'CheckExist', false);

            % Merge per-band Nsrc into cumulative Nsrc
            Nsrc = mergeNsrc(Nsrc, BandNsrc);

            clear AllData;

            % Mark band files as completed
            for K = 1:numel(OverlapIdx)
                Idx = OverlapIdx(K);
                if ~ismember(AllFiles{Idx}, CompletedFiles)
                    CompletedFiles{end+1} = AllFiles{Idx}; %#ok<AGROW>
                end
            end
            save(CompletionLog, 'CompletedFiles');

            if Args.Verbose
                fprintf('  Band done (%.1f min)\n', toc(BandTic) / 60);
            end

            % Clean download cache: delete files not needed for next band
            if IsRemote && Args.CleanCache
                cleanDownloadCache(AllFiles, OverlapIdx, FileDecRanges, ...
                    DownloadDir, Iband, Nbands, DecEdges, MarginDeg);
            end

            % Incremental NFS copy: safe threshold = next band's lower Dec - margin
            if ~isempty(TargetDir)
                if Iband < Nbands
                    SafeDecRad = DecEdges(Iband + 1) / RAD - MarginRad;
                else
                    SafeDecRad = Inf;  % last band: copy everything
                end
                CopiedFiles = copyCompletedFiles(CopiedFiles, HdfFileMaxDec, ...
                    HdfFileNames, SafeDecRad, LocalDir, TargetDir, Args.Verbose);
            end
        end

    end  % if perfile / else band

    %------------------------------------------------------------------
    % Step 4: Copy remaining data files to TargetDir via NFS
    %------------------------------------------------------------------
    if ~isempty(TargetDir)
        CopiedFiles = copyCompletedFiles(CopiedFiles, HdfFileMaxDec, ...
            HdfFileNames, Inf, LocalDir, TargetDir, Args.Verbose);
    end

    %------------------------------------------------------------------
    % Step 5: Build index from accumulated Nsrc (no HDF5 scan needed)
    %------------------------------------------------------------------
    if Args.Verbose
        fprintf('\nBuilding HTM index ...\n');
    end

    IndFileName = sprintf('%s_htm.hdf5', CatName);
    if exist(IndFileName, 'file')
        delete(IndFileName);
    end
    % Replace any remaining NaN (unprocessed cells) with 0
    Nsrc(isnan(Nsrc(:, 2)), 2) = 0;
    celestial.htm.saveHTMIndexFast(Args.HTM_Level, IndFileName, [], {}, Nsrc);

    if Args.Verbose
        fprintf('Total sources: %d\n', sum(Nsrc(:, 2)));
    end

    % Copy index file to TargetDir
    if ~isempty(TargetDir)
        IndFullPath = fullfile(LocalDir, IndFileName);
        if isfile(IndFullPath)
            tools.os.copyFileOverNFS({IndFullPath}, TargetDir, ...
                'RemoteUser', 'euclid', 'RemoveOrigin', true);
            if Args.Verbose
                fprintf('  Copied: %s\n', IndFileName);
            end
        end
    end

    if Args.Verbose
        fprintf('Done (%.1f min total).\n', toc(TotalTic) / 60);
    end

    catch ME
        fprintf('\n*** buildHTMfromFiles ERROR ***\n');
        fprintf('Message: %s\n', ME.message);
        fprintf('Identifier: %s\n', ME.identifier);
        for IStack = 1:numel(ME.stack)
            fprintf('  %s:%d (%s)\n', ME.stack(IStack).file, ...
                ME.stack(IStack).line, ME.stack(IStack).name);
        end
        if exist('Iband', 'var')
            fprintf('Failed at band %d/%d, Dec [%.1f, %.1f]\n', ...
                Iband, Nbands, DecEdges(Iband), DecEdges(Iband + 1));
        end
        if exist('Idx', 'var') && exist('AllFiles', 'var') && Idx <= numel(AllFiles)
            fprintf('Last file: %s\n', AllFiles{Idx});
        end
        cd(OrigDir);
        rethrow(ME);
    end

    cd(OrigDir);
end


%% Local functions

function FileList = scrapeFileList(BaseURL, Pattern)
    % Scrape HTML directory listing for file URLs matching Pattern
    if ~endsWith(BaseURL, '/'), BaseURL = [BaseURL '/']; end

    TmpFile = [tempname '.html'];
    [Status, ~] = system(sprintf('wget -q -O "%s" "%s"', TmpFile, BaseURL));
    if Status ~= 0
        error('VO:prep:buildHTMfromFiles', ...
            'Failed to download directory listing from %s', BaseURL);
    end

    Html = fileread(TmpFile);
    delete(TmpFile);

    RegexPat = ['^' regexptranslate('wildcard', Pattern) '$'];
    Tokens = regexp(Html, 'href="([^"]*)"', 'tokens');

    FileList = {};
    for Itoken = 1:numel(Tokens)
        Fname = Tokens{Itoken}{1};
        [~, Bn, Ext] = fileparts(Fname);
        if ~isempty(regexp([Bn Ext], RegexPat, 'once'))
            if startsWith(Fname, 'http')
                FileList{end+1} = Fname; %#ok<AGROW>
            else
                FileList{end+1} = [BaseURL Fname]; %#ok<AGROW>
            end
        end
    end
end


function [DecLo, DecHi, RALo, RAHi] = readHdf5RangeAttrs(FilePath)
    % Read RA/Dec range attributes from an HDF5 file's '/data' dataset.
    % Attributes are expected to be in radians; returned in degrees.
    RAD = 180 / pi;
    try
        DecLo = h5readatt(FilePath, '/data', 'MinDec') * RAD;
        DecHi = h5readatt(FilePath, '/data', 'MaxDec') * RAD;
        RALo  = h5readatt(FilePath, '/data', 'MinRA')  * RAD;
        RAHi  = h5readatt(FilePath, '/data', 'MaxRA')  * RAD;
    catch
        DecLo = NaN; DecHi = NaN; RALo = NaN; RAHi = NaN;
    end
end


function cleanDownloadCache(AllFiles, OverlapIdx, FileDecRanges, ...
        DownloadDir, Iband, Nbands, DecEdges, MarginDeg)
    % Delete cached downloads not needed for the next band
    if Iband < Nbands
        NextDecHiDeg = DecEdges(Iband + 2);
        for K = 1:numel(OverlapIdx)
            Idx = OverlapIdx(K);
            % File not needed if it doesn't overlap with next band + margin
            NextBandLoDeg = DecEdges(Iband + 1);
            if FileDecRanges(Idx, 2) <= (NextBandLoDeg - MarginDeg) || ...
               FileDecRanges(Idx, 1) >= (NextDecHiDeg + MarginDeg)
                [~, Bn, Ext] = fileparts(AllFiles{Idx});
                CachedFile = fullfile(DownloadDir, [Bn Ext]);
                if exist(CachedFile, 'file')
                    delete(CachedFile);
                end
            end
        end
    else
        % Last band: clean all cached files
        for K = 1:numel(OverlapIdx)
            [~, Bn, Ext] = fileparts(AllFiles{OverlapIdx(K)});
            CachedFile = fullfile(DownloadDir, [Bn Ext]);
            if exist(CachedFile, 'file')
                delete(CachedFile);
            end
        end
    end
end


function [MaxDec, FileNames] = precomputeFileMaxDec(HTM, ListIndexHTM, ...
        CatName, NcatInFile)
    % For each grouped HDF5 file, compute the maximum MeanDec among its
    % leaf cells. A file is safe to copy when its MaxDec is below the
    % safe Dec threshold (= min Dec of remaining unprocessed files - margin).
    FileMap = containers.Map();
    for Ihtm = 1:numel(ListIndexHTM)
        IndHTM = ListIndexHTM(Ihtm);
        MeanDec = mean(HTM(IndHTM).coo(:, 2));
        [FileName, ~] = HDF5.get_file_var_from_htmid(CatName, IndHTM, NcatInFile);
        if FileMap.isKey(FileName)
            if MeanDec > FileMap(FileName)
                FileMap(FileName) = MeanDec;
            end
        else
            FileMap(FileName) = MeanDec;
        end
    end
    FileNames = FileMap.keys()';
    MaxDec = cellfun(@(K) FileMap(K), FileNames);
end


function [Nsrc, CompletedFiles] = processPerFile(Nsrc, CompletedFiles, CompletionLog, ...
        AllFiles, Nfiles, FileDecRanges, FileRARanges, ...
        IsRemote, DownloadDir, MarginDeg, RAD, HTM, LevelHTM, ...
        ListIndexHTM, CatName, LocalDir, TargetDir, ...
        HdfFileMaxDec, HdfFileNames, CopiedFiles, Args)
    % Process one sweep file at a time with margin from neighbors.
    % For each file, read it plus overlapping neighbor files, then call
    % build_htm_catalog with RA/Dec range limited to the file's footprint.
    % This avoids accumulating all files per band in memory.
    %
    % Files are sorted by Dec (south to north) so that incremental NFS
    % copy can safely remove local files: once all files below a Dec
    % threshold have been processed, grouped HDF5 files below that
    % threshold cannot receive more sources.

    MarginRad = MarginDeg / RAD;

    % Sort files by DecLo (south to north) for safe incremental copy
    [~, SortOrder] = sort(FileDecRanges(:, 1));
    AllFiles      = AllFiles(SortOrder);
    FileDecRanges = FileDecRanges(SortOrder, :);
    FileRARanges  = FileRARanges(SortOrder, :);

    % Eliminate Dec gaps between adjacent files: extend each file's
    % MaxDec to the next file's MinDec where a gap exists. This ensures
    % HTM cells whose center falls between adjacent files' actual data
    % ranges are still processed (no orphaned cells, no lost sources).
    % Files with overlapping Dec ranges are left unchanged.
    for Ifile = 1:(Nfiles - 1)
        if FileDecRanges(Ifile, 2) < FileDecRanges(Ifile + 1, 1)
            FileDecRanges(Ifile, 2) = FileDecRanges(Ifile + 1, 1);
        end
    end

    for Ifile = 1:Nfiles
        [~, Bn, Ext] = fileparts(AllFiles{Ifile});

        % File's RA/Dec footprint
        DecLoDeg = FileDecRanges(Ifile, 1);
        DecHiDeg = FileDecRanges(Ifile, 2);
        RALoDeg  = FileRARanges(Ifile, 1);
        RAHiDeg  = FileRARanges(Ifile, 2);

        DecLoRad = DecLoDeg / RAD;
        DecHiRad = DecHiDeg / RAD;
        RALoRad  = RALoDeg / RAD;
        RAHiRad  = RAHiDeg / RAD;

        if Args.Verbose
            fprintf('\n[File %d/%d] %s  RA [%.0f,%.0f] Dec [%+.0f,%+.0f]\n', ...
                Ifile, Nfiles, Bn, RALoDeg, RAHiDeg, DecLoDeg, DecHiDeg);
        end

        % Check if this file was already processed in a previous run
        if Args.Resume && ismember(AllFiles{Ifile}, CompletedFiles)
            if Args.Verbose
                fprintf('  Already processed, skipping\n');
            end
            % Fill Nsrc for skipped cells from existing HDF5 files
            Nsrc = fillNsrcFromHDF5(Nsrc, HTM, ListIndexHTM, ...
                DecLoRad, DecHiRad, CatName, Args.NcatInFile, TargetDir);
            continue;
        end

        FileTic = tic;

        % Find neighbor files: overlap with this file's footprint + margin
        NeighborIdx = find( ...
            FileDecRanges(:,2) > (DecLoDeg - MarginDeg) & ...
            FileDecRanges(:,1) < (DecHiDeg + MarginDeg) & ...
            FileRARanges(:,2)  > (RALoDeg  - MarginDeg) & ...
            FileRARanges(:,1)  < (RAHiDeg  + MarginDeg));

        % Read primary file + neighbors
        AllData = [];
        for K = 1:numel(NeighborIdx)
            Idx = NeighborIdx(K);
            [~, Nbn, Next] = fileparts(AllFiles{Idx});

            % Download if remote
            if IsRemote
                LocalFile = fullfile(DownloadDir, [Nbn Next]);
                if ~exist(LocalFile, 'file')
                    if Args.Verbose
                        fprintf('  Downloading %s ...\n', Nbn);
                    end
                    [Status, ~] = system(sprintf( ...
                        'wget -q -c -O "%s" "%s"', LocalFile, AllFiles{Idx}));
                    if Status ~= 0
                        fprintf('  WARNING: download failed for %s\n', Nbn);
                        continue;
                    end
                end
            else
                LocalFile = AllFiles{Idx};
            end

            % Read with retry on corrupt files
            [~, ~, Fext] = fileparts(LocalFile);
            IsFits = strcmpi(Fext, '.fits') || strcmpi(Fext, '.fit');
            ReadOK = false;
            for Iattempt = 1:2
                try
                    if ~isempty(Args.PostReadFun)
                        if ~IsFits
                            Mat = Args.PostReadFun(LocalFile);
                        else
                            T = FITS.readTable1(LocalFile, 'OutClass', []);
                            Mat = Args.PostReadFun(T);
                            clear T;
                        end
                    else
                        if IsFits
                            T = FITS.readTable1(LocalFile, 'OutClass', []);
                        else
                            T = readtable(LocalFile, 'FileType', 'text', ...
                                'TreatAsMissing', {'null', 'NA', 'N/A', ''});
                        end
                        if ~isempty(Args.Columns)
                            T = T(:, Args.Columns);
                        end
                        Mat = table2array(T);
                        clear T;
                    end
                    ReadOK = true;
                    break;
                catch ME
                    if Iattempt == 1 && IsRemote
                        fprintf('  WARNING: read failed (%s), re-downloading %s\n', ...
                            ME.message, Nbn);
                        delete(LocalFile);
                        [Status, ~] = system(sprintf( ...
                            'wget -q -O "%s" "%s"', LocalFile, AllFiles{Idx}));
                        if Status ~= 0
                            fprintf('  WARNING: re-download failed for %s\n', Nbn);
                            break;
                        end
                    else
                        fprintf('  WARNING: read failed for %s (%s), skipping\n', ...
                            Nbn, ME.message);
                    end
                end
            end
            if ~ReadOK
                continue;
            end

            % Convert coordinates to radians
            if strcmpi(Args.CoorUnits, 'deg')
                Mat(:, Args.ColRA)  = Mat(:, Args.ColRA)  .* (pi / 180);
                Mat(:, Args.ColDec) = Mat(:, Args.ColDec) .* (pi / 180);
            end

            % Filter to region of interest + margin
            InRegion = Mat(:, Args.ColDec) >= (DecLoRad - MarginRad) & ...
                       Mat(:, Args.ColDec) <= (DecHiRad + MarginRad) & ...
                       Mat(:, Args.ColRA)  >= (RALoRad  - MarginRad) & ...
                       Mat(:, Args.ColRA)  <= (RAHiRad  + MarginRad);
            Mat = Mat(InRegion, :);

            AllData = [AllData; Mat]; %#ok<AGROW>

            if Args.Verbose
                fprintf('  %s: %d sources (in region)\n', Nbn, size(Mat, 1));
            end
            clear Mat;
        end

        if isempty(AllData)
            if Args.Verbose
                fprintf('  No sources, skipping\n');
            end
            continue;
        end

        if Args.Verbose
            fprintf('  Total: %d sources, building HTM cells ...\n', size(AllData, 1));
        end

        % Build HTM cells for this file's RA/Dec footprint only
        FileNsrc = VO.prep.build_htm_catalog(AllData, ...
            'CatName', CatName, ...
            'HTM_Level', Args.HTM_Level, ...
            'ColRA', Args.ColRA, ...
            'ColDec', Args.ColDec, ...
            'ColCell', Args.ColNames, ...
            'ColUnits', Args.ColUnits, ...
            'DecRange', [DecLoRad, DecHiRad], ...
            'RARange',  [RALoRad,  RAHiRad], ...
            'HTM', HTM, ...
            'LevelHTM', LevelHTM, ...
            'NfilesInHDF', Args.NcatInFile, ...
            'IndStep', Args.IndStep, ...
            'SaveInd', false, ...
            'CheckExist', false);

        % Merge per-file Nsrc into cumulative Nsrc
        Nsrc = mergeNsrc(Nsrc, FileNsrc);

        clear AllData;

        if Args.Verbose
            fprintf('  Done (%.1f sec)\n', toc(FileTic));
        end

        % Incremental NFS copy: safe Dec threshold = min Dec of remaining
        % unprocessed files - margin. Grouped files below this can't
        % receive more sources from future files.
        if ~isempty(TargetDir)
            if Ifile < Nfiles
                SafeDecRad = FileDecRanges(Ifile + 1, 1) / RAD - MarginRad;
            else
                SafeDecRad = Inf;  % last file: copy everything
            end
            CopiedFiles = copyCompletedFiles(CopiedFiles, HdfFileMaxDec, ...
                HdfFileNames, SafeDecRad, LocalDir, TargetDir, Args.Verbose);
        end

        % Mark this file as completed in the log
        CompletedFiles{end+1} = AllFiles{Ifile}; %#ok<AGROW>
        save(CompletionLog, 'CompletedFiles');

        % Clean downloaded neighbor files no longer needed.
        % Keep files that overlap with any future file's footprint + margin.
        if IsRemote && Args.CleanCache
            for K = 1:numel(NeighborIdx)
                Idx = NeighborIdx(K);
                % Check if any future file needs this neighbor
                NeededLater = false;
                for Ifuture = (Ifile + 1):Nfiles
                    if FileDecRanges(Idx,2) > (FileDecRanges(Ifuture,1) - MarginDeg) && ...
                       FileDecRanges(Idx,1) < (FileDecRanges(Ifuture,2) + MarginDeg) && ...
                       FileRARanges(Idx,2)  > (FileRARanges(Ifuture,1)  - MarginDeg) && ...
                       FileRARanges(Idx,1)  < (FileRARanges(Ifuture,2)  + MarginDeg)
                        NeededLater = true;
                        break;
                    end
                end
                if ~NeededLater
                    [~, Nbn, Next] = fileparts(AllFiles{Idx});
                    CachedFile = fullfile(DownloadDir, [Nbn Next]);
                    if exist(CachedFile, 'file')
                        delete(CachedFile);
                    end
                end
            end
        end
    end
end


function CopiedFiles = copyCompletedFiles(CopiedFiles, HdfFileMaxDec, ...
        HdfFileNames, SafeDecRad, LocalDir, TargetDir, Verbose)
    % Copy grouped HDF5 files whose max cell MeanDec < SafeDecRad.
    % SafeDecRad is the lowest Dec of all remaining unprocessed files
    % minus margin. No future file can contribute sources to cells
    % below this threshold.
    for Ihdf = 1:numel(HdfFileNames)
        if CopiedFiles(Ihdf)
            continue;
        end
        if HdfFileMaxDec(Ihdf) < SafeDecRad
            FullPath = fullfile(LocalDir, HdfFileNames{Ihdf});
            if isfile(FullPath)
                tools.os.copyFileOverNFS({FullPath}, TargetDir, ...
                    'RemoteUser', 'euclid', 'RemoveOrigin', true);
                CopiedFiles(Ihdf) = true;
                if Verbose
                    fprintf('  Copied: %s\n', HdfFileNames{Ihdf});
                end
            end
        end
    end
end


function Nsrc = fillNsrcFromHDF5(Nsrc, HTM, ListIndexHTM, ...
        DecLoRad, DecHiRad, CatName, NcatInFile, TargetDir)
    % Fill Nsrc entries for cells in a Dec range by reading HDF5 metadata.
    % Tries local files first, then TargetDir (remote). Used when Resume
    % skips a band/file whose cells were written in a previous run.
    InfoCache = containers.Map();
    for Ihtm = 1:numel(ListIndexHTM)
        IndHTM  = ListIndexHTM(Ihtm);
        MeanDec = mean(HTM(IndHTM).coo(:, 2));
        if MeanDec < DecLoRad || MeanDec >= DecHiRad
            continue;
        end
        Pos = Nsrc(:, 1) == IndHTM;
        if any(Pos) && Nsrc(Pos, 2) > 0
            continue;  % already have a count
        end
        [FileName, DataName] = HDF5.get_file_var_from_htmid(CatName, IndHTM, NcatInFile);
        % Try local file first, then remote
        if ~isfile(FileName) && ~isempty(TargetDir)
            FileName = fullfile(TargetDir, FileName);
        end
        if ~isfile(FileName)
            continue;
        end
        if ~InfoCache.isKey(FileName)
            try
                InfoCache(FileName) = h5info(FileName);
            catch
                continue;
            end
        end
        Info = InfoCache(FileName);
        Idx = strcmp({Info.Datasets.Name}, DataName);
        if any(Idx)
            DsSize = Info.Datasets(Idx).Dataspace.Size;
            if any(Pos)
                Nsrc(Pos, 2) = DsSize(end);
            end
        end
    end
end


function Nsrc = mergeNsrc(Nsrc, NewNsrc)
    % Merge Nsrc from a build_htm_catalog call into the cumulative Nsrc.
    % Both are [IndHTM, Nsrc] matrices. NewNsrc has NaN for cells outside
    % the processed range; 0 for empty cells; >0 for populated cells.
    % We skip NaN entries (not processed) but accept 0 (confirmed empty).
    if isempty(NewNsrc)
        return;
    end
    for K = 1:size(NewNsrc, 1)
        if isnan(NewNsrc(K, 2))
            continue;  % cell was outside the processed range
        end
        Pos = Nsrc(:, 1) == NewNsrc(K, 1);
        if any(Pos)
            if isnan(Nsrc(Pos, 2))
                Nsrc(Pos, 2) = NewNsrc(K, 2);
            else
                Nsrc(Pos, 2) = max(Nsrc(Pos, 2), NewNsrc(K, 2));
            end
        end
    end
end
