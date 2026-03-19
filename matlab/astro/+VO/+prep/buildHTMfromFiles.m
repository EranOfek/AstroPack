function buildHTMfromFiles(Args)
% Build an HTM catalog from local or remote data files
% Package: VO.prep
% Description: Processes a collection of FITS or text files into an
%              HTM-structured HDF5 catalog. Files are processed by
%              declination band to limit memory usage. Supports
%              downloading from remote URLs with caching and resume.
%              Uses celestial.htm.saveHTMIndexFast for index building.
%
%              The function works in four steps:
%              1. Scrape file list from URL or local directory
%              2. Process files by Dec band: download, select columns,
%                 build HTM cells using VO.prep.build_htm_catalog
%              3. Build index with celestial.htm.saveHTMIndexFast
%              4. Copy HDF5 files to TargetDir (if specified)
%
%              Files are downloaded one at a time and cached in
%              DownloadDir. By default, files not needed for the
%              next band are deleted (CleanCache). When Resume is
%              true, CleanCache is automatically disabled to keep
%              cached files for re-runs.
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
%            'DecBandWidth' - Dec band width [deg] for processing. Default: 5.
%            'Resume'       - Skip existing HTM cells. Default: true.
%                             Also disables CleanCache automatically.
%            'CleanCache'   - Delete cached downloads after each Dec
%                             band. Default: true (but false when Resume).
%            'Verbose'      - Print progress. Default: true.
%
% Output : null
% Author : Dana Kovaleva (Mar 2026)
% Example:
%   % DECaLS DR10 example:
%   VO.prep.buildHTMfromFiles(...
%       'SourceURL', 'https://portal.nersc.gov/cfs/cosmo/data/legacysurvey/dr10/south/sweep/10.1/', ...
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
        Args.DecBandWidth double = 5
        Args.Resume       logical = true
        Args.StartBand    double = 1
        Args.CleanCache   logical = true
        Args.Verbose      logical = true
    end

    LocalDir    = char(Args.LocalDir);
    TargetDir   = char(Args.TargetDir);
    DownloadDir = char(Args.DownloadDir);
    CatName     = char(Args.CatName);
    RAD         = 180 / pi;

    % Disable cache cleanup when resuming — cached files avoid re-downloads
    if Args.Resume
        Args.CleanCache = false;
    end

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
        d = dir(fullfile(char(Args.SourceDir), char(Args.FilePattern)));
        AllFiles = fullfile({d.folder}, {d.name});
        IsRemote = false;
    else
        error('VO:prep:buildHTMfromFiles', 'Specify SourceURL or SourceDir');
    end

    Nfiles = numel(AllFiles);
    if Args.Verbose
        fprintf('Found %d files\n', Nfiles);
    end

    %------------------------------------------------------------------
    % Step 2: Determine Dec range for each file
    %------------------------------------------------------------------
    FileDecRanges = nan(Nfiles, 2);
    for i = 1:Nfiles
        [~, bn, ~] = fileparts(AllFiles{i});
        [lo, hi] = parseSweepDecRange(bn);
        FileDecRanges(i, :) = [lo, hi];
    end

    if any(isnan(FileDecRanges(:)))
        warning('VO:prep:buildHTMfromFiles', ...
            'Could not parse Dec ranges from filenames. Assuming full sky per file.');
        FileDecRanges(:, 1) = -90;
        FileDecRanges(:, 2) = 90;
    end

    %------------------------------------------------------------------
    % Step 3: Build HTM by Dec bands
    %------------------------------------------------------------------
    [HTM, LevelHTM] = celestial.htm.htm_build(Args.HTM_Level);
    RadiusHTM = (sqrt(2) * 90 / (2^(Args.HTM_Level - 1))) / RAD;
    MarginDeg = RadiusHTM * RAD * 1.5;

    ListIndexHTM = LevelHTM(Args.HTM_Level).ptr;

    DecEdges = -90 : Args.DecBandWidth : 90;
    Nbands = numel(DecEdges) - 1;

    OrigDir = pwd;
    cd(LocalDir);

    TotalTic = tic;

    % File data cache: avoid re-reading files that span multiple bands.
    % FileCache{idx} holds the full numeric matrix (with RA/Dec in radians)
    % for file idx. Cleared when the file is no longer needed.
    FileCache = cell(Nfiles, 1);

    try
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

            % When resuming, check if all HTM cells for this band already
            % exist in HDF5 — if so, skip reading source files entirely
            if Args.Resume
                BandComplete = checkBandComplete(CatName, HTM, ...
                    ListIndexHTM, DecLoRad, DecHiRad, Args.NcatInFile);
                if BandComplete
                    if Args.Verbose
                        fprintf('  All HTM cells exist, skipping band\n');
                    end
                    continue;
                end
            end

            BandTic = tic;

            % Load and accumulate data for this band
            AllData = [];
            MarginRad = MarginDeg / RAD;

            for k = 1:numel(OverlapIdx)
                idx = OverlapIdx(k);
                [~, bn, ext] = fileparts(AllFiles{idx});

                % Use cached data if available
                if ~isempty(FileCache{idx})
                    Mat = FileCache{idx};
                    if Args.Verbose
                        fprintf('  [%d/%d] Using cached %s\n', ...
                            k, numel(OverlapIdx), bn);
                    end
                else
                    % Download if remote
                    if IsRemote
                        localFile = fullfile(DownloadDir, [bn ext]);
                        if ~exist(localFile, 'file')
                            if Args.Verbose
                                fprintf('  [%d/%d] Downloading %s ...\n', ...
                                    k, numel(OverlapIdx), bn);
                            end
                            [status, ~] = system(sprintf( ...
                                'wget -q -c -O "%s" "%s"', localFile, AllFiles{idx}));
                            if status ~= 0
                                fprintf('  WARNING: download failed for %s\n', bn);
                                continue;
                            end
                        else
                            if Args.Verbose
                                fprintf('  [%d/%d] Using downloaded %s\n', ...
                                    k, numel(OverlapIdx), bn);
                            end
                        end
                    else
                        localFile = AllFiles{idx};
                    end

                    % Read table (FITS or text)
                    if Args.Verbose
                        fprintf('  Reading %s ...\n', bn);
                    end
                    [~, ~, fext] = fileparts(localFile);
                    IsText = ismember(lower(fext), {'.txt', '.csv', '.tsv', '.dat'});

                    % Select columns / transform (with retry on corrupt files)
                    ReadOK = false;
                    for Iattempt = 1:2
                        try
                            if ~isempty(Args.PostReadFun)
                                % For text files, pass filename to PostReadFun
                                % (avoids slow readtable on large files).
                                % For FITS, pass table as before.
                                if IsText
                                    Mat = Args.PostReadFun(localFile);
                                else
                                    T = FITS.readTable1(localFile, 'OutClass', []);
                                    Mat = Args.PostReadFun(T);
                                    clear T;
                                end
                            else
                                if IsText
                                    T = readtable(localFile, 'FileType', 'text', ...
                                        'TreatAsMissing', {'null', 'NA', 'N/A', ''});
                                else
                                    T = FITS.readTable1(localFile, 'OutClass', []);
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
                                    ME.message, bn);
                                delete(localFile);
                                [status, ~] = system(sprintf( ...
                                    'wget -q -O "%s" "%s"', localFile, AllFiles{idx}));
                                if status ~= 0
                                    fprintf('  WARNING: re-download failed for %s, skipping\n', bn);
                                    break;
                                end
                            else
                                fprintf('  WARNING: read failed for %s (%s), skipping\n', ...
                                    bn, ME.message);
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
                        FileDecRanges(idx,2) > (DecEdges(1:end-1)' - MarginDeg) & ...
                        FileDecRanges(idx,1) < (DecEdges(2:end)' + MarginDeg));
                    if NbandsForFile > 1
                        FileCache{idx} = Mat;
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
                for idx = 1:Nfiles
                    if ~isempty(FileCache{idx}) && ...
                       (FileDecRanges(idx,2) <= (NextBandLoDeg - MarginDeg) || ...
                        FileDecRanges(idx,1) >= (NextBandHiDeg + MarginDeg))
                        FileCache{idx} = [];
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
            VO.prep.build_htm_catalog(AllData, ...
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
                'CheckExist', Args.Resume);

            clear AllData;

            if Args.Verbose
                fprintf('  Band done (%.1f min)\n', toc(BandTic) / 60);
            end

            % Clean download cache: delete files not needed for next band
            if IsRemote && Args.CleanCache
                cleanDownloadCache(AllFiles, OverlapIdx, FileDecRanges, ...
                    DownloadDir, Iband, Nbands, DecEdges, MarginDeg);
            end
        end

        %------------------------------------------------------------------
        % Step 4: Build index
        %------------------------------------------------------------------
        if Args.Verbose
            fprintf('\nBuilding HTM index ...\n');
        end

        IndFileName = sprintf('%s_htm.hdf5', CatName);
        if exist(IndFileName, 'file')
            delete(IndFileName);
        end
        Nsrc = HDF5.get_nsrc(CatName);
        celestial.htm.saveHTMIndexFast(Args.HTM_Level, IndFileName, [], {}, Nsrc);

        HDF5.save_cat_colcell(CatName, Args.ColNames, Args.ColUnits);

        %------------------------------------------------------------------
        % Step 5: Copy to TargetDir via NFS
        %------------------------------------------------------------------
        if ~isempty(TargetDir)
            if Args.Verbose
                fprintf('\nCopying HDF5 files to %s ...\n', TargetDir);
            end

            % Copy all HDF5 data files
            HdfFiles = dir(fullfile(LocalDir, [CatName '*.hdf5']));
            for iFile = 1:numel(HdfFiles)
                FullPath = fullfile(LocalDir, HdfFiles(iFile).name);
                tools.os.copyFileOverNFS({FullPath}, TargetDir, ...
                    'RemoteUser', 'euclid', 'RemoveOrigin', true);
                if Args.Verbose
                    fprintf('  Copied: %s\n', HdfFiles(iFile).name);
                end
            end

            % Copy ColCell .mat file
            ColCellFile = fullfile(LocalDir, sprintf('%s_htmColCell.mat', CatName));
            if isfile(ColCellFile)
                tools.os.copyFileOverNFS({ColCellFile}, TargetDir, ...
                    'RemoteUser', 'euclid', 'RemoveOrigin', true);
                if Args.Verbose
                    fprintf('  Copied: %s_htmColCell.mat\n', CatName);
                end
            end
        end

        if Args.Verbose
            fprintf('Done (%.1f min total).\n', toc(TotalTic) / 60);
        end

    catch ME
        cd(OrigDir);
        rethrow(ME);
    end

    cd(OrigDir);
end


%% Local functions

function FileList = scrapeFileList(BaseURL, Pattern)
    % Scrape HTML directory listing for file URLs matching Pattern
    if ~endsWith(BaseURL, '/'), BaseURL = [BaseURL '/']; end

    tmpFile = [tempname '.html'];
    [status, ~] = system(sprintf('wget -q -O "%s" "%s"', tmpFile, BaseURL));
    if status ~= 0
        error('VO:prep:buildHTMfromFiles', ...
            'Failed to download directory listing from %s', BaseURL);
    end

    html = fileread(tmpFile);
    delete(tmpFile);

    RegexPat = ['^' regexptranslate('wildcard', Pattern) '$'];
    tokens = regexp(html, 'href="([^"]*)"', 'tokens');

    FileList = {};
    for i = 1:numel(tokens)
        fname = tokens{i}{1};
        [~, bn, ext] = fileparts(fname);
        if ~isempty(regexp([bn ext], RegexPat, 'once'))
            if startsWith(fname, 'http')
                FileList{end+1} = fname; %#ok<AGROW>
            else
                FileList{end+1} = [BaseURL fname]; %#ok<AGROW>
            end
        end
    end
end


function [DecLo, DecHi] = parseSweepDecRange(BaseName)
    % Parse Dec range from sweep-style filename
    % Format: sweep-{3digitRA}{p|m}{2-3digitDec}-{3digitRA}{p|m}{2-3digitDec}
    tokens = regexp(BaseName, ...
        'sweep-\d{3}([pm]\d{2,3})-\d{3}([pm]\d{2,3})', 'tokens');
    if isempty(tokens)
        DecLo = NaN;
        DecHi = NaN;
        return;
    end
    DecLo = parseSweepCoord(tokens{1}{1});
    DecHi = parseSweepCoord(tokens{1}{2});
end


function Val = parseSweepCoord(Str)
    % Parse coordinate string like 'p035' -> 35 or 'm010' -> -10
    if Str(1) == 'p'
        Val = str2double(Str(2:end));
    else
        Val = -str2double(Str(2:end));
    end
end


function cleanDownloadCache(AllFiles, OverlapIdx, FileDecRanges, ...
        DownloadDir, Iband, Nbands, DecEdges, MarginDeg)
    % Delete cached downloads not needed for the next band
    if Iband < Nbands
        NextDecHiDeg = DecEdges(Iband + 2);
        for k = 1:numel(OverlapIdx)
            idx = OverlapIdx(k);
            % File not needed if it doesn't overlap with next band + margin
            NextBandLoDeg = DecEdges(Iband + 1);
            if FileDecRanges(idx, 2) <= (NextBandLoDeg - MarginDeg) || ...
               FileDecRanges(idx, 1) >= (NextDecHiDeg + MarginDeg)
                [~, bn, ext] = fileparts(AllFiles{idx});
                cachedFile = fullfile(DownloadDir, [bn ext]);
                if exist(cachedFile, 'file')
                    delete(cachedFile);
                end
            end
        end
    else
        % Last band: clean all cached files
        for k = 1:numel(OverlapIdx)
            [~, bn, ext] = fileparts(AllFiles{OverlapIdx(k)});
            cachedFile = fullfile(DownloadDir, [bn ext]);
            if exist(cachedFile, 'file')
                delete(cachedFile);
            end
        end
    end
end


function Complete = checkBandComplete(CatName, HTM, ListIndexHTM, ...
        DecLoRad, DecHiRad, NcatInFile)
    % Check if all HTM cells for a Dec band already exist in HDF5.
    % Returns true only if every cell whose mean Dec falls in [DecLo,DecHi)
    % either has Nsrc==0 (empty) or already exists as an HDF5 dataset.
    Complete = true;
    FileCache = containers.Map();  % cache h5info per HDF5 file
    for i = 1:numel(ListIndexHTM)
        IndHTM  = ListIndexHTM(i);
        MeanDec = mean(HTM(IndHTM).coo(:, 2));
        if MeanDec < DecLoRad || MeanDec >= DecHiRad
            continue;
        end
        [FileName, DataName] = HDF5.get_file_var_from_htmid(CatName, IndHTM, NcatInFile);
        if ~isfile(FileName)
            Complete = false;
            return;
        end
        if ~FileCache.isKey(FileName)
            try
                FileCache(FileName) = h5info(FileName);
            catch
                Complete = false;
                return;
            end
        end
        Info = FileCache(FileName);
        if ~any(strcmp({Info.Datasets.Name}, DataName))
            Complete = false;
            return;
        end
    end
end
