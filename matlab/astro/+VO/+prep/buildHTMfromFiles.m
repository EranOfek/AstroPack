function buildHTMfromFiles(Args)
% Build an HTM catalog from local or remote FITS files
% Package: VO.prep
% Description: Processes a collection of FITS files into an
%              HTM-structured HDF5 catalog. Files are processed by
%              declination band to limit memory usage. Supports
%              downloading from remote URLs with caching and resume.
%              Uses celestial.htm.saveHTMIndexFast for index building.
%
%              The function works in three steps:
%              1. Scrape file list from URL or local directory
%              2. Process files by Dec band: download, select columns,
%                 build HTM cells using VO.prep.build_htm_catalog
%              3. Build index with celestial.htm.saveHTMIndexFast
%
%              Files are downloaded one at a time and cached in
%              DownloadDir. After each Dec band, files not needed
%              for the next band are deleted to limit disk usage.
%
% Input  : * ...,key,val,...
%            --- Source files ---
%            'SourceURL'    - URL of directory listing to scrape.
%            'SourceDir'    - Local directory with FITS files.
%                             One of SourceURL or SourceDir is required.
%            'FilePattern'  - Glob pattern for files. Default: '*.fits'
%            --- Column selection ---
%            'Columns'      - Cell array of source column names to keep.
%                             Default: {} (all columns).
%            'ColNames'     - Cell array of output column names for HTM.
%                             Default: {} (use source names).
%            'ColUnits'     - Cell array of output column units.
%            'ColRA'        - RA column index in output matrix. Default: 1.
%            'ColDec'       - Dec column index in output matrix. Default: 2.
%            'CoorUnits'    - Input coordinate units 'deg'|'rad'. Default: 'deg'
%            'PostReadFun'  - Function handle: Mat = fun(Table).
%                             If provided, overrides Columns selection.
%                             Must return a numeric matrix with RA in ColRA
%                             and Dec in ColDec (in original CoorUnits).
%            --- HTM ---
%            'CatName'      - Output catalog base name.
%            'HTM_Level'    - HTM level. Default: 9.
%            'NcatInFile'   - HTM cells per HDF5 file. Default: 100.
%            'IndStep'      - Index step for HDF5.save_cat. Default: 30.
%            --- Directories ---
%            'OutputDir'    - Directory for HDF5 output files.
%                             Default: pwd. Can be a remote mount (e.g. /euclid/...).
%            'DownloadDir'  - Temp directory for downloaded files.
%                             Default: tempdir.
%            --- Processing ---
%            'DecBandWidth' - Dec band width [deg] for processing. Default: 5.
%            'Resume'       - Skip existing HTM cells. Default: true.
%            'Verbose'      - Print progress. Default: true.
%
% Output : null
% Author : Dana + Claude (Mar 2026)
% Example:
%   % DECaLS DR10 example:
%   VO.prep.buildHTMfromFiles(...
%       'SourceURL', 'https://portal.nersc.gov/cfs/cosmo/data/legacysurvey/dr10/south/sweep/10.1/', ...
%       'PostReadFun', @decalsPostRead, ...
%       'ColNames', {'RA','Dec','RA_IVAR','DEC_IVAR','Type', ...
%           'Flux_g','Flux_r','Flux_i','Flux_z', ...
%           'Flux_W1','Flux_W2','Flux_W3','Flux_W4', ...
%           'FluxIvar_g','FluxIvar_r','FluxIvar_i','FluxIvar_z', ...
%           'FluxIvar_W1','FluxIvar_W2','FluxIvar_W3','FluxIvar_W4', ...
%           'MaskBits','ShapeR'}, ...
%       'CatName', 'DECaLS10', 'HTM_Level', 9, ...
%       'OutputDir', '/euclid/catsHTM/NewCats/DECaLS10/', ...
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
        Args.OutputDir    string = string(pwd)
        Args.DownloadDir  string = string(tempdir)
        Args.DecBandWidth double = 5
        Args.Resume       logical = true
        Args.Verbose      logical = true
    end

    OutputDir   = char(Args.OutputDir);
    DownloadDir = char(Args.DownloadDir);
    CatName     = char(Args.CatName);
    RAD         = 180 / pi;

    if ~exist(OutputDir, 'dir'),    mkdir(OutputDir); end
    if ~exist(DownloadDir, 'dir'), mkdir(DownloadDir); end

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

    DecEdges = -90 : Args.DecBandWidth : 90;
    Nbands = numel(DecEdges) - 1;

    OrigDir = pwd;
    cd(OutputDir);

    TotalTic = tic;

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

            BandTic = tic;

            % Load and accumulate data for this band
            AllData = [];
            MarginRad = MarginDeg / RAD;

            for k = 1:numel(OverlapIdx)
                idx = OverlapIdx(k);
                [~, bn, ext] = fileparts(AllFiles{idx});

                % Download if remote (with caching)
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
                            fprintf('  [%d/%d] Using cached %s\n', ...
                                k, numel(OverlapIdx), bn);
                        end
                    end
                else
                    localFile = AllFiles{idx};
                end

                % Read FITS table
                if Args.Verbose
                    fprintf('  Reading %s ...\n', bn);
                end
                T = FITS.readTable1(localFile, 'OutClass', []);

                % Select columns / transform
                if ~isempty(Args.PostReadFun)
                    Mat = Args.PostReadFun(T);
                else
                    if ~isempty(Args.Columns)
                        T = T(:, Args.Columns);
                    end
                    Mat = table2array(T);
                end
                clear T;

                % Convert coordinates to radians
                if strcmpi(Args.CoorUnits, 'deg')
                    Mat(:, Args.ColRA)  = Mat(:, Args.ColRA)  .* (pi / 180);
                    Mat(:, Args.ColDec) = Mat(:, Args.ColDec) .* (pi / 180);
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
            if IsRemote
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
