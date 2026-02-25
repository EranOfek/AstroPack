function Result = downloadACwithTAP(BaseQuery, Args)
% Download large TAP query results by splitting the sky into chunks
% Package: VO.prep
% Description: Splits a large ADQL query into spatial chunks (declination
%              bands, HealPix tiles, etc.) and downloads each chunk as a
%              separate file using STILTS or HTTP. Supports resume,
%              retries, sync/async modes, and multiple output formats.
%              After downloading, chunks can be merged into a single file
%              or loaded as an AstroCatalog array.
% Input  : - Base ADQL query string containing a WHERE clause.
%            The spatial constraint for each chunk is appended automatically.
%          * ...,key,val,...
%            --- Subdivision ---
%            'Method'    - Sky subdivision method:
%                          'DecBands' - declination bands (default)
%                          'HealPix'  - HealPix tiles (placeholder)
%            'DecEdges'  - Vector of declination band edges [deg].
%                          Used when Method='DecBands'.
%                          Default: -90:5:90 (36 bands of 5 deg).
%            'DecColumn' - Name of the Dec column in the ADQL table.
%                          Default: 'dec'.
%            'RAColumn'  - Name of the RA column in the ADQL table.
%                          Default: 'ra'.
%            'HealPixNside' - HealPix Nside parameter (placeholder).
%                          Default: 8.
%            'ChunkIndices' - Vector of chunk indices to download.
%                          Default: [] (all chunks). Use this to download
%                          a specific subset, e.g. [5 10 15] downloads
%                          only chunks 5, 10, 15.
%            --- Output ---
%            'OutputDir'  - Directory for output files. Default: pwd.
%            'FilePrefix' - Prefix for output filenames.
%                           Default: 'tap_chunk'.
%            'Ofmt'       - Output format: 'fits' | 'csv' | 'tsv' | 'votable'.
%                           Default: 'fits'.
%            --- TAP connection ---
%            'TapUrl'     - TAP service URL.
%                           Default: Gaia ESA TAP.
%            'SyncMode'   - Query execution mode:
%                           'async' - asynchronous UWS (default, for large queries)
%                           'sync'  - synchronous (for small queries)
%            'Backend'    - Download backend:
%                           'stilts' - use STILTS jar (default)
%                           'http'   - use VO.TopCat.queryHttp
%            'StiltsJar'  - Path to stilts.jar.
%                           Default: from VO.TopCat.getStiltsJarPath().
%            'TimeoutSec' - Timeout per chunk query [sec]. Default: 3600.
%            'MaxRetries' - Number of retries per chunk. Default: 3.
%            --- Post-processing ---
%            'Merge'      - Merge downloaded chunks after completion:
%                           'none'  - keep separate files (default)
%                           'fits'  - merge into single FITS file
%                           'table' - load and concatenate into MATLAB table
%            'MergeFile'  - Output filename for merged result.
%                           Default: '<FilePrefix>_merged.<Ofmt>'.
%            'DeleteChunks' - Delete individual chunk files after merge.
%                           Default: false.
%            'DryRun'     - If true, list all chunks and return without
%                           downloading. Use to inspect ChunkIndices.
%                           Default: false.
%            'Verbose'    - Print progress. Default: true.
% Output : - Struct with fields:
%            .Files    - Cell array of output file paths.
%            .Status   - Vector of status codes (0=success, NaN=not attempted).
%            .Nchunks  - Number of chunks.
%            .Chunks   - Nx3 cell: {Label, ADQLConstraint, FileSuffix}.
%            .Queries  - Cell array of full ADQL queries per chunk.
%            .MergedFile - Path to merged file (if Merge ~= 'none').
% Author : Dana Kovaleva (Feb 2026)
% Example:
%   Q = ['SELECT * FROM gaiaedr3.gaia_source ', ...
%        'WHERE parallax > 1 ', ...
%        'AND parallax_over_error > 5 ', ...
%        'AND parallax_error < 2 ', ...
%        'AND phot_g_mean_mag IS NOT NULL'];
%   % List chunks without downloading (DryRun):
%   Result = VO.prep.downloadACwithTAP(Q, 'DryRun', true);
%   % Download all chunks:
%   Result = VO.prep.downloadACwithTAP(Q, ...
%       'OutputDir', '/data/gaia_plx', 'FilePrefix', 'gaia_plx');
%   % Download only chunks 5-10:
%   Result = VO.prep.downloadACwithTAP(Q, ...
%       'OutputDir', '/data/gaia_plx', 'ChunkIndices', 5:10);
%   % Sync mode, CSV output:
%   Result = VO.prep.downloadACwithTAP(Q, 'SyncMode', 'sync', 'Ofmt', 'csv');

    arguments
        BaseQuery
        % Subdivision
        Args.Method         = 'DecBands'
        Args.DecEdges       = -90:5:90
        Args.DecColumn      = 'dec'
        Args.RAColumn       = 'ra'
        Args.HealPixNside   = 8
        Args.ChunkIndices   = []
        Args.DryRun logical = false
        % Output
        Args.OutputDir      = pwd
        Args.FilePrefix     = 'tap_chunk'
        Args.Ofmt           = 'fits'
        % TAP connection
        Args.TapUrl         = 'https://gea.esac.esa.int/tap-server/tap'
        Args.SyncMode       = 'async'
        Args.Backend        = 'stilts'
        Args.StiltsJar      = VO.TopCat.getStiltsJarPath()
        Args.TimeoutSec     = 3600
        Args.MaxRetries     = 3
        % Post-processing
        Args.Merge          = 'none'
        Args.MergeFile      = ''
        Args.DeleteChunks   = false
        Args.Verbose        = true
    end

    % Strip trailing semicolons and whitespace from the base query
    BaseQuery = strtrim(char(BaseQuery));
    if endsWith(BaseQuery, ';'), BaseQuery = BaseQuery(1:end-1); end

    % Normalize TAP URL
    TapUrl = char(Args.TapUrl);
    if endsWith(TapUrl, '/'), TapUrl = TapUrl(1:end-1); end

    Ofmt = lower(char(Args.Ofmt));

    %--------------------------------------------------------------
    % Generate chunk definitions: {label, adqlConstraint, fileSuffix}
    %--------------------------------------------------------------
    switch lower(Args.Method)
        case 'decbands'
            Chunks = generateDecBandChunks(Args.DecEdges, Args.DecColumn);

        case 'healpix'
            Chunks = generateHealPixChunks(Args.HealPixNside, ...
                         Args.RAColumn, Args.DecColumn);

        otherwise
            error('VO:prep:downloadACwithTAP', ...
                'Unknown Method ''%s''. Supported: DecBands, HealPix.', ...
                Args.Method);
    end

    Nchunks = size(Chunks, 1);

    %--------------------------------------------------------------
    % DryRun: list chunks and return without downloading
    %--------------------------------------------------------------
    if Args.DryRun
        fprintf('Chunk list (%d chunks, Method=%s):\n', Nchunks, Args.Method);
        fprintf('  %5s  %-30s  %s\n', 'Index', 'Label', 'Output file');
        fprintf('  %5s  %-30s  %s\n', '-----', '-----', '-----------');
        for Ic = 1:Nchunks
            OutFile = fullfile(Args.OutputDir, ...
                sprintf('%s_%s.%s', Args.FilePrefix, Chunks{Ic, 3}, Ofmt));
            Exists = exist(OutFile, 'file') == 2;
            if Exists
                Mark = ' [exists]';
            else
                Mark = '';
            end
            fprintf('  %5d  %-30s  %s%s\n', Ic, Chunks{Ic, 1}, OutFile, Mark);
        end
        Result.Files   = cell(Nchunks, 1);
        Result.Status  = nan(Nchunks, 1);
        Result.Nchunks = Nchunks;
        Result.Queries = cell(Nchunks, 1);
        Result.Chunks  = Chunks;
        Result.MergedFile = '';
        for Ic = 1:Nchunks
            Result.Queries{Ic} = sprintf('%s AND %s', BaseQuery, Chunks{Ic, 2});
            Result.Files{Ic}   = fullfile(Args.OutputDir, ...
                sprintf('%s_%s.%s', Args.FilePrefix, Chunks{Ic, 3}, Ofmt));
        end
        return;
    end

    % Select subset of chunks if requested
    if ~isempty(Args.ChunkIndices)
        ValidIdx = Args.ChunkIndices(Args.ChunkIndices >= 1 & ...
                                     Args.ChunkIndices <= Nchunks);
        if isempty(ValidIdx)
            error('VO:prep:downloadACwithTAP', ...
                'No valid ChunkIndices in range [1, %d].', Nchunks);
        end
    else
        ValidIdx = 1:Nchunks;
    end

    if ~exist(Args.OutputDir, 'dir')
        mkdir(Args.OutputDir);
    end

    %--------------------------------------------------------------
    % Build queries, filenames
    %--------------------------------------------------------------
    Result.Files   = cell(Nchunks, 1);
    Result.Status  = nan(Nchunks, 1);  % NaN = not attempted
    Result.Nchunks = Nchunks;
    Result.Chunks  = Chunks;
    Result.Queries = cell(Nchunks, 1);
    Result.MergedFile = '';

    for Ic = 1:Nchunks
        Constraint = Chunks{Ic, 2};
        Suffix     = Chunks{Ic, 3};
        Result.Queries{Ic} = sprintf('%s AND %s', BaseQuery, Constraint);
        Result.Files{Ic}   = fullfile(Args.OutputDir, ...
            sprintf('%s_%s.%s', Args.FilePrefix, Suffix, Ofmt));
    end

    %--------------------------------------------------------------
    % Download loop
    %--------------------------------------------------------------
    TotalTic = tic;

    for Ii = 1:numel(ValidIdx)
        Ic = ValidIdx(Ii);
        Label   = Chunks{Ic, 1};
        Query   = Result.Queries{Ic};
        OutFile = Result.Files{Ic};

        % Skip if file already exists (resume support)
        if exist(OutFile, 'file') == 2
            Info = dir(OutFile);
            if Info.bytes > 0
                Result.Status(Ic) = 0;
                if Args.Verbose
                    fprintf('[%d/%d] SKIP %s — exists (%.1f MB)\n', ...
                        Ii, numel(ValidIdx), Label, Info.bytes/1e6);
                end
                continue;
            end
        end

        % Execute query with retries
        Result.Status(Ic) = 1;  % assume failure
        for Itry = 1:Args.MaxRetries
            if Args.Verbose
                fprintf('[%d/%d] %s attempt %d/%d ...', ...
                    Ii, numel(ValidIdx), Label, Itry, Args.MaxRetries);
            end
            ChunkTic = tic;

            try
                switch lower(Args.Backend)
                    case 'stilts'
                        [Status, OutTxt] = executeStilts(Query, OutFile, ...
                            TapUrl, Ofmt, Args.SyncMode, ...
                            Args.StiltsJar, Args.TimeoutSec);
                    case 'http'
                        Status = executeHttp(Query, OutFile, ...
                            TapUrl, Ofmt, Args.TimeoutSec);
                        OutTxt = '';
                    otherwise
                        error('Unknown Backend: %s', Args.Backend);
                end
            catch ME
                Status = -1;
                OutTxt = ME.message;
            end

            Elapsed = toc(ChunkTic);

            if Status == 0 && exist(OutFile, 'file') == 2
                Info = dir(OutFile);
                if Args.Verbose
                    fprintf(' OK (%.1f s, %.1f MB)\n', Elapsed, Info.bytes/1e6);
                end
                Result.Status(Ic) = 0;
                break;
            else
                if Args.Verbose
                    fprintf(' FAILED (status=%d, %.1f s)\n', Status, Elapsed);
                    if ~isempty(OutTxt)
                        fprintf('  %s\n', strtrim(OutTxt));
                    end
                end
                if exist(OutFile, 'file') == 2
                    delete(OutFile);
                end
                if Itry < Args.MaxRetries
                    pause(5 * Itry);
                end
            end
        end

        if Result.Status(Ic) ~= 0
            fprintf('  WARNING: chunk %d (%s) failed after %d retries\n', ...
                Ic, Label, Args.MaxRetries);
        end
    end

    %--------------------------------------------------------------
    % Post-processing: merge
    %--------------------------------------------------------------
    if ~strcmpi(Args.Merge, 'none')
        SuccessIdx = find(Result.Status == 0);
        SuccessFiles = Result.Files(SuccessIdx);

        if isempty(SuccessFiles)
            fprintf('No successful downloads to merge.\n');
        else
            if isempty(Args.MergeFile)
                Args.MergeFile = fullfile(Args.OutputDir, ...
                    sprintf('%s_merged.%s', Args.FilePrefix, Ofmt));
            end

            if Args.Verbose
                fprintf('Merging %d chunks into %s ...\n', ...
                    numel(SuccessFiles), Args.MergeFile);
            end

            switch lower(Args.Merge)
                case 'fits'
                    mergeChunksFits(SuccessFiles, Args.MergeFile, Ofmt);
                case 'table'
                    mergeChunksFits(SuccessFiles, Args.MergeFile, Ofmt);
                otherwise
                    error('Unknown Merge option: %s', Args.Merge);
            end

            Result.MergedFile = Args.MergeFile;

            if Args.DeleteChunks
                for If = 1:numel(SuccessFiles)
                    delete(SuccessFiles{If});
                end
                if Args.Verbose
                    fprintf('Deleted %d chunk files.\n', numel(SuccessFiles));
                end
            end
        end
    end

    %--------------------------------------------------------------
    % Summary
    %--------------------------------------------------------------
    Nattempted = sum(~isnan(Result.Status));
    Nsuccess   = sum(Result.Status == 0);
    if Args.Verbose
        fprintf('\nDone: %d/%d chunks downloaded (%.1f min total)\n', ...
            Nsuccess, Nattempted, toc(TotalTic)/60);
        if Nattempted < Nchunks
            fprintf('  (%d chunks were not attempted)\n', Nchunks - Nattempted);
        end
        FailIdx = find(Result.Status == 1);
        if ~isempty(FailIdx)
            fprintf('Failed chunks:\n');
            for If = 1:numel(FailIdx)
                fprintf('  %s\n', Chunks{FailIdx(If), 1});
            end
        end
    end

end


%==========================================================================
% CHUNK GENERATION METHODS
%==========================================================================

function Chunks = generateDecBandChunks(DecEdges, DecColumn)
% Generate chunk definitions for declination bands.
% Output: Nx3 cell array {Label, ADQLConstraint, FileSuffix}

    DecEdges = DecEdges(:)';
    Nbands   = numel(DecEdges) - 1;
    Chunks   = cell(Nbands, 3);

    for Ib = 1:Nbands
        DecLo = DecEdges(Ib);
        DecHi = DecEdges(Ib + 1);

        Chunks{Ib, 1} = sprintf('Dec [%+.1f, %+.1f]', DecLo, DecHi);
        Chunks{Ib, 2} = sprintf('%s BETWEEN %.6f AND %.6f', ...
                                DecColumn, DecLo, DecHi);

        LoStr = strrep(sprintf('%+.0f', DecLo), '+', 'p');
        LoStr = strrep(LoStr, '-', 'm');
        HiStr = strrep(sprintf('%+.0f', DecHi), '+', 'p');
        HiStr = strrep(HiStr, '-', 'm');
        Chunks{Ib, 3} = sprintf('dec_%s_%s', LoStr, HiStr);
    end
end


function Chunks = generateHealPixChunks(Nside, RAColumn, DecColumn)
% Generate chunk definitions for HealPix tiles.
% Placeholder — generates ADQL using CONTAINS(POINT, CIRCLE) for each tile center.
% A full implementation would compute exact tile boundaries.

    % HealPix ring scheme: Npix = 12 * Nside^2
    Npix = 12 * Nside^2;

    % TODO: Replace with proper HealPix center/boundary computation.
    %       For now, use a simple equal-area grid approximation.
    %       A proper implementation would use:
    %         [RA, Dec] = healpix.pix2ang(Nside, 0:Npix-1, 'ring');
    %       and compute the tile radius from the tile area.
    error('VO:prep:downloadACwithTAP:NotImplemented', ...
        ['HealPix subdivision is not yet implemented. ', ...
         'Requires HealPix toolbox for pix2ang conversion. ', ...
         'Npix would be %d for Nside=%d.'], Npix, Nside);

    % Skeleton for future implementation:
    % TileRadius = sqrt(4*pi / (12*Nside^2)) * 180/pi;  % approx radius [deg]
    % Chunks = cell(Npix, 3);
    % for Ip = 1:Npix
    %     CenRA  = ...;  % from pix2ang
    %     CenDec = ...;  % from pix2ang
    %     Chunks{Ip, 1} = sprintf('HealPix %d/%d', Ip, Npix);
    %     Chunks{Ip, 2} = sprintf('1=CONTAINS(POINT(''ICRS'', %s, %s), CIRCLE(''ICRS'', %.6f, %.6f, %.6f))', ...
    %                             RAColumn, DecColumn, CenRA, CenDec, TileRadius);
    %     Chunks{Ip, 3} = sprintf('hp_%05d', Ip);
    % end
end


%==========================================================================
% DOWNLOAD BACKENDS
%==========================================================================

function [Status, OutTxt] = executeStilts(Query, OutFile, TapUrl, Ofmt, SyncMode, StiltsJar, TimeoutSec)
% Execute a TAP query via STILTS command line.

    Jar = char(StiltsJar);
    if ~isempty(Jar) && Jar(1) == '~'
        Home = getenv('HOME');
        if strlength(Jar) >= 2 && (Jar(2) == '/' || Jar(2) == '\')
            Jar = fullfile(Home, Jar(3:end));
        end
    end
    StiltsBase = sprintf('java -Xmx2g -jar "%s"', Jar);

    % Escape ADQL for shell double quotes
    AdqlEsc = strrep(char(Query), '"', '\"');

    % sync parameter
    if strcmpi(SyncMode, 'sync')
        SyncStr = 'true';
    else
        SyncStr = 'false';
    end

    Cmd = sprintf('%s tapquery tapurl="%s" language=ADQL adql="%s" omode=out ofmt=%s out="%s" sync=%s 2>&1', ...
        StiltsBase, TapUrl, AdqlEsc, Ofmt, OutFile, SyncStr);

    if ispc
        [Status, OutTxt] = system(Cmd);
    else
        [HasTimeout, ~] = system('command -v timeout >/dev/null 2>&1');
        if HasTimeout == 0
            [Status, OutTxt] = system(sprintf('timeout %ds %s', ...
                round(TimeoutSec), Cmd));
        else
            [Status, OutTxt] = system(Cmd);
        end
    end
end


function Status = executeHttp(Query, OutFile, TapUrl, Ofmt, TimeoutSec)
% Execute a TAP query via VO.TopCat.queryHttp and save to file.

    try
        T = VO.TopCat.queryHttp(char(Query), ...
            'TapUrl', TapUrl, 'Ofmt', Ofmt, 'TimeoutSec', TimeoutSec);

        if strcmpi(Ofmt, 'fits')
            % T is an AstroCatalog — write to FITS
            T.write1(OutFile);
        else
            % T is a table — write to file
            writetable(T, OutFile);
        end
        Status = 0;
    catch
        Status = 1;
    end
end


%==========================================================================
% MERGE
%==========================================================================

function mergeChunksFits(ChunkFiles, MergeFile, Ofmt)
% Merge chunk files into a single file using STILTS tcat.
% Falls back to MATLAB-based concatenation if STILTS is unavailable.

    try
        StiltsJar = VO.TopCat.getStiltsJarPath();
        Jar = char(StiltsJar);
        StiltsBase = sprintf('java -Xmx4g -jar "%s"', Jar);

        % Build input file list for tcat
        InArgs = '';
        for If = 1:numel(ChunkFiles)
            InArgs = sprintf('%s in%d="%s" ifmt%d=%s', ...
                InArgs, If, ChunkFiles{If}, If, Ofmt);
        end

        Cmd = sprintf('%s tcat%s omode=out ofmt=%s out="%s" 2>&1', ...
            StiltsBase, InArgs, Ofmt, MergeFile);
        [Status, OutTxt] = system(Cmd);

        if Status ~= 0
            error('STILTS tcat failed: %s', OutTxt);
        end
    catch ME
        fprintf('STILTS merge failed (%s), falling back to MATLAB merge.\n', ME.message);
        mergeChunksMatlab(ChunkFiles, MergeFile, Ofmt);
    end
end


function mergeChunksMatlab(ChunkFiles, MergeFile, Ofmt)
% Merge chunk files by loading into MATLAB and concatenating.
% Works for FITS and CSV/TSV files.

    AllData = [];
    for If = 1:numel(ChunkFiles)
        switch lower(Ofmt)
            case 'fits'
                T = FITS.readTable1(ChunkFiles{If}, 'OutClass', []);
                if isempty(AllData)
                    AllData = T;
                else
                    AllData = [AllData; T]; %#ok<AGROW>
                end
            otherwise
                T = readtable(ChunkFiles{If});
                if isempty(AllData)
                    AllData = T;
                else
                    AllData = [AllData; T]; %#ok<AGROW>
                end
        end
        fprintf('  Merged chunk %d/%d\n', If, numel(ChunkFiles));
    end

    switch lower(Ofmt)
        case 'fits'
            AC = AstroCatalog;
            AC.Catalog = AllData;
            AC.write1(MergeFile);
        otherwise
            writetable(AllData, MergeFile);
    end
end
