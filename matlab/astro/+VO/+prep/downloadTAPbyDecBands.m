function Result = downloadTAPbyDecBands(BaseQuery, Args)
% Download large TAP query results by splitting into declination bands
% Package: VO.prep
% Description: Splits a large ADQL query into declination bands and
%              downloads each band as a separate FITS file using STILTS.
%              Avoids TAP row limits by keeping each band small enough.
%              Does not load data into MATLAB memory.
% Input  : - Base ADQL query string containing a WHERE clause.
%            The Dec constraint is appended automatically.
%          * ...,key,val,...
%            'DecEdges' - Vector of declination band edges [deg].
%                         Default: -90:5:90 (36 bands of 5 deg).
%            'OutputDir' - Directory for output FITS files.
%                          Default: pwd.
%            'FilePrefix' - Prefix for output filenames.
%                           Default: 'tap_band'.
%            'TapUrl' - TAP service URL.
%                       Default: Gaia ESA TAP.
%            'StiltsJar' - Path to stilts.jar.
%                          Default: from VO.TopCat.getStiltsJarPath().
%            'TimeoutSec' - Timeout per band query [sec]. Default: 3600.
%            'MaxRetries' - Number of retries for failed bands. Default: 3.
%            'DecColumn'  - Name of the Dec column in the ADQL table.
%                           Default: 'dec'.
%            'Verbose' - Print progress. Default: true.
% Output : - Struct with fields:
%            .Files   - Cell array of output FITS file paths.
%            .Status  - Vector of status codes (0=success).
%            .Nbands  - Number of bands.
% Author : Dana Kovaleva (Feb 2026)
% Example:
%   Q = ['SELECT * FROM gaiaedr3.gaia_source ', ...
%        'WHERE parallax > 1 ', ...
%        'AND parallax_over_error > 5 ', ...
%        'AND parallax_error < 2 ', ...
%        'AND phot_g_mean_mag IS NOT NULL'];
%   Result = VO.prep.downloadTAPbyDecBands(Q, ...
%       'OutputDir', '/data/gaia_parallax', ...
%       'FilePrefix', 'gaia_plx');

    arguments
        BaseQuery
        Args.DecEdges       = -90:1:90
        Args.OutputDir      = pwd
        Args.FilePrefix     = 'tap_band'
        Args.TapUrl         = 'https://gea.esac.esa.int/tap-server/tap'
        Args.StiltsJar      = VO.TopCat.getStiltsJarPath()
        Args.TimeoutSec     = 3600
        Args.MaxRetries     = 3
        Args.DecColumn      = 'dec'
        Args.Verbose        = true
    end

    DecEdges = Args.DecEdges(:)';
    Nbands   = numel(DecEdges) - 1;

    if ~exist(Args.OutputDir, 'dir')
        mkdir(Args.OutputDir);
    end

    % Build STILTS launcher
    Jar = char(Args.StiltsJar);
    if ~isempty(Jar) && Jar(1) == '~'
        Home = getenv('HOME');
        if strlength(Jar) >= 2 && (Jar(2) == '/' || Jar(2) == '\')
            Jar = fullfile(Home, Jar(3:end));
        end
    end
    StiltsBase = sprintf('java -Xmx2g -jar "%s"', Jar);

    % Normalize TAP URL
    TapUrl = char(Args.TapUrl);
    if endsWith(TapUrl, '/'), TapUrl = TapUrl(1:end-1); end

    % Strip trailing semicolons and whitespace from the base query
    BaseQuery = strtrim(char(BaseQuery));
    if endsWith(BaseQuery, ';'), BaseQuery = BaseQuery(1:end-1); end

    Result.Files  = cell(Nbands, 1);
    Result.Status = zeros(Nbands, 1);
    Result.Nbands = Nbands;

    TotalTic = tic;

    for Ib = 1:Nbands
        DecLo = DecEdges(Ib);
        DecHi = DecEdges(Ib + 1);

        % Output filename encodes the Dec range
        LoStr = strrep(sprintf('%+.0f', DecLo), '+', 'p');
        LoStr = strrep(LoStr, '-', 'm');
        HiStr = strrep(sprintf('%+.0f', DecHi), '+', 'p');
        HiStr = strrep(HiStr, '-', 'm');
        OutFile = fullfile(Args.OutputDir, ...
            sprintf('%s_dec_%s_%s.fits', Args.FilePrefix, LoStr, HiStr));
        Result.Files{Ib} = OutFile;

        % Skip if file already exists (resume support)
        if exist(OutFile, 'file') == 2
            Info = dir(OutFile);
            if Info.bytes > 0
                if Args.Verbose
                    fprintf('[%d/%d] SKIP Dec [%+.0f, %+.0f] — %s exists (%.1f MB)\n', ...
                        Ib, Nbands, DecLo, DecHi, OutFile, Info.bytes/1e6);
                end
                continue;
            end
        end

        % Build the band query
        BandQuery = sprintf('%s AND %s BETWEEN %.6f AND %.6f', ...
            BaseQuery, Args.DecColumn, DecLo, DecHi);

        % Escape for shell double quotes
        AdqlEsc = strrep(BandQuery, '"', '\"');

        % Assemble STILTS command (async mode for large results)
        Cmd = sprintf('%s tapquery tapurl="%s" language=ADQL adql="%s" omode=out ofmt=fits out="%s" sync=false 2>&1', ...
            StiltsBase, TapUrl, AdqlEsc, OutFile);

        % Execute with retries
        Success = false;
        for Itry = 1:Args.MaxRetries
            if Args.Verbose
                fprintf('[%d/%d] Dec [%+.0f, %+.0f] attempt %d/%d ...', ...
                    Ib, Nbands, DecLo, DecHi, Itry, Args.MaxRetries);
            end
            BandTic = tic;

            if ispc
                [Status, OutTxt] = system(Cmd);
            else
                % Use system timeout if available
                [HasTimeout, ~] = system('command -v timeout >/dev/null 2>&1');
                if HasTimeout == 0
                    [Status, OutTxt] = system(sprintf('timeout %ds %s', ...
                        round(Args.TimeoutSec), Cmd));
                else
                    [Status, OutTxt] = system(Cmd);
                end
            end

            Elapsed = toc(BandTic);

            if Status == 0 && exist(OutFile, 'file') == 2
                Info = dir(OutFile);
                if Args.Verbose
                    fprintf(' OK (%.1f s, %.1f MB)\n', Elapsed, Info.bytes/1e6);
                end
                Success = true;
                break;
            else
                if Args.Verbose
                    fprintf(' FAILED (status=%d, %.1f s)\n', Status, Elapsed);
                    if ~isempty(OutTxt)
                        fprintf('  %s\n', strtrim(OutTxt));
                    end
                end
                % Clean up partial file
                if exist(OutFile, 'file') == 2
                    delete(OutFile);
                end
                if Itry < Args.MaxRetries
                    pause(5 * Itry);  % back off before retry
                end
            end
        end

        if ~Success
            Result.Status(Ib) = 1;
            fprintf('  WARNING: band %d Dec [%+.0f, %+.0f] failed after %d retries\n', ...
                Ib, DecLo, DecHi, Args.MaxRetries);
        end
    end

    % Summary
    Nfailed = sum(Result.Status ~= 0);
    if Args.Verbose
        fprintf('\nDone: %d/%d bands downloaded (%.1f min total)\n', ...
            Nbands - Nfailed, Nbands, toc(TotalTic)/60);
        if Nfailed > 0
            fprintf('Failed bands:\n');
            FailIdx = find(Result.Status ~= 0);
            for If = 1:numel(FailIdx)
                Idx = FailIdx(If);
                fprintf('  Dec [%+.0f, %+.0f]\n', DecEdges(Idx), DecEdges(Idx+1));
            end
        end
    end

end
