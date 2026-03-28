function ps1dr2CsvToFits(CsvDir, FitsDir, Args)
    % Convert PS1DR2 CSV files to FITS binary tables.
    %   Reads each CSV, applies transforms (RA/Dec to radians, -999 to NaN,
    %   computed delta columns replacing kronmag columns), writes FITS.
    %   Zero-size CSV files are skipped.
    %
    % Input  : - CsvDir  : directory containing *_S*_P*.csv files
    %          - FitsDir : output directory for FITS files
    % Args   : 'DeleteCsv' - delete CSV after successful conversion (false)
    %          'Resume'     - skip already-converted files (true)
    %          'Verbose'    - print progress (true)
    %
    % Example:
    %   VO.prep.ps1dr2CsvToFits('~/tmp/PS1DR2/csv/', '~/tmp/PS1DR2/fits/');
    %
    % Author : Dana Kovaleva + Claude (2026 Mar)

    arguments
        CsvDir   string
        FitsDir  string
        Args.DeleteCsv  logical = false
        Args.Resume     logical = true
        Args.Verbose    logical = true
    end

    if ~isfolder(FitsDir)
        mkdir(FitsDir);
    end

    % Column definitions (56 input columns from SQL query)
    % Columns 1-56 as received from the database:
    %  1  RA              (float)   -> radians
    %  2  Dec             (float)   -> radians
    %  3  raerr           (float)
    %  4  decerr          (float)
    %  5  objinfoflag     (int)
    %  6  qualityflag     (tinyint)
    %  7  epochmean       (float)
    %  8  posmeanchisq    (float)
    %  9  gqfperfect      (real)
    % 10  gmeanpsfmag     (real)
    % 11  gmeanpsfmagerr  (real)
    % 12  gmeanpsfmagstd  (real)
    % 13  gmeanpsfmagnpt  (smallint)
    % 14  gmeanpsfmagmin  (real)
    % 15  gmeanpsfmagmax  (real)
    % 16  gmeankronmag    (real)   -> replaced by g_delta_psf_kron
    % 17  rqfperfect      (real)
    % 18  rmeanpsfmag     (real)
    % 19  rmeanpsfmagerr  (real)
    % 20  rmeanpsfmagstd  (real)
    % 21  rmeanpsfmagnpt  (smallint)
    % 22  rmeanpsfmagmin  (real)
    % 23  rmeanpsfmagmax  (real)
    % 24  rmeankronmag    (real)   -> replaced by r_delta_psf_kron
    % 25  iqfperfect      (real)
    % 26  imeanpsfmag     (real)
    % 27  imeanpsfmagerr  (real)
    % 28  imeanpsfmagstd  (real)
    % 29  imeanpsfmagnpt  (smallint)
    % 30  imeanpsfmagmin  (real)
    % 31  imeanpsfmagmax  (real)
    % 32  imeankronmag    (real)   -> replaced by i_delta_psf_kron
    % 33  zqfperfect      (real)
    % 34  zmeanpsfmag     (real)
    % 35  zmeanpsfmagerr  (real)
    % 36  zmeanpsfmagstd  (real)
    % 37  zmeanpsfmagnpt  (smallint)
    % 38  zmeanpsfmagmin  (real)
    % 39  zmeanpsfmagmax  (real)
    % 40  yqfperfect      (real)
    % 41  ymeanpsfmag     (real)
    % 42  ymeanpsfmagerr  (real)
    % 43  ymeanpsfmagstd  (real)
    % 44  ymeanpsfmagnpt  (smallint)
    % 45  ymeanpsfmagmin  (real)
    % 46  ymeanpsfmagmax  (real)
    % 47  gfmeanmagr5     (real)
    % 48  gfmeanmagr5err  (real)
    % 49  rfmeanmagr5     (real)
    % 50  rfmeanmagr5err  (real)
    % 51  ifmeanmagr5     (real)
    % 52  ifmeanmagr5err  (real)
    % 53  zfmeanmagr5     (real)
    % 54  zfmeanmagr5err  (real)
    % 55  yfmeanmagr5     (real)
    % 56  yfmeanmagr5err  (real)

    Ncols = 56;
    Fmt = repmat('%f', 1, Ncols);
    Delim = ',';

    % Output column names (56 columns: 3 kronmag replaced by 3 deltas)
    ColNames = {'RA','Dec','raerr','decerr', ...
        'objinfoflag','qualityflag','epochmean','posmeanchisq', ...
        'gqfperfect','gmeanpsfmag','gmeanpsfmagerr','gmeanpsfmagstd', ...
        'gmeanpsfmagnpt','gmeanpsfmagmin','gmeanpsfmagmax','g_delta_psf_kron', ...
        'rqfperfect','rmeanpsfmag','rmeanpsfmagerr','rmeanpsfmagstd', ...
        'rmeanpsfmagnpt','rmeanpsfmagmin','rmeanpsfmagmax','r_delta_psf_kron', ...
        'iqfperfect','imeanpsfmag','imeanpsfmagerr','imeanpsfmagstd', ...
        'imeanpsfmagnpt','imeanpsfmagmin','imeanpsfmagmax','i_delta_psf_kron', ...
        'zqfperfect','zmeanpsfmag','zmeanpsfmagerr','zmeanpsfmagstd', ...
        'zmeanpsfmagnpt','zmeanpsfmagmin','zmeanpsfmagmax', ...
        'yqfperfect','ymeanpsfmag','ymeanpsfmagerr','ymeanpsfmagstd', ...
        'ymeanpsfmagnpt','ymeanpsfmagmin','ymeanpsfmagmax', ...
        'gfmeanmagr5','gfmeanmagr5err', ...
        'rfmeanmagr5','rfmeanmagr5err', ...
        'ifmeanmagr5','ifmeanmagr5err', ...
        'zfmeanmagr5','zfmeanmagr5err', ...
        'yfmeanmagr5','yfmeanmagr5err'};

    % Find CSV files
    CsvFiles = dir(fullfile(CsvDir, '*_S*_P*.csv'));
    Nfiles = numel(CsvFiles);
    if Args.Verbose
        fprintf('Found %d CSV files in %s\n', Nfiles, CsvDir);
    end

    TotalTic = tic;
    Nskipped = 0;
    Nconverted = 0;
    Nempty = 0;

    for Ifile = 1:Nfiles
        CsvPath = fullfile(CsvFiles(Ifile).folder, CsvFiles(Ifile).name);
        [~, bn, ~] = fileparts(CsvFiles(Ifile).name);
        FitsPath = fullfile(FitsDir, [bn, '.fits']);

        % Skip zero-size files
        if CsvFiles(Ifile).bytes == 0
            Nempty = Nempty + 1;
            continue;
        end

        % Resume: skip already-converted
        if Args.Resume && isfile(FitsPath)
            Nskipped = Nskipped + 1;
            continue;
        end

        FileTic = tic;

        % Read CSV (no header, 56 numeric columns)
        fid = fopen(CsvPath, 'r');
        C = textscan(fid, Fmt, 'Delimiter', Delim);
        fclose(fid);

        Nrows = numel(C{1});
        if Nrows == 0
            Nempty = Nempty + 1;
            continue;
        end

        % Assemble numeric matrix
        Mat = zeros(Nrows, Ncols);
        for k = 1:Ncols
            Mat(:, k) = C{k};
        end
        clear C;

        % Replace -999 with NaN
        Mat(Mat == -999) = NaN;

        % Compute delta columns: psf - kron (replaces kronmag columns)
        % Col 16: gmeankronmag -> g_delta_psf_kron = gmeanpsfmag(10) - gmeankronmag(16)
        % Col 24: rmeankronmag -> r_delta_psf_kron = rmeanpsfmag(18) - rmeankronmag(24)
        % Col 32: imeankronmag -> i_delta_psf_kron = imeanpsfmag(26) - imeankronmag(32)
        Mat(:, 16) = Mat(:, 10) - Mat(:, 16);
        Mat(:, 24) = Mat(:, 18) - Mat(:, 24);
        Mat(:, 32) = Mat(:, 26) - Mat(:, 32);

        % Convert RA/Dec from degrees to radians
        Mat(:, 1) = Mat(:, 1) .* (pi / 180);
        Mat(:, 2) = Mat(:, 2) .* (pi / 180);

        % Write FITS binary table
        AT = AstroTable({Mat}, 'ColNames', ColNames);
        FITS.writeTable1(AT, FitsPath);
        clear Mat AT;

        Nconverted = Nconverted + 1;

        if Args.Verbose
            fprintf('[%d/%d] %s: %d rows (%.1f sec)\n', ...
                Ifile, Nfiles, bn, Nrows, toc(FileTic));
        end

        % Delete CSV after successful conversion
        if Args.DeleteCsv
            delete(CsvPath);
        end
    end

    if Args.Verbose
        fprintf('\nDone: %d converted, %d skipped, %d empty (%.1f min total)\n', ...
            Nconverted, Nskipped, Nempty, toc(TotalTic) / 60);
    end
end
