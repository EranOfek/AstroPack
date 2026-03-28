function ps1dr2CsvToHdf5(CsvDir, Hdf5Dir, Args)
    % Convert PS1DR2 CSV files to HDF5 binary files.
    %   Reads each CSV, applies transforms (RA/Dec to radians, -999 to NaN,
    %   computed delta columns replacing kronmag columns), writes HDF5.
    %   Zero-size CSV files are skipped.
    %
    % Input  : - CsvDir   : directory containing *_S*_P*.csv files
    %          - Hdf5Dir  : output directory for HDF5 files
    % Args   : 'DeleteCsv' - delete CSV after successful conversion (false)
    %          'Resume'     - skip already-converted files (true)
    %          'Verbose'    - print progress (true)
    %
    % Output HDF5 structure: dataset '/data' contains Nrows x 56 double matrix.
    % Use VO.prep.ps1dr2ReadHdf5 as PostReadFun when calling buildHTMfromFiles.
    %
    % Example:
    %   VO.prep.ps1dr2CsvToHdf5('~/tmp/PS1DR2/csv/', '~/tmp/PS1DR2/hdf5/');
    %
    % Author : Dana Kovaleva + Claude (2026 Mar)

    arguments
        CsvDir   string
        Hdf5Dir  string
        Args.DeleteCsv  logical = false
        Args.Resume     logical = true
        Args.Verbose    logical = true
    end

    if ~isfolder(Hdf5Dir)
        mkdir(char(Hdf5Dir));
    end

    Ncols = 56;
    Fmt = repmat('%f', 1, Ncols);
    Delim = ',';

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
        Hdf5Path = fullfile(char(Hdf5Dir), [bn, '.hdf5']);

        % Skip zero-size files
        if CsvFiles(Ifile).bytes == 0
            Nempty = Nempty + 1;
            continue;
        end

        % Resume: skip already-converted
        if Args.Resume && isfile(Hdf5Path)
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

        % Write HDF5
        if isfile(Hdf5Path)
            delete(Hdf5Path);
        end
        h5create(Hdf5Path, '/data', size(Mat));
        h5write(Hdf5Path, '/data', Mat);
        clear Mat;

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
