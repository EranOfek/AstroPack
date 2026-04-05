function decalsSweepToHdf5(SourceURL, FitsDir, Hdf5Dir, Args)
    % Download DECaLS DR10 sweep FITS files and convert to HDF5.
    % Package: VO.prep
    % Description: Downloads sweep-*.fits files from the NERSC URL to
    %              FitsDir and converts each to an HDF5 file in Hdf5Dir.
    %              The HDF5 file stores the extracted 23-column numeric
    %              matrix under '/data' (with RA/Dec in radians) plus
    %              MinDec/MaxDec/MinRA/MaxRA attributes in radians.
    %              FITS files are kept after conversion.
    %
    %              Column extraction and TYPE encoding are done by
    %              VO.prep.decalsPostRead. Output columns match the
    %              ColNames list used in the buildHTMfromFiles call.
    %
    %              Designed to work with VO.prep.buildHTMfromFiles in
    %              per-file mode using @(F) h5read(F, '/data') as
    %              PostReadFun. Range attributes are auto-detected by
    %              buildHTMfromFiles for *.hdf5 files (no ParseRangeFun
    %              needed).
    %
    % Input  : - SourceURL : URL of directory listing with sweep-*.fits.
    %                        e.g. 'https://portal.nersc.gov/cfs/cosmo/data/
    %                        legacysurvey/dr10/south/sweep/10.1/'
    %          - FitsDir   : local directory for downloaded FITS files.
    %          - Hdf5Dir   : local directory for converted HDF5 files.
    % Args   : 'Resume'      - skip files whose HDF5 already exists.
    %                          Default is true.
    %          'Verbose'     - print progress. Default is true.
    %          'RetryOnFail' - retry download on corrupt FITS. Default is true.
    % Output : null
    % Author : Dana Kovaleva + Claude (Apr 2026)
    % Example:
    %   VO.prep.decalsSweepToHdf5( ...
    %       'https://portal.nersc.gov/cfs/cosmo/data/legacysurvey/dr10/south/sweep/10.1/', ...
    %       '/home/dana/tmp/DECaLS10/fits/', ...
    %       '/home/dana/tmp/DECaLS10/hdf5/');

    arguments
        SourceURL   string
        FitsDir     string
        Hdf5Dir     string
        Args.Resume      logical = true
        Args.Verbose     logical = true
        Args.RetryOnFail logical = true
    end

    FitsDir = char(FitsDir);
    Hdf5Dir = char(Hdf5Dir);

    if ~isfolder(FitsDir), mkdir(FitsDir); end
    if ~isfolder(Hdf5Dir), mkdir(Hdf5Dir); end

    %--- Scrape file list from URL -----------------------------------
    AllFiles = scrapeSweepList(char(SourceURL));
    Nfiles = numel(AllFiles);
    if Args.Verbose
        fprintf('Found %d sweep files at %s\n', Nfiles, SourceURL);
    end

    TotalTic = tic;
    Nconverted = 0;
    Nskipped   = 0;
    Nfailed    = 0;

    for Ifile = 1:Nfiles
        FileURL = AllFiles{Ifile};
        [~, Bn, Ext] = fileparts(FileURL);
        FitsPath = fullfile(FitsDir, [Bn, Ext]);
        Hdf5Path = fullfile(Hdf5Dir, [Bn, '.hdf5']);

        % Resume: skip files whose HDF5 already exists
        if Args.Resume && isfile(Hdf5Path)
            Nskipped = Nskipped + 1;
            continue;
        end

        FileTic = tic;

        % Download FITS if not already present
        if ~isfile(FitsPath)
            if Args.Verbose
                fprintf('[%d/%d] Downloading %s ...\n', Ifile, Nfiles, Bn);
            end
            [Status, ~] = system(sprintf('wget -q -c -O "%s" "%s"', ...
                FitsPath, FileURL));
            if Status ~= 0
                fprintf('  WARNING: download failed for %s\n', Bn);
                Nfailed = Nfailed + 1;
                continue;
            end
        end

        % Read FITS and apply decalsPostRead (with one retry on corrupt FITS)
        Mat = [];
        for Iattempt = 1:2
            try
                T = FITS.readTable1(FitsPath, 'OutClass', []);
                Mat = VO.prep.decalsPostRead(T);
                clear T;
                break;
            catch ME
                if Iattempt == 1 && Args.RetryOnFail
                    fprintf('  WARNING: read failed (%s), re-downloading %s\n', ...
                        ME.message, Bn);
                    delete(FitsPath);
                    [Status, ~] = system(sprintf('wget -q -O "%s" "%s"', ...
                        FitsPath, FileURL));
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
        if isempty(Mat)
            Nfailed = Nfailed + 1;
            continue;
        end

        Nrows = size(Mat, 1);
        if Nrows == 0
            Nfailed = Nfailed + 1;
            continue;
        end

        % Convert RA/Dec (cols 1,2) from degrees to radians
        Mat(:, 1) = Mat(:, 1) .* (pi / 180);
        Mat(:, 2) = Mat(:, 2) .* (pi / 180);

        % Compute RA/Dec range attributes (in radians)
        MinDec = min(Mat(:, 2));
        MaxDec = max(Mat(:, 2));
        MinRA  = min(Mat(:, 1));
        MaxRA  = max(Mat(:, 1));

        % Write HDF5
        if isfile(Hdf5Path)
            delete(Hdf5Path);
        end
        h5create(Hdf5Path, '/data', size(Mat));
        h5write(Hdf5Path, '/data', Mat);
        h5writeatt(Hdf5Path, '/data', 'MinDec', MinDec);
        h5writeatt(Hdf5Path, '/data', 'MaxDec', MaxDec);
        h5writeatt(Hdf5Path, '/data', 'MinRA',  MinRA);
        h5writeatt(Hdf5Path, '/data', 'MaxRA',  MaxRA);
        clear Mat;

        Nconverted = Nconverted + 1;

        if Args.Verbose
            fprintf('[%d/%d] %s: %d rows (%.1f sec)\n', ...
                Ifile, Nfiles, Bn, Nrows, toc(FileTic));
        end
    end

    if Args.Verbose
        fprintf('\nDone: %d converted, %d skipped, %d failed (%.1f min total)\n', ...
            Nconverted, Nskipped, Nfailed, toc(TotalTic) / 60);
    end
end


function FileList = scrapeSweepList(BaseURL)
    % Scrape HTML directory listing for sweep-*.fits files
    if ~endsWith(BaseURL, '/'), BaseURL = [BaseURL '/']; end

    TmpFile = [tempname '.html'];
    [Status, ~] = system(sprintf('wget -q -O "%s" "%s"', TmpFile, BaseURL));
    if Status ~= 0
        error('VO:prep:decalsSweepToHdf5', ...
            'Failed to download directory listing from %s', BaseURL);
    end

    Html = fileread(TmpFile);
    delete(TmpFile);

    Tokens = regexp(Html, 'href="(sweep-[^"]*\.fits)"', 'tokens');
    FileList = cell(numel(Tokens), 1);
    for Itoken = 1:numel(Tokens)
        Fname = Tokens{Itoken}{1};
        if startsWith(Fname, 'http')
            FileList{Itoken} = Fname;
        else
            FileList{Itoken} = [BaseURL Fname];
        end
    end
end
