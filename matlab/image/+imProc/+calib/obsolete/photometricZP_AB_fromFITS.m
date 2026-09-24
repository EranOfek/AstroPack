function [Result, ResFit] = photometricZP_AB_fromFITS(FitsFile, varargin)
    % Wrapper for photometricZP_AB_DRAFT that accepts FITS file paths
    % This function loads LAST catalog FITS files and performs transmission-based
    % photometric calibration using Gaia XP spectra.
    %
    % Input  : - FitsFile - Path to LAST catalog FITS file (string or char)
    %                       OR cell array of FITS file paths for multiple images
    %          * ...,key,val,...
    %            All arguments from photometricZP_AB_DRAFT are supported
    %
    % Output : - Result - AstroImage array with calibrated catalogs
    %          - ResFit - Structure array with calibration results
    %
    % Author : D. Kovaleva (Nov 2025)
    % Example: % Calibrate single FITS file
    %          fits_file = '~/matlab/data/transmission_fitter/LASTfiles/LAST.01.08.03_20230616.222625.384_clear_346+79_000_001_001_sci_coadd_Cat_1.fits';
    %          [Result, ResFit] = imProc.calib.photometricZP_AB_fromFITS(fits_file, 'Verbose', true);
    %
    %          % Calibrate multiple FITS files
    %          fits_files = {file1, file2, file3};
    %          [Result, ResFit] = imProc.calib.photometricZP_AB_fromFITS(fits_files, 'Verbose', true);

    % Parse verbose flag from varargin
    p = inputParser;
    addParameter(p, 'Verbose', true, @islogical);
    parse(p, varargin{:});
    Verbose = p.Results.Verbose;

    % Convert to cell array if single file
    if ischar(FitsFile) || isstring(FitsFile)
        FitsFile = {char(FitsFile)};
    end

    % Expand tilde if present
    for i = 1:length(FitsFile)
        if startsWith(FitsFile{i}, '~')
            FitsFile{i} = strrep(FitsFile{i}, '~', getenv('HOME'));
        end
    end

    Nfiles = length(FitsFile);

    if Verbose
        fprintf('\n=== Loading %d FITS file(s) ===\n', Nfiles);
    end

    % Load FITS files into AstroImage array
    AI = AstroImage([Nfiles, 1]);

    for i = 1:Nfiles
        fits_path = FitsFile{i};

        % Check if file exists
        if ~exist(fits_path, 'file')
            error('FITS file not found: %s', fits_path);
        end

        if Verbose
            fprintf('Loading file %d/%d: %s\n', i, Nfiles, fits_path);
        end

        % Read FITS file into AstroImage
        % The LAST catalog FITS files contain the catalog in a binary table extension
        AI(i) = AstroImage(fits_path);

        % Verify catalog was loaded
        if isempty(AI(i).CatData) || AI(i).CatData.sizeCatalog == 0
            error('No catalog data found in FITS file: %s', fits_path);
        end

        if Verbose
            fprintf('  Loaded %d sources\n', AI(i).CatData.sizeCatalog);
        end
    end

    if Verbose
        fprintf('=== All FITS files loaded successfully ===\n\n');
    end

    % Call the main calibration function
    [Result, ResFit] = imProc.calib.photometricZP_AB_DRAFT(AI, varargin{:});

end
