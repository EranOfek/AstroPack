function TESSpointpipe(FFIDataPath, RA, Dec, SavePath, Args)
    %{
    Runs a lightweight TESS point-source pipeline on a set of TESS FFI FITS
    files in a directory for a fixed sky position (RA, Dec). For each FFI, the
    pipeline:
      (1) reads the calibrated science image (via loadreadyFFI),
      (2) applies basic quality checks,
      (3) creates a calibrated cutout around the target coordinates,
      (4) writes requested cutout products to a visit directory under SavePath,
      (5) optionally performs image subtraction with a reference image using
          AstroZOGY and writes subtraction products,
      (6) optionally performs PSF photometry on the subtraction (D) and New
          images and writes a light curve table.
    
    Logging is written to Args.LogFile using MsgLogger. The pipeline is designed
    to continue processing the next FFI if a given FFI fails to load or fails
    cutout creation, while recording errors and stack traces in the log.
    
    Input   : - FFIDataPath. Path to a directory containing TESS FFI FITS files
                (currently matched by "*.fits").
              - RA. Right ascension of the target position. Units: degrees.
              - Dec. Declination of the target position. Units: degrees.
              - SavePath. Path to a directory in which to create per-FFI visit
                directories and save products.
    
              * ...,key,val,...
                'LogFile' - Path to a log file. If empty, MsgLogger default
                       behavior is used. Default is ''.
                'CleanRun' - Bool on whether to delete the existing log file at
                       Args.LogFile at the start of the run. Default is true.
                'cutoutFrame' - Cutout frame definition passed to
                       pipeline.tess.reduction.FFI2calibCutout. Default is
                       [100 100 100 100].
                'SaveProducts' - Cell array of product names to be written from
                       the cutout AstroImage using write1. Default is
                       {'Image','Mask','Cat','PSF'}.
                'TargetName' - Target label used in output filenames. If empty,
                       the pipeline uses the 'Sector' header value as a label.
                       Default is ''.
    
                'runSubtraction' - Bool on whether to run AstroZOGY subtraction
                       between the cutout (New) and a reference image (Ref).
                       Default is false.
                'RefPath' - Path to directory containing the reference image.
                       The reference file is searched using pattern
                       "TESS*_Image_1.fits". Default is ''.
                'SaveSubProducts' - Cell array of product names to be written
                       from the AstroZOGY object using write1. Default is
                       {'Image','Mask','Cat','PSF'}.
    
                'runPhotometry' - Bool on whether to run PSF photometry on the
                       difference image (D) and New image for the target
                       position and save a light curve table. Requires
                       runSubtraction = true. Default is false.
                'PhotometryFile' - Output CSV filename for the photometry light
                       curve table, written using writetable. Default is ''.
    
    Output  : - None. Products are written to disk under SavePath and messages
                are written to Args.LogFile.
    
    Products:
      - For each FFI, a per-FFI "visit" directory is created under SavePath
        using DATE-OBS from the cutout header (format: yyyyMMdd.HHmmss.SSS).
      - Cutout products (Args.SaveProducts) are written with filenames of the
        form:
          TESS.<CamID>.<CCDID>_<Time>_<Filter>_<TargetName>_000_<Counter>_<CropID>_<Type>_<Level>_<Product>_<Version>.fits
      - Subtraction products (Args.SaveSubProducts) are written similarly, but
        with SubLevel = 'proc.zogyD' in the filename level field.
      - If runPhotometry is enabled, a CSV file is written to
        Args.PhotometryFile with columns:
          JD, SN, FLUX_PSF, FLUXERR_PSF, MAG_PSF, MAGERR_PSF,
          N_SN, N_FLUX_PSF, N_FLUXERR_PSF, N_MAG_PSF, N_MAGERR_PSF
    
    Notes   : - The current FFI search uses "*.fits" in FFIDataPath. If your
                FFIs are ".fits.gz" you may need to adjust the file pattern.
              - Errors in opening an FFI or creating a cutout are caught and
                logged (including stack traces), and the pipeline continues to
                the next file.
              - When Args.CleanRun is true, the pipeline deletes Args.LogFile
                at start. If Args.LogFile is empty, delete('') may error; ensure
                a valid LogFile is provided when CleanRun is enabled.
    
    Author  : Ruslan Konno (Jan 2026)
    Example : FFIDataPath = '/marvin/TESS/SNe/SN2025cnu/FFIs';
              RA = 159.277033151;
              Dec = -7.46790523632;
              SavePath = '/marvin/TESS/SNe/SN2025cnu/proc';
    
              pipeline.tess.TESSpointpipe(FFIDataPath, RA, Dec, SavePath, ...
                  'LogFile', '/marvin/TESS/SNe/SN2025cnu/status/tess_pointpipe.log', ...
                  'CleanRun', true, ...
                  'TargetName', 'SN2025cnu', ...
                  'runSubtraction', true, ...
                  'RefPath', '/marvin/TESS/SNe/SN2025cnu/Ref', ...
                  'runPhotometry', true, ...
                  'PhotometryFile', '/marvin/TESS/SNe/SN2025cnu/results/photometry.csv');
    %}

    arguments
        FFIDataPath
        RA
        Dec
        SavePath

        Args.LogFile = '';
        Args.CleanRun = true;

        Args.cutoutFrame = [100 100 100 100];

        Args.SaveProducts = {'Image','Mask','Cat','PSF'};
        Args.TargetName = '';

        Args.runSubtraction = false;
        Args.RefPath = '';
        Args.SaveSubProducts = {'Image','Mask','Cat','PSF'};

        Args.runPhotometry = false;
        Args.PhotometryFile = '';

        Args.useMultiIterPSF = false;
    end

    if Args.CleanRun
        delete(Args.LogFile);
    end

    if ~exist(SavePath, 'dir')
       mkdir(SavePath)
    end

    if Args.runPhotometry && ~isempty(Args.PhotometryFile)
        PhotometryFileParentDir = fileparts(Args.PhotometryFile);

        if ~isfolder(PhotometryFileParentDir)
            mkdir(PhotometryFileParentDir)
        end
    end

    % Set up logging
    Logger = MsgLogger('FileName', Args.LogFile);

    % Print preamble
    PreambleMSG = sprintf('Running TESSpointpipe on FFIs in %s for coordinates RA,Dec = %f,%f', ...
        FFIDataPath, RA, Dec);
    Logger.msgLog(LogLevel.Info, PreambleMSG);

    % Get FFI Paths and verify they exist
    FFIPaths = dir(fullfile(FFIDataPath, "*.fits"));

    if isempty(FFIPaths)
        Logger.msgLog(LogLevel.Error, 'No FFIs found in %s', FFIDataPath);
    end

    NFFIs = numel(FFIPaths);

    Logger.msgLog(LogLevel.Info, 'Found %i FFIs fits files', NFFIs);
    
    if Args.runSubtraction
        
        if ~exist(Args.RefPath, 'dir')
            Logger.msgLog(LogLevel.Error, 'Reference path not found - %s', Args.RefPath);
            return
        end

        RefFileName = strcat(Args.RefPath,'/TESS*_Image_1.fits');
        Ref = AstroImage.readFileNamesObj(RefFileName, 'Path', Args.RefPath);

        if Args.useMultiIterPSF
            [Ref, ~] = imProc.sources.multiIterExtractor(Ref, ...
                'backVarArgs', {'Block',[128 128], 'Method',@imUtil.background.modeVar_LogHist, 'MethodArgs',{{'MinVal',100, 'MaxVal',120},{}}}, ...
                'ZP', 20.44, 'UseOriginalPSF', false);
        end

        if Ref.isemptyImage
            Logger.msgLog(LogLevel.Error, 'Reference image is empty.');
            return
        end

        SubLevel = 'proc.zogyD';
        NumSaveSubProd = numel(Args.SaveSubProducts);
    end

    Filter = 'clear';
    Counter = 1;
    CropID = 0;
    Type = 'sci';
    Level = 'proc';
    Version = 1;
    FileType = 'fits';
    NumSaveProd = numel(Args.SaveProducts);

    % If running photometry, pre-allocate memory for arrays.
    if Args.runPhotometry
        JD = zeros(NFFIs,1);
        CHI2DOF= zeros(NFFIs,1);
        D_SN = zeros(NFFIs,1);
        D_FLUX_PSF = zeros(NFFIs,1);
        D_FLUXERR_PSF = zeros(NFFIs,1);
        D_MAG_PSF = zeros(NFFIs,1);
        D_MAGERR_PSF = zeros(NFFIs,1);
        
        N_SN = zeros(NFFIs,1);
        N_FLUX_PSF = zeros(NFFIs,1);
        N_FLUXERR_PSF= zeros(NFFIs,1);
        N_MAG_PSF = zeros(NFFIs,1);
        N_MAGERR_PSF = zeros(NFFIs,1);
    end

    for IFFI = 1:NFFIs

        FFIPath = fullfile(FFIPaths(IFFI).folder, FFIPaths(IFFI).name);
        Logger.msgLog(LogLevel.Info, '>>> Processing %s (%i/%i)', FFIPath, IFFI, NFFIs);
    
        try
            FFI = pipeline.tess.reduction.loadreadyFFI(FFIPath);
        catch ME
            Logger.msgLog(LogLevel.Error, 'Failure opening FFI');
            Logger.msgLog(LogLevel.Error, ME.message);

            Logger.msgLog(LogLevel.Error, 'Traceback: ');
            for k = 1:numel(ME.stack)
                s = ME.stack(k);
                Logger.msgLog(LogLevel.Error, "%s (line %d)", s.name, s.line);
            end
            
            continue
        end

        BaseQuality = pipeline.tess.quality.checkBaseQuality(FFI, 'Logger', Logger);

        if ~BaseQuality
            Logger.msgLog(LogLevel.Info, 'FFI fails base quality check, skipping processing.')
            continue
        end

        Logger.msgLog(LogLevel.Info, 'Creating cutout');

        try
            FFIc = pipeline.tess.reduction.FFI2calibCutout(FFI, RA, Dec, ...
                'cutoutFrame', Args.cutoutFrame);
        catch ME
            Logger.msgLog(LogLevel.Error, 'Failure creating cutout');
            Logger.msgLog(LogLevel.Error, ME.message);

            Logger.msgLog(LogLevel.Error, 'Traceback: ');
            for k = 1:numel(ME.stack)
                s = ME.stack(k);
                Logger.msgLog(LogLevel.Error, "%s (line %d)", s.name, s.line);
            end

            continue
        end

        ObsDate = FFIc.HeaderData.getVal('DATE-OBS');
        CamID = FFIc.HeaderData.getVal('CAMERA');
        CCDID = FFIc.HeaderData.getVal('CCD');
        Sector = FFIc.HeaderData.getVal('Sector');

        DateTime = datetime(ObsDate,"InputFormat","yyyy-MM-dd'T'HH:mm:ss.SSS");
        Time =  convertStringsToChars(string(DateTime,'yyyyMMdd.HHmmss.SSS'));

        SaveVisitPath = strcat(SavePath, '/', Time);

        if ~exist(SaveVisitPath, 'dir')
           mkdir(SaveVisitPath)
        end
                
        ProjName = strcat('TESS.',sprintf('%02.0f', CamID),'.',sprintf('%02.0f', CCDID));

        if Args.TargetName
            TargetName = Args.TargetName;
        else
            TargetName = Sector;
        end

        Logger.msgLog(LogLevel.Info, 'Saving cutout products to %s', SaveVisitPath);
       
        for ISaveProducts=1:NumSaveProd
            ISaveProd = Args.SaveProducts{ISaveProducts};
            ISaveProdFilename = strcat(ProjName,'_',Time,'_',Filter,'_',TargetName,'_', ...
                '000','_',num2str(Counter,'%03.f'),'_', ...
                num2str(CropID,'%03.f'),'_', Type,'_', Level,'_', ISaveProd, '_', ...
                int2str(Version), '.',FileType);
            ISaveProdFilename = strcat(SaveVisitPath,'/',ISaveProdFilename);
            FFIc.write1(ISaveProdFilename, ISaveProd, ...
                'OverWrite', true, 'WriteHeader', true);
        end

        if ~Args.runSubtraction
            Logger.msgLog(LogLevel.Info, '<<< FFI processed.');
            continue
        end

        Logger.msgLog(LogLevel.Info, 'Running subtraction with reference image %s', Ref.ImageData.FileName);
        
        AD = AstroZOGY(FFIc, Ref);
        
        % Estimate backround and variance of FFIc and Ref
        AD.estimateBackVar('useHeaderVal',false);
        % Register images
        AD.register;
        % Estimate zero points
        AD.estimateFnFr;
        % Create proper subtraction image D
        AD.subtractionD;
        % Derive S stat image
        AD.subtractionS;

        Logger.msgLog(LogLevel.Info, 'Saving subtraction products to %s', SaveVisitPath);
        for ISaveSubProducts=1:NumSaveSubProd
            ISaveSubProd = Args.SaveSubProducts{ISaveSubProducts};
            ISaveSubProdFilename = strcat(ProjName,'_',Time,'_',Filter,'_',TargetName,'_', ...
                '000','_',num2str(Counter,'%03.f'),'_', ...
                num2str(CropID,'%03.f'),'_', Type,'_', SubLevel,'_', ISaveSubProd, '_', ...
                int2str(Version), '.',FileType);
            ISaveSubProdFilename = strcat(SaveVisitPath,'/',ISaveSubProdFilename);
            AD.write1(ISaveSubProdFilename, ISaveSubProd, ...
                'OverWrite', true, 'WriteHeader', true);
        end

        if ~Args.runPhotometry
            Logger.msgLog(LogLevel.Info, '<<< FFI processed.');
            continue
        end

        Logger.msgLog(LogLevel.Info, 'Running photometry.');

        [X,Y] = AD.WCS.sky2xy(RA,Dec);
        X = cast(X,'single');
        Y = cast(Y,'single');
        
        % PSF fit source in the D image
        PSFSize = floor(size(AD.PSFData.getPSF,2)/2);
        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD.Dbs, X, Y, PSFSize);

        VarD = AD.Var(X,Y);
        StdD = sqrt(VarD);
        [ResultD, ~] = imUtil.sources.psfPhotCube(Cube, ...
            'PSF', AD.PSFData.getPSF, 'Back', 0, 'Std', StdD,...
            'ZP', AD.ZpD);
        
        % PSF fit source in the New image
        CutHalfSize =  floor(size(AD.New.PSFData.getPSF,2)/2);
        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD.Nbs, X, Y, CutHalfSize);
        
        [ResultN, ~] = imUtil.sources.psfPhotCube(Cube,...
            'PSF', AD.New.PSFData.getPSF, 'Back', 0, 'Std', AD.SigmaN, ...
            'ZP', AD.ZpN);
                
        % Get JD
        JD(IFFI) = AD.New.julday;

        ExposureN = AD.New.HeaderData.getVal('Exposure')*24*3600;
        
        % Get chi2 per degrees of freedom of the PSF fit on the difference
        % image.
        CHI2DOF(IFFI) = ResultD.Chi2./ResultD.Dof;
        
        % Estimate flux and magnitude error
        D_SN(IFFI) = ResultD.SNm;
        D_FLUX_PSF(IFFI) = ResultD.Flux;
        D_FLUXERR_PSF(IFFI) = sqrt(abs(ResultD.Flux))/sqrt(ExposureN);
        D_MAG_PSF(IFFI) = ResultD.Mag;
        D_MAGERR_PSF(IFFI) = 1.086./D_FLUXERR_PSF(IFFI);
        
        N_SN(IFFI) = ResultN.SNm;
        N_FLUX_PSF(IFFI) = ResultN.Flux;
        N_FLUXERR_PSF(IFFI) = sqrt(abs(ResultN.Flux))/sqrt(ExposureN);
        N_MAG_PSF(IFFI) = ResultN.Mag;
        N_MAGERR_PSF(IFFI) = 1.086./N_FLUXERR_PSF(IFFI);

        Logger.msgLog(LogLevel.Info, '<<< FFI processed.');

    end

    if Args.runPhotometry && ~isempty(JD)
        Logger.msgLog(LogLevel.Info, 'Saving photometry results to %s.', Args.PhotometryFile);
        
        LCTableColumns = {'JD','SN','FLUX_PSF','FLUXERR_PSF','MAG_PSF','MAGERR_PSF',...
            'N_SN','N_FLUX_PSF','N_FLUXERR_PSF','N_MAG_PSF','N_MAGERR_PSF'};
    
        LCTable = table(JD, D_SN, D_FLUX_PSF, D_FLUXERR_PSF, D_MAG_PSF, D_MAGERR_PSF,...
            N_SN, N_FLUX_PSF, N_FLUXERR_PSF, N_MAG_PSF, N_MAGERR_PSF, ...
            'VariableNames', LCTableColumns);
        
        writetable(LCTable, Args.PhotometryFile);
    end

    Logger.msgLog(LogLevel.Info, 'End of pipeline process.');
    
end