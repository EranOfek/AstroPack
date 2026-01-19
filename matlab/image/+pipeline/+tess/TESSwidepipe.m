function TESSwidepipe(FFIDataPath, SavePath, Args)
    %{
    Runs a wide-field TESS FFI processing pipeline on all FITS files in a
    directory. For each input FFI, the pipeline can:
      (1) load and sanitize the FFI header (via loadreadyFFI),
      (2) apply basic quality checks,
      (3) split the FFI into overlapping calibrated sub-images (tiles),
      (4) write requested per-tile products (Image/Mask/Cat/PSF) to a per-FFI
          visit directory under SavePath,
      (5) optionally build reference sub-images from a separate reference FFI
          directory and write them to Args.RefPath,
      (6) optionally perform tile-by-tile image subtraction with the matching
          reference tiles using AstroZOGY, derive statistics images, find and
          measure transient candidates, flag non-transients using a filter
          configuration file, and optionally write a merged transient catalog.
    
    Logging is written to Args.LogFile using MsgLogger. The pipeline is designed
    to continue to the next FFI if a given file fails to load or tile creation
    fails, while recording errors and stack traces in the log.
    
    Input   : - FFIDataPath. Path to directory containing TESS FFI FITS files
                (currently matched by "*.fits").
              - SavePath. Path to directory in which to create per-FFI visit
                directories and save products.
    
              * ...,key,val,...
                'CleanRun' - Bool on whether to run the pipeline from scratch
                       (true) or pick up from a previous run (false). False
                       is not yet implemented. Default is true.
                'LogFile' - Path to a log file written by MsgLogger. Default
                       is ''.
                'SaveProducts' - Cell array of product names to write for each
                       tile/sub-image using AstroImage.write1. Default is
                       {'Image','Mask','Cat','PSF'}.
                'makeRefs' - Bool on whether to generate reference tiles from
                       FFIs in Args.FFIRefDataPath and save them into Args.RefPath.
                       Default is false.
                'FFIRefDataPath' - Path to directory containing reference FFIs
                       (matched by "*.fits") used when makeRefs is true.
                       Default is ''.
                'SaveRefProducts' - Cell array of product names to write for
                       reference tiles using AstroImage.write1. Default is
                       {'Image','Mask','Cat','PSF'}.
                'RefPath' - Output directory for reference tile products when
                       makeRefs is true, and also the directory searched for
                       reference tiles when runSubtraction is true. Default
                       is ''.
                'runSubtraction' - Bool on whether to perform AstroZOGY image
                       subtraction of each science tile against a corresponding
                       reference tile located under Args.RefPath. Default is false.
                'FilterConfigFile' - Path to a JSON configuration file used by
                       AD.flagNonTransients to reject non-transient candidates.
                       Default is ''.
                'saveMergedCat' - Bool on whether to save the per-FFI merged
                       transient catalog (after filtering) to the visit directory.
                       The merged catalog is written only if it is non-empty.
                       Default is false.
    Output  : - None. All products are written to disk and messages are written
                to Args.LogFile.
    Author  : Ruslan Konno (Jan 2026)
    Example : % Run wide pipeline with reference creation + subtraction + merged catalog:
              FFIDataPath = '/marvin/TESS/GRBs/GRB251013C/FFIs';
              SavePath    = '/marvin/TESS/GRBs/GRB251013C/proc';
    
              pipeline.tess.TESSwidepipe(FFIDataPath, SavePath, ...
                  'LogFile', '/path/to/target/status/tess_widepipe.log', ...
                  'CleanRun', true, ...
                  'makeRefs', true, ...
                  'FFIRefDataPath', '/path/to/target/FFIs_Ref', ...
                  'RefPath', '/path/to/target/ref', ...
                  'runSubtraction', true, ...
                  'FilterConfigFile', '/path/to/configs/TESS.FilterParameters.json', ...
                  'saveMergedCat', true);
    %}

    arguments
        FFIDataPath
        SavePath

        Args.CleanRun = true;
        Args.LogFile = '';

        Args.SaveProducts = {'Image','Mask','Cat','PSF'};

        Args.runSubtraction = false;
        Args.RefPath = '';

        Args.makeRefs = false;
        Args.SaveRefProducts = {'Image','Mask','Cat','PSF'};
        
        Args.FFIRefDataPath = '';

        Args.FilterConfigFile = '';

        Args.saveMergedCat = false;
    end

    if Args.CleanRun
        delete(Args.LogFile);
    end

    if ~exist(SavePath, 'dir')
       mkdir(SavePath)
    end
    
    % Set up logging
    Logger = MsgLogger('FileName', Args.LogFile);

    % Print preamble
    PreambleMSG = sprintf('Running TESSwidepipe on FFIs in %s', FFIDataPath);
    Logger.msgLog(LogLevel.Info, PreambleMSG);

    Filter = 'clear';
    Counter = 1;
    Type = 'sci';
    Level = 'proc';
    Version = 1;
    FileType = 'fits';


    if Args.makeRefs
        Logger.msgLog(LogLevel.Info, 'Creating references.');

        % Get Ref FFI Paths and verify they exist
        FFIRefPaths = dir(fullfile(Args.FFIRefDataPath, "*.fits"));

        if isempty(FFIRefPaths)
            Logger.msgLog(LogLevel.Error, 'No reference FFIs found in %s', Args.FFIRefDataPath);
        end
    
        NRefFFIs = numel(FFIRefPaths);

        for IRefFFI = 1:NRefFFIs
            FFIRefPath = fullfile(FFIRefPaths(IRefFFI).folder, FFIRefPaths(IRefFFI).name);

            Logger.msgLog(LogLevel.Info, '>>> Processing reference %s (%i/%i)', FFIRefPath, IRefFFI, NRefFFIs);
        
            try
                RefFFI = pipeline.tess.reduction.loadreadyFFI(FFIRefPath);
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
    
            BaseQuality = pipeline.tess.quality.checkBaseQuality(RefFFI, 'Logger', Logger);
    
            if ~BaseQuality
                Logger.msgLog(LogLevel.Info, 'FFI fails base quality check, skipping processing.')
                continue
            end
    
            Logger.msgLog(LogLevel.Info, 'Creating sub-images');
            
            try
                RefFFIs = pipeline.tess.reduction.FFI2calibSubimages(RefFFI);
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
    
            ObsDate = RefFFI.HeaderData.getVal('DATE-OBS');
            CamID = RefFFI.HeaderData.getVal('CAMERA');
            CCDID = RefFFI.HeaderData.getVal('CCD');
            Sector = RefFFI.HeaderData.getVal('Sector');
    
            DateTime = datetime(ObsDate,"InputFormat","yyyy-MM-dd'T'HH:mm:ss.SSS");
            Time =  convertStringsToChars(string(DateTime,'yyyyMMdd.HHmmss.SSS'));
   
            if ~exist(Args.RefPath, 'dir')
               mkdir(Args.RefPath)
            end
                    
            ProjName = strcat('TESS.',sprintf('%02.0f', CamID),'.',sprintf('%02.0f', CCDID));
    
            Logger.msgLog(LogLevel.Info, 'Saving reference sub-image products to %s', Args.RefPath);
           
            NSubFFIs = numel(RefFFIs);
    
            for ISubFFI = 1:NSubFFIs
            
                CropID  = ISubFFI;
                RefFFIs(ISubFFI).HeaderData.insertKey({'CropID', CropID});
                
                Saturated = RefFFIs(ISubFFI).ImageData.Image > 100000;
                
                RefFFIs(ISubFFI) = RefFFIs(ISubFFI).maskSet(Saturated, ...
                    'Saturated', true, 'CreateNewObj',false);
    
                for ISaveProducts=1:4
                    ISaveProd = Args.SaveRefProducts{ISaveProducts};
                    ISaveProdFilename = strcat(ProjName,'_',Time,'_',Filter,'_', ...
                        num2str(Sector,'%04.f'),'_', '000','_', ...
                        num2str(Counter,'%03.f'),'_', ...
                        num2str(CropID,'%03.f'),'_', Type,'_', Level,'_', ISaveProd, '_', ...
                        int2str(Version), '.',FileType);
                    ISaveProdFilename = strcat(Args.RefPath,'/',ISaveProdFilename);
                    RefFFIs(ISubFFI).write1(ISaveProdFilename, ISaveProd, ...
                        'OverWrite', true, 'WriteHeader', true);                
               end
            end
        end

        Logger.msgLog(LogLevel.Info, 'Reference images created.')
    end
    
    % Get FFI Paths and verify they exist
    FFIPaths = dir(fullfile(FFIDataPath, "*.fits"));

    if isempty(FFIPaths)
        Logger.msgLog(LogLevel.Error, 'No FFIs found in %s', FFIDataPath);
    end

    NFFIs = numel(FFIPaths);

    Logger.msgLog(LogLevel.Info, 'Found %i FFIs fits files', NFFIs);

    NumSaveProd = numel(Args.SaveProducts);

    % Some unit conversion parameters
    Rad2Arcsec = 3600.*180./pi; %206265;
    Arcsec2Rad = 1./Rad2Arcsec; %4.84814e-6;

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

        Logger.msgLog(LogLevel.Info, 'Creating sub-images');
        
        try
            FFIs = pipeline.tess.reduction.FFI2calibSubimages(FFI);
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

        ObsDate = FFI.HeaderData.getVal('DATE-OBS');
        CamID = FFI.HeaderData.getVal('CAMERA');
        CCDID = FFI.HeaderData.getVal('CCD');
        Sector = FFI.HeaderData.getVal('Sector');

        DateTime = datetime(ObsDate,"InputFormat","yyyy-MM-dd'T'HH:mm:ss.SSS");
        Time =  convertStringsToChars(string(DateTime,'yyyyMMdd.HHmmss.SSS'));

        SaveVisitPath = strcat(SavePath, '/', Time);

        if ~exist(SaveVisitPath, 'dir')
           mkdir(SaveVisitPath)
        end
                
        ProjName = strcat('TESS.',sprintf('%02.0f', CamID),'.',sprintf('%02.0f', CCDID));

        Logger.msgLog(LogLevel.Info, 'Saving sub-image products to %s', SaveVisitPath);
       
        NSubFFIs = numel(FFIs);

        for ISubFFI = 1:NSubFFIs
        
            CropID  = ISubFFI;
            FFIs(ISubFFI).HeaderData.insertKey({'CropID', CropID});

            Saturated = FFIs(ISubFFI).ImageData.Image > 100000;

            FFIs(ISubFFI) = FFIs(ISubFFI).maskSet(Saturated, ...
                'Saturated', true, 'CreateNewObj',false);

            for ISaveProducts=1:NumSaveProd
                ISaveProd = Args.SaveProducts{ISaveProducts};
                ISaveProdFilename = strcat(ProjName,'_',Time,'_',Filter,'_', ...
                    num2str(Sector,'%04.f'),'_', '000','_', ...
                    num2str(Counter,'%03.f'),'_', ...
                    num2str(CropID,'%03.f'),'_', Type,'_', Level,'_', ISaveProd, '_', ...
                    int2str(Version), '.',FileType);
                ISaveProdFilename = strcat(SaveVisitPath,'/',ISaveProdFilename);
                FFIs(ISubFFI).write1(ISaveProdFilename, ISaveProd, ...
                    'OverWrite', true, 'WriteHeader', true);                
           end
        end

        if ~Args.runSubtraction
            Logger.msgLog(LogLevel.Info, '<<< FFI processed.');
            continue
        end

        Logger.msgLog(LogLevel.Info, 'Finding references.');

        for ISubFFI = NSubFFIs:-1:1
            CropID  = ISubFFI;
            RefFilename = strcat(ProjName,'_*.*.*_',Filter,'_', ...
                    num2str(Sector,'%04.f'),'_', '000','_', ...
                    num2str(Counter,'%03.f'),'_', ...
                    num2str(CropID,'%03.f'),'_', Type,'_proc_Image_', ...
                    int2str(Version), '.',FileType);
            RefFilename = strcat(Args.RefPath,'/',RefFilename);

            % Load Ref image as AstroImage and Ref image FileName object
            Ref = AstroImage.readFileNamesObj(RefFilename, 'Path', Args.RefPath);

            if Ref.isemptyImage()
                continue
            end

            AD(ISubFFI) = AstroZOGY(FFIs(ISubFFI), Ref);

        end

        AD.register;
        % Estimate backround and variance of New and Ref
        AD.estimateBackVar;
        % Estimate zero points
        AD.estimateFnFr;
        
        Logger.msgLog(LogLevel.Info, 'Performing subtraction.');
        % Create proper subtraction image D
        AD.subtractionD;
        % Derive Gabor stat image
        AD.matchfilterGabor;
        % Derive S stat image
        AD.subtractionS;
        % Derive Scorr stat image
        AD.subtractionScorr();
        % Derive Z2 stat image
        AD.translient('PrecompKxKySize',[744, 744]);
        
        % 7: ----- Find and process transients -----

        Logger.msgLog(LogLevel.Info, 'Finding transient candidates.');
        
        % Find transients
        AD.findTransients('includePsfFit', false, 'includeAperturePhot', false, ...
            'include2ndMoment', false, 'includeGradientDir', false);

        % Measure transients
        AD.measureTransients('applyDSDFcorrection',false);
        
        % Flag non transients
        AD.flagNonTransients('ConfigFile', Args.FilterConfigFile);
        
        % Get cutouts only for transients
        ADn = AD.removeNonTransients;
        
        for Iobj=NSubFFIs:-1:1
            NumTran = size(ADn(Iobj).CatData.Catalog,1);
            OnesArray = ones(NumTran,1);

            Sector_Array = ADn(Iobj).HeaderData.getVal('Sector')*OnesArray;
            CamID_Array = ADn(Iobj).HeaderData.getVal('CAMERA')*OnesArray;
            CCDID_Array = ADn(Iobj).HeaderData.getVal('CCD')*OnesArray;
            CropID_Array = ADn(Iobj).HeaderData.getVal('CropID')*OnesArray;
    
            ADn(Iobj).CatData.insertCol(...
                cell2mat({...
                    cast(Sector_Array,'double'), cast(CamID_Array,'double'), ...
                    cast(CCDID_Array,'double'), cast(CropID_Array,'double')}), ...
                'SCORE',...
                {'Sector','CAM','CCD','CropID'}, ...
                {'','','',''});
        
            TranCat(Iobj) = ADn(Iobj).CatData;
        end

        MergedTranCat = merge(TranCat);
        MergedTranCat.sortrows('Dec');

        % Save merged catalog
        if Args.saveMergedCat && MergedTranCat.sizeCatalog > 0
                MergedCatFilename = strcat(ProjName,'_',Time,'_',Filter,'_', ...
                    num2str(Sector,'%04.f'),'_', '000','_', ...
                    num2str(Counter,'%03.f'),'_000_', ...
                    Type,'_proc.zogyD_Cat_', ...
                    int2str(Version), '.',FileType);

                Logger.msgLog(LogLevel.Info, 'Saving catalog %s.', MergedCatFilename);


                MergedCatFN = FileNames.generateFromFileName({MergedCatFilename});
                MergedCatFN.FullPath = SaveVisitPath;
                
                [~,~,~] = imProc.io.writeProduct(MergedTranCat, MergedCatFN, ...
                    'Level', 'coadd.zogyD', 'Product', {'Cat'},...
                    'WriteHeader',false,'Overwrite', true, 'GetHeaderJD', false, ...
                    'CropID_FromIndex',false);
        end

        Logger.msgLog(LogLevel.Info, '<<< FFI processed.');
    end
end
