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
          directory and write them to Args.RefPath; per sector/camera/CCD,
          the reference is the FFI within Args.RefIntervals that passes the
          base quality check with the lowest median background (ties: the
          latest),
      (6) optionally perform tile-by-tile image subtraction with the matching
          reference tiles using AstroZOGY, derive statistics images, find and
          measure transient candidates, flag non-transients using a filter
          configuration file, and optionally write a merged transient catalog.
    
    Steps (2)-(6) for a single FFI are done by pipeline.tess.TESSwideFFI,
    which can also be called directly to get the AstroZOGY objects back.
    
    Logging is written to Args.LogFile using MsgLogger. The pipeline is designed
    to continue to the next FFI if a given file fails to load or to process,
    while recording errors and stack traces in the log.
    
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
                'FFIRefDataPath' - Path to directory containing candidate
                       reference FFIs (matched by "*.fits") used when makeRefs
                       is true; one is selected per sector/camera/CCD.
                       Default is ''.
                'RefIntervals' - Collection of [start end] time intervals in
                       which a reference FFI may be selected. A bound is a JD
                       or a UTC datetime string (yyyy-MM-dd'T'HH:mm:ss[.SSS],
                       yyyy-MM-dd HH:mm:ss or yyyy-MM-dd); an empty bound
                       (or NaN, or JSON null) leaves that side open. Given as
                       an N x 2 numeric array, an N x 2 cell array, or nested
                       pairs as decoded from JSON. A candidate is eligible if
                       its mid-exposure time is within any interval. If the
                       collection is empty, all candidates are eligible.
                       Default is [].
                'SaveRefProducts' - Cell array of product names to write for
                       reference tiles using AstroImage.write1. The Var of a
                       reference tile is its physical per-pixel noise
                       (pipeline.tess.reduction.tessPixelVar). Default is
                       {'Image','Mask','Cat','PSF','Back','Var'}.
                'RefPath' - Output directory for reference tile products when
                       makeRefs is true, and also the directory searched for
                       reference tiles when runSubtraction is true. Default
                       is ''.
                'runSubtraction' - Bool on whether to perform AstroZOGY image
                       subtraction of each science tile against a corresponding
                       reference tile located under Args.RefPath. Default is false.
                'FilterConfigFile' - Path to a JSON configuration file used by
                       AD.flagNonTransients to reject non-transient candidates.
                       Its flagPSFShape also sets whether subtractionS builds
                       the PSF-residual template. Default is
                       config/FilterParameters.TransientsFilter.TESS.json.
                'saveMergedCat' - Bool on whether to save the per-FFI merged
                       transient catalog (after filtering) to the visit directory.
                       The merged catalog is written only if it is non-empty.
                       Default is false.
    Output  : - None. All products are written to disk and messages are written
                to Args.LogFile.
    Author  : Ruslan Konno (Jan 2026)
    Example : % Run wide pipeline with reference creation + subtraction + merged catalog:
              FFIDataPath = '/path/to/target/FFIs';
              SavePath    = '/path/to/target/proc';
    
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
        Args.SaveRefProducts = {'Image','Mask','Cat','PSF','Back','Var'};
        
        Args.FFIRefDataPath = '';
        Args.RefIntervals = [];

        Args.FilterConfigFile = fullfile(Configuration.getSysConfigPath, ...
            'FilterParameters.TransientsFilter.TESS.json');

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
    
        RefFFIPaths = selectReferenceFFIs(FFIRefPaths, intervalsJD(Args.RefIntervals), Logger);
        NRefFFIs = numel(RefFFIPaths);

        for IRefFFI = 1:NRefFFIs
            FFIRefPath = RefFFIPaths{IRefFFI};

            Logger.msgLog(LogLevel.Info, '>>> Processing reference %s (%i/%i)', FFIRefPath, IRefFFI, NRefFFIs);
        
            try
                RefFFI = pipeline.tess.reduction.loadreadyFFI(FFIRefPath);
            catch ME
                Logger.msgLog(LogLevel.Error, 'Failure opening FFI');
                logTraceback(Logger, ME);
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

                RefFFIs(ISubFFI).Var = pipeline.tess.reduction.tessPixelVar(RefFFIs(ISubFFI), RefFFIs(ISubFFI).Back);
    
                for ISaveProducts=1:numel(Args.SaveRefProducts)
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

    for IFFI = 1:NFFIs

        FFIPath = fullfile(FFIPaths(IFFI).folder, FFIPaths(IFFI).name);
        Logger.msgLog(LogLevel.Info, '>>> Processing %s (%i/%i)', FFIPath, IFFI, NFFIs);
    
        try
            FFI = pipeline.tess.reduction.loadreadyFFI(FFIPath);
        catch ME
            Logger.msgLog(LogLevel.Error, 'Failure opening FFI');
            logTraceback(Logger, ME);
            continue
        end

        try
            pipeline.tess.TESSwideFFI(FFI, 'RefPath', Args.RefPath, ...
                'runSubtraction', Args.runSubtraction, ...
                'FilterConfigFile', Args.FilterConfigFile, ...
                'SavePath', SavePath, 'SaveProducts', Args.SaveProducts, ...
                'saveMergedCat', Args.saveMergedCat, 'Logger', Logger);
        catch ME
            Logger.msgLog(LogLevel.Error, 'Failure processing FFI');
            logTraceback(Logger, ME);
            continue
        end
    end
end

function logTraceback(Logger, ME)
    % Log an error message and its stack.
    Logger.msgLog(LogLevel.Error, ME.message);
    Logger.msgLog(LogLevel.Error, 'Traceback: ');
    for k = 1:numel(ME.stack)
        Logger.msgLog(LogLevel.Error, "%s (line %d)", ME.stack(k).name, ME.stack(k).line);
    end
end

function Selected = selectReferenceFFIs(Files, Intervals, Logger)
    % Choose one reference FFI per sector/camera/CCD among the candidate FFIs
    % in Files (dir output): of those within the time Intervals (N x 2 JD;
    % empty: no constraint) that pass the base quality check, the one with
    % the lowest median background (least scattered light); ties go to the
    % latest. Returns the full paths of the selected FFIs.
    N = numel(Files);
    Path = cell(N,1);  Key = strings(N,1);  Pass = false(N,1);  Back = nan(N,1);  JD = nan(N,1);
    InWin = true(N,1);
    for I = 1:N
        Path{I} = fullfile(Files(I).folder, Files(I).name);
        try
            FFI = pipeline.tess.reduction.loadreadyFFI(Path{I});
        catch ME
            Logger.msgLog(LogLevel.Error, 'Failure opening reference candidate %s', Path{I});
            logTraceback(Logger, ME);
            continue
        end
        Key(I)  = sprintf('sector %d camera %d CCD %d', FFI.HeaderData.getVal('Sector'), ...
            FFI.HeaderData.getVal('CAMERA'), FFI.HeaderData.getVal('CCD'));
        JD(I)   = midExposureJD(FFI);
        if ~isempty(Intervals)
            InWin(I) = any(JD(I) >= Intervals(:,1) & JD(I) <= Intervals(:,2));
        end
        Pass(I) = pipeline.tess.quality.checkBaseQuality(FFI, 'Logger', Logger);
        Back(I) = median(FFI.Image(:), 'omitnan');
        Logger.msgLog(LogLevel.Info, 'Reference candidate %s: %s, JD %.5f, in permitted interval %d, base quality %d, median background %.2f e-/s', ...
            Files(I).name, Key(I), JD(I), InWin(I), Pass(I), Back(I));
    end

    Selected = {};
    for K = unique(Key(Key ~= ""))'
        Cand = find(Key == K & InWin & Pass);
        if isempty(Cand)
            Logger.msgLog(LogLevel.Error, 'No reference candidate for %s is within the permitted intervals and passes the base quality check (%d candidates, %d within the intervals).', ...
                K, nnz(Key == K), nnz(Key == K & InWin));
            continue
        end
        [~, Order] = sortrows([Back(Cand), -JD(Cand)]);
        Best = Cand(Order(1));
        Logger.msgLog(LogLevel.Info, 'Selected reference for %s: %s (median background %.2f e-/s, %d of %d candidates pass)', ...
            K, Files(Best).name, Back(Best), numel(Cand), nnz(Key == K));
        Selected{end+1} = Path{Best}; %#ok<AGROW>
    end
end

function JD = midExposureJD(FFI)
    % Mid-exposure JD of a TESS FFI from DATE-OBS and DATE-END.
    Fmt = "yyyy-MM-dd'T'HH:mm:ss.SSS";
    T0 = datetime(FFI.HeaderData.getVal('DATE-OBS'), 'InputFormat', Fmt, 'TimeZone', 'UTC');
    T1 = datetime(FFI.HeaderData.getVal('DATE-END'), 'InputFormat', Fmt, 'TimeZone', 'UTC');
    JD = juliandate(T0 + (T1 - T0)/2);
end

function JD = intervalsJD(Intervals)
    % Collection of [start end] intervals as an N x 2 array of JD, with
    % -Inf/Inf for open bounds. Accepts an N x 2 numeric array (NaN: open),
    % an N x 2 cell array, or nested pairs as decoded from JSON; bounds are
    % JD numbers, UTC datetime strings, or empty (open).
    if isempty(Intervals)
        JD = [];
        return
    end
    if isnumeric(Intervals)
        if isvector(Intervals)
            Intervals = reshape(Intervals, 1, []);
        end
        Pairs = num2cell(Intervals);
    elseif iscell(Intervals) && all(cellfun(@(P) numel(P) == 2 && (iscell(P) || isnumeric(P)), Intervals(:)))
        % nested pairs, e.g. from JSON: each a 2-element cell or numeric pair
        Pairs = cellfun(@(P) reshape(pairCell(P), 1, []), Intervals(:), 'UniformOutput', false);
        Pairs = vertcat(Pairs{:});
    elseif iscell(Intervals) && isvector(Intervals)
        Pairs = reshape(Intervals, 1, []);
    else
        Pairs = Intervals;
    end
    if size(Pairs, 2) ~= 2
        error('TESSwidepipe:RefIntervals', 'RefIntervals must be a collection of [start end] pairs.');
    end
    JD = [cellfun(@(B) boundJD(B, -Inf), Pairs(:,1)), cellfun(@(B) boundJD(B, Inf), Pairs(:,2))];
end

function P = pairCell(P)
    % A [start end] pair as a cell.
    if isnumeric(P)
        P = num2cell(P);
    end
end

function JD = boundJD(B, Open)
    % One interval bound as JD; Open (-Inf or Inf) if empty or NaN.
    if isempty(B) || (isnumeric(B) && isnan(B)) || (isstring(B) && strlength(B) == 0)
        JD = Open;
    elseif isnumeric(B)
        JD = double(B);
    else
        JD = NaN;
        for Fmt = ["yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd'T'HH:mm:ss", "yyyy-MM-dd HH:mm:ss", "yyyy-MM-dd"]
            try
                JD = juliandate(datetime(char(B), 'InputFormat', Fmt, 'TimeZone', 'UTC'));
                break
            catch
            end
        end
        if isnan(JD)
            error('TESSwidepipe:RefIntervals', 'Cannot parse the RefIntervals bound "%s".', char(B));
        end
    end
end
