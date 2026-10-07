function [AD, ADc, TranCat, Status] = TESSwideFFI(FFI, Args)
    %{
    Runs the wide-field TESS pipeline on a single FFI: loads it, applies the
    base quality check, splits it into calibrated sub-images (tiles),
    subtracts the matching reference tiles with AstroZOGY, derives the
    statistics images, and finds, measures and flags transient candidates.
    This is the per-FFI step of pipeline.tess.TESSwidepipe; it can also be
    run interactively, e.g. to inspect the results with displayTransients.

    Input   : - FFI. Path to a TESS FFI FITS file, or an AstroImage returned
                by pipeline.tess.reduction.loadreadyFFI.
              * ...,key,val,...
                'RefPath' - Directory with the reference tiles. Default is ''.
                'CropIDs' - Tiles (crop IDs) to process. If empty, process
                       all tiles. Default is [].
                'runSubtraction' - Bool on whether to subtract the reference
                       tiles and search for transients. If false, only the
                       tiles are made (and saved). Default is true.
                'FilterConfigFile' - Path to a JSON configuration file used by
                       AD.flagNonTransients to reject non-transient candidates.
                       Its flagPSFShape also sets whether subtractionS builds
                       the PSF-residual template. Default is
                       config/FilterParameters.TransientsFilter.TESS.json.
                'SavePath' - Directory in which a per-FFI visit directory
                       (named by DATE-OBS) is created for the products. If
                       empty, nothing is written. Default is ''.
                'SaveProducts' - Cell array of tile products to write with
                       AstroImage.write1 when SavePath is set. Default is {}.
                'saveMergedCat' - Bool on whether to write the merged
                       transient catalog to the visit directory (when SavePath
                       is set and the catalog is not empty). Default is false.
                'Logger' - MsgLogger object for progress messages. If empty,
                       no messages are logged. Default is [].

    Output  : - AD. AstroZOGY array, one element per processed tile, with the
                subtraction products and the flagged candidates in CatData.
                Empty if the FFI fails the quality check or no tile has a
                reference.
              - ADc. AstroZOGY cutouts around the candidates that pass all
                filters (only computed when requested).
              - TranCat. AstroCatalog of the candidates that pass all
                filters, merged over the tiles, with Sector, CAM, CCD and
                CropID columns, sorted by Dec.
              - Status. Message on how the processing ended.

    Author  : Ruslan Konno (Jan 2026)
    Example : [AD, ADc, TranCat, Status] = pipeline.tess.TESSwideFFI(...
                  '/path/to/FFIs/tess2018213055942-s0001-3-1-0120-s_ffic.fits', ...
                  'RefPath', '/path/to/ref', 'CropIDs', 5);
              AD(1).displayTransients('OtherImages', {'S'})
    %}

    arguments
        FFI

        Args.RefPath = '';
        Args.CropIDs = [];
        Args.runSubtraction logical = true;
        Args.FilterConfigFile = fullfile(Configuration.getSysConfigPath, ...
            'FilterParameters.TransientsFilter.TESS.json');

        Args.SavePath = '';
        Args.SaveProducts = {};
        Args.saveMergedCat logical = false;

        Args.Logger = [];
    end

    AD = AstroZOGY.empty;
    ADc = AstroZOGY.empty;
    TranCat = AstroCatalog;

    Filter = 'clear';
    Counter = 1;
    Type = 'sci';
    Level = 'proc';
    Version = 1;
    FileType = 'fits';

    if ischar(FFI) || isstring(FFI)
        logMsg(Args.Logger, LogLevel.Info, '>>> Processing %s', FFI);
        FFI = pipeline.tess.reduction.loadreadyFFI(FFI);
    end

    if ~pipeline.tess.quality.checkBaseQuality(FFI, 'Logger', Args.Logger)
        Status = 'FFI fails base quality check, skipping processing.';
        logMsg(Args.Logger, LogLevel.Info, Status);
        return
    end

    logMsg(Args.Logger, LogLevel.Info, 'Creating sub-images');

    FFIs = pipeline.tess.reduction.FFI2calibSubimages(FFI);

    ObsDate = FFI.HeaderData.getVal('DATE-OBS');
    CamID = FFI.HeaderData.getVal('CAMERA');
    CCDID = FFI.HeaderData.getVal('CCD');
    Sector = FFI.HeaderData.getVal('Sector');

    DateTime = datetime(ObsDate,"InputFormat","yyyy-MM-dd'T'HH:mm:ss.SSS");
    Time =  convertStringsToChars(string(DateTime,'yyyyMMdd.HHmmss.SSS'));

    ProjName = strcat('TESS.',sprintf('%02.0f', CamID),'.',sprintf('%02.0f', CCDID));

    CropIDs = 1:numel(FFIs);
    if ~isempty(Args.CropIDs)
        CropIDs = intersect(CropIDs, Args.CropIDs);
    end

    for CropID = CropIDs
        FFIs(CropID).HeaderData.insertKey({'CropID', CropID});

        Saturated = FFIs(CropID).ImageData.Image > 100000;

        FFIs(CropID) = FFIs(CropID).maskSet(Saturated, ...
            'Saturated', true, 'CreateNewObj',false);
    end

    SaveVisitPath = '';
    if ~isempty(Args.SavePath)
        SaveVisitPath = strcat(Args.SavePath, '/', Time);

        if ~exist(SaveVisitPath, 'dir')
           mkdir(SaveVisitPath)
        end
    end

    if ~isempty(SaveVisitPath) && ~isempty(Args.SaveProducts)
        logMsg(Args.Logger, LogLevel.Info, 'Saving sub-image products to %s', SaveVisitPath);

        for CropID = CropIDs
            for ISaveProducts=1:numel(Args.SaveProducts)
                ISaveProd = Args.SaveProducts{ISaveProducts};
                ISaveProdFilename = strcat(ProjName,'_',Time,'_',Filter,'_', ...
                    num2str(Sector,'%04.f'),'_', '000','_', ...
                    num2str(Counter,'%03.f'),'_', ...
                    num2str(CropID,'%03.f'),'_', Type,'_', Level,'_', ISaveProd, '_', ...
                    int2str(Version), '.',FileType);
                ISaveProdFilename = strcat(SaveVisitPath,'/',ISaveProdFilename);
                FFIs(CropID).write1(ISaveProdFilename, ISaveProd, ...
                    'OverWrite', true, 'WriteHeader', true);
            end
        end
    end

    if ~Args.runSubtraction
        Status = 'Sub-images created, no subtraction requested.';
        logMsg(Args.Logger, LogLevel.Info, '<<< FFI processed.');
        return
    end

    logMsg(Args.Logger, LogLevel.Info, 'Finding references.');

    ADcell = {};
    for CropID = CropIDs
        RefFilename = strcat(ProjName,'_*.*.*_',Filter,'_', ...
                num2str(Sector,'%04.f'),'_', '000','_', ...
                num2str(Counter,'%03.f'),'_', ...
                num2str(CropID,'%03.f'),'_', Type,'_proc_Image_', ...
                int2str(Version), '.',FileType);
        RefFilename = strcat(Args.RefPath,'/',RefFilename);

        % Load Ref image as AstroImage and Ref image FileName object
        Ref = AstroImage.readFileNamesObj(RefFilename, 'Path', Args.RefPath);

        if Ref.isemptyImage()
            logMsg(Args.Logger, LogLevel.Info, 'No reference for crop %d.', CropID);
            continue
        end

        ADcell{end+1} = AstroZOGY(FFIs(CropID), Ref); %#ok<AGROW>
    end

    if isempty(ADcell)
        Status = 'No reference tiles found.';
        logMsg(Args.Logger, LogLevel.Error, Status);
        return
    end
    AD = [ADcell{:}];

    AD.register;

    % Background noise per pixel of New and Ref (e-/s): sky photons plus
    % read noise.
    for Iobj = 1:numel(AD)
        AD(Iobj).New.Var = tessPixelVar(AD(Iobj).New, AD(Iobj).New.Back);
        if isempty(AD(Iobj).Ref.Back)
            AD(Iobj).Ref.Back = repmat(AD(Iobj).Ref.HeaderData.getVal('MEDBCK'), size(AD(Iobj).Ref.Image));
        end
        AD(Iobj).Ref.Var = tessPixelVar(AD(Iobj).Ref, AD(Iobj).Ref.Back);
    end

    % Estimate backround and variance of New and Ref
    AD.estimateBackVar;
    % Estimate zero points
    AD.estimateFnFr;

    % The PSF-residual template is only used by the flagPSFShape filter, and
    % needs RA/Dec and PSF photometry in the Ref catalogue, which TESS
    % reference tiles do not carry. Build it only if the filter is on
    % (flagNonTransients defaults to on when the config does not set it).
    PopPSFresid = true;
    if isfile(Args.FilterConfigFile)
        FilterConfig = jsondecode(fileread(Args.FilterConfigFile));
        if isfield(FilterConfig, 'flagPSFShape')
            PopPSFresid = logical(FilterConfig.flagPSFShape);
        end
    end

    logMsg(Args.Logger, LogLevel.Info, 'Performing subtraction.');
    % Create proper subtraction image D
    AD.subtractionD;
    % Derive Gabor stat image
    AD.matchfilterGabor;
    % Derive S stat image
    AD.subtractionS('PopS_PSFresid', PopPSFresid);
    % Derive Scorr stat image. TESS images are in e-/s, so the source
    % variance is image/t: pass the exposure times as Ncoadd in Scorr's
    % image/Ncoadd source term.
    ExpNew = FFI.HeaderData.getVal('EXPOSURE')*86400;
    ExpRef = AD(1).Ref.HeaderData.getVal('EXPOSURE')*86400;
    AD.subtractionScorr('ExpTimeNewArr', {0,0}, 'ExpTimeRefArr', {0,0}, ...
        'NcoaddNew', ExpNew, 'NcoaddRef', ExpRef);
    % Derive Z2 stat image
    AD.translient('PrecompKxKySize',[744, 744]);

    logMsg(Args.Logger, LogLevel.Info, 'Finding transient candidates.');

    % Find transients
    AD.findTransients('includePsfFit', false, 'includeAperturePhot', false, ...
        'include2ndMoments', false);

    % Measure transients
    AD.measureTransients('applyDSDFcorrection',false);

    % Flag non transients
    AD.flagNonTransients('ConfigFile', Args.FilterConfigFile);

    % Keep only the candidates that pass all filters
    ADn = AD.removeNonTransients;

    for Iobj = numel(ADn):-1:1
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

        TranCats(Iobj) = ADn(Iobj).CatData;
    end

    TranCat = merge(TranCats);
    TranCat.sortrows('Dec');

    if nargout > 1
        ADc = ADn.cutoutTransients;
    end

    % Save merged catalog
    if Args.saveMergedCat && ~isempty(SaveVisitPath) && TranCat.sizeCatalog > 0
        MergedCatFilename = strcat(ProjName,'_',Time,'_',Filter,'_', ...
            num2str(Sector,'%04.f'),'_', '000','_', ...
            num2str(Counter,'%03.f'),'_000_', ...
            Type,'_proc.zogyD_Cat_', ...
            int2str(Version), '.',FileType);

        logMsg(Args.Logger, LogLevel.Info, 'Saving catalog %s.', MergedCatFilename);

        MergedCatFN = FileNames.generateFromFileName({MergedCatFilename});
        MergedCatFN.FullPath = SaveVisitPath;

        [~,~,~] = imProc.io.writeProduct(TranCat, MergedCatFN, ...
            'Level', 'coadd.zogyD', 'Product', {'Cat'},...
            'WriteHeader',false,'Overwrite', true, 'GetHeaderJD', false, ...
            'CropID_FromIndex',false);
    end

    Status = sprintf('FFI processed, %d transient candidate(s) pass all filters.', TranCat.sizeCatalog);
    logMsg(Args.Logger, LogLevel.Info, '<<< FFI processed.');
end

function logMsg(Logger, Level, varargin)
    % Log through Logger if one is given.
    if ~isempty(Logger)
        Logger.msgLog(Level, varargin{:});
    end
end

function Var = tessPixelVar(AI, Back)
    % Per-pixel noise variance of a TESS FFI tile in (e-/s)^2: sky photons
    % Back/t plus read noise NREADOUT*RN^2/t^2, with RN of the CCD output
    % (512 columns each) of every column, located through the tile CCDSEC.
    H  = AI.HeaderData;
    t  = H.getVal('EXPOSURE')*86400;
    NR = H.getVal('NREADOUT');
    RN = [H.getVal('READNOIA') H.getVal('READNOIB') H.getVal('READNOIC') H.getVal('READNOID')];
    CCDSEC = H.getVal('CCDSEC');
    if ischar(CCDSEC) || isstring(CCDSEC)
        CCDSEC = str2num(CCDSEC); %#ok<ST2NM>
    end
    [Ny, Nx] = size(AI.Image);
    XFFI   = CCDSEC(1) - 1 + (1:Nx);
    Output = min(4, max(1, ceil(XFFI./512)));
    Var = double(Back)./t + repmat(NR.*RN(Output).^2./t.^2, Ny, 1);
end
