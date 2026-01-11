function pipelineI(RawImageList, CI, Args)
    %
    % Example: D.loadCalib();
    %          pipeline.last.pipes.pipelineI([],D.CI);

    arguments
        RawImageList                       = [];
        CI                                 = [];   
        Args.UseParfor                     = true;
        Args.Nworkers                      = 16;
        Args.TempName                      = 'LAST*.fit*';
        Args.prePrepArgs                   = {};
        Args.basicCalibArgs                = {};

        % Sub image partitioning
        Args.SubSizeXY                     = [1716 1716]; % tested using: RR=imUtil.filter.fft_size_timing([Size Size],false,10000);
        Args.EdgesCCDSEC                   = [];
        Args.NoOverlapCCDSEC               = [];
        Args.ListCenters                   = [];
        Args.NewNoOverlap                  = [];


        Args.image2subimagesArgs           = {};
        Args.multiIterExtractorArgs        = {};
        Args.astrometryVisitSubImageArgs   = {};
        Args.forcedPhotArgs                = {};
        Args.matchExternal_Indiv           = true;
        Args.matchExternalArgs_Indiv       = {};

        

        Args.ForcedPhotCat               = 'WDEDR3';
        Args.CornersRA                   = {'RA1','RA2','RA3','RA4'};
        Args.CornersDec                  = {'DEC1','DEC2','DEC3','DEC4'};
        Args.MinNstars                   = 50;
        Args.MaxFracGrad                 = 0.2;

        Args.Logger                      = [];
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;

    if isempty(RawImageList)
        % mainly for debuging/manual purposses - read file from current dir:
        Files = dir(Args.TempName);
        Nfile = numel(Files);
        if Nfile==0 || Nfile>40
            % critical error [in manula mode] - stop
            error('Number of files (%d) is too small or too larege',Nfile);
        end
        RawImageList = {Files.name};
    end

    

    Nepoch = numel(RawImageList);
    % load images and check quality
    % AI putput is of size [Nimages x 1]
    [AI, TableForDB, TableHeader] = pipeline.generic.prePrep(RawImageList, Args.prePrepArgs{:});  %5.9s

    % basic calibration (bias, flat,...) 
    AI = pipeline.generic.basicCalib(AI, CI, Args.basicCalibArgs{:}); %17.1s

    

    % break images into sub images
    % 1st dim is epoch; 2nd dim is sub image
    % include sub image partitioning
    if isempty(Args.EdgesCCDSEC)
        SizeXY = fliplr(size(AI(1).ImageData.Data));
        [Args.EdgesCCDSEC, ~, Args.NoOverlapCCDSEC, Args.NewNoOverlap, Args.ListCenters] = imUtil.cut.gridSubImage(SizeXY, Args.SubSizeXY);
    end
    AllSI=imProc.image.images2subImages(AI, 'SubSizeXY',Args.SubSizeXY, 'EdgesCCDSEC',Args.EdgesCCDSEC, 'ListCenters',Args.ListCenters, 'NoOverlapCCDSEC',Args.NoOverlapCCDSEC, 'NewNoOverlap',Args.NewNoOverlap);  % 8.9s
    
    [Nepoch, Nsub] = size(AllSI);
    Nobj = numel(AllSI);

    % get JD of all epoch - once
    JD = AI.julday;
    JD = repmat(JD(:), 1, Nsub); % faster than getting the JD for AllSI

    % measure background, PSF, search for stars in all images
    if isempty(Args.UseParfor)
        PP = gcp('nocreate');
        if isempty(PP)
            PP = parpool(Args.Nworkers);
        end
        [AllSI] = imProc.sources.multiIterExtractor(AllSI, Args.multiIterExtractorArgs{:},...
                                                    'JD',JD,...
                                                    'AddSkyCoo',false);  % 513 s
       
    else
        tic;
        parfor Iobj=1:1:Nobj
            [AllSI(Iobj)] = imProc.sources.multiIterExtractor(AllSI(Iobj), Args.multiIterExtractorArgs{:},...
                                                    'JD',JD(Iobj),...
                                                    'AddSkyCoo',false);  % 193 s
        end
        toc
    end


    % solve astrometry of all images
    [ResFit, AllSI, CatName] = imProc.astrometry.astrometryVisitSubImage(AllSI, Args.astrometryVisitSubImageArgs{:}); % 24s

    % add coordinates to catalogs
    AllSI = imProc.astrometry.addCoordinates2catalog(AllSI, 'UpdateCoo',true);
    
    
    % Individual sub images : quality           
    % astrometry
    IsGoodWCS = imProc.astrometry.isSuccessWCS(AllSI);  % 1.3 s
    % Nstars
    Nstars    = AllSI.sizeCatalog;
    % background variations
    MeanBack     = imProc.stat.mean(AllSI);
    MeanVar      = imProc.stat.mean(AllSI);
    MeanMeanBack = mean(MeanBack, 1); % mean background over all sub images in each epoch
    MaxFracGrad  = (max(MeanBack,[],1) - min(MeanBack,[],1))./MeanMeanBack; % max fractional background gradient per epoch

    IsGood = IsGoodWCS & Nstars>Args.MinNstars & MaxFracGrad<Args.MaxFracGrad;

    % write stat data to header: Nstars, PSF, Scale, Rotation,...
    % background, var: written as part of the background estimation
    AllSI = imProc.header.writeStat2Header(AllSI, 'WriteBack',false);  % 4.2s

    % forced photometry
    % forced photometry on pre-selected targets
    if ~isempty(Args.ForcedPhotCat)
        tic;
        MidEpoch = ceil(Nepoch.*0.5);
        CatForcedPhot = imProc.cat.catsHTM_inImage(Args.ForcedPhotCat, AllSI(MidEpoch,:));  % 0.2

        AllFP = AstroCatalog([Nepoch, Nsub]);
        for Isub=1:1:Nsub
            % for each sub image - run over all epochs
            Coo = CatForcedPhot(Isub).getCol({'RA','Dec'}).*RAD;
            AllFP(:,Isub) = imProc.sources.forcedPhot(AllSI(:,Isub), 'OutType','AstroCatalog', 'Coo',Coo, 'Moving',false, 'AddRefStarsDist',0, Args.forcedPhotArgs{:});  % 10 s [for all in loop]
        end
        toc
        AFP = AllFP(:).merge; % 0.05s
    end

    % match external
    if Args.matchExternal_Indiv
        % current default is true - do we want this?
        AllSI = pipeline.generic.matchExternal(AllSI, Args.matchExternalArgs_Indiv{:});
    end
    
    % Merge catalogs
    MS = pipeline.generic.proc2MatchedSources(AllSI, 'FlagGood',IsGood);

    % Calculate drift between epochs
    % Note that MS is already filerted! I.e., some epochs may not
    % be included
    [GlobalMotion, ShiftInfo] = lcUtil.positionDrift(MS);

    % The following logic is applied:
    % MatchedSources and photometric calibration is done only after
    % the photometric calibration of the coadd image
    % Such that we can use the photometric calibration of the coadd
    % for the individual images.
    
    % coadd images
    
    Coadd = pipeline.generic.procCoadd(AllSI, 'ShiftXY',ShiftInfo,...
                                              'PropShiftXY','ShiftXY',...
                                              'IShiftXYfiltered',true);

   
    % coadd: search stars
    [Coadd] = imProc.sources.multiIterExtractor(Coadd, Args.multiIterExtractorArgs{:},...
                                                    'AddSkyCoo',true);

    % coadd: solve astrometry
    imProc.astrometry.astrometryAllSubImage

    % coadd: match external
    pipeline.generic.matchExternal

    % MISSING - write drifts to header

    

    % save products
    imProc.io.saveProductImage
    imProc.io.saveProductMatchedSources

    
    % coadd: photometric calibration

    % save products
    imProc.io.saveProductImage

    % write status
    

end
