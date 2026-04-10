function [TableRaw, AllSI, MS, Coadd, OnlyMP] = pipelineI(RawImageList, CI, Args)
    %
    % Example: D.loadCalib();
    %          [AllSI, MS, Coadd, OnlyMP]=pipeline.last.pipes.pipelineI([],D.CI);

    arguments
        RawImageList                       = [];
        CI                                 = [];   
        Args.UseParfor                     = true;
        Args.Nworkers                      = 16;
        Args.TempName                      = 'LAST*.fit*';
        Args.prePrepArgs                   = {}; % e.g., {'AstroImageReadArgs',{'UseMex', true}};
        Args.basicCalibArgs                = {};
        Args.KeyMidJD                      = 'MIDJD';

        % Sub image partitioning
        Args.SubSizeXY                     = [1716 1716]; % tested using: RR=imUtil.filter.fft_size_timing([Size Size],false,10000);
        Args.EdgesCCDSEC                   = [];
        Args.NoOverlapCCDSEC               = [];
        Args.ListCenters                   = [];
        Args.NewNoOverlap                  = [];

        Args.ColCell                       = {'XPEAK','YPEAK',...
                                              'X1', 'Y1',...
                                              'X2','Y2','XY',...
                                              'SN','BACK_IM','VAR_IM',...
                                              'BACK_ANNULUS', 'STD_ANNULUS', ...
                                              'FLUX_APER', 'FLUXERR_APER',...
                                              'MAG_APER', 'MAGERR_APER',...
                                              'FLUX_XYPEAK'};
        Args.image2subimagesArgs           = {};
        Args.multiIterExtractorArgs        = {}; %{'psfFitPhotArgs',{'Method','exp'}};
        Args.astrometryVisitSubImageArgs   = {};
        Args.forcedPhotArgs                = {};
        Args.matchExternal_Indiv           = true;
        Args.matchExternalArgs_Indiv       = {};
        Args.procCoaddArgs                 = {};
        Args.StackMethod                   = 'wrobust';  % 'sigmaclip';
        Args.coadd_WRobustArgs             = {};
        Args.generateImageIDArgs           = {};
        Args.fitPhotCalibTransArgs         = {};

        Args.ForcedPhotCat               = 'WDEDR3';  % UPDATE
        Args.CornersRA                   = {'RA1','RA2','RA3','RA4'};
        Args.CornersDec                  = {'DEC1','DEC2','DEC3','DEC4'};
        Args.MinNstars                   = 50;
        Args.MaxFracGrad                 = 0.2;

        Args.AddMergedCat                = true;
        Args.AddKnownAst                 = true;
        Args.GeoPos                      = [];
        Args.OrbEl                       = [];
        Args.INPOP                       = celestial.INPOP.init;
        Args.AsteroidSearchRadius        = 10;

        Args.KeysGlobalMotion            = {'GM_RATEX', 'GM_STDX', 'GM_RATEY', 'GM_STDY'};

        Args.Header_addAirMassArgs       = {};
        Args.Cat_addAirMassArgs          = {};
        Args.AddSrcAM                    = true;
        

        Args.Logger                      = [];
        %Args.Sa

        % 
        Args.UseMex                      = true;
        
        Args.DBobj                       = [];
        Args.DB_Table_Raw                = [];
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;

    %ProcessingStep = 11;

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

    %ProcessingStep = 21;
    Nepoch = numel(RawImageList);
    % load images and check quality
    % AI putput is of size [Nimages x 1]
    [AI, TableForDB, TableHeader, JD_AI] = pipeline.generic.prePrep(RawImageList, Args.prePrepArgs{:});  %5.9s
    TableRaw = [TableHeader, TableForDB];
    %
    if ~isempty(Args.DBobj)
        Args.DBobj.insert(Args.DB_Table_Raw,TableRaw); 
    end
    % basic calibration (bias, flat,...) 
    % FixJD false, since already done in prePrep
    %ProcessingStep = 31;
    AI = pipeline.generic.basicCalib(AI, CI, Args.basicCalibArgs{:}, 'UpdateJD',false); %31.2s
    
    %ProcessingStep = 41;
    % Add MIDJD to header % 0.03s
    Nepoch = numel(AI);
    for Iepoch=1:1:Nepoch
        AI(Iepoch).HeaderData.insertKey({Args.KeyMidJD, JD_AI(Iepoch)});
    end

    % break images into sub images
    % 1st dim is epoch; 2nd dim is sub image
    % include sub image partitioning
    %ProcessingStep = 51;
    if isempty(Args.EdgesCCDSEC)
        SizeXY = fliplr(size(AI(1).ImageData.Data));
        [Args.EdgesCCDSEC, ~, Args.NoOverlapCCDSEC, Args.NewNoOverlap, Args.ListCenters] = imUtil.cut.gridSubImage(SizeXY, Args.SubSizeXY);  % 0.01s
    end
    % No WCS/PSF/Cat so no need to update them
    %ProcessingStep = 61;
    AllSI=imProc.image.images2subImages(AI, 'SubSizeXY',Args.SubSizeXY, 'EdgesCCDSEC',Args.EdgesCCDSEC, 'ListCenters',Args.ListCenters, 'NoOverlapCCDSEC',Args.NoOverlapCCDSEC, 'NewNoOverlap',Args.NewNoOverlap,...
                                            'UpdateWCS',false, 'UpdatePSF',false, 'UpdateCat',false, 'UpdateXY',false);  % 6.6s
    [Nepoch, Nsub] = size(AllSI);
    Nobj = numel(AllSI);

    
    % get JD of all epoch - once
    %ProcessingStep = 71;
    JD = repmat(JD_AI(:), 1, Nsub); % faster than getting the JD for AllSI


    % initiate parpool if needed
    %ProcessingStep = 81;
    PP = [];
    if Args.UseParfor
        PP = gcp('nocreate');
        if isempty(PP)
            % no parpool exist
            % create new parpool
            PP = parpool(Args.Nworkers);
        end
    else
        PP = [];
    end

    % Add ImageID to individual cropped images: in ID_PROC
    %ProcessingStep = 91;
    [AllSI, ID_Coadd] = imProc.db.generateImageID(AllSI, 'JD',JD, Args.generateImageIDArgs{:}); % 0.5 s
   
    
    % measure background, PSF, search for stars in all images
    if isempty(PP)
        %ProcessingStep = 101;
        [AllSI] = imProc.sources.multiIterExtractor(AllSI, Args.multiIterExtractorArgs{:},...
                                                    'JD',JD,...
                                                    'ColCell',Args.ColCell,...
                                                    'UseMex',Args.UseMex,...
                                                    'AddSkyCoo',false);  % 466 s (with UseMex=false)
       
    else
        %ProcessingStep = 102;
        %tic;
        parfor (Iobj=1:1:Nobj, 0)
            [AllSI(Iobj)] = imProc.sources.multiIterExtractor(AllSI(Iobj), Args.multiIterExtractorArgs{:},...
                                                    'JD',JD(Iobj),...
                                                    'ColCell',Args.ColCell,...
                                                    'UseMex',Args.UseMex,...
                                                    'AddSkyCoo',false);  % 119 s (on 16 cores): 169s -> 135s (with UseMex=true)
        end
        %toc
    end
        

    % solve astrometry of all images
    %ProcessingStep = 301;
    [ResFit, AllSI, CatName] = imProc.astrometry.astrometryVisitSubImage(AllSI, Args.astrometryVisitSubImageArgs{:}); % 22s
    % add coordinates to catalogs
    %ProcessingStep = 401;
    AllSI = imProc.astrometry.addCoordinates2catalog(AllSI, 'UpdateCoo',true, 'OutUnits','deg');  % 0.8s
    
    % add PSF FWHM to header - after astrometry, beacuse WCS is needed
    %ProcessingStep = 201;
    % This must be done after astrometry as the Scale is used
    AllSI = imProc.psf.fwhm(AllSI, 'AddMorphology',true, 'UseLegacy',false);
        
    
    % Update Airmass header keyword to based on measured crop center
    %ProcessingStep = 411;
    [AllSI, AllImagesAirMass] = imProc.header.addAirMass(AllSI, 'JD',JD, 'HealpixType','nested', Args.Header_addAirMassArgs{:}); % 0.3s

    % Individual sub images : quality           
    % astrometry
    %ProcessingStep = 421;
    IsGoodWCS = imProc.astrometry.isSuccessWCS(AllSI);  % 1.3 s
    % Nstars
    Nstars    = AllSI.sizeCatalog;
    % background variations
    MeanBack     = imProc.stat.mean(AllSI);
    %MeanVar      = imProc.stat.mean(AllSI);
    MeanMeanBack = mean(MeanBack, 2); % mean background over all sub images in each epoch
    MaxFracGrad  = (max(MeanBack,[],2) - min(MeanBack,[],2))./MeanMeanBack; % max fractional background gradient per epoch

    IsGood = IsGoodWCS & Nstars>Args.MinNstars & MaxFracGrad<Args.MaxFracGrad;

    % Photometric calibration of individual images:
    %[Result, PC, FitRes] = imProc.calib.fitPhotCalibTrans(AllSI);



    % write stat data to header: Nstars, PSF, Scale, Rotation,...
    % background, var: written as part of the background estimation
    %ProcessingStep = 431;
    AllSI = imProc.header.writeStat2Header(AllSI, 'WriteBack',false);  % 4.2s

    % ADD - IsGoodWCS, IsGood, MaxFracGrad to image header


    % forced photometry
    % forced photometry on pre-selected targets
    %ProcessingStep = 441;
    if ~isempty(Args.ForcedPhotCat)
        %tic;
        MidEpoch = ceil(Nepoch.*0.5);
        CatForcedPhot = imProc.cat.catsHTM_inImage(Args.ForcedPhotCat, AllSI(MidEpoch,:));  % 0.2
        
        ColNamesFF = AllSI(1).CatData.ColNames;

        %AllFP = AstroCatalog([Nepoch, Nsub]);
        for Isub=1:1:Nsub
            % for each sub image - run over all epochs
            Coo = CatForcedPhot(Isub).getCol({'RA','Dec'}).*RAD;
            %if strcmpi(Args.OutputType, 'concatai')
                AllSI(:,Isub) = imProc.sources.forcedPhotNew(AllSI(:,Isub), 'OutputType','ConcatAI', 'Coo',Coo, 'Moving',false, 'AddRefStarsDist',0, 'CatIsUniform',true, 'ColCell',ColNamesFF, 'ReadColFromHeader',false, Args.forcedPhotArgs{:});  % 8.3 s [for all in loop]
            %else
            %    error('Currently, adding forced phot is supported only using the ConcatAI option');
            %end
            %for Iepoch=1:1:Nepoch
            %    AllSI(Iepoch,Isub).CatData.insertCol(AllFP)
            %end

            % need to add CropID to catalog
            % XXX?
        end
        %toc

        % Merge AllFP into AllSI catalog
        % XXX?

        % mege into a single catalog:
        %AllForcedPhot = AllFP(:).merge; % 0.05s
    else
        %AllForcedPhot = [];
    end

    % Sort all catalogs by Dec
    %ProcessingStep = 451;
    for Iim=1:1:Nsub.*Nepoch
        AllSI(Iim).CatData.sortrows('Dec');  % 0.16s (for all in loop)
    end

    % match external / too expensive
    %if Args.matchExternal_Indiv
    %    % current default is true - do we want this?
    %    AllSI = pipeline.generic.matchExternal(AllSI, Args.matchExternalArgs_Indiv{:});
    %end
    %AllSI = imProc.match.match_catsHTMmerged(AllSI); % 240 s

    % Merge catalogs
    %ProcessingStep = 501;
    MS = pipeline.generic.proc2MatchedSources(AllSI, 'FlagGood',IsGood, 'DimEpoch',1);   % 9.6 s



    % Calculate drift between epochs
    % Note that MS is already filerted! I.e., some epochs may not
    % be included
    %ProcessingStep = 601;
    [GlobalMotion, ShiftInfo] = lcUtil.positionDrift(MS);
    
    %ProcessingStep = 701;
    %tic;
    for Isub=1:1:Nsub
        [AllSI(:,Isub), ZP] = imProc.calib.photometricZP(AllSI(:,Isub), 'CatName',CatName(Isub));  % 10s for all in loop
        %[Coadd(Isub), ZP]   = imProc.calib.photometricZP(Coadd(Isub), 'CatName',CatName(Isub));  % 2.4s for all in loop
    end
    %toc

    % The following logic is applied:
    % MatchedSources and photometric calibration is done only after
    % the photometric calibration of the coadd image
    % Such that we can use the photometric calibration of the coadd
    % for the individual images.
   
    % coadd images 
    % Only coaddition: 56 s 
    % only multiIterationPSF: 35 s (UseMex=false)
    % coadd+multiIterPSF+astrometry+PhotCalibSimple : 95 s (UseMex=false)
    % (93 s with parfor)
    
    % 64 s
    %tic;
    % Phot calib is done later (after adding airmass columns):
    %ProcessingStep = 801;
    [Coadd, ResCoadd] = pipeline.generic.procCoadd(AllSI, Args.procCoaddArgs{:},...
                                              'SubBack',false,...
                                              'CatName',CatName,...
                                              'ShiftXY',ShiftInfo,...
                                              'IsGood',IsGood,...
                                              'PropShiftXY','ShiftXY',...
                                              'IsShiftXYfiltered',true,...
                                              'StackMethod',Args.StackMethod,...
                                              'UseMex',Args.UseMex,...
                                              'PhotCalibSimple',false,...
                                              'PhotCalibTrans',false,...
                                              Args.multiIterExtractorArgs{:});
  
    %toc

    % 96 s
    % tic;
    % parfor Isub=1:1:Nsub
    %     [Coadd(Isub), ResCoadd(Isub)] = pipeline.generic.procCoadd(AllSI(:,Isub), Args.procCoaddArgs{:},...
    %                                           'SubBack',false,...
    %                                           'CatName',CatName,...
    %                                           'ShiftXY',ShiftInfo,...
    %                                           'IsGood',IsGood,...
    %                                           'PropShiftXY','ShiftXY',...
    %                                           'IsShiftXYfiltered',true,...
    %                                           'StackMethod',Args.StackMethod,...
    %                                           'UseMex',Args.UseMex,...
    %                                           'PhotCalibSimple',false,...
    %                                           'PhotCalibTrans',false);
    % end
    % toc


    % Add image ID to coadd images: in: ID_PROC
    %ProcessingStep = 901;
    JD_Coadd = [ResCoadd.MidMidJD];
    [Coadd, ID_Coadd] = imProc.db.generateImageID(Coadd, 'KeyID','ID_COADD', 'JD',JD_Coadd, Args.generateImageIDArgs{:});  % 0.05 s

    % Update Airmass (And UPIX) header keyword to based on measured crop center
    %ProcessingStep = 911;
    [Coadd, AllCoaddAirMass] = imProc.header.addAirMass(Coadd, 'JD',JD_Coadd, 'HealpixType','nested', Args.Header_addAirMassArgs{:}); % 0.3s

    % Add catsHTM MergedCat column to Coadd catalogs
    %ProcessingStep = 921;
    if Args.AddMergedCat
        %tic;
        if isempty(Args.UseParfor)
            Coadd = imProc.match.match_catsHTMmerged(Coadd, 'SameField',false, 'CreateNewObj',false);  % 23 s
        else
            PP = gcp('nocreate');
            if isempty(PP)
                PP = parpool(Args.Nworkers);
            end
            %tic;
            parfor Isub=1:1:Nsub
                Coadd(Isub) = imProc.match.match_catsHTMmerged(Coadd(Isub), 'SameField',false, 'CreateNewObj',false);  % 8 s
            end
            %toc
        end
    end

    %tic;
    %ProcessingStep = 931;
    if Args.AddKnownAst
        % slower with parfor
        [OnlyMP,~,Coadd] = imProc.match.match2solarSystem(Coadd, 'JD',[], 'GeoPos',Args.GeoPos, 'OrbEl',Args.OrbEl, 'SearchRadius',Args.AsteroidSearchRadius, 'INPOP',Args.INPOP);  % 7 s
        Nast = OnlyMP.sizeCatalog;
        if sum(Nast)>0
            % add CropID, Node, Mount, Cam, ID_COADD:
            Cols = {'NODENUMB', 'MOUNTNUM', 'CAMNUM', 'ID_COADD'};
            StKey = Coadd(1).getStructKey(Cols);
            Vals  = struct2array(StKey);
            AllCols = {'CROPID', Cols{:}};
            for Isub=1:1:Nsub
                if Nast(Isub)>0
                    OnlyMP(Isub).insertCol([Isub, Vals], Inf, AllCols);
                end
            end
            OnlyMP = OnlyMP.merge('IsTable',true);
        else
            OnlyMP = AstroCatalog;
        end
    else
        OnlyMP = AstroCatalog;
    end
    %toc
    

    % write drifts to header
    %ProcessingStep = 941;
    for Isub=1:1:Nsub      
        DataGM = [Args.KeysGlobalMotion(:), num2cell([GlobalMotion(Isub).RateX; GlobalMotion(Isub).StdX; GlobalMotion(Isub).RateY; GlobalMotion(Isub).StdY])];
        Coadd(Isub).HeaderData.insertKey(DataGM,'end');
    end
    
    %ProcessingStep = 951;
    %tic;
    if Args.AddSrcAM
        AllSI = imProc.cat.addAirMass(AllSI, 'JD',JD, Args.Cat_addAirMassArgs{:});
        Coadd = imProc.cat.addAirMass(Coadd, 'JD',JD, Args.Cat_addAirMassArgs{:});
    end
    %toc


    % photometric calibration
    %ProcessingStep = 961;
    %tic;
    for Isub=1:1:Nsub
        %[AllSI(:,Isub), ZP] = imProc.calib.photometricZP(AllSI(:,Isub), 'CatName',CatName(Isub));  % 10s for all in loop
        [Coadd(Isub), ZP]   = imProc.calib.photometricZP(Coadd(Isub), 'CatName',CatName(Isub));  % 2.4s for all in loop
    end
    %toc

    % Coadd images
    % Photometric calibration of coadd images:
    %ProcessingStep = 971;
    %tic;
    [Coadd, PC, FitRes] = imProc.calib.fitPhotCalibTrans(Coadd, Args.fitPhotCalibTransArgs{:}, 'Verbose',false, 'AddMagErr', false); % 8.7s for all in loop
    %toc


    % proapage photometric calibration to individual images

    % propagae photometric calibration to MatchedSources

    % save products
    %imProc.io.saveProductImage
    %imProc.io.saveProductMatchedSources

    
    
    % save products
    %imProc.io.saveProductImage

    % write status
    
    % Finish
    %ProcessingStep = 1000;
end
