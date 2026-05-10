function [Status, TableRaw, AllSI, MS, Coadd, OnlyMP, JD] = pipelineI(RawImageList, CI, Args)
    %
    % Example: D.loadCalib();
    %          [AllSI, MS, Coadd, OnlyMP]=pipeline.last.pipes.pipelineI([],D.CI);

    arguments
        RawImageList                       = [];
        CI                                 = [];   
        Args.DefScale                      = 1.25;  % Default scale if WCS is empty
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

        Args.backVarArgs                   = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[128 128], 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}}; % both for single epoch and coadd

        Args.ColCell                       = {'XPEAK','YPEAK',...
                                              'X1', 'Y1',...
                                              'X2','Y2','XY',...
                                              'SN','BACK_IM','VAR_IM',...
                                              'BACK_ANNULUS', 'STD_ANNULUS', ...
                                              'FLUX_APER', 'FLUXERR_APER',...
                                              'MAG_APER', 'MAGERR_APER',...
                                              'FLUX_XYPEAK', 'FORCED'};
        Args.AperRadius                    = [2, 4, 6];
        Args.Annulus                       = [10 12];
        Args.MomentsMethod                 = 'mex';  %'legacy'|'mex'
        Args.AperPhotMethod                = 'interp';  % 'simple'|'interp'

        Args.ShiftMethod                   = 'lanczos3';  % 'fft'|'lanczos3'
        Args.PsfPhotMethod                 = '2DGN';    % 'legacy'/'old' |'1D'|'2D'|'2DGN'

        Args.image2subimagesArgs           = {};
        Args.multiIterExtractorArgs        = {}; %{'psfFitPhotArgs',{'Method','exp'}};
        Args.maskCR_Args                   = {'RemoveFromCat',true}; % <-- remove CR
        Args.astrometryVisitSubImageArgs   = {};
        Args.forcedPhotArgs                = {};
        %--- pipeline.generic.proc2MatchedSources args ---
        Args.proc2MatchedSourcesArgs       = {};
        Args.ColUse                        = 'FORCED';
        Args.AddUnUse                      = true;

        Args.matchExternal_Indiv           = true;
        Args.matchExternalArgs_Indiv       = {};
        Args.procCoaddArgs                 = {};
        Args.StackMethod                   = 'wrobust';  % 'sigmaclip';
        Args.coadd_WRobustArgs             = {};
        Args.generateImageIDArgs           = {};
        Args.fitPhotCalibTransArgs         = {};
        
        Args.photometricZPArgs             = {};

        Args.ForcedPhotCat               = 'WDEDR3';  % UPDATE
        Args.CornersRA                   = {'RA1','RA2','RA3','RA4'};
        Args.CornersDec                  = {'DEC1','DEC2','DEC3','DEC4'};
        Args.MinNstars                   = 50;
        Args.MaxFracGrad                 = 1.0;

        Args.AddMergedCat                = true;
        Args.AddKnownAst                 = true;
        Args.GeoPos                      = [];
        Args.OrbEl                       = [];
        Args.INPOP                       = celestial.INPOP.init;
        Args.AsteroidSearchRadius        = 10;

        Args.KeysGlobalMotion            = {'GM_RATEX', 'GM_STDX', 'GM_RATEY', 'GM_STDY'};
        Args.KeyRelPhotRMS               = {'RP_MRMS','RP_MMRMS'};
        Args.KeyIDProc                   = {'ID_PROCF','ID_PROCL'}; % ID of the first and last proc images that were used to create the visit - will be written into the Coadd header

        Args.Header_addAirMassArgs       = {};
        Args.Cat_addAirMassArgs          = {};
        Args.AddSrcAM                    = true;
        

        Args.Logger                      = [];
        %Args.Sa

        % 
        Args.UseMex                      = true;
        
        Args.DBobj                       = [];
        Args.DB_Table_Raw                = [];

        Args.MatchMethod                 = 'mex'; % 'old'|'mex'
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;

    Status.PipeI   = true;
    Status.ME      = [];
    %ProcessingStep = 11;

    if isempty(RawImageList)
        % mainly for debuging/manual purposses - read file from current dir:
        Files = dir(Args.TempName);
        Nfile = numel(Files);
        if Nfile==0 || Nfile>40
            % critical error [in manual mode] - stop
            error('Number of files (%d) is too small or too larege',Nfile);
        end
        RawImageList = {Files.name};
    end

    %ProcessingStep = 21;
    Nepoch = numel(RawImageList);

    % load images and check quality
    % AI putput is of size [Nimages x 1]
    try
        [AI, TableForDB, TableHeader, JD_AI, FlagGoodImages] = pipeline.generic.prePrep(RawImageList, Args.prePrepArgs{:});  %5.9s
        % Note that AI may be shorter than TableRaw
        % It contains only: TableRaw.SelectedImages

        TableRaw = [TableHeader, TableForDB]; 
        TableRaw.PrepPrepOK = true(size(TableRaw,1), 1);
        RawImageList = RawImageList(FlagGoodImages,:);
    catch ME
        Status.PipeI   = false;
        Status.ME      = ME;
        TableRaw.FileName   = string(RawImageList(:));
        TableRaw.PrepPrepOK = false(numel(RawImageList), 1);
        TableRaw.Exception  = true(numel(RawImageList), 1); % Exception in this stage will have PrePrepOK = false

        AllSI    = [];
        MS       = [];
        Coadd    = [];
        OnlyMP   = [];
        JD       = [];
    end

    if Status.PipeI
        try
            
            % basic calibration (bias, flat,...) 
            % FixJD false, since already done in prePrep
            %ProcessingStep = 31;
            AI = pipeline.generic.basicCalib(AI, CI, Args.basicCalibArgs{:}, 'UpdateJD',false); %31.2s

            TableRaw.BasicCalib(TableRaw.SelectedImages) = true(numel(AI),1);  % basic calib success
        
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
            [AllSI, ~, ID_Epoch_Str] = imProc.db.generateImageID(AllSI, 'JD',JD, Args.generateImageIDArgs{:}); % 0.5 s
           
            
            % measure background, PSF, search for stars in all images
            if isempty(PP)
                %ProcessingStep = 101;
                [AllSI] = imProc.sources.multiIterExtractor(AllSI, Args.multiIterExtractorArgs{:},...
                                                            'JD',JD,...
                                                            'ColCell',Args.ColCell,...
                                                            'UseMex',Args.UseMex,...
                                                            'backVarArgs',Args.backVarArgs,...
                                                            'AperRadius',Args.AperRadius,...
                                                            'Annulus',Args.Annulus,...
                                                            'MomentsMethod',Args.MomentsMethod,...
                                                            'ShiftMethod',Args.ShiftMethod,...
                                                            'PsfPhotMethod',Args.PsfPhotMethod,...
                                                            'maskCR_Args',Args.maskCR_Args,...
                                                            'AddSkyCoo',false);  % 466 s (with UseMex=false)
               
            else
                %ProcessingStep = 102;
                %tic;
                % parfor (Iobj=1:1:Nobj, 0)  % no par for!
                parfor Iobj=1:1:Nobj
                    [AllSI(Iobj)] = imProc.sources.multiIterExtractor(AllSI(Iobj), Args.multiIterExtractorArgs{:},...
                                                            'JD',JD(Iobj),...
                                                            'ColCell',Args.ColCell,...
                                                            'UseMex',Args.UseMex,...
                                                            'backVarArgs',Args.backVarArgs,...
                                                            'AperRadius',Args.AperRadius,...
                                                            'Annulus',Args.Annulus,...
                                                            'MomentsMethod',Args.MomentsMethod,...
                                                            'ShiftMethod',Args.ShiftMethod,...
                                                            'PsfPhotMethod',Args.PsfPhotMethod,...
                                                            'maskCR_Args',Args.maskCR_Args,...
                                                            'AddSkyCoo',false);  % 119 s (on 16 cores): 169s -> 135s (with UseMex=true)
                end
                %toc
            end
        
            % Consider update TableRaw - No PSF, etc? 
            %TableRaw.BasicCalib(TableRaw.SelectedImages) = true(numel(AI),1); 
        
            % solve astrometry of all images
            %ProcessingStep = 301;
            [ResFit, AllSI, CatName] = imProc.astrometry.astrometryVisitSubImage(AllSI, 'MatchMethod',Args.MatchMethod, Args.astrometryVisitSubImageArgs{:}); % 22s
        
            % add coordinates to catalogs
            %ProcessingStep = 401;
            AllSI = imProc.astrometry.addCoordinates2catalog(AllSI, 'UpdateCoo',true, 'OutUnits','deg');  % 0.8s
            
            % add PSF FWHM to header - after astrometry, beacuse WCS is needed
            %ProcessingStep = 201;
            % This must be done after astrometry as the Scale is used
            AllSI = imProc.psf.fwhm(AllSI, 'AddMorphology',true, 'UseLegacy',false, 'DefScale',Args.DefScale);
                
            
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
            TableRaw.MaxFracGrad(TableRaw.SelectedImages) = MaxFracGrad;

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
                % select only images with good astrometry+ (IsGood)
                IsForcedPhot = IsGood(MidEpoch,:);

                if any(IsForcedPhot)
                    CatForcedPhot = imProc.cat.catsHTM_inImage(Args.ForcedPhotCat, AllSI(MidEpoch,IsForcedPhot));  % 0.2
                    
                    ColNamesFF = AllSI(1).CatData.ColNames;
            
                    %AllFP = AstroCatalog([Nepoch, Nsub]);
                    for Isub=1:1:Nsub
                        % for each sub image - run over all epochs
                        if IsForcedPhot(Isub)
                            IsubGood = find(find(IsForcedPhot)==Isub);
                            Coo = CatForcedPhot(IsubGood).getCol({'RA','Dec'}).*RAD;
                            %if strcmpi(Args.OutputType, 'concatai')
        
                            if ~isempty(Coo)
                                IsGoodEpoch = IsGood(:,Isub);
                                AllSI(IsGoodEpoch,Isub) = imProc.sources.forcedPhotNew(AllSI(IsGoodEpoch,Isub), 'OutputType','ConcatAI', 'Coo',Coo, 'Moving',false, 'AddRefStarsDist',0, 'CatIsUniform',true, 'ColCell',ColNamesFF, 'ReadColFromHeader',false, 'PsfPhotMethod',Args.PsfPhotMethod, 'ShiftMethod',Args.ShiftMethod, Args.forcedPhotArgs{:});  % 8.3 s [for all in loop]
                            end
                           
                        end
                    end % for Isub=1:1:Nsub
                    %toc
                end
        
            else
                %AllForcedPhot = [];
            end
        
            % Sort all catalogs by Dec
            %ProcessingStep = 451;
            for Iim=1:1:Nsub.*Nepoch
                AllSI(Iim).CatData.sortrows('Dec');  % 0.16s (for all in loop)
            end
        
            if Args.AddSrcAM
                AllSI = imProc.cat.addAirMass(AllSI, 'JD',JD, 'IsGood',IsGood, Args.Cat_addAirMassArgs{:});
            end

            % match external / too expensive
            %if Args.matchExternal_Indiv
            %    % current default is true - do we want this?
            %    AllSI = pipeline.generic.matchExternal(AllSI, Args.matchExternalArgs_Indiv{:});
            %end
            %AllSI = imProc.match.match_catsHTMmerged(AllSI); % 240 s
        
            %ProcessingStep = 701;
            %tic;
            for Isub=1:1:Nsub
                [AllSI(:,Isub), ZP] = imProc.calib.photometricZP(AllSI(:,Isub), 'CatName',CatName(Isub), ...
                    'UpdateMagCols',false, Args.photometricZPArgs{:});  % 10s for all in loop
                %[Coadd(Isub), ZP]   = imProc.calib.photometricZP(Coadd(Isub), 'CatName',CatName(Isub));  % 2.4s for all in loop
            end
            %toc

            % Merge catalogs
            %ProcessingStep = 501;
            [MS,ResRelZP] = pipeline.generic.proc2MatchedSources(AllSI, Args.proc2MatchedSourcesArgs{:}, 'FlagGood',IsGood, 'DimEpoch',1, 'ColUse',Args.ColUse, 'AddUnUse',Args.AddUnUse);   % 9.6 s -> 1.3s (with MatchMethod='unify')
        
            % calculate the photometric rms per crop
            
            PhotRMS = MS.calcRMS('FieldX','MAG_APER_3');
            Phot_MinRMS    = [PhotRMS.MinRMS];
            Phot_MagMinRMS = [PhotRMS.MagMinRMS];
            

            % Calculate drift between epochs
            % Note that MS is already filerted! I.e., some epochs may not
            % be included
            %ProcessingStep = 601;
            [GlobalMotion, ShiftInfo] = lcUtil.positionDrift(MS);
            
            
        
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
            % If there is not ShiftInfo, the no poinmt of coadding images
            [Coadd, ResCoadd] = pipeline.generic.procCoadd(AllSI, Args.procCoaddArgs{:},...
                                                          'DefScale',Args.DefScale,...
                                                          'SubBack',false,...
                                                          'SetBackTo0',false,...
                                                          'ReMeasureBackVar',true,...
                                                          'CatName',CatName,...
                                                          'ShiftXY',ShiftInfo,...
                                                          'IsGood',IsGood,...
                                                          'PropShiftXY','ShiftXY',...
                                                          'IsShiftXYfiltered',true,...
                                                          'StackMethod',Args.StackMethod,...
                                                          'UseMex',Args.UseMex,...
                                                          'PhotCalibSimple',false,...
                                                          'PhotCalibTrans',false,...
                                                          'MatchMethod',Args.MatchMethod,...
                                                          'backVarArgs',Args.backVarArgs,...
                                                          'AperRadius',Args.AperRadius,...
                                                          'Annulus',Args.Annulus,...
                                                          'MomentsMethod',Args.MomentsMethod,...
                                                          'AperPhotMethod',Args.AperPhotMethod,...
                                                          'ShiftMethod',Args.ShiftMethod,...
                                                          'PsfPhotMethod',Args.PsfPhotMethod,...
                                                          'maskCR_Args',Args.maskCR_Args,...
                                                          'WriteStatHeader',true,...
                                                          Args.multiIterExtractorArgs{:});
            
              
            %toc

            
        
            % Add image ID to coadd images: in: ID_PROC
            NotIsEmptyCoadd = ~Coadd.isemptyImage;
            JD_Coadd = [ResCoadd(NotIsEmptyCoadd).MidMidJD];
            %Ncoadd   = numel(Coadd);
            %CoaddID  = nan(Ncoadd,1);
            %[Coadd(NotIsEmptyCoadd), CoaddID(NotIsEmptyCoadd)]
            [Coadd(NotIsEmptyCoadd)] = imProc.db.generateImageID(Coadd(NotIsEmptyCoadd), 'KeyID','ID_COADD', 'JD',JD_Coadd, Args.generateImageIDArgs{:});  % 0.05 s
        
    

            % Update header keywords:
            % Airmass, UPIX, relative photometry of epochs
            % header keyword to based on measured crop center
            AnyCoaddExist = any(NotIsEmptyCoadd);
            if AnyCoaddExist
                % Add airmass + UPIX to header
                [Coadd(NotIsEmptyCoadd)] = imProc.header.addAirMass(Coadd(NotIsEmptyCoadd), 'JD',JD_Coadd, 'HealpixType','nested', Args.Header_addAirMassArgs{:}); % 0.3s


                % Add relphot rms to Coadd header:
                HCell = [Args.KeyRelPhotRMS(:); Args.KeyIDProc(:)];
                for Isub=1:1:Nsub
                    if NotIsEmptyCoadd(Isub)
                        % Add ID of first and last proc images
                        ID_Str = ID_Epoch_Str(IsGood(:,Isub), Isub);
                        
                        % Add relphot rms to Coadd header:
                        [HCell(1:4,2)] = {Phot_MinRMS(Isub); Phot_MagMinRMS(Isub); char(ID_Str(1)); char(ID_Str(end)) }';
                        %HCell(:,2) = num2cell([Phot_MinRMS(Isub); Phot_MagMinRMS(Isub); char(ID_Str(1)); char(ID_Str(end))]);
                        Coadd(Isub).HeaderData.insertKey(HCell,'end-1');
                    end
                end

            end

            % Add catsHTM MergedCat column to Coadd catalogs
            if Args.AddMergedCat && AnyCoaddExist
                %tic;
                if isempty(Args.UseParfor)
                    Coadd(NotIsEmptyCoadd) = imProc.match.match_catsHTMmerged(Coadd(NotIsEmptyCoadd), 'SameField',false, 'CreateNewObj',false);  % 23 s
                else
                    PP = gcp('nocreate');
                    if isempty(PP)
                        PP = parpool(Args.Nworkers);
                    end
                    %tic;
                    parfor Isub=1:1:Nsub
                        if NotIsEmptyCoadd(Isub)
                            Coadd(Isub) = imProc.match.match_catsHTMmerged(Coadd(Isub), 'SameField',false, 'CreateNewObj',false);  % 8 s
                        end
                    end
                    %toc
                end
            end
        
            %tic;
            %ProcessingStep = 931;
            if Args.AddKnownAst && AnyCoaddExist
                % slower with parfor
                [OnlyMP,~,Coadd(NotIsEmptyCoadd)] = imProc.match.match2solarSystem(Coadd(NotIsEmptyCoadd), 'JD',[], 'GeoPos',Args.GeoPos, 'OrbEl',Args.OrbEl, 'SearchRadius',Args.AsteroidSearchRadius, 'INPOP',Args.INPOP);  % 7 s

                Nast = OnlyMP.sizeCatalog;
                if sum(Nast)>0
                    % add CropID, Node, Mount, Cam, ID_COADD:
                    Cols = {'NODENUMB', 'MOUNTNUM', 'CAMNUM', 'ID_COADD'};
                    StKey = Coadd(1).getStructKey(Cols);
                    Vals  = struct2array(StKey);
                    AllCols = {'CROPID', Cols{:}};
                    for Isub=1:1:Nsub
                        if Nast(Isub)>0
                            Nrow = size(OnlyMP(Isub).Catalog,1);
                            OnlyMP(Isub).insertMultiCol(repmat([Isub, Vals], Nrow,1), AllCols, repmat({''},1, numel(AllCols)));
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
                if NotIsEmptyCoadd(Isub)
                    DataGM = [Args.KeysGlobalMotion(:), num2cell([GlobalMotion(Isub).RateX; GlobalMotion(Isub).StdX; GlobalMotion(Isub).RateY; GlobalMotion(Isub).StdY])];
                    Coadd(Isub).HeaderData.insertKey(DataGM,'end');
                end
            end
            
            %ProcessingStep = 951;
            %tic;
            if Args.AddSrcAM
                Coadd(NotIsEmptyCoadd) = imProc.cat.addAirMass(Coadd(NotIsEmptyCoadd), 'JD',JD, Args.Cat_addAirMassArgs{:});
            end
            %toc
        
        
            % photometric calibration
            %ProcessingStep = 961;
            %tic;
            for Isub=1:1:Nsub
                %[AllSI(:,Isub), ZP] = imProc.calib.photometricZP(AllSI(:,Isub), 'CatName',CatName(Isub));  % 10s for all in loop
                if NotIsEmptyCoadd(Isub)
                    [Coadd(Isub), ZP]   = imProc.calib.photometricZP(Coadd(Isub), 'CatName',CatName(Isub), ...
                        'UpdateMagCols',false, Args.photometricZPArgs{:});  % 2.4s for all in loop
                end
            end
            %toc
        
            % Coadd images
            % Photometric calibration of coadd images:
            %ProcessingStep = 971;
            %tic;
            if AnyCoaddExist
                [Coadd, PC, FitRes] = imProc.calib.fitPhotCalibTrans(Coadd, Args.fitPhotCalibTransArgs{:}, 'Verbose',false, 'AddMagErr', false); % 8.7s for all in loop
            end
            %toc
        
        
            % propagate photometric calibration to individual images
            if AnyCoaddExist
                % tic;
                GoodCrop = ~tools.cell.isempty_cell({ResRelZP.FitZP});
                DeltaZP = reshape([ResRelZP.FitZP], Nepoch, sum(GoodCrop));
                AllSI(:,GoodCrop) = PC(GoodCrop).applyPhotCalibShifts(AllSI(:,GoodCrop), 'DeltaZP',DeltaZP);
                % toc
            end


            
            % Finish
            %ProcessingStep = 1000;
        catch ME
            Status.PipeI   = false;
            Status.ME      = ME;
                        
%             TableRaw.FileName   = strings(RawImageList(:));
            TableRaw.FileName   = RawImageList(:);
            TableRaw.Exception(TableRaw.SelectedImages)  = true(numel(RawImageList), 1); % Exception in this stage will have PrePrepOK = true

            % TableRaw is populated!
            AllSI    = [];
            MS       = [];
            Coadd    = [];
            OnlyMP   = [];
            JD       = [];

        end
    end % if Status.Success
end
