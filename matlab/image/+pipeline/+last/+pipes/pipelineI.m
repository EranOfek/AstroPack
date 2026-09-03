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
        Args.histAnomalyArgs               = {'CCDSEC',[1 6388 25 9600]};
                                                 % args for prePrep's histogram-anomaly check
                                                 % (imUtil.image.histAnomaly); appended to prePrepArgs
                                                 % as {'histAnomalyArgs',...}.
                                                 % Default restricts the histogram to the LIGHTSEC
                                                 % region: the overscan strip's bias-level peak
                                                 % otherwise falsely triggers the bi-modality detector
                                                 % on dark-sky nights (issue #1216).
                                                 % Pass {'CCDSEC',[]} to restore the full-frame
                                                 % (overscan-included) histogram.
        Args.basicCalibArgs                = {};
        Args.KeyMidJD                      = 'MIDJD';

        % Sub image partitioning
        Args.SubSizeXY                     = [1716 1716]; % tested using: RR=imUtil.filter.fft_size_timing([Size Size],false,10000);
        Args.EdgesCCDSEC                   = [];
        Args.NoOverlapCCDSEC               = [];
        Args.ListCenters                   = [];
        Args.NewNoOverlap                  = [];
        Args.ExclusiveCCDSEC               = [];   % single-coverage sections in the full image frame (ORIGESEC header keyword)
        Args.NewExclusive                  = [];   % single-coverage sections (sub image frame); the Overlap bit marks their complement, i.e. the full overlap region, in all the crops covering it (issue #1180)
        Args.AddPrimary logical            = true; % add the 'primary' ownership column (imProc.cat.addPrimary) to the sub image and coadd catalogs

        %Args.backVarArgs                   = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[256 256], 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}}; % both for single epoch and coadd
        %Args.backVarArgs                   = {'Method',@imUtil.background.modeVar_LeftHist, 'Block',[256 256], 'PoissVar',true, 'Ncoadd',1, 'RN2',13, 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}},{}}}; % both for single epoch and coadd
        %Args.backVarArgs                   = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[256 256], 'PoissVar',true, 'Ncoadd',1, 'RN2',13, 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}}; % both for single epoch and coadd
        Args.backVarArgs                   = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[512 512], 'PoissVar',true, 'Ncoadd',1, 'RN2',13, 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}}; % both for single epoch and coadd
        %Args.backVarCoaddArgs              = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[256 256], 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}}; % both for single epoch and coadd
        %Args.backVarCoaddArgs              = {'Method',@imUtil.background.modeVar_LeftHist, 'Block',[256 256], 'PoissVar',true, 'RN2',13, 'MethodArgs',{{'VarianceRatio',1},{}}}; % both for single epoch and coadd
        %Args.backVarCoaddArgs              = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[256 256], 'PoissVar',true, 'Ncoadd',1, 'RN2',13, 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}}; % both for single epoch and coadd
        Args.backVarCoaddArgs              = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[512 512], 'PoissVar',true, 'Ncoadd',1, 'RN2',13, 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}}; % both for single epoch and coadd

        Args.Threshold                     = [500 50 4]; %[500 50 20 4];
        Args.ColCell                       = {'XPEAK','YPEAK',...
                                              'X1', 'Y1',...
                                              'X2','Y2','XY',...
                                              'SN','BACK_IM','VAR_IM',...
                                              'BACK_ANNULUS', 'STD_ANNULUS', ...
                                              'FLUX_APER', 'FLUXERR_APER',...
                                              'MAG_APER', 'MAGERR_APER',...
                                              'FLUX_XYPEAK', 'FORCED'};
        Args.AperRadius                    = [3, 5, 6, 7];
        Args.Annulus                       = [10 12];
        Args.MomentsMethod                 = 'mex'; %'mex'; %'mex';  %'legacy'|'mex'
        Args.AperPhotMethod                = 'simple'; % 'interp';  % 'simple'|'interp'

        Args.ShiftMethod                   = 'lanczos3'; %'lanczos3';  % 'fft'|'lanczos3'
        Args.PsfPhotMethod                 = '2DGN';    % 'legacy'/'old' |'1D'|'2D'|'2DGN'

        Args.BitName       = 'Streak';
        Args.SemiWidth     = 3;

        Args.image2subimagesArgs           = {};
        Args.multiIterExtractorArgs        = {}; %{'psfFitPhotArgs',{'Method','exp'}};
        Args.VisitLevelWings               = false; % Phase 1 of the single-PSF scheme: measure ONE
                                                    % empirical PSF-wing shape per crop for the whole
                                                    % visit (imProc.psf.visitWingProfile pre-pass) and
                                                    % share it across the epochs' PSFs (each epoch keeps
                                                    % its own core; buildPSF re-anchors the shape onto
                                                    % it). Removes the per-epoch wing-estimation noise
                                                    % from the PSF normalization (measured ~9 mmag on the
                                                    % bright end, issue #1178 thread). Default false =
                                                    % legacy per-epoch wing calibration.
        Args.visitWingProfileArgs          = {};    % extra args for imProc.psf.visitWingProfile
        Args.SearchStreaksEpoch            = true;  % search streaks in epoch images
        Args.maskCR_Args                   = {'RemoveFromCat',true}; % <-- remove CR
        Args.MaskHole                      = true;
        Args.maskHolesArgs                 = {};
        Args.astrometryVisitSubImageArgs   = {};
        Args.MinFracIsolated               = 0.5;   % minimum fraction of isolated reference sources - see imProc.cat.getAstrometricCatalog
        Args.forcedPhotArgs                = {};
        %--- pipeline.generic.proc2MatchedSources args ---
        Args.proc2MatchedSourcesArgs       = {};


        Args.MatchedCols                   = {'RA','Dec',...
                                              'X','Y',...
                                              'X1','Y1','X2','Y2','XY',...
                                              'SN','SN_1','SN_2',...
                                              'MAG_PSF','MAGERR_PSF','PSF_CHI2DOF','FLUX_PSF',...
                                              'MAG_APER_2','MAGERR_APER_2',...
                                              'MAG_APER_3','MAGERR_APER_3',...
                                              'MAG_APER_4','MAGERR_APER_4',...
                                              'FLUX_APER_3',...
                                              'FLAGS',...
                                              'BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS',...
                                              'primary',...
                                              'FORCED'};
        Args.ColUse                        = 'FORCED';
        Args.AddUnUse                      = true;
        

        Args.matchExternal_Indiv           = true;
        Args.matchExternalArgs_Indiv       = {};
        Args.procCoaddArgs                 = {};
        Args.StackMethod                   = 'wrobust';  % 'sigmaclip';
        Args.coadd_WRobustArgs             = {};
        Args.generateImageIDArgs           = {};
        Args.fitPhotCalibTransArgs         = {};
        Args.MagType char {mustBeMember(Args.MagType, {'lup','mag'})} = 'lup'  % flux->mag conversion for EVERY MAG_* column produced by this pipeline - the instrumental ones from the extractors (and hence the MatchedSources light curves) and the calibrated ones from fitPhotCalibTrans/applyPhotCalibShifts: 'lup' convert.luptitude (default) | 'mag' convert.magnitude (NaN for non-positive flux). PipelineDemon sets 'mag'.
        Args.NaNUncalibMag logical         = false;  % if true, NaN-fill the MAG_*/MAGERR_* columns of crops whose photometric calibration did not run (no coadd, or a crop with no relative-ZP fit), instead of leaving uncalibrated instrumental values in the products. PipelineDemon sets true.
        
        %Args.PoissVar                      = true;
        %Args.RN2                           = 12;

        Args.photometricZPArgs             = {};

        Args.LimMagArgs                    = {};
        Args.BackMagArgs                   = {};
        Args.KeyZP                         = 'PT_ZP'; % photometric ZP from fitPhotCalibTrans (propagated to AllSI via applyPhotCalibShifts)

        Args.ForcedPhotCat               = 'ForcedPhotList'; %'WDEDR3';  % UPDATE
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
        
        Args.AddNdet                     = true;
        Args.NdetSearchRadius            = 1.5;  % [arcsec]


        Args.Logger                      = [];
        %Args.Sa

        % 
        Args.UseMex                      = true;
        
        Args.DBobj                       = [];
        Args.DB_Table_Raw                = [];

        Args.MatchMethod                 = 'mex'; % 'old'|'mex'

        Args.Status                      = [];
    end
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;

    if ~isempty(Args.Status)
        Status = Args.Status;
    end
    
    Status.PipeI   = true;
    Status.ME      = [];
    Status.NfailedBack = 0;   % sub images whose background estimation failed (#1226)
    Status.NbadShiftXY = 0;   % sub image groups whose ShiftXY was unusable and were registered by WCS (#1162)
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
    % Normalize to a column: downstream code (e.g. line 192,
    % RawImageList(FlagGoodImages,:)) uses 2-D logical indexing that
    % assumes RawImageList is [Nepoch x 1]. A row-oriented RawImageList
    % (a natural, common construction - the manual-mode fallback right
    % above builds one itself via {Files.name}) previously reached that
    % indexing unreshaped, since row-position-1 has size 1, not Nepoch,
    % throwing "logical indices ... contain a true value outside of the
    % array bounds" for any FlagGoodImages(k>1)==true. See issue #1213.
    RawImageList    = RawImageList(:);
    RawImageListAll = RawImageList;

    %ProcessingStep = 21;
    Nepoch = numel(RawImageList);

    % load images and check quality
    % AI putput is of size [Nimages x 1]
    % Fold the dedicated histAnomalyArgs into prePrepArgs; appended AFTER the
    % caller's prePrepArgs so on a duplicate 'histAnomalyArgs' name-value pair
    % the dedicated argument wins (last occurrence prevails in NV parsing).
    PrePrepArgs = Args.prePrepArgs(:).';
    if ~isempty(Args.histAnomalyArgs)
        PrePrepArgs = [PrePrepArgs, {'histAnomalyArgs', Args.histAnomalyArgs}];
    end
    try
        [AI, TableForDB, TableHeader, JD_AI, FlagGoodImages, ExpTime_AI] = pipeline.generic.prePrep(RawImageList, PrePrepArgs{:});  %5.9s
        % Note that AI may be shorter than TableRaw
        % It contains only: TableRaw.SelectedImages

        TableRaw = [TableHeader, TableForDB];
        TableRaw.PrePrepOK = true(size(TableRaw,1), 1);

        if isempty(AI)
            % No useful RAW images survived prePrep quality checks. This is a
            % normal observing condition (e.g., clouds, closed shutter), not an
            % error: unwind peacefully so the caller records the status and
            % moves the raw images to the failed/ directory.
            Status.PipeI        = false;
            Status.NoGoodImages = true;
            Status.Msg          = sprintf('No useful RAW images in visit - all %d images rejected by prePrep quality checks', numel(RawImageList));

            TableRaw.PrePrepOK  = false(size(TableRaw,1), 1);
            TableRaw.Exception  = false(size(TableRaw,1), 1); % rejected, not an exception

            AllSI    = [];
            MS       = [];
            Coadd    = [];
            OnlyMP   = [];
            JD       = [];
        else
            RawImageList = RawImageList(FlagGoodImages,:);
        end

    catch ME
        Status.PipeI   = false;
        Status.ME      = ME;
        TableRaw.FileName   = string(RawImageList(:));
        TableRaw.PrePrepOK  = false(numel(RawImageList), 1);
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
                [Args.EdgesCCDSEC, ~, Args.NoOverlapCCDSEC, Args.NewNoOverlap, Args.ListCenters, Args.ExclusiveCCDSEC, Args.NewExclusive] = imUtil.cut.gridSubImage(SizeXY, Args.SubSizeXY);  % 0.01s
            end
            % No WCS/PSF/Cat so no need to update them
            %ProcessingStep = 61;
            AllSI=imProc.image.images2subImages(AI, 'SubSizeXY',Args.SubSizeXY, 'EdgesCCDSEC',Args.EdgesCCDSEC, 'ListCenters',Args.ListCenters, 'NoOverlapCCDSEC',Args.NoOverlapCCDSEC, 'NewNoOverlap',Args.NewNoOverlap, 'NewExclusive',Args.NewExclusive, 'ExclusiveCCDSEC',Args.ExclusiveCCDSEC,...
                                                    'UpdateWCS',false, 'UpdatePSF',false, 'UpdateCat',false, 'UpdateXY',false);  % 6.6s
            [Nepoch, Nsub] = size(AllSI);
            Nobj = numel(AllSI);
        
            
            % get JD of all epoch - once
            %ProcessingStep = 71;
            JD = repmat(JD_AI(:), 1, Nsub); % faster than getting the JD for AllSI
            ExpTime = repmat(ExpTime_AI(:), 1, Nsub);

            % Visit-level PSF wing profiles: one wing SHAPE per crop, pooled
            % from bright stars of several epochs, shared by all the epochs'
            % PSFs (per-epoch cores are kept; see Args.VisitLevelWings).
            % WingArgCell{Iobj} is spliced into each extractor call: {} when
            % disabled (bit-identical legacy behavior), or
            % {'WingProfile', <this crop's profile>} when enabled. AllSI is
            % [Nepoch x Nsub], so linear object Iobj maps to crop
            % ceil(Iobj/Nepoch).
            WingArgCell   = repmat({{}}, 1, Nobj);
            WingArgSerial = {};
            if Args.VisitLevelWings
                WingProf = imProc.psf.visitWingProfile(AllSI, Args.visitWingProfileArgs{:});
                for Iwp = 1:Nobj
                    WingArgCell{Iwp} = {'WingProfile', WingProf(ceil(Iwp./Nepoch))};
                end
                % whole-array (serial) form: one profile per linear object
                WingArgSerial = {'WingProfile', WingProf(ceil((1:Nobj)./Nepoch))};
            end

            % initiate parpool if needed
            %ProcessingStep = 81;
            PP = [];
            if Args.UseParfor
                PP = gcp('nocreate');
                if isempty(PP)
                    % no parpool exist
                    % create new parpool
                    PP = parpool(localCluster(Args.Nworkers), Args.Nworkers);
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
                                                            WingArgSerial{:},...
                                                            'JD',JD,...
                                                            'ColCell',Args.ColCell,...
                                                            'MagType',Args.MagType,...
                                                            'UseMex',Args.UseMex,...
                                                            'backVarArgs',Args.backVarArgs,...
                                                            'AperRadius',Args.AperRadius,...
                                                            'AperPhotMethod',Args.AperPhotMethod,...
                                                            'Annulus',Args.Annulus,...
                                                            'MomentsMethod',Args.MomentsMethod,...
                                                            'ShiftMethod',Args.ShiftMethod,...
                                                            'PsfPhotMethod',Args.PsfPhotMethod,...
                                                            'maskCR_Args',Args.maskCR_Args,...
                                                            'SearchStreaks',Args.SearchStreaksEpoch,...
                                                            'Threshold',Args.Threshold,...
                                                            'AddSkyCoo',false);  % 466 s (with UseMex=false)
               
            else
                %ProcessingStep = 102;
                %tic;
                % parfor (Iobj=1:1:Nobj, 0)  % no par for!
                parfor Iobj=1:1:Nobj
                    [AllSI(Iobj)] = imProc.sources.multiIterExtractor(AllSI(Iobj), Args.multiIterExtractorArgs{:},...
                                                            WingArgCell{Iobj}{:},...
                                                            'JD',JD(Iobj),...
                                                            'ColCell',Args.ColCell,...
                                                            'MagType',Args.MagType,...
                                                            'UseMex',Args.UseMex,...
                                                            'backVarArgs',Args.backVarArgs,...
                                                            'AperRadius',Args.AperRadius,...
                                                            'AperPhotMethod',Args.AperPhotMethod,...
                                                            'Annulus',Args.Annulus,...
                                                            'MomentsMethod',Args.MomentsMethod,...
                                                            'ShiftMethod',Args.ShiftMethod,...
                                                            'PsfPhotMethod',Args.PsfPhotMethod,...
                                                            'maskCR_Args',Args.maskCR_Args,...
                                                            'SearchStreaks',Args.SearchStreaksEpoch,...
                                                            'Threshold',Args.Threshold,...
                                                            'AddSkyCoo',false);  % 119 s (on 16 cores): 169s -> 135s (with UseMex=true)
                end
                %toc
            end

            % Consider update TableRaw - No PSF, etc?
            %TableRaw.BasicCalib(TableRaw.SelectedImages) = true(numel(AI),1); 
        
            
            % Mask holes
            if Args.MaskHole
                AllSI = imProc.mask.maskHoles(AllSI, Args.maskHolesArgs{:}); % 9s
            end

            % solve astrometry of all images
            %ProcessingStep = 301;
            [ResFit, AllSI, CatName] = imProc.astrometry.astrometryVisitSubImage(AllSI, 'MatchMethod',Args.MatchMethod, 'JD',JD, 'MinFracIsolated',Args.MinFracIsolated, Args.astrometryVisitSubImageArgs{:}); % 22s
        
            % add coordinates to catalogs
            %ProcessingStep = 401;
            AllSI = imProc.astrometry.addCoordinates2catalog(AllSI, 'UpdateCoo',true, 'OutUnits','deg');  % 0.8s

            % Add JD, RA, Dec, IsEdge to streaks data:
            AllSI=imProc.streaks.addSkyCoo(AllSI, 'PopJD',true, 'JD',JD, 'ExpTime',ExpTime);
            % populate streak mask:
            AllSI = imProc.streaks.addStreak2Mask(AllSI, 'BitName', Args.BitName, 'SemiWidth',Args.SemiWidth);

            % Args.DistEdgeStreak = 10;
            % for Iobj=1:1:Nobj
            %     if ~isempty(AllSI(Iobj).Streaks) && ~isempty(AllSI(Iobj).Streaks.Segs)
            % 
            %         XY = fliplr(reshape(AllSI(Iobj).Streaks.Segs,2,2).');
            %         [S_RA, S_Dec] = AllSI(Iobj).WCS.xy2sky(XY(:,1),XY(:,2));
            %         AllSI(Iobj).Streaks.XY  = XY;
            %         AllSI(Iobj).Streaks.RA  = S_RA;
            %         AllSI(Iobj).Streaks.Dec = S_Dec;
            %         AllSI(Iobj).Streaks.IsEdge = [any(XY(1,:)<Args.DistEdgeStreak) || any(XY(1,:)>(SizeImage-Args.DistEdgeStreak)),...
            %                                       any(XY(2,:)<Args.DistEdgeStreak) || any(XY(2,:)>(SizeImage-Args.DistEdgeStreak))];
            % 
            %     end
            % end
        

            % add PSF FWHM to header - after astrometry, beacuse WCS is needed
            %ProcessingStep = 201;
            % This must be done after astrometry as the Scale is used
            AllSI = imProc.psf.fwhm(AllSI, 'AddMorphology',true, 'AddErr',true, 'UseLegacy',true, 'DefScale',Args.DefScale);
                
            
            % Update Airmass header keyword to based on measured crop center
            % Need to precess coordinates to Equnnox of date
            % The Epoch difference within a visit is small and results in
            % 1e-10 difference in airmass and hence ignored
            [AllSI, AllImagesAirMass] = imProc.header.addAirMass(AllSI, 'JD',JD, 'HealpixType','nested', 'EquinoxJD',JD(1,1), Args.Header_addAirMassArgs{:}); % 0.4s for all images

            % Individual sub images : quality           
            % astrometry
            %ProcessingStep = 421;
            IsGoodWCS = imProc.astrometry.isSuccessWCS(AllSI);  % 1.3 s
            % Nstars
            Nstars    = AllSI.sizeCatalog;
            % background variations
            MeanBack     = imProc.stat.mean(AllSI);
            %MeanVar      = imProc.stat.mean(AllSI);
            MeanMeanBack = mean(MeanBack, 2, 'omitnan'); % mean background over all sub images in each epoch
            MaxFracGrad  = (max(MeanBack,[],2) - min(MeanBack,[],2))./MeanMeanBack; % max fractional background gradient per epoch
            TableRaw.MaxFracGrad(TableRaw.SelectedImages) = MaxFracGrad;

            % background estimation failures (issue #1226)
            % Such a crop is still written to disk, with blank background
            % keywords, but it must not take part in the coaddition, the
            % matched sources or the forced photometry. Today it is already
            % excluded by the two terms above - it extracts no sources and its
            % astrometry does not converge - but only as a side effect; the
            % term below makes it deliberate. Note that MaxFracGrad cannot
            % catch it: it is computed from the image, not from the background.
            IsFailedBack      = imProc.background.isFailedBack(AllSI);
            Status.NfailedBack = sum(IsFailedBack, 'all');
            if Status.NfailedBack>0
                warning('Background estimation failed for %d of %d sub images - they are saved but excluded from the coadd and the matched sources', Status.NfailedBack, numel(AllSI));
            end

            IsGood = IsGoodWCS & Nstars>Args.MinNstars & MaxFracGrad<Args.MaxFracGrad & ~IsFailedBack;
        
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
                    
                    ColNamesFF = AllSI(find(IsGood==1,1)).CatData.ColNames;
            
                    %AllFP = AstroCatalog([Nepoch, Nsub]);
                        % for each sub image where IsForcedPhot is true, run over all the epochs
                        Ind = find(IsForcedPhot);
                        for IsubGood = 1:numel(Ind)                            
                            % May need to update the column names:
                            % Note that the exact JD doesn't matter much,
                            % therefore using JD(1).
                            [~, RA, Dec] = imProc.cat.applyProperMotionSimple(CatForcedPhot(IsubGood), JD(1), 'OutUnits','rad', 'OutEpochUnits','JD', 'InEpoch','Epoch', 'ColPMRA','PMRA', 'ColPMDec','PMDec', 'OutUnits','deg');
                            Coo = [RA, Dec];                            
                            %if strcmpi(Args.OutputType, 'concatai')       
                            if ~isempty(Coo)
                                IsGoodEpoch = IsGood(:,Ind(IsubGood));
                                AllSI(IsGoodEpoch,Ind(IsubGood)) = imProc.sources.forcedPhotNew(AllSI(IsGoodEpoch,Ind(IsubGood)), ...
                                    'OutputType','ConcatAI', ...
                                    'Coo',Coo, 'Moving',false, 'AddRefStarsDist',0, 'CatIsUniform',true, 'ColCell',ColNamesFF, ...
                                    'ReadColFromHeader',false, 'PsfPhotMethod',Args.PsfPhotMethod, 'ShiftMethod',Args.ShiftMethod, ...
                                    'UseMex',Args.UseMex, ...
                                    Args.forcedPhotArgs{:}, 'MagType',Args.MagType);  % 8.3 s [for all in loop]
                            end                           
                        end
                    %toc
                end
        
            else
                %AllForcedPhot = [];
            end

            % ownership column (issue #1180): primary=1 for the sources whose
            % exact X,Y is inside the crop unique section, 0 for the copies in
            % the overlapping neighbours. The Overlap FLAGS bit marks the full
            % overlap region in all the crops covering it, so de-duplication
            % of the concatenated crop catalogs uses this column.
            % Placed after the forced photometry, which appends rows at the
            % coadd positions, so that the forced rows get a value too
            % (addPrimary recomputes the whole column from X,Y and replaces
            % it in place).
            if Args.AddPrimary && ~isempty(Args.NewNoOverlap)
                for Isub=1:1:Nsub
                    IsecP = min(Isub, size(Args.NewNoOverlap,1));
                    imProc.cat.addPrimary(AllSI(:,Isub), Args.NewNoOverlap(IsecP,:));
                end
            end

            % Sort all catalogs by Dec
            %ProcessingStep = 451;
            for Iim=1:1:Nsub.*Nepoch
                AllSI(Iim).CatData.sortrows('Dec');  % 0.16s (for all in loop)
            end
        
            if Args.AddSrcAM
                AllSI = imProc.cat.addAirMass(AllSI, 'JD',JD, 'IsGood',IsGood, 'EquinoxJD',JD(1), Args.Cat_addAirMassArgs{:});
            end

            % Add XFULL/YFULL
            [~,AllSI] = imProc.cat.addXYfull(AllSI);

            
            % Add PSF fraction to header
            [~,AllSI] = imProc.psf.aperFrac(AllSI, 'AperRadius',Args.AperRadius);

            % match external / too expensive
            %if Args.matchExternal_Indiv
            %    % current default is true - do we want this?
            %    AllSI = pipeline.generic.matchExternal(AllSI, Args.matchExternalArgs_Indiv{:});
            %end
            %AllSI = imProc.match.match_catsHTMmerged(AllSI); % 240 s
        
            %ProcessingStep = 701;
            %tic;
            for Isub=1:1:Nsub
                % add old photometric calibration
                % do not add LimMag and BackMag to header
                [AllSI(:,Isub), ZP] = imProc.calib.photometricZP(AllSI(:,Isub), 'CatName',CatName(Isub),...
                                            'AddBackMag',false,...
                                            'LimMagSN',[],...
                                            'MinFracIsolated',Args.MinFracIsolated,...
                                            'UpdateMagCols',false, Args.photometricZPArgs{:});  % 10s for all in loop
                %[Coadd(Isub), ZP]   = imProc.calib.photometricZP(Coadd(Isub), 'CatName',CatName(Isub));  % 2.4s for all in loop
            end
            %toc

            % Merge catalogs
            %ProcessingStep = 501;
            [MS,ResRelZP] = pipeline.generic.proc2MatchedSources(AllSI, Args.proc2MatchedSourcesArgs{:}, 'FlagGood',IsGood, 'DimEpoch',1, 'ColUse',Args.ColUse, 'AddUnUse',Args.AddUnUse, 'MatchedCols',Args.MatchedCols);   % 9.6 s -> 1.3s (with MatchMethod='unify')

            % Stamp the flux->magnitude convention of the MAG_* fields onto the
            % MatchedSources, so that the saved product records whether its
            % magnitudes are luptitudes or magnitudes (issue #1161).
            % write1 stores it as the HDF5 root attribute 'MagType'.
            % Guarded: deal() on an empty [] would silently turn MS into a
            % struct rather than leaving it empty.
            if ~isempty(MS)
                [MS(1:numel(MS)).MagType] = deal(Args.MagType);
            end
        
            % calculate the photometric rms per crop
            
            PhotRMS = MS.calcRMS('FieldY','MAG_APER_3', 'FieldX','MAG_APER_3');
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
                                                          'ReMeasureBack',true,...                                                          
                                                          'CatName',CatName,...
                                                          'ShiftXY',ShiftInfo,...
                                                          'IsGood',IsGood,...
                                                          'PropShiftXY','ShiftXY',...
                                                          'IsShiftXYfiltered',true,...
                                                          'UNIQSEC',Args.NewNoOverlap,...
                                                          'EXCLSEC',Args.NewExclusive,...
                                                          'AddPrimary',Args.AddPrimary,...
                                                          'StackMethod',Args.StackMethod,...
                                                          'UseMex',Args.UseMex,...
                                                          'PhotCalibSimple',false,...
                                                          'PhotCalibTrans',false,...
                                                          'MatchMethod',Args.MatchMethod,...
                                                          'backVarArgs',Args.backVarCoaddArgs,...
                                                          'AperRadius',Args.AperRadius,...
                                                          'Annulus',Args.Annulus,...
                                                          'MomentsMethod',Args.MomentsMethod,...
                                                          'AperPhotMethod',Args.AperPhotMethod,...
                                                          'ShiftMethod',Args.ShiftMethod,...
                                                          'PsfPhotMethod',Args.PsfPhotMethod,...
                                                          'maskCR_Args',Args.maskCR_Args,...
                                                          'WriteStatHeader',true,...
                                                          'photometricZP_UpdateMagCols',false,...
                                                          'MinFracIsolated',Args.MinFracIsolated,...
                                                          'Threshold',Args.Threshold,...
                                                          'multiIterExtractorArgs',[Args.multiIterExtractorArgs, {'MagType',Args.MagType}]);
            % Crops whose positionDrift ShiftXY was unusable were registered
            % by WCS instead (issue #1162). Counted here, logged by
            % PipelineDemon (no console warning by design).
            Status.NbadShiftXY = sum(strcmp({ResCoadd.RegisteredBy}, 'wcs-fallback'));
            % NOTE: multiIterExtractorArgs is passed as procCoadd's dedicated
            % pass-through (procCoadd forwards it to the coadd's own
            % multiIterExtractor call). Splatting the cell directly into the
            % procCoadd argument list (the previous form) crashed on any
            % extractor-specific name that procCoadd's arguments block does
            % not share (e.g. 'PsfAnnulus', 'populatePSFArgs', 'psfFitPhotArgs').
            
              
            %toc

            
        
            % Add image ID to coadd images: in: ID_PROC
            NotIsEmptyCoadd = ~Coadd.isemptyImage;
            NotIsEmptyCat   = ~Coadd.isemptyCatalog;
            JD_Coadd = [ResCoadd(NotIsEmptyCoadd).WMeanJD];
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
                % All Coadd images have the same epoch (roughly)
                [Coadd(NotIsEmptyCoadd)] = imProc.header.addAirMass(Coadd(NotIsEmptyCoadd), 'JD',JD_Coadd, 'HealpixType','nested', Args.Header_addAirMassArgs{:}, 'EquinoxJD',JD_Coadd(1)); % 0.3s


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
                if ~Args.UseParfor
                    Coadd(NotIsEmptyCoadd) = imProc.match.match_catsHTMmerged(Coadd(NotIsEmptyCoadd), 'SameField',false, 'CreateNewObj',false);  % 23 s
                else
                    PP = gcp('nocreate');
                    if isempty(PP)
                        PP = parpool(localCluster(Args.Nworkers), Args.Nworkers);
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
                OnlyMP = AstroCatalog([1, Nsub]);
                [OnlyMP(NotIsEmptyCoadd),~,Coadd(NotIsEmptyCoadd)] = imProc.match.match2solarSystem(Coadd(NotIsEmptyCoadd), 'JD',[], 'GeoPos',Args.GeoPos, 'OrbEl',Args.OrbEl, 'SearchRadius',Args.AsteroidSearchRadius, 'INPOP',Args.INPOP);  % 7 s

                Nast = OnlyMP.sizeCatalog;
                if sum(Nast)>0
                    % add CropID, Node, Mount, Cam, ID_COADD:
                    Cols = {'NODENUMB', 'MOUNTNUM', 'CAMNUM', 'ID_COADD'};
                    StKey = Coadd.getStructKey(Cols);
                    
                    AllCols = {'CROPID', Cols{:}};
                    for Isub=1:1:Nsub
                        if Nast(Isub)>0
                            Vals  = struct2array(StKey(Isub));
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
                    if isempty(GlobalMotion(Isub).RateX)
                        GMvals = nan(4,1);
                    else
                        GMvals = [GlobalMotion(Isub).RateX; GlobalMotion(Isub).StdX; GlobalMotion(Isub).RateY; GlobalMotion(Isub).StdY];
                    end
                    DataGM = [Args.KeysGlobalMotion(:), num2cell(GMvals)];
                    Coadd(Isub).HeaderData.insertKey(DataGM,'end');
                end
            end
            
            
            %ProcessingStep = 951;
            %tic;
            if Args.AddSrcAM
                JD_CoaddPerSub = nan(1, Nsub);
                JD_CoaddPerSub(NotIsEmptyCoadd) = JD_Coadd;
                JD_Eq = mean(JD_CoaddPerSub,'all','omitnan');
                Coadd(NotIsEmptyCat) = imProc.cat.addAirMass(Coadd(NotIsEmptyCat), 'JD',JD_CoaddPerSub(NotIsEmptyCat), 'EquinoxJD',JD_Eq, Args.Cat_addAirMassArgs{:});

                %Coadd(NotIsEmptyCat) = imProc.cat.addAirMass(Coadd(NotIsEmptyCat), 'JD',JD_Coadd, 'EquinoxJD',JD_Coadd(1), Args.Cat_addAirMassArgs{:});
            end
            %toc
            if Args.AddNdet
                Coadd = imProc.cat.addNdet(Coadd, MS, 'NotIsEmptyImages',NotIsEmptyCat, 'SearchRadius',Args.NdetSearchRadius);
                %Coadd(NotIsEmptyCat) = imProc.cat.addNdet(Coadd(NotIsEmptyCat), MS, 'SearchRadius',Args.NdetSearchRadius);
            end
        
            % photometric calibration
            %ProcessingStep = 961;
            %tic;
            for Isub=1:1:Nsub
                %[AllSI(:,Isub), ZP] = imProc.calib.photometricZP(AllSI(:,Isub), 'CatName',CatName(Isub));  % 10s for all in loop
                if NotIsEmptyCat(Isub)
                    % do not add LimMag and BackMag to header
                    [Coadd(Isub), ZP]   = imProc.calib.photometricZP(Coadd(Isub), 'CatName',CatName(Isub), ...
                                            'AddBackMag',false,...
                                            'LimMagSN',[],...
                                            'MinFracIsolated',Args.MinFracIsolated,...
                                            'UpdateMagCols',false, Args.photometricZPArgs{:});  % 2.4s for all in loop
                end
            end
            %toc
        
            % Coadd images
            % Photometric calibration of coadd images:
            %ProcessingStep = 971;
            %tic;
            if AnyCoaddExist
                [Coadd, PC, FitRes] = imProc.calib.fitPhotCalibTrans(Coadd, 'MagType', Args.MagType, Args.fitPhotCalibTransArgs{:}, 'Verbose',false, 'AddMagErr', true); % 8.7s for all in loop
            end
            %toc
        
        
            % propagate photometric calibration to individual images
            if AnyCoaddExist
                % tic;
                GoodCrop = ~tools.cell.isempty_cell({ResRelZP.FitZP});
                %DeltaZP = reshape([ResRelZP.FitZP], Nepoch, sum(GoodCrop));
                DeltaZP = reshape([ResRelZP(GoodCrop).FitZP], Nepoch, sum(GoodCrop));
                AllSI(:,GoodCrop) = PC(GoodCrop).applyPhotCalibShifts(AllSI(:,GoodCrop), 'DeltaZP',DeltaZP);
                % toc
            else
                GoodCrop = false(1, size(AllSI,2));
            end

            % Crops for which the photometric calibration did not run keep the
            % instrumental magnitudes of the extractor (arbitrary ZP, no
            % relative-ZP correction) under calibrated column names. When
            % requested, NaN-fill them instead - see issue #1161.
            % Crops with an empty TransModel are already NaN-filled inside
            % applyPhotCalibShifts, so only the two branches below are left.
            if Args.NaNUncalibMag && ~all(GoodCrop)
                % An empty PhotCalibTrans (empty TransModel) writes the full
                % PT_* key set with NaN values; the mex header writers
                % serialize a non-finite value as a blank (FITS undefined)
                % card - the representation agreed in issue #1194 - so the
                % keywords are present but empty. Without this the crop would
                % carry no PT_* at all, and a consumer could not tell
                % "calibration did not run" from "no sources found".
                PCuncalib   = PhotCalibTrans;
                UncalibCrop = find(~GoodCrop);
                for Iuc=1:1:numel(UncalibCrop)
                    for Iep=1:1:size(AllSI,1)
                        try
                            AllSI(Iep,UncalibCrop(Iuc)).CatData = ...
                                PhotCalibTrans.nanFillMagCols(AllSI(Iep,UncalibCrop(Iuc)).CatData);
                            if ~isempty(AllSI(Iep,UncalibCrop(Iuc)).HeaderData)
                                AllSI(Iep,UncalibCrop(Iuc)).HeaderData = ...
                                    PCuncalib.photCalibTransToHeader(AllSI(Iep,UncalibCrop(Iuc)).HeaderData);
                            end
                        catch ME
                            fprintf('pipelineI: NaN-fill of uncalibrated epoch %d crop %d failed: %s\n', ...
                                    Iep, UncalibCrop(Iuc), ME.message);
                        end
                    end
                end
            end

            % Add LimMag and BackMag
            % LimMag is applied to all the crops, including those with an
            % empty catalog, so that LIMMAG is always present in the header
            % (undefined value when it can not be estimated)
            [Coadd] = imProc.calib.limmag(Coadd, Args.LimMagArgs{:});  
            [Coadd(NotIsEmptyCat)] = imProc.calib.backmag(Coadd(NotIsEmptyCat), 'KeyZP',Args.KeyZP, Args.BackMagArgs{:});   
            % Add LimMag and BackMag / AllSI (after propagation to all
            % images)
            [AllSI] = imProc.calib.limmag(AllSI, Args.LimMagArgs{:});  % 0.3s
            [AllSI] = imProc.calib.backmag(AllSI, 'KeyZP',Args.KeyZP, Args.BackMagArgs{:}); % 0.2s
            % Add XFULL/YFULL
            [~,Coadd(NotIsEmptyCat)] = imProc.cat.addXYfull(Coadd(NotIsEmptyCat));
            % Add PSF fraction to header
            [~,Coadd(NotIsEmptyCat)] = imProc.psf.aperFrac(Coadd(NotIsEmptyCat), 'AperRadius',Args.AperRadius);

            % Give the source-less crops the column set of the visit (#1226).
            % A crop which extracted nothing - e.g. one whose background
            % estimation failed - ends with a catalogue that has neither rows
            % nor columns, and a catalogue without columns cannot be written as
            % a FITS binary table, so no data product would be saved for it at
            % all. With the columns in place it is saved as a zero-row
            % catalogue, whose header records why it is empty.
            % Done here, after every stage that adds columns, so that the empty
            % catalogues match the ones actually written for this visit.
            AllSI = imProc.cat.fillEmptyCatColumns(AllSI);

            % Finish
            %ProcessingStep = 1000;
        catch ME
            Status.PipeI   = false;
            Status.ME      = ME;
                        
%             TableRaw.FileName   = strings(RawImageList(:));
            TableRaw.FileName   = RawImageListAll(:);
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


function C = localCluster(~)
    % A 'local' cluster with a job storage location private to this process.
    %   Two MATLAB clients on one machine (the two LAST demons, one per
    %   DataDir) cannot both start a pool from the shared default location:
    %   both fail with "Parallel pool failed to start ... validate the
    %   profile 'local'", pipelineI then throws, and the demon moves the
    %   whole visit to failed/.
    C = parcluster('local');
    JSL = fullfile(tempdir, sprintf('matlab_jsl_%d', feature('getpid')));
    if ~isfolder(JSL)
        mkdir(JSL);
    end
    C.JobStorageLocation = JSL;
end
