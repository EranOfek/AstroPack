function [SI, AstrometricCat, Result] = singleRaw2proc(File, Args)
    % Basic processing of a single raw image into a processed image
    %   Including:
    %       Reading the image
    %       Generate a mask image and mask saturated pixels
    %       Subtract bias/dark image
    %       Divide by flat image
    %       Remove Fringe image
    %       Break image to sub images
    %       Estimate background
    %       Basic source findinging
    %       Astrometry
    %       Update astrometry in catalog
    %       Photometric ZP
    %       Update photometric ZP in catalog
    %       Save products
    % Input  : -
    % Output : -
    % Author : Eran Ofek (Aug 2021)
    % Example: pipeline.generic.singleRaw2proc
    %          % generate CalibImages using example in CalibImages/unitTest
    %          File = 'LAST.2.1.2_20200821.020230.952_clear_0_science.fits';
    %          File = 'LAST.2.1.2_20200820.184642.957_clear_0_science.fits';
    %          [SI, AstrometricCat,Result]=pipeline.generic.singleRaw2proc(File,'CalibImages',CI);
    %          [SI, ~,Result]=pipeline.generic.singleRaw2proc(File,'CalibImages',CI, 'CatName',AstrometricCat);
    
    arguments
        File                   % FileName+path / AstroImage
        Args.Dir                              = '';
        Args.HDU                              = 1;
        Args.CalibImages CalibImages          = [];
        Args.Dark                             = []; % [] - do nothing
        Args.Flat                             = []; % [] - do nothing
        Args.Fringe                           = []; % [] - do nothing
        Args.BlockSize                        = [1600 1600];  % empty - full image
        Args.OverlapXY                        = [64 64];
        
        Args.CCDSEC                           = []; % if provided then override BlockSize and OverlapXY
        Args.OVERSCAN                         = [];
        
        Args.Scale                            = 1.25;
        
        Args.AddHeadKeys                      = {'FILTER','clear';...
                                                 'SATURVAL',55000;...
                                                 'OBSLON',35.0;...
                                                 'OBSLAT',31.0;...
                                                 'OBSALT',400;...
                                                 'TIMEZONE',2;...
                                                 'FIELDID',1;...
                                                 'COUNTER',1;...
                                                 'CCDID',1;...
                                                 'CROPID',0;...
                                                 'LEVEL','raw';...
                                                 'VERSION','1';...
                                                 'SUBDIR','';...
                                                 'OVERSCAN','[6355 6388 1 9600]'};   % '[1 6354 1 9600]'};
        
        Args.MultiplyByGain logical           = true; % after fringe correction
        Args.MaskSaturated(1,1) logical       = true;
        Args.DoAstrometry(1,1) logical        = true;
        Args.DoPhotometry(1,1) logical        = true;
        Args.MatchExternal(1,1) logical       = false;
        Args.SaveProducts(1,1) logical        = true;
        
        
        Args.BitNameBadPix                  = {'Saturated','NaN'};
        Args.BitNameInterpolated            = 'Interpolated';
                
        
        Args.InterpolateOverProblems logical  = true;
        Args.BitNamesToInterp                 = {'Saturated','HighRN','DarkHighVal','Hole','Spike','CR_DeltaHT'};
        Args.interpOverNanArgs cell           = {};
        
        Args.maskSaturatedArgs cell           = {};
        Args.debiasArgs cell                  = {};
        Args.SubtractOverscan logical         = false;
        Args.FinalCrop                        = [1 6354 1 9600];  % if empty do nothing
        Args.MethodOverScan                   = 'globalmedian';
        Args.deflatArgs cell                  = {};
        Args.CorrectFringing logical          = false;
        Args.image2subimagesArgs cell         = {};
        
        Args.backgroundArgs cell              = {};
        Args.BackSubSizeXY                    = [128 128];
        Args.DiluteStep                       = 2;
        Args.EstimateRowColNoise logical      = false;
        Args.SubCorrelaredColRow logical      = true;
        Args.subtractMeanColRowArgs cell      = {};
        
        Args.findMeasureSourcesArgs cell      = {};
        Args.ZP                               = 25;
        Args.photometricZPArgs cell           = {};
        Args.astrometrySubImagesArgs cell     = {};
        Args.astrometryRefineArgs cell        = {};
        Args.RefineSearchRadius               = 5;
        Args.CatName                          = 'GAIAEDR3';  % or AstroCatalog
        Args.Tran                             = Tran2D('poly3');
        Args.WCS                              = [];   % WCS/AstroImage with WCS - will use astrometryRefine...
        Args.addCoordinates2catalogArgs cell  = {'OutUnits','deg'};
        
        % source finding
        Args.Threshold                        = 5;
        Args.ColCell cell                     = {'XPEAK','YPEAK','SN','BACK_IM','VAR_IM',...           
                                                'X', 'Y',...
                                                'X2','Y2','XY',...
                                                'FLUX_APER', 'FLUXERR_APER',...
                                                'APER_AREA','BACK_ANNULUS', 'STD_ANNULUS', ...
                                                'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS',...
                                                'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV'};
        Args.DeletePropAfterSrcFinding        = {'Back','Var'};
        
        Args.UpdateHeader logical             = true;   % CROPID & LEVEL
        
        Args.OrbEl                            = []; %celestial.OrbitalEl.loadSolarSystem;  % prepare ahead to save time % empty/don't match
        Args.match2solarSystemArgs            = {};
        Args.GeoPos                           = [];
        
        Args.SaveFileName                     = [];  % full path or ImagePath object
        Args.CreateNewObj logical             = false;
    end
    
    % Get Image
    OverscanValue = [];
    if ischar(File)
        if ~isempty(Args.Dir)
            File = sprintf('%s%s%s',Args.Dir, filesep, File);
        end
        
        if ~isempty(Args.CCDSEC)
            % read into multiple images [ccdsec]
            AI = AstroImage.readByCCDSEC(File, Args.CCDSEC, 'ReadHeader',true, 'HDU',Args.HDU);
            
            if ~isempty(Args.OVERSCAN)
                OverScanRegion = AstroImage.readByCCDSEC(File, Args.OVERSCAN, 'ReadHeader',false, 'HDU',Args.HDU);
                OverscanValue  = median(OverScanRegion,'all','omitnan');
            end
        else
            % read into a single image
            AI = AstroImage(File, 'HDU',Args.HDU);
        end
        
    elseif isa(File, 'AstroImage')
        AI = File;
    else
        error('Unsupported File type option');
    end
    
    % createNewObj
    if Args.CreateNewObj
        AI = AI.copy;
    end
    
    % set CalibImages
    if isempty(Args.CalibImages)
        CI = CalibImages;
        CI.Dark   = Args.Dark;
        CI.Flat   = Args.Flat;
        CI.Fringe = Args.Fringe;
    else
        CI = Args.CalibImages;
    end
        
    
    % add additional header keywords
    if ~isempty(Args.AddHeadKeys)
        AI.setKeyVal(Args.AddHeadKeys(:,1), Args.AddHeadKeys(:,2));
    end
    
    % fix date
    % JD is can't be written with exponent
    Nai = numel(AI);
    for Iai=1:1:Nai
        Date = AI(Iai).HeaderData.getVal('DATE-OBS');
        Date = sprintf('%s:%s:%s', Date(1:13), Date(14:15), Date(16:end));
        JD   = celestial.time.julday(Date);
        StrJD = sprintf('%16.8f',JD);
        AI(Iai).setKeyVal('JD',StrJD);
    end    
    
    
    % Note that InterpolateOverSaturated is false, because this is done
    % later on in this function
    AI = CI.processImages(AI, 'SubtractOverscan',false,...
                              'SingleFilter',true,...
                              'InterpolateOverBadPix',true,...
                              'BitNameBadPix',Args.BitNameBadPix,...
                              'BitNameInterpolated',Args.BitNameInterpolated,...
                              'MaskSaturated',Args.MaskSaturated,...
                              'maskSaturatedArgs',{},...
                              'debiasArgs',Args.debiasArgs,...
                              'SubtractOverscan',Args.SubtractOverscan,...
                              'MethodOverScan',Args.MethodOverScan,...
                              'deflatArgs',Args.deflatArgs,...
                              'CorrectFringing',Args.CorrectFringing,...
                              'MultiplyByGain',Args.MultiplyByGain);
                   
    % specail treatment of overscan
    if ~isempty(OverscanValue)
        for Iai=1:1:Nai
            AI.Image = AI.Image - OverscanValue;
        end
    end
        
    % crop overscan
    if ~isempty(Args.FinalCrop) && isempty(Args.CCDSEC)
        % do this only for full images
        AI.crop(Args.FinalCrop, 'UpdateWCS',false, 'CreateNewObj',false);
    end
    
    % get JD from header
    JD = julday(AI.HeaderData);
    
    % Sub Images - divide the image to multiple sub images
    % Set UpdatCat to false, since in this stage there is no catalog
    [SI, InfoCCDSEC] = imProc.image.image2subimages(AI, Args.BlockSize, 'UpdateCat',false,...
                                                                        Args.image2subimagesArgs{:},...
                                                                        'OverlapXY',Args.OverlapXY,...
                                                                        'UpdateWCS',false,...
                                                                        'UpdatePSF',false);
    clear AI;
    
    
    % Background 
    SI = imProc.background.background(SI, Args.backgroundArgs{:},...
                                          'SubSizeXY',Args.BackSubSizeXY,...
                                          'DiluteStep',Args.DiluteStep,...
                                          'EstimateRowColNoise',Args.EstimateRowColNoise);
    
    if Args.SubCorrelaredColRow
        SI = imProc.background.subtractMeanColRow(SI,'SubOnlyRowColComp',true,...
                                                     'RetBack',true,...
                                                     Args.subtractMeanColRowArgs{:});
    end
                                                                            
    % Source finding
    %SI.cast('double');
    SI = imProc.sources.findMeasureSources(SI, Args.findMeasureSourcesArgs{:},...
                                               'RemoveBadSources',true,...
                                               'Threshold',Args.Threshold,...
                                               'ColCell',Args.ColCell,...
                                               'ZP',Args.ZP,...
                                               'CreateNewObj',false);
    %SI.cast('single');
    
    % FFU: flags Holes
    % imProc.mask.maskHoles
    
    % Astrometry, including update coordinates in catalog
    if Args.DoAstrometry
        if isempty(Args.WCS)
            [Result.AstrometricFit, SI, AstrometricCat] = imProc.astrometry.astrometrySubImages(SI, Args.astrometrySubImagesArgs{:},...
                                                                                            'EpochOut',JD,...
                                                                                            'Scale',Args.Scale,...
                                                                                            'CatName',Args.CatName,...
                                                                                            'CCDSEC', InfoCCDSEC.EdgesCCDSEC,...
                                                                                            'Tran',Args.Tran,...
                                                                                            'CreateNewObj',false);
        else
            [Result.AstrometricFit, SI, AstrometricCat] = imProc.astrometry.astrometryRefine(SI, Args.astrometryRefineArgs{:},...
                                                                                            'WCS',Args.WCS,...
                                                                                            'EpochOut',JD,...
                                                                                            'Scale',Args.Scale,...
                                                                                            'CatName',Args.CatName,...
                                                                                            'Tran',Args.Tran,...
                                                                                            'SearchRadius',Args.RefineSearchRadius,...
                                                                                            'IncludeDistortions',true,...
                                                                                            'CreateNewObj',false);
            
        end
                                                                                    
    
        % Update Cat astrometry
        %SI = imProc.astrometry.addCoordinates2catalog(SI, Args.addCoordinates2catalogArgs{:},'UpdateCoo',true);
    end
    
    % Photometric ZP
    if Args.DoPhotometry
        % FFU: add CatName with the previously aquired AstrometricCat
        [SI, Result.ZP, ~] = imProc.calib.photometricZP(SI, 'CreateNewObj',false,...
                                                            'MagZP',Args.ZP,...
                                                            'CatName',AstrometricCat,...
                                                            Args.photometricZPArgs{:});
    
        % Update Cat photometry
    end
    
    
    % interpolate over problematic pixels
    if Args.InterpolateOverProblems
        % interpolate over staurated pixels
        SI = imProc.mask.interpOverMaskedPix(SI, 'BitNamesToInterp',Args.BitNamesToInterp,...
                                                     'interpOverNanArgs', Args.interpOverNanArgs,...
                                                     'BitNameInterpolated',Args.BitNameInterpolated,...
                                                     'CreateNewObj',false);
    end
    
    % update Header
    % CROPID, LEVEL
    if Args.UpdateHeader
        Nim = numel(SI);
        for Iim=1:1:Nim
            SI(Iim).HeaderData.replaceVal({'CROPID','LEVEL'}, {Iim, 'proc'});
        end
                                          
    end
    
    
    
    % match known solar system objects
    if ~isempty(Args.OrbEl)
        % NOTE TIME SHOULD be in TT scale
        %tic;
        TTmUTC = 70./86400;
        [SourcesWhichAreMP, SI] = imProc.match.match2solarSystem(SI, 'JD',JD+TTmUTC, 'OrbEl',Args.OrbEl, 'GeoPos', Args.GeoPos, Args.match2solarSystemArgs{:});
        %toc
    end
    
    % match against external catalogs
    if Args.MatchExternal
        % 0. search for non-MP transients
        
        % 1. Add columns for matched sources
        
        % 2. generate a new catalog of only matched sources
        
    end
    
    % Save products
%     if ~isempty(Args.SaveFileName)
%        
%     end
    
    
end
