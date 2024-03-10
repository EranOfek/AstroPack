function [MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd, OnlyMP] = procMergeCoadd(AllSI, Args)
    % Given a list of processed images, merged their catalogs, coadd
    % their images, and produce coadd catalogs.
    %   This is a basic generic pipeline conducting the following steps:
    %   0. Input is an array of processed images in which different epochs
    %      are in different lines, while different fields are in columns.
    %   1. Merge the catalogs for each field (columns of the input array)
    %      using imProc.match.mergeCatalogs
    %   2. Search asteroids using proper motions measured in the merged
    %      catalogs and using imProc.asteroids.searchAsteroids_pmCat
    %   3. Register the images of each field using their existing WCS and
    %      imProc.transIm.imwarp
    %   4. Coadd the images of each field using imProc.stack.coadd
    %   5. Measure background of coadd images using imProc.background.background
    %   6. Mask pixels dominated by source noise using imProc.mask.maskSourceNoise
    %   7. Find and measure sources using imProc.sources.findMeasureSources
    %   8. AStrometry of the coadd images using imProc.astrometry.astrometryRefine
    %   9. Photometric ZP of the coadd images using imProc.calib.photometricZP
    %   10. Match the coadd catalogs against the catsHTM MergedCat using
    %       imProc.match.match_catsHTMmerged
    % Input  : -
    % Output : -
    % Author : Eran Ofek (Jan 2022)
    % Example: 
    
    
    arguments
        AllSI
        Args.mergeCatalogsArgs cell           = {};
        Args.MergedMatchMergedCat logical     = true;
        Args.CoaddMatchMergedCat logical      = true;
        Args.coaddArgs cell                   = {'StackArgs',{'MeanFun',@mean, 'StdFun',@tools.math.stat.nanstd, 'Nsigma',[3 3], 'MaxIter',2}};
        Args.backgroundArgs cell              = {};
        Args.BackSubSizeXY                    = [128 128];
        Args.findMeasureSourcesArgs cell      = {};
        Args.ZP                               = 25;
        Args.ColCell cell                     = {'XPEAK','YPEAK',...
                                                 'X1', 'Y1',...
                                                 'X2','Y2','XY',...
                                                 'SN','BACK_IM','VAR_IM',...  
                                                 'BACK_ANNULUS', 'STD_ANNULUS', ...
                                                 'FLUX_APER', 'FLUXERR_APER',...
                                                 'MAG_APER', 'MAGERR_APER'};
                                                 %'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV'};
        Args.Threshold                        = 5;
        Args.astrometryRefineArgs cell        = {};
        Args.Scale                            = 1.25;
        Args.Tran                             = Tran2D('poly3');
        Args.CatName                          = 'GAIAEDR3';
        Args.photometricZPArgs cell           = {};                                                              
        Args.ReturnRegisteredAllSI logical    = true; % false;  % if true it means that AllSI will be modified and contain the registered images
        Args.interp2affineArgs cell           = {};
        Args.interp2wcsArgs cell              = {};

        Args.CoaddLessFrac                    = 0.6; % if number of imagesx in pix is below this frac, than open the CoaddLessImages bit - empty - ignore
        Args.BitName_CoaddLess                = 'CoaddLessImages';
        
        %Args.RemoveHighBackImages logical     = true;   % remove images which background differ from median back by 'HighBackNsigma' sigma
        Args.HighBackNsigma                   = 3;
        
        Args.ColX                             = 'X1';   % used for shift estimate
        Args.ColY                             = 'Y1';
        Args.StackMethod                      = 'sigmaclip';      
        Args.StackArgs                        = {'MeanFun',@tools.math.stat.nanmean, 'StdFun', @tools.math.stat.std_mad, 'Nsigma',[2 2]};
        Args.Asteroids_PM_MatchRadius         = 3;

        Args.DeleteBackBeforeCoadd logical    = false;
        Args.DeleteVarBeforeCoadd logical     = false;

        Args.AddGlobalMotion logical          = true;
        Args.UseShift logical                 = true;
        Args.UseInterp2 logical               = true;
        Args.constructPSFArgs cell            = {}; % can be, e.g. {'CropByQuantile',true,'Quantile',0.999};
        Args.psfFitPhotArgs cell              = {};
        
        Args.Col2copy cell                    = {'Nobs'};  % cell array of columns to copy from MergedCat to Coadd

        Args.SelectKnownAsteroid logical      = false;
        Args.GeoPos                           = [];
        Args.OrbEl                            = [];
        Args.INPOP                            = [];
        Args.AsteroidSearchRadius             = 10;
    
        Args.HostName                         = [];
    end
    SEC_DAY = 86400;
    
    % get JD
    JD = julday(AllSI(:,1));
    
    % merge catalogs % note that the merging works only on columns of AllSI !!!
    % In principle mergeCatalogs can work on all sub images simoultanouly
    % however, if one of the ephocs in one of the sub images is missing
    % then it will fail.
    % To avoid this problem we are calling imProc.match.mergeCatalogs in a
    % loop
    
    % continue only for fields for which all visits astrometry is good
    FlagGoodAstrometry = all(imProc.astrometry.isSuccessWCS(AllSI));
    if ~all(FlagGoodAstrometry)
        warning('Some sub images have bad astrometry');
    end
    [MergedCat, MatchedS, ResultSubIm.ResZP, ResultSubIm.ResVar, ResultSubIm.FitMotion] = imProc.match.mergeCatalogs(AllSI,...
                                                                                                            Args.mergeCatalogsArgs{:},...
                                                                                                            'FlagGood',FlagGoodAstrometry,...
                                                                                                            'MergedMatchMergedCat',Args.MergedMatchMergedCat);
    
    % search for asteroids - proper motion channel
    % Used to be: ResultAsteroids.AstCrop
    [MergedCat, ResultAsteroids] = imProc.asteroids.searchAsteroids_pmCat(MergedCat,...
                                                                                  'BitDict',AllSI(1).MaskData.Dict,...
                                                                                  'JD',JD,...
                                                                                  'PM_Radius',Args.Asteroids_PM_MatchRadius,...
                                                                                  'Images',AllSI);
    
    % search for asteroids - orphan channel
    % imProc.asteroids.searchAsteroids_orphans
    
    % cross match with external catalogs
    
    % flag orphans
    
    
    % delete Back and Var before coaddition
    if Args.DeleteBackBeforeCoadd
        AllSI.deleteProp('Back');
    end
    if Args.DeleteVarBeforeCoadd
        AllSI.deleteProp('Var');
    end
    
    % coadd images
    %Nfields = numel(MatchedS);
    [Nepoch, Nfields]  = size(AllSI);
    % check if all sub images has equal size
    % if so preallocate memory for cube
    [SizeSI, SizeSJ] = sizeImage(AllSI);
    if numel(unique(SizeSI))==1 && numel(unique(SizeSJ))==1
        % all sub images have equal size
        %%% FFU: in order for this to work the PreAllocCube must be an handle object...
        PreAllocCube = []; %ImageComponent({zeros(Nepoch, SizeSI(1), SizeSJ(1), 'like',AllSI(1).Image)});
    else
        PreAllocCube = []; 
    end
        
    ResultCoadd = struct('ShiftX',cell(Nfields,1),...
                         'ShiftY',cell(Nfields,1),...
                         'CoaddN',cell(Nfields,1),...
                         'AstrometricFit',cell(Nfields,1),...
                         'ZP',cell(Nfields,1),...
                         'PhotCat',cell(Nfields,1)); % ini ResultCoadd struct
    Coadd       = AstroImage([Nfields, 1]);  % ini Coadd AstroImage
    for Ifields=1:1:Nfields
        
        io.msgLog(LogLevel.Info, '%s: procMergeCoadd: coadding field %d',Args.HostName,Ifields);
        
        if FlagGoodAstrometry(Ifields)
            ResultCoadd(Ifields).ShiftX = median(diff(MatchedS(Ifields).Data.(Args.ColX),1,1), 2, 'omitnan');
            ResultCoadd(Ifields).ShiftY = median(diff(MatchedS(Ifields).Data.(Args.ColY),1,1), 2, 'omitnan');

            ShiftXY = cumsum([0 0; -[ResultCoadd(Ifields).ShiftX, ResultCoadd(Ifields).ShiftY]]);

            % Check that all images have astrometric solution
            FlagGoodWCS = imProc.astrometry.isSuccessWCS(AllSI(:,Ifields));

            % Remove Images with high background
            if ~isempty(Args.HighBackNsigma)
                MedBack = imProc.stat.medianSparse(AllSI(:,Ifields));
                FlagGoodBack = MedBack < (median(MedBack) + Args.HighBackNsigma.*tools.math.stat.rstd(MedBack));

                FlagGood = FlagGoodWCS & FlagGoodBack;
            else
                FlagGood = FlagGoodWCS;
            end
            
            % no need to transform WCS - as this will be dealt later on
            % 'ShiftXY',ShiftXY,...
            % 'RefWCS',AllSI(1,Ifields).WCS,...
            % if sum(FlagGood)<20
            %     'a'
            % end
            
            % Note that the RefWCS is given because we are using the
            % ShiftXY option:

            
            if Args.UseShift
                
                if Args.UseInterp2
                    RegisteredImages = imProc.transIm.interp2affine(AllSI(FlagGood,Ifields), ShiftXY(FlagGood,:),...
                                                                    'WCS',AllSI(find(FlagGood,1,'first'),Ifields).WCS,...
                                                                    Args.interp2affineArgs{:});
                else


                    RegisteredImages = imProc.transIm.imwarp(AllSI(FlagGood,Ifields), ShiftXY(FlagGood,:),...
                                                     'RefWCS',AllSI(find(FlagGood,1,'first'),Ifields).WCS,...
                                                     'FillValues',0,...
                                                     'ReplaceNaN',true,...
                                                     'CreateNewObj',~Args.ReturnRegisteredAllSI);
                end

            else
                % Use WCS:
                if Args.UseInterp2
                    RegisteredImages = imProc.transIm.interp2wcs(AllSI(FlagGood,Ifields), AllSI(find(FlagGood,1,'first'),Ifields),...
                                                                 Args.interp2wcsArgs{:});
                else
                    RegisteredImages = imProc.transIm.imwarp(AllSI(FlagGood,Ifields), AllSI(find(FlagGood,1,'first'),Ifields),...
                                                     'TransWCS',true,...
                                                     'FillValues',0,...
                                                     'ReplaceNaN',true,...
                                                     'CreateNewObj',~Args.ReturnRegisteredAllSI);
                end
            end

            % use sigma clipping...
            % 1. NOTE that the mean image is returned so that the effective gain
            % is now Gain/Nimages
            % 2. RegisteredImages has no header so no JD...

            [Coadd(Ifields), ResultCoadd(Ifields).CoaddN] = imProc.stack.coadd(RegisteredImages, Args.coaddArgs{:},...
                                                                                                 'Cube',PreAllocCube,...
                                                                                                 'StackMethod',Args.StackMethod,...
                                                                                                 'StackArgs',Args.StackArgs);
                                                                                                
            % In some cases the first image of the stack is rejected, so
            % the 'DATEOBS' in the resulting Coadd may be not the same 
            % in all the subimages. Here we correct it taking the date from the first Proc image:
            Coadd(Ifields).HeaderData.setVal('DATEOBS',AllSI(1,1).HeaderData.getVal('DATEOBS'));
                                                                                             
            % Background
            Coadd(Ifields) = imProc.background.background(Coadd(Ifields), Args.backgroundArgs{:},...
                                                                          'SubSizeXY',Args.BackSubSizeXY);


            % Mask Source noise dominated pixels
            Coadd(Ifields) = imProc.mask.maskSourceNoise(Coadd(Ifields), 'Factor',1, 'CreateNewObj',false);

            % Mask pixels with less than X% of the images
            if ~isempty(Args.CoaddLessFrac)
                NregIm = numel(RegisteredImages);
                FlagCoaddLess = ResultCoadd(Ifields).CoaddN<(NregIm.*Args.CoaddLessFrac);
                maskSet(Coadd(Ifields).MaskData, FlagCoaddLess, Args.BitName_CoaddLess, 1, 'CreateNewObj',false);  %, 'DefBitDict',Args.DefBitDict);
            end
            
            % Source finding
            Coadd(Ifields) = imProc.sources.findMeasureSources(Coadd(Ifields), Args.findMeasureSourcesArgs{:},...
                                                       'RemoveBadSources',true,...
                                                       'ZP',Args.ZP,...
                                                       'ColCell',Args.ColCell,...
                                                       'Threshold',Args.Threshold,...
                                                       'CreateNewObj',false);

            % Estimate PSF
            [Coadd(Ifields), Summary] = imProc.psf.populatePSF(Coadd(Ifields), 'Method', 'new', Args.constructPSFArgs{:}, 'DataType',@single);

            % PSF photometry
            [Coadd(Ifields), ResPSF] = imProc.sources.psfFitPhot(Coadd(Ifields), 'CreateNewObj',false, 'ZP',Args.ZP, Args.psfFitPhotArgs{:});                  

            % astrometry    
            % Note that if available, will use the "X" & "Y" positions produced
            % by the PSF photometry
            MeanJD = mean(JD);
            [ResultCoadd(Ifields).AstrometricFit, Coadd(Ifields), AstrometricCat] = imProc.astrometry.astrometryRefine(Coadd(Ifields), Args.astrometryRefineArgs{:},...
                                                                                                    'WCS',AllSI(1,Ifields).WCS,...
                                                                                                    'EpochOut',MeanJD,...
                                                                                                    'Scale',Args.Scale,...
                                                                                                    'CatName',Args.CatName,...
                                                                                                    'Tran',Args.Tran,...
                                                                                                    'CreateNewObj',false);

            % add PSF FWHM to header - after astrometry, beacuse WCS is needed
            imProc.psf.fwhm(Coadd(Ifields));
           
            % photometric calibration
            % change to PSF phot...
            %CatColNameMag            = 'MAG_APER_3';
            %CatColNameMagErr   = 'MAGERR_APER_3';

            [Coadd(Ifields), ResultCoadd(Ifields).ZP, ResultCoadd(Ifields).PhotCat] = imProc.calib.photometricZP(Coadd(Ifields),...
                                                                                                        'CreateNewObj',false,...
                                                                                                        'MagZP',Args.ZP,...
                                                                                                        'CatName',AstrometricCat,...
                                                                                                        Args.photometricZPArgs{:});

            % Add GlobalMotion information to header
            % calculate tracking rate information
            if Args.AddGlobalMotion
                RelTimeDay            = JD-mean(JD);
                Par                   = polyfit(RelTimeDay, ShiftXY(:,1),1);
                GlobalMotion.ResidX   = ShiftXY(:,1) - polyval(Par, RelTimeDay);
                GlobalMotion.StdX     = std(GlobalMotion.ResidX);
                GlobalMotion.RateX    = Par(1)./SEC_DAY;
                Par                   = polyfit(RelTimeDay, ShiftXY(:,2),1);
                GlobalMotion.ResidY   = ShiftXY(:,2) - polyval(Par, RelTimeDay);
                GlobalMotion.StdY     = std(GlobalMotion.ResidY);
                GlobalMotion.RateY    = Par(1)./SEC_DAY;

                Coadd(Ifields).HeaderData.insertKey({'GM_RATEX',GlobalMotion.RateX,''});
                Coadd(Ifields).HeaderData.insertKey({'GM_STDX',GlobalMotion.StdX,''});
                Coadd(Ifields).HeaderData.insertKey({'GM_RATEY',GlobalMotion.RateY,''});
                Coadd(Ifields).HeaderData.insertKey({'GM_STDY',GlobalMotion.StdY,''});
            end


        end
    end
    
    
    % plot for LAST pipeline paper
    % semilogy(ResultCoadd(1).AstrometricFit.ResFit.RefMag, ResultCoadd(1).AstrometricFit.ResFit.Resid.*3600,'k.')
    % H=xlabel('$B_{\rm p}$ [mag]'); H.Interpreter='latex'; H.FontSize=18;                                 
    % H=ylabel('Residual [arcsec]'); H.Interpreter='latex'; H.FontSize=18;

    % semilogy(ResultCoadd(5).ZP.RefMag, abs(ResultCoadd(5).ZP.Resid),'k.')
    % H=xlabel('$B_{\rm p}$ [mag]'); H.Interpreter='latex'; H.FontSize=18;
    % H=ylabel('$|$Residual$|$ [mag]'); H.Interpreter='latex'; H.FontSize=18;
    
    % 
    
    if Args.CoaddMatchMergedCat
        % match against external catalogs
        Coadd = imProc.match.match_catsHTMmerged(Coadd, 'SameField',false, 'CreateNewObj',false);
    end
    
    % match Coadd catalog against MergedCat
    [Coadd] = imProc.match.insertColFromMatched_matchIndices(Coadd, MergedCat, [], 'CreateNewObj',false, 'Col2copy', Args.Col2copy);
    
    % adding known minor planets
    % FFU
    if Args.SelectKnownAsteroid
        [OnlyMP,~,Coadd] = imProc.match.match2solarSystem(Coadd, 'JD',[], 'GeoPos',Args.GeoPos, 'OrbEl',Args.OrbEl, 'SearchRadius',Args.AsteroidSearchRadius, 'INPOP',Args.INPOP);
    else
        OnlyMP = [];
    end


end

