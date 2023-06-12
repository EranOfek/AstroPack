function [Coadd,ResultCoadd]=procCoadd(AllSI, Args)
    % Coadd a list of processed images, estimate background and find sources in the coadd images.
    %   The function works on 2-D array of of AstroImage objects in which
    %   one dimension (default is 1) corresponds to the epoch and the other
    %   to the field (i.e., different field). Each field will be coadded,
    %   so the result is a vector of AstroImage objects which length equal
    %   to the number of fields.
    %   
    %   This is a basic generic pipeline conducting the following steps:
    %   1. Register the images of each field using their existing WCS and
    %      imProc.transIm.imwarp
    %   2. Coadd the images of each field using imProc.stack.coadd
    %   3. Measure background of coadd images using imProc.background.background
    %   4. Mask pixels dominated by source noise using imProc.mask.maskSourceNoise
    %   5. Find and measure sources using imProc.sources.findMeasureSources
    %   6. AStrometry of the coadd images using imProc.astrometry.astrometryRefine
    %   7. Photometric ZP of the coadd images using imProc.calib.photometricZP
    %   8. Match the coadd catalogs against the catsHTM MergedCat using
    %       imProc.match.match_catsHTMmerged
    % Input  : - A 2-D array of AstroImage objects in which
    %            one dimension (default is 1) corresponds to the epoch and the other
    %            to the field (i.e., different field). Each field will be coadded,
    %            so the result is a vector of AstroImage objects which length equal
    %            to the number of fields.
    %          * ...,key,val,..
    %            'EpochDim' - Dimension (1 or 2) of the epoch axis in the
    %                   input AstroImage object. Default is 1.
    %
    %            'coaddArgs' - A cell array of arguments to pass to the 
    %                   imProc.stack.coadd function.
    %                   default is {'StackArgs',{'MeanFun',@mean, 'StdFun',@tools.math.stat.nanstd, 'Nsigma',[3 3], 'MaxIter',2}};
    %            'backgroundArgs' - A cell array of arguments to pass to
    %                   the imProc.background.background function.
    %                   Default is {}.
    %            'BackSubSizeXY' - A value to pass to the SubSizeXY of the
    %                   background function (i.e., the sub images size in
    %                   which the background is calculated.
    %                   Default is [128 128].
    %            'findMeasureSourcesArgs' - A cell array of arguments to
    %                   pass to the imProc.sources.findMeasureSources
    %                   Default is {}.
    %            'ZP' - Zero point of the photometry to pass to the findMeasureSources
    %                   and psfPhotCube functions.
    %                   Default is 25.
    %            'ColCell' - A cell array of 'ColCell' argument to pass to
    %                   findMeasureSources. This indicate the columns
    %                   output of the source finding routine.
    %                   Default is {'XPEAK','YPEAK',...
    %                                'X1', 'Y1',...
    %                                'X2','Y2','XY',...
    %                                'SN','BACK_IM','VAR_IM',...  
    %                                'BACK_ANNULUS', 'STD_ANNULUS', ...
    %                                'FLUX_APER', 'FLUXERR_APER',...
    %                                'MAG_APER', 'MAGERR_APER',...
    %                                'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV'};
    %            'Threshold' - Detection threshold for source finding.
    %                   Default is 5.
    %            'astrometryRefineArgs' - A cell array of arguments to pass
    %                   to imProc.astrometry.astrometryRefine
    %                   Default is {}.
    %            'Scale' - Pixel scale of detector.
    %                   Default is 1.25 arcsec/pix.
    %            'Tran' - A Tran2D object to use for the astrometric solution.
    %                   Default is Tran2D('poly3')
    %            'CatName' - catsHTM astrometric catalog to use for the
    %                   astrometric solution.
    %                   Default is 'GAIADR3'
    %            'photometricZPArgs' - A cell array of arguments to pass to
    %                   the imProc.calib.photometricZP function.
    %                   Default is {}.
    %            'ReturnRegisteredAllSI' - If true, then the 1st input argument
    %                   AllSI will be modified and contain the registered
    %                   images.
    %                   Default is true.
    %            'CoaddLessFrac' - A fraction of images. If the fraction of
    %                   images in the coadd of a specific pixel is smaller than
    %                   this value, then the 'CoaddLessImages' bit in the bit mask image
    %                   will be flagged.
    %                   Default is 0.6.
    %            'BitName_CoaddLess' - The bit name in the bit mask image
    %                   containing the coadd less images.
    %                   Default is 'CoaddLessImages'.
    %            'HighBackNsigma' - If not empty, then will remove images
    %                   with high background. This is the number of sigmas
    %                   of the background above the median images
    %                   background, to remove.
    %                   Default is 3.
    %
    %            'MatchedS' - An optional MatchedSources object containing
    %                   the matched sources of the ind the individual
    %                   object (element per image field).
    %                   If given, then the the x/y shift between the images
    %                   will be calculated from this input, and will be
    %                   used in the imProc.transIm.imwarp function.
    %                   If empty, then will use the WCS for transformation.
    %                   Default is [].
    %            'ColX' - Column name in the input MatchedS MatchedSources that
    %                   will be used to calculate the X shift between the images.
    %                   Default is 'X1'.
    %            'ColY' - The same as 'ColX', but for the Y axis.
    %                   Default is 'Y1'.
    %            'DeleteBackBeforeCoadd' - Delete Back property from input
    %                   AstroImage object. Default is false.
    %            'DeleteVarBeforeCoadd' - Delete Var property from input
    %                   AstroImage object. Default is false.
    %            'AddGlobalMotion' - A logical indicating if to add a
    %                   keywords to the header of the coadd images that
    %                   contains information on the mean X and Y motion
    %                   between the images.
    %                   Default is true.
    %            'constructPSFArgs' - A cell array of arguments to pass to
    %                   imProc.psf.constructPSF
    %                   Default is {}.
    %            'psfFitPhotArgs' - A cell array of arguments to pass to
    %                   imProc.sources.psfFitPhot
    %                   Default is {}.
    %
    %            'CoaddMatchMergedCat' - A logical indicating if to add a
    %                   MergedCat column to the catalog of sources in the coadd
    %                   image. If true, then each source will be matched
    %                   against the catsHTM MergedCat catalog and a bit
    %                   mask of catalogs in which the source appear will be
    %                   added to the catalog.
    %                   Default is true.
    %            'MergedCat' - An optional AstroCatalog Merged catalog of all the images
    %                   to coadd. If not empty, then will copy columns
    %                   specified in 'Col2copy' from the MergedCat to the
    %                   catalogs in the coadd images.
    %                   The number of elements shold be equal to the numbre
    %                   of coadd images.
    %                   Default is [].
    %            'Col2copy' - A cell array of columns to copy from the
    %                   the MergedCat input argument to the catalog of the
    %                   coadd image.
    %                   Default is {'Nobs'}.
    % Output : - A vector of AstroImage object containing the coadd images.
    %            One image per field.
    %          - A structure array containing information regarding the
    %            coaddition process.
    % Author : Eran Ofek (Jun 2023)
    % Example: 
   
    arguments
        AllSI
        Args.EpochDim   = 1;
        
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
                                                 'MAG_APER', 'MAGERR_APER',...
                                                 'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV'};
        Args.Threshold                        = 5;
        Args.astrometryRefineArgs cell        = {};
        Args.Scale                            = 1.25;
        Args.Tran                             = Tran2D('poly3');
        Args.CatName                          = 'GAIAEDR3';
        Args.photometricZPArgs cell           = {};                                                              
        Args.ReturnRegisteredAllSI logical    = true; % false;  % if true it means that AllSI will be modified and contain the registered images
          
        Args.CoaddLessFrac                    = 0.6; % if number of imagesx in pix is below this frac, than open the CoaddLessImages bit - empty - ignore
        Args.BitName_CoaddLess                = 'CoaddLessImages';
        
        %Args.RemoveHighBackImages logical     = true;   % remove images which background differ from median back by 'HighBackNsigma' sigma
        Args.HighBackNsigma                   = 3;
        
        Args.MatchedS                         = [];
        Args.ColX                             = 'X1';   % used for shift estimate
        Args.ColY                             = 'Y1';
        
        Args.DeleteBackBeforeCoadd logical    = false;
        Args.DeleteVarBeforeCoadd logical     = false;

        Args.AddGlobalMotion logical          = true;
        Args.constructPSFArgs cell            = {};
        Args.psfFitPhotArgs cell              = {};

        Args.CoaddMatchMergedCat logical      = true;
        Args.MergedCat                        = [];
        Args.Col2copy cell                    = {'Nobs'};  % cell array of columns to copy from MergedCat to Coadd
    end
    
    SEC_DAY = 86400;
    
    if Args.EpochDim==2
        % transpose in order to make the epochs in the 1st dimension
        AllSI = AllSI.';
    end
    
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
        if FlagGoodAstrometry(Ifields)
            if isempty(Args.MatchedS)
                ResultCoadd(Ifields).ShiftX = NaN;
                ResultCoadd(Ifields).ShiftY = NaN;
            else
                ResultCoadd(Ifields).ShiftX = median(diff(Args.MatchedS(Ifields).Data.(Args.ColX),1,1), 2, 'omitnan');
                ResultCoadd(Ifields).ShiftY = median(diff(Args.MatchedS(Ifields).Data.(Args.ColY),1,1), 2, 'omitnan');
            end

            ShiftXY = cumsum([0 0; -[ResultCoadd(Ifields).ShiftX, ResultCoadd(Ifields).ShiftY]]);

            % Check that all images have astrometric solution
            FlagGoodWCS = imProc.astrometry.isSuccessWCS(AllSI(:,Ifields));

            % Remove Images with high background
            if isempty(Args.HighBackNsigma)
                FlagGood = FlagGoodWCS;
            else
                MedBack = imProc.stat.median(AllSI(:,Ifields));
                FlagGoodBack = MedBack < (median(MedBack) + Args.HighBackNsigma.*tools.math.stat.rstd(MedBack));

                FlagGood = FlagGoodWCS & FlagGoodBack;
            end
            
            % no need to transform WCS - as this will be dealt later on
            % 'ShiftXY',ShiftXY,...
            % 'RefWCS',AllSI(1,Ifields).WCS,...
            % if sum(FlagGood)<20
            %     'a'
            % end
            
            error('Need to update - copy from procMergedCoadd')

            if isempty(Args.MatchedS)
                Igood = find(FlagGood, 1, 'first');

                RegisteredImages = imProc.transIm.imwarp(AllSI(FlagGood,Ifields), AllSI(Igood, Ifields).WCS,...
                                                     'TransWCS',false,...
                                                     'FillValues',0,...
                                                     'ReplaceNaN',true,...
                                                     'CreateNewObj',~Args.ReturnRegisteredAllSI);

            else
                RegisteredImages = imProc.transIm.imwarp(AllSI(FlagGood,Ifields), ShiftXY(FlagGood,:),...
                                                     'TransWCS',false,...
                                                     'FillValues',0,...
                                                     'ReplaceNaN',true,...
                                                     'CreateNewObj',~Args.ReturnRegisteredAllSI);

            end

            % use sigma clipping...
            % 1. NOTE that the mean image is returned so that the effective gain
            % is now Gain/Nimages
            % 2. RegisteredImages has no header so no JD...

            [Coadd(Ifields), ResultCoadd(Ifields).CoaddN] = imProc.stack.coadd(RegisteredImages, Args.coaddArgs{:},...
                                                                                                 'Cube',PreAllocCube,...
                                                                                                 'StackMethod',Args.StackMethod,...
                                                                                                 'StackArgs',{'MeanFun',@tools.math.stat.nanmean, 'Nsigma',[2 2]});


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
            [Coadd(Ifields), Summary] = imProc.psf.constructPSF(Coadd(Ifields), Args.constructPSFArgs{:},'TypePSF',@single);


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
    if ~isempty(Args.MergedCat)
        [Coadd] = imProc.match.insertColFromMatched_matchIndices(Coadd, Args.MergedCat, [], 'CreateNewObj',false, 'Col2copy', Args.Col2copy);
    end
    
       


end
