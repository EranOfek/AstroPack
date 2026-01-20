function [Result, SourceLess, SubtractedImage] = multiIterExtractor(Obj, Args)
    % Source finding and multi-iteration PSF fitting.
    %   This function find, fit, and subtract sources in SNR bins.
    %   Starting with the brightest sytars to the faintest stars.
    %   The outcome is finding more stars then regular source finding.
    %   The function include the following heuristic step:
    %   For the stars in the brighteest SNR bin, an extran background and
    %   noise is added around the stars in order to avoid finding artifacts
    %   due to the finite size of the PSF.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'ExcludeEmpty' - A logical indicating if to exclude empty
    %                   images.
    %                   If true, then will not keep the shape of the output
    %                   to be the same as the shape of the input.
    %                   Default is false.
    %            'BitDict' - A BitDictionary object for the bit mask image.
    %                   Default is BitDictionary('BitMask.Image.Default')
    %            'JD' - A vector of JD of the input images.
    %                   If empty, then read JD from the header.
    %                   Default is [].
    %            'KeyJD' - Header keyword containing the JD keyword.
    %                   Will be used only if 'JD' is empty.
    %                   If empty, then will use the AstroImage/julday
    %                   method without arguments (i.e., using the header
    %                   configuration files). Default is [].
    %            'KeyGain' - Header keyword containing the image Gain
    %                   information. If gain is not available in the header
    %                   then set to 1. Default is 'GAIN'.
    %            'KeyNcoadd' - Like 'KeyGain', but for the number of
    %                   coadded images. Default is 'NCOADD'.
    %
    %            --- Background arguments ---
    %            'backVarArgs' - A cell array of additional arguments to
    %                   pass to the background and variance estimation
    %                   function: imProc.background.backVar.
    %                   Default is {'Block',[128 128], 'Method',@imUtil.background.modeVar_LogHist, 'MethodArgs',{{'MinVal',5, 'MaxVal',3000},{}}}
    %            'ReCalcBackIter' - A list of iterations indices in which
    %                   to recalculate the background and variance of the image.
    %                   Default is [].
    %
    %            --- PSF arguments ---
    %            'ReCalcPsfIter' - A vector of iteration indices in which
    %                   to re-calculate the PSF from the data. Default is [].
    %            'UseOriginalPSF' - A logical indicating if to use the PSF
    %                   already attached to the input AstroImage. If false,
    %                   then the PSF is re-calculated. This can be forced by
    %                   'ReCalcPsfIter'. Default is true.
    %            'populatePSFArgs' - A cell array of additional arguments
    %                   to pass to imProc.psf.populatePSF when constructing
    %                   or updating the PSF. Default is
    %                   {'CropByQuantile',false, 'SuppressWidth',2}.
    %            'RadiusPSF' - PSF radius in pixels used in the PSF
    %                   construction and cutouts.
    %                   This radius should be larger/equal then the outer annulus
    %                   radius.
    %                   Default is 12.
    %            'AperRadius' - A vector of aperture radii [pix] used for
    %                   aperture photometry. Default is [2 4 6].
    %            'Annulus' - Inner and outer radii [pix] of the sky
    %                   annulus used for local background/variance
    %                   estimation. Default is [10 12].
    %            'ThresholdPSF' - S/N threshold used to select stars for
    %                   PSF construction. Default is 100.
    %            'RangeSN' - Two-elements vector [SNmin SNmax] specifying
    %                   the S/N range of stars used for PSF construction.
    %                   Default is [100 1000].
    %            'InitPsf' - Function handle that generates the initial PSF
    %                   model used in populatePSF. Default is
    %                   @imUtil.kernel2.gauss.
    %            'InitPsfArgs' - A cell array of arguments to pass to
    %                   InitPsf when generating the initial PSF model.
    %                   Default is {[0.1; 1.2]}.
    %            'ConvFunExtendedPSF' - Function handle that generates an
    %                   extended PSF component (e.g., scattered light
    %                   wings) to be convolved with the empirical PSF.
    %                   Default is @imUtil.kernel2.sersic.
    %            'ConvFunExtendedPSF_Args' - A cell array of arguments to
    %                   pass to ConvFunExtendedPSF. Default is {[1 2 1]}.
    %
    %            --- PSF fitting arguments ---
    %            'MomRadius' - A vector of radii [pix] used for moment
    %                   measurements at each iteration. If scalar, it is
    %                   replicated to the number of iterations.
    %                   Default is [4].
    %            'psfFitPhotArgs' - A cell array of additional key/val
    %                   arguments to pass to imProc.sources.psfFitPhot.
    %                   Default is {}.
    %            'suppressEdgesArgs' - A cell array of arguments passed to
    %                   imUtil.psf.suppressEdges in order to taper/suppress
    %                   the PSF edges. Default is
    %                   {'Fun',@imUtil.kernel2.cosbell, 'FunPars',[9 10], 'Norm',true}.
    %            'UsePSFInterpolant' - A logical indicating if to use an
    %                   interpolated PSF image instead of the FFT-shifted
    %                   PSF templates returned by psfFitPhot. Default is false.
    %            'FitRadius' - A vector specifying the PSF fit radius [pix]
    %                   for each iteration. If scalar, it is replicated to
    %                   the number of iterations. Default is [3].
    %            'MaxIter' - Maximum number of iterations for the PSF fit
    %                   per source. Default is 8.
    %            'mexCutout' - A logical indicating if to use the MEX
    %                   implementation of the image cutout routines where
    %                   available. Default is true.
    %
    %            --- Source detection arguments ---
    %            'FindWithEmpiricalPSF' - A logical indicating if to use
    %                   empirical PSF templates for source detection
    %                   (true) or to use analytic PSF models defined by
    %                   'PsfFunPar' (false). Default is true.
    %            'PsfFunPar' - A cell array containing the parameters of
    %                   the analytic PSF model used when
    %                   'FindWithEmpiricalPSF' is false. Default is
    %                   {[0.1;1.0;1.5]}.
    %            'Threshold' - A vector of S/N thresholds (in sigma) for
    %                   the multi-iteration source extraction. The length
    %                   of this vector defines the number of iterations
    %                   (from brightest to faintest sources). Default is
    %                   [500 50 5].
    %            'ColCell' - A cell array of column names requested from
    %                   imProc.sources.findMeasureSources and stored in the
    %                   AstroCatalog, including positions, S/N, background,
    %                   aperture fluxes and magnitudes. Default is
    %                   {'XPEAK','YPEAK', 'X1','Y1','X2','Y2','XY',...
    %                    'SN','BACK_IM','VAR_IM', 'BACK_ANNULUS','STD_ANNULUS',...
    %                    'FLUX_APER','FLUXERR_APER','MAG_APER','MAGERR_APER'}.
    %
    %            --- Source cleaning and mask arguments ---
    %            'RemoveEdgeDist' - A scalar specifying the distance from
    %                   image edges [pix] within which sources are removed.
    %                   Set to NaN for no removal. Default is 0.
    %            'FlagCR' - A logical indicating if to flag and remove
    %                   cosmic rays during source detection/cleaning.
    %                   Default is true.
    %            'maskCR_Args' - A cell array of additional arguments to
    %                   pass to imProc.mask.maskCR when masking CRs.
    %                   Default is {}.
    %            'FlagDiffXY' - A logical indicating if to flag sources
    %                   with inconsistent X/Y positions (e.g., due to
    %                   artifacts). Default is true.
    %            'maskDiffXY_Args' - A cell array of additional arguments
    %                   to pass to imProc.mask.xpeak_x1_diff when masking
    %                   such sources. Default is {}.
    %
    %            --- Add Back/Var noise ---
    %            'AddBackNoise' - A logical indicating if to add additional
    %                   scattered light / noise around very bright sources
    %                   in the first iteration in order to suppress
    %                   artificial detections in the PSF wings. Default is true.
    %            'ScatteredLightFrac' - Fraction of bright-star flux used
    %                   to construct a scattered light / variance map
    %                   around very bright sources in the first iteration.
    %                   Default is 0.05.
    %
    %            --- Catalog column names ---
    %            'ColRA' - J2000 RA column name to add to the AstroCatalog
    %                   object. Default is 'RA'.
    %            'ColDec' - J2000 Dec column name to add to the
    %                   AstroCatalog object. Default is 'Dec'.
    %            'ColPITER' - Column name used to store the PSF iteration
    %                   index for each extracted source. Default is 'PITER'.
    %            'RedoUpIter' - A scalar or vector of iteration numbers
    %                   specifying up to which iteration bright sources are
    %                   re-measured in a dedicated final step. Default is [1].
    %            'ColPsfFlux' - AstroCatalog column name containing the PSF
    %                   flux. This is used only if 'RedoUpIter' is not
    %                   empty. The new flux estimate is written into this
    %                   column. Default is 'FLUX_PSF'
    %            'ColPsfFluxErr' - AstroCatalog column name containing the
    %                   uncertainty of the PSF flux for the bright-source
    %                   refinement. Default is 'FLUXERR_PSF'.
    %            'ColPsfMag' - AstroCatalog column name containing the PSF
    %                   magnitude for bright sources. Default is 'MAG_PSF'.
    %            'ColPsfMagErr' - AstroCatalog column name containing the
    %                   PSF magnitude error for bright sources. Default is
    %                   'MAGERR_PSF'.
    %            'ColPsfSN' - AstroCatalog column name containing the PSF
    %                   based S/N of the bright sources. Default is 'SN'.
    %            'ColPsfChi2' - AstroCatalog column name containing the
    %                   reduced chi-square (chi^2/d.o.f.) of the PSF fit
    %                   for bright sources. Default is 'PSF_CHI2DOF'.
    %            'ColFlux' - Base AstroCatalog column name for aperture
    %                   fluxes; individual aperture radii get numeric
    %                   suffixes (e.g., FLUX_APER_1, FLUX_APER_2, ...).
    %                   Default is 'FLUX_APER'.
    %            'ColFluxErr' - Base AstroCatalog column name for aperture
    %                   flux errors, with numeric suffixes per aperture
    %                   radius. Default is 'FLUXERR_APER'.
    %            'ColMag' - Base AstroCatalog column name for aperture
    %                   magnitudes, with numeric suffixes per aperture
    %                   radius. Default is 'MAG_APER'.
    %            'ColMagErr' - Base AstroCatalog column name for aperture
    %                   magnitude errors, with numeric suffixes per
    %                   aperture radius. Default is 'MAGERR_APER'.
    %
    %            --- Photometric and scattered light arguments ---
    %            'ZP' - Photometric zero point used for converting fluxes
    %                   to (luptitude-like) magnitudes in the PSF and
    %                   aperture photometry. Default is 25.
    %
    %            --- Miscellaneous arguments ---
    %            'AddSkyCoo' - A logical indicating if to add RA,Dec sky
    %                   coordinates to the final catalogs using the image
    %                   WCS, when present. Default is true.
    %            'CreateNewObj' - A logical indicating if to work on a deep
    %                   copy of the input AstroImage stack (true) or to
    %                   modify the input objects in-place (false). Default
    %                   is false.
    %            'Verbose' - A logical indicating if to print progress and
    %                   diagnostic information to the screen. Default is false.
    %            'WriteDs9Regions' - A logical indicating if to write DS9
    %                   region files with the extracted sources at each
    %                   iteration. Default is false.
    %
    % Output : - (Result) An AstroImage array, same size as the
    %            input Obj, in which the Image, Back, Var, PSF and
    %            CatData properties are updated by the multi-iteration
    %            PSF source extraction.
    %          - (SourceLess) An AstroImage array containing copies of
    %            the input images with the final (last-iteration)
    %            source-subtracted images stored in the Image
    %            property. Returned only if requested (nargout>1).
    %          - (SubtractedImage) A numeric cube of size [Ny, Nx, Niter]
    %            containing, for each processed AstroImage, the
    %            subtracted image after each iteration of the
    %            algorithm. Returned only if requested (nargout>2).
    %
    % Author : Eran Ofek (2025 Nov) 
    % Example: [AI1,AI2]=imProc.sources.multiIterExtractor(AI);

    arguments
        Obj AstroImage

        % pre subtraction treatment
        Args.ExcludeEmpty              = false;  % if true, will not keep the shape
        Args.BitDict                   = BitDictionary('BitMask.Image.Default');
        Args.JD                        = [];
        Args.KeyJD                     = [];
        Args.KeyGain                   = 'GAIN';
        Args.KeyNcoadd                 = 'NCOADD';

        % background and variance measurement:
        Args.backVarArgs               = {'Block',[128 128], 'Method',@imUtil.background.modeVar_LogHist, 'MethodArgs',{{'MinVal',5, 'MaxVal',3000},{}}};
        Args.ReCalcBackIter            = []; % list of iterations in which to re-calc the background. If 1, recalc also in the begining.

        % measure PSF
        Args.ReCalcPsfIter             = [];  % Index of iterations in which to re-calc PSF; if UseOriginalPSF=true, then no need to set this to 1.
        Args.UseOriginalPSF logical    = true;   % use the PSF already attached to the input AstroImage
        Args.populatePSFArgs cell      = {'CropByQuantile',false, 'SuppressWidth',2}; % {'CropByQuantile',true,'Quantile',0.5}
        Args.RadiusPSF                 = 12;
        Args.AperRadius                = [2, 4, 6];
        Args.Annulus                   = [10 12];
        Args.ThresholdPSF              = 100;
        Args.RangeSN                   = [100 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1; 1.2]}; %{[0.1;1.0;1.5]};  
        Args.ConvFunExtendedPSF        = @imUtil.kernel2.sersic;
        Args.ConvFunExtendedPSF_Args   = {[1 2 1]}; 
        
        
        % PSF fitting
        Args.MomRadius                 = [4];  % [pix] for each iteration % recommended MomRadius = 1.7 * FWHM ~ 3.8 (for LAST!)
        Args.psfFitPhotArgs            = {};
        Args.suppressEdgesArgs         = {'Fun',@imUtil.kernel2.cosbell, 'FunPars', [9, 10], 'Norm', true};
        Args.UsePSFInterpolant         = false;
        Args.FitRadius                 = [3];% PSF fit radius at each iteration
        Args.MaxIter                   = 8;
        Args.mexCutout                 = true;

        
        % source detection:        
        Args.FindWithEmpiricalPSF logical = true;
        Args.PsfFunPar cell            = {[0.1;1.0;1.5]};  % search for sources                 
        Args.Threshold                 = [500 50 5]; % [50 16.5 5]; % in sigma, this also specifies the # of iterations   
        Args.ColCell cell              = {'XPEAK','YPEAK',...
                                        'X1', 'Y1',...
                                        'X2','Y2','XY',...
                                        'SN','BACK_IM','VAR_IM',...
                                        'BACK_ANNULUS', 'STD_ANNULUS', ...
                                        'FLUX_APER', 'FLUXERR_APER',...
                                        'MAG_APER', 'MAGERR_APER'};
        
        % source cleaning and mask
        Args.RemoveEdgeDist            = 0;  % NaN for non removal
        Args.FlagCR logical            = true;
        Args.maskCR_Args cell          = {};
        Args.FlagDiffXY logical        = true;
        Args.maskDiffXY_Args cell      = {};

        % add back/var noise -  Bright stars increase back/var
        Args.AddBackNoise              = true;
        Args.ScatteredLightFrac = 0.05;

        
        % Column names to add the catalog
        Args.ColRA                     = 'RA';
        Args.ColDec                    = 'Dec';
        Args.ColPITER                  = 'PITER';  % column name for PSF iteration


        % cleaning of the subtracted image:        
        %Args.RemoveMasked              = false;  % the input AI.Mask should be filled, but seems like this filter does not influence the result much ? 
        %Args.RemovePSFCore             = false;  % not decided if this is useful and correct

        Args.RedoUpIter = [];
        

        Args.ColPsfFlux        = 'FLUX_PSF';
        Args.ColPsfFluxErr     = 'FLUXERR_PSF';
        Args.ColPsfMag         = 'MAG_PSF';
        Args.ColPsfMagErr      = 'MAGERR_PSF';
        Args.ColPsfSN          = 'SN';
        Args.ColPsfChi2        = 'PSF_CHI2DOF';
        Args.ColFlux           = 'FLUX_APER';
        Args.ColFluxErr        = 'FLUXERR_APER';
        Args.ColMag            = 'MAG_APER';
        Args.ColMagErr         = 'MAGERR_APER'
        
        Args.ZP                = 25;

       
        % miscellaneous:
        %Args.DeleteInputCatalog        = true;  % delete the catalog property from the input AI stack 
        Args.AddSkyCoo                 = true;  % add RA, Dec from the AstroImage WCS if it is present 
        Args.CreateNewObj logical      = false;   
        %Args.SaveSourcelessImage logical= false; % save the cleaned sourceless image as the second result
        Args.Verbose logical           = false;  
        Args.WriteDs9Regions logical   = false;
    end
    
    % check consistency
    % if numel(Args.Threshold) > numel(Args.MomRadius) || numel(Args.Threshold) > numel(Args.FitRadius)
    %     error('The length of Args.Threshold does must comply with that of Args.MomRadius');
    % end
    Niter = numel(Args.Threshold);
    Nobj  = numel(Obj);

    % repair some parameters if needed: 
    Args.MomRadius = Args.MomRadius(:).*ones(Niter,1);
    Args.FitRadius = Args.FitRadius(:).*ones(Niter,1);
    
    % create a new object if requested  
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
     
    % exclude objects with empty images
    if Args.ExcludeEmpty
        FlagEmptyImage = Result.isemptyProperty('Image');
        Result         = Result(~FlagEmptyImage);
    end

    if isempty(Args.JD)
        JD = Result.julday('KeyJD',Args.KeyJD);
    else
        JD = Args.JD;
    end
    
    % measure background and variance if it is missing or if the object is new
    ReCalcBackIter1 = any(Args.ReCalcBackIter==1); % re-calc backgroun in 1st iteration
    FlagBack = ReCalcBackIter1 | Result.isemptyProperty('Back') | Result.isemptyProperty('Var');
    if any(FlagBack)
        % redo everything - will keep the shape
        Result = imProc.background.backVar(Result, Args.backVarArgs{:});
    end
    
    % measure PSF if it does not exist or if the user requested to re-calc
    % NB: if the input catalog is empty, the catalog struct need for PSF measurements
    % will be generated inside imUtil.psf.constructPSF by imUtil.sources.findSources 
    % at Threshold > 20 sigma, but the object's catalog property will not be populated
    FlagPSF = Result.isemptyPSF | ~Args.UseOriginalPSF; 
    if any(FlagPSF)
        % redo everything - keep the shape:
        [Result] = imProc.psf.populatePSF(Result,...
                                                   Args.populatePSFArgs{:},...
                                                   'RadiusPSF',Args.RadiusPSF,...
                                                   'Annulus',Args.Annulus,...
                                                   'ThresholdPSF',Args.ThresholdPSF,...
                                                   'RangeSN',Args.RangeSN,...
                                                   'InitPsf',Args.InitPsf,...
                                                   'InitPsfArgs',Args.InitPsfArgs,...
                                                   'RePopulatePSF',true);
    end
    

    % delete the object's input catalog 
    % if the catalog is not removed, it may conflict with the new ones 
    FlagPopCat = Result.sizeCatalog>0;
    if any(FlagPopCat)
        Result(FlagPopCat).deleteProp('CatData');
    end
    
    % Define AstroImage of subtracted sources
    if nargout>1
        ExtraOutput = true;
    else
        ExtraOutput = false;
    end
    if ExtraOutput
        SourceLess = AstroImage(size(Result));   
    end

    % get GAIN and NCOADD
    Keys = Result.getStructKey({Args.KeyGain, Args.KeyNcoadd});

    % find and measure sources using multi-iteration PSF fitting    
    FWHM = nan(Nobj,1);
    for Iobj=1:1:Nobj
        if Args.Verbose
            fprintf('Image %d of %d \n',Iobj,Nobj);
        end    
        %Result(Iobj).Table = [];
        FWHM(Iobj) = Result(Iobj).PSFData.fwhm;

        if isnan(Keys(Iobj).(Args.KeyGain))
            Gain = 1;
        else
            Gain = Keys(Iobj).(Args.KeyGain);
        end
        if isnan(Keys(Iobj).(Args.KeyNcoadd))
            Ncoadd = 1;
        else
            Ncoadd = Keys(Iobj).(Args.KeyNcoadd);
        end
        


        % PSFTemplate = Args.InitPsf(Args.InitPsfArgs{:});
        % PSFTemplate = repmat(single(0), )

        
        % we need a deep copy here, otherwise, the initial image is not kept in the AI!
        AI              = Result(Iobj).copy;                                    % this AI will be iterated for each Obj 
        AI.CatData      = [];
        %AI.Table        = [];
        Cat             = AstroCatalog([1 Niter]);                              % catalogs produced at each iter, merged afterwards 
        
        SizeImage       = size(AI.ImageData.Image);
        SourceImage     = repmat(single(0),SizeImage(1), SizeImage(2), Niter);    % source image after each iteration
        SumSourceImage  = repmat(single(0),SizeImage(1), SizeImage(2));    % source image after each iteration
        if ExtraOutput
            SubtractedImage = repmat(single(0), SizeImage(1), SizeImage(2), Niter);    % subtracted image after each iteration
        end

        for Iiter=1:1:Niter     

            % find sources (without background recalculation) with the empirical PSF or with a set of Gaussians                     
            % in each case the sources identified as CRs are removed from the catalog
            % NB: 1. If 'Psf' is provided, this parameter overrides the PsfFun input argument
            %     2. When a PSF stamp is used for source detection, the output catalog does not contain SN_3, just SN_1 and SN_2!                
            if Args.FindWithEmpiricalPSF                   
                
                if Iiter==1
                    SizePSF = size(AI.PSFData.DataPSF);
                    if ~isempty(Args.ConvFunExtendedPSF)
                        ConvExtended = Args.ConvFunExtendedPSF(Args.ConvFunExtendedPSF_Args{:}, SizePSF);
                        PSFTemplate = repmat(single(0), [SizePSF, 3]);
                        PSFTemplate(:,:,3) = conv2(AI.PSFData.DataPSF, ConvExtended, 'same');
                    else
                        PSFTemplate = repmat(single(0), [SizePSF, 2]);
                    end
                    
                    PSFTemplate(:,:,1) = Args.InitPsf(Args.InitPsfArgs{1}(1),size(AI.PSF)); % a narrow delta-like PSF for CR rejection                 
                    PSFTemplate(:,:,2) = AI.PSFData.DataPSF; % the empirical PSF
                    
                    % check the information content overlap between the PSF
                    % and extended PSF:
                    % tools.math.filter.infoOverlapFilters(squeeze(PSFTemplate(:,:,3)),squeeze(PSFTemplate(:,:,2)))

                    %PSFTemplate = AI.PSF; % the empirical PSF
                    
                end

                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                                                          'RemoveEdgeDist',Args.RemoveEdgeDist,...
                                                          'MomPar',{'MomRadius',Args.MomRadius(Iiter), 'AperRadius',Args.AperRadius, 'Annulus',Args.Annulus},...
                                                          'Psf',PSFTemplate,...
                                                          'FlagCR',Args.FlagCR,'maskCR_Args',Args.maskCR_Args,...
                                                          'FlagDiffXY',Args.FlagDiffXY, 'maskDiffXY_Args',Args.maskDiffXY_Args,...
                                                          'ColCell',Args.ColCell,...
                                                          'BitDict',Args.BitDict,...
                                                          'JD',JD,...
                                                          'ZP',Args.ZP);
               
                ColSN = 'SN_2';            
                %clear PSFTemplate
            else
                AI = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                                                          'RemoveEdgeDist',Args.RemoveEdgeDist,...
                                                          'MomPar',{'MomRadius',Args.MomRadius(Iiter), 'AperRadius',Args.AperRadius, 'Annulus',Args.Annulus},...
                                                          'PsfFunPar',Args.PsfFunPar,...
                                                          'FlagCR',Args.FlagCR,'maskCR_Args',Args.maskCR_Args,...
                                                          'FlagDiffXY',Args.FlagDiffXY, 'maskDiffXY_Args',Args.maskDiffXY_Args,...
                                                          'ColCell',Args.ColCell,...
                                                          'BitDict',Args.BitDict,...
                                                          'JD',JD,...
                                                          'ZP',Args.ZP);
                ColSN = 'SN_2';
            end                         
            
            NumSrc = height(AI.CatData.Catalog);
            
            if Args.Verbose
                fprintf('Iter. %d: S/N > %d, mean bkg = %.0f, mean var = %.0f, Nobj: %d\n',...
                                    Iiter,Args.Threshold(Iiter),mean(AI.Back,'all','omitnan'),mean(AI.Var,'all','omitnan'),NumSrc);
            end            
            % insert a column with iteration number into the source catalog
            AI.CatData = insertCol(AI.CatData, repmat(Iiter,1,NumSrc)', Inf, 'ITER', {''});
            
            % measure the PSF (if we believe that the PSF is flux-dependent?) or use the previous one 
            ReCalcPSF = any(Args.ReCalcPsfIter==Iiter);
            if ReCalcPSF || isempty(AI.PSF)
                AI = imProc.psf.populatePSF(AI,Args.populatePSFArgs{:});                
            end
            
            % PSF photometry
            [AI, Res] = imProc.sources.psfFitPhot(AI,'ColSN',ColSN,'FitRadius',Args.FitRadius(Iiter), 'MaxIter',Args.MaxIter, 'ZP',Args.ZP, Args.psfFitPhotArgs{:});  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter

            
            % use either a) interpolation (experimental) or b) FFT shift (obtained above as Res.ShiftedPSF) + edge suppression
            if Args.UsePSFInterpolant
                ShiftedPSF = imUtil.trans.shift_interp(AI.SPFData.Data, Res.DX, Res.DY, 'Norm',true);
            else
                ShiftedPSF = Res.ShiftedPSF;
                % already done in PSF construction
                %ShiftedPSF = imUtil.psf.suppressEdges(Res.ShiftedPSF, Args.suppressEdgesArgs{:}); 
            end            
    
            % subtract the newly found and measured sources:
            % 1. construct a source image
            % 2. subtract the source image from the current image
            [CubePSF, XY]                = imUtil.art.createSourceCube(ShiftedPSF, [Res.RoundY Res.RoundX], Res.Flux, ...
                                                                        'Recenter', false,'PositivePSF',false, 'FunEdge',[]);
           
            SourceImage(:,:,Iiter)       = imUtil.art.addSources(repmat(single(0), SizeImage), permute(CubePSF,[2,1,3]),XY,...
                                                                        'Oversample',[],'Subtract',false);  
            SumSourceImage = SumSourceImage + SourceImage(:,:,Iiter);
            Subtracted                   = AI.ImageData.Image - SourceImage(:,:,Iiter);  
            
            
            % optionaly set pixels with Mask > 0 to the background values (in practice this does not influence the result?)
            %if Args.RemoveMasked
            %    Ind = AI.Mask > 0;                
            %    Subtracted(Ind) = AI.Back(Ind);
            %end

            % NOT CLEAR WHAT IS THIS FOR?
            % optionaly set pixels with reconstructed source PSFs to the background values 
            %if Args.RemovePSFCore
            %    Ind = SourceImage(:,:,Iiter) > 0;
            %    Subtracted(Ind) = AI.Back(Ind); % need to be tested and improved to operate only on a 3x3 (5x5?) pixel core
            %end              
                        
            Cat(Iiter)                   = AI.CatData; 
            
            % add PITER to catalog
            Nsrc = size(Cat(Iiter).Catalog, 1);
            Cat(Iiter).insertCol(Iiter.*ones(Nsrc,1),Inf, Args.ColPITER,'');
            
            AI.Image                     = Subtracted; % replace the image with the subtracted image
            
            if ExtraOutput
                SubtractedImage(:,:,Iiter)   = Subtracted; % populate the array of subtracted images 
            end

            % re-measure background at each Iter > 1 if Args.ReCalcBack = true and add source noise to the variance                
            ReCalcBackIterI = any(Args.ReCalcBackIter==Iiter); % re-calc backgroun in Iiter iteration
            if ReCalcBackIterI
                FlagBack         = ReCalcBackIterI | Result.isemptyProperty('Back') | Result.isemptyProperty('Var');
                Result(FlagBack) = imProc.background.backVar(Result, Args.backVarArgs{:});
            end
            
            % add local variance from the sources revealed at all the previous iterations
            % This is not enough - for bright stars the PSF is more
            % extended and the star edges are not subtracted
            AI.VarData.Image  = AI.VarData.Image  + SumSourceImage./(Ncoadd.*Gain);
            AI.BackData.Image = AI.BackData.Image + SumSourceImage;  


            if Iiter==1 && Args.AddBackNoise
                % Add noise/back around bright sources
                %GK = imUtil.kernel2.gauss(FWHM);
                %AI.Var  = conv2(AI.Var, GK, 'same'); 
                LK = imUtil.kernel2.lorentzian(4,[101 101]);
                CK = imUtil.kernel2.circ(ceil(2.*FWHM(Iobj)),[15 15]);
                CK = CK./max(CK,[],'all');
                EdgesVarMap = repmat(single(0), SizeImage);
                ColData = AI.CatData.getCol({'XPEAK','YPEAK','FLUX_APER_3'});
                LinIndex = imUtil.image.sub2ind_fast(SizeImage, ColData(:,2), ColData(:,1));
                %LinIndex = sub2ind(SizeImage, AI.CatData.Table.YPEAK, AI.CatData.Table.XPEAK);
                %EdgesVarMap(LinIndex) = AI.CatData.Table.FLUX_APER_3;
                
                MinFluxFlag = ColData(:,3)>1e5;
                EdgesVarMap(LinIndex) = ColData(:,3).*Args.ScatteredLightFrac.*max(1, log10(ColData(:,3)./1e5)).*MinFluxFlag;
                %AI.Back(AI.Image>5000) = 5000;
                ConvBright = conv2(EdgesVarMap, LK, 'same');
                ConvCore   = conv2(EdgesVarMap, CK, 'same')./Args.ScatteredLightFrac;
                AI.VarData.Image  = AI.VarData.Image  + ConvBright./(Ncoadd.*Gain) + ConvCore;
                AI.BackData.Image = AI.BackData.Image + ConvBright + ConvCore;
            end
            

            % write region files with extracted objects 
            if Args.WriteDs9Regions
                writeDS9region(AI, Args);
            end 


            % Yes - this is needed
            % Should be replaced with a cleaner way...
            AI.CatData = []; 

        end % end of iterations  

        
        if ~isempty(Args.RedoUpIter)
            if Niter<=Args.RedoUpIter
                error('Niter is <= RedoUpIter - does not make sense');
            end
            % yet another iteration for the bright sources only
            SubImageFaint = Result(Iobj).ImageData.Image - sum(SourceImage(:,:,(Args.RedoUpIter+1):end),3) - Result(Iobj).BackData.Image;
            
            % merge the Catalogs
            CatBright = Cat(1:Args.RedoUpIter).merge;
            CatFaint  = Cat((Args.RedoUpIter+1):end).merge;
            % do aper phot only on bright stars
            BrightXY = CatBright.getXY;

            % See alos:
            % R=imProc.sources.aperPhot(AI);

            % perform only aperture photometry on brightest sources
            [Cube] = imUtil.cut.image2cutouts(SubImageFaint, BrightXY(:,1), BrightXY(:,2), Args.RadiusPSF, 'mexCutout',Args.mexCutout, 'Circle',false);
            ResAperBright = imUtil.sources.aperPhotCube(Cube, 'AperRad',Args.AperRadius, 'AnnulusRad',Args.Annulus);

            PsfHalfSize = (size(Result(Iobj).PSFData.Data,1)-1)./2;
            [Cube] = imUtil.cut.image2cutouts(SubImageFaint, BrightXY(:,1), BrightXY(:,2), PsfHalfSize, 'mexCutout',Args.mexCutout, 'Circle',false);
            ResPsfBright  = imUtil.sources.psfPhotCube(Cube, 'FitRadius',Args.FitRadius(1),...
                                                               'PSF',Result(Iobj).PSFData.Data,...
                                                               'MaxIter',Args.MaxIter,...
                                                               'ZP',Args.ZP,...
                                                               Args.psfFitPhotArgs{:});


            % insert aper phot data to catalog of Cat(1:Args.RedoUpIter)
             
            Naper      = numel(Args.AperRadius);
            ColFlux    = tools.cell.cellNumericSuffix(Args.ColFlux, (1:Naper));
            ColFluxErr = tools.cell.cellNumericSuffix(Args.ColFluxErr, (1:Naper));
            ColMag     = tools.cell.cellNumericSuffix(Args.ColMag, (1:Naper));
            ColMagErr  = tools.cell.cellNumericSuffix(Args.ColMagErr, (1:Naper));

            % order of elements in ColsToAdd and FluxMagData must be
            % consistent!
            ColsToAdd  = [ColFlux, ColFluxErr, ColMag, ColMagErr, Args.ColPsfFlux, Args.ColPsfMag, Args.ColPsfMagErr, Args.ColPsfSN, Args.ColPsfChi2];
            %[C1{1:Naper.*2}] = deal('');
            %[C2{1:Naper.*2}] = deal('mag');
            %ColUnits         = [C1, C2];

            FluxMagData = [ResAperBright.AperPhot,...
                           ResAperBright.AperPhotErr,...
                           convert.luptitude(ResAperBright.AperPhot, 10.^(0.4.*Args.ZP)),...
                           1.086.*ResAperBright.AperPhotErr./ResAperBright.AperPhot,...
                           ResPsfBright.Flux,...
                           convert.luptitude(ResPsfBright.Flux, 10.^(0.4.*Args.ZP)),...
                           1.086./ResPsfBright.SNm,...
                           ResPsfBright.SNm,...
                           ResPsfBright.Chi2./ResPsfBright.Dof];


            % update also Chi2?
             
            CatBright.replaceCol(FluxMagData, ColsToAdd);
            
            Result(Iobj).CatData.Catalog = [CatBright.Catalog; CatFaint.Catalog];
            Result(Iobj).CatData.ColNames = CatBright.ColNames;
        else
            % merge the catalogs of objects extracted at all the iterations
            Result(Iobj).CatData = merge(Cat);
        end




        

        % Cleaning:
        % remove sources on edge
        % mask CR
        % remove CR

        % remove bad sources
            % % works only for Gaussian PSF
            % if Args.FlagCR && ~isemptyImage(Obj(Iobj), 'Mask')
            %     Result(Iobj) = imProc.mask.maskCR(Result(Iobj), Args.maskCR_Args{:});
            % end
            % if Args.FlagDiffXY
            %     Result(Iobj) = imProc.mask.xpeak_x1_diff(Result(Iobj), Args.maskDiffXY_Args{:});
            % end
            % 
            % if Args.RemoveBadSources
            %     [Result(Iobj)] = imProc.sources.cleanSources(Result(Iobj), 'SigmaPSF',Args.PsfFunPar{1}(1:2),...
            %                                                                'ColNamsSN',{'SN_1','SN_2'},...
            %                                                                'RemoveBadSources',Args.RemoveBadSources,...
            %                                                                'CreateNewObj',false);
            % end
               
        % add RA, Dec from the object's WCS if it is present
        if Args.AddSkyCoo && ~isempty(Result(Iobj).WCS)
            XY        = Result(Iobj).CatData.getXY();
            [RA, Dec] = Result(Iobj).WCS.xy2sky(XY(:,1), XY(:,2));
            Result(Iobj).CatData = insertCol(Result(Iobj).CatData, RA, Inf, Args.ColRA, {''});
            Result(Iobj).CatData = insertCol(Result(Iobj).CatData, Dec, Inf, Args.ColDec, {''});
            Result(Iobj).CatData.sortrows(Args.ColDec);    
        end        
        
        % save a copy of the AI object with the image replaced by the final subtracted image
        if ExtraOutput
            SourceLess(Iobj)       = Result(Iobj).copy;
            SourceLess(Iobj).Image = SubtractedImage(:,:,Niter); % or just  = Subtracted ?
        end        

        if Args.Verbose
            fprintf('Total %d objects extracted \n',height(Result(Iobj).CatData.Catalog));
        end
    end    
    % Find diffraction spikes?    
    % Cleaning?     
end

%------------------------------------------------

function writeDS9region(AI, Args)
    % help function for writing ds9 region file

    RegName = sprintf('~/%s_it%d.reg',AI.getStructKey('OBJECT').OBJECT,Iiter);
    if     Iiter == 1
        Clr = 'blue';
    elseif Iiter == 2
        Clr = 'red';
    elseif Iiter == 3
        Clr = 'green';
    end
    DS9_new.regionWrite([AI.CatData.getCol('X') AI.CatData.getCol('Y')],...
                        'FileName',RegName,'Color',Clr,'Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);

end
