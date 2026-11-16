function [Result, CoaddN, MidJD, EffectiveGain] = coadd_Proper(Obj, Args)
    % Proper coaddition (Zackay & Ofek 2017) of registered images stored in AstroImage object.
    %   The function works on registered images (e.g., using
    %   imProc.transIm.register).
    %   It coadds the images using the imUtil.properCoadd.combine_proper
    %   function, using the image cube, variance information, flux matching
    %   factors, and the PSF cube. The header is updated.
    % Input  : - An AstroImage/AstroZOGY/AstroDiff object.
    %          * ...,key,val,...
    %            'SubBack' - Subtract background prior to coaddition.
    %                   Default is true.
    %            'BackArgs' - A cell array of additional input arguments to
    %                   pass to the imProc.background.backVar function.
    %                   This function will be used if the Back property is
    %                   not populated, and also for background estimation of
    %                   the coadded image when 'AddBack' is true.
    %            'ScalarVar' - Logical indicating if to replace the
    %                   variance image of each input image by a scalar
    %                   variance estimated from the mean variance over the
    %                   image. If false, use the full variance cube.
    %                   Default is true.
    %            'CCDSEC' - CCDSEC region [Xmin Xmax Ymin Ymax] to extract
    %                   from each image prior to coaddition. If empty use
    %                   the full image. Default is [].
    %            'ZP' - Zero-point values or keyword/property from which to
    %                   derive the flux matching factors between images. If
    %                   empty then assume equal flux scaling for all images.
    %                   Default is [].
    %            'ZP0' - Reference zero point to which all images are
    %                   scaled when 'ZP' is provided. Default is 25.
    %            'AddBack' - Estimate and populate the background of the
    %                   coadded image. Default is true.
    %            'UnitVar' - If true, set the variance of the coadd to 1, its
    %                   value by construction, instead of the variance
    %                   measured by backVar (the background is still
    %                   measured). On dense fields the measured variance
    %                   changes with a sub-pixel shift of the same coadd
    %                   (issue #1466). Default is true.
    %            'AddMask' - Coadd the mask images into the output mask
    %                   image using bitwise OR. Default is true.
    %            'BitCoaddUseMex' - Use MEX implementation in
    %                   tools.array.bitor_array when available.
    %                   Default is true.
    %            'HeaderCopy1' - Copy header template from the first image.
    %                   Default is true.
    %            'NewHeader' - Optional header object/content to use as the
    %                   basis for the output header. Default is [].
    %            'UpdateTimes' - Update time-related header keywords in the
    %                   output header. Default is true.
    %            'SumExpTime' - Sum the exposure times into the output
    %                   header. Default is true.
    %            'UpdateImagePathKeys' - Update path/name related header
    %                   keywords in the output header. Default is true.
    %            'StackMethod' - String describing the stack method to
    %                   write to the header. If empty, the function uses its
    %                   internal method label.
    %            'KeyExpTime' - Header keyword containing the exposure
    %                   time. Default is 'EXPTIME'.
    %            'ZeroWeightBits' - Mask bit names whose pixels get zero weight
    %                   in the 'robust' solver, so that the other frames
    %                   fill them in. Pixels with such a bit in every frame
    %                   keep the solver's estimate from the surrounding
    %                   pixels; their mask bit is kept in the coadd mask.
    %                   Not used by 'fft'. Default is {} (none): on a
    %                   reference built from visit coadds, {'Saturated'}
    %                   leaves the cores of stars saturated in every input
    %                   unconstrained, the fill-in there rings to a
    %                   negative core flux, and the extraction then loses
    %                   the brightest stars and raises the background under
    %                   faint ones (issue #1401).
    %            'SourceNoise' - For 'robust': add the source-dependent
    %                   variance of Zackay & Ofek Appendix C (Eq. C2) to the
    %                   background variance, so that valid bright-star
    %                   cores are not rejected. See
    %                   imUtil.properCoadd.properCoaddLinear. Default is true.
    %            'ModelErr' - Fractional model error for 'SourceNoise'.
    %                   Default is 0.01.
    %            'AstErr' - Astrometric error for 'SourceNoise': a header
    %                   keyword in arcsec, divided by 'KeyPixScale' per
    %                   image (missing -> 0.02 pix), or a scalar/vector in
    %                   pixels. Default is 'AST_ARMS'.
    %            'KeyPixScale' - Header keyword of the pixel scale
    %                   [arcsec/pix]. Default is 'PIXSCALE'.
    %            'MaxIter' - For 'robust': maximum number of IRLS
    %                   (outlier-rejection) iterations, passed to
    %                   imUtil.properCoadd.properCoaddLinear. Rejection
    %                   should stay mild (issue #1401). Default is 2.
    %            'PreAllocCube' - Optional preallocated cube to use in
    %                   imProc.image.images2cube. Default is [].
    % Output : - An AstroImage object containing the proper coadd image,
    %            PSF image, mask image and optional background.
    %          - (CoaddN) Number of input images used in the coadd.
    %          - Exposure time weighted mean of mid times of the coadd
    %            exposures (also written to header).
    %          - (EffectiveGain) The effective gain [e-/ADU] of the proper
    %            coadd output image, i.e. the factor for which the source
    %            Poisson variance is Flux/EffectiveGain in the output units.
    %            The proper coadd is normalized to unit background variance,
    %            so this is NOT the mean-coadd gain (Gain*N); it is
    %                EffectiveGain = Gain * (sum_j F_j^2/V_j)^1.5 / sum_j F_j^3/V_j^2
    %            where F_j = transparency (1/flux-match factor) and V_j = background variance
    %            of input image j (and Gain = input single-image gain).
    %            For N identical inputs this reduces to Gain*sqrt(N)*sigma.
    %            Derived by conserving the integrated source-flux variance
    %            through the ZOGY matched-filter combination (see issue
    %            #1251); exact for a real/symmetric, shape-matched PSF.
    % Notes   : - If 'SubBack' is true and some images do not have the Back
    %            property populated, then the function estimates the
    %            background using imProc.background.backVar.
    %          - If 'ScalarVar' is true, then the variance used in the
    %            proper coaddition is the mean variance of each image,
    %            rather than the full variance image.
    %          - If 'ZP' is provided, then images are flux matched before
    %            coaddition using imProc.stack.getFluxMatch.
    %          - The PSF cube is generated using imProc.psf.psf2cube and
    %            is passed to imUtil.properCoadd.combine_proper.
    %          - The output mask is generated by bitwise OR of all input
    %            masks.
    %          - NaN pixels (e.g., registration borders) are zero-filled
    %            before the solve. With 'ProperMethod'='robust' they also get
    %            zero per-pixel weight, so they do not enter the solution;
    %            with 'fft' the closed form takes one weight per frame, so
    %            the zero fill enters at full weight there - an inherent
    %            approximation of that method. Pixels with no valid frame at
    %            all are NaN in the output image (issue #1399).
    %          - With 'robust', pixels flagged by 'ZeroWeightBits'
    %            (none by default) get zero weight (issue #1401); where
    %            they are flagged in every frame the output keeps the
    %            solver's fill-in, not NaN.
    %          - The output header is generated using
    %            imProc.stack.coaddHeader.
    % Author : Eran Ofek (Apr 2026)
    % Example: % Proper coadd all images in Obj after background subtraction:
    %          Result = imProc.stack.coadd_Proper(Obj);
    %          % Proper coadd a CCDSEC region:
    %          Result = imProc.stack.coadd_Proper(Obj, 'CCDSEC',...
    %                   [100 500 200 700]);
    %          % Proper coadd while using zero-point based flux matching:
    %          Result = imProc.stack.coadd_Proper(Obj, 'ZP', 'ZP', ...
    %                   'ZP0', 25);

    arguments
        Obj   % AstroImage, AstroDiff, AstroZOGY
        Args.ProperMethod    = 'robust';
        Args.SubBack         = true;
        Args.BackArgs        = {};
        %Args.FinalBackArgs   = {'Method',@imUtil.background.modeVar_Hist};
        Args.backVarArgs     = {'Method',@imUtil.background.modeVar_Hist};
        Args.ScalarVar       = true;
        Args.CCDSEC          = [];
        Args.ZP              = 'PH_ZP';  % if empty use equal flux matching
        Args.ZP0             = 25;
        Args.Gain            = 1;   % input single-image gain [e-/ADU]; used for the EffectiveGain output (issue #1251) and the SourceNoise Poisson term

        %--- robust solver ---
        Args.ZeroWeightBits  = {};   % {'Saturated'} rings in the cores of stars saturated in every input (issue #1401)
        Args.SourceNoise logical = true;
        Args.ModelErr        = 0.01;
        Args.AstErr          = 'AST_ARMS';
        Args.KeyPixScale     = 'PIXSCALE';
        Args.MaxIter         = 2;

        Args.SizePSF         = [25 25];

        %--- coadd ---
        Args.AddBack         = true;
        Args.UnitVar logical = true;   % coadd variance = 1 by construction (issue #1466)

        %--- Mask ---
        Args.AddMask         = true;
        Args.BitCoaddUseMex  = true;
        Args.KeyNaN          = 'NaN';
        Args.KeyCoaddLess    = 'CoaddLessImages';
        Args.FracCoaddLess   = 0.6; 

        %--- Header ---
        Args.HeaderCopy1 logical                    = true;
        Args.NewHeader                              = [];
        Args.UpdateTimes(1,1) logical               = true;
        Args.SumExpTime(1,1) logical                = true;
        Args.UpdateImagePathKeys logical            = true;
        Args.StackMethod                            = '';
        Args.KeyExpTime                             = 'EXPTIME';

        %--- aux ---
        Args.PreAllocCube    = [];

    end
    StackMethod = 'proper';
    
    Nim = numel(Obj);
    DimIndex  = 3;
    % is background needed?
    if Args.SubBack
        % Get background
        Flag = Obj.isemptyProperty('Back');
        Obj(Flag) = imProc.background.backVar(Obj(Flag), Args.BackArgs{:});
    
        [ImageCube, BackCube, VarCube, MaskCube] = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC, 'DimIndex',DimIndex,...
                                                                                 'DataProp',{'ImageData','BackData', 'VarData', 'MaskData'},...
                                                                                 'DataPropIn','Data',...
                                                                                 'Cube',Args.PreAllocCube);
        ImageCube = ImageCube - BackCube; % subtract background
    else
        % no background subtraction is needed
        [ImageCube, VarCube, MaskCube] = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC, 'DimIndex',DimIndex,...
                                                                                 'DataProp',{'ImageData', 'VarData', 'MaskData'},...
                                                                                 'DataPropIn','Data',...
                                                                                 'Cube',Args.PreAllocCube);
        BackCube = [];
    end

    % convert Var cube to scalar per pimage
    if Args.ScalarVar
        Var = squeeze(mean(VarCube,[1 2], 'omitnan'));
    else
        Var = VarCube;
    end
    if ~strcmp(class(ImageCube), class(Var))
        Var = cast(Var, 'like',ImageCube);
    end

    % flux matching parameters
    if isempty(Args.ZP)
        % Equal flux matching
        FluxMatch = ones(Nim,1);
        ZP0       = NaN;
    else
        [FluxMatch,ZP0] = imProc.stack.getFluxMatch(Obj, 'ZP',Args.ZP, 'ZP0',Args.ZP0);
    end
    % make sure Flux match has the same units as the images
    FluxMatch = cast(FluxMatch, 'like',ImageCube);
    % The proper coadd takes the transparency F_j (flux scale of each image),
    % the inverse of the factor that brings an image to a common zero point
    % (issue #1462).
    Transp    = 1./FluxMatch;

    % create AstroImage for results
    Result = AstroImage;

    % coadd
    CubePSF   = imProc.psf.psf2cube(Obj, 'StampSize',Args.SizePSF);
    
    PixNaN = isnan(ImageCube);
    % Pixels with a ZeroWeightBits bit (e.g., saturated: they hold the clip
    % level, not the flux) are excluded like missing pixels in the per-pixel
    % weights of the 'robust' solver, and the other frames fill them in.
    PixUnused = PixNaN;
    if strcmp(Args.ProperMethod,'robust') && ~isempty(Args.ZeroWeightBits) && ~isempty(MaskCube)
        % registration may return masks without a dictionary (as procCoadd)
        if isempty(Obj(1).MaskData.Dict)
            MaskDict = BitDictionary('BitMask.Image.Default');
        else
            MaskDict = Obj(1).MaskData.Dict;
        end
        [~,~,BitsDec] = MaskDict.name2bit(Args.ZeroWeightBits);
        PixUnused = PixUnused | bitand(MaskCube, cast(BitsDec,'like',MaskCube))>0;
    end
    CoaddN = sum(~PixNaN,3);
    % for proper coaddition - replace all NaNs with background level.
    if Args.SubBack
        % background supoosed to be 0
        ImageCube(PixNaN) = 0; % replace NaN with 0
    else
        error('SubBack false is not supported');
    end

    % A zero-filled missing pixel must not enter the solve as a real zero-flux
    % measurement, so give it zero weight. Only the 'robust' solver can take a
    % per-pixel weight; for 'fft' the zero fill at full weight is an inherent
    % approximation of the closed-form method (issue #1399).
    if any(PixUnused(:))
        PixWeight = cast(~PixUnused, 'like',ImageCube);
    else
        PixWeight = 1;
    end

    % astrometric error of each frame [pix] for the source noise: a header
    % keyword in arcsec divided by the pixel scale, or numbers
    if Args.SourceNoise && ischar(Args.AstErr)
        AstErr = zeros(Nim,1);
        for Iim=1:Nim
            AstErr(Iim) = Obj(Iim).HeaderData.getVal(Args.AstErr)./Obj(Iim).HeaderData.getVal(Args.KeyPixScale);
        end
        AstErr(~isfinite(AstErr)) = 0.02;
    else
        AstErr = Args.AstErr;
    end

    switch Args.ProperMethod
        case 'fft'
            [Result.ImageData.Data, Result.PSFData.Data] = imUtil.properCoadd.combine_proper(ImageCube, CubePSF, 'F',Transp, 'Var',Var, 'Norm',true);
        case 'robust'
            % possible bug in PSF generation
            [Result.ImageData.Data, Result.PSFData.Data] = imUtil.properCoadd.properCoaddLinear(ImageCube, CubePSF, Var', PixWeight, 'SigmaIsVariance',true, 'Flux',Transp,'Robust',true, ...
                                                                    'SourceNoise',Args.SourceNoise, 'Gain',Args.Gain, 'ModelErr',Args.ModelErr, 'AstErr',AstErr, 'MaxIter',Args.MaxIter);
            % properCoaddLinear returns full size PSF
            Result.PSFData.Data = imUtil.psf.full2stampPsf(Result.PSFData.Data, Args.SizePSF, 'FullPosition','center', 'Supress',false, 'Norm',true);   % the uniPSF inputs already end at the stamp edge
        otherwise
            error('Unknown ProperMethod option');
    end

    Result.ImageData.Data = real(Result.ImageData.Data);

    % A pixel with no valid data in any frame has no measured value, so write
    % NaN there instead of the solver's smooth fill-in, following the same
    % convention as the production coadd (issue #1399).
    Result.ImageData.Data(CoaddN==0) = NaN;

    % The coadd PSF is computed from the input PSFs rather than stacked from
    % stars, so the stars behind it are the ones the input PSFs were built
    % from. Without this count Nstars stays NaN and imProc.psf.fwhm treats
    % the coadd as having no PSF, leaving FWHM blank (issue #1390).
    InPSF = [Obj.PSFData];
    Result.PSFData.Nstars = sum([InPSF.Nstars], 'omitnan');


    % coadd mask
    if Args.AddMask
        Result.MaskData.Dict  = Obj(1).MaskData.Dict; % copy dictionary (pointer)
        Result.MaskData.Data  = squeeze(tools.array.bitor_array(MaskCube, DimIndex, Args.BitCoaddUseMex));
    
        % update Mask to NaN for pixels in which CoaddN<Nim
        Result.MaskData.maskSet(CoaddN<Nim, Args.KeyNaN);
        % Update CoaddLessImages
        Result.MaskData.maskSet( CoaddN<(Args.FracCoaddLess.*Nim), Args.KeyCoaddLess);       
    end

    % Update header:
    [Result.HeaderData, MidJD] = imProc.stack.coaddHeader(Obj, 'HeaderCopy1', Args.HeaderCopy1,...
                                                      'NewHeader',Args.NewHeader,...
                                                      'UpdateTimes',Args.UpdateTimes,...
                                                      'SumExpTime',Args.SumExpTime,...
                                                      'UpdateImagePathKeys',Args.UpdateImagePathKeys,...
                                                      'StackMethod',StackMethod,...
                                                      'CoaddN',CoaddN,...
                                                      'KeyExpTime',Args.KeyExpTime);

    % measure background and variance of the coadd. Must follow coaddHeader:
    % backVar writes MEDBCK/MEDVAR/... into Result.HeaderData, and
    % coaddHeader replaces that header by a copy of the first input image
    % header, i.e. the keywords would describe the first epoch (issue #1326).
    if Args.AddBack
        % R is background-subtracted and divided by its own noise, so the
        % counts-scale settings a mean coadd uses do not apply here: the
        % histogram window admits only star pixels, and the Poisson model
        % (Back+RN2)/Ncoadd describes photons, not a normalized image. Measure
        % the variance instead (issue #1329). A later name-value pair wins, so
        % these override whatever the caller passed, as coadd_WRobust does for
        % Ncoadd.
        Result = imProc.background.backVar(Result, Args.backVarArgs{:}, ...
                                           'MethodArgs',{{},{}}, 'PoissVar',false);
    end

    % R has unit background noise by construction; the variance measured on
    % it is unstable on dense fields (issue #1466)
    if Args.UnitVar
        Result.VarData.Image = ones(size(Result.ImageData.Image), 'like',Result.ImageData.Image);
        [~,UnitCode] = imUtil.background.backDictionary('unit');
        Result.HeaderData.replaceVal({'MEANVAR','MEDVAR','VARMTHD'}, {1, 1, UnitCode}, 'Comment',{'','','unit'});
    end

    % Effective gain of the proper (ZOGY) coadd (issue #1251).
    % The proper coadd R is normalized to unit background variance, so its
    % gain is NOT the mean-coadd Gain*N. Conserving the integrated source-flux
    % variance through the matched-filter combination (the PSF-shape factor
    % cancels for a real/symmetric, shape-matched PSF) gives:
    %    EffectiveGain = Gain * (sum_j F_j^2/V_j)^(3/2) / sum_j F_j^3/V_j^2
    % with F_j = Transp (1/FluxMatch) and V_j = per-image background variance (the same
    % F and Var passed to imUtil.properCoadd.combine_proper). For N identical
    % inputs this reduces to Gain*sqrt(N)*sigma.
    Fvec = double(Transp(:));
    if Args.ScalarVar
        Vvec = double(Var(:));
    else
        Vvec = double(squeeze(mean(Var, [1 2], 'omitnan')));
        Vvec = Vvec(:);
    end
    Good = isfinite(Fvec) & isfinite(Vvec) & Vvec>0;
    SumW = sum(Fvec(Good).^2 ./ Vvec(Good));           % sum_j F_j^2/V_j
    SumD = sum(Fvec(Good).^3 ./ Vvec(Good).^2);        % sum_j F_j^3/V_j^2
    if isfinite(SumW) && isfinite(SumD) && SumD>0 && SumW>0
        EffectiveGain = Args.Gain .* SumW.^1.5 ./ SumD;
    else
        EffectiveGain = NaN;
    end
end
