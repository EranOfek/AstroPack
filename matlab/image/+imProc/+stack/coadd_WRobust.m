function [Result, CoaddN, MidJD] = coadd_WRobust(Obj, Args)
    % Weighted/robust coaddition of registeed images stored in AstroImage object.
    %   The function works on regsitered images (e.g., using
    %   imProc.transIm.register).
    %   It coadd the images using the imUtil.stack.wcoaddRobust function
    %   that include min/max rejection and sigma clipping using several
    %   methods. The header is updated.
    % Input  : - An AstroImage/AstroZOGY/AstroDiff object.
    %          * ...,key,val,... 
    %            'SubBack' - Subtract background prior to coaddition.
    %                   Default is true.
    %            'BackArgs' - A cell array of additional input arguments to
    %                   pass to the imProc.background.backVar function.
    %                   This function will be used if the Back property is
    %                   not populated.
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
    %            'RemoveMinMax' - Remove minimum and maximum values in each
    %                   pixel stack prior to sigma clipping/coaddition.
    %                   Default is true.
    %            'Niter' - Number of sigma-clipping iterations.
    %                   Default is 1.
    %            'SigmaClip' - Two-element vector of [Low High] sigma
    %                   clipping thresholds. Default is [2.5 2.5].
    %            'StdMethod' - Standard-deviation / scatter estimator
    %                   option passed to imUtil.stack.wcoaddRobust.
    %                   Default is 3.
    %            'CoaddUseMex' - Use MEX implementation in
    %                   imUtil.stack.wcoaddRobust when available.
    %                   Default is true.
    %            'AddBack' - Estimate and populate the background of the
    %                   coadded image. Default is true.
    %            'backVarArgs' - Cell array of additional arguments to pass
    %                   to imUtil.background.backVar when estimating the
    %                   background/variance of the coadd. Default is {}.
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
    %            'CoaddN' - Value to record in the header for the number of
    %                   coadded images. If NaN, use the actual number of
    %                   input images. Default is NaN.
    %            'KeyExpTime' - Header keyword containing the exposure
    %                   time. Default is 'EXPTIME'.
    %            'PreAllocCube' - Optional preallocated cube to use in
    %                   imProc.image.images2cube. Default is [].
    % Output : - An AstroImage object containing the coadd image, mask
    %            image and optional background and variance.
    %          - (CoaddN) A 2D image indicating how many images were used
    %            in each pixel.
    %          - Exposure time weighted mean of mid times of the coadd
    %            exposures (also written to header).
    % More   : - If 'SubBack' is true and some images do not have the Back
    %            property populated, then the function estimates the
    %            background using imProc.background.backVar.
    %          - If 'ScalarVar' is true, then the variance used in the
    %            weighted coaddition is the mean variance of each image,
    %            rather than the full variance image.
    %          - If 'ZP' is provided, then images are flux matched before
    %            coaddition using imProc.stack.getFluxMatch.
    %          - The output mask is generated by bitwise OR of all input
    %            masks.
    %          - The output header is generated using
    %            imProc.stack.coaddHeader.
    % Example: % Coadd all images in Obj after background subtraction:
    %          Result = imProc.stack.coadd_WRobust(Obj);
    %          % Coadd a CCDSEC region using two clipping iterations:
    %          Result = imProc.stack.coadd_WRobust(Obj, 'CCDSEC',...
    %                   [100 500 200 700], 'Niter', 2,...
    %                   'SigmaClip', [3 3]);
    %          % Coadd while using zero-point based flux matching:
    %          Result = imProc.stack.coadd_WRobust(Obj, 'ZP', 'ZP', ...
    %                   'ZP0', 25);
    % See also: imUtil.stack.wcoaddRobust, imProc.image.images2cube,
    %           imProc.stack.coaddHeader, imProc.background.backVar

    arguments
        Obj   % AstroImage, AstroDiff, AstroZOGY
        Args.SubBack         = true;
        Args.BackArgs        = {};
        Args.ScalarVar       = true;
        Args.CCDSEC          = [];
        Args.ZP              = [];  % if empty use equal flux matching
        Args.ZP0             = 25;
        
        %--- coadd ---
        Args.RemoveMinMax    = true;
        Args.Niter           = 1;
        Args.SigmaClip       = [2.5 2.5];
        Args.StdMethod       = 3;
        Args.CoaddUseMex     = true;
        Args.AddBack         = true;
        Args.backVarArgs     = {};

        %--- Mask ---
        Args.AddMask         = true;
        Args.BitCoaddUseMex  = true;
        Args.KeyNaN          = 'NaN';
        Args.KeyCoaddLess    = 'CoaddLessImages';
        Args.FracCoaddLess   = 0.8; 
        
        %--- Header ---
        Args.HeaderCopy1 logical                    = true;
        Args.NewHeader                              = [];
        Args.UpdateTimes(1,1) logical               = true;
        Args.SumExpTime(1,1) logical                = true;
        Args.UpdateImagePathKeys logical            = true;
        Args.StackMethod                            = '';
        Args.CoaddN                                 = NaN;
        Args.KeyExpTime                             = 'EXPTIME';

        %--- aux ---
        Args.PreAllocCube    = [];

    end
    StackMethod = 'wrbosut';
    
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
    % make sure Flux match ha sthe same units as the images
    FluxMatch = cast(FluxMatch, 'like',ImageCube);

    % create AstroImage for results
    Result = AstroImage;

    % coadd
    [Result.ImageData.Data, Result.VarData.Data] = imUtil.stack.wcoaddRobust(ImageCube, BackCube, 'Var',Var, 'F',FluxMatch, 'ZP',[],'ZP0',[],...
                                                                       'RemoveMinMax',Args.RemoveMinMax,'Niter',Args.Niter,'SigmaClip',Args.SigmaClip, 'StdMethod',Args.StdMethod,...
                                                                       'UseMex',Args.CoaddUseMex);

    CoaddN = Nim;

    if Args.AddBack
        Result.BackData.Data = imUtil.background.backVar(Result.ImageData.Data, Args.backVarArgs{:});
    end

    % coadd mask
    if Args.AddMask
        Result.MaskData.Dict  = Obj(1).MaskData.Dict; % copy dictionary (pointer)
        Result.MaskData.Data  = squeeze(tools.array.bitor_array(MaskCube, DimIndex, Args.BitCoaddUseMex));

        % update Mask to NaN for pixels in which CoaddN<Nim
        Result.MaskData.maskSet(isnan(Result.ImageData.Data), Args.KeyNaN);
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


end
