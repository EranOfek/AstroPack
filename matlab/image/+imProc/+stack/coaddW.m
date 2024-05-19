function [Result] = coaddW(Obj, Args)
    % Weighted Stack/Coadd images in AstroImage object, based on measured Back, Var, and ZP.
    %   The coadd images are background subtracted and flux matched, and
    %   weighted by the Transmission/divided by invese variance.
    % Input  : - An AstriImage.
    %            If the background and variance are not populated, then
    %            running this function will populate the Back and Var
    %            properties of the input argument.
    %          - 
    %          * ...,key,val,... 
    %            'CCDSEC' - CCDSEC on which to operate:
    %                   [Xmin, Xmax, Ymin, Ymax].
    %                   Use [] for the entire image.
    %                   If not [], then DataPropIn/Out will be
    %                   modified to 'Image'.
    %            'DataPropIn' - The data property that contains the
    %                   the data in the ImageComponent.
    %                   Default is 'Data'.
    %            'SubBack - A logical indicating if to subtract the
    %                   background.
    %                   Default is true.
    %            'BackArgs' - A cell array of additional arguments to pass
    %                   to imProc.background.background.
    %                   Default is {}.
    %            'FluxMatch' - A vector of flux match values.
    %                   Each image will be divided by its corresponding value.
    %                   If empty, then do not flux match.
    %                   If char array, then this is an header keyword
    %                   containing the ZP key haeder name. The ZP key will
    %                   be used to calculate the flux matching vector.
    %            'StackMethod' - Stack method:
    %                   'wmean'
    %                   'sigmaclip'
    %                   'wsigmaclip'
    %            'StackArgs' - A cell array of additional arguments to pass
    %                   to imUtil.image.wmean_sigclip or
    %                   imUtil.image.mean_sigclip
    %                   Default is {}.
    %            'AddCoaddN' - Add the CoaddN image (i.e., numer of images
    %                   in each pixel) to the 'Exp' property. 
    %                   Default is false.
    %            'AddMask' - Add the propgated Mask image.
    %                   Default is true.
    %            'BitCoaddUseMex' - Use mex function in tools.array.bitor_array
    %                   Default is true.
    %            
    %            'HeaderCopy1' - A logical indicating if to copy
    %                   the header from the 1st coadd image.
    %                   Default is true.
    %            'NewHeader' - An header to add to the coadd
    %                   image header. This can be a 3 column cell
    %                   array, an AstroHeader or AstroImage. If
    %                   empty do nothing. Default is [].
    %            'UpdateTimes' - A logical indicatin if to add
    %                   keywords regarding the number of coadded
    %                   images and update the EXPTIME and MIDJD.
    %                   Default is true.
    %            'SumExpTime' - A logical indicating if to sum
    %                   the EXPTIME in the new header, or to use
    %                   the mean (false). Default is true.
    %            'UpdateImagePathKeys' - A logical indicating if to
    %                   add the LEVEL, SUBLEVEL and CROPID keywords to
    %                   header. Default is true.
    %            'KeyExpTime' - EXPTIME header keyword name.
    %                   Default is 'EXPTIME'.
    %
    %            'Cube' - Pre allocated cube. This can be a
    %                   pre-allocated cube with the exact same size
    %                   needed by the function. If provided, this
    %                   will be used instaed of allocating new
    %                   memory using the zeros command.
    %                   If empty, then the Cube will be allocated.
    %                   Default is [].           
    % Output : - An AstroImage object containing the coadd image.
    % Author : Eran Ofek (2024 May) 
    % Example: 

    arguments
        Obj
     
        Args.CCDSEC                                 = [];
        Args.DataPropIn char                        = 'Data';

        Args.SubBack logical                        = true;
        Args.BackArgs                               = {};
        Args.FluxMatch                              = 'ZP';     % [] - no, vector of numbres or header key name of ZP [mag]
        Args.StackMethod                            = 'wmean';
        Args.StackArgs cell                         = {};
        Args.AddCoaddN logical                      = false;
        Args.AddMask logical                        = true;
        Args.BitCoaddUseMex logical                 = true;
        
        Args.ReplaceNaN                             = 'interp';   % 'replace' | 'interp'
        Args.FillValues                             = 'back';
        Args.BitNameNaN                             = 'NaN';
        Args.BitNameInterpolated                    = 'Interpolated';
        Args.interpOverNanArgs cell                 = {};
        
        %
        Args.HeaderCopy1(1,1) logical               = true;
        Args.NewHeader                              = [];
        Args.UpdateTimes(1,1) logical               = true;
        Args.SumExpTime(1,1) logical                = true;
        Args.UpdateImagePathKeys logical            = true;
        Args.KeyExpTime                             = 'EXPTIME';
        
        Args.Cube                                   = [];
    end

    DataProp                      = {'ImageData','BackData', 'VarData', 'MaskData'};
    IndexDim                      = 1;

    
    Result = AstroImage;

    Nim = numel(Obj);

    Flag = Obj.isemptyProperty('Back');
    Obj(Flag) = imProc.background.background(Obj(Flag), Args.BackArgs{:});
    [ImageCube, BackCube, VarCube, MaskCube] = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC, 'DimIndex',IndexDim, 'DataProp',DataProp, 'DataPropIn',Args.DataPropIn, 'Cube',Args.Cube);
    
    % subtract background
    if Args.SubBack
        ImageCube = ImageCube - BackCube;
    end
    
    % flux match images
    if ~isempty(Args.FluxMatch)
        if isnumeric(Args.FluxMatch)
            % do nothing
        else
            % get ZP from header
            StZP = Obj.getStructKey(Args.FluxMatch);
            ZP   = [StZP.(Args.FluxMatch)].';
            if any(isnan(ZP))
                error('ZP is NaN or not in header');
            end
            MedZP = median(ZP);
            Args.FluxMatch = 10.^(0.4.*(ZP-MedZP));
        end
        ImageCube = ImageCube./Args.FluxMatch;
    end
        
    % weighted coadd
    switch Args.StackMethod
        case 'wmean'
            if isempty(VarCube)
                error('Can not calc wmean without the variance cube');
            end
            InvVarCube    = Args.FluxMatch./VarCube;
            SumInvVarCube = sum(InvVarCube, IndexDim, 'omitnan');
            CoaddVar      = 1./SumInvVarCube;
            Coadd         = sum(ImageCube.*InvVarCube, IndexDim, 'omitnan') .* CoaddVar;
            CoaddN        = sum(~isnan(ImageCube),3);

        case 'sigmaclip'
            [Coadd, CoaddVar, ~, CoaddN] = imUtil.image.mean_sigclip(ImageCube, Args.IndexDim, Args.StackArgs{:});
            % e.g., {'MeanFun',@nanamean, 'StdFun','rstd','Nsigma',[5
            % 5],'MaxIter',3}

        case 'wsigmaclip'
            [Coadd, ~, ~, CoaddN] = imUtil.image.wmean_sigclip(ImageCube,Args.VarCube, Args.IndexDim, Args.StackArgs{:});
             % e.g., {'MeanFun',@nanamean, 'StdFun','rstd','Nsigma',[5
             % 5],'MaxIter',3}

            InvVarCube    = Args.FluxMatch./VarCube;
            SumInvVarCube = sum(InvVarCube, IndexDim, 'omitnan');
            CoaddVar      = 1./SumInvVarCube;
             
        otherwise
            error('Unknown StackMethod option');
    end
    Result.Image = squeeze(Coadd);
    Result.Back  = 0;
    Result.Var   = squeeze(CoaddVar);
    if Args.AddCoaddN
        Result.Exp   = squeeze(CoaddN);
    end
    
    % coadd Mask
    if Args.AddMask
        Result.Mask  = tools.array.bitor_array(MaskCube, IndexDim, Args.BitCoaddUseMex);
        
        % Update Mask
        % Mark NaN pixels in the mask image and interpolate over these pixels

        switch lower(Args.ReplaceNaN)
            case 'none'
                % do nothing
            otherwise
                FlagNaN         = isnan(Result.Image);
                Result.MaskData = maskSet(Result.MaskData, FlagNaN, Args.BitNameNaN, 1, 'CreateNewObj',false);

                switch lower(Args.ReplaceNaN)

                    case 'interp'

                        Result = imProc.mask.interpOverMaskedPix(Result, 'BitNamesToInterp',{Args.BitNameNaN},...
                                                                     'interpOverNanArgs',Args.interpOverNanArgs,...
                                                                     'BitNameInterpolated',Args.BitNameInterpolated,...
                                                                     'CreateNewObj',false);
                    case 'replace'
                        % NaNs can be generated when CoaddN=0
                        if ischar(Args.FillValues)
                            % estimate from median
                            FillVal = median(Result.Image, 'all', 'omitnan');
                        else
                            FillVal = Args.FillValues;
                        end
                        Result.Image(FlagNaN) = FillVal;

                        Result.MaskData = maskSet(Result.MaskData, FlagNaN, Args.BitNameInterpolated, 1);
                    otherwise
                        error('Unknown ReplaceNaN option');
                end  % end inner switch
        end % end outer switch
        
    end
    
    % Update header:
    Result.HeaderData = imProc.stack.coaddHeader(Obj, 'HeaderCopy1', Args.HeaderCopy1,...
                                                      'NewHeader',Args.NewHeader,...
                                                      'UpdateTimes',Args.UpdateTimes,...
                                                      'SumExpTime',Args.SumExpTime,...
                                                      'UpdateImagePathKeys',Args.UpdateImagePathKeys,...
                                                      'KeyExpTime',Args.KeyExpTime);
                                                  
end
