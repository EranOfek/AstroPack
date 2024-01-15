function [Result, CoaddN, ImageCube] = coadd(ImObj, Args)
    % Coadd images in AstroImage object including pre/post normalization
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'CCDSEC' - CCDSEC on which to operate:
    %                   [Xmin, Xmax, Ymin, Ymax].
    %                   Use [] for the entire image.
    %                   If not [], then DataPropIn/Out will be
    %                   modified to 'Image'.
    %            'DataPropIn' - The data property that contains the
    %                   the data in the ImageComponent.
    %                   Default is 'Data'.
    %            'Offset' - Either a function handle, a vector, or
    %                   empty. If function handle, then will apply
    %                   it to the ImageData to calculate an offset
    %                   per image. This offset will be subtracted
    %                   from each image. If vector, then this is a
    %                   offset value per image. If empty, do not
    %                   apply offset. Default is [].
    %            'OffsetArgs' - A cell array of additional
    %                   arguments to pass to the offset function.
    %                   Default is {}.
    %            'PreNorm' - Like offset, but for the
    %                   pre-normalization for the images. The
    %                   image is divided by the pre-normalization after the offset.
    %                   Default is [].
    %            'PreNormArgs' - A cell array of additional
    %                   arguments to pass to the pre-normalization function.
    %                   Default is {}.
    %            'UseWeights' - A logical indicating if to apply
    %                   weights. Default is true.
    %            'Weights' - A vector of variances (one per image).
    %                   If empty, then will attempt to use the
    %                   VarImage image in the AstroImage.
    %                   Default is [].
    %            'StackMethod' - - Stacking method. Options are:
    %                   'sum'
    %                   ['mean']
    %                   'median'
    %                   'var'
    %                   'rvar'
    %                   'min'
    %                   'max'
    %                   'range'
    %                   'quantile' - rquires a quqntile argument.
    %                   'wmean' 
    %                   'sigmaclip' - for arguments see: imUtil.image.mean_sigclip
    %                   'wsigmaclip' - for arguments see: imUtil.image.wmean_sigclip
    %                   'bitor' - bit-wise or operation. Return only Stack.
    %                   'bitand' - bit-wise and operation. Return only Stack.
    %                   'bitnot' - bit-wise not operation. Return only Stack.
    %              'StackArgs' - A cell array of arguments to pass to the
    %                   method function. Default is {}.
    %              'MaskStackMethod' - Like 'StackMethod', but for the
    %                   coaddition of the Mask. Default is 'bitor'.
    %              'MaskStackArgs' - A cell array of arguments to pass to the
    %                   mask method function. Default is {}.
    %              'CombineBack' - A logical indicating if to
    %                   combine the background image (using the
    %                   StackMethod). Default is true.
    %              'CombineMask' - A logical indicating if to
    %                   combine the mask image. Default is true.
    %
    %              'ReplaceNaN' - Replace NaN optiion:
    %                   'none'
    %                   'interp' - use imProc.mask.interpOverMaskedPix
    %                   'replace' - default.    
    %              'FillValues' - A value to replace NaNs with.
    %                   If 'back', then used median of image.
    %                   Default is 'back'.
    %              'BitNameNaN' - Bit name for the NaN bit mask.
    %                   Default is 'NaN'.
    %              'BitNameInterpolated' - Bit name for the interpolated
    %                   bit mask. Default is 'Interpolated'.
    %              'interpOverNanArgs' - Cell array of additional arguments
    %                   to pass to imProc.mask.interpOverMaskedPix.
    %                   Default is {}.
    %
    %              'EmpiricalVarFun' - Default is @var.
    %              'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.
    %              'MedianVarCorrForEmpirical' - A logical indicating if to
    %                   correct the variance calculation by the ratio between
    %                   the variance of the median and variance of the mean.
    %                   Default is false.
    %              'DivideEmpiricalByN' - A logical indicating if to divide
    %                   CoaddVarEmpirical by N. Default is false.
    %              'PostNorm' - Like offset, but for the
    %                   post-normalization for the images (a scalar). The
    %                   post-normalization is done after the stacking.
    %                   Default is [].
    %              'PostNormArgs' - A cell array of additional
    %                   arguments to pass to the post-normalization function.
    %                   Default is {}.
    %              'HeaderCopy1' - A logical indicating if to copy
    %                   the header from the 1st coadd image.
    %                   Default is true.
    %              'NewHeader' - An header to add to the coadd
    %                   image header. This can be a 3 column cell
    %                   array, an AstroHeader or AstroImage. If
    %                   empty do nothing. Default is [].
    %              'UpdateTimes' - A logical indicatin if to add
    %                   keywords regarding the number of coadded
    %                   images and update the EXPTIME and MIDJD.
    %                   Default is true.
    %              'SumExpTime' - A logical indicating if to sum
    %                   the EXPTIME in the new header, or to use
    %                   the mean (false). Default is true.
    %              'UpdateImagePathKeys' - A logical indicating if to
    %                   add the LEVEL, SUBLEVEL and CROPID keywords to
    %                   header. Default is true.
    %              'Cube' - Pre allocated cube. This can be a
    %                   pre-allocated cube with the exact same size
    %                   needed by the function. If provided, this
    %                   will be used instaed of allocating new
    %                   memory using the zeros command.
    %                   If empty, then the Cube will be allocated.
    %                   Default is [].
    % Output : - An AstroImage with the coadded image, includinf
    %            the coadded background and mask. The VarData is always
    %            including the empirical variance.
    %          - A matrix in which each pixel give the number of
    %            images on which the coaddition was based.
    %          - The cube of images
    % Author : Eran Ofek (Apr 2021)
    % Example: AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
    %          [Result, CoaddN] = imProc.stack.coadd(AI);

    arguments
        ImObj                                       = [];

        Args.CCDSEC                                 = [];
        Args.DataPropIn char                        = 'Data';

        Args.Offset                                 = [];  % function_handle, vector, or []
        Args.OffsetArgs cell                        = {};

        Args.PreNorm                                = [];
        Args.PreNormArgs cell                       = {};

        Args.UseWeights(1,1) logical                = true;
        Args.Weights                                = [];  % if empty use inverse variance

        Args.StackMethod                            = 'mean';
        Args.StackArgs cell                         = {};

        Args.MaskStackMethod                        = 'bitor';
        Args.MaskStackArgs cell                     = {};

        Args.CombineBack(1,1) logical               = true;
        Args.CombineMask(1,1) logical               = true;

        Args.ReplaceNaN                             = 'interp';   % 'replace' | 'interp'
        Args.FillValues                             = 'back';
        Args.BitNameNaN                             = 'NaN';
        Args.BitNameInterpolated                    = 'Interpolated';
        Args.interpOverNanArgs cell                 = {};
    
        
        Args.EmpiricalVarFun function_handle        = @var;
        Args.EmpiricalVarFunArgs                    = {[],1,'omitnan'};
        Args.MedianVarCorrForEmpirical(1,1) logical = false;
        Args.DivideEmpiricalByN(1,1) logical        = false;

        Args.PostNorm                               = [];   % function_handle or []
        Args.PostNormArgs cell                      = {};

        Args.HeaderCopy1(1,1) logical               = true;
        Args.NewHeader                              = [];
        Args.UpdateTimes(1,1) logical               = true;
        Args.SumExpTime(1,1) logical                = true;
        Args.UpdateImagePathKeys logical            = true;
        Args.Cube                                   = [];

    end
    DataProp                      = {'ImageData','BackData', 'VarData', 'MaskData'};
    IndexDim                      = 1;

    % allocate output
    Result = AstroImage;

    Nim = numel(ImObj);

    [ImageCube, BackCube, VarCube, MaskCube] = imProc.image.images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DimIndex',IndexDim, 'DataProp',DataProp, 'DataPropIn',Args.DataPropIn, 'Cube',Args.Cube);
    
    if isa(ImageCube, 'ImageComponent')
        IsIC = true;
    else
        IsIC = false;
    end
    
    if IsIC
        % This is a copy of the else block, but using a cube in an
        % ImageComponent
        DataProp = 'Data';
        
        % subtract offset (only from image)
        if ~isempty(Args.Offset)
            if isa(Args.Offset,'function_handle')
                Args.Offset = Args.Offset(ImageCube.(DataProp), Args.OffsetArgs{:});
            end
            Noff = numel(Args.Offset);
            if Noff~=1 && Noff~=Nim
                error('Number of offsets must be 1 or equal to number of images');
            end
            if IndexDim==3
                ImageCube.(DataProp) = ImageCube.(DataProp) - reshape(Args.Offset,[1 1 Noff]);
            elseif IndexDim==1
                ImageCube.(DataProp) = ImageCube.(DataProp) - reshape(Args.Offset,[Noff 1 1]);
            else
                error('IndexDim must be 1 or 3');
            end
        end

        % pre normalization (only from image and variance)
        if ~isempty(Args.PreNorm)
            if isa(Args.PreNorm,'function_handle')
                Args.PreNorm = Args.PreNorm(ImageCube.(DataProp), Args.PreNormArgs{:});
           end
            Nnorm = numel(Args.PreNorm);
            if Nnorm~=1 && Nnorm~=Nim
                error('Number of pre normalizations mnust be 1 or equal to number of images');
            end
            if IndexDim==3
                PreNorm = reshape(1./Args.PreNorm,[1 1 Nnorm]);
            elseif IndexDim==1
                PreNorm = reshape(1./Args.PreNorm,[Nnorm 1 1]);
            else
                error('IndexDim must be 1 or 3');
            end
            ImageCube.(DataProp) = ImageCube.(DataProp) .* PreNorm;
            VarCube.(DataProp)   = VarCube.(DataProp)   .* PreNorm.^2;
        end

        % stack the images
        if Args.UseWeights
            %
            if isempty(Args.Weights)
                % use inverse variance as weights
                Args.Weights = VarCube.(DataProp);
            else
                Nw = nuem(Args.Weights);

                if IndexDim==3
                    Args.Weights = reshape(Args.Weights, [1 1 Nw]);
                elseif IndexDim==1
                    Args.Weights = reshape(Args.Weights, [Nw 1 1]);
                else
                    error('IndexDim must be 1 or 3');
                end  
            end
        else
            Args.Weights = [];
        end
        [Coadd, CoaddVarEmpirical, ~, CoaddN] = imUtil.image.stackCube(ImageCube.(DataProp), 'StackMethod',Args.StackMethod,...
                                                                                 'StackArgs',Args.StackArgs,...
                                                                                 'IndexDim',IndexDim,...
                                                                                 'EmpiricalVarFun',Args.EmpiricalVarFun,...
                                                                                 'EmpiricalVarFunArgs',Args.EmpiricalVarFunArgs,...
                                                                                 'VarCube',Args.Weights,...
                                                                                 'MedianVarCorrForEmpirical',Args.MedianVarCorrForEmpirical,...
                                                                                 'DivideEmpiricalByN',Args.DivideEmpiricalByN,...
                                                                                 'DivideVarByN',false,...
                                                                                 'CalcCoaddVarEmpirical',true,...
                                                                                 'CalcCoaddVar',false,...
                                                                                 'CalcCoaddN',true);




        if Args.CombineBack && ~isempty(BackCube.(DataProp))
            [BackCoadd] = imUtil.image.stackCube(BackCube.(DataProp), 'StackMethod',Args.StackMethod,...
                                                                                 'StackArgs',Args.StackArgs,...
                                                                                 'IndexDim',IndexDim,...
                                                                                 'VarCube',[],...
                                                                                 'CalcCoaddVarEmpirical',false,...
                                                                                 'CalcCoaddVar',false,...
                                                                                 'CalcCoaddN',false);
             Result.BackData.(Args.DataPropIn)  = BackCoadd;                                                                
        end
        if Args.CombineMask && ~isempty(MaskCube.(DataProp))
            [MaskCoadd] = imUtil.image.stackCube(MaskCube.(DataProp), 'StackMethod',Args.MaskStackMethod,...
                                                                                 'StackArgs',Args.MaskStackArgs,...
                                                                                 'IndexDim',IndexDim,...
                                                                                 'VarCube',[],...
                                                                                 'CalcCoaddVarEmpirical',false,...
                                                                                 'CalcCoaddVar',false,...
                                                                                 'CalcCoaddN',false);
            Result.MaskData.(Args.DataPropIn)  = MaskCoadd;   

        end
        
        
    else
    
        % subtract offset (only from image)
        if ~isempty(Args.Offset)
            if isa(Args.Offset,'function_handle')
                Args.Offset = Args.Offset(ImageCube, Args.OffsetArgs{:});
            end
            Noff = numel(Args.Offset);
            if Noff~=1 && Noff~=Nim
                error('Number of offsets must be 1 or equal to number of images');
            end
            if IndexDim==3
                ImageCube = ImageCube - reshape(Args.Offset,[1 1 Noff]);
            elseif IndexDim==1
                ImageCube = ImageCube - reshape(Args.Offset,[Noff 1 1]);
            else
                error('IndexDim must be 1 or 3');
            end
        end

        % pre normalization (only from image and variance)
        if ~isempty(Args.PreNorm)
            if isa(Args.PreNorm,'function_handle')
                Args.PreNorm = Args.PreNorm(ImageCube, Args.PreNormArgs{:});
           end
            Nnorm = numel(Args.PreNorm);
            if Nnorm~=1 && Nnorm~=Nim
                error('Number of pre normalizations mnust be 1 or equal to number of images');
            end
            if IndexDim==3
                PreNorm = reshape(1./Args.PreNorm,[1 1 Nnorm]);
            elseif IndexDim==1
                PreNorm = reshape(1./Args.PreNorm,[Nnorm 1 1]);
            else
                error('IndexDim must be 1 or 3');
            end
            ImageCube = ImageCube .* PreNorm;
            VarCube   = VarCube   .* PreNorm.^2;
        end

        % stack the images
        if Args.UseWeights
            %
            if isempty(Args.Weights)
                % use inverse variance as weights
                Args.Weights = VarCube;
            else
                Nw = nuem(Args.Weights);

                if IndexDim==3
                    Args.Weights = reshape(Args.Weights, [1 1 Nw]);
                elseif IndexDim==1
                    Args.Weights = reshape(Args.Weights, [Nw 1 1]);
                else
                    error('IndexDim must be 1 or 3');
                end  
            end
        else
            Args.Weights = [];
        end
        [Coadd, CoaddVarEmpirical, ~, CoaddN] = imUtil.image.stackCube(ImageCube, 'StackMethod',Args.StackMethod,...
                                                                                 'StackArgs',Args.StackArgs,...
                                                                                 'IndexDim',IndexDim,...
                                                                                 'EmpiricalVarFun',Args.EmpiricalVarFun,...
                                                                                 'EmpiricalVarFunArgs',Args.EmpiricalVarFunArgs,...
                                                                                 'VarCube',Args.Weights,...
                                                                                 'MedianVarCorrForEmpirical',Args.MedianVarCorrForEmpirical,...
                                                                                 'DivideEmpiricalByN',Args.DivideEmpiricalByN,...
                                                                                 'DivideVarByN',false,...
                                                                                 'CalcCoaddVarEmpirical',true,...
                                                                                 'CalcCoaddVar',false,...
                                                                                 'CalcCoaddN',true);




        if Args.CombineBack && ~isempty(BackCube)
            [BackCoadd] = imUtil.image.stackCube(BackCube, 'StackMethod',Args.StackMethod,...
                                                                                 'StackArgs',Args.StackArgs,...
                                                                                 'IndexDim',IndexDim,...
                                                                                 'VarCube',[],...
                                                                                 'CalcCoaddVarEmpirical',false,...
                                                                                 'CalcCoaddVar',false,...
                                                                                 'CalcCoaddN',false);
             Result.BackData.(Args.DataPropIn)  = BackCoadd;                                                                
        end
        if Args.CombineMask && ~isempty(MaskCube)
            [MaskCoadd] = imUtil.image.stackCube(MaskCube, 'StackMethod',Args.MaskStackMethod,...
                                                                                 'StackArgs',Args.MaskStackArgs,...
                                                                                 'IndexDim',IndexDim,...
                                                                                 'VarCube',[],...
                                                                                 'CalcCoaddVarEmpirical',false,...
                                                                                 'CalcCoaddVar',false,...
                                                                                 'CalcCoaddN',false);
            Result.MaskData.(Args.DataPropIn)  = MaskCoadd;   

        end
    end

    
    % FINISH DEALING WITH CUBE
    
    % post normalization (image and variance)
    if ~isempty(Args.PostNorm)
        if isa(Args.PostNorm,'function_handle')
            Args.PostNorm = Args.PostNorm(Coadd, Args.PostNormArgs{:});
       end
        Nnorm = numel(Args.PostNorm);
        if Nnorm~=1
            error('Number of post normalizations mnust be 1');
        end
        PostNorm          = 1./Args.PostNorm;
        Coadd             = Coadd             .* PostNorm;
        CoaddVarEmpirical = CoaddVarEmpirical .* PostNorm.^2;
    end

    % store in AstroImage object
    Result.ImageData.(Args.DataPropIn) = Coadd;
    Result.VarData.(Args.DataPropIn)   = CoaddVarEmpirical;

    % FFU: update header
    if Args.HeaderCopy1
        % copy image header from first image
        Result.HeaderData.Data = ImObj(1).HeaderData.Data;
    end
    if ~isempty(Args.NewHeader)
        if isa(Args.NewHeader,'AstroHeader')
            Result.HeaderData = Args.NewHeader;
        elseif iscell(Args.NewHeader)
            Result.HeaderData.Data = Args.NewHeader;
        elseif isa(Args.NewHeader,'AstroImage')
            Result.HeaderData = Args.NewHeader.HeaderData;
        else
            error('Unknown NewHeader option');
        end
    end
    if Args.UpdateTimes
        % update ExpTime, and MIDJD + add info re coaddition
        Filter     = getVal(ImObj(1).HeaderData','FILTER');
        Type       = getVal(ImObj(1).HeaderData','IMTYPE');
        
        VecExpTime = funHeader(ImObj, @getVal,'EXPTIME');
        MidJD      = funHeader(ImObj, @julday);
        InfoCell = {'IMTYPE',Type,'';...
                    'FILTER',Filter,'';...
                    'NCOADD',Nim,'Number of coadded images';...
                    'COADDOP',Args.StackMethod,'Coaddition method';...
                    'AVNCOADD',mean(CoaddN,'all'),'Mean number of coadded images per pixel';...
                    'MINCOADD',min(CoaddN,[],'all'),'Minimum number of coadded images per pixel';...
                    'MIDJD',0.5.*(max(MidJD)+min(MidJD)),'Middle time of observations';...
                    'MINJD',min(MidJD),'MIDJD of first coadded observation';...
                    'MAXJD',max(MidJD),'MIDJD of last coadded observation'};
               
            
        Result.HeaderData = insertKey(Result.HeaderData, InfoCell, 'end');

        if Args.SumExpTime
            Result.HeaderData = replaceVal(Result.HeaderData, 'EXPTIME', {sum(VecExpTime)});
        else
            Result.HeaderData = replaceVal(Result.HeaderData, 'EXPTIME', {mean(VecExpTime)});
        end
%         Result.HeaderData = replaceVal(Result.HeaderData, 'MIDJD', {median(MidJD)});
        Result.HeaderData = replaceVal(Result.HeaderData, 'MIDJD', {sum(MidJD.*VecExpTime)/sum(VecExpTime)});        

    end
    
    % Update header ImagePath parameters
    
    if Args.UpdateImagePathKeys
        %CCDID, CROPID, FieldID,
        
        CropID   = getVal(ImObj(1).HeaderData,'CROPID');
        CCDID    = getVal(ImObj(1).HeaderData,'CCDID');
        FieldID  = getVal(ImObj(1).HeaderData,'FieldID');
        TimeZone = getVal(ImObj(1).HeaderData,'TIMEZONE');
        
        InfoCell = {'LEVEL','coadd','';...
                    'SUBLEVEL','','';...
                    'CROPID',CropID,'';...
                    'CCDID',CCDID,'';...
                    'FIELDID',FieldID,'';...
                    'TIMEZONE',TimeZone,''};
                    
        %Result.HeaderData = insertKey(Result.HeaderData, InfoCell, 'end');
        Result.HeaderData = replaceVal(Result.HeaderData, InfoCell(:,1), InfoCell(:,2), 'Comment',InfoCell(:,3), 'AddPos','end');
        
    end
    
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
        