function Result = replaceWithNoise(Obj, Args)
    % Replace selected pixels with (global) noise and background
    %       Can be use to replace some pixels with background and noise
    %       values. The pixels are either user defined (logicals or
    %       indices) or selected by some bits in the MaskData property in
    %       the AstroImage, or by NaNs found in the image.
    %       The background and variance may be obtained from the AstroImage
    %       or are given by the user. The noise may be normal or Poisson.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'FlagReplace' - Either a vector of indices or a matrix of
    %                   logical flags indicating which pixels should be
    %                   replaced. If empty, then will check if the
    %                   'BitNamesReplace' argument is not empty. If so,
    %                   then will select these pixels by their Mask bit
    %                   value. If the 'BitNamesReplace' is empty, then will
    %                   search pixels that are NaN, and these pixels will
    %                   be replaced. Default is [].
    %            'BitNamesReplace' - A cell array of bit mask names. If
    %                   'FlagReplace' is empty, then will search for pixels
    %                   which have the specific bit masks. Default is {}.
    %            'BitSearchMethod' - Indicate how to search for bit mask names.
    %                   Either 'any' | 'all' of the bits are open.
    %                   Default is 'any'.
    %            'AddBack' - A logical indicating if to add the background
    %                   to the replaced pixeks. Default is true.
    %            'AddNoise' - A logical indicating if to add the random
    %                   noise to the replaced pixeks. Default is true.
    %            'Back' - A background scalar or matrix to use for the
    %                   replacment. If empty, then will attempt to use the
    %                   'BackData' property in the AstroImage.
    %                   Default is [].
    %            'Var' - A variance scalar or matrix to use for the
    %                   replacment. If empty, then will attempt to use the
    %                   'VarData' property in the AstroImage.
    %                   Default is [].
    %            'NoiseType' - 'normal' | 'poisson'. Default is 'normal'.
    %            'MeanFun' - A function handle to use for the calculation
    %                   of a global background and variance from the Back
    %                   and Var matrices. Default is @median.
    %            'MeanFunArgs' - A cell array of additional arguments to
    %                   pass to 'MeanFun'. Default is {'all','omitnan'}.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   output object. Default is false.
    %            'CreateNewOnlyImage' - A logical indicating if to create a
    %                   new object only for the 'ImageData' property.
    %                   Default is true.
    %                   This will be used only if CreateNewObj=true.
    % Output : - An AstroImage object in which the selected pixels are
    %            replaced with some background and noise.
    % Author : Eran Ofek (Mar 2022)
    % Example: AI = AstroImage({randn(1000,1000)+1000});
    %          AI.Image(20,20) = NaN;
    %          AI = imProc.background.background(AI);
    %          AI = imProc.image.replaceWithNoise(AI);
    
    
    arguments
        Obj AstroImage
        Args.FlagReplace                = [];  % If given, use it.
        Args.BitNamesReplace cell       = {};  % if given and FlagReplace is not given, theb use it
                                               % if FlagReplace and
                                               % BitNamesReplace are not given
                                               % then select NaN pixels.
        Args.BitSearchMethod            = 'any';  % 'any' | 'all'
        Args.AddBack logical            = true;
        Args.AddNoise logical           = true;
        Args.Back                       = [];
        Args.Var                        = [];
        Args.NoiseType                  = 'normal';   % 'normal' | 'poisson'
        Args.MeanFun function_handle    = @median;
        Args.MeanFunArgs cell           = {'all','omitnan'};
        
        Args.CreateNewObj logical       = false;
        Args.CreateNewOnlyImage logical = true;
    end
   
    Nobj = numel(Obj);
    
    if Args.CreateNewObj
        if Args.CreateNewOnlyImage
            Result = Obj;
            for Iobj1=1:1:Nobj
                Result(Iobj).ImageData = Obj(Iobj).ImageData.copy;
            end
        else
            % copy the entire object
            Result = Obj.copy;
        end
    else
        Result = Obj;
    end
    
    for Iobj=1:1:Nobj
        if ~isempty(Args.FlagReplace)
            Flag        = Args.FlagReplace;
            NpixReplace = sum(Flag,'all');
        else
            % check if bit names are available
            if ~isempty(Args.BitNamesReplace)
                % search by bit names
                [~, ~, Flag] = findBit(Result(Iobj).MaskData, Args.BitNamesReplace, 'BitSearchMethod',Args.BitSearchMethod);
                NpixReplace = numel(Flag);
            else
                % search NaNs in image
                Flag        = isnan(Obj(Iobj).Image);
                NpixReplace = sum(Flag,'all');
            end
        end
    
        if NpixReplace>0
            % replace pixels
            
            % get background
            if Args.AddBack
                if isempty(Args.Back)
                    % use Back property in AstroImage
                    Back = Obj(Iobj).Back;
                else
                    if isa(Args.Back, 'ImageComponent')
                        Back = Args.Back.Data;
                    else
                        % assume Back is in a matrix form
                        Back = Args.Back;
                    end
                end
            else
                Back = 0;
            end
            
            % get variance
            if Args.AddNoise
                if isempty(Args.Var)
                    % use Var property in AstroImage
                    Var = Obj(Iobj).Var;
                else
                    if isa(Args.Var, 'ImageComponent')
                        Var = Args.Var.Data;
                    else
                        % assume Back is in a matrix form
                        Var = Args.Var;
                    end
                end
            else
                Var = 0;
            end
            
            % calculate the global background and variance
            BackVal = Args.MeanFun(Back, Args.MeanFunArgs{:});
            VarVal  = Args.MeanFun(Var, Args.MeanFunArgs{:});
            
            % generate random values
            switch lower(Args.NoiseType)
                case 'normal'
                    % new value to put in each flaged pixel
                    NewVal = BackVal + randn(NpixReplace,1).*sqrt(VarVal);
                case 'poisson'
                    % new value to put in each flaged pixel
                    NewVal = BackVal + poissrnd(VarVal, NpixReplace,1);
                otherwise
                    error('Unknown NoiseType option: should be normal or poisson');
            end
            % insert NewVal into image data
            Result(Iobj).Image(Flag) = NewVal;
        end
    end
end
