function [Result, IsBias, CoaddN] = bias(ImObj, Args)
    % Generate a super bias image from a s et of bias images.
    % Input  : - An AstroImage object with multiple images.
    %          * ...,key,val,...
    %            'BitDictinaryName' - A BitDictionary name.
    %                   If empty, will use existing BitDictionary.
    %                   Note, that if BitDictionary doesn't exist
    %                   and not provided, the function will fail.
    %                   Default is 'BitMask.Image.Default' (located
    %                   in the config/ directory).
    %            'IsBias' - A function handle for a function that
    %                   selects and validates bias/dark images
    %                   (e.g., @imProc.dark.isBias, @isDark).
    %                   Alternatively, a vector of logicals
    %                   indicating which image is a bias/dark
    %                   image. If empty use all images.
    %                   The function must be a method of Dark.
    %                   Default is @imProc.dark.isBiasDark.
    %            'IsBiasArgs' - A cell array of arguments to pass
    %                   to the  IsBias function. Default is {}.
    %            'StackMethod' - For options, see
    %                   imProc.image.Stack.coadd).
    %                   Default is 'sigmaclip'.
    %            'StackArgs' - A cell array of arguments to pass to the
    %                   method function. Default is
    %                   {'MeanFun',@tools.math.stat.nanmean, 'StdFun',@std, 'Nsigma',[5 5], 'MaxIter',1}.
    %            'EmpiricalVarFun' - Default is @var.
    %            'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.
    %            'DivideEmpiricalByN' - A logical indicating if to divide
    %                   CoaddVarEmpirical by N. Default is false.
    %            'getValArgs' - A cell array of arguments to pass
    %                   to the Header/getVal function. Default is {}.
    %            'LowRN_BitName' - LowRN bit name.
    %                   This bit flag pixels which variance is
    %                   larger than Threshold*RN^2, where RN is the
    %                   ReadNoise.
    %                   Default is 'LowRN'.
    %            'LowRN_Threshold' - Threshold value.
    %                   Default is 0.05.
    %            'LowRN_MeanFun' - A string, a function handle or
    %                   numerical value. If string then this is an
    %                   header keyword name, and will attempt to
    %                   look for this keyword (using the getVal
    %                   function). If numerical value, than assume
    %                   this is the RN. If a function handle than
    %                   this function will be applied on the
    %                   variance image to estimate the RN.
    %                   Default is @median.
    %            'HighRN_BitName'
    %                   This bit flag pixels which variance is
    %                   smaller than Threshold*RN^2, where RN is the
    %                   ReadNoise.
    %                   Default is 'HighRN'.
    %            'HighRN_Threshold' - Threshold value.
    %                   Default is 10.
    %            'HighRN_MeanFun' - Like LowRN, buit for the
    %                   HighRN bit.
    %                   Default is @median.
    %            'DarkHighVal_BitName' - Bit name for high
    %                   dark/bias values. Defined as image values
    %                   larger than Threshold*Mean, where Mean is
    %                   the image mean.
    %                   Default is 'DarkHighVal'.
    %            'DarkHighVal_Threshold' - Threshold value.
    %                   Default is 2.
    %                   DarkLowVal_BitName' - Bit name for low
    %                   dark/bias values. Defined as image values
    %                   smaller than Threshold*Mean, where Mean is
    %                   the image mean.
    %                   Default is 'DarkLowVal'.
    %            'DarkLowVal_Threshold' - Threshold value.
    %                   Default is 0.d.
    %            'BiasFlaring_BitName' - Bit name for flaring
    %                   pixels identified using
    %                   identifyFlaringPixels.
    %            'BiasFlaring_Threshold' - A threshold value
    %                   (number of sigma above mean). Default is 20.
    %            'BiasFlaringArgs' - A cell array of additional
    %                   arguments to pass to identifyFlaringPixels.
    %                   Default is {}.
    %            'AddHeader' - A 3 column cell array to add to
    %                   header. Default is {}.
    %            'AddHeaderPos' - Position of the added header.
    %                   Default is 'end'.
    %            'SumExpTime' - A logical indicating if to sum
    %                   (true) or take the mean (false) of the
    %                   EXPTIME header keyword. Default is false.
    % Output : - An AstroImage containing the bias/dark image.
    %          - A vector of logical indicating which images were
    %            used.
    %          - A matrix of the number of images used in each
    %            pixel.
    % Example: A=AstroImage('LAST.*_dark.fits')
    %          Bias = imProc.dark.bias(A)

    arguments
        ImObj AstroImage
        Args.BitDictinaryName           = 'BitMask.Image.Default';  % char array or BitDictionary

        Args.IsBias                     = @imProc.dark.isBiasDark;  % @isBias, @isDark, vector of logical or [] - use all.
        Args.IsBiasArgs cell            = {};

        Args.StackMethod                = 'sigmaclip';   
        Args.StackArgs                  = {'MeanFun',@tools.math.stat.nanmean, 'StdFun',@std, 'Nsigma',[5 5], 'MaxIter',1};
        Args.EmpiricalVarFun            = @var;
        Args.EmpiricalVarFunArgs        = {[],3,'omitnan'};
        Args.DivideEmpiricalByN         = false;

        Args.getValArgs                 = {};
        Args.LowRN_BitName              = 'LowRN';
        Args.LowRN_Threshold            = 0.05;
        Args.LowRN_MeanFun              = @median;   % or RN or RN keyword...

        Args.HighRN_BitName             = 'HighRN';
        Args.HighRN_Threshold           = 10;
        Args.HighRN_MeanFun             = @median;   % or RN or RN keyword...

        Args.DarkHighVal_BitName        = 'DarkHighVal';
        Args.DarkHighVal_Threshold      = 2;

        Args.DarkLowVal_BitName         = 'DarkLowVal';
        Args.DarkLowVal_Threshold       = 0.3;

        Args.BiasFlaring_BitName        = 'BiasFlaring';
        Args.BiasFlaring_Threshold      = 20;
        Args.BiasFlaringArgs cell       = {};

        Args.AddHeader                  = {};
        Args.AddHeaderPos               = 'end';
        Args.SumExpTime(1,1) logical    = false;

    end

    Nim = numel(ImObj);

    if isempty(Args.IsBias)
        % use all images
        IsBias = true(Nim,1);
    else
        if isa(Args.IsBias,'function_handle')
            % call the function
            IsBias = Args.IsBias(ImObj, Args.IsBiasArgs{:});
        elseif islogical(Args.IsBias) || isnumeric(Args.IsBias)
            IsBias = Args.IsBias;
        else
            error('Unknown IsBias option');
        end
    end

    [Result, CoaddN, ImageCube] = imProc.stack.coadd(ImObj(IsBias), 'CCDSEC',[],...
                                      'Offset',[],...
                                      'PreNorm',[],...
                                      'UseWeights',false,...
                                      'StackMethod',Args.StackMethod,...
                                      'StackArgs',Args.StackArgs,...
                                      'CombineBack',false,...
                                      'CombineMask',true,...
                                      'EmpiricalVarFun',Args.EmpiricalVarFun,...
                                      'EmpiricalVarFunArgs',Args.EmpiricalVarFunArgs,...
                                      'MedianVarCorrForEmpirical',false,...
                                      'DivideEmpiricalByN',Args.DivideEmpiricalByN,...
                                      'PostNorm',[],...
                                      'SumExpTime',Args.SumExpTime);

     % Make sure BitDictionary is populated
     if ~isempty(Args.BitDictinaryName)
         if ischar(Args.BitDictinaryName)
             Result.MaskData.Dict = BitDictionary(Args.BitDictinaryName);
         elseif isa(Args.BitDictinaryName,'BitDictionary')
             Result.MaskData.Dict = Args.BitDictinaryName;
         else
             error('BitDictinaryName must be a char array or a BitDictionary');
         end
     end % else do nothing

     % Prepare Mask image
     % mask LowRN             
     if isa(Args.LowRN_MeanFun,'function_handle')
         FlagLowRN = Result.Var < (Args.LowRN_Threshold.*Args.LowRN_MeanFun(Result.Var,'all'));
     elseif isnumeric(Args.LowRN_MeanFun)
         % value for RN
         FlagLowRN = Result.Var < (Args.LowRN_Threshold.*Args.LowRN_MeanFun.^2);
     elseif ischar(Args.LowRN_MeanFun)
         % header keyword for RN
         Args.LowRN_MeanFun = funHeader(Result, @getVal, Args.LowRN_MeanFun, Args.getValArgs{:});
         FlagLowRN = Result.Var < (Args.LowRN_Threshold.*Args.LowRN_MeanFun.^2);
     else
         error('Unknown LowRN_MeanFun option');
     end
     Result = maskSet(Result, FlagLowRN, Args.LowRN_BitName, 1);

     % mask HighRN
     if isa(Args.HighRN_MeanFun,'function_handle')
         FlagHighRN = Result.Var > (Args.HighRN_Threshold.*Args.HighRN_MeanFun(Result.Var,'all'));
     elseif isnumeric(Args.HighRN_MeanFun)
         % value for RN
         FlagHighRN = Result.Var > (Args.HighRN_Threshold.*Args.HighRN_MeanFun.^2);
     elseif ischar(Args.HighRN_MeanFun)
         % header keyword for RN
         Args.HighRN_MeanFun = funHeader(Result, @getVal, Args.HighRN_MeanFun, Args.getValArgs{:});
         FlagHighRN = Result.Var > (Args.HighRN_Threshold.*Args.HighRN_MeanFun.^2);
     else
         error('Unknown LowRN_MeanFun option');
     end
     Result = maskSet(Result, FlagHighRN, Args.HighRN_BitName, 1);

     % mask DarkHighVal
     FlagHigh = Result.Image > (Args.DarkHighVal_Threshold.*mean(Result.Image,'all'));
     Result = maskSet(Result, FlagHigh, Args.DarkHighVal_BitName, 1);

     % mask DarkLowVal
     FlagLow = Result.Image < (Args.DarkLowVal_Threshold.*mean(Result.Image,'all'));
     Result = maskSet(Result, FlagLow, Args.DarkLowVal_BitName, 1);

     % mask BiasFlaring
     [FlagFlaring] = imProc.dark.identifyFlaringPixels(ImageCube, Args.BiasFlaringArgs{:}, 'Threshold',Args.BiasFlaring_Threshold);
     Result = maskSet(Result, FlagFlaring, Args.BiasFlaring_BitName, 1);                             

     % Update Header
     if ~isempty(Args.AddHeader)
        Result.HeaderData = insertKey(Result.HeaderData, Args.AddHeader, Args.AddHeaderPos);
     end

end
