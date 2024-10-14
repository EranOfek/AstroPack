function [Result, IsFlat, CoaddN] = flat(ImObj, Args)
    % Generate a super flat image from a set of flat images.
    %   A flat image will be generated for each Filter
    % Input  : - An AstroImage object with multiple images.
    %          * ...,key,val,...
    %            'BitDictinaryName' - A BitDictionary name.
    %                   If empty, will use existing BitDictionary.
    %                   Note, that if BitDictionary doesn't exist
    %                   and not provided, the function will fail.
    %                   Default is 'BitMask.Image.Default' (located
    %                   in the config/ directory).
    %            'IsFlat' - A function handle for a function that
    %                   selects and validates flat images
    %                   (e.g., @imProc.flat.isFlat).
    %                   Alternatively, a vector of logicals
    %                   indicating which image is a flat
    %                   image. If empty use all images.
    %                   The function must be a method of Flat.
    %                   Default is @imProc.flat.isFlat.
    %            'IsFlatArgs' - A cell array of arguments to pass
    %                   to the IsFlat function. Default is {}.
    %            'StackMethod' - For options, see
    %                   imProc.image.Stack.coadd).
    %                   Default is 'sigmaclip'.
    %            'StackArgs' - A cell array of arguments to pass to the
    %                   method function. Default is
    %                   {'MeanFun',@tools.math.stat.nanmedian, 'StdFun',@std, 'Nsigma',[3 3], 'MaxIter',2}.
    %            'EmpiricalVarFun' - Default is @var.
    %            'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.
    %            'DivideEmpiricalByN' - A logical indicating if to divide
    %                   CoaddVarEmpirical by N. Default is false.
    %            'FilterKey' - A string of Filter main dictionary name.
    %                   This is used in order to group the images by
    %                   filter. If empty, then use all images.
    %                   Default is 'FILTER'.
    %            'getValArgs' - A cell array of arguments to pass
    %                   to the Header/getVal function. Default is {}.
    %            'FlatHighStd_BitName' - FlatHighStd bit name.
    %                   Default is 'FlatHighStd'.
    %            'FlatHighStd_Threshold' - Threshold value for
    %                   FlatHighStd, in units of the MeanFun value.
    %            'FlatHighStd_MeanFun' - Either a function handle
    %                   or a value. The function handle is used for
    %                   the calculation of the mean of the image std map.
    %                   The threshold is in units of this mean
    %                   value. Default is 1e-2.
    %            'FlatLowVal_BitName' - A FlatLowVal bit name.
    %                   Default is 'FlatLowVal'.
    %            'FlatLowVal_Threshold' - Flat low value
    %                   threshold below to flag as FlatLowVal.
    %                   Default is 0.5.
    %            'Replace0' - Replace 0 or negative values with NaN.
    %                   Default is true.
    %            'NaN_BitName' - A NaN bit name.
    %                   Default is 'NaN';
    %            'AddHeader' - A 3 column cell array to add to
    %                   header. Default is {}.
    %            'AddHeaderPos' - Position of the added header.
    %                   Default is 'end'.
    %            'SumExpTime' - A logical indicating if to sum
    %                   (true) or take the mean (false) of the
    %                   EXPTIME header keyword. Default is false.
    %            'GenerateID' - A logical indicating if to generate image
    %                   ID (using imProc.db.generateImageID) and to insert
    %                   it to header. Default is true.
    %            'ArgsGenerateID' - A cell array of additional arguments to
    %                   pass to imProc.db.generateImageID
    %                   Default is {'KeyID','ID_FLAT'}.
    % Output : - An arrray of AstroImage containing the Flat image per
    %            filter.
    %          - A vector of logical indicating which images were
    %            used.
    %          - A matrix of the number of images used in each
    %            pixel.
    %            If multiple filters, then this corresponds to the last
    %            filter.
    % Example: AI = AstroImage('LAST.*_dark.fits')
    %          Bias = imProc.dark.bias(AI);
    %          AI = AstroImage('LAST.*_twflat.fits');
    %          imProc.flat.isFlat(AI)
    %          imProc.dark.debias(AI,Bias);  % note that with CreateNewObj it fails
    %          [Flat,IsFlat,CoaddN]=imProc.flat.flat(AI);

    arguments
        ImObj AstroImage
        Args.BitDictinaryName           = 'BitMask.Image.Default';  % char array or BitDictionary

        Args.IsFlat                     = @imProc.flat.isFlat;  % @isFlat, vector of logical or [] - use all.
        Args.IsFlatArgs cell            = {};

        Args.PreNorm                    = @median;
        Args.PreNormArgs cell           = {[2 3],'omitnan'};
        Args.PostNorm                   = @median;
        Args.PostNormArgs cell          = {[2 3],'omitnan'};

        Args.StackMethod                = 'sigmaclip';   
        Args.StackArgs                  = {'MeanFun',@tools.math.stat.nanmedian, 'StdFun',@std, 'Nsigma',[5 5], 'MaxIter',1};
        Args.EmpiricalVarFun            = @var;
        Args.EmpiricalVarFunArgs        = {[],1,'omitnan'};
        Args.DivideEmpiricalByN         = false;

        Args.FilterKey                  = 'FILTER';

        Args.getValArgs                 = {};

        Args.FlatHighStd_BitName        = 'FlatHighStd';
        Args.FlatHighStd_Threshold      = 1;
        Args.FlatHighStd_MeanFun        = 0.01;   %@median;   % or RN or RN keyword...

        Args.FlatLowVal_BitName         = 'FlatLowVal';
        Args.FlatLowVal_Threshold       = 0.5;
        Args.Replace0 logical           = true;   % replace 0 or negative with NaN
        Args.NaN_BitName                = 'NaN';

        Args.AddHeader                  = {};
        Args.AddHeaderPos               = 'end';
        Args.SumExpTime(1,1) logical    = false;

        Args.GenerateID logical         = true;
        Args.ArgsGenerateID cell        = {'KeyID','ID_FLAT'};
    end

    Nim = numel(ImObj);

    if isempty(Args.IsFlat)
        % use all images
        IsFlat = true(Nim,1);
    else
        if isa(Args.IsFlat,'function_handle')
            % call the function
            IsFlat = Args.IsFlat(ImObj, Args.IsFlatArgs{:});
        elseif islogical(Args.IsFlat) || isnumeric(Args.IsFlat)
            IsFlat = Args.IsFlat;
        else
            error('Unknown IsFlat option');
        end
    end

    % check filters
    if isempty(Args.FilterKey)
        % use all images
        Nufilt = 1;
    else
        if ~ImObj(1).HeaderData.isKeyExist(Args.FilterKey)
            error('Header keyword %s - does not exist - If not filters, use FilterKey=[]');
        end
        St           = getStructKey(ImObj, Args.FilterKey, 'UseDict',true);
        FilterCell   = {St.(Args.FilterKey)};
        UniqueFilter = unique(FilterCell);
        Nufilt       = numel(UniqueFilter);
    end
    
    Result = AstroImage([Nufilt 1]); % allocate AstroImage
    for Iufilt=1:1:Nufilt
        % for each unique filter
        % flag of images taken using current filter
        if isempty(Args.FilterKey)
            % use all images
            FlagFilter = true(numel(ImObj),1);
        else
            FlagFilter = strcmp(UniqueFilter{Iufilt}, FilterCell);
        end
        
        [Result(Iufilt), CoaddN, ImageCube] = imProc.stack.coadd(ImObj(FlagFilter(:) & IsFlat(:)), 'CCDSEC',[],...
                                      'Offset',[],...
                                      'PreNorm',Args.PreNorm,...
                                      'PreNormArgs',Args.PreNormArgs,...
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
                                      'PostNormArgs',Args.PostNormArgs,...
                                      'SumExpTime',Args.SumExpTime);

         % Make sure BitDictionary is populated
         if ~isempty(Args.BitDictinaryName)
             if ischar(Args.BitDictinaryName)
                 Result(Iufilt).MaskData.Dict = BitDictionary(Args.BitDictinaryName);
             elseif isa(Args.BitDictinaryName,'BitDictionary')
                 Result(Iufilt).MaskData.Dict = Args.BitDictinaryName;
             else
                 error('BitDictinaryName must be a char array or a BitDictionary');
             end
         end % else do nothing

         % FlatHighStd
         if isa(Args.FlatHighStd_MeanFun,'function_handle')
             FlagFlatHighStd = Result(Iufilt).Var > (Args.FlatHighStd_Threshold.*Args.FlatHighStd_MeanFun(Result(Iufilt).Var,'all'));
         elseif isnumeric(Args.FlatHighStd_MeanFun)
             % value for RN
             FlagFlatHighStd = Result(Iufilt).Var > (Args.FlatHighStd_Threshold.*Args.FlatHighStd_MeanFun.^2);
         else
             error('Unknown FlatHighStd_MeanFun option');
         end
         Result(Iufilt) = maskSet(Result(Iufilt), FlagFlatHighStd, Args.FlatHighStd_BitName, 1);

         % FlatLowVal
         FlagFlatLowVal = Result(Iufilt).Image < Args.FlatLowVal_Threshold;
         Result(Iufilt) = maskSet(Result(Iufilt), FlagFlatLowVal, Args.FlatLowVal_BitName, 1);

         if Args.Replace0
             Flag = Result(Iufilt).Image <= eps;
             Result(Iufilt).Image(Flag) = NaN;
         end
         
         % NaN
         FlagNaN = isnan(Result(Iufilt).Image);
         Result(Iufilt)  = maskSet(Result(Iufilt), FlagNaN, Args.NaN_BitName, 1);

         % Update Header
         if ~isempty(Args.AddHeader)
            Result(Iufilt).HeaderData = insertKey(Result(Iufilt).HeaderData, Args.AddHeader, Args.AddHeaderPos);
         end
    end

    % Add Image ID to header
    if Args.GenerateID
        [Result] = imProc.db.generateImageID(Result, Args.ArgsGenerateID{:});
    end
            
end
