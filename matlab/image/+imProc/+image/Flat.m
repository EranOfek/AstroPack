%


classdef Flat < Component
    properties
        FlatIm AstroImage
    end
    
    methods  % Constructor
        function Obj = Flat(Args)
            % Constructor for a Flat object
            % Input  : * ...,key,val,...
            %            Can be any Flat object property name followed by
            %            its new value.
            % Output : - A Flat object
            % Author : Eran Ofek (Apr 2021)
            % Example: D = imProc.image.Flat
           
            arguments
                Args.StackMethod
                
            end
            
            FN = fieldnames(Args);
            for Ifn=1:1:numel(FN)
                Obj.(FN{Ifn}) = Args.(FN{Ifn});
            end
        end
    end
   
    methods % aux functions
        % fit a surface
        
        % remove stars
        
        % filter
        
    end
    
    methods % isFlat
        function [Result,Flag] = isFlat(Obj, AI, Args)
            % Check and validate that a set of images in an AstroImage object are flat images
            % Input  : - A imProc.image.Flat object.
            %          - An AstroImage object.
            %          * ...,key,val,...
            %            'Template' - A template image with the same size
            %                   of the input image. This can be either a
            %                   matrix or an AstroImage object.
            %                   If this is an AstroImage it may include a
            %                   variance image.
            %            'TemplateVar' - A variance image. If provided, and
            %                   the template is an AstroImage, this will
            %                   override the content of the variance image
            %                   in the AstroImage.
            %            'Nsigma' - Threshold in units of number of sigmas.
            %                   Defualt is 5.
            %            'MaxFracBadPixels' - If the fraction of pixels
            %                   which value deviates from the template by
            %                   more/less than Nsigma is larger than this
            %                   threshold then the output FlagBad will be
            %                   set to true. Default is 0.0001.
            %            'UseImageVar' - A logical indicating if to add the
            %                   AstroImage variance to the template
            %                   variance. If false, use only the template
            %                   variance. Default is true.
            %            'ImTypeKeyName' - IMTYPE header keyword name.
            %                   Default is 'IMTYPE'.
            %            Additional parameters to pass yo isImType.
            % Output : - A vector of logical indicating if an
            %            image is a validate flat image.
            %          - A structure containing vector of logicals for
            %            individaul tests.
            % Author : Eran Ofek (May 2021)
            
            % NOT TESTED
            
            % Example: A=AstroImage('LAST.*_dark.fits');
            %          D=imProc.image.Flat;
            %          [Result,Flag] = D.isFlat(A)
            
            arguments
                Obj(1,1)
                AI                                                              = [];
                Args.MaxAllowedFrac                                             = 0.2;
                Args.Template                                                   = [];
                Args.TemplateVar                                                = [];
                Args.Nsigma                                                     = 5;
                Args.MaxFracBadPixels(1,1)                                      = 0.0001;
                Args.UseImageVar                                                = true;
                
                Args.ImTypeKeyName                                              = 'IMTYPE';                
                Args.UseDict(1,1) logical                                       = true;
                Args.CaseSens(1,1) logical                                      = true;
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.IsInputAlt(1,1) logical                                    = true;
                Args.KeyDict                                                    = [];
            end
            ImTypeVal = 'Flat';
            
            % AI is now an AstroImage object
            Flag.IsImType = isImType(AI, ImTypeVal, 'UseDict',Args.UseDict,...
                                             'CaseSens',Args.CaseSens,...
                                             'SearchAlgo',Args.SearchAlgo,...
                                             'IsInputAlt',Args.IsInputAlt,...
                                             'KeyDict',Args.KeyDict);
            
            % validation
            if isempty(Args.Template)
                Flag.TemplateOK = true(size(Flag.IsImType));
            else
                [Flag.FlagBad, FracBadPixels, Z] = compare2template(Obj, AI, 'Template',Args.Template,...
                                                                    'TemplateVar',Args.TemplateVar,...
                                                                    'Nsigma',Args.Nsigma,...
                                                                    'MaxFracBadPixels',Args.MaxFracBadPixels,...
                                                                    'UseImageVar',Args.UseImageVar,...
                                                                    'DataProp','Image',...
                                                                    'VarProp','Var');
                Flag.TemplateOK = ~Flag.FlagBad;                       
            end                   
            Result = Flag.IsImType & Flag.TemplateOK;
            
        end
        
    end
    
    methods % flat, deflat
        function [Result, IsBias, CoaddN] = flat(Obj, ImObj, Args)
            % Generate a super flat image from a set of flat images.
            % Input  : - A Flat object.
            %          - An AstroImage object with multiple images.
            %          * ...,key,val,...
            %            'BitDictinaryName' - A BitDictionary name.
            %                   If empty, will use existing BitDictionary.
            %                   Note, that if BitDictionary doesn't exist
            %                   and not provided, the function will fail.
            %                   Default is 'BitMask.Image.Default' (located
            %                   in the config/ directory).
            %            'IsFlat' - A function handle for a function that
            %                   selects and validates flat images
            %                   (e.g., @isFlat).
            %                   Alternatively, a vector of logicals
            %                   indicating which image is a flat
            %                   image. If empty use all images.
            %                   The function must be a method of Flat.
            %                   Default is @isFlat.
            %            'IsBiasArgs' - A cell array of arguments to pass
            %                   to the IsFlat function. Default is {}.
            
            %            'StackMethod' - For options, see
            %                   imProc.image.Stack.coadd).
            %                   Default is 'sigmaclip'.
            %            'StackArgs' - A cell array of arguments to pass to the
            %                   method function. Default is
            %                   {'MeanFun',@nanmean, 'StdFun','std', 'Nsigma',[5 5], 'MaxIter',1}.
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
            %                   Default is 0.2.
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
            %          D = imProc.image.Dark;
            %          Bias = D.bias(A)
            
            arguments
                Obj
                ImObj AstroImage
                Args.BitDictinaryName           = 'BitMask.Image.Default';  % char array or BitDictionary
                
                Args.IsFlat                     = @isFlat;  % @isFlat, vector of logical or [] - use all.
                Args.IsFlatArgs cell            = {};
                
                Args.PerNorm                    = @median;
                Args.PreNormArgs cell           = {[1 2],'omitnan'};
                Args.PostNorm                   = @median;
                Args.PostNormArgs cell          = {[1 2],'omitnan'};
                
                Args.StackMethod                = 'sigmaclip';   
                Args.StackArgs                  = {'MeanFun',@nanmean, 'StdFun','std', 'Nsigma',[5 5], 'MaxIter',1};
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
                Args.DarkLowVal_Threshold       = 0.2;
                
                Args.BiasFlaring_BitName        = 'BiasFlaring';
                Args.BiasFlaring_Threshold      = 20;
                Args.BiasFlaringArgs cell       = {};
                
                Args.AddHeader                  = {};
                Args.AddHeaderPos               = 'end';
                Args.SumExpTime(1,1) logical    = false;
                
            end
            
            Nim = numel(ImObj);
            
            if isempty(Args.IsFlat)
                % use all images
                IsFlat = true(Nim,1);
            else
                if isa(Args.IsFlat,'function_handle')
                    % call the function
                    IsFlat = Args.IsFlat(Obj, ImObj, Args.IsFlatArgs{:});
                elseif islogical(Args.IsFlat) || isnumeric(Args.IsFlat)
                    IsFlat = Args.IsFlat;
                else
                    error('Unknown IsFlat option');
                end
            end
                    
            C = imProc.image.Stack;
            [Result, CoaddN, ImageCube] = C.coadd(ImObj, 'CCDSEC',[],...
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
                     Result.MaskData.Dict = BitDictionary(Args.BitDictinaryName);
                 elseif isa(Args.BitDictinaryName,'BitDictionary')
                     Result.MaskData.Dict = Args.BitDictinaryName;
                 else
                     error('BitDictinaryName must be a char array or a BitDictionary');
                 end
             end % else do nothing
             
             % got here
             
             
             
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
             [FlagFlaring] = identifyFlaringPixels(Obj, ImageCube, Args.BiasFlaringArgs{:}, 'Threshold',Args.BiasFlaring_Threshold);
             Result = maskSet(Result, FlagFlaring, Args.BiasFlaring_BitName, 1);                             
                                          
             % Update Header
             if ~isempty(Args.AddHeader)
                Result.HeaderData = insertKey(Result.HeaderData, Args.AddHeader, Args.AddHeaderPos);
             end
             
             % store Bias in Dark object
             Obj.Bias = Result;
                
        end
        
    end
    
    methods % nonlinearity
    
        
    end
    
    methods (Static) % unitTest
        function Result = unitTest()
            % unitTest for imProc.image.Flat
            % Example : Result = imProc.image.Flat.unitTest;
            
            
            AI = AstroImage({rand(100,100)});
            F  = imProc.image.Flat;
            
            % isFlat
            [Result,Flag] = F.isFlat(AI);
            if Result
                error('Problem with isFlat');
            end
            
            insertKey(AI.HeaderData, {'IMTYPE','Flat',''});
            [Result,Flag] = F.isFlat(AI);
            if ~Result
                error('Problem with isFlat');
            end
            
            
            Result = true;
            
        end
    end
    
    
end