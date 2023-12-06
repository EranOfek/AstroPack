%


classdef Flat < Component
    properties
        %FlatIm AstroImage
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
            % Example: A=AstroImage('LAST.*_dark.fits');
            %          [Result,Flag] = imProc.image.Flat.isFlat(A)
            
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
        function [Result, IsFlat, CoaddN] = flat(Obj, ImObj, Args)
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
            %            'NaN_BitName' - A NaN bit name.
            %                   Default is 'NaN';
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
            % Example: AI = AstroImage('LAST.*_dark.fits')
            %          D = imProc.image.Dark;
            %          Bias = D.bias(AI);
            %          AI = AstroImage('LAST.*_twflat.fits');
            %          F = imProc.image.Flat;
            %          F.isFlat(AI)
            %          D.debias(AI,Bias);  % note that with CreateNewObj it fails
            %          [Flat,IsFlat,CoaddN]=F.flat(AI);
            
            arguments
                Obj
                ImObj AstroImage
                Args.BitDictinaryName           = 'BitMask.Image.Default';  % char array or BitDictionary
                
                Args.IsFlat                     = @isFlat;  % @isFlat, vector of logical or [] - use all.
                Args.IsFlatArgs cell            = {};
                
                Args.PreNorm                    = @median;
                Args.PreNormArgs cell           = {[1 2],'omitnan'};
                Args.PostNorm                   = @median;
                Args.PostNormArgs cell          = {[1 2],'omitnan'};
                
                Args.StackMethod                = 'sigmaclip';   
                Args.StackArgs                  = {'MeanFun',@nanmean, 'StdFun','std', 'Nsigma',[5 5], 'MaxIter',2};
                Args.EmpiricalVarFun            = @var;
                Args.EmpiricalVarFunArgs        = {[],3,'omitnan'};
                Args.DivideEmpiricalByN         = false;
                
                
                Args.getValArgs                 = {};
                
                Args.FlatHighStd_BitName        = 'FlatHighStd';
                Args.FlatHighStd_Threshold      = 1;
                Args.FlatHighStd_MeanFun        = 0.01;   %@median;   % or RN or RN keyword...
                
                Args.FlatLowVal_BitName         = 'FlatLowVal';
                Args.FlatLowVal_Threshold       = 0.5;
                
                Args.NaN_BitName                = 'NaN';
                
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
             
             % FlatHighStd
             if isa(Args.FlatHighStd_MeanFun,'function_handle')
                 FlagFlatHighStd = Result.Var > (Args.FlatHighStd_Threshold.*Args.FlatHighStd_MeanFun(Result.Var,'all'));
             elseif isnumeric(Args.FlatHighStd_MeanFun)
                 % value for RN
                 FlagFlatHighStd = Result.Var > (Args.FlatHighStd_Threshold.*Args.FlatHighStd_MeanFun.^2);
             else
                 error('Unknown FlatHighStd_MeanFun option');
             end
             Result = maskSet(Result, FlagFlatHighStd, Args.FlatHighStd_BitName, 1);
             
             % FlatLowVal
             FlagFlatLowVal = Result.Image < Args.FlatLowVal_Threshold;
             Result = maskSet(Result, FlagFlatLowVal, Args.FlatLowVal_BitName, 1);
             
             % NaN
             FlagNaN = isnan(Result.Image);
             Result  = maskSet(Result, FlagNaN, Args.NaN_BitName, 1);
                                   
             % Update Header
             if ~isempty(Args.AddHeader)
                Result.HeaderData = insertKey(Result.HeaderData, Args.AddHeader, Args.AddHeaderPos);
             end
             
             % store Bias in Dark object
             % Obj.FlatIm = Result;
                
        end
        
        function [Result, Flat, IsFlat, IsNotFlat] = deflat(Obj, ImObj, Flat, Args)
            % Divide by flat (and construct if needed) from a list of images
            % Input  : - A Flat object.
            %          - An AstroImage object. Either containing images
            %            from which to divide the flat, or all the images
            %            including the flat images.
            %            All the images are assumed to be bias/dark
            %            subtracted.
            %          - A flat image. If empty, will attempt to construct
            %            the flat image. Default is [].
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
            %                   Default is @isFlat.
            %            'FlatArgs' - A cell array of additional arguments
            %                   to pass to the flat method.
            %                   Default is {}.
            %            'CCDSEC' - A CCDSEC on which to operate the bias
            %                   subtraction (will be used both on the bias
            %                   and target images). If empty, use the
            %                   entire image. Default is [].
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - An AstroImage object containing the non-flat
            %            images after division by flat and mask propagation.
            %          - A flat image.
            %          - A vector of logicals indicating flat images.
            %          - A vector of logicals indicating non-flat images.
            % Author : Eran Ofek (May 2021)
            % Example: AI = AstroImage('LAST.*_dark.fits');
            %          D = imProc.image.Dark;
            %          Bias = D.bias(AI);
            %          AI = AstroImage('LAST.*_twflat.fits');
            %          F = imProc.image.Flat;
            %          F.isFlat(AI)
            %          D.debias(AI,Bias);  % note that with CreateNewObj it fails
            %          [Flat,IsFlat,CoaddN]=F.flat(AI);
            %          A = AstroImage('LAST.2.1.2_20200821.020129.546_clear_0_science.fits');
            %          D.debias(A, Bias);
            %          F.deflat(A, Flat);
           
            arguments
                Obj(1,1)
                ImObj AstroImage
                Flat                            = [];  % A flat (AstroImage) image 
                Args.BitDictinaryName           = 'BitMask.Image.Default';  % char array or BitDictionary
                Args.IsBias                     = @isFlat;  % @isBias, @isDark, vector of logical or [] - use all.                
                Args.BiasArgs cell              = {};
                Args.CCDSEC                     = [];
                Args.CreateNewObj               = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            
            if isempty(Flat)
                % generate bias image
                %KeyVal                 = namedargs2cell(Args);
                % IsBias = Obj.isBias(ImObj)
                [Flat, IsFlat]         = flat(Obj, ImObj, Args.FlatArgs{:}, 'BitDictinaryName',Args.BitDictinaryName,...
                                                                            'IsFlat',Args.IsFlat); % KeyVal{:});
                IsNotFlat              = ~IsFlat;
            else
                % bias image is provided
                IsFlat                 = [];    % unknown
                IsNotFlat              = true(size(ImObj));
            end
            
            % subtract the bias image
            if ~any(IsNotFlat)
                error('No non-flat image from which to divide flat');
            end
            Result = funBinaryProp(ImObj(IsNotFlat), Flat, @rdivide, 'OpArgs',{},...
                                                        'DataProp','ImageData',...
                                                        'DataPropIn','Data',...
                                                        'CCDSEC',Args.CCDSEC,...
                                                        'CCDSEC1',Args.CCDSEC,...
                                                        'CCDSEC2',Args.CCDSEC,...
                                                        'UseOrForMask',true,...
                                                        'CreateNewObj',Args.CreateNewObj,...
                                                        'Result',[]);
            % propagate the mask image
            Result = funBinaryProp(ImObj(IsNotFlat), Flat, @rdivide, 'OpArgs',{},...
                                                        'DataProp','MaskData',...
                                                        'DataPropIn','Data',...
                                                        'CCDSEC',Args.CCDSEC,...
                                                        'CCDSEC1',Args.CCDSEC,...
                                                        'CCDSEC2',Args.CCDSEC,...
                                                        'UseOrForMask',true,...
                                                        'CreateNewObj',false,...
                                                        'Result',Result);
        
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