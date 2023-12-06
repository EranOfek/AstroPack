% imProc.image.Dark class
%       This class provides functionality for generation and operations
%       related to Dark/Bias images.
% Functionality:
%   identifySimilarImages - Search for sucessive images with a fraction of identical pixel values
%   compare2template - Compare AstroImage to a template and variance and flag image
%           which are different than the template.
%   identifyFlaringPixels - Identify flaring pixels in a cube of images
%   isBias - Check and validate that a set of images in an AstroImage object are bias images
%   isDark - Check and validate that a set of images in an AstroImage object are dark images
%   bias   - Generate a super bias image from a s et of bias images.
%   debias - Subtract bias (and construct if needed) from a list of images
%   overscan - Create overscan images and optionally subtract from images


classdef Dark < Component
    properties
        BiasIm AstroImage
    end
    
    methods  % Constructor
        function Obj = Dark(Args)
            % Constructor for a Dark object
            % Input  : * ...,key,val,...
            %            Can be any Dark object property name followed by
            %            its new value.
            % Output : - A Dark object
            % Author : Eran Ofek (Apr 2021)
            % Example: D = imProc.image.Dark
           
            arguments
                Args.StackMethod
                
            end
            
            FN = fieldnames(Args);
            for Ifn=1:1:numel(FN)
                Obj.(FN{Ifn}) = Args.(FN{Ifn});
            end
        end
    end
    
    methods % identify and validate bias/dark images
        function [PassedThreshold, FracIdentical] = identifySimilarImages(DarkObj, Obj, Args)
            % Search for sucessive images with a fraction of identical pixel values
            %   This is useful in order to identify problems with the
            %   detector firmware (i.e., some regions of the detector in
            %   two surcessive images are identical due to a readout
            %   problem).
            % Input  : - A Dark object.
            %          - An AstroImage or ImageComonent object.
            %          * ...,key,val,...
            %            'DataProp' - Data property containing the image.
            %                   Default is 'Image'.
            %            'MaxAllowedFrac' - The fraction of identical
            %                   pixels above to set the output argument
            %                   PassedThreshold to true.
            %                   Default is 0.2.
            % Output : - PassedThreshold. True if the fraction of identical
            %            pixels in two sucessive images is larger than the
            %            MaxAllowedFrac threshold.
            %          - An array of the fraction of identical pixels in
            %            and image and the sucessive image.
            %            The last value is always NaN.
            % Author : Eran Ofek
            % Example: AI=AstroImage({rand(1000,1000), rand(1000,1000)});
            %          D = Dark;
            %          [a,b]=D.identifySimilarImages(AI) 
            
            arguments
                DarkObj
                Obj AstroImage
                Args.DataProp                = 'Image';
                Args.MaxAllowedFrac          = 0.2;
            end
            
            % use object default arguments if not supplied by user
            Args = selectDefaultArgsFromProp(DarkObj, Args);
            if isempty(Obj)
                Obj = DarkObj.Images;
            end
            
            
            Nobj = numel(Obj);
            FracIdentical = nan(size(Obj));
            for Iobj=1:1:Nobj-1
                Nidentical          = sum(Obj(Iobj).(Args.DataProp) == Obj(Iobj+1).(Args.DataProp),'all');
                Npix                = numel(Obj(Iobj).(Args.DataProp));
                FracIdentical(Iobj) = Nidentical./Npix;
            end
            if any(FracIdentical>Args.MaxAllowedFrac)
                PassedThreshold = true;
            else
                PassedThreshold = false;
            end
            
        end
        
        function [FlagBad, FracBadPixels, Z] = compare2template(DarkObj, Obj, Args)
            % Compare AstroImage to a template and variance and flag image
            %   which are different than the template.
            % Input  : - A Dark object.
            %          - An AstroImage object containing images.
            %            The comparison is done 1 to 1, 1 to many, or many
            %            to 1.
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
            %            'DataProp' - Data property in the AstroImage.
            %                   Default is 'Image'.
            %            'VarProp' - Variance property in the template
            %                   image. Default is 'Var'.
            % Output : - A column vector of logicals indicating if the
            %            fraction of bad pixels (above or below threshold)
            %            is above MaxFracBadPixels.
            %          - A column vector of the fraction of bad pixels in
            %            each image.
            %          - An ImageComponent containing the Z image.
            %            (i.e., Image-Template)/sqrt(Var).
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({2.*randn(10,10)});
            %          Template = AstroImage({0},'Var',{4});
            %          D = imProc.image.Dark;
            %          [FlagBad, FracbadPixels, Z] = D.compare2template(AI, 'Template',Template)
            
            arguments
                DarkObj(1,1)
                Obj AstroImage
                Args.Template
                Args.TemplateVar                      = [];
                Args.Nsigma                           = 5;
                Args.MaxFracBadPixels(1,1)            = 0.0001;
                Args.UseImageVar                      = true;
                
                Args.DataProp                         = 'Image';
                Args.VarProp                          = 'Var';
            end
            
            if isa(Args.Template,'AstroImage')
                % assume that the Template include the variance image
                Template = Args.Template;
                % check if TemplateVar is given
                if ~isempty(Args.TemplateVar)
                    % override the template variance in the Template
                    % AstroImage
                    Template.(Args.VarProp) = Args.TemplateVar;
                end
            elseif isnumeric(Args.Template)
                Template = AstroImage({Args.Template},'Var',{Args.TemplateVar});
            else
                error('Template must be an AstroImage or matrix');
            end
            
            Ntemp = numel(Template);
            Nobj  = numel(Obj);
            
            Nmax    = max(Ntemp, Nobj);
            FlagBad       = false(Nmax,1);
            FracBadPixels = nan(Nmax,1);
            if nargout>2
                Z = ImageComponent([Nmax,1]);
            end
            for Imax=1:1:Nmax
                Iobj  = min(Imax, Nobj);
                Itemp = min(Imax, Ntemp);
                
                if Args.UseImageVar && ~isempty(Obj(Iobj).(Args.VarProp))
                    % comobine the image and template variances
                    TotVar = Template(Itemp).(Args.VarProp) + Obj(Iobj).(Args.VarProp);
                else
                    TotVar = Template(Itemp).(Args.VarProp);
                end
                Zstat   = (Obj(Iobj).(Args.DataProp) - Template(Itemp).(Args.DataProp))./sqrt(TotVar);
                NbadPix = sum(abs(Zstat)>Args.Nsigma,'all');
                
                FracBadPixels(Imax) = NbadPix./numel(Zstat);
                FlagBad(Imax)       = FracBadPixels(Imax)>Args.MaxFracBadPixels;
                
                if nargout>2
                    Z(Imax).Image = Zstat;
                end
                
            end
               
            
        end
        
        function [Result, Mean, Std, Max] = identifyFlaringPixels(DarkObj, Cube, Args)
            % Identify flaring pixels in a cube of images
            %       Searched by looking at (Cube-Mean)/Std>Threshold
            % Input  : - A Dark object
            %          - A cube of images. Usulally the image index is in
            %            the 3rd dimension.
            %          * ...,key,val,...
            %            'MeanFun' - Either a number, a matrix or a
            %                   function handle by which to calculate the
            %                   mean function. Default is @median.
            %            'MeanFunArgs' - A cell array of arguments to pass
            %                   to the mean function.
            %                   Default is {3,'omitnan'}.
            %            'MaxFun' - A function handle by which to calculate
            %                   the max of the data. Default is @max.
            %            'MaxFunArgs' - A cell array of arguments to pass
            %                   to the max function.
            %                   Default is {[],3}.
            %            'StdFun' - Either a number, a matrix or a
            %                   function handle by which to calculate the
            %                   mean function. Default is @imUtil.background.rstd
            %            'StdFunArgs' - A cell array of arguments to pass
            %                   to the std function.
            %                   Default is {3}.
            %            'Threshold' - Threshold above to flag the pixel.
            %                   Default is 10.
            % Output : - A matrix of logicals indicating pixels that are
            %            above the flaring threshold.
            %          - A matrix of the mean values.
            %          - A matrix of the std values.
            %          - A matrix of the max values.
            % Author : Eran Ofek (May 2021)
            % Example: Cube = randn(100,100,10); Cube(1,1,1)=30;
            %          Dark = imProc.image.Dark;
            %          [Result,Mean,Std,Max] = identifyFlaringPixels(Dark, Cube);
            %          [Result,Mean,Std,Max] = identifyFlaringPixels(Dark, Cube,'MeanFunArgs',{'all'});
            
            arguments
                DarkObj(1,1)
                Cube
                Args.MeanFun                            = @median;  % or number or imagew
                Args.MeanFunArgs cell                   = {3,'omitnan'};
                Args.MaxFun function_handle             = @max;
                Args.MaxFunArgs cell                    = {[],3};
                Args.StdFun                             = @imUtil.background.rstd;   % or number or image
                Args.StdFunArgs cell                    = {3};
                Args.Threshold                          = 20;   % number of sigmas
            end
            
            if isa(Args.MeanFun,'function_handle')
                Mean = Args.MeanFun(Cube, Args.MeanFunArgs{:});
            else
                Mean = Args.MeanFun;
            end
            Max  = Args.MaxFun(Cube,  Args.MaxFunArgs{:});
            if isa(Args.StdFun,'function_handle')
                Std = Args.StdFun(Cube, Args.StdFunArgs{:});
            else
                Std = Args.StdFun;
            end
            
            Result = (Max - Mean)./Std > Args.Threshold;
            
        end
        
        function [Result,Flag] = isBias(Obj, AI, Args)
            % Check and validate that a set of images in an AstroImage object are bias images
            % Input  : - A imProc.image.Dark object.
            %          - An AstroImage object.
            %          * ...,key,val,...
            %            'MaxAllowedFrac' - The fraction of identical
            %                   pixels above to set the output argument
            %                   PassedThreshold to true.
            %                   This parameter is passed to identifySimilarImages
            %                   Default is 0.2.
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
            %            image is a validate bias/dark image.
            %          - A structure containing vector of logicals for
            %            individaul tests.
            % Author : Eran Ofek (May 2021)
            % Example: A=AstroImage('LAST.*_dark.fits');
            %          D=imProc.image.Dark;
            %          [Result,Flag] = D.isBias(A)
            
            arguments
                Obj(1,1)
                AI AstroImage
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
            ImTypeVal = 'Bias';
            
            % AI is now an AstroImage object
            Flag.IsImType = isImType(AI, ImTypeVal, 'UseDict',Args.UseDict,...
                                             'CaseSens',Args.CaseSens,...
                                             'SearchAlgo',Args.SearchAlgo,...
                                             'IsInputAlt',Args.IsInputAlt,...
                                             'KeyDict',Args.KeyDict);
            
            % validation
            if isempty(Args.MaxAllowedFrac)
                Flag.IdenticalPixOK = true(size(Flag.IsImType));
            else
                [PassedThresholdIdentical, FracIdentical] = identifySimilarImages(Obj, AI, 'DataProp','Image', 'MaxAllowedFrac',Args.MaxAllowedFrac);
                Flag.IdenticalPixOK = ~PassedThresholdIdentical;
            end
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
            Result = Flag.IsImType & Flag.IdenticalPixOK & Flag.TemplateOK;
            
        end
        
        function [Result,Flag] = isDark(Obj, AI, Args)
            % Check and validate that a set of images in an AstroImage object are dark images
            % Input  : - A imProc.image.Dark object.
            %          - An AstroImage object.
            %          * ...,key,val,...
            %            'MaxAllowedFrac' - The fraction of identical
            %                   pixels above to set the output argument
            %                   PassedThreshold to true.
            %                   This parameter is passed to identifySimilarImages
            %                   Default is 0.2.
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
            %            image is a validate bias/dark image.
            %          - A structure containing vector of logicals for
            %            individaul tests.
            % Author : Eran Ofek (May 2021)
            % Example: A=AstroImage('LAST.*_dark.fits');
            %          D=imProc.image.Dark;
            %          [Result,Flag] = D.isDark(A)
            
            arguments
                Obj(1,1)
                AI AstroImage
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
            ImTypeVal = 'Dark';
            
            % AI is now an AstroImage object
            Flag.IsImType = isImType(AI, ImTypeVal, 'UseDict',Args.UseDict,...
                                             'CaseSens',Args.CaseSens,...
                                             'SearchAlgo',Args.SearchAlgo,...
                                             'IsInputAlt',Args.IsInputAlt,...
                                             'KeyDict',Args.KeyDict);
            
            % validation
            if isempty(Args.MaxAllowedFrac)
                Flag.IdenticalPixOK = true(size(Flag.IsImType));
            else
                [PassedThresholdIdentical, FracIdentical] = identifySimilarImages(Obj, AI, 'DataProp','Image', 'MaxAllowedFrac',Args.MaxAllowedFrac);
                Flag.IdenticalPixOK = ~PassedThresholdIdentical;
            end
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
            Result = Flag.IsImType & Flag.IdenticalPixOK & Flag.TemplateOK;
            
        end
        
    end
    
    methods % bias/dark
        
        function [Result, IsBias, CoaddN] = bias(Obj, ImObj, Args)
            % Generate a super bias image from a s et of bias images.
            % Input  : - A Dark object.
            %          - An AstroImage object with multiple images.
            %          * ...,key,val,...
            %            'BitDictinaryName' - A BitDictionary name.
            %                   If empty, will use existing BitDictionary.
            %                   Note, that if BitDictionary doesn't exist
            %                   and not provided, the function will fail.
            %                   Default is 'BitMask.Image.Default' (located
            %                   in the config/ directory).
            %            'IsBias' - A function handle for a function that
            %                   selects and validates bias/dark images
            %                   (e.g., @isBias, @isDark).
            %                   Alternatively, a vector of logicals
            %                   indicating which image is a bias/dark
            %                   image. If empty use all images.
            %                   The function must be a method of Dark.
            %                   Default is @isBias.
            %            'IsBiasArgs' - A cell array of arguments to pass
            %                   to the  IsBias function. Default is {}.
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
                
                Args.IsBias                     = @isBias;  % @isBias, @isDark, vector of logical or [] - use all.
                Args.IsBiasArgs cell            = {};
                
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
            
            if isempty(Args.IsBias)
                % use all images
                IsBias = true(Nim,1);
            else
                if isa(Args.IsBias,'function_handle')
                    % call the function
                    IsBias = Args.IsBias(Obj, ImObj, Args.IsBiasArgs{:});
                elseif islogical(Args.IsBias) || isnumeric(Args.IsBias)
                    IsBias = Args.IsBias;
                else
                    error('Unknown IsBias option');
                end
            end
                    
            C = imProc.image.Stack;
            [Result, CoaddN, ImageCube] = C.coadd(ImObj, 'CCDSEC',[],...
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
             [FlagFlaring] = identifyFlaringPixels(Obj, ImageCube, Args.BiasFlaringArgs{:}, 'Threshold',Args.BiasFlaring_Threshold);
             Result = maskSet(Result, FlagFlaring, Args.BiasFlaring_BitName, 1);                             
                                          
             % Update Header
             if ~isempty(Args.AddHeader)
                Result.HeaderData = insertKey(Result.HeaderData, Args.AddHeader, Args.AddHeaderPos);
             end
             
             % store Bias in Dark object
             Obj.BiasIm = Result;
                
        end
        
        function [Result, Bias, IsBias, IsNotBias] = debias(Obj, ImObj, Bias, Args)
            % Subtract bias (and construct if needed) from a list of images
            % Input  : - A Dark object.
            %          - An AstroImage object. Either containing images
            %            from which to subtract the bias, or all the images
            %            including the bias images.
            %          - A bias image. If empty, will attempt to construct
            %            the bias/dark image. Default is [].
            %          * ...,key,val,...
            %            'BitDictinaryName' - A BitDictionary name.
            %                   If empty, will use existing BitDictionary.
            %                   Note, that if BitDictionary doesn't exist
            %                   and not provided, the function will fail.
            %                   Default is 'BitMask.Image.Default' (located
            %                   in the config/ directory).
            %            'IsBias' - A function handle for a function that
            %                   selects and validates bias/dark images
            %                   (e.g., @isBias, @isDark).
            %                   Alternatively, a vector of logicals
            %                   indicating which image is a bias/dark
            %                   image. If empty use all images.
            %                   Default is @isBias.
            %            'BiasArgs' - A cell array of additional arguments
            %                   to pass to the bias method.
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
            % Output : - An AstroImage object containing the non-bias
            %            images after bias subtraction and mask propagation.
            %          - A bias image.
            %          - A vector of logicals indicating bias images.
            %          - A vector of logicals indicating non-bias images.
            % Author : Eran Ofek (May 2021)
            % Example: D = imProc.image.Dark;
            %          AI = AstroImage('LAST.2.1.2_20200821.015445.457_clear_0_science.fits');
            %          AB = D.debias(AI,Bias);
           
            arguments
                Obj(1,1)
                ImObj AstroImage
                Bias                            = [];  % A bias (AstroImage) image 
                Args.BitDictinaryName           = 'BitMask.Image.Default';  % char array or BitDictionary
                Args.IsBias                     = @isBias;  % @isBias, @isDark, vector of logical or [] - use all.                
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
            
            if isempty(Bias)
                % generate bias image
                %KeyVal                 = namedargs2cell(Args);
                % IsBias = Obj.isBias(ImObj)
                [Bias, IsBias]         = bias(Obj, ImObj, Args.BiasArgs{:}, 'BitDictinaryName',Args.BitDictinaryName,...
                                                                            'IsBias',Args.IsBias); % KeyVal{:});
                IsNotBias              = ~IsBias;
            else
                % bias image is provided
                IsBias                 = [];    % unknown
                IsNotBias              = true(size(ImObj));
            end
            
            % subtract the bias image
            if ~any(IsNotBias)
                error('No non-bias image from which to subtract bias');
            end
            Result = funBinaryProp(ImObj(IsNotBias), Bias, @minus, 'OpArgs',{},...
                                                        'DataProp','ImageData',...
                                                        'DataPropIn','Data',...
                                                        'CCDSEC',Args.CCDSEC,...
                                                        'CCDSEC1',Args.CCDSEC,...
                                                        'CCDSEC2',Args.CCDSEC,...
                                                        'UseOrForMask',true,...
                                                        'CreateNewObj',Args.CreateNewObj,...
                                                        'Result',[]);
            % propagate the mask image
            Result = funBinaryProp(ImObj(IsNotBias), Bias, @minus, 'OpArgs',{},...
                                                        'DataProp','MaskData',...
                                                        'DataPropIn','Data',...
                                                        'CCDSEC',Args.CCDSEC,...
                                                        'CCDSEC1',Args.CCDSEC,...
                                                        'CCDSEC2',Args.CCDSEC,...
                                                        'UseOrForMask',true,...
                                                        'CreateNewObj',false,...
                                                        'Result',Result);
        
        end
        
        function [Result, OverScanAI] = overscan(Obj, ImObj, Args)
            % Create overscan images and optionally subtract from images
            % Input  : - A Dark object.
            %          - An AstroImage object of images that contains an
            %            overscan region.
            %          * ...,key,val,...
            %            'OverScan' - Either an header keyword containing
            %                   the overscan region, or an [Xmin Xmax Ymin Ymax]
            %                   vector for the overscan.
            %                   Default is 'OVERSCAN'.
            %            'Subtract' - A logical indicating if to subtract
            %                   the overscan from the image. Default is true.
            %            'RemoveOverScan' - A logical indicating if to crop
            %                   out the overscan region. Default is true.
            %            'RemoveOthers' - a logical indicating if to crop
            %                   also the Back, Var, and Mask fields.
            %                   Default is true (independent of RemoveOverScan).
            %            'OverScanDir' - Indicating the direction of the overscan:
            %                   'x'|'y'|1|2| [].
            %                   'x', or 2 means that the overscan is read along
            %                       the x-axis (meaning the overscan is in
            %                       columns).
            %                   'y', or 1 means that the overscan is read along
            %                       the y-axis.
            %                   [] - choose the direction automatically,
            %                   where the smaller dimension of the overscan
            %                   region is assumed to be the direction.
            %                   Default is [].
            %            'Method' - Method by which to calculate the overscan:
            %                   'globalmedian' - global median (default).
            %                   'globalmean' - global mean.
            %                   'median' - median long the overscan 
            %                   'mean' - mean along the overscan
            %                   'medmedfilt' - median along the overscan
            %                           followed by median filtering
            %                           smoothing. Smoothing default is {50}.
            %                   'medsgolayfilt' - median along the overscan
            %                           followed by Svitzk-Golay filtering.
            %                           Smoothing default is {3 50}.
            %            'MethodArgs' - A cell array of additional
            %                   arguments to pass to the method.
            %                   (Defaults are defined for each medthod).
            %            'UpdateCCDSEC' - A logical indicating if to update
            %                   the CCDSEC header keyword in the header.
            %                   Will also update NAXIS* keywords.
            %                   Default is true.
            %            'KeyCCDSEC' - Header keyword CCDSEC. Default is 'CCDSEC'.
            %            'DataProp' - Data property on which to operate.
            %                   Defaultis 'ImageData'.
            %            'DataPropIn' - Data property, in the image component, on which to operate.
            %                   Defaultis 'Image'.
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
            % Output : - An AstroImage object with the overscan subtracted
            %            (and croped) images.
            %          - OverScanAI is an AstroImage object that stores the
            %            overscan image in the 'ImageData' field, and the
            %            calculated (global or line) overscan in the
            %            'BackData' field.
            % Author : Eran Ofek (May 2021)
            % Example: D = imProc.image.Dark;
            %          [Result, OverScanAI] = D.overscan(AI, 'OverScan',[1 10 1 9600])
            %          [Result, OverScanAI] = D.overscan(AI, 'OverScan',[6379 6388 1 9600])
            %          [Result, OverScanAI] = D.overscan(AI, 'OverScan',[6379 6388 1 9600],'Method','medmedfilt')
            
            arguments
                Obj(1,1)
                ImObj AstroImage
                Args.OverScan                    = 'OVERSCAN';  % keyword or CCDSEC
                Args.Subtract(1,1) logical       = true;
                Args.RemoveOverScan(1,1) logical = true;
                Args.RemoveOthers(1,1) logical   = true;
                Args.OverScanDir                 = []; % 'x'|'y',1,2,[]- auto
                Args.Method                      = 'globalmedian';
                Args.MethodArgs cell             = {50};
                Args.UpdateCCDSEC(1,1) logical   = true;
                Args.KeyCCDSEC                   = 'CCDSEC';
                Args.DataProp                    = 'ImageData';
                Args.DataPropIn                  = 'Image';
                
                Args.CreateNewObj                = [];
            end
            DefArgSGolay = {3 50}; 
            
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            
            if Args.CreateNewObj
                Result = ImObj.copy();
            else
                Result = ImObj;
            end
            
            Nim = numel(ImObj);
            if nargout>1
                OverScanAI = AstroImage(size(ImObj));
            end
            for Iim=1:1:Nim
                if ischar(Args.OverScan)
                    % read from header
                    OverScan = getval(ImObj(Iim).HeaderData,'ReadCCDSEC',true);
                else
                    OverScan = Args.OverScan;
                end
                
                % cut overscan
                OverScanImage = ImObj(Iim).Image(OverScan(3):OverScan(4), OverScan(1):OverScan(2));
                
                
                % overscan direction
                if isempty(Args.OverScanDir)
                    % select according to the long axis
                    SizeOverScan = size(OverScanImage);
                    if SizeOverScan(1)>SizeOverScan(2)
                        % If the overscan region is columns than Dir=2
                        Dir = 2;
                    else
                        % If the overscan region is rows than Dir=1
                        Dir = 1;
                    end
                else
                    if isnumeric(Args.OverScanDir)
                        Dir = Args.OverScanDir;
                    else
                        switch lower(Args.OverScanDir)
                            case 'x'
                                Dir = 2;
                            case 'y'
                                Dir = 1;
                            otherwise
                                error('Unknown OverScanDir option');
                        end
                    end
                end
                
                % perform analysis on OverScanImage
                switch lower(Args.Method)
                    case 'globalmedian'
                        OverScanLine = median(OverScanImage,'all','omitnan');
                    case 'globalmean'
                        OverScanLine = mean(OverScanImage,'all','omitnan');
                    case 'median'
                        OverScanLine = median(OverScanImage,Dir,'omitnan');
                    case 'mean'
                        OverScanLine = mean(OverScanImage,Dir,'omitnan');
                    case 'medmedfilt'
                        % medain followed by median filter
                        OverScanLine = median(OverScanImage,Dir,'omitnan');
                        OverScanLine = medfilt1(OverScanLine,Args.MethodArgs{:});
                    case 'medsgolayfilt'
                        % medain followed by Svitzky-Golay filter
                        OverScanLine = median(OverScanImage,Dir,'omitnan');
                        if numel(Args.MethodArgs)<2
                            Args.MethodArgs = DefArgSGolay;
                        end
                        OverScanLine = sgolayfilt(OverScanLine,Args.MethodArgs{:});
                    otherwise
                        error('Unknown Method option');
                end
                
                % subtract OverScanLine
                if Args.Subtract
                    Result(Iim).(Args.DataProp).(Args.DataPropIn) = Result.(Args.DataProp).(Args.DataPropIn) - OverScanLine;
                end
                
                SizeOriginalImageIJ = size(Result.(Args.DataProp).(Args.DataPropIn));
                CCDSEC = imUtil.ccdsec.remove_edge_section(SizeOriginalImageIJ, OverScan, Dir);
                % remove overscan from image
                if Args.RemoveOverScan
                    % calculate CCDSEC (the non-overscan region)
                    Result(Iim).(Args.DataProp).(Args.DataPropIn) = Result.(Args.DataProp).(Args.DataPropIn)(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
                end
                
                % remove overscan from other images
                if Args.RemoveOthers
                    % calculate CCDSEC (the non-overscan region)
                    if ~isempty(Result(Iim).BackData.Data)
                        Result(Iim).BackData.Image = Result.BackData.Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
                    end
                    if ~isempty(Result(Iim).VarData.Data)
                        Result(Iim).VarData.Image = Result.VarData.Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
                    end
                    if ~isempty(Result(Iim).MaskData.Data)
                        Result(Iim).MaskData.Image = Result.MaskData.Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
                    end
                end
                
                % update Header
                if Args.UpdateCCDSEC
                    StrCCDSEC = imUtil.ccdsec.ccdsec2str(CCDSEC);
                    Result(Iim).HeaderData.replaceVal(Args.KeyCCDSEC, {StrCCDSEC});
                    
                    % update the NAXIS keywords
                    SizeImage = size(Result(Iim).(Args.DataProp).(Args.DataPropIn));
                    Result(Iim).HeaderData.replaceVal('NAXIS1', {SizeImage(1)});
                    Result(Iim).HeaderData.replaceVal('NAXIS1', {SizeImage(2)});
                end
                
                % store over scan in AstroImage (only if requested)
                if nargout>1
                    OverScanAI(Iim).Image = OverScanImage;
                    OverScanAI(Iim).Back  = OverScanLine;
                end        
                        
            end
            
        end
    end
    
    methods (Static) % unitTest
        function Result = unitTest()
            % unitTest for the Dark class
            % Example: Result = imProc.image.Dark.unitTest
            
            D  = imProc.image.Dark;
            % identifySimilarImages
            AI = AstroImage({ones(100,100), ones(100,100)});
            [PassedThreshold, FracIdentical] = D.identifySimilarImages(AI);
            if ~PassedThreshold
                error('identifySimilarImages failed on detecting identical pixels');
            end
            AI = AstroImage({ones(100,100), zeros(100,100)});
            [PassedThreshold, FracIdentical] = D.identifySimilarImages(AI);
            if PassedThreshold
                error('identifySimilarImages failed on detecting identical pixels');
            end
            
            % compare2template
            AI = AstroImage({2.*randn(10,10)});
            Template = AstroImage({0},'Var',{4});
            D = imProc.image.Dark;
            [FlagBad, FracbadPixels, Z] = D.compare2template(AI, 'Template',Template);
            if FlagBad
                error('Possible problem with compare2template');
            end
            
            % identifyFlaringPixels
            Cube = randn(100,100,10);
            Cube(1,1,1)=30;
            Dark = imProc.image.Dark;
            [Result,Mean,Std,Max] = identifyFlaringPixels(Dark, Cube);
            [Result,Mean,Std,Max] = identifyFlaringPixels(Dark, Cube,'MeanFunArgs',{'all'});
            if ~Result(1,1) && all(Result(:,2))
                error('Possible problem with identifyFlaringPixels');
            end
            
            % isBias/isDark
            AI=AstroImage({rand(100,100)});
            [Result,Flag] = D.isBias(AI);
            if Result
                error('isBias problem');
            end
            AI.HeaderData.insertKey({'IMTYPE','Dark'});
            [Result,Flag] = D.isBias(AI);
            if Result
                error('isBias problem');
            end
            [Result,Flag] = D.isDark(AI);
            if ~Result
                error('isBias problem');
            end
            
            % bias
            AI=AstroImage({rand(100,100), rand(100,100), rand(100,100), rand(100,100), rand(100,100)});
            AI.funHeader(@insertKey, {'IMTYPE','Bias'});
            AI = [AI, AstroImage({rand(100,100)})];
            Bias = D.bias(AI);
            
            % debias
            AI = D.debias(AI);
            
            % overscan
            AI = AstroImage({rand(200,100)});
            [Result, OverScanAI] = D.overscan(AI, 'OverScan',[1 10 1 200]);
            [Result, OverScanAI] = D.overscan(AI, 'OverScan',[91 100 1 200]);
            [Result, OverScanAI] = D.overscan(AI, 'OverScan',[91 100 1 200],'Method','medmedfilt');
            [y,x] = sizeImage(Result);
            if ~(y==90 && x==200)
                error('Problem with overscan subtraction');
            end
            
            Result = true;
            
        end
    end
end
