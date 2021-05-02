
classdef Dark < Component
    properties
        Images                               % Images on which to work
        Template
        TemplateVar
        Nsigma
        MaxFracBadPixels
        StackMethod 
        
    end
    
    methods  % Constructor
        function Obj = Dark(Args)
            % Constructor for a Dark object
            % Input  : * ...,key,val,...
            %            Can be any Match object property name followed by
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
            %                   Default is 0.001.
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
                Obj
                Args.DataProp                = 'Image';
                Args.MaxAllowedFrac          = 0.001;
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
            %       
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
                Obj
                Args.Template
                Args.TemplateVar                      = [];
                Args.Nsigma                           = 5;
                Args.MaxFracBadPixels(1,1)            = 0.0001;
                Args.UseImageVar                      = true;
                
                Args.DataProp                         = 'Image';
                Args.VarProp                          = 'Var';
            end
            
            % use object default arguments if not supplied by user
            Args = selectDefaultArgsFromProp(DarkObj, Args);
            if isempty(Obj)
                Obj = DarkObj.Images;
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
        
%         function Result = validateNoise(DarkObj, DarkImages, SuperDark, Args)
%             % Validate that the noise in the image is consistent with readnoise and/or dark current noise
%             % Input  : -
%             % Output : -
%             % Author : 
%             % Example: 
%            
%             arguments
%                 DarkObj
%                 Obj          
%                 Args.X
%             end
%             
%             
%         end
        
        function Result = isBias(Obj, AI, Args)
            % 
            % Input  : - An AstroImage object.
            
            arguments
                Obj
                AI
                Args.ImTypeKeyName                                              = 'IMTYPE';
                
                Args.UseDict(1,1) logical                                       = true;
                Args.CaseSens(1,1) logical                                      = true;
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.IsInputAlt(1,1) logical                                    = true;
                Args.KeyDict                                                    = [];
            end
            ImTypeVal = 'Bias';
            
            % AI is now an AstroImage object
            Result = isImType(Obj, ImTypeVal, Args);
            
            % validation
            
            
        end
        
        function Bias = bias(Obj, ImObj, Args)
            %
            
            arguments
                Obj
                ImObj AstroImage
                Args.IsBias                     = true;  % if empty - call isBias
                Args.IsBiasArgs cell            = {};
                Args.StackMethod                = 'sigmaclip';   
                Args.StackArgs                  = {'MeanFun',@nanmean, 'StdFun','rstd', 'Nsigma',[5 5], 'MaxIter',3};
                Args.EmpiricalVarFun            = @var;
                Args.EmpiricalVarFunArgs        = {[],3,'omitnan'};
                Args.DivideEmpiricalByN         = false;
                
                Args.StackVarMethod
                Args.StackVarArgs
            end
            
            Nim = numel(ImObj);
            if isempty(Args.IsBias)
                IsBias = Obj.isBias(ImObj, Args.IsBiasArgs{:});
            else
                IsBias = Args.IsBias;
                if numel(IsBias)==1 && Nim>1
                    if IsBias
                        IsBias = true(Nim,1);
                    else
                        IsBias = false(Nim,1);
                    end
                end
            end

            % validate individual bias images
            
            C = imProc.image.Stack;
            [Result, CoaddN, ImageCube] = C.coadd(ImObj, 'CCDSEC',[],...
                                              'Offset',[],...
                                              'PreNorm',[],...
                                              'UseWeighs',false,...
                                              'StackMethod',Args.StackMethod,...
                                              'StackArgs',Args.StackArgs,...
                                              'CombineBack',false,...
                                              'CombineMask',true,...
                                              'EmpiricalVarFun',Args.EmpiricalVarFun,...
                                              'EmpiricalVarFunArgs',Args.EmpiricalVarFunArgs,...
                                              'MedianVarCorrForEmpirical',false,...
                                              'DivideEmpiricalByN',Args.DivideEmpiricalByN,...
                                              'PostNorm',[]);
                                          
             % Prepare Mask image
             % mask LowRN
             
             % mask HighRN
             
             % mask DarkHighVal
             
             % mask DarkLowVal
             
             % mask BiasFlaring
             
                                          
                                          
                                          
                
        end
        
        function Dark = dark(DarkObj, Images, Args)
            %
            
            arguments
                DarkObj
                Images AstroImage
                Args.StackMethod              
                Args.StackArgs
                Args.StackVarMethod
                Args.StackVarArgs
            end
            
        end
        
        
        
        
    end
    
end
