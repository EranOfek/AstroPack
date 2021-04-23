
classdef Dark < Component
    properties
        Images                               % Images on which to work
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
        
        function Result = compare2template(DarkObj, Obj, Args)
            %
            
            arguments
                DarkObj(1,1)
                Obj
                Args.Template
                Args.TemplateVar
                Args.Nsigma                           = 5;
                Args.MaxFracBadPixels(1,1)            = 0.0001;
            end
            
            
        end
        
        function Result = validateNoise(DarkObj, DarkImages, SuperDark, Args)
            % Validate that the noise in the image is consistent with readnoise and/or dark current noise
            % Input  : -
            % Output : -
            % Author : 
            % Example: 
           
            arguments
                DarkObj
                Obj          
                Args
            end
            
            
        end
        
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
            
        end
        
        
        
        
    end
    
end
