
classdef Flat < Component
    properties
        Flat AstroImage
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
        
        
    end
    
    methods % nonlinearity
    
        
    end
    
end