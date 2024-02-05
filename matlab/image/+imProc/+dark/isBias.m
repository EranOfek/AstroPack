function [Result,Flag] = isBias(AI, Args)
    % Check and validate that a set of images in an AstroImage object are bias images
    % Input  : - An AstroImage object.
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
    %          F=imProc.image.isBias(A);

    arguments
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
        Args.SearchAlgo char                                            = 'strcmp'; 
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
        [PassedThresholdIdentical, FracIdentical] = imProc.dark.identifySimilarImages(AI, 'DataProp','Image', 'MaxAllowedFrac',Args.MaxAllowedFrac);
        Flag.IdenticalPixOK = ~PassedThresholdIdentical;
    end
    if isempty(Args.Template)
        Flag.TemplateOK = true(size(Flag.IsImType));
    else
        [Flag.FlagBad, FracBadPixels, Z] = imProc.dark.compare2template(AI, Args.Template,...
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
