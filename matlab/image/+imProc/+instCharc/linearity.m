function Result = linearity(Obj, Args)
    % Estimate the non-linearity of a detector
    %   using the imUtil.calib.pixel_flat_response function
    % Input  : - An AstroImage object containing multiple flat images with
    %            different illumination intensity.
    %          * ...,key,val,...
    %            'Method' - Options are:
    %                   ['constFlux'] - Estimate the non-linearity of each
    %                       pixel by hypothesis testing of two models:
    %                       c+x and c+x+x^2.
    %                       This assumes that the source of light is
    %                       constant, so the intensity is linear with the
    %                       exposre time.
    %            'KeyExpTime' - Header keyword name (or dictionary name)
    %                   containing the exp. time. Default is 'EXPTIME'.
    %            'UseDict' - A logical indicating if to use dictionary for
    %                   the exp. time keyword. Default is true.
    %            'CCDSEC' - CCDSEC on which to calculate the non-linearity.
    %                   If empty, use all image. Default is [].
    %            'Gain' - Detector gain. Default is 1.
    %            'ReadNoise' - Detector readnoise. Default is 5 e-.
    %            'Model' - Models to fit. Default is {'c+x','c+x+x^2'}.
    %                   See imUtil.calib.pixel_flat_response for details.
    % Output : - A structure of the fit results.
    %            See imUtil.calib.pixel_flat_response for details.
    % Author : Eran Ofek (Jun 2021)
    % Example: Im = ones(500,500);
    %          AI = AstroImage({poissrnd(Im.*1e3), poissrnd(Im*3e3), poissrnd(Im.*1e4), poissrnd(Im.*1e4), poissrnd(Im.*2e4)});
    %          AI(1).HeaderData.insertKey({'EXPTIME',1}); AI(2).HeaderData.insertKey({'EXPTIME',3}); AI(3).HeaderData.insertKey({'EXPTIME',10});
    %          AI(4).HeaderData.insertKey({'EXPTIME',10}); AI(5).HeaderData.insertKey({'EXPTIME',20});
    %          Result = imProc.instCharc.linearity(AI);
    
    arguments
        Obj
        Args.Method                   = 'constFlux';
        Args.KeyExpTime               = 'EXPTIME';
        Args.UseDict(1,1) logical     = true;
        Args.CCDSEC                   = [];
        Args.Gain                     = 1;
        Args.ReadNoise                = 5;  % e-
        Args.Model cell               = {'c+x','c+x+x^2'};
    end
    
    switch lower(Args.Method)
        case 'constflux'
            % constant flux / variable exp time
            Nim     = numel(Obj);
            ExpTime = getStructKey(Obj, Args.KeyExpTime, 'UseDict',Args.UseDict); 
            
            Cube    = imProc.image.images2cube(Obj, 'CCDSEC',Args.CCDSEC);
            
            Result = imUtil.calib.pixel_flat_response(Cube, 'Gain',Args.Gain, 'ReadNoise',Args.ReadNoise,...
                                                            'Model',Args.Model,...
                                                            'Intensity',[ExpTime.(Args.KeyExpTime)]);
            
        otherwise
            error('Unknown Method option');
    end
    
    
end
