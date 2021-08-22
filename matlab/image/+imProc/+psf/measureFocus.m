function [FWHM, Nstars] = measureFWHM(Obj, Args)
    % Estimate image seeing or focus state
    % Input  : - AN image. Either a matrix, a cell of matrices, or an
    %            AstroImage object.
    %          * list of ...,key,val,...
    %            'MinSN' - Minimum S/N to use. Default is 50.
    %            'Background' - A background image. Default is [].
    %            'Variance'   - A variance image. Default is [].
    %            'SigmaVec'   - Vector of the Gaussian bank sigmas.
    %                           This should not include a sharp object (such a
    %                           sharp object is added by the code).
    %                           Default is logspace(0,1,25).'
    %            'MinStars'   - Minimum numbre of stars needed to estimate
    %                           FWHM. Default is 5.
    %            'PixScale'   - Pixel scale ["/pix]. Default is 1.
    %            'Method'     - Method: 
    %                           'bisec' - Bi-sector search
    %                           'MaxNdet' - Choose the filter with the max
    %                                   number of detections.
    %                           'MaxNdetInterp' - Same as 'MaxNdet', but with
    %                                   interpolation over number of detections.
    %                           Default is 'bisec'.
    %            'MaxIter'    - Numbre of iterations for the 'bisec' method.
    %                           Default is 6.
    % Output : - FWHM [arcsec].
    %          - Number of stars used for estimating the FWHM.
    % Author : Eran Ofek (Aug 2021)
    % Example: [F,N]=imProc.psf.measureFocus(AI)
    
    arguments
        Obj
        Args.CCDSEC       = [];
        Args.MinSN        = 50;
        Args.SigmaVec     = logspace(0,2,5);
        Args.MinStars     = 5;
        Args.PixScale     = 1;  % "/pix
        Args.Method       = 'bisec';
        Args.MaxIter      = 6;
    end

    if isnumeric(Obj) || iscell(Obj)
        Obj = AstroImage(Obj);
    end
        
    Nobj   = numel(Obj);
    FWHM   = nan(size(Obj));
    Nstars = nan(size(Obj));
    for Iobj=1:1:Nobj
        [FWHM(Iobj), Nstars(Iobj)] = imUtil.psf.fwhm_fromBank(Obj(Iobj).Image, 'Background',Obj(Iobj).Back,...
                                                         'Variance', Obj(Iobj).Var,...
                                                         'CCDSEC',Args.CCDSEC,...
                                                         'MinSN',Args.MinSN,...
                                                         'SigmaVec',Args.SigmaVec,...
                                                         'MinStars',Args.MinStars,...
                                                         'PixScale',Args.PixScale,...
                                                         'Method',Args.Method,...
                                                         'MaxIter',Args.MaxIter);
    end
    
    
end