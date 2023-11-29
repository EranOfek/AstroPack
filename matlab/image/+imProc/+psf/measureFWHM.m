function [FWHM, Nstars] = measureFWHM(Obj, Args)
    % Estimate image seeing or focus state
    % Input  : - An image. Either a matrix, a cell of matrices, or an
    %            AstroImage object.
    %          * list of ...,key,val,...
    %            'CCDSEC' - CCDSEC [Xmin, Xmax, Ymin, Ymax] on which to run
    %                   the code.
    %                   If The input AstroImage is a single element object,
    %                   then this can be a multiple lines CCDSEC, and the
    %                   FWHM will be calculated in each CCDSEC of the same
    %                   image.
    %                   If empty, use all. Dedault is [].
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
    % Example: [F,N]=imProc.psf.measureFWHM(AI)
    
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

    
    if isnumeric(Obj)
        Obj = AstroImage({Obj});
    elseif iscell(Obj)
        Obj = AstroImage(Obj);
    end
        
    Nobj   = numel(Obj);
    Nccdsec = size(Args.CCDSEC,1);
    if Nobj==1 && Nccdsec>1
        FWHM   = nan(Nccdsec,1);
        Nstars = nan(Nccdsec,1);
        Iobj   = 1;
        for Iccd=1:1:Nccdsec
            [FWHM(Iccd), Nstars(Iccd)] = imUtil.psf.fwhm_fromBank(Obj(Iobj).Image, 'Background',Obj(Iobj).Back,...
                                                             'Variance', Obj(Iobj).Var,...
                                                             'CCDSEC',Args.CCDSEC(Iccd,:),...
                                                             'MinSN',Args.MinSN,...
                                                             'SigmaVec',Args.SigmaVec,...
                                                             'MinStars',Args.MinStars,...
                                                             'PixScale',Args.PixScale,...
                                                             'Method',Args.Method,...
                                                             'MaxIter',Args.MaxIter);
        end
    else
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
    
end