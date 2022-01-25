function Obj = fwhm(Obj, Args)
    % Measure the FWHM from the PSF in an AstroImage and write in Header.
    %   If the AstroPSF is not pupulated, this function will populate it.
    %   see also AstroPSF/fwhm
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'Scale' - Image scale (arcsec/pix) that will be used in order to
    %                   convert the FWHM to arcsec. If empty, then will
    %                   take the Scale from the AstroImage.WCS object.
    %                   Default is [].
    %            'AddToHeader' - A logical indicating if to add FWHM based
    %                   on cumsum to header. Default is true.
    %            'HeaderKey' - Header keyword in which to add the FWHM.
    %                   Default is 'FWHM'.
    %            'AddPos' - Position in the hedaer in which to add the FWHM
    %                   keyword. Default is Inf.
    % Output : - The AstroImage object with the populated PSF object and FWHM in
    %            the header.
    % Author : Eran Ofek (Jan 2022)
    % Example: imProc.psf.fwhm(Coadd);
    
    arguments
        Obj AstroImage
        Args.Scale                  = [];  % if empty - figure out from WCS
        Args.AddToHeader logical    = true;
        Args.HeaderKey              = 'FWHM';
        Args.AddPos                 = Inf;
    end
    ARCSEC_DEG = 3600;    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isemptyPSF(Obj(Iobj).PSFData)
            % construct the PSF
            [Obj(Iobj)] = imProc.psf.constructPSF(Obj(Iobj), Args.constructPSFArgs{:});
        end
        
        if isempty(Args.Scale)
            % get scale from WCS
            Scale = 0.5.*(abs(Obj(Iobj).WCS.CD(1,1)) + abs(Obj(Iobj).WCS.CD(2,2))) .* ARCSEC_DEG;
        else
            Scale = Args.Scale;
        end
        
        [FWHM_C, FWHM_H] = Obj(Iobj).PSFData.fwhm;
        FWHM_C = FWHM_C.*Scale;
        %FWHM_H = FWHM_H.*Scale;
        
        % add FWHM to header
        if Args.AddToHeader
            Obj(Iobj).HeaderData.replaceVal(Args.HeaderKey, FWHM_C, 'AddPos',Args.AddPos);
        end
    
    end    
end

