function Obj = fwhm(Obj, Args)
    % Measure the FWHM from the PSF in an AstroImage and write in Header.
    %   If the AstroPSF is not pupulated, this function will populate it.
    %   Also add the median A, B, Theta of sources.
    %   If PSF can't be constructed (i.e., no stars) then populate header
    %   with NaNs.
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
    %            'KeysMom2' - 2nd moment column names in catalog.
    %                   Default is {'X2','Y2','XY'}.
    % Output : - The AstroImage object with the populated PSF object and FWHM in
    %            the header [arcsec].
    %            Also pipulated are the MED_A [pix], MED_B [pix], MED_TH [deg]
    % Author : Eran Ofek (Jan 2022)
    % Example: imProc.psf.fwhm(Coadd);
    
    arguments
        Obj AstroImage
        Args.Scale                  = [];  % if empty - figure out from WCS
        Args.AddToHeader logical    = true;
        Args.HeaderKey              = 'FWHM';
        Args.AddPos                 = Inf;
        Args.AddMom2 logical        = true;
        Args.KeysMom2 cell          = {'X2','Y2','XY'};
    end
    ARCSEC_DEG = 3600;    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isemptyPSF(Obj(Iobj).PSFData)
            % construct the PSF
            if isnan(Obj(Iobj).PSFData.Nstars)
                [Obj(Iobj)] = imProc.psf.constructPSF(Obj(Iobj), Args.constructPSFArgs{:});
            end
        end
        
        if Obj(Iobj).PSFData.Nstars>0
            if isempty(Args.Scale)
                % get scale from WCS
                Scale = 0.5.*(abs(Obj(Iobj).WCS.CD(1,1)) + abs(Obj(Iobj).WCS.CD(2,2))) .* ARCSEC_DEG;
            else
                Scale = Args.Scale;
            end
            
            [FWHM_C, FWHM_H] = Obj(Iobj).PSFData.fwhm;
            FWHM_C = FWHM_C.*Scale;
            %FWHM_H = FWHM_H.*Scale;
        else
            % NO PSF - put NaNs in header
            FWHM_C = NaN;
        end

        % add FWHM to header
        if Args.AddToHeader
            Obj(Iobj).HeaderData.replaceVal(Args.HeaderKey, FWHM_C, 'AddPos',Args.AddPos);
        end
    
        % add 2nd moment information
        if Args.AddMom2
            if Obj(Iobj).PSFData.Nstars>0
                M2 = Obj(Iobj).CatData.getCol(Args.KeysMom2);
                [AB] = imUtil.psf.mom2shape(M2(:,1),M2(:,2),M2(:,3));
                Med  = median([AB.A, AB.B, AB.Theta],1,'omitnan');
                Med(3) = Med(3).*180./pi;
            else
                Med = nan(1,3);
            end
            Obj(Iobj).HeaderData.replaceVal({'MED_A','MED_B','MED_TH'}, Med, 'AddPos',Args.AddPos);
        end
    end    
end

