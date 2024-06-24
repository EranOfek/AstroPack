function [Flux] = sn2flux(SN, Back, PSF)
    % Convert S/N for dtection to Flux, given background and PSF.
    % Input  : - Vector of S/N.
    %          - Background (scalar).
    %          - A scalar with the sigma-width of a
    %            Gaussian PSF.
    % Output : - Flux of source that will generate the requested S/N for
    %            optimal detection.
    % Author : Eran Ofek (2024 Jun) 
    % Example: telescope.sn.sn2flux(5,100,K)
    %          SN=telescope.sn.sn_det_psf(500, 100, 1.5)
    %          telescope.sn.sn2flux(SN,100,1.5)
    %          

    arguments
        SN
        Back(1,1)
        PSF
    end
    
    if numel(PSF)>1
        % PSF
        error('bug');
        
        SN1  = sqrt(sum((1.*PSF./sqrt(Back)).^2, [1 2]));
        SN10 = sqrt(sum((10.*PSF./sqrt(Back)).^2, [1 2]));
        PV = polyfit([1 10],[SN1 SN10],1);
        Flux = polyval(PV,SN);
        
    else
        % assume PSF contains Sigma width of Gaussian PSF
        Flux = SN.*sqrt(4.*pi.*PSF.^2 .* (Back));
    end
    
end
