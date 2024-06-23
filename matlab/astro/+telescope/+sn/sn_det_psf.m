function [SN] = sn_det_psf(Flux, Back, Sigma, Args)
    % Calculate S/N for detection of a Gaussian PSF
    %     Using Equation 1 in Ofek & Ben-Ami (2020).
    % Input  : - Flux from sources (either [e or e/s].
    %          - Background per pixel (either e or e/s].
    %          - Sigma of Gaussian PSF. Default is 1.
    %            Sigma = FWHM/2.35.
    %          * ...,key,val,... 
    %            'Texp' - Exposure time. Default is 1.
    %            'RN' - Readout noise [e]. Default is 0.
    % Output : - S/N for detection of a Gaussian PSF assuming optimal
    %            matched filtering detection.
    % Author : Eran Ofek (2024 Jun) 
    % Example: telescope.sn.sn_det_psf(500, 100, 1.5)

    arguments
        Flux
        Back
        Sigma                  = 1;
        Args.Texp              = 1;
        Args.RN                = 0;
    end
    
    SN = Flux.*Args.Texp./sqrt(4.*pi.*Sigma.^2 .* (Back.* Args.Texp + Args.RN.^2));
    
end
