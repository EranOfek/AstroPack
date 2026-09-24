function [MTF, OTF] = psf2mtf(PSF)
    % Convert a PSF to MTF and OTF
    %     MTF is modulation transfer function
    %     OTF is optical transfer function
    % Input  : - PSF stamp
    % Output : - MTF
    %          - OTF
    % Author : Eran Ofek + ChatGPT (2026 Sep) 
    % Example: imUtil.psf.psf2mtf(imUtil.kernel2.gauss)

    PSF = PSF ./ sum(PSF(:));
    
    OTF = fftshift(fft2(ifftshift(PSF)));
    MTF = abs(OTF);

end
