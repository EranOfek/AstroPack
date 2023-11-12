function Stamp = cropByQuantile(PSF, Quantile, Args)
    % crop a PSF stamp so that to keep only Quantile of the total flux 
    % Input :  - PSF stamp
    %          - Quantile (0-1)
    %          * ...,key,val,...
    %          'Normalize' - whether to normalize the output PSF stamp
    % Output : - the cropped PSF stamp       
    % Author: A.M. Krassilchtchikov (Nov 2023)
    % Example: P = imUtil.kernel2.gauss;
    %          P95 = imUtil.psf.cropByQuantile(P,0.95);
    arguments
        PSF
        Quantile       = 0.95;
        Args.Normalize = true;
    end
    [~, Stamp] = imUtil.psf.quantileRadius(PSF, 'Level', Quantile);
    if Args.Normalize
        Stamp = imUtil.psf.normPSF(Stamp);
    end
end