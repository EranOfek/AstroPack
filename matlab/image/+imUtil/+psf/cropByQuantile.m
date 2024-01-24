function [Stamp, Var] = cropByQuantile(PSF, Quantile, Args)
    % crop a PSF stamp so that to keep only Quantile of the total flux 
    % Input :  - PSF stamp
    %          - Quantile (0-1)
    %          * ...,key,val,...
    %          'Normalize' - whether to normalize the output PSF stamp
    %          'Variance'  - a variance matrix (optional); once it is given, it is also cropped 
    % Output : - the cropped PSF stamp   
    %          - the cropped Variance image (optional)
    % Author: A.M. Krassilchtchikov (Nov 2023)
    % Example: P = imUtil.kernel2.gauss;
    %          P95 = imUtil.psf.cropByQuantile(P,0.95);
    arguments
        PSF
        Quantile       = 0.95;
        Args.Normalize = true;
        Args.Variance  = [];
    end
    if isempty(PSF) % do nothing
        Stamp = PSF;
        return
    end
    if ~isempty(Args.Variance)        
        [~, Stamp, Var] = imUtil.psf.quantileRadius(PSF, 'Level', Quantile, 'Variance', Args.Variance);
    else
        [~, Stamp, ~]   = imUtil.psf.quantileRadius(PSF, 'Level', Quantile, 'Variance', Args.Variance);
    end
    if Args.Normalize
        Stamp = imUtil.psf.normPSF(Stamp); 
    end
end