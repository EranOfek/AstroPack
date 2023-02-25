function [Xsize, Ysize] = pseudoFWHM (Args)
    % Measure pseudo FWHM width in a PSF stamp at a given level
    % Package: imUtil.psf
    % Description: Measure pseudo FWHM width in a PSF stamp at a given level
    %          - Args.PSF: a 2D array containing the PSF stamp 
    %          - Args.Level: signal level relative to the maximum, 0.5 is default
    % Output : - Rad: pseudo FWHM width in pixels
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Feb 2023
    % Example: [FWHM_X, FWHM_Y] = pseudoFWHM (PSF, 0.5)
    
    arguments
        
        Args.PSF
        
        Args.Level     =    0.5; % at half maximum
        
    end
    
    % normalize the PSF stamp to 1:
    
    PSF = Args.PSF / sum(Args.PSF,'all');

    % find the maximal level:

    MaxLevel = max(PSF,[],'all');
    HalfMax  = MaxLevel * Args.Level; % actually may differ from "half"
    
    % find the smaller size of an M x N array:
    
    SizeX       = size(PSF,1);
    SizeY       = size(PSF,2);

    Xmin        = SizeX;
    Xmax        = 1;
    Ymin        = SizeY;
    Ymax        = 1;

    for iX = 1:1:SizeX
        for iY = 1:1:SizeY

            if PSF(iX, iY) >= HalfMax

                Xmin = min(iX, Xmin);
                Xmax = max(iX, Xmax);
                Ymin = min(iY, Ymin);
                Ymax = max(iY, Ymax);

            end

        end
    end

    Xsize = Xmax - Xmin;
    Ysize = Ymax - Ymin;
    
end