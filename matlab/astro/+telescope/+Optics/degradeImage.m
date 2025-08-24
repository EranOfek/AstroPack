function [NewImage] = degradeImage(Image, PixScale, PSF, PSFScale, Args)
    % Degrate image quality by convolving with a PSF
    % Input  : - Input image.
    %          - Pixel scale of input image. Default is 1.
    %          - PSF image. If scale, then a Gaussian PSF sigma-width.
    %            If 'speckle', then generate a random speckle image
    %            with D and r0 (see args).
    %            Default is 2.
    %          - PSFScale. Default is 1.
    %          * ...,key,val,... 
    %            See code for options.
    %       
    % Output : - Degraded image.
    % Author : Eran Ofek (2025 Aug) 
    % Example: NIm = telescope.Optics.degradeImage(Image, 0.3, 2n);

    arguments
        Image
        PixScale      = 1;   % [e.g., arcsec/pix]
        PSF           = 2;
        PSFScale      = 1;   % [e.g., arcsec/pix]
        Args.InterpMethod = 'lanczos2';
        Args.NormPSF  = true;
        Args.D        = 100;
        Args.r0       = 5;
    end

    if ischar(PSF)
        switch PSF
            case 'speckle'
                % generate a speckle PSF

                J = (1:1:100);
                
                [AmpC,J,C]  = telescope.Optics.zer_cj_variance(100, 'Nrand',1, 'D',Args.D, 'r0',Args.r0);
                [~,PSF]   = telescope.Optics.zerwavefront2image(J,[],C);
                %pcolor(log10(Image)), shading interp; axis square, colorbar
        end
    elseif numel(PSF)==1
        PSF = imUtil.kernel2.gauss(PSF);
    else
        % do nothing
    end

    % Scale the PSF to the scale of the image
    
    NewPSF = imresize(PSF, PSFScale./PixScale, 'Method',Args.InterpMethod);
    
    if Args.NormPSF
        NormPSF1      = sum(PSF, 'all');
        NormNewPSF1   = sum(NewPSF, 'all');
        NewPSF        = NewPSF.*NormPSF1./NormNewPSF1;
    end

    NewImage = conv2(Image, NewPSF, 'same');


end
