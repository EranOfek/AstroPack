function [MeanImage, PowerSpec] = genAveragePSF(Args)
    % Generate an average PSF from multiple speckle images
    % Input  : * ...,key,val,...
    %            'Nimages' - Numbre of speckle realizatons to add.
    %                   Default is 100.
    %            'ImageSize' - Image size. Default is 256.
    %            'Norder' - Zernike polynomial order. Default is 100.
    %            'r0' - Fried length. Default is 5.
    %            'D' - Telescope diameter. Default is 100.
    % Output : - A combined average image.
    %          - The average power spectrum, where the zero frequency is at
    %            the image edges.
    % Author : Eran Ofek (Feb 2022)
    % Example: [MeanImage, PowerSpec] = telescope.Optics.genAveragePSF;
    %          surface(MeanImage); shading interp; colorbar
    %          surface(log10(PowerSpec)); shading interp; colorbar
    %          [Radius,Prof,ProfErr,Npix]=ImUtil.Im.radial_profile(fftshift(PowerSpec));
    %          loglog(Radius,Prof); axis([1 128 1e-6 1])
    %          
    
    arguments
        Args.Nimages      = 100;
        Args.ImageSize    = 256;
        Args.Norder       = 100;
        Args.r0           = 5;
        Args.D            = 100;
    end
    
    J = (1:1:Args.Norder);
    
    MeanImage = zeros(Args.ImageSize, Args.ImageSize);
    PowerSpec = zeros(Args.ImageSize, Args.ImageSize);
    for Iim=1:1:Args.Nimages
        % generate a random realization of the zernike coef.
        [~,J,C]=telescope.Optics.zer_cj_variance(Args.Norder, 'Nrand',1, 'D',Args.D, 'r0',Args.r0);
        % generate an image based on the current realization
        [Image_NS,Image] = telescope.Optics.zerwavefront2image(J,[],C);
        MeanImage = MeanImage + Image;
        
        if nargout>1
            PowerSpec = PowerSpec + abs(fft2(Image_NS)).^2;                                               
        end
    end
    % averaging
    MeanImage = MeanImage./Args.Nimages;
    if nargout>1
        PowerSpec = PowerSpec./Args.Nimages;
    end
    
end