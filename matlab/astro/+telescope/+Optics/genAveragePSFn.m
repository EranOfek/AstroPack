function [Image, PowerSpec, AllImages] = genAveragePSFn(Args)
    % Generate a multiple-stars PSF from multiple speckle images
    % Input  : * ...,key,val,...
    %            'Flux' - Vector of flux, per image, of sources.
    %                   Default is [1000 100].
    %            'PosXY' - A two column matrix [X, Y] of positions of the
    %                   2:end stars relative to the position of the 1st
    %                   star. The first star is always in the image center.
    %                   Default is [1000 100].
    %            'PoissNoise' - A logical indicating if to add a Poisson
    %                   noise. Default is true.
    %            'Nimages' - Numbre of speckle realizatons to add.
    %                   Default is 100.
    %            'ImageSize' - Image size. Default is 256.
    %            'Norder' - Zernike polynomial order. Default is 100.
    %            'r0' - Fried length. Default is 5.
    %            'D' - Telescope diameter. Default is 100.
    % Output : - A combined average image.
    %          - The average power spectrum, where the zero frequency is at
    %            the image edges.
    %          - A cube of all individual generated images.
    % Author : Eran Ofek (Feb 2022)
    % Example: [Image, PowerSpec] = telescope.Optics.genAveragePSFn;
    %          surface(Image); shading interp; colorbar
    %          surface(log10(PowerSpec)); shading interp; colorbar
    %          [Radius,Prof,ProfErr,Npix]=ImUtil.Im.radial_profile(fftshift(PowerSpec));
    %          loglog(Radius,Prof); axis([1 128 1e-6 1])
    %          
    
    arguments
        Args.Flux         = [1000 100];
        Args.PosXY        = [20 0]
        Args.PoissNoise logical = false;
        Args.Nimages      = 100;
        Args.ImageSize    = 256;
        Args.Norder       = 100;
        Args.r0           = 5;
        Args.D            = 100;
    end
    
    Nflux = numel(Args.Flux);
    J = (1:1:Args.Norder);
    
    Image     = zeros(Args.ImageSize, Args.ImageSize);
    PowerSpec = zeros(Args.ImageSize, Args.ImageSize);
    AllImages = zeros(Args.ImageSize, Args.ImageSize, Args.Nimages);
    
    for Iim=1:1:Args.Nimages
        % generate a random realization of the zernike coef.
        [~,J,C]=telescope.Optics.zer_cj_variance(Args.Norder, 'Nrand',1, 'D',Args.D, 'r0',Args.r0);
        % generate an image based on the current realization
        [~,Image1] = telescope.Optics.zerwavefront2image(J,[],C);
        
        Image1 = Image1.*Args.Flux(1);
        
        if Nflux==1
            Image = Image + Image1;
            AllImages(:,:,Iim) = Image1;
        else
            for Iflux=2:1:Nflux
                Image2 = circshift(Image1, Args.PosXY(Iflux-1,2), 1);
                Image2 = circshift(Image2, Args.PosXY(Iflux-1,1), 2);

                Image  = Image + Image2.*Args.Flux(Iflux);
                AllImages(:,:,Iim) = Image2;
            end
        end
        
        if Args.PoissNoise
            Image = poissrnd(Image);
        end
        
        if nargout>1
            Image_NS  = fftshift(Image);
            PowerSpec = PowerSpec + abs(fft2(Image_NS)).^2;                                               
        end
    end
end