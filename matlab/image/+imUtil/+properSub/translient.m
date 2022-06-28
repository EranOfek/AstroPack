function [Z2] = translient(N, R, Pn, Pr, SigmaN, SigmaR, Args)
    % Image substruction for detection of point source motion using the 
    %       TRANSLIENT algorithm of Springer et al.(2022). 
    %       The function returns the proper subtraction statistics Z2.
    % Input  : - The background sybtracted new image (N). This can be in
    %            the image domain or fourier domain (i.e., 'IsImFFT'=true).
    %          - Like N but, the background subtracted reference image (R).
    %          - The PSF of the new image N. The PSF image size must be
    %            equal to the N and R image sizes, and the PSF center
    %            should be located at pixel 1,1 (corner).
    %            The input may be in the image domain or Fourier domain
    %            (i.e., 'IsPsfFFT'=true).
    %          - Like Pn, but the PSF for the reference image.
    %          - (SigmaN) the standard deviation of the background new
    %            image.
    %          - (SigmaR) the standard deviation of the background
    %            reference image.   
    % Output : - (Z2) The translient statistic.
    %          
    %          - (Scorr) The corrected score image (S_corr).
    % Author : Eran Ofek (Dec 2021)
    % Example: Size=300;  N = randn(Size,Size); R=randn(Size,Size);
    %          Pn = randn(Size,Size); Pr=randn(Size,Size);
    %          [D, Pd, S, Scorr] = imUtil.properSub.subtraction(N, R, Pn, Pr,1,1);

    arguments
        N         % Background subtracted N
        R         % Background subtracted R
        Pn        % must have the same size as N, with PSF in the corner
        Pr        % must have the same size as N, with PSF in the corner
        SigmaN
        SigmaR

        Args.IsImFFT(1,1) logical     = false;
        Args.IsPsfFFT(1,1) logical    = false;
        Args.ShiftIm(1,1) logical     = false;
        Args.ShiftPsf(1,1) logical    = false;
    end
   
    if Args.IsImFFT
        Nhat = N;
        Rhat = R;
    else
        Nhat  = fft2(N);
        Rhat  = fft2(R);
    end
    if Args.IsPsfFFT
        Pnhat = Pn;
        Prhat = Pr;
    else
        Pnhat = fft2(Pn);
        Prhat = fft2(Pr);
    end
    if Args.ShiftIm
        Nhat = fftshift(Nhat);
        Rhat = fftshift(Rhat);
    end
    if Args.ShiftPsf
        Pnhat = fftshift(Pnhat);
        Prhat = fftshift(Prhat);
    end

    M     = size(Pnhat,1); % assume sqaure images for now
    Znom = conj(Pnhat).*conj(Prhat).*(Pnhat.*Rhat - Prhat.*Nhat);
    Zden = abs(Prhat).^2 .* SigmaN.^2 + abs(Pnhat).^2 .*SigmaR.^2;
    Zhat = 4*pi/M*Znom./Zden;
    
    [Kx,Ky] = meshgrid(0:(M-1));

    Zx = imag(ifft2(Kx.*Zhat));
    Zy = imag(ifft2(Ky.*Zhat));

    Z2 = Zx.^2 + Zy.^2;
    
    
end