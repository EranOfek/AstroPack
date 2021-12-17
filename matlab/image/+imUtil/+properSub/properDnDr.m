function [Dn_hat, Dr_hat] = proper(N, R, Pn, Pr, SigmaN, SigmaR, Args)
    % Proper image subtraction between two images.
    %       Given a new (N) and reference (R) images, along with their
    %       respective PSFs (Pn and Pr), and background noise (SigmaN,
    %       SigmaR), and flux normalizations (Fn, Fr), apply the proper
    %       image subtraction formulae of Zackay, Ofek, & Gal-Yam (2016;
    %       ApJ 830, 27).
    %       Optionaly include the source noise and astrometric noise.
    %       The function returns the uncorelated difference image D, and
    %       its PSF Pd,
    %       the proper subtraction statistics S, and the source noise and
    %       astrometric-noise corrected statistics S_corr.
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
    %          * ...,key,val,...
    %            'Beta'
    %            'OutIsFT'
    %
    %
    % Example: Size=300;  N = randn(Size,Size); R=randn(Size,Size);
    %          Pn = randn(Size,Size); Pr=randn(Size,Size);
    %          [D, Pd, S, Scorr] = imUtil.subtraction.proper(N, R, Pn, Pr,1,1);
    
    
   
    arguments
        N         % Background subtracted N
        R         % Background subtracted R
        Pn        % must have the same size as N, with PSF in the corner
        Pr        % must have the same size as N, with PSF in the corner
        SigmaN
        SigmaR
        
        Args.Beta                     = 1;
        
        Args.OutIsFT logical          = false;
        
        Args.IsImFFT(1,1) logical     = true;
        Args.IsPsfFFT(1,1) logical    = true;
        Args.ShiftIm(1,1) logical     = false;
        Args.ShiftPsf(1,1) logical    = false;
        Args.Eps                      = 0;
        Args.AbsUsingConj logical     = false;
        
        
    end
    
    if Args.AbsUsingConj
        AbsFun = @(X) conj(X).*X;
    else
        AbsFun = @(X) abs(X);
    end
        
    % convert to fft
    if Args.IsImFFT
        N_hat = N;
        R_hat = R;
    else
        N_hat  = fft2(N);
        R_hat  = fft2(R);
    end
    if Args.IsPsfFFT
        Pn_hat = Pn;
        Pr_hat = Pr;
    else
        Pn_hat = fft2(Pn);
        Pr_hat = fft2(Pr);
    end
    if Args.ShiftIm
        N = fftshift(N);
        R = fftshift(R);
    end
    if Args.ShiftPsf
        Pn = fftshift(Pn);
        Pr = fftshift(Pr);
    end
    
    % denominator of D
    
    
    D_den_1   = SigmaN.^2 .* AbsFun(Pr_hat).^2;
    D_den_2   = SigmaR.^2 .* AbsFun(Pn_hat).^2;
    D_den     = Fr.^2 .* D_den_1 + Fn.^2 .* D_den_2 + Args.Eps;
        
    % calculate the numerators of Dn and Dr
    Dn_hat  = Pr_hat.*N_hat;  % numerator of Dn
    Dr_hat  = Pn_hat.*R_hat;  % numerator of Dr
    D_num   = Fr.*Dn_hat - Fn.*Dr_hat;
    Dnr_den = sqrt(D_den_1 + Args.Beta.^2 .* D_den_2 + Args.Eps);
    Dn_hat  = Dn_hat./Dnr_den;
    Dr_hat  = Dr_hat./Dnr_den;
   
    if Args.OutIsFT
        % do nothing - already in Fourier domain
    else
        Dn_hat = ifft2(Dn_hat);
        Dr_hat = ifft2(Dr_hat);
    end
    
end