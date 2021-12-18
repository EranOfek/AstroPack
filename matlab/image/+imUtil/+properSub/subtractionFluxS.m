function F_S = subtractionFluxS(Pn, Pr, SigmaN, SigmaR, Args)
    % Calculate the proper subtraction flux normalization factor (F_S)
    %       Equation (42) in Zackay, Ofek, & Gal-Yam (2016; ApJ 830, 27).
    % Input  : - The PSF of the new image N. The PSF image size must be
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
    %            'Fn' - The new image (N) flux calibration factor.
    %                   Default is 1.
    %            'Fr' - The reference image (R) flux calibration factor.
    %                   Default is 1.
    %            'IsPsfFFT' - A logical indicating if the input Pn and Pr
    %                   PSFs are in Fourier domain. Default is false.
    %            'ShiftPsf' - A logical indicating if to fftshift the input
    %                   Pn and Pr PSFs. Default is false.
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    %            'AbsUsingConj' - A logical indicating how to calculate the
    %                   abs value of a complex matrix.
    %                   If true, use M*conj(M).
    %                   If false, use abs(M).
    %                   Default is false.
    % Output : - (F_S) The scalar by which to divide the subtraction
    %            statistics (S) in order to convert it to units of flux.
    % Author : Eran Ofek (Dec 2021)
    % Example: Size=300;  
    %          Pn = randn(Size,Size); Pr=randn(Size,Size);
    %          [F_S] = imUtil.properSub.subtractionFluxS(Pn, Pr,1,1);
    
    arguments
        Pn
        Pr
        SigmaN
        SigmaR
        Args.Fn                     = 1;
        Args.Fr                     = 1;
        
        Args.IsPsfFFT(1,1) logical  = false;
        Args.ShiftPsf(1,1) logical  = false;
        Args.Eps                    = 0;
        Args.AbsUsingConj logical   = false;
    end
    
    if Args.IsPsfFFT
        Pn_hat = Pn;
        Pr_hat = Pr;
    else
        Pn_hat = fft2(Pn);
        Pr_hat = fft2(Pr);
    end
    if Args.ShiftPsf
        Pn_hat = fftshift(Pn_hat);
        Pr_hat = fftshift(Pr_hat);
    end
    
    if Args.AbsUsingConj
        AbsFun = @(X) conj(X).*X;
    else
        AbsFun = @(X) abs(X);
    end
    
    FnPn2 = Args.Fn.^2.*AbsFun(Pn_hat).^2;
    FrPr2 = Args.Fr.^2.*AbsFun(Pr_hat).^2;
    
    F_S   = sum(FnPn2 .* FrPr2./(SigmaN.^2 .* FrPr2 + SigmaR.^2 .* FnPn2 + Args.Eps),'all');
    
    
end