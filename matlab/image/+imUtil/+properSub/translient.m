function [Z2,Zhat,Norm] = translient(N, R, Pn, Pr, SigmaN, SigmaR, Args)
    % Image substruction for detection of point source motion using the 
    %       TRANSLIENT algorithm of Springer et al.(2022). 
    %       The function returns the proper subtraction statistics Z2.
    % Input  : - The background subtracted new image (N). This can be in
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
    %            'Fn' - The new image (N) flux calibration factor.
    %                   Will multiply N.
    %                   Default is 1.
    %            'Fr' - The reference image (R) flux calibration factor.
    %                   Will multiply R.
    %                   Default is 1.  
    %            'IsImFFT' - A logical indicating if the input N and R
    %                   images are in Fourier domain. Default is false.
    %            'IsPsfFFT' - A logical indicating if the input Pn and Pr
    %                   PSFs are in Fourier domain. Default is false.
    %            'ShiftIm' - A logical indicating if to fftshift the input
    %                   N and R images. Default is false.
    %            'ShiftPsf' - A logical indicating if to fftshift the input
    %                   Pn and Pr PSFs. Default is false.
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    %            'NormalizeZ2' - A logical indicating whether to return Z2
    %            after normalization, that is Z2->Z2./Norm
    % Output : - (Z2) The translient statistic.
    %          - (Zhat) The translient Zhat vector. Size (M,M,2) where M is
    %            the image size.
    %          - (Norm) Normalization factor so that Z2/Norm is distributed
    %            as a chi-squared dist. with 2 degrees of freedom. 
    %          
    % Author : Amir Sharon (June 2022)
    % Example: Size=300;  N = randn(Size,Size); R=randn(Size,Size);
    %          Pn = randn(Size,Size); Pr=randn(Size,Size);
    %          [Z2,Zhat,Norm] = imUtil.properSub.translient(N, R, Pn, Pr,1,1);

    arguments
        N         % Background subtracted N
        R         % Background subtracted R
        Pn        % must have the same size as N, with PSF in the corner
        Pr        % must have the same size as N, with PSF in the corner
        SigmaN
        SigmaR

        Args.Fn                            = 1;
        Args.Fr                            = 1;

        Args.IsImFFT(1,1) logical     = false;
        Args.IsPsfFFT(1,1) logical    = false;
        Args.ShiftIm(1,1) logical     = false;
        Args.ShiftPsf(1,1) logical    = false;

        Args.Eps                      = 0;

        Args.NormalizeZ2(1,1) logical = true;
    end


    N = N.*Args.Fn;
    SigmaN = SigmaN/Args.Fn;
    R = R.*Args.Fr;
    SigmaR = SigmaR/Args.Fr;
   
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


    [Z2Prefactors,Norm] = imUtil.properSub.translientAuxiliary(Pnhat, Prhat, ...
        SigmaN, SigmaR, 'IsPsfFFT',true,'Eps',Args.Eps);
    
    Zhat = Z2Prefactors.*(Pnhat.*Rhat - Prhat.*Nhat);

    Z = imag(ifft2(Zhat));

    Z2 = sum(Z.^2,3);

    if Args.NormalizeZ2
        Z2 = Z2./Norm;
        Z2 = Z2 - median(Z2, [1 2], 'omitnan');
        Z2 = Z2./tools.math.stat.rstd(Z2, [1 2]);
    end
         
end