function F_S = subtractionFluxS(Pn, Pr, SigmaN, SigmaR, Fn, Fr, Args)
    % Calculate the proper subtraction flux normalization factor (F_S)
    %       F_S is used to convert the S subtraction statistics into flux
    %       normalized image on which photometry can be done.
    %       Equation (42) in Zackay, Ofek, & Gal-Yam (2016; ApJ 830, 27).
    %   The function can deal with cube inputs in which the image index is
    %   in the 3rd dimension.
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
    %          - (Fn) (New image flux normalization). Default is 1.
    %          - (Fr) (Ref image flux normalization). Default is 1.
    %          * ...,key,val,...
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
    %          [F_S] = imUtil.properSub.subtractionFluxS(rand(25,25), rand(25,25),1,1);
    %          [F_S] = imUtil.properSub.subtractionFluxS(rand(25,25,3), rand(25,25,3),ones(3,1),ones(3,1));
    
    arguments
        Pn
        Pr
        SigmaN
        SigmaR
        Fn                     = 1;
        Fr                     = 1;
        
        Args.IsPsfFFT(1,1) logical  = false;
        Args.ShiftPsf(1,1) logical  = false;
        Args.Eps                    = 0;
        Args.AbsUsingConj logical   = false;
    end
    
    if ndims(Pn)==3 && ndims(Fn)==2
        % treat cube input
        % assume F is given as a vector - move to the 3rd dim:
        Fn = reshape(Fn(:),[1 1 numel(Fn)]);
        Fr = reshape(Fr(:),[1 1 numel(Fr)]);
    end
    if ndims(Pn)==3 && ndims(SigmaN)==2
        % treat cube input
        % assume SigmaN is given as a vector - move to the 3rd dim:
        SigmaN = reshape(SigmaN(:),[1 1 numel(SigmaN)]);
        SigmaR = reshape(SigmaR(:),[1 1 numel(SigmaR)]);
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
    
    FnPn2 = Fn.^2.*AbsFun(Pn_hat).^2;
    FrPr2 = Fr.^2.*AbsFun(Pr_hat).^2;
    
    F_S   = sum(FnPn2 .* FrPr2./(SigmaN.^2 .* FrPr2 + SigmaR.^2 .* FnPn2 + Args.Eps),[1 2]);
    F_S   = squeeze(F_S);
    
    
end