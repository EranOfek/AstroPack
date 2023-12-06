function [Dn_hat, Dr_hat] = subtractionDnDr(N, R, Pn, Pr, SigmaN, SigmaR, Fn, Fr, Args)
    % The partial proper image subtraction between two images.
    %       These are Dn/Dr (Equations 37, 38) in Zackay, Ofek, & Gal-Yam
    %       (2016; ApJ 830, 27).
    %       Dn/Dr are required for estimating the flux correction ratio
    %       (beta= Fn/Fr).
    %   The function can deal with cube inputs in which the image index is
    %   in the 3rd dimension.
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
    %          - (SigmaR) the standar d deviation of the background
    %            reference image.
    %          - (Fn) (New image flux normalization). Default is 1.
    %          - (Fr) (Ref image flux normalization). Default is 1.
    %          * ...,key,val,...
    %            'Beta' - The flux ratio Fn/Fr. Default is 1.
    %            'OutIsFT' - A logical flag indicating if the output is in
    %                   Fourier domain (true), or not (false).
    %                   Default is false.
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
    %            'AbsUsingConj' - A logical indicating how to calculate the
    %                   abs value of a complex matrix.
    %                   If true, use M*conj(M).
    %                   If false, use abs(M).
    %                   Default is false.
    % Output : - Dn (in real space, unless OutIsFT=true).
    %          - Dr.
    % Author : Eran Ofek (Dec 2021)
    % Example: Size=300;  N = randn(Size,Size); R=randn(Size,Size);
    %          Pn = randn(Size,Size); Pr=randn(Size,Size);
    %          [Dn,Dr] = imUtil.properSub.subtractionDnDr(N, R, Pn, Pr,1,1);
    %          [Dn,Dr] = imUtil.properSub.subtractionDnDr(rand(25,25), rand(25,25), rand(25,25), rand(25,25),1,1);
    %          [Dn,Dr] = imUtil.properSub.subtractionDnDr(rand(25,25,4), rand(25,25,4), rand(25,25,4), rand(25,25,4),ones(4,1),ones(4,1));
       
    arguments
        N         % Background subtracted N
        R         % Background subtracted R
        Pn        % must have the same size as N, with PSF in the corner
        Pr        % must have the same size as N, with PSF in the corner
        SigmaN
        SigmaR
        Fn          = 1;
        Fr          = 1;
        
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
        
    if ndims(N)==3 && ndims(Fn)==2
        % treat cube input
        % assume F is given as a vector - move to the 3rd dim:
        Fn = reshape(Fn(:),[1 1 numel(Fn)]);
        Fr = reshape(Fr(:),[1 1 numel(Fr)]);
    end
    if ndims(N)==3 && ndims(SigmaN)==2
        % treat cube input
        % assume SigmaN is given as a vector - move to the 3rd dim:
        SigmaN = reshape(SigmaN(:),[1 1 numel(SigmaN)]);
        SigmaR = reshape(SigmaR(:),[1 1 numel(SigmaR)]);
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