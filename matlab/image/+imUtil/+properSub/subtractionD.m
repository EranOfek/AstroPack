function [D_hat, Pd_hat, Fd, F_S, D_den, D_num, D_denSqrt, P_deltaNhat, P_deltaRhat] = subtractionD(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fn, Fr, Args)
    % Return the D_hat subtraction image (Fourier transform of proper subtraction)
    %   The function can deal with cube inputs in which the image index is
    %   in the 3rd dimension.
    % Input  : - N_hat (Fourier Transform of new image).
    %            N, R, Pn, Pr can be either matrices or cubes in which the
    %            image index is in the 3rd dimension (see example).
    %            If you want to provide N instead of N_hat (and R, Pn, Pr),
    %            then set the IsFFT argument to false.
    %          - R_hat (Fourier Transform of ref image).
    %          - Pn_hat (Fourier Transform of new image PSF with size equal
    %            to the new image size).
    %            Prior to FT, the PSF should be in the image corner.
    %          - Pr_hat (like Pn_hat, but for the ref image).
    %          - SigmaN (std of new image background).
    %          - SigmaR (std of ref image background).
    %          - Fn (New image flux normalization).
    %          - Fr(Ref image flux normalization).
    %          * ...,key,val,...
    %            'AbsFun' - absolute value function.
    %                   Default is @(X) abs(X)
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    %            'IsFFT' - A logical indicating if the input N_hat, R_hat, Pn_hat, Pr_hat
    %                   input arguments are in Fourier space.
    %                   Default is true.
    %            'IsOutFFT' - A logical indicating if the output D and Pd
    %                   are ffted (true) or in regular space (false).
    %                   Default is true.
    %            'CleanPd' - A logical indicating if to clean Pd (zero low
    %                   frequencies).
    %                   Default is true.
    % Output : - D_hat
    %          - Pd_hat - Note that Pd is not cleaned.
    %          - Fd
    %          - F_S
    %          - D_den
    %          - D_num
    %          - D_denSqrt
    %          - P_deltaNhat (Eq. 129)
    %          - P_deltaRhat (Eq. 133)
    % Author : Eran Ofek (Apr 2022)
    % Example: [D_hat, Pd_hat, Fd, F_S, D_den, D_num, D_denSqrt] = imUtil.properSub.subtractionD(rand(25,25), rand(25,25), rand(25,25), rand(25,25), 1, 1, 1, 1)
    %          [D_hat, Pd_hat, Fd, F_S, D_den, D_num, D_denSqrt] = imUtil.properSub.subtractionD(rand(25,25,4), rand(25,25,4), rand(25,25,4), rand(25,25,4), ones(4,1), ones(4,1), ones(4,1), ones(4,1))
    
    arguments
        N_hat
        R_hat
        Pn_hat
        Pr_hat
        SigmaN
        SigmaR
        Fn
        Fr
        Args.AbsFun           = @(X) abs(X);
        Args.Eps              = 0;
        Args.IsFFT logical    = true;
        Args.IsOutFFT logical = true;
        Args.CleanPd logical  = true;
    end
        
    if ndims(N_hat)==3 && ndims(Fn)==2
        % treat cube input
        % assume F is given as a vector - move to the 3rd dim:
        Fn = reshape(Fn(:),[1 1 numel(Fn)]);
        Fr = reshape(Fr(:),[1 1 numel(Fr)]);
    end
    if ndims(N_hat)==3 && ndims(SigmaN)==2
        % treat cube input
        % assume SigmaN is given as a vector - move to the 3rd dim:
        SigmaN = reshape(SigmaN(:),[1 1 numel(SigmaN)]);
        SigmaR = reshape(SigmaR(:),[1 1 numel(SigmaR)]);
    end

    if ~Args.IsFFT
        % N_hat, R_hat, Pn_hat, Pr_hat are in real space, calc their FT:
        N_hat = fft2(N_hat);
        R_hat = fft2(R_hat);
        Pn_hat = fft2(Pn_hat);
        Pr_hat = fft2(Pr_hat);
    end


    D_den     = (SigmaN.^2 .* Fr.^2) .* Args.AbsFun(Pr_hat).^2 + (SigmaR.^2 .*Fn.^2) .* Args.AbsFun(Pn_hat).^2 + Args.Eps;
    D_num     = Fr.*Pr_hat.*N_hat - Fn.*Pn_hat.*R_hat;
    D_denSqrt = sqrt(D_den);
    D_hat     = D_num./D_denSqrt;
    
    %F_num     = sqrt( (SigmaN.*Fr).^2 + (SigmaR.*Fn).^2 );
    % faster
    F_num      = hypot(SigmaN.*Fr, SigmaR.*Fn);

    Fd        = Fr .* Fn ./F_num;
    
    Pd_num    = Fr .* Fn .* Pr_hat .* Pn_hat;
    Pd_den    = Fd .* D_denSqrt;
    Pd_hat    = Pd_num./Pd_den;
    
    if nargout>3
        FnPn2 = Fn.^2.*Args.AbsFun(Pn_hat).^2;
        FrPr2 = Fr.^2.*Args.AbsFun(Pr_hat).^2;
        F_S   = sum(FnPn2 .* FrPr2./(SigmaN.^2 .* FrPr2 + SigmaR.^2 .* FnPn2 + Args.Eps),[1 2]);
        F_S   = squeeze(F_S);
    end

    % clean Pd
    % if Args.CleanPd
    %     PdS    = imUtil.psf.full2stamp(ifft2(Pd_hat), 'IsCorner',true, 'StampHalfSize',[7 7]);
    %     PdSS   = imUtil.psf.padShift(PdS,size(D_hat));
    %     PdSS   = fftshift(fftshift(PdSS,1),2);
    %     Pd_hat = fft2(PdSS);
    % end

    if ~Args.IsOutFFT
        % convert D and Pd to regular space
        D_hat  = ifft2(D_hat);
        Pd_hat = ifft2(Pd_hat); 
        Pd     = Pd_hat;
    else
        Pd     = [];
    end

    if nargout>7
        P_deltaNhat = Pr_hat.*F_num./D_denSqrt;
        P_deltaRhat = Pn_hat.*F_num./D_denSqrt;
    end
    
end