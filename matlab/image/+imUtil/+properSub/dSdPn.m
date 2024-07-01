function [DSDPn] = dSdPn(N_hat, R_hat, Pn_hat, Pr_hat, VarN, VarR, Args)
    % Calculate the dS/dP_n (Fn=Fr=1) derivative for ZOGY subtraction.
    %   The calculation corresponds to S_hat variations in response to
    %   Pn_hat variations.
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
    %          - VarN (var of new image background).
    %          - VarR (var of ref image background).
    %          * ...,key,val,...
    %            'AbsFun' - absolute value function.
    %                   Default is @(X) abs(X)
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    %            'IsOutFFT' - A logical indicating if the output S, D and Pd
    %                   are ffted (true) or in regular space (false).
    %                   Default is true.
    % Output : - dS/dP_n - Be careful this is unnormalized.
    %            Normalization the same as S is needed.
    % Author : Eran Ofek (2024 Jul) 
    % Example: DS=imUtil.properSub.dSdPn; 

    arguments
        N_hat     = [];
        R_hat     = [];
        Pn_hat    = [];
        Pr_hat    = [];
        VarN      = []
        VarR      = [];
        Args.AbsFun           = @(X) abs(X);
        Args.Eps              = 0;
        %Args.IsFFT logical    = true;
        Args.IsOutFFT logical = true;

    end

    if isempty(N_hat)
        Pr_hat = fft2(imUtil.kernel2.gauss(1.5));
        Pn_hat = fft2(imUtil.kernel2.gauss(2));
        Back   = 100;
        R      = poissrnd(Back.*ones(size(Pr_hat)));
        N      = poissrnd(Back.*ones(size(Pn_hat)));
        R_hat  = fft2(R);
        N_hat  = fft2(N+imUtil.kernel2.gauss(1.5));
        VarN   = Back;
        VarR   = Back;
        Args.IsOutFFT = false;
        [S] = imUtil.properSub.subtractionS(N_hat, R_hat, Pn_hat, Pr_hat, sqrt(VarN), sqrt(VarR), 1, 1, 'IsOutFFT',false);
    end

    AbsPn2 = Args.AbsFun(Pn_hat).^2;
    AbsPr2 = Args.AbsFun(Pr_hat).^2;
    RealPn = real(Pn_hat);

    DSDPn = (AbsPr2.*(N_hat.*VarN.*AbsPr2 + N_hat.*VarR.*AbsPn2 - 2.*N_hat.*VarR.*RealPn.*conj(Pn_hat) - 2.*R_hat.*VarN.*RealPn.*conj(Pr_hat)))./(VarN.*AbsPr2 + VarR.*AbsPn2 + Args.Eps).^2;

    if ~Args.IsOutFFT
        DSDPn = ifft2(DSDPn);
    end

end
