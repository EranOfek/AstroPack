function [DSDFn] = dSdF(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fr, Args)
    % Calculate the dS/dFn (Fr=1) derivative for ZOGY subtraction.
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
    %          - Fr (Ref image flux normalization). Default is 1.
    %          * ...,key,val,...
    %            'AbsFun' - absolute value function.
    %                   Default is @(X) abs(X)
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    %            'IsOutFFT' - A logical indicating if the output S, D and Pd
    %                   are ffted (true) or in regular space (false).
    %                   Default is true.
    % Output : - dS/dFn (for Fr=1).
    % Author : Eran Ofek (2024 Jun) 
    % Example: 

    
    arguments
        N_hat
        R_hat
        Pn_hat
        Pr_hat
        VarN
        VarR
        %Fn = 1;
        Fr = 1;
        Args.AbsFun           = @(X) abs(X);
        Args.Eps              = 0;
        %Args.IsFFT logical    = true;
        Args.IsOutFFT logical = true;
    end

    Pn2 = Args.AbsFun(Pn_hat).^2;
    Pr2 = Args.AbsFun(Pr_hat).^2;
    ConjPr = conj(Pr);

    DSDFn = Pn2 .* (2.*Fr.*VarR .* Pr2 .* conj(Pn) .* N_hat + Pn2.*ConjPr .*(-VarR.*R_hat + VarR.*Fr.^2.*R_hat))./((VarN.*Fr.^2.*Pr2 + VarR.*Pn2 + Args.Eps).^2);

    if ~Args.IsOutFFT
        DSDFn = ifft2(DSDFn);
    end

end
