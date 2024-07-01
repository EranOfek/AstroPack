function [DSDFn] = dSdF(N_hat, R_hat, Pn_hat, Pr_hat, VarN, VarR, Fr, Args)
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
    % Output : - dS/dFn (for Fr=1). Be careful this is unnormalized.
    %            Normalization the same as S is needed.
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
    ConjPr = conj(Pr_hat);
    ConjPn = conj(Pn_hat);

    DSDFn_numN = 2.* Fr.* VarR .* ConjPn .* Pr2 .* N_hat;
    DSDFn_numR = - ConjPr .*(VarR.* Pn2 - VarN.* Fr.^2.* Pr2).*R_hat;
    DSDFn_num = DSDFn_numN + DSDFn_numR;

    DSDFn_denum = ((VarR.* Pn2 + VarN.* Fr.^2.* Pr2 + Args.Eps).^2);

    DSDFn = Pn2 .* DSDFn_num ./ DSDFn_denum;

    if ~Args.IsOutFFT
        DSDFn = ifft2(DSDFn);
    end

end
