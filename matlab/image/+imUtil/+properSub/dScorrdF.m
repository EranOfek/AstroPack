function [DScorrDFn] = dScorrdF(S, N_hat, R_hat, Pn_hat, Pr_hat, VarN, VarR, D_den_hat, VN, VR, Fr, Args)
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
    % Author : Ruslan Konno (2024 Jun) 
    % Example: 

    arguments
        S
        N_hat
        R_hat
        Pn_hat
        Pr_hat
        VarN
        VarR
        D_den_hat
        VN
        VR
        %Fn = 1;
        Fr = 1;
        Args.AbsFun           = @(X) abs(X);
        Args.Eps              = 0;
    end

    [Kn_hat, Kr_hat, Kn, Kr] = imUtil.properSub.knkr( ...
        1, Fr, Pn_hat, Pr_hat, D_den_hat);
    [Vsrc]      = imUtil.properSub.sourceNoise(VN, VR, Kn, Kr);

    DSDFn = imUtil.properSub.dSdF(N_hat, R_hat, Pn_hat, Pr_hat, VarN, VarR, ...
        Fr, 'IsOutFFT', false);

    Pn2 = Args.AbsFun(Pn_hat).^2;
    Pr2 = Args.AbsFun(Pr_hat).^2;
    ConjPr = conj(Pr_hat);
    ConjPn = conj(Pn_hat);

    DknDFn_num = 2.* Fr.* VarR.* ConjPn.* Pr2.* Pn2;

    DkrDFn_numN = VarR.* Pn2;
    DkrDFn_numR = Fr.^2.* VarN.* Pr2;
    DkrDFn_num = ConjPr.* Pn2.* (DkrDFn_numN-DkrDFn_numR);

    DkDFn_denum = ((VarR.* Pn2 + VarN.* Fr.^2.* Pr2 + Args.Eps).^2);

    DknDFn = DknDFn_num./ DkDFn_denum;
    DkrDFn = DkrDFn_num./ DkDFn_denum;

    dV_Sr = ifft2(2.* fft2(VR).* Kn_hat.* DknDFn);
    dV_Sn = ifft2(2.* fft2(VN).* Kr_hat.* DkrDFn);
    dVsrc = dV_Sn + dV_Sr;

    DScorrDFn = DSDFn./sqrt(Vsrc) - S.*dVsrc./sqrt(Vsrc.^3);

end
