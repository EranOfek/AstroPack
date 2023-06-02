function [Vast, Vast_SN, Vast_SR]=astrometricNoise(N_hat, R_hat, Kn_hat, Kr_hat, SigmaAstN, SigmaAstR)
    % Calculate the subtraction astrometric noise varainace based on kr_hat, kn_hat
    %    ZOGY Equations 30-33
    %   The function can deal with cube inputs in which the image index is
    %   in the 3rd dimension.
    % Input  : - (N_hat) The Fourier transform of the new image.
    %            The inputs can be matrices or a cube in which the image
    %            index is in the 3rd dimension.
    %          - (R_hat) The Fourier transform of the ref image.
    %          - (Kn_hat) The Fourier transform of k_n (Eqs. 28-29)
    %          - (Kr_hat) The Fourier transform of k_r (Eqs. 28-29)
    %          - The astrometric noise of the new image in [X, Y]
    %            If one colum is given then assume the noise in X and Y
    %            direction are identical.
    %            If several lines are given, then each line corresponds to
    %            the image index (in the image cube input).
    %          - The  astrometric noise of the ref image in [X, Y].
    % Output : - Vcorr Variance for S_corr (source noise for S).
    %          - V_Sn
    %          - V_Sr
    %          - Kn
    %          - Kr
    % Author : Eran Ofek (May 2023)
    % Example: [Vast, Vast_SN, Vast_SR]=imUtil.properSub.astrometricNoise(N_hat, R_hat, Kn_hat, Kr_hat, SigmaAstN, SigmaAstR)
    %          [Vast, Vast_SN, Vast_SR]=imUtil.properSub.astrometricNoise(rand(25,25), rand(25,25), rand(25,25), rand(25,25),0.1,0.1)
    %          [Vast, Vast_SN, Vast_SR]=imUtil.properSub.astrometricNoise(rand(25,25,2), rand(25,25,2), rand(25,25,2), rand(25,25,2),[0.1 0.1;0.2 0.2],0.1)
    
    arguments
        N_hat
        R_hat
        Kn_hat
        Kr_hat
        SigmaAstN = 0;
        SigmaAstR = 0;
    end
    
    if size(SigmaAstN,2)==1
        SigmaAstNx = SigmaAstN;
        SigmaAstNy = SigmaAstN;
    else
        SigmaAstNx = SigmaAstN(:,1);
        SigmaAstNy = SigmaAstN(:,2);
    end

    if size(SigmaAstR,2)==1
        SigmaAstRx = SigmaAstR;
        SigmaAstRy = SigmaAstR;
    else
        SigmaAstRx = SigmaAstR(:,1);
        SigmaAstRy = SigmaAstR(:,2);
    end

    if ndims(N_hat)==3 && ndims(SigmaAstN)==2
        % treat cube input
        % assume F is given as a vector - move to the 3rd dim:
        SigmaAstNx = reshape(SigmaAstNx,[1 1 numel(SigmaAstNx)]);
        SigmaAstNy = reshape(SigmaAstNy,[1 1 numel(SigmaAstNy)]);
    end
    if ndims(R_hat)==3 && ndims(SigmaAstR)==2
        % treat cube input
        % assume F is given as a vector - move to the 3rd dim:
        SigmaAstRx = reshape(SigmaAstRx,[1 1 numel(SigmaAstRx)]);
        SigmaAstRy = reshape(SigmaAstRy,[1 1 numel(SigmaAstRy)]);
    end



    % ZOGY Equations 26-29
    %[Kn_hat, Kr_hat, Kn, Kr] = imUtil.properSub.knkr(Fn, Fr, Pn_hat, Pr_hat, D_den, AbsFun);

    SN = ifft2(Kn_hat.*N_hat);
    SR = ifft2(Kr_hat.*R_hat);

    % ZOGY Eqs. 30-33:
    [GradNx, GradNy] = gradient(SN);
    [GradRx, GradRy] = gradient(SR);

    Vast_SN   = SigmaAstNx.^2 .* GradNx.^2 + SigmaAstNy.^2 .* GradNy.^2;
    Vast_SR   = SigmaAstRx.^2 .* GradRx.^2 + SigmaAstRy.^2 .* GradRy.^2;
    Vast      = Vast_SN + Vast_SR;
    
end
