function [Kr_hat, Kn_hat, V_Sr, V_Sn, Vcorr] = sourceNoise(Fr, Fn, Pr_hat, Pn_hat, D_den, VN, VR, AbsFun)
    % Calculate the subtraction source noise varainace and kr_hat, kn_hat
    %    ZOGY Equations 26-29
    % Input  : - (Fr) Flux normalization of ref.
    %          - (Fn) Flux normalization of new.
    %          - (Pr_hat) fft of ref PSF.
    %          - (Pn_hat) fft of new PSF.
    %          - (D_den) D denominator = (SigmaN.^2 .* Fr.^2) .* AbsFun(Pr_hat).^2 + (SigmaR.^2 .*Fn.^2) .* AbsFun(Pn_hat).^2 + Args.Eps;
    %          - (VN) new variance.
    %          - (VR) ref variance.
    %          - Absolute value function - e.g., @(X) conj(X).*X or @(X) abs(X);
    % Output : - kr_hat
    %          - kn_hat
    %          - V_Sr
    %          - V_Sn
    %          - Vcorr Variance for S_corr (source noise corrected S).
    % Author : Eran Ofek (Apr 2022)
    
    
    % ZOGY Equations 26-29
    Kr_hat    = Fr.*Fn.^2.*conj(Pr_hat).*AbsFun(Pn_hat).^2./D_den;
    Kn_hat    = Fn.*Fr.^2.*conj(Pn_hat).*AbsFun(Pr_hat).^2./D_den;
    
    if nargout>2
        V_Sr      = imUtil.filter.conv2_fft(VR, Kr_hat.^2);
        V_Sn      = imUtil.filter.conv2_fft(VN, Kn_hat.^2);

        if nargout>4
            Vcorr     = V_Sn + V_Sr;
        end
    end
end
