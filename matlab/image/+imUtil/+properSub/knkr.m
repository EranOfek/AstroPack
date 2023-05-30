function [Kn_hat, Kr_hat, Kn, Kr]=knkr(Fn, Fr, Pn_hat, Pr_hat, D_den)
    % Calculate the subtraction kr_hat and kn_hat
    %    ZOGY Equations 26-29
    % Input  : - (Fn) Flux normalization of ref.
    %          - (Fr) Flux normalization of new.
    %          - (Pn_hat) fft of ref PSF.
    %          - (Pr_hat) fft of new PSF.
    %          - (D_den) D denominator = (SigmaN.^2 .* Fr.^2) .* AbsFun(Pr_hat).^2 + (SigmaR.^2 .*Fn.^2) .* AbsFun(Pn_hat).^2 + Args.Eps;
    %          - Absolute value function - e.g., @(X) conj(X).*X or @(X) abs(X);
    %            Default is @(x) abs(X)
    % Output : - kn_hat
    %          - kr_hat
    %          - kn
    %          - kr
    % Author : Eran Ofek (May 2023)
    
    arguments
        Fn
        Fr
        Pn_hat
        Pr_hat
        D_den
        AbsFun   = @(x) abs(x);

    end
    
    % ZOGY Equations 26-29
    Kr_hat    = Fr.*Fn.^2.*conj(Pr_hat).*AbsFun(Pn_hat).^2./D_den;
    Kn_hat    = Fn.*Fr.^2.*conj(Pn_hat).*AbsFun(Pr_hat).^2./D_den;
    
    if nargout>2
        Kr = ifft2(Kr_hat);
        Kn = ifft2(Kn_hat);
    end
end