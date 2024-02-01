function [Kn_hat, Kr_hat, Kn, Kr]=knkr(Fn, Fr, Pn_hat, Pr_hat, D_den, AbsFun, Norm)
    % Calculate the subtraction kr_hat and kn_hat
    %    ZOGY Equations 26-29
    %   The function can deal with cube inputs in which the image index is
    %   in the 3rd dimension.
    % Input  : - (Fn) Flux normalization of ref.
    %          - (Fr) Flux normalization of new.
    %          - (Pn_hat) fft of ref PSF.
    %          - (Pr_hat) fft of new PSF.
    %          - (D_den) D denominator fft = (SigmaN.^2 .* Fr.^2) .* AbsFun(Pr_hat).^2 + (SigmaR.^2 .*Fn.^2) .* AbsFun(Pn_hat).^2 + Args.Eps;
    %          - Absolute value function - e.g., @(X) conj(X).*X or @(X) abs(X);
    %            Default is @(x) abs(X)
    %          - Renormalize kn and kr to unity. Default is false.
    % Output : - kn_hat
    %          - kr_hat
    %          - kn
    %          - kr
    % Author : Eran Ofek (May 2023)
    % Example: [Kn_hat, Kr_hat, Kn, Kr]=imUtil.properSub.knkr(1, 1, rand(25,25), rand(25,25), rand(25,25))
    %          [Kn_hat, Kr_hat, Kn, Kr]=imUtil.properSub.knkr(rand(3,1), rand(3,1), rand(25,25,3), rand(25,25,3), rand(25,25,3))

    arguments
        Fn
        Fr
        Pn_hat
        Pr_hat
        D_den
        AbsFun   = @(x) abs(x);

        Norm logical = false;
    end
    
    % ZOGY Equations 26-29
    if ndims(Pn_hat)==3 && ndims(Fr)==2
        % treat cube input
        Fn = reshape(Fn(:),[1 1 numel(Fn)]);
        Fr = reshape(Fr(:),[1 1 numel(Fr)]);
    end

    Kr_hat    = Fr.*Fn.^2.*conj(Pr_hat).*AbsFun(Pn_hat).^2./D_den;
    Kn_hat    = Fn.*Fr.^2.*conj(Pn_hat).*AbsFun(Pr_hat).^2./D_den;
    
    if nargout>2
        Kr = ifft2(Kr_hat);
        Kn = ifft2(Kn_hat);

        if Norm
            Kr = Kr./sum(Kr, [1 2]);
            Kn = Kn./sum(Kn, [1 2]);
        end
    end

    
        
end