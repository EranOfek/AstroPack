function [Vcorr, V_Sn, V_Sr] = sourceNoise(VN, VR, Kn, Kr)
    % Calculate the subtraction source noise varainace based on kr_hat, kn_hat
    %    ZOGY Equations 26-29
    %   The function can deal with cube inputs in which the image index is
    %   in the 3rd dimension.
    % Input  : - (VN) New image variance map including background and sources.
    %          - (VR) Ref image variance map.
    %          - k_n (calculated using imUtil.properSub.knkr).
    %          - k_r (calculated using imUtil.properSub.knkr)
    % Output : - Vcorr Variance for S_corr (source noise for S).
    %          - V_Sn
    %          - V_Sr
    %          - Kn
    %          - Kr
    % Author : Eran Ofek (Apr 2022)
    % Example: [Vcorr, V_Sn, V_Sr] = imUtil.properSub.sourceNoise(rand(25,25), rand(25,25), rand(25,25), rand(25,25));
    %          [Vcorr, V_Sn, V_Sr] = imUtil.properSub.sourceNoise(rand(25,25,4), rand(25,25,4), rand(25,25,4), rand(25,25,4));
    
    arguments
        VN
        VR
        Kn             = [];
        Kr             = [];
    end
    
    % ZOGY Equations 26-29
    %[Kn_hat, Kr_hat, Kn, Kr] = imUtil.properSub.knkr(Fn, Fr, Pn_hat, Pr_hat, D_den, AbsFun);

    Kn_sq_hat = fft2(Kn.^2);
    Kr_sq_hat = fft2(Kr.^2);

    %Kr_sq_hat = fft2(ifft2(Kr_hat).^2);
    %Kn_sq_hat = fft2(ifft2(Kn_hat).^2);
    V_Sr = ifft2(fft2(VR).* Kr_sq_hat);
    V_Sn = ifft2(fft2(VN).* Kn_sq_hat);

    Vcorr     = V_Sn + V_Sr;
    
end
