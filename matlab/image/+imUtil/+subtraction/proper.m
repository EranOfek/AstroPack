function proper(N, R, Pn, Pr, Args)
    %
   
    arguments
        N         % Background subtracted N
        R         % Background subtracted R
        Pn
        Pr
        Args.Fr                       = 1;
        Args.Fn                       = 1;
        
        Args.VN                       = [];
        Args.VR                       = [];
        Args.SigmaAstR                = [0.02, 0.02];
        Args.SigmaAstNx               = [0.02, 0.02];
        
        Args.IsImFFT(1,1) logical     = true;
        Args.IsPsfFFT(1,1) logical    = true;
        Args.ShiftIm(1,1) logical     = false;
        Args.ShiftPsf(1,1) logical    = false;
        Args.Eps                      = 0;
        Args.AbsUsingConj logical     = false;
        
        
    end
    
    if Args.AbsUsingConj
        AbsFun = @(X) conj(X).*X;
    else
        AbsFun = @(X) abs(X);
    end
        
    % convert to fft
    if Args.IsImFFT
        N_hat = N;
        R_hat = R;
    else
        N_hat  = fft2(N);
        R_hat  = fft2(R);
    end
    if Args.IsPsfFFT
        Pn_hat = Pn;
        Pr_hat = Pr;
    else
        Pn_hat = fft2(Pn);
        Pr_hat = fft2(Pr);
    end
    if Args.ShiftIm
        N = fftshift(N);
        R = fftshift(R);
    end
    if Args.ShiftPsf
        Pn = fftshift(Pn);
        Pr = fftshift(Pr);
    end
    
    D_num     = Fr.*Pr_hat.*N_hat - Fn.*Pn_hat.*R_hat;
    D_den     = sigmaN.^2 .* Fr.^2 .* AbsFun(Pr_hat).^2 + sigmaR.^2 .*Fn.^2 .* AbsFun(Pn_hat).^2 + Args.Eps;
    D_denSqrt = sqrt(D_den);
    D_hat     = D_num./D_denSqrt;
    
    Fd        = Fr .* Fn ./ sqrt( (sigmaN.*Fr).^2 + (sigmaR.*Fn).^2 );
    
    Pd_num    = Fr .* Fn .* Pr_hat .* Pn_hat;
    Pd_den    = Fd .* D_denSqrt;
    Pd_hat    = Pd_num./Pd_den;
    
    S_hat     = Fd .* D_hat .* conj(Pd_hat);
   
    % apply source noise 
    Kr_hat    = Fr.*Fn.^2.*cong(Pr_hat).*AbsFun(Pn_hat).^2./D_den;
    Kn_hat    = Fn.*Fr.^2.*conj(Pn_hat).*AbsFun(Pr_hat).^2./D_den;
    V_Sn      = imUtil.filter.conv2_fft(Args.VN, Kn_hat.^2);
    V_Sr      = imUtil.filter.conv2_fft(Args.VR, Kr_hat.^2);
    
    % apply astrometric noise
    Sn        = Kn_hat.*N_hat;
    Sr        = Kr_hat.*R_hat;
    [GradNx, GradNy] = gradient(Sn);
    [GradRx, GradRy] = gradient(Sr);
    Vast_Sn   = Args.SigmaAstN(1).^2 .* GradNx.^2 + Args.SigmaAstN(2).^2 .* GradNy.^2;
    Vast_Sr   = Args.SigmaAstR(1).^2 .* GradRx.^2 + Args.SigmaAstR(2).^2 .* GradRy.^2;
    
    
    
    
    
end