function proper(N, R, Pn, Pr, Args)
    %
   
    arguments
        N
        R
        Pn
        Pr
        Args.IsImFFT(1,1) logical     = true;
        Args.IsPsfFFT(1,1) logical    = true;
        Args.ShiftIm(1,1) logical     = false;
        Args.ShiftPsf(1,1) logical    = false;
        Args.Eps                      = 0;
    end
    
    % convert to fft
    if ~Args.IsImFFT
        N  = fft2(N);
        R  = fft2(R);
    end
    if ~Args.IsPsfFFT
        Pn = fft2(Pn);
        Pr = fft2(Pr);
    end
    if Args.ShiftIm
        N = fftshift(N);
        R = fftshift(R);
    end
    if Args.ShiftPsf
        Pn = fftshift(Pn);
        Pr = fftshift(Pr);
    end
    
    D_num     = Fr.*Pr.*N - Fn.*Pn.*R;
    D_den     = sigmaN.^2 .* Fr.^2 .* abs(Pr).^2 + sigmaR.^2 .*Fn.^2 .* abs(Pn).^2 + Args.Eps;
    D_denSqrt = sqrt(D_den);
    D_hat     = D_num./D_denSqrt;
    
    Fd        = Fr .* Fn ./ sqrt( (sigmaN.*Fr).^2 + (sigmaR.*Fn).^2 );
    
    Pd_num    = Fr .* Fn .* Pr .* Pn;
    Pd_den    = Fd .* D_denSqrt;
    Pd_hat    = Pd_num./Pd_den;
    
    S_hat     = Fd .* D_hat .* conj(Pd_hat);
    
    
    
    
end