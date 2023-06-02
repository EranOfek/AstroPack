function [Scorr, S, D, Pd_hat, Fd, D_den, D_num, D_denSqrt] = subtractionScorr(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fn, Fr, Args)
    % Return the S_corr, S, D subtraction images (proper subtraction)
    %   The function can deal with cube inputs in which the image index is
    %   in the 3rd dimension.
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
    %          - SigmaN (std of new image background).
    %          - SigmaR (std of ref image background).
    %          - Fn (New image flux normalization).
    %          - Fr(Ref image flux normalization).
    %          * ...,key,val,...
    %            'AbsFun' - absolute value function.
    %                   Default is @(X) abs(X)
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    %            'IsFFT' - A logical indicating if the input N_hat, R_hat, Pn_hat, Pr_hat
    %                   input arguments are in Fourier space.
    %                   Default is true.
    % Output : - S_corr
    %          - S
    %          - D
    %          - Pd
    %          - Fd
    %          - D_den
    %          - D_num
    %          - D_denSqrt
    % Author : Eran Ofek (Apr 2022)
    % Example: [S_hat, D_hat, Pd_hat, Fd, D_den, D_num, D_denSqrt] = imUtil.properSub.subtractionS(rand(25,25), rand(25,25), rand(25,25), rand(25,25), 1, 1, 1, 1)
    %          [S_hat, D_hat, Pd_hat, Fd, D_den, D_num, D_denSqrt] = imUtil.properSub.subtractionS(rand(25,25,4), rand(25,25,4), rand(25,25,4), rand(25,25,4), ones(4,1), ones(4,1), ones(4,1), ones(4,1))
    
    arguments
        N_hat
        R_hat
        Pn_hat
        Pr_hat
        SigmaN
        SigmaR
        Fn
        Fr
        Args.VN               = [];
        Args.VR               = [];
        Args.SigmaAstN        = [];
        Args.SigmaAstR        = [];
        
        Args.AbsFun           = @(X) abs(X);
        Args.Eps              = 0;
        Args.IsFFT logical    = true;
    end


    [D_hat, Pd_hat, Fd, D_den, D_num, D_denSqrt] = imUtil.properSub.subtractionD(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fn, Fr,...
                                                                                 'AbsFun',Args.AbsFun, 'Eps',Args.Eps, 'IsOutFFT',true);
    S_hat = D_hat.*conj(Pd_hat);
    
    % convert D and Pd to regular space
    S  = ifft2(S_hat);
    D  = ifft2(D_hat);
    Pd = ifft2(Pd_hat); 
    
    
    [Kn_hat, Kr_hat, Kn, Kr] = imUtil.properSub.knkr(Fn, Fr, Pn_hat, Pr_hat, D_den, Args.AbsFun);
    if isempty(Args.VN) || isempty(Args.VR)
        Vcorr = 0;
    else
        [Vcorr]      = imUtil.properSub.sourceNoise(VN, VR, Kn, Kr);
    end
    
    if isempty(Args.SigmaAstN) || isempty(Args.SigmaAstR)
        Vast = 0;
    else
        [Vast] = imUtilproperSub.astrometricNoise(N_hat, R_hat, Kn_hat, Kr_hat, SigmaAstN, SigmaAstR);
    end
    
    Scorr = S./sqrt(Vcorr + Vast);
    
end


