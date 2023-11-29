function [Scorr, S, S2, D, Pd, Fd, F_S, D_den, D_num, D_denSqrt, SdeltaN, SdeltaR] = subtractionScorr(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fn, Fr, Args)
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
    %            'VN' - New image variance map including background and sources.
    %                   If VN or VR are empty, then, do not add source
    %                   noise.
    %                   Default is [].
    %            'VR' - Ref image variance map including background and sources.
    %                   If VN or VR are empty, then, do not add source
    %                   noise.
    %                   Default is [].
    %            'SigmaAstN' - The astrometric noise of the new image in [X, Y]
    %                   If one colum is given then assume the noise in X and Y
    %                   direction are identical.
    %                   If several lines are given, then each line corresponds to
    %                   the image index (in the image cube input).
    %                   Default is [].
    %            'SigmaAstR' - The  astrometric noise of the ref image in [X, Y].
    %                   Default is [].
    %
    %            'AbsFun' - absolute value function.
    %                   Default is @(X) abs(X)
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    %            'IsFFT' - A logical indicating if the input N_hat, R_hat, Pn_hat, Pr_hat
    %                   input arguments are in Fourier space.
    %                   Default is true.
    %            'SetToNaN' - A matrix of logical flags which size is equal
    %                   to N and R. Before the normalization: D, S, Scorr,
    %                   will be set to NaN when the flags map equal true.
    %                   If empty, then skip this step.
    %                   Default is [].
    %            'NormS' - A logical indicating if to subtract median and
    %                   divide by RStD, from S, Scorr.
    %                   Default is true.
    %            'NormD' - - A logical indicating if to subtract median and
    %                   divide by RStD, from D.
    %                   Default is false.
    %            'NormDbyFd' - A logical indicating if to divide D by Fd.
    %                   Default is true.
    %            'NormDeltaAsS' - A logical indicating on how to normalize
    %                   SdeltaN and SdeltaR. If true, then will use the
    %                   median and std of S, and if false, will use their
    %                   own median and std. Default is false.
    % Output : - S_corr
    %          - S
    %          - D
    %          - Pd
    %          - Fd
    %          - D_den
    %          - D_num
    %          - D_denSqrt
    %          - SdeltaN
    %          - SdeltaR
    % Author : Eran Ofek (Apr 2022)
    % Example: [Scorr, S, D, Pd, Fd, D_den, D_num, D_denSqrt, SdeltaN, SdeltaR] = imUtil.properSub.subtractionScorr(rand(25,25), rand(25,25), rand(25,25), rand(25,25), 1, 1, 1, 1)
    
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
    
        Args.SetToNaN         = [];
        Args.NormS logical    = true;
        Args.NormD logical    = false;
        Args.NormDbyFd logical = true;
        Args.NormDeltaAsS logical = false;
    end


    [D_hat, Pd_hat, Fd, F_S, D_den, D_num, D_denSqrt, P_deltaNhat, P_deltaRhat] = imUtil.properSub.subtractionD(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fn, Fr,...
                                                                                 'AbsFun',Args.AbsFun, 'Eps',Args.Eps, 'IsFFT',true, 'IsOutFFT',true);
    S_hat       = D_hat.*conj(Pd_hat);
    if nargout>9
        SdeltaN = ifft2(D_hat.*conj(P_deltaNhat));
        SdeltaR = ifft2(D_hat.*conj(P_deltaRhat));
    end

    % convert D and Pd to regular space
    S  = ifft2(S_hat);
    D  = ifft2(D_hat);
    Pd = ifft2(Pd_hat); 
    

    if ~isempty(Args.SetToNaN)
        D(Args.SetToNaN)     = NaN;
        S(Args.SetToNaN)     = NaN;
    end

    if Args.NormS
        MedS = median(S, [1 2], 'omitnan');
        S = S - MedS;
        StdS = tools.math.stat.rstd(S, [1 2]);
        S = S./StdS;

        if Args.NormDeltaAsS
            SdeltaN = SdeltaN - MedS;
            SdeltaN = SdeltaN./StdS;
            SdeltaR = SdeltaR - MedS;
            SdeltaR = SdeltaR./StdS;

        else
            SdeltaN = SdeltaN - median(SdeltaN, [1 2], 'omitnan');
            SdeltaN = SdeltaN./tools.math.stat.rstd(SdeltaN, [1 2]);
            SdeltaR = SdeltaR - median(SdeltaR, [1 2], 'omitnan');
            SdeltaR = SdeltaR./tools.math.stat.rstd(SdeltaR, [1 2]);
        end

    end
    
    [Kn_hat, Kr_hat, Kn, Kr] = imUtil.properSub.knkr(Fn, Fr, Pn_hat, Pr_hat, D_den, Args.AbsFun);
    if isempty(Args.VN) || isempty(Args.VR)
        Vcorr = 0;
    else
        [Vcorr]      = imUtil.properSub.sourceNoise(Args.VN, Args.VR, Kn, Kr);
    end
    
    if isempty(Args.SigmaAstN) || isempty(Args.SigmaAstR)
        Vast = 0;
    else
        [Vast] = imUtil.properSub.astrometricNoise(N_hat, R_hat, Kn_hat, Kr_hat, Args.SigmaAstN, Args.SigmaAstR);
    end
    
    Scorr = S./sqrt(Vcorr + Vast);
    S2 = S.^2;
    
    if ~isempty(Args.SetToNaN)
        Scorr(Args.SetToNaN) = NaN;
    end

    % normalize D
    if Args.NormD
        D = D - median(D, [1 2], 'omitnan');
        D = D./tools.math.stat.rstd(D, [1 2]);
    end
    if Args.NormDbyFd
        D = D./Fd;
    end
    
    % normalize S, S2 and Scorr
    if Args.NormS
        Scorr = Scorr - median(Scorr, [1 2], 'omitnan');
        Scorr = Scorr./tools.math.stat.rstd(Scorr, [1 2]);

        S2 = S2 - median(S2, [1 2], 'omitnan');
        S2 = S2./tools.math.stat.rstd(S2, [1 2]);    
    end
    
end


