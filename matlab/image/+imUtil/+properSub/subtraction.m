function [D_hat, Pd_hat, S_hat, Scorr] = subtraction(N, R, Pn, Pr, SigmaN, SigmaR, Args)
    % OBSOLOTE - Proper image subtraction between two images (D, Pd, S, S_corr).
    %       Given a new (N) and reference (R) images, along with their
    %       respective PSFs (Pn and Pr), and background noise (SigmaN,
    %       SigmaR), and flux normalizations (Fn, Fr), apply the proper
    %       image subtraction formulae of Zackay, Ofek, & Gal-Yam (2016;
    %       ApJ 830, 27).
    %       Optionaly include the source noise and astrometric noise.
    %       The function returns the uncorelated difference image D, and
    %       its PSF Pd,
    %       the proper subtraction statistics S, and the source noise and
    %       astrometric-noise corrected statistics S_corr.
    % Input  : - The background subtracted new image (N). This can be in
    %            the image domain or fourier domain (i.e., 'IsImFFT'=true).
    %          - Like N but, the background subtracted reference image (R).
    %          - The PSF of the new image N. The PSF image size must be
    %            equal to the N and R image sizes, and the PSF center
    %            should be located at pixel 1,1 (corner).
    %            The input may be in the image domain or Fourier domain
    %            (i.e., 'IsPsfFFT'=true).
    %          - Like Pn, but the PSF for the reference image.
    %          - (SigmaN) the standard deviation of the background new
    %            image.
    %          - (SigmaR) the standard deviation of the background
    %            reference image.
    %          * ...,key,val,...
    %            'Fn' - The new image (N) flux calibration factor.
    %                   Default is 1.
    %            'Fr' - The reference image (R) flux calibration factor.
    %                   Default is 1.
    %            'OutIsFT' - A logical flag indicating if the output is in
    %                   Fourier domain (true), or not (false).
    %                   Default is false.
    %            'VN' - A matrix of variance image of the new (N) including
    %                   the background and sources variance. This is used 
    %                   if S_corr is requested.
    %            'VR' - Like 'VN', but for the reference (R).
    %            'SigmaAstN' - A two element vector of the X and Y
    %                   astrometric uncertanties in the new (N) image. If
    %                   given, and S_corr is requested, then the
    %                   astrometric errors will be propagated into S_corr.
    %                   Default is [].
    %            'SigmaAstR' - Like 'SigmaAstN', but for the reference
    %                   image (R). Default is [].
    %            'IsImFFT' - A logical indicating if the input N and R
    %                   images are in Fourier domain. Default is false.
    %            'IsPsfFFT' - A logical indicating if the input Pn and Pr
    %                   PSFs are in Fourier domain. Default is false.
    %            'ShiftIm' - A logical indicating if to fftshift the input
    %                   N and R images. Default is false.
    %            'ShiftPsf' - A logical indicating if to fftshift the input
    %                   Pn and Pr PSFs. Default is false.
    %            'Eps' - A small value to add to the demoninators in order
    %                   to avoid division by zero due to roundoff errors.
    %                   Default is 0. (If needed set to about 100.*eps).
    %            'AbsUsingConj' - A logical indicating how to calculate the
    %                   abs value of a complex matrix.
    %                   If true, use M*conj(M).
    %                   If false, use abs(M).
    %                   Default is false.
    %            
    %            'AnalyticNorm' - Normalize S analitically. Default is true.
    %            'EmpiricalNorm' - Normalize S and Scorr empirically by
    %                   subtracting the mean and dividing by the std.
    %                   Default is false.
    %            'MeanFun' - Mean function for 'EmpiricalNorm'.
    %                   Default is @tools.math.stat.nanmedian
    %            'StdFun' - Std function for 'EmpiricalNorm'.
    %                   Default is @tools.math.stat.rstd
    % Output : - (D_hat) Proper subtraction difference image in real space.
    %            In Fourier domain if OutIsFFT=true.
    %          - (Pd_hat) The PSF of D.
    %          - (S_hat) The score image (S).
    %          - (Scorr) The corrected score image (S_corr).
    % Author : Eran Ofek (Dec 2021)
    % Example: Size=300;  N = randn(Size,Size); R=randn(Size,Size);
    %          Pn = randn(Size,Size); Pr=randn(Size,Size);
    %          [D, Pd, S, Scorr] = imUtil.properSub.subtraction(N, R, Pn, Pr,1,1);
       
    arguments
        N         % Background subtracted N
        R         % Background subtracted R
        Pn        % must have the same size as N, with PSF in the corner
        Pr        % must have the same size as N, with PSF in the corner
        SigmaN
        SigmaR
        
        Args.Fn                       = 1;
        Args.Fr                       = 1;
        
        Args.OutIsFT logical          = false;
        
        Args.VN                       = [];
        Args.VR                       = [];
        Args.SigmaAstN                = []; %[0.02, 0.02];
        Args.SigmaAstR                = []; %[0.02, 0.02];
        
        Args.IsImFFT(1,1) logical     = false;
        Args.IsPsfFFT(1,1) logical    = false;
        Args.ShiftIm(1,1) logical     = false;
        Args.ShiftPsf(1,1) logical    = false;
        Args.Eps                      = 0;
        Args.AbsUsingConj logical     = false;
        
        Args.AnalyticNorm logical     = true;
        Args.EmpiricalNorm logical    = false;
        Args.MeanFun function_handle  = @tools.math.stat.nanmedian;
        Args.StdFun function_handle   = @tools.math.stat.rstd;

    end
    
    error('need to be modified')
    
    Fr = Args.Fr;
    Fn = Args.Fn;
    
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
        N_hat = fftshift(N_hat);
        R_hat = fftshift(R_hat);
    end
    if Args.ShiftPsf
        Pn_hat = fftshift(Pn_hat);
        Pr_hat = fftshift(Pr_hat);
    end
    
    [D_hat, Pd_hat, Fd, D_den, D_num, D_denSqrt] = imUtil.properSub.subtractionD(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fn, Fr, 'AbsFun',AbsFun, 'Eps',Args.Eps);
    
    % denominator of D
%     D_den     = (SigmaN.^2 .* Fr.^2) .* AbsFun(Pr_hat).^2 + (SigmaR.^2 .*Fn.^2) .* AbsFun(Pn_hat).^2 + Args.Eps;
%     D_num     = Fr.*Pr_hat.*N_hat - Fn.*Pn_hat.*R_hat;
%     D_denSqrt = sqrt(D_den);
%     D_hat     = D_num./D_denSqrt;
%     
%     Fd        = Fr .* Fn ./ sqrt( (SigmaN.*Fr).^2 + (SigmaR.*Fn).^2 );
%     
%     Pd_num    = Fr .* Fn .* Pr_hat .* Pn_hat;
%     Pd_den    = Fd .* D_denSqrt;
%     Pd_hat    = Pd_num./Pd_den;
    
    S_hat     = Fd .* D_hat .* conj(Pd_hat);
   
    if nargout>3 
        if ~isempty(Args.VN) && ~isempty(Args.VR)
            ApplySourceNoise = true;
        else
            ApplySourceNoise = false;
        end
        if ~isempty(Args.SigmaAstN) && ~isempty(Args.SigmaAstR)
            ApplyAstNoise = true;
        else
            ApplyAstNoise = false;
        end
        
        % apply source noise 
        if ApplySourceNoise
            % ZOGY Equations 26-29
            [Kr_hat, Kn_hat, V_Sr, V_Sn, Vcorr] = imUtil.properSub.sourceNoise(Fr, Fn, Pr_hat, Pn_hat, D_den, Args.VN, Args.VR, AbsFun);
            
%             Kr_hat    = Fr.*Fn.^2.*conj(Pr_hat).*AbsFun(Pn_hat).^2./D_den;
%             Kn_hat    = Fn.*Fr.^2.*conj(Pn_hat).*AbsFun(Pr_hat).^2./D_den;
%             V_Sr      = imUtil.filter.conv2_fft(Args.VR, Kr_hat.^2);
%             V_Sn      = imUtil.filter.conv2_fft(Args.VN, Kn_hat.^2);           
%             Vcorr     = V_Sn + V_Sr;
        else
            Vcorr     = 0;
        end
    
        % apply astrometric noise

        if ApplyAstNoise
            [Kr_hat, Kn_hat] = imUtil.properSub.sourceNoise(Fr, Fn, Pr_hat, Pn_hat, D_den, Args.VN, Args.VR, AbsFun); % need to change later
            Sn        = ifft2(Kn_hat.*N_hat);
            Sr        = ifft2(Kr_hat.*R_hat);
            [GradNx, GradNy] = gradient(Sn);
            [GradRx, GradRy] = gradient(Sr);

            Vast_Sn   = Args.SigmaAstN(1).^2 .* GradNx.^2 + Args.SigmaAstN(2).^2 .* GradNy.^2;
            Vast_Sr   = Args.SigmaAstR(1).^2 .* GradRx.^2 + Args.SigmaAstR(2).^2 .* GradRy.^2;
            
            Vcorr     = Vcorr + Vast_Sn + Vast_Sr;
        else
            %Vcorr     = Vcorr + 0;
        end
    
        if ApplySourceNoise || ApplyAstNoise
            % denominator of S_corr
            %Vcorr     = sqrt(Vcorr);

            S         = ifft2(S_hat);
            Scorr     = S./sqrt(Vcorr);

            %Scorr = S./sqrt(Vcorr./median(Vcorr,'all','omitnan'));
        else
            Scorr     = [];
        end
    else
        Scorr = [];
    end

    if ~Args.OutIsFT
        D_hat  = ifft2(D_hat);
        Pd_hat = ifft2(Pd_hat);
        S_hat  = ifft2(S_hat);
        % Scorr is already in real space

        if Args.AnalyticNorm && ~Args.EmpiricalNorm
            Norm = (sqrt(sum(abs(Pd_hat(:)).^2))*Fd);
            S_hat = S_hat/Norm;
%             Scorr = Scorr/Norm;
        

        elseif Args.EmpiricalNorm
            S_hat = S_hat - Args.MeanFun(S_hat,'all');
            Norm = Args.StdFun(S_hat,'all');
            S_hat = S_hat./Norm;

            Scorr = Scorr - Args.MeanFun(Scorr,'all');
            Norm = Args.StdFun(Scorr,'all');
            Scorr = Scorr./Norm;
        end
    else
        % convert Scorr to fft
        Scorr  = fft2(Scorr);
    end
    
end