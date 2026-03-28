function [VarInt, Mu, ErrVarInt, ErrMu, ExitFlag] = weightedIntrinsicVarMLE(M, Err, Dim)
    % Maximum likelihood estimator for the intrinsic variance of a sample
    %   Given a sample of measurements and their measurement errors,
    %   calculate the maximum likelihood estimate of the intrinsic variance
    %   of the sample, assuming a Gaussian parent distribution and Gaussian
    %   measurement errors.
    %   Also calculate the maximum likelihood errors on the estimated
    %   intrinsic variance and mean.
    %   Ignoring NaNs.
    % Input  : - Measurements.
    %          - Errors.
    %          - Dimension along to calculate the variance. Default is 1.
    % Output : - Intrinsic variance.
    %          - Mean.
    %          - Error on the intrinsic variance.
    %          - Error on the mean.
    %          - Optimizer exit flag.
    % Author : ChatGPT + Eran Ofek (2026 Mar)
    % Example: [VarInt, Mu, ErrVarInt, ErrMu, ExitFlag] = tools.math.stat.weightedIntrinsicVarMLE(M, Err, Dim)

    arguments
        M
        Err
        Dim = 1
    end

    % valid data
    Flag = isfinite(M) & isfinite(Err) & Err > 0;

    % set invalid entries to NaN
    M(~Flag)   = NaN;
    Err(~Flag) = NaN;

    % permute Dim to the first dimension
    Nd   = ndims(M);
    Perm = 1:max(Nd, Dim);
    Perm([1, Dim]) = Perm([Dim, 1]);

    Mp = permute(M, Perm);
    Ep = permute(Err, Perm);

    Sz   = size(Mp);
    N    = Sz(1);
    Ncol = prod(Sz(2:end));

    Mp = reshape(Mp, N, Ncol);
    Ep = reshape(Ep, N, Ncol);

    VarInt    = NaN(1, Ncol);
    Mu        = NaN(1, Ncol);
    ErrVarInt = NaN(1, Ncol);
    ErrMu     = NaN(1, Ncol);
    ExitFlag  = NaN(1, Ncol);

    Opt = optimset('TolX', 1e-10, 'Display', 'off');

    for Icol = 1:Ncol
        Mi = Mp(:, Icol);
        Ei = Ep(:, Icol);

        Fi = isfinite(Mi) & isfinite(Ei) & Ei > 0;
        Mi = Mi(Fi);
        Ei = Ei(Fi);

        if numel(Mi) < 2
            continue;
        end

        Err2 = Ei.^2;

        % initial guess
        Var0   = max(var(Mi, 1) - mean(Err2), 0);
        Theta0 = log(Var0 + 1e-12);

        Fun = @(Theta) negLogLikeTheta(Theta, Mi, Err2);

        [ThetaBest, ~, ExitFlag(Icol)] = fminsearch(Fun, Theta0, Opt);

        VarBest      = exp(ThetaBest);
        V            = VarBest + Err2;
        W            = 1 ./ V;
        MuBest       = sum(W .* Mi) ./ sum(W);

        VarInt(Icol) = VarBest;
        Mu(Icol)     = MuBest;

        % MLE error on Mu from Fisher information
        ErrMu(Icol) = sqrt(1 ./ sum(W));

        % error on VarInt from profile-likelihood curvature
        Step = max(1e-6, 1e-4 .* max(1, abs(ThetaBest)));
        Fm   = Fun(ThetaBest - Step);
        F0   = Fun(ThetaBest);
        Fp   = Fun(ThetaBest + Step);

        D2 = (Fp - 2.*F0 + Fm) ./ (Step.^2);

        if isfinite(D2) && D2 > 0
            % Var(ThetaHat) ~ 1 / D2
            % delta method: dVar/dTheta = exp(Theta) = VarInt
            ErrVarInt(Icol) = VarBest .* sqrt(1 ./ D2);
        end
    end

    % reshape outputs
    OutSz = Sz(2:end);
    if isempty(OutSz)
        OutSz = [1 1];
    elseif isscalar(OutSz)
        OutSz = [OutSz 1];
    end

    VarInt    = reshape(VarInt,    OutSz);
    Mu        = reshape(Mu,        OutSz);
    ErrVarInt = reshape(ErrVarInt, OutSz);
    ErrMu     = reshape(ErrMu,     OutSz);
    ExitFlag  = reshape(ExitFlag,  OutSz);

    % match MATLAB reduction behavior
    if ndims(M)==2
        if Dim==1
            % result should be 1 x size(M,2)
            VarInt    = reshape(VarInt,    1, []);
            Mu        = reshape(Mu,        1, []);
            ErrVarInt = reshape(ErrVarInt, 1, []);
            ErrMu     = reshape(ErrMu,     1, []);
            ExitFlag  = reshape(ExitFlag,  1, []);
        elseif Dim==2
            % result should be size(M,1) x 1
            VarInt    = reshape(VarInt,    [], 1);
            Mu        = reshape(Mu,        [], 1);
            ErrVarInt = reshape(ErrVarInt, [], 1);
            ErrMu     = reshape(ErrMu,     [], 1);
            ExitFlag  = reshape(ExitFlag,  [], 1);
        end
    elseif numel(Sz) > 2
        PermOut = Perm;
        PermOut(1) = [];
        PermOut = PermOut - 1;

        VarInt    = ipermute(VarInt,    PermOut);
        Mu        = ipermute(Mu,        PermOut);
        ErrVarInt = ipermute(ErrVarInt, PermOut);
        ErrMu     = ipermute(ErrMu,     PermOut);
        ExitFlag  = ipermute(ExitFlag,  PermOut);
    end

end


function NLL = negLogLikeTheta(Theta, M, Err2)
    Var = exp(Theta);
    V   = Var + Err2;
    W   = 1 ./ V;
    Mu  = sum(W .* M) ./ sum(W);

    NLL = 0.5 .* sum(log(V) + (M - Mu).^2 ./ V);
end



% function [WVarErrSub,WVar, WErr2, WMean] = weightedVarErrSub(M, Err, Dim)
%     % Weighted variance error subtracted of a sample
%     %   Given a sample of measurments and their measurement errors,
%     %   calculate the natural weighted variance of the sample which is not
%     %   due to the measurement errors.
%     %   Ignoring NaNs.
%     % Input  : - Measurements
%     %          - Errors
%     %          - Dimension along to calculate the variance. Default is 1.
%     % Output : - Weighted Var which is error subtracted.
%     %          - Weighted var.
%     %          - Weighted Err^2.
%     %          - Weighted mean.
%     % Author : Eran Ofek (2026 Mar) 
%     % Example: [WVarErrSub,WVar, WErr2, WMean] = tools.math.stat.weightedVarErrSub(M, Err, Dim)
% 
%     arguments
%         M
%         Err
%         Dim       = 1;
%     end
% 
%     W = 1./Err.^2;
%     WMean = sum(W.*M, Dim, 'omitmissing')./sum(W, Dim, 'omitmissing');
% 
%     SumW  = sum(W, Dim, 'omitmissing');
%     SumW2 = sum(W.^2, Dim, 'omitmissing'); 
% 
%     WVar  = sum(W.*(M - WMean).^2, Dim, 'omitmissing')./(SumW - SumW2./SumW);
%     WErr2 = sum(W.*Err.^2, Dim, 'omitmissing')./SumW;
% 
%     WVarErrSub = WVar - WErr2;
% 
% end
