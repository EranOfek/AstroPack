function [Norm, Tran2DCoefs, FitInfo] = fitJointNormTran2D(BaseResiduals, XFULL, YFULL, MagErr, FieldTran2DObj, Args)
    % DEPRECATED (Apr 2026): superseded by CompositeFun.fitMultiStage on a
    % pooled CompositeFun. The refactored PhotCalibTrans.fitJointVisit calls
    % JointCF.fitMultiStage with OptSeqName='LAST_Joint_2Iter', which routes
    % through the existing per-crop JOINT_FC handler at field scope.
    % This standalone primitive is kept for diagnostics / quick LS sanity
    % checks but is not exercised by the production joint pipeline.
    %
    % Joint linear least-squares: ONE global Norm + global Tran2D coefficients.
    % Pure LS primitive — no multi-stage outer loop, no nonlinear stages,
    % no writeback to a PhotCalibTrans array. Caller wires those.
    %
    % Math (magnitude space):
    %   m_obs_i - m_model_base_i  =  NormShift + sum_k coef_k * H_k(XFULL_i, YFULL_i)
    %
    % where m_model_base is the per-crop CompositeFun prediction with
    % Norm=1 and Tran2D ParX=0 (atmospheric + base only), NormShift =
    % -2.5*log10(Norm) is the (single, image-wide) time-dependent throughput
    % shift, and H_k is the kth Tran2D basis function evaluated at the
    % FULL-image coordinate via FieldTran2DObj. There is one Norm for the
    % whole image; spatial variability lives entirely in Tran2D.
    %
    % Degeneracy: the first Tran2D basis function H_1 is identically 1
    % (constant term), so the NormShift column and the k_1 column of the
    % naive design matrix would be identical -> rank-1 deficiency, cond(M)
    % near machine-precision. We instead enforce the gauge condition
    % Tran2D(FieldCenter) = 0 as a hard constraint by DROPPING k_1 from
    % the LS unknowns:
    %
    %   M = [ones(N,1), Hx(:, 2:end)]     size N x Nparam     (well-conditioned)
    %   theta = lscov(M, BaseResiduals, MagErr.^2)
    %   NormShift   = theta(1)
    %   k_2..k_Nparam = theta(2:end)
    %
    % then reconstruct k_1 analytically from the gauge condition:
    %   k_1 = -sum_{i>1} k_i * H_i(FieldCenter)
    %        (computed via FieldTran2DObj.forward at the center with k_1 = 0)
    % which gives Tran2D(FieldCenter) = 0 exactly by construction.
    % Input  : - BaseResiduals  - Nx1 magnitude residuals (m_obs - m_model_base).
    %          - XFULL, YFULL   - Nx1 full-image pixel coords; should be in
    %                             the same frame that FieldTran2DObj's
    %                             ParNX/ParNY normalize against.
    %          - MagErr         - Nx1 per-source MagErr [mag] for weighted LS.
    %                             Pass [] for unweighted LS (M \ b).
    %          - FieldTran2DObj - Tran2D object whose ParNX/ParNY describe
    %                             the FIELD frame (not a single crop). Used
    %                             to build the design matrix via its
    %                             design_matrix(Coo) method. ParX of this
    %                             object will be overwritten on return.
    %          * Args - struct or key/val with:
    %             .FieldCenterX - default FieldTran2DObj.ParNX(1).
    %             .FieldCenterY - default FieldTran2DObj.ParNY(1).
    %             .Verbose      - default false.
    % Output : - Norm        - scalar Norm in linear scale (10^(-NormShift/2.5)).
    %          - Tran2DCoefs - 1 x Nparam row vector of Tran2D ParX
    %                          coefficients after the FieldCenter split.
    %                          Tran2D(FieldCenter) = 0 by construction.
    %          - FitInfo struct with:
    %             .Residuals    - Nx1 post-fit mag residuals.
    %             .RMS          - sqrt(mean(Residuals.^2)).
    %             .Chi2         - sum((Residuals./MagErr).^2) (NaN if no MagErr).
    %             .DOF          - N - (1 + Nparam).
    %             .CondM        - cond(M) - LS conditioning diagnostic (~10
    %                             for typical cheby1_4_xt sampling).
    %             .DeltaField   - the Tran2D(FieldCenter) shift applied
    %                             during the split.
    %             .NormShift    - scalar pre-Norm shift (mag).
    % Author : D. Kovaleva (April 2026)
    % Example: [Norm, T2DCoefs, Info] = PhotCalibTrans.fitJointNormTran2D(...
    %              BaseResid, XFULL, YFULL, MagErr, FieldT2D);
    %          FieldT2D.ParX = T2DCoefs;   % ready for evaluation
    arguments
        BaseResiduals
        XFULL
        YFULL
        MagErr
        FieldTran2DObj
        Args.FieldCenterX = []
        Args.FieldCenterY = []
        Args.Verbose logical = false
    end

    BaseResiduals = BaseResiduals(:);
    XFULL         = XFULL(:);
    YFULL         = YFULL(:);

    N = numel(BaseResiduals);
    assert(numel(XFULL) == N && numel(YFULL) == N, ...
        'PhotCalibTrans:fitJointNormTran2D:LengthMismatch', ...
        'BaseResiduals, XFULL, YFULL must have the same length.');

    % ---- Design matrix: [ones, Hx(:, 2:end)] -----------------------------
    % H_1 (kx0 basis) is identically 1, which would duplicate the Norm
    % column and cause a rank-1 deficiency. We drop it from the LS and
    % reconstruct k_1 analytically from the FieldCenter gauge.
    [Hx, ~] = FieldTran2DObj.design_matrix([XFULL, YFULL]);
    Hx = double(Hx);
    Nparam = size(Hx, 2);
    if Nparam < 1
        error('PhotCalibTrans:fitJointNormTran2D:EmptyBasis', ...
              'FieldTran2DObj returned an empty basis.');
    end

    M = [ones(N, 1), Hx(:, 2:end)];

    % ---- Solve LS --------------------------------------------------------
    if ~isempty(MagErr) && all(MagErr(:) > 0) && numel(MagErr) == N
        W = (MagErr(:)).^2;             % lscov takes variance weights
        theta = lscov(M, BaseResiduals, W);
    else
        theta = M \ BaseResiduals;
    end

    NormShift   = theta(1);                                % scalar
    Tran2DCoefs = zeros(1, Nparam);
    Tran2DCoefs(2:end) = theta(2:end).';                   % k_2..k_Nparam

    % ---- Reconstruct k_1 from Tran2D(FieldCenter) = 0 gauge ---------------
    if isempty(Args.FieldCenterX)
        Args.FieldCenterX = FieldTran2DObj.ParNX(1);
    end
    if isempty(Args.FieldCenterY)
        Args.FieldCenterY = FieldTran2DObj.ParNY(1);
    end

    % Tran2D.forward checks BOTH ParX and ParY are sized correctly via
    % isParKnown; we only fit ParX (the mag-correction polynomial) so make
    % sure ParY has the right length zeros before forward is called.
    [~, NparYExpected] = nfuns(FieldTran2DObj);
    if numel(FieldTran2DObj.ParY) ~= NparYExpected
        FieldTran2DObj.ParY = zeros(1, NparYExpected);
    end

    % With k_1 = 0, evaluate Tran2D at the field centre — that's the
    % contribution of k_2..k_Nparam. k_1 is whatever cancels it.
    FieldTran2DObj.ParX = Tran2DCoefs;                      % k_1 still 0 here
    DeltaOther = FieldTran2DObj.forward([Args.FieldCenterX, Args.FieldCenterY]);
    DeltaOther = DeltaOther(1);
    Tran2DCoefs(1) = -DeltaOther;
    FieldTran2DObj.ParX = Tran2DCoefs;

    DeltaField = 0;     % gauge enforced exactly by construction

    % ---- Norm (linear scale) ---------------------------------------------
    Norm = 10^(-NormShift / 2.5);

    % ---- Diagnostics -----------------------------------------------------
    % Residuals must use the full design matrix (NormShift + all 10 coefs);
    % M only carries 10 columns (we dropped H_1), so compute prediction
    % directly from Norm + Tran2D forward evaluation.
    FullPred = NormShift + Hx * Tran2DCoefs.';
    Residuals = BaseResiduals - FullPred;

    FitInfo = struct();
    FitInfo.Residuals  = Residuals;
    FitInfo.RMS        = sqrt(mean(Residuals.^2));
    if ~isempty(MagErr) && all(MagErr(:) > 0) && numel(MagErr) == N
        FitInfo.Chi2 = sum((Residuals ./ MagErr(:)).^2);
    else
        FitInfo.Chi2 = NaN;
    end
    FitInfo.DOF        = N - (1 + Nparam);
    try
        FitInfo.CondM  = cond(M);
    catch
        FitInfo.CondM  = NaN;
    end
    FitInfo.DeltaField = DeltaField;
    FitInfo.NormShift  = NormShift;

    if Args.Verbose
        fprintf('  Joint LS: N=%d obs, 1 Norm, %d Tran2D coefs, DOF=%d\n', ...
                N, Nparam, FitInfo.DOF);
        fprintf('  cond(M)=%.3g, RMS=%.4f mag, FieldCenter shift=%.4f mag, Norm=%.6g\n', ...
                FitInfo.CondM, FitInfo.RMS, DeltaField, Norm);
    end
end
