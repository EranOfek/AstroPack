function [ContSpec, ResidualSpec] = filterLines_WhittakerIRLS(Wave, Flux, Args)
    % Filter spectral lines from 1D spectrum based on iterative Whittaker smoothing + IRLS reweighting
    %   Robust continuum estimation via Whittaker smoothing + IRLS reweighting
    %   to suppress absorption/emission lines (narrow -> very broad).
    % Description:
    %   This function estimates the continuum of a 1D spectrum by fitting a
    %   globally smooth baseline using Whittaker (penalized least-squares)
    %   smoothing. An iterative reweighted least-squares (IRLS) scheme is
    %   used to progressively downweight spectral features that deviate
    %   significantly from the smooth component, thereby suppressing both
    %   absorption and emission lines. The method is robust to narrow and
    %   broad features (provided >~50% of the spectrum represents continuum)
    %   and returns both the continuum estimate and the residual spectrum.
    %
    % Input  : - (Wave) vector (Nx1 or 1xN), wavelength/frequency grid (only used for size checks)
    %          - (Flux) vector (Nx1 or 1xN), observed spectrum
    %          * ...,key,val,...
    %            'Lambda'      - Controls smoothness. Larger => smoother continuum 
    %                            (less line-following).
    %                            Rule of thumb:
    %                            Increase if broad lines are being followed;
    %                            Decrease if continuum curvature is underfit.
    %                            Default is 1e6.
    %            'DiffOrder'   - Order of finite-difference penalty (1,2,3).
    %                            2 is recommended for most continua.
    %                            Default is 2.
    %            'MaxIter'     - Maximum number of IRLS iterations.
    %                            Default is 30.
    %            'TolRel'      - Relative convergence tolerance on continuum change.
    %                            Iteration stops when relative change < TolRel.
    %                            Default is 1e-4.
    %            'Kappa'       - Tukey biweight cutoff in sigma units.
    %                            Smaller => more aggressive rejection of lines.
    %                            Typical range: 3–6.
    %                            Default is 4.5.
    %            'MinSigma'    - Minimum allowed robust sigma to avoid division by zero.
    %                            Default is 1e-12.
    %            'MaskDilate'  - Number of pixels to dilate rejected regions each iteration.
    %                            Helps prevent continuum from cutting through broad wings.
    %                            Default is 0 (no dilation).
    %            'BaseWeights' - Optional vector (Nx1) of prior weights 
    %                            (e.g., 1./Err.^2). Must match Flux size.
    %                            Default is [] (uniform weights).
    %            'UseMedianCenter' - Logical. If true, robust sigma is computed around
    %                            the median residual (more stable for asymmetric spectra).
    %                            Default is true.
    % Output : - Estimated continuum spectrum (same shape as Flux)
    %          - Residual spectrum (Flux - Continuum)
    % Author : Eran Ofek + ChatGPT
    % Example:
    %   [Cont, Res] = imUtil.spec.lines.filterLines_WhittakerIRLS(Wave, Flux, ...
    %       'Lambda',1e7, ...
    %       'Kappa',4, ...
    %       'MaskDilate',10);



    arguments
        Wave (:,1) double
        Flux (:,1) double
    
        Args.Lambda (1,1) double {mustBePositive} = 1000;
        % Lambda controls smoothness. Larger => smoother continuum (less line-following).
        % Rule of thumb:
        %   Increase if broad lines are being followed;
        %   Decrease if continuum curvature is underfit.
    
        Args.DiffOrder (1,1) double {mustBeMember(Args.DiffOrder,[1 2 3])} = 2
        % Typically 2 is best for continua.
    
        Args.MaxIter (1,1) double {mustBeInteger,mustBePositive} = 30
        Args.TolRel (1,1) double {mustBePositive} = 1e-4
    
        Args.Kappa (1,1) double {mustBePositive} = 4.5
        % Tukey biweight cutoff in sigma units. Smaller => more aggressive line rejection.
    
        Args.MinSigma (1,1) double {mustBePositive} = 1e-12
        % Floor for robust sigma to avoid division by zero.
    
        Args.MaskDilate (1,1) double {mustBeInteger,mustBeNonnegative} = 0
        % Optional: dilate (expand) rejected regions by this many pixels each iteration.
        % Helps not to "cut through" wings of broad lines.
    
        Args.BaseWeights (:,1) double = []
        % Optional pre-weights (e.g., 1./Err^2). Must be Nx1 if provided.
    
        Args.UseMedianCenter (1,1) logical = true
        % If true, robust sigma estimated around median residual (more stable with asymmetry).
    end
    
    % ---- Shape / sanity ----
    FluxShape = size(Flux);
    N = numel(Flux);
    
    if numel(Wave) ~= N
        error('Wave and Flux must have the same number of elements.');
    end
    
    Flux = Flux(:);
    Wave = Wave(:);
    
    if ~isempty(Args.BaseWeights)
        BaseW = Args.BaseWeights(:);
        if numel(BaseW) ~= N
            error('Args.BaseWeights must have the same length as Flux.');
        end
    else
        BaseW = ones(N,1);
    end
    
    % Handle NaNs/Infs: treat as missing
    Good = isfinite(Flux) & isfinite(BaseW) & (BaseW > 0);
    if nnz(Good) < max(10, 0.1*N)
        error('Too few finite/valid data points to estimate continuum.');
    end
    
    % Work on good indices only via weights (set bad weights to 0)
    W = zeros(N,1);
    W(Good) = BaseW(Good);
    
    % ---- Build difference operator D (sparse) ----
    D = localDiffMatrix(N, Args.DiffOrder);          % (N-Order) x N
    H = Args.Lambda * (D' * D);                      % NxN sparse, banded
    
    % ---- Initialize ----
    Cont = Flux;
    Cont(~Good) = median(Flux(Good));                % harmless init
    LastCont = Cont;
    
    for Iter = 1:Args.MaxIter
        % Weighted Whittaker solve: (diag(Wi) + Lambda*D'*D) * Cont = diag(Wi) * Flux
        A = spdiags(W, 0, N, N) + H;
        b = W .* Flux;
    
        % Solve sparse SPD system
        Cont = A \ b;
    
        % Residuals
        Res = Flux - Cont;
        Res(~Good) = NaN;
    
        % Robust sigma estimate (MAD)
        if Args.UseMedianCenter
            Center = median(Res(Good), 'omitnan');
        else
            Center = 0.0;
        end
        Sigma = 1.4826 * median(abs(Res(Good) - Center), 'omitnan');
        Sigma = max(Sigma, Args.MinSigma);
    
        % Tukey biweight weights on |residual|
        U = (Res - Center) ./ (Args.Kappa * Sigma);
        NewW = zeros(N,1);
        AbsU = abs(U);
    
        In = Good & (AbsU < 1);
        T = 1 - (U(In)).^2;
        RobustW = (T.^2);                             % Tukey biweight
    
        NewW(In) = BaseW(In) .* RobustW;
    
        % Optional dilation of rejected regions (helps broad wings)
        if Args.MaskDilate > 0
            Rej = Good & (NewW == 0);
            if any(Rej)
                Kernel = ones(2*Args.MaskDilate+1,1);
                Dil = conv(double(Rej), Kernel, 'same') > 0;
                NewW(Dil) = 0;
            end
        end
    
        % Update weights
        W = NewW;
    
        % Convergence check (relative change in continuum on good points)
        Den = max(1e-12, norm(LastCont(Good)));
        RelChange = norm(Cont(Good) - LastCont(Good)) / Den;
        if RelChange < Args.TolRel
            break;
        end
        LastCont = Cont;
    end
    
    ContSpec = reshape(Cont, FluxShape);
    ResidualSpec = reshape(Flux - Cont, FluxShape);
    
    end
    
    
    % --------- helpers ---------
    function D = localDiffMatrix(N, Order)
    % Create sparse finite-difference matrix of given order.
    % Order=1: first difference, size (N-1)xN
    % Order=2: second difference, size (N-2)xN
    % Order=3: third difference, size (N-3)xN
    
    switch Order
        case 1
            % [-1 1]
            D = spdiags([-ones(N,1), ones(N,1)], [0 1], N-1, N);
        case 2
            % [1 -2 1]
            D = spdiags([ones(N,1), -2*ones(N,1), ones(N,1)], [0 1 2], N-2, N);
        case 3
            % [-1 3 -3 1] (third difference)
            D = spdiags([-ones(N,1), 3*ones(N,1), -3*ones(N,1), ones(N,1)], [0 1 2 3], N-3, N);
    end

end