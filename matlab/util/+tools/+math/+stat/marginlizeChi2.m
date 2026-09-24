function [MatMargChi2, RemainDim] = marginlizeChi2(MatChi2, MargDim, Args)
    % Marginalize an N-D chi-square grid over selected dimensions, including priors.
    %   This function marginalizes an N-dimensional chi-square array over the
    %   dimensions listed in MargDim by converting chi-square to likelihood,
    %   integrating/summing the likelihood over the requested dimensions, and
    %   converting the result back to a chi-square-like quantity:
    %
    %       L \propto exp(-Chi2/2)
    %       L_marg = \int L d(phi)
    %       Chi2_marg = -2*log(L_marg) + C
    %
    %   The additive constant C is arbitrary. By default, the output is shifted
    %   such that its minimum is zero.
    %
    % Input : - (MatChi2) N-D array containing chi-square values.
    %           Must be real. Integer types are converted to double.
    %           NaN and -Inf are not allowed (see Notes). +Inf is allowed and
    %           is interpreted as zero likelihood (excluded grid point).
    %         - (MargDim) Vector of dimensions over which to marginalize.
    %           Must contain at least 1 dimension and at most N-1 dimensions.
    %         * ...,key,val,...
    %           'Method' - Integration method over marginalized dimensions:
    %                        'sum'   : simple summation (default)
    %                        'trapz' : trapezoidal integration
    %                        Default: 'sum'
    %
    %                        IMPORTANT - 'sum' is NOT equivalent to 'trapz'
    %                        in general. Summation approximates
    %                            \int L d(phi)
    %                        only up to a constant factor, and only when the
    %                        grid spacing along the marginalized dimension is
    %                        UNIFORM. On a uniform grid the two methods differ
    %                        by the constant prod_i Delta_i, which is removed
    %                        anyway by Normalize='min0', so the results are
    %                        identical up to the arbitrary constant C.
    %
    %                        On a NON-UNIFORM grid (e.g. log-spaced, or a grid
    %                        refined near the minimum), summation implicitly
    %                        weights each node equally, which is equivalent to
    %                        imposing an unintended prior proportional to
    %                        1/Delta(phi) on the marginalized parameter. This
    %                        produces a silently biased marginal. For any
    %                        non-uniform grid use Method='trapz' together with
    %                        'Grid', or supply the spacing explicitly through
    %                        'Prior'.
    %
    %                        Jacobians are the caller's responsibility. If a
    %                        grid axis is parameterized in a transformed
    %                        variable (e.g. the grid is in log10(A) rather than
    %                        in A), then integrating over that axis computes
    %                        \int L d(log10 A), which corresponds to a prior
    %                        flat in log10(A). To marginalize with a prior flat
    %                        in A instead, fold the Jacobian
    %                        dA/d(log10 A) = A*ln(10) into 'Prior'.
    %           'Grid' - Cell array of grid vectors for each dimension.
    %                        Used only when Args.Method='trapz'.
    %                        Args.Grid{I} should contain the coordinate vector
    %                        for dimension I. For dimensions not marginalized,
    %                        the corresponding cell may be empty.
    %                        Each grid vector used must be real, finite and
    %                        strictly increasing.
    %                        Default: {}
    %           'Prior' - Prior specification. One of:
    %                        (a) positive finite scalar
    %                        (b) array, same size as MatChi2
    %                        (c) cell array of prior vectors, where Prior{I}
    %                            is a vector of length size(MatChi2,I) for
    %                            dimension I. Empty cells are treated as 1.
    %
    %                        In case (c), the total prior is assumed separable:
    %                            PriorTotal = prod_i Prior{i}(x_i)
    %
    %                        In cases (b) and (c) the values must be finite,
    %                        non-negative, and not identically zero. Zeros are
    %                        allowed and act as hard exclusions (they yield
    %                        Chi2_marg = Inf for slices that are entirely
    %                        excluded).
    %
    %                        The marginalized likelihood is computed from:
    %                            exp(-MatChi2/2) .* PriorTotal
    %                        Default: 1
    %           'Normalize' - Normalization of the output marginalized chi-square:
    %                        'min0'  : subtract minimum so min(MatMargChi2)=0
    %                        'none'  : do not shift
    %                        Default: 'min0'
    %
    % Output : - Marginalized chi-square array. Its dimensions correspond
    %            to the dimensions not listed in MargDim, in their original
    %            order. Entries whose marginal likelihood is exactly zero
    %            (excluded by the prior, or underflowed) are returned as +Inf.
    %          - Row vector containing the indices of the remaining
    %            dimensions, in the same order as in MatMargChi2.
    %
    % Notes  : * Dynamic range. The computation is stabilized by subtracting
    %            the global maximum of log(L) before exponentiating, so the
    %            peak is always representable. However, regions lying more
    %            than about 1420 in chi-square below the peak underflow to
    %            zero in double precision and are returned as +Inf rather
    %            than being silently clamped to a finite plateau. A warning
    %            (marginlizeChi2:underflow) is issued when this happens.
    %          * NaN. NaN in MatChi2 or in the prior is rejected up front
    %            rather than being propagated, because a single NaN would
    %            otherwise contaminate an entire marginalized slice.
    %          * Singleton dimensions. A marginalized dimension of length 1
    %            carries no integration measure; it is dropped from the output
    %            without any reduction (trapz over a single node would return
    %            zero). A warning (marginlizeChi2:singletonMargDim) is issued.
    %
    % Author : (original) ; hardened version
    % Example: Chi2 = randn(20,30,10).^2;
    %          [C2, RD] = marginlizeChi2(Chi2, 3);
    %          [C2, RD] = marginlizeChi2(Chi2, [3 1], 'Method','trapz',...
    %                                    'Grid',{1:20, 1:30, linspace(0,1,10)});

    arguments
        MatChi2 {mustBeNumeric, mustBeNonempty, mustBeReal}
        MargDim (1,:) {mustBeInteger, mustBePositive}
        Args.Method (1,1) string {mustBeMember(Args.Method, ["sum","trapz"])} = "sum"
        Args.Grid cell = {}
        Args.Prior = 1
        Args.Normalize (1,1) string {mustBeMember(Args.Normalize, ["min0","none"])} = "min0"
    end

    %--------------------------------------------------------------------------
    % Input class: promote integer/logical input to double.
    % (Integer arithmetic would round -0.5*MatChi2 and exp() is undefined for
    %  integer types.)
    %--------------------------------------------------------------------------
    if ~isfloat(MatChi2)
        MatChi2 = double(MatChi2);
    end

    %--------------------------------------------------------------------------
    % Basic checks
    %--------------------------------------------------------------------------
    Ndim     = ndims(MatChi2);
    SizeChi2 = size(MatChi2);

    MargDim = unique(MargDim(:).', 'stable');

    if any(MargDim > Ndim)
        error('marginlizeChi2:badMargDim',...
              ['MargDim contains a dimension index larger than ndims(MatChi2)=%d. ',...
               'Note that MATLAB drops trailing singleton dimensions, so a grid ',...
               'of size [N M 1] reports ndims=2.'], Ndim);
    end

    if numel(MargDim) < 1
        error('marginlizeChi2:badMargDim','MargDim must contain at least one dimension.');
    end

    if numel(MargDim) > Ndim - 1
        error('marginlizeChi2:badMargDim','MargDim must contain at most Ndim-1 dimensions.');
    end

    RemainDim = setdiff(1:Ndim, MargDim, 'stable');

    %--------------------------------------------------------------------------
    % Validate chi-square values.
    % NaN is rejected: sum/trapz propagate NaN, and a single NaN would turn an
    % entire marginalized slice into a plausible-looking finite chi-square.
    % -Inf is rejected: it corresponds to infinite likelihood.
    %--------------------------------------------------------------------------
    if any(isnan(MatChi2(:)))
        error('marginlizeChi2:nanChi2',...
              ['MatChi2 contains NaN. Remove or replace NaN entries before ',...
               'marginalizing (e.g. set unconstrained grid points to +Inf, ',...
               'which corresponds to zero likelihood).']);
    end

    if any(MatChi2(:) == -Inf)
        error('marginlizeChi2:negInfChi2','MatChi2 contains -Inf (infinite likelihood).');
    end

    if all(isinf(MatChi2(:)))
        error('marginlizeChi2:allInfChi2',...
              'All entries of MatChi2 are +Inf; the likelihood is identically zero.');
    end

    %--------------------------------------------------------------------------
    % Validate prior
    % NOTE: iscell is tested FIRST, because isscalar({v}) is true for a
    % one-element cell array and would otherwise fall into the scalar branch.
    %--------------------------------------------------------------------------
    PriorType = "";

    if iscell(Args.Prior)
        PriorType = "cell";

        if numel(Args.Prior) < Ndim
            error('marginlizeChi2:badPrior',...
                  'If Args.Prior is a cell array, it must contain at least ndims(MatChi2)=%d cells.', Ndim);
        end

        AllPriorZero = true;
        for I = 1:Ndim
            if isempty(Args.Prior{I})
                AllPriorZero = false;   % empty cell is treated as 1
                continue;
            end

            Pi = Args.Prior{I};

            if ~(isnumeric(Pi) || islogical(Pi)) || ~isvector(Pi)
                error('marginlizeChi2:badPrior','Args.Prior{%d} must be a numeric vector or empty.', I);
            end

            Pi = double(Pi);

            if numel(Pi) ~= SizeChi2(I)
                error('marginlizeChi2:badPrior',...
                      'Length of Args.Prior{%d} must match size(MatChi2,%d)=%d.', I, I, SizeChi2(I));
            end

            if ~isreal(Pi) || any(isnan(Pi(:)))
                error('marginlizeChi2:badPrior','Args.Prior{%d} must be real and free of NaN.', I);
            end

            if any(isinf(Pi(:)))
                error('marginlizeChi2:badPrior','Args.Prior{%d} must be finite.', I);
            end

            if any(Pi(:) < 0)
                error('marginlizeChi2:badPrior','Args.Prior{%d} must contain non-negative values.', I);
            end

            if ~any(Pi(:) > 0)
                error('marginlizeChi2:zeroPrior',...
                      'Args.Prior{%d} is identically zero; the posterior would be undefined.', I);
            end

            Args.Prior{I} = Pi;
            AllPriorZero  = false;
        end

        if AllPriorZero
            % all cells empty -> prior is 1 everywhere; nothing to do
        end

    elseif isnumeric(Args.Prior) || islogical(Args.Prior)

        Args.Prior = double(Args.Prior);

        if isscalar(Args.Prior)
            PriorType = "scalar";
            if ~isfinite(Args.Prior) || Args.Prior <= 0
                error('marginlizeChi2:badPrior','Scalar Args.Prior must be positive and finite.');
            end

        else
            PriorType = "array";
            if ~isequal(size(Args.Prior), SizeChi2)
                error('marginlizeChi2:badPrior',...
                      ['Args.Prior must be either a scalar, a cell array, or an array ',...
                       'with the same size as MatChi2.']);
            end
            if any(isnan(Args.Prior(:)))
                error('marginlizeChi2:badPrior','Numeric array Args.Prior must be free of NaN.');
            end
            if any(isinf(Args.Prior(:)))
                error('marginlizeChi2:badPrior','Numeric array Args.Prior must be finite.');
            end
            if any(Args.Prior(:) < 0)
                error('marginlizeChi2:badPrior','Numeric array Args.Prior must be non-negative.');
            end
            if ~any(Args.Prior(:) > 0)
                error('marginlizeChi2:zeroPrior',...
                      'Args.Prior is identically zero; the posterior would be undefined.');
            end
        end

    else
        error('marginlizeChi2:badPrior',...
              'Args.Prior must be either a scalar, a numeric array, or a cell array of prior vectors.');
    end

    %--------------------------------------------------------------------------
    % Identify singleton marginalized dimensions.
    % A dimension of length 1 has no integration measure: trapz over a single
    % node returns exactly 0, which would silently zero the whole result.
    % Such dimensions are simply dropped (equivalent to multiplying by 1).
    %--------------------------------------------------------------------------
    SizeMarg      = SizeChi2(MargDim);
    IsSingleton   = (SizeMarg == 1);
    EffectiveMarg = MargDim(~IsSingleton);

    if any(IsSingleton)
        warning('marginlizeChi2:singletonMargDim',...
                ['Marginalized dimension(s) [%s] have length 1 and carry no integration ',...
                 'measure. They are dropped from the output without integration.'],...
                num2str(MargDim(IsSingleton)));
    end

    %--------------------------------------------------------------------------
    % Validate grid if needed
    %--------------------------------------------------------------------------
    if Args.Method == "trapz"
        if isempty(Args.Grid)
            error('marginlizeChi2:badGrid','Args.Grid must be provided when Args.Method is ''trapz''.');
        end

        if numel(Args.Grid) < Ndim
            error('marginlizeChi2:badGrid',...
                  'Args.Grid must contain at least ndims(MatChi2)=%d cells.', Ndim);
        end

        for I = EffectiveMarg
            Xi = Args.Grid{I};

            if isempty(Xi)
                error('marginlizeChi2:badGrid',...
                      'Args.Grid{%d} must be provided for all marginalized dimensions when using ''trapz''.', I);
            end

            if ~isnumeric(Xi) || ~isvector(Xi)
                error('marginlizeChi2:badGrid','Args.Grid{%d} must be a numeric vector.', I);
            end

            Xi = double(Xi(:).');

            if numel(Xi) ~= SizeChi2(I)
                error('marginlizeChi2:badGrid',...
                      'Length of Args.Grid{%d} must match size(MatChi2,%d)=%d.', I, I, SizeChi2(I));
            end

            if ~isreal(Xi) || any(~isfinite(Xi))
                error('marginlizeChi2:badGrid','Args.Grid{%d} must be real and finite.', I);
            end

            % A descending or non-monotonic grid makes trapz return a negative
            % or meaningless "integral", which would then be exponentiated back
            % into a plausible-looking chi-square.
            if any(diff(Xi) <= 0)
                error('marginlizeChi2:badGrid',...
                      ['Args.Grid{%d} must be strictly increasing. Sort the grid (and the ',...
                       'corresponding slices of MatChi2) before calling.'], I);
            end

            Args.Grid{I} = Xi;
        end
    end

    %--------------------------------------------------------------------------
    % Convert chi-square to relative likelihood (log-sum-exp stabilization)
    %--------------------------------------------------------------------------
    LogL    = -0.5 .* MatChi2;
    MaxLogL = max(LogL(:));         % finite: NaN/-Inf rejected, not all +Inf
    MatLike = exp(LogL - MaxLogL);  % Chi2=+Inf -> exactly 0

    %--------------------------------------------------------------------------
    % Apply prior
    %--------------------------------------------------------------------------
    switch PriorType
        case {"scalar","array"}
            MatLike = MatLike .* Args.Prior;

        case "cell"
            for I = 1:Ndim
                Pi = Args.Prior{I};

                if isempty(Pi)
                    continue;
                end

                % reshape prior vector so it broadcasts only along dimension I
                Shape    = ones(1, Ndim);
                Shape(I) = SizeChi2(I);

                Pi      = reshape(Pi, Shape);
                MatLike = MatLike .* Pi;
            end
    end

    %--------------------------------------------------------------------------
    % Permute so that remaining dimensions come first, marginalized last
    %--------------------------------------------------------------------------
    PermuteOrder = [RemainDim, MargDim];
    MatLike      = permute(MatLike, PermuteOrder);

    SizeRemain = SizeChi2(RemainDim);

    MatLike = reshape(MatLike, [SizeRemain, SizeMarg]);

    %--------------------------------------------------------------------------
    % Marginalize (reduce from the last dimension backwards so that the
    % index of the not-yet-reduced dimensions stays valid)
    %--------------------------------------------------------------------------
    Nremain = numel(RemainDim);
    Nmarg   = numel(MargDim);

    for I = Nmarg:-1:1
        if IsSingleton(I)
            % No measure along this dimension. Skipping also avoids calling
            % sum/trapz with dim > ndims(MatLike), since reshape cannot create
            % trailing singleton dimensions.
            continue;
        end

        Dim = Nremain + I;

        switch Args.Method
            case "sum"
                MatLike = sum(MatLike, Dim);

            case "trapz"
                MatLike = trapz(Args.Grid{MargDim(I)}, MatLike, Dim);
        end
    end

    %--------------------------------------------------------------------------
    % Convert marginalized likelihood back to chi-square.
    % Entries with zero marginal likelihood (prior exclusion, or underflow of
    % exp(-Chi2/2) beyond ~1420 below the peak) become +Inf rather than being
    % clamped to a finite plateau that would masquerade as a poor-but-valid fit.
    %--------------------------------------------------------------------------
    MatLike = max(MatLike, 0);   % guard against -0 / tiny negative round-off

    if any(MatLike(:) == 0)
        warning('marginlizeChi2:underflow',...
                ['%d of %d output element(s) have zero marginal likelihood (prior ',...
                 'exclusion, or dynamic range beyond ~1420 in chi-square below the ',...
                 'peak) and are returned as +Inf.'], nnz(MatLike(:)==0), numel(MatLike));
    end

    MatMargChi2 = -2 .* log(MatLike) - 2 .* MaxLogL;

    %--------------------------------------------------------------------------
    % Normalize if requested
    %--------------------------------------------------------------------------
    switch Args.Normalize
        case "min0"
            MinVal = min(MatMargChi2(isfinite(MatMargChi2)));
            if isempty(MinVal)
                error('marginlizeChi2:noFiniteOutput',...
                      'No finite marginalized chi-square values; cannot normalize with ''min0''.');
            end
            MatMargChi2 = MatMargChi2 - MinVal;
        case "none"
            % Do nothing
    end

    %--------------------------------------------------------------------------
    % Ensure output shape is exactly the remaining dimensions
    %--------------------------------------------------------------------------
    if numel(RemainDim) == 1
        MatMargChi2 = reshape(MatMargChi2, [SizeChi2(RemainDim), 1]);
    else
        MatMargChi2 = reshape(MatMargChi2, SizeChi2(RemainDim));
    end

    RemainDim = reshape(RemainDim, 1, []);
end