function [MatMargChi2, RemainDim] = marginalizeChi2(MatChi2, MargDim, Args)
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
    %         - (MargDim) Vector of dimensions over which to marginalize.
    %           Must contain at least 1 dimension and at most N-1 dimensions.
    %         * ...,key,val,...
    %           'Method' - Integration method over marginalized dimensions:
    %                        'sum'   : simple summation (default)
    %                        'trapz' : trapezoidal integration
    %                        Default: 'sum'
    %           'Grid' - Cell array of grid vectors for each dimension.
    %                        Used only when Args.Method='trapz'.
    %                        Args.Grid{I} should contain the coordinate vector
    %                        for dimension I. For dimensions not marginalized,
    %                        the corresponding cell may be empty.
    %                        Default: {}
    %           'Prior' - Prior specification. One of:
    %                        (a) scalar
    %                        (b) array, same size as MatChi2
    %                        (c) cell array of prior vectors, where Prior{I}
    %                            is a vector of length size(MatChi2,I) for
    %                            dimension I. Empty cells are treated as 1.
    %
    %                        In case (c), the total prior is assumed separable:
    %                            PriorTotal = prod_i Prior{i}(x_i)
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
    %            order.
    %          - Row vector containing the indices of the remaining
    %            dimensions, in the same order as in MatMargChi2.

    arguments
        MatChi2 {mustBeNumeric, mustBeNonempty}
        MargDim (1,:) {mustBeInteger, mustBePositive}
        Args.Method (1,1) string {mustBeMember(Args.Method, ["sum","trapz"])} = "sum"
        Args.Grid cell = {}
        Args.Prior = 1
        Args.Normalize (1,1) string {mustBeMember(Args.Normalize, ["min0","none"])} = "min0"
    end

    %--------------------------------------------------------------------------
    % Basic checks
    %--------------------------------------------------------------------------
    Ndim = ndims(MatChi2);
    SizeChi2 = size(MatChi2);

    MargDim = unique(MargDim(:).', 'stable');

    if any(MargDim > Ndim)
        error('MargDim contains a dimension index larger than ndims(MatChi2).');
    end

    if numel(MargDim) < 1
        error('MargDim must contain at least one dimension.');
    end

    if numel(MargDim) > Ndim - 1
        error('MargDim must contain at most Ndim-1 dimensions.');
    end

    RemainDim = setdiff(1:Ndim, MargDim, 'stable');

    %--------------------------------------------------------------------------
    % Validate prior
    %--------------------------------------------------------------------------
    PriorType = "";

    if isscalar(Args.Prior)
        PriorType = "scalar";
        if Args.Prior <= 0
            error('Scalar Args.Prior must be positive.');
        end

    elseif isnumeric(Args.Prior)
        PriorType = "array";
        if ~isequal(size(Args.Prior), SizeChi2)
            error('Args.Prior must be either a scalar, a cell array, or an array with the same size as MatChi2.');
        end
        if any(Args.Prior(:) < 0)
            error('Numeric array Args.Prior must be non-negative.');
        end

    elseif iscell(Args.Prior)
        PriorType = "cell";

        if numel(Args.Prior) < Ndim
            error('If Args.Prior is a cell array, it must contain at least ndims(MatChi2) cells.');
        end

        for I = 1:Ndim
            if isempty(Args.Prior{I})
                continue;
            end

            Pi = Args.Prior{I};

            if ~isnumeric(Pi) || ~isvector(Pi)
                error('Args.Prior{%d} must be a numeric vector or empty.', I);
            end

            if numel(Pi) ~= SizeChi2(I)
                error('Length of Args.Prior{%d} must match size(MatChi2,%d).', I, I);
            end

            if any(Pi(:) < 0)
                error('Args.Prior{%d} must contain non-negative values.', I);
            end
        end

    else
        error('Args.Prior must be either a scalar, a numeric array, or a cell array of prior vectors.');
    end

    %--------------------------------------------------------------------------
    % Validate grid if needed
    %--------------------------------------------------------------------------
    if Args.Method == "trapz"
        if isempty(Args.Grid)
            error('Args.Grid must be provided when Args.Method is ''trapz''.');
        end

        if ~iscell(Args.Grid)
            error('Args.Grid must be a cell array.');
        end

        if numel(Args.Grid) < Ndim
            error('Args.Grid must contain at least ndims(MatChi2) cells.');
        end

        for I = MargDim
            if isempty(Args.Grid{I})
                error('Args.Grid{%d} must be provided for all marginalized dimensions when using ''trapz''.', I);
            end

            if numel(Args.Grid{I}) ~= SizeChi2(I)
                error('Length of Args.Grid{%d} must match size(MatChi2,%d).', I, I);
            end
        end
    end

    %--------------------------------------------------------------------------
    % Convert chi-square to relative likelihood
    %--------------------------------------------------------------------------
    LogL = -0.5 .* MatChi2;
    MaxLogL = max(LogL(:));
    MatLike = exp(LogL - MaxLogL);

    %--------------------------------------------------------------------------
    % Apply prior
    %--------------------------------------------------------------------------
    switch PriorType
        case "scalar"
            MatLike = MatLike .* Args.Prior;

        case "array"
            MatLike = MatLike .* Args.Prior;

        case "cell"
            for I = 1:Ndim
                Pi = Args.Prior{I};

                if isempty(Pi)
                    continue;
                end

                % reshape prior vector so it broadcasts only along dimension I
                Shape = ones(1, Ndim);
                Shape(I) = SizeChi2(I);

                Pi = reshape(Pi, Shape);
                MatLike = MatLike .* Pi;
            end
    end

    %--------------------------------------------------------------------------
    % Permute so that remaining dimensions come first, marginalized last
    %--------------------------------------------------------------------------
    PermuteOrder = [RemainDim, MargDim];
    MatLike = permute(MatLike, PermuteOrder);

    SizeRemain = SizeChi2(RemainDim);
    SizeMarg = SizeChi2(MargDim);

    if isempty(SizeRemain)
        SizeRemain = 1;
    end

    if isempty(SizeMarg)
        SizeMarg = 1;
    end

    MatLike = reshape(MatLike, [SizeRemain, SizeMarg]);

    %--------------------------------------------------------------------------
    % Marginalize
    %--------------------------------------------------------------------------
    Nremain = numel(RemainDim);
    Nmarg = numel(MargDim);

    switch Args.Method
        case "sum"
            for I = Nmarg:-1:1
                MatLike = sum(MatLike, Nremain + I);
            end

        case "trapz"
            for I = Nmarg:-1:1
                DimOrig = MargDim(I);
                X = Args.Grid{DimOrig};
                MatLike = trapz(X, MatLike, Nremain + I);
            end
    end

    %--------------------------------------------------------------------------
    % Convert marginalized likelihood back to chi-square
    %--------------------------------------------------------------------------
    MatLike = max(MatLike, realmin(class(MatLike)));
    MatMargChi2 = -2 .* log(MatLike) - 2 .* MaxLogL;

    %--------------------------------------------------------------------------
    % Normalize if requested
    %--------------------------------------------------------------------------
    switch Args.Normalize
        case "min0"
            MatMargChi2 = MatMargChi2 - min(MatMargChi2(:));
        case "none"
            % Do nothing
    end

    %--------------------------------------------------------------------------
    % Ensure output shape is exactly the remaining dimensions
    %--------------------------------------------------------------------------
    if isempty(RemainDim)
        MatMargChi2 = MatMargChi2(1);
    elseif numel(RemainDim)==1
        MatMargChi2 = reshape(MatMargChi2, [SizeChi2(RemainDim), 1]);
    else
        MatMargChi2 = reshape(MatMargChi2, SizeChi2(RemainDim));
    end

    RemainDim = reshape(RemainDim, 1, []);
end