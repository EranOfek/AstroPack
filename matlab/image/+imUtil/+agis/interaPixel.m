function Term = intraPixel(Data, Args)
    % Build the intra-pixel systematic term (Segev et al. 2025, S8.3): a
    % single, global 2D polynomial in sub-pixel position, with independent
    % x- and y-axis coefficients.
    % Input  : - Data : struct from imUtil.agis.buildFitData (needs .X, .Y).
    %          * ...,key,val,...
    %            'Degree' - max total degree of the 2D polynomial in
    %                       (u,v) = centered sub-pixel position.
    %                       Default: 5 (matches the paper).
    %            'Name'   - term name. Default: 'IntraPixel'.
    % Output : - Term : struct conforming to the imUtil.agis term contract.
    %            Parameter grouping: a single GLOBAL block (NBlocks=1) --
    %            the correction is a universal function of sub-pixel
    %            position, not per-source or per-epoch.
    %            Parameters: [x: all monomials u^a*v^b, a+b<=Degree ;
    %                         y: same set] (zero-padded per axis).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: Term = imUtil.agis.intraPixel(Data);

    arguments
        Data (1,1) struct
        Args.Degree (1,1) double {mustBeNonnegative, mustBeInteger} = 5
        Args.Name char = 'IntraPixel'
    end

    Exponents = localExponents(Args.Degree);   % [Nterm x 2], columns = [a,b]
    NPerAxis  = size(Exponents, 1);
    NParams   = 2 * NPerAxis;

    ParamNames = cell(1, NParams);
    for K = 1:NPerAxis
        Tag = sprintf('u%dv%d', Exponents(K,1), Exponents(K,2));
        ParamNames{K}            = ['x_' Tag];
        ParamNames{NPerAxis + K} = ['y_' Tag];
    end

    Term.Name       = Args.Name;
    Term.NParams    = NParams;
    Term.ParamNames = ParamNames;
    Term.Active     = true;

    % single global block for every observation
    Term.GroupFun = @(D, ~) ones(D.Nepoch, D.Nsrc);

    Term.DesignFun = @(D, ~) localDesign(D, Exponents, NPerAxis);

    Term.InitFun = @(D) zeros(NParams, 1);
end

function [Dx, Dy] = localDesign(D, Exponents, NPerAxis)
    U = mod(D.X, 1) - 0.5;    % centered sub-pixel position, [-0.5, 0.5)
    V = mod(D.Y, 1) - 0.5;

    Basis = zeros(D.Nepoch, D.Nsrc, NPerAxis);
    for K = 1:NPerAxis
        A = Exponents(K,1);
        B = Exponents(K,2);
        Basis(:,:,K) = (U.^A) .* (V.^B);
    end

    ZeroHalf = zeros(D.Nepoch, D.Nsrc, NPerAxis);
    Dx = cat(3, Basis, ZeroHalf);
    Dy = cat(3, ZeroHalf, Basis);
end

function Exponents = localExponents(Degree)
    % all (a,b) with a>=0, b>=0, a+b<=Degree, in a fixed, deterministic order
    Exponents = zeros(0, 2);
    for TotDeg = 0:Degree
        for A = 0:TotDeg
            B = TotDeg - A;
            Exponents(end+1, :) = [A, B]; %#ok<AGROW>
        end
    end
end
