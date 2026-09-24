function Term = properMotion(Data, Args)
    % Build the source reference-position + proper-motion (+ optional
    % parallax) term (Segev et al. 2025, Eq. 1-2, 15-16).
    % Input  : - Data : struct from imUtil.agis.buildFitData.
    %          * ...,key,val,...
    %            'UsePlx' - include the parallax parameter (5th column).
    %                       Requires Data.PlxFactorX / Data.PlxFactorY to be
    %                       precomputed and attached to Data (see
    %                       imUtil.agis.parallaxFactors). Default: false.
    %            'Name'   - term name (used as the State.Params field name).
    %                       Default: 'ProperMotion'.
    % Output : - Term : struct conforming to the imUtil.agis term contract.
    %            Parameter grouping: one block per SOURCE.
    %            Parameters per block: [x0; y0; mux; muy] (+ [plx] if UsePlx).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: Term = imUtil.agis.properMotion(Data);
    %          Term = imUtil.agis.properMotion(Data, 'UsePlx', true);

    arguments
        Data (1,1) struct
        Args.UsePlx (1,1) logical = false
        Args.Name char = 'ProperMotion'
    end

    if Args.UsePlx
        NParams = 5;
        ParamNames = {'x0', 'y0', 'mux', 'muy', 'plx'};
        if ~isfield(Data, 'PlxFactorX') || ~isfield(Data, 'PlxFactorY')
            error('imUtil:agis:properMotion:missingParallaxFactors', ...
                ['Data.PlxFactorX / Data.PlxFactorY not found. Precompute the ', ...
                 'parallax apparent-motion factors (Eq. 3-4 of Segev et al. 2025) ', ...
                 'via imUtil.agis.parallaxFactors and attach them to Data before ', ...
                 'enabling UsePlx.']);
        end
    else
        NParams = 4;
        ParamNames = {'x0', 'y0', 'mux', 'muy'};
    end

    Term.Name       = Args.Name;
    Term.NParams    = NParams;
    Term.ParamNames = ParamNames;
    Term.Active     = true;
    Term.PinFun     = [];   % no gauge-fix needed for this term

    % one parameter block per source, shared across all epochs
    Term.GroupFun = @(D, ~) repmat((1:D.Nsrc), D.Nepoch, 1);

    Term.DesignFun = @(D, ~) localDesign(D, Args.UsePlx);

    Term.InitFun = @(D) localInit(D, NParams);
end

function [Dx, Dy] = localDesign(D, UsePlx)
    One  = ones(D.Nepoch, D.Nsrc);
    Zero = zeros(D.Nepoch, D.Nsrc);
    T    = repmat(D.T, 1, D.Nsrc);

    if UsePlx
        Wx = D.PlxFactorX;
        Wy = D.PlxFactorY;
        if isequal(size(Wx), [D.Nepoch, 1])
            Wx = repmat(Wx, 1, D.Nsrc);
        end
        if isequal(size(Wy), [D.Nepoch, 1])
            Wy = repmat(Wy, 1, D.Nsrc);
        end
        Dx = cat(3, One, Zero, T, Zero, Wx);
        Dy = cat(3, Zero, One, Zero, T, Wy);
    else
        Dx = cat(3, One, Zero, T, Zero);
        Dy = cat(3, Zero, One, Zero, T);
    end
end

function P0 = localInit(D, NParams)
    P0 = zeros(NParams, D.Nsrc);
    for Isrc = 1:D.Nsrc
        Ok = find(~isnan(D.X(:, Isrc)) & ~isnan(D.Y(:, Isrc)), 1, 'first');
        if isempty(Ok)
            Ok = 1;
        end
        P0(1, Isrc) = D.X(Ok, Isrc);
        P0(2, Isrc) = D.Y(Ok, Isrc);
    end
end
