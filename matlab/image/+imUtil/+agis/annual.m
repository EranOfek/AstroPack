function Term = annual(Data, Args)
    % Build the annual/seasonal systematic term (Segev et al. 2025, S8.2):
    % a polynomial in fractional year, fitted per color bin, with
    % independent x- and y-axis coefficients.
    % Input  : - Data : struct from imUtil.agis.buildFitData (needs
    %                   .FracYear [always present] and .ColorBin
    %                   [requires a Color field at buildFitData time]).
    %          * ...,key,val,...
    %            'Degree' - polynomial degree in fractional year.
    %                       Default: 4 (matches the paper's 4th-order fit).
    %            'Name'   - term name. Default: 'Annual'.
    % Output : - Term : struct conforming to the imUtil.agis term contract.
    %            Parameter grouping: one block per COLOR BIN.
    %            Parameters per block: [x: c0..c4 ; y: c0..c4] (zero-padded
    %            per axis, as in imUtil.agis.dcr).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: Term = imUtil.agis.annual(Data);

    arguments
        Data (1,1) struct
        Args.Degree (1,1) double {mustBeNonnegative, mustBeInteger} = 4
        Args.Name char = 'Annual'
    end

    if ~isfield(Data, 'FracYear')
        error('imUtil:agis:annual:missingField', ...
            'Data.FracYear not found. Build Data via imUtil.agis.buildFitData.');
    end
    if ~isfield(Data, 'ColorBin')
        error('imUtil:agis:annual:missingField', ...
            'Data.ColorBin not found. Build Data with a Color field (imUtil.agis.buildFitData).');
    end

    NPerAxis = Args.Degree + 1;     % c0 ... c_Degree
    NParams  = 2 * NPerAxis;

    ParamNames = cell(1, NParams);
    for K = 0:Args.Degree
        ParamNames{K+1}            = sprintf('x_c%d', K);
        ParamNames{NPerAxis + K+1} = sprintf('y_c%d', K);
    end

    Term.Name       = Args.Name;
    Term.NParams    = NParams;
    Term.ParamNames = ParamNames;
    Term.Active     = true;

    Term.GroupFun = @(D, ~) repmat(D.ColorBin, D.Nepoch, 1);

    Term.DesignFun = @(D, ~) localDesign(D, Args.Degree, NPerAxis);

    Term.InitFun = @(D) localInit(D, NParams);
end

function [Dx, Dy] = localDesign(D, Degree, NPerAxis)
    Basis = zeros(D.Nepoch, D.Nsrc, NPerAxis);
    for K = 0:Degree
        Basis(:,:,K+1) = D.FracYear .^ K;
    end

    ZeroHalf = zeros(D.Nepoch, D.Nsrc, NPerAxis);
    Dx = cat(3, Basis, ZeroHalf);
    Dy = cat(3, ZeroHalf, Basis);
end

function P0 = localInit(D, NParams)
    NBins = max(D.ColorBin);
    P0 = zeros(NParams, NBins);
end
