function Term = dcr(Data, Args)
    % Build the Differential Chromatic Refraction term (Segev et al. 2025,
    % Eq. 5, 23-26), grouped by color bin, with independent x- and y-axis
    % coefficients (see note below).
    % Input  : - Data : struct from imUtil.agis.buildFitData (needs
    %                   .SecZ, .ParAngRad, .ColorBin -- i.e. AirMass, ParAng
    %                   and Color must have been supplied to buildFitData).
    %          * ...,key,val,...
    %            'Order' - highest trig order (1..Order, each contributing
    %                      a sin and a cos term). Default: 4 (Eq. 23).
    %            'Name'  - term name. Default: 'DCR'.
    % Output : - Term : struct conforming to the imUtil.agis term contract.
    %            Parameter grouping: one block per COLOR BIN (Data.ColorBin).
    %            Parameters per block: 2*Order*2 = [x-axis: sin1,cos1,...,
    %            sinN,cosN ; y-axis: sin1,cos1,...,sinN,cosN], i.e. the
    %            x-half and y-half of the design are mutually zero-padded
    %            (same pattern as imUtil.agis.properMotion / affine), so
    %            the x and y chromatic corrections are fitted independently
    %            even though the color-bin grouping is shared.
    % Note   : The paper's Eq. 23-26 present a single H_c shared across axes
    %          for brevity; here we allow independent x/y amplitudes, which
    %          is consistent with the axis-specific residual trends in
    %          Figs. 2-3 of the paper. If a shared-amplitude model is
    %          preferred instead, set Dx==Dy by using this term's 'Order'
    %          basis directly (straightforward local edit to localDesign).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: Term = imUtil.agis.dcr(Data);

    arguments
        Data (1,1) struct
        Args.Order (1,1) double {mustBePositive, mustBeInteger} = 4
        Args.Name char = 'DCR'
    end

    if ~isfield(Data, 'SecZ')
        error('imUtil:agis:dcr:missingField', ...
            'Data.SecZ not found. Build Data with an AirMass field (imUtil.agis.buildFitData).');
    end
    if ~isfield(Data, 'ParAngRad')
        error('imUtil:agis:dcr:missingField', ...
            'Data.ParAngRad not found. Build Data with a ParAng field (imUtil.agis.buildFitData).');
    end
    if ~isfield(Data, 'ColorBin')
        error('imUtil:agis:dcr:missingField', ...
            'Data.ColorBin not found. Build Data with a Color field (imUtil.agis.buildFitData).');
    end

    NPerAxis = 2 * Args.Order;          % sin_k, cos_k for k=1..Order
    NParams  = 2 * NPerAxis;            % x-half + y-half

    ParamNames = cell(1, NParams);
    for K = 1:Args.Order
        ParamNames{2*K-1} = sprintf('x_sin%d', K);
        ParamNames{2*K}   = sprintf('x_cos%d', K);
        ParamNames{NPerAxis + 2*K-1} = sprintf('y_sin%d', K);
        ParamNames{NPerAxis + 2*K}   = sprintf('y_cos%d', K);
    end

    Term.Name       = Args.Name;
    Term.NParams    = NParams;
    Term.ParamNames = ParamNames;
    Term.Active     = true;

    Term.GroupFun = @(D, ~) repmat(D.ColorBin, D.Nepoch, 1);

    Term.DesignFun = @(D, ~) localDesign(D, Args.Order, NPerAxis);

    Term.InitFun = @(D) localInit(D, NParams);
end

function [Dx, Dy] = localDesign(D, Order, NPerAxis)
    Basis = zeros(D.Nepoch, D.Nsrc, NPerAxis);
    for K = 1:Order
        Basis(:,:,2*K-1) = D.SecZ .* sin(K * D.ParAngRad);
        Basis(:,:,2*K)   = D.SecZ .* cos(K * D.ParAngRad);
    end

    ZeroHalf = zeros(D.Nepoch, D.Nsrc, NPerAxis);
    Dx = cat(3, Basis, ZeroHalf);
    Dy = cat(3, ZeroHalf, Basis);
end

function P0 = localInit(D, NParams)
    NBins = max(D.ColorBin);
    P0 = zeros(NParams, NBins);
end
