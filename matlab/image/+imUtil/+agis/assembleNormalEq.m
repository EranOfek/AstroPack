function [N, b] = assembleNormalEq(Dx, Dy, GroupId, W, Rx, Ry, NParams, NBlocks)
    % Assemble the (block-diagonal) normal matrix and RHS vector for one term.
    % This single function replaces the term-specific calculateN*/calculateB*
    % pairs of the original pipeline (Nss/Nee/Ncc/Naa/Npix/Nhalat...).
    %
    % Input  : - Dx, Dy   : [Nobs x NParams] local design matrices (see
    %                       imUtil.agis.evalDesign).
    %          - GroupId  : [Nobs x 1] block id per observation (1..NBlocks).
    %          - W        : [Nobs x 1] observation weights (0 = excluded).
    %          - Rx, Ry   : [Nobs x 1] residuals.
    %          - NParams  : scalar, parameters per block.
    %          - NBlocks  : scalar, number of blocks.
    % Output : - N : sparse [NBlocks*NParams x NBlocks*NParams] normal matrix.
    %          - b : [NBlocks*NParams x 1] right-hand-side vector.
    %
    % Note   : N,b solve for a parameter *increment* epsilon (Newton step),
    %          consistent with Rx,Ry being residuals computed with the
    %          term's own current parameters still included (see Segev et
    %          al. 2025, Eq. 8-11). Solve via bicg(N,b,...) and add epsilon
    %          to the current parameters (imUtil.agis.updateTerm).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: [N,b] = imUtil.agis.assembleNormalEq(Dx,Dy,GroupId,W(:),Rx(:),Ry(:),NParams,NBlocks);

    arguments
        Dx (:,:) double
        Dy (:,:) double
        GroupId (:,1) double
        W (:,1) double
        Rx (:,1) double
        Ry (:,1) double
        NParams (1,1) double {mustBePositive, mustBeInteger}
        NBlocks (1,1) double {mustBePositive, mustBeInteger}
    end

    Nobs = numel(GroupId);
    if ~isequal(size(Dx), [Nobs, NParams]) || ~isequal(size(Dy), [Nobs, NParams])
        error('imUtil:agis:assembleNormalEq:sizeMismatch', ...
            'Dx/Dy must be [Nobs x NParams] = [%d x %d].', Nobs, NParams);
    end

    % --- valid observations only: nonzero weight, finite residuals & design
    Good = W > 0 & ~isnan(Rx) & ~isnan(Ry) & all(isfinite(Dx), 2) & all(isfinite(Dy), 2);
    IdxObs = find(Good);

    if isempty(IdxObs)
        N = sparse(NBlocks*NParams, NBlocks*NParams);
        b = zeros(NBlocks*NParams, 1);
        return
    end

    % --- build (obs,param) -> global column index for the sparse design
    [ObsRep, ParamRep] = ndgrid(IdxObs, (1:NParams).');
    ColIdx  = (GroupId(ObsRep) - 1) * NParams + ParamRep;
    LinIdx  = sub2ind([Nobs, NParams], ObsRep, ParamRep);

    Hx = sparse(ObsRep(:), ColIdx(:), Dx(LinIdx(:)), Nobs, NBlocks*NParams);
    Hy = sparse(ObsRep(:), ColIdx(:), Dy(LinIdx(:)), Nobs, NBlocks*NParams);

    Wd = spdiags(W, 0, Nobs, Nobs);

    N = Hx.' * Wd * Hx + Hy.' * Wd * Hy;
    b = Hx.' * (W .* Rx) + Hy.' * (W .* Ry);
end
