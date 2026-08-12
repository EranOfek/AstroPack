function Term = affine(Data, Args)
    % Build the per-epoch affine transformation term (Segev et al. 2025,
    % Eq. 4, 17-18, 28). Couples to a source-position term via State.
    %
    % IMPORTANT - gauge freedom: the model is additive (Eq. 28):
    %   x_ij = x0_i + mux_i*t_j + (a1_j*x0_i + a2_j*y0_i + a3_j)
    % so "no distortion" at epoch j means a1_j=...=a6_j=0 (NOT a
    % transform-matrix identity like [1,0,0,0,1,0]). Consequently there is
    % an EXACT 6-dimensional gauge freedom between the source reference
    % frame (x0,y0) and the per-epoch affine parameters: any global affine
    % reparametrization of the source frame can be exactly absorbed by a
    % compensating change to every epoch's affine parameters, leaving all
    % residuals unchanged. This term therefore pins ONE reference epoch's
    % affine parameters to zero (via Term.PinFun, applied after every
    % update) to anchor the frame; without this, x0,y0 (and the fitted
    % affine parameters) are unidentifiable even though proper motion
    % slopes typically still recover correctly.
    %
    % Input  : - Data : struct from imUtil.agis.buildFitData.
    %          * ...,key,val,...
    %            'SourceTermName' - name of the term supplying reference
    %                               positions x0,y0 (rows 1,2 of its
    %                               parameter matrix). Default: 'ProperMotion'.
    %            'FixEpoch'       - 'auto' (pin the epoch with T closest to
    %                               0), a positive integer epoch index, or
    %                               'none' (disable pinning -- NOT
    %                               recommended, the fit will be rank-
    %                               deficient). Default: 'auto'.
    %            'Name'           - term name. Default: 'Affine'.
    % Output : - Term : struct conforming to the imUtil.agis term contract
    %            (including .PinFun, applied by imUtil.agis.updateTerm).
    %            Parameter grouping: one block per EPOCH.
    %            Parameters per block: [a1;a2;a3;a4;a5;a6], matching
    %              x' = a1*x0 + a2*y0 + a3   (added to properMotion's x0)
    %              y' = a4*x0 + a5*y0 + a6   (added to properMotion's y0)
    % Author : N. Segev / imUtil.agis rewrite
    % Example: Term = imUtil.agis.affine(Data);

    arguments
        Data (1,1) struct
        Args.SourceTermName char = 'ProperMotion'
        Args.FixEpoch = 'auto'
        Args.Name char = 'Affine'
    end

    Term.Name       = Args.Name;
    Term.NParams    = 6;
    Term.ParamNames = {'a1', 'a2', 'a3', 'a4', 'a5', 'a6'};
    Term.Active     = true;

    Term.GroupFun = @(D, ~) repmat((1:D.Nepoch).', 1, D.Nsrc);

    Term.DesignFun = @(D, S) localDesign(D, S, Args.SourceTermName);

    Term.InitFun = @(D) localInit(D);

    % --- gauge-fix: pin one epoch's affine parameters to zero
    if ischar(Args.FixEpoch) || isstring(Args.FixEpoch)
        switch lower(string(Args.FixEpoch))
            case 'auto'
                [~, FixIdx] = min(abs(Data.T));
            case 'none'
                FixIdx = [];
            otherwise
                error('imUtil:agis:affine:badFixEpoch', ...
                    'FixEpoch must be ''auto'', ''none'', or a positive integer.');
        end
    else
        FixIdx = Args.FixEpoch;
    end

    if isempty(FixIdx)
        Term.PinFun = [];
        warning('imUtil:agis:affine:noGaugeFix', ...
            ['Affine term has no fixed reference epoch: the joint fit with a ', ...
             'source-position term is rank-deficient (6-dof gauge freedom). ', ...
             'Set FixEpoch to pin one epoch.']);
    else
        Term.PinFun = @(S) localPin(S, Args.Name, FixIdx);
    end
end

function [Dx, Dy] = localDesign(D, S, SourceTermName)
    if ~isfield(S.Params, SourceTermName)
        error('imUtil:agis:affine:missingSourceTerm', ...
            ['Affine term requires State.Params.%s (reference source positions). ', ...
             'Make sure a source-position term named "%s" is included in Terms.'], ...
            SourceTermName, SourceTermName);
    end

    P0 = S.Params.(SourceTermName);          % [NParams x Nsrc], rows 1,2 = x0,y0
    X0 = repmat(P0(1, :), D.Nepoch, 1);
    Y0 = repmat(P0(2, :), D.Nepoch, 1);
    One  = ones(D.Nepoch, D.Nsrc);
    Zero = zeros(D.Nepoch, D.Nsrc);

    Dx = cat(3, X0, Y0, One, Zero, Zero, Zero);
    Dy = cat(3, Zero, Zero, Zero, X0, Y0, One);
end

function P0 = localInit(D)
    % "no distortion" state: all-zero (additive-correction convention, Eq. 28)
    P0 = zeros(6, D.Nepoch);
end

function S = localPin(S, Name, FixIdx)
    S.Params.(Name)(:, FixIdx) = 0;
end
