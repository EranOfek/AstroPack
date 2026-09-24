function [State, Diag] = stepTerm(Terms, TermIdx, Data, State, W, Args)
    % Perform one Gauss-Seidel update of a single term's parameters.
    % Input  : - Terms   : [1 x Nterm] struct array (full model).
    %          - TermIdx : index into Terms of the term to update.
    %          - Data    : struct from imUtil.agis.buildFitData.
    %          - State   : struct with field .Params.
    %          - W       : [Nepoch x Nsrc] observation weights.
    %          * ...,key,val,...
    %            'Tol'   - bicg tolerance. Default 1e-9.
    %            'MaxIt' - bicg max iterations. Default 100.
    % Output : - State : updated struct.
    %          - Diag  : struct with fields .Name, .BicgFlag, .BicgRelRes,
    %                    .BicgIter, .RMS2D (pre-update 2D RMS residual).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: [State,Diag] = imUtil.agis.stepTerm(Terms,1,Data,State,W);

    arguments
        Terms (1,:) struct
        TermIdx (1,1) double {mustBePositive, mustBeInteger}
        Data (1,1) struct
        State (1,1) struct
        W (:,:) double
        Args.Tol (1,1) double = 1e-9
        Args.MaxIt (1,1) double = 100
    end

    Term = Terms(TermIdx);

    % residual includes ALL active terms' current parameters (this term's
    % own current estimate too) -- bicg then solves for a correction.
    [Rx, Ry] = imUtil.agis.computeResiduals(Data, Terms, State);

    [Dx, Dy, GroupId, NBlocks] = imUtil.agis.evalDesign(Term, Data, State);

    [N, b] = imUtil.agis.assembleNormalEq(Dx, Dy, GroupId(:), W(:), Rx(:), Ry(:), ...
        Term.NParams, NBlocks);

    [Epsilon, Flag, RelRes, Iter] = bicg(N, b, Args.Tol, Args.MaxIt);

    State = imUtil.agis.updateTerm(State, Term, Epsilon);

    Diag.Name       = Term.Name;
    Diag.BicgFlag   = Flag;
    Diag.BicgRelRes = RelRes;
    Diag.BicgIter   = Iter;
    Diag.RMS2D      = sqrt(mean(Rx(:).^2 + Ry(:).^2, 'omitnan'));
end
