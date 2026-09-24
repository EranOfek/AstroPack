function [Dx, Dy, GroupId, NBlocks] = evalDesign(Term, Data, State)
    % Evaluate and flatten a term's design matrices and block-grouping.
    % Input  : - Term  : struct with fields .Name, .NParams, .GroupFun,
    %                    .DesignFun (see term contract).
    %                    .DesignFun(Data,State) must return [Dx3,Dy3], each
    %                    sized [Nepoch x Nsrc x NParams].
    %                    .GroupFun(Data,State) must return an [Nepoch x Nsrc]
    %                    matrix of positive integer block ids.
    %          - Data  : struct from imUtil.agis.buildFitData.
    %          - State : struct with field .Params (see imUtil.agis.initState).
    % Output : - Dx, Dy   : [Nobs x NParams] flattened local design matrices,
    %                       Nobs = Data.Nepoch*Data.Nsrc, ordered consistently
    %                       with Data.X(:).
    %          - GroupId  : [Nobs x 1] block id per observation.
    %          - NBlocks  : scalar, max(GroupId).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: [Dx,Dy,GroupId,NBlocks] = imUtil.agis.evalDesign(Term,Data,State);

    arguments
        Term (1,1) struct
        Data (1,1) struct
        State (1,1) struct
    end

    [Dx3, Dy3] = Term.DesignFun(Data, State);
    G = Term.GroupFun(Data, State);

    ExpectedSize = [Data.Nepoch, Data.Nsrc, Term.NParams];
    if ~isequal(size(Dx3), ExpectedSize) || ~isequal(size(Dy3), ExpectedSize)
        error('imUtil:agis:evalDesign:sizeMismatch', ...
            'Design matrices for term "%s" must be [%d x %d x %d].', ...
            Term.Name, ExpectedSize(1), ExpectedSize(2), ExpectedSize(3));
    end
    if ~isequal(size(G), [Data.Nepoch, Data.Nsrc])
        error('imUtil:agis:evalDesign:groupSizeMismatch', ...
            'GroupFun output for term "%s" must be [%d x %d].', ...
            Term.Name, Data.Nepoch, Data.Nsrc);
    end

    Nobs = Data.Nepoch * Data.Nsrc;
    Dx = reshape(Dx3, Nobs, Term.NParams);
    Dy = reshape(Dy3, Nobs, Term.NParams);
    GroupId = G(:);

    if any(GroupId <= 0 | isnan(GroupId))
        error('imUtil:agis:evalDesign:invalidGroupId', ...
            'GroupFun output for term "%s" must be positive integers (use NaN-safe design + zero weight instead).', ...
            Term.Name);
    end

    NBlocks = max(GroupId);
end
