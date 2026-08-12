function State = initState(Data, Terms)
    % Initialize the parameter State struct for a set of fit Terms.
    % Input  : - Data  : struct produced by imUtil.agis.buildFitData.
    %          - Terms : [1 x Nterm] struct array; each element must contain
    %                    at least .Name, .Active, .InitFun (see term contract).
    % Output : - State : struct with field .Params, a struct whose field
    %            names match Terms(:).Name and whose values are
    %            [NParams x NBlocks] initial parameter matrices, as returned
    %            by each term's InitFun(Data).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: State = imUtil.agis.initState(Data, Terms);

    arguments
        Data (1,1) struct
        Terms (1,:) struct
    end

    State = struct();
    State.Params = struct();

    for Ik = 1:numel(Terms)
        Term = Terms(Ik);
        if ~Term.Active
            continue
        end
        if ~isfield(Term, 'InitFun') || isempty(Term.InitFun)
            error('imUtil:agis:initState:missingInitFun', ...
                'Term "%s" has no InitFun.', Term.Name);
        end
        State.Params.(Term.Name) = Term.InitFun(Data);
    end
end
