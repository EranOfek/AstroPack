function [PredX, PredY] = predictTerm(Term, Data, State)
    % Evaluate a single term's contribution to the predicted position.
    % Input  : - Term  : term struct (see term contract).
    %          - Data  : struct from imUtil.agis.buildFitData.
    %          - State : struct with field .Params.
    % Output : - PredX, PredY : [Nepoch x Nsrc] model contribution of this
    %            term alone (zeros outside its support, if any).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: [Px,Py] = imUtil.agis.predictTerm(Term,Data,State);

    arguments
        Term (1,1) struct
        Data (1,1) struct
        State (1,1) struct
    end

    [Dx, Dy, GroupId] = imUtil.agis.evalDesign(Term, Data, State);

    P = State.Params.(Term.Name);              % [NParams x NBlocks]
    Pperobs = P(:, GroupId).';                  % [Nobs x NParams]

    PredXv = sum(Dx .* Pperobs, 2);
    PredYv = sum(Dy .* Pperobs, 2);

    PredX = reshape(PredXv, Data.Nepoch, Data.Nsrc);
    PredY = reshape(PredYv, Data.Nepoch, Data.Nsrc);
end
