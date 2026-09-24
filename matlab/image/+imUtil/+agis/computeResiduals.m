function [Rx, Ry] = computeResiduals(Data, Terms, State)
    % Compute the full model residuals: observed minus sum of all active terms.
    % Input  : - Data  : struct from imUtil.agis.buildFitData.
    %          - Terms : [1 x Nterm] struct array of terms.
    %          - State : struct with field .Params.
    % Output : - Rx, Ry : [Nepoch x Nsrc] residuals = Data.X/Y - model.
    % Author : N. Segev / imUtil.agis rewrite
    % Example: [Rx,Ry] = imUtil.agis.computeResiduals(Data,Terms,State);

    arguments
        Data (1,1) struct
        Terms (1,:) struct
        State (1,1) struct
    end

    PredX = zeros(Data.Nepoch, Data.Nsrc);
    PredY = zeros(Data.Nepoch, Data.Nsrc);

    for Ik = 1:numel(Terms)
        if ~Terms(Ik).Active
            continue
        end
        [Px, Py] = imUtil.agis.predictTerm(Terms(Ik), Data, State);
        PredX = PredX + Px;
        PredY = PredY + Py;
    end

    Rx = Data.X - PredX;
    Ry = Data.Y - PredY;
end
