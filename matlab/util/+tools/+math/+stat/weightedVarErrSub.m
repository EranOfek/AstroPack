function [WVarErrSub,WVar, WErr2, WMean] = weightedVarErrSub(M, Err, Dim)
    % Weighted variance error subtracted of a sample
    %   Given a sample of measurments and their measurement errors,
    %   calculate the natural weighted variance of the sample which is not
    %   due to the measurement errors.
    %   Ignoring NaNs.
    % Input  : - Measurements
    %          - Errors
    %          - Dimension along to calculate the variance. Default is 1.
    % Output : - Weighted Var which is error subtracted.
    %          - Weighted var.
    %          - Weighted Err^2.
    %          - Weighted mean.
    % Author : Eran Ofek (2026 Mar) 
    % Example: [WVarErrSub,WVar, WErr2, WMean] = tools.math.stat.weightedVarErrSub(M, Err, Dim)

    arguments
        M
        Err
        Dim       = 1
    end

    W = 1./Err.^2;
    WMean = sum(W.*M, Dim, 'omitmissing')./sum(W, Dim, 'omitmissing');

    SumW  = sum(W, Dim, 'omitmissing');
    SumW2 = sum(W.^2, Dim, 'omitmissing'); 

    WVar  = sum(W.*(M - WMean).^2, Dim, 'omitmissing')./(SumW - SumW2./SumW);
    WErr2 = sum(W.*Err.^2, Dim, 'omitmissing')./SumW;

    WVarErrSub = WVar - WErr2;

end
