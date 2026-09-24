function [Result] = unitTest()
    % unitTest for tools.math.fun
    % Example: tools.math.fun.unitTest
    

    %% tools.math.fun.mex.logApprox

    R = single(rand(1e4,1).*10);
    L0 = log(R);
    L1 = tools.math.fun.logApprox(R);

    if max(abs(L0-L1),[],'all')>1e-5
        error('Problem with tools.math.fun.mex.logApprox');
    end


    R = double(rand(1e4,1).*10);
    L0 = log(R);
    L1 = tools.math.fun.logApprox(R);

    if max(abs(L0-L1),[],'all')>1e-5
        error('Problem with tools.math.fun.mex.logApprox');
    end

    %%

    R = single(rand(1e4,1).*10);
    S0=sin(R); C0=cos(R);

    [S1,C1]=tools.math.fun.mex.sincos(R);

    if max(abs(S0-S1),[],'all')>1e-6 || max(abs(C0-C1),[],'all')>1e-6
        max(abs(S0-S1),[],'all')
        max(abs(C0-C1),[],'all')
        error('tools.math.fun.mex.sincos problem');
    end

    
    %%

    Result = true;

end

