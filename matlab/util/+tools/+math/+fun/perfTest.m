function [Result] = perfTest(X, Y, Args)
    % perfTest for tools.math.fun package

    %% tools.math.fun.mex.logApprox

    R = single(rand(1e4,1).*10);
    tic; for i=1:100, L0 = log(R); end, T1=toc;
    tic; for i=1:100, L1 = tools.math.fun.logApprox(R); end, T2=toc;
    fprintf('tools.math.fun.mex.logApprox is x %f faster than matlab (single)\n',T1./T2)

    R = double(rand(1e4,1).*10);
    tic; for i=1:100, L0 = log(R); end, T1=toc;
    tic; for i=1:100, L1 = tools.math.fun.logApprox(R); end, T2=toc;
    fprintf('tools.math.fun.mex.logApprox is x %f faster than matlab (double)\n',T1./T2)



    %% tools.math.fun.mex.sincos

    R = single(rand(1e5,1));
    tic;for i=1:100, S0=sin(R); C0=cos(R);end,T1=toc;
    tic;for i=1:100, [S1,C1]=tools.math.fun.mex.sincos(R);end,T2=toc;
    fprintf('tools.math.fun.mex.sincos is x %f faster than matlab (single)\n',T1./T2);

    R = double(rand(1e5,1));
    tic;for i=1:100, S0=sin(R); C0=cos(R);end,T1=toc;
    tic;for i=1:100, [S1,C1]=tools.math.fun.mex.sincos(R);end,T2=toc;
    fprintf('tools.math.fun.mex.sincos is x %f faster than matlab (double)\n',T1./T2);


    %%
    Result = true;

end
