function Result=perfTest()
    % perfTest for: tools.math.stat

    %% tools.math.stat.mex.quantile1
    R = rand(1726,1726);
    tic;
    for i=1:10
        q1=quantile(R(:),0.92);
    end
    T1=toc;
    tic;
    for i=1:10
        q2=tools.math.stat.mex.quantile1(R(:),0.92);
    end
    T2=toc;

    if abs(q1-q2)>1e-6
        error('Problem with tools.math.stat.mex.quantile1');
    end
    fprintf('tools.math.stat.mex.quantile1 is faster tahn quantile by: %f\n',T1./T2)
    if (T1./T2)<1
        error('tools.math.stat.mex.quantile1 is slower than quantile');
    end

    %% tools.math.stat.mex.wmedian_mex
    Nsim=1e4;
    R = rand(1e3,3);
    W = rand(1e3,3)+2;
    tic;for i=1:Nsim,[M]=tools.math.stat.mex.wmedian_mex(R,W);end, T1=toc;
    Err = 1./sqrt(W);
    tic;for i=1:Nsim,[M1]=tools.math.stat.wmedian(R,Err);end, T2=toc;
    fprintf('tools.math.stat.mex.wmedian_mex is x %f faster compared to tools.math.stat.wmedian\n',T2./T1);

    %% tools.math.stat.mex.wmedianStd_mex
    Nsim=1e4;
    R = rand(1e3,3);
    W = rand(1e3,3)+2;
    tic;for i=1:Nsim,[M,S]=tools.math.stat.mex.wmedianStd_mex(R,W);end, T1=toc;
    Err = 1./sqrt(W);
    tic;for i=1:Nsim,[M1]=tools.math.stat.wmedian(R,Err);end, T2=toc;
    fprintf('tools.math.stat.mex.wmedianStd_mex is x %f faster compared to tools.math.stat.wmedian\n',T2./T1);


    %% tools.math.stat.mex.wmedian_mex
    Nsim=1e4;
    R = rand(1e3,3);
    W = rand(1e3,3)+2;
    tic;for i=1:Nsim,[M,S,E]=tools.math.stat.mex.wMeanStd_mex(R,W);end, T1=toc;
    Err = 1./sqrt(W);
    tic;for i=1:Nsim,[M1,E1,S1]=tools.math.stat.wmean(R,Err);end, T2=toc;
    fprintf('tools.math.stat.mex.WmeanStd_mex is x %f faster compared to tools.math.stat.wmean\n',T2./T1);

    %% tools.math.stat.mex.rstd_mex
    R = rand(1726,1726);
    Nsim=1e2;
    tic; for i=1:Nsim, r1=tools.math.stat.rstd(R,1); end, T1=toc;
    tic; for i=1:Nsim, r2=tools.math.stat.mex.rstd_mex(R,[],1); end, T2=toc;
    fprintf('rstd_mex is x %f times faster compared to rstd (Dim=1)\n',T1./T2);
    tic; for i=1:Nsim, r1=tools.math.stat.rstd(R,2); end, T1=toc;
    tic; for i=1:Nsim, r2=tools.math.stat.mex.rstd_mex(R,[],2); end, T2=toc;
    fprintf('rstd_mex is x %f times faster compared to rstd (Dim=2)\n',T1./T2);

        
        
    Result = true;
end
