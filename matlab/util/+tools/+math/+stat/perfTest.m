function Result=perfTest()
    % perfTest for: tools.math.stat

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

    Result = true;
end
