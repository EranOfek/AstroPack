function Result = unitTest()
    % PerfTest for tools.math.integral
    % Example: tools.ath.integral.perfTest

    W=(1:1:200)';
    Spec=rand(200,3000);
    tic; for I=1:1000, R1=tools.math.integral.trapzmat(W,Spec,1,true); end, T1=toc;
    tic; for I=1:1000, R2=tools.math.integral.trapzmat(W,Spec,1,false); end, T2=toc;
    fprintf('tools.math.integral.mex.trapzmat_mex is faster then trapzmat by : %f\n',T2./T1);
    
    %io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

