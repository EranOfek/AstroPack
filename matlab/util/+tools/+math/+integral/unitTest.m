function Result = unitTest()
    % UnitTest for tools.math.integral
    % Example: tools.ath.integral.unitTest

    W=(1:1:200)';
    Spec=rand(200,3000);
    R1=tools.math.integral.trapzmat(W,Spec,1,true);
    R2=tools.math.integral.trapzmat(W,Spec,1,false);
    if max(abs(R1-R2))>1e-13
        error('tools.math.integral.mex.trapzmat_mex poor accuracy');
    end
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

