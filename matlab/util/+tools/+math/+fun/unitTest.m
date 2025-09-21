function [Result] = unitTest()
    % unitTest for tools.math.fun
    % Example: tools.math.fun.unitTest
    
    %%
    R = rand(1726,1726, 'single').*2.*pi;
    test_sincos(R);

    R = rand(1726,1726, 'double').*2.*pi;
    test_sincos(R);

    %%


    Result = true;

end


function test_sincos(R)
    % test sincos

    tic;for i=1:100, S0=sin(R); C0=cos(R);end,T=toc;
    % Elapsed time is 4.883494 seconds.
    fprintf('matlab sin and cos run time: %f\n',T);

    tic;for i=1:100, [S1,C1]=tools.math.fun.mex.sincos(R);end,T=toc;
    % Elapsed time is 1.774913 seconds.
    fprintf('tools.math.fun.mex.sincos run time: %f\n',T);

    %tic;for i=1:100, [S2,C2]=tools.math.fun.mex.sincos_approx(R);end,T=toc;
    % Elapsed time is 1.686673 seconds.
    %fprintf('tools.math.fun.mex.sincos_approx run time: %f\n',T);

    if max(abs(S0-S1),[],'all')>1e-6 || max(abs(C0-C1),[],'all')>1e-6
        max(abs(S0-S1),[],'all')
        max(abs(C0-C1),[],'all')
        error('tools.math.fun.mex.sincos problem');
    end
    
    %if max(abs(S0-S2),[],'all')>1e-5 || max(abs(C0-C2),[],'all')>1e-5
    %    error('tools.math.fun.mex.sincos_approx problem');
    %end
end