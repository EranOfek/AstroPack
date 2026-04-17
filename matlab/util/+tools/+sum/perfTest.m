function [Result] = perfTest()
    % perfTest for tools.sum package

    %% tools.sum.mex.sumPowers_mex

    R = rand(1e4,1);
    tic;for i=1:1e4, a1=tools.sum.mex.sumPowers_mex(R,3); end,T2=toc; 
    tic;for i=1:1e4, aa=[sum(R(:)), sum(R(:).^2), sum(R(:).^3)]; end,T1=toc;
    fprintf('tools.sum.mex.sumPowers_mex is x %f faster than matlab (power of 3)\n',T1./T2);
   
    tic;for i=1:1e4, a1=tools.sum.mex.sumPowers_mex(R,2); end,T2=toc; 
    tic;for i=1:1e4, aa=[sum(R(:)), sum(R(:).^2)]; end,T1=toc;
    fprintf('tools.sum.mex.sumPowers_mex is x %f faster than matlab (power of 2)\n',T1./T2);
   
    %% tools.sum.mex.sum2_mex

    R = rand(1e3,1e3);
    tic; for i=1:100, S0 = sum(R.^2, 1, 'omitnan'); end, T1=toc;
    tic; for i=1:100, S1 = tools.sum.mex.sum2_mex(R, 1); end, T2=toc;
    fprintf('tools.sum.mex.sum2_mex is x %f faster than matlab (double)\n',T1./T2);

    R = single(rand(1e3,1e3));
    tic; for i=1:100, S0 = sum(R.^2, 1, 'omitnan'); end, T1=toc;
    tic; for i=1:100, S1 = tools.sum.mex.sum2_mex(R, 1); end, T2=toc;
    fprintf('tools.sum.mex.sum2_mex is x %f faster than matlab (single)\n',T1./T2);



    %%
    Result = true;

end
