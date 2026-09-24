function [Result] = perfTest()
    % perfTest for imUtil.background package

    %% imUtil.background.modeVar_SampleHist + modeVar_SampleHist_mex

    Im=single(poissrnd(ones(256,256).*100));
    tic;for i=1:1e4,[m1,v1]=imUtil.background.modeVar_LogHist(Im);end,T1=toc;
    tic;for i=1:1e4,[m2,v2]=imUtil.background.modeVar_SampleHist(Im);end,T2=toc;
    tic;for i=1:1e4,[m3,v3]=imUtil.background.modeVar_SampleHist(Im, 'UseMex',false);end,T3=toc;
    fprintf('imUtil.background.modeVar_SampleHist (mex) is x %f faster than imUtil.background.modeVar_LogHist\n',T1./T2);
    fprintf('imUtil.background.modeVar_SampleHist (non mex) is x %f faster than imUtil.background.modeVar_LogHist\n',T1./T3);
    

    %%
    
    Result = true;
end