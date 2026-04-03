function [Result] = perfTest(X, Y, Args)
    % Perf Test for imUtil.cut

    %% imUtil.cut.mex.image2cube
    
    VX=(1:1:1716); VY=VX.';
    Im=VX.*1.1+VY.*1.2;
    [Sub_CCDSEC, NSub, NoOverlapCCDSEC, NewNoOverlapCCDSEC, CentersXY] = imUtil.cut.gridSubImage([1716 1716], [256 256]);
    
    Nsim = 300;
    tic;for i=1:1:Nsim, Sub=imUtil.cut.partition_subimage(Im,Sub_CCDSEC);end,T1=toc;
    tic;for i=1:1:Nsim, Cube=imUtil.cut.mex.image2cube(Im,Sub_CCDSEC);end,T2=toc;
    fprintf('imUtil.cut.mex.image2cube is x %f faster than imUtil.cut.partition_subimage\n',T1./T2);
    
    tic; for i=1:Nsim, FullImage = imUtil.cut.mex.cube2image(Cube, Sub_CCDSEC, NoOverlapCCDSEC, NewNoOverlapCCDSEC); end, T2=toc;


    %%
end
