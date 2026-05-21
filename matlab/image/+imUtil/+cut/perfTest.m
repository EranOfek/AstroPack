function [Result] = perfTest(X, Y, Args)
    % Perf Test for i

    %% imUtil.cut.mex.imageCutouts

    R=rand(1716,1716);
    X=rand(3000,1).*1716;
    Y=rand(3000,1).*1716;
    [a1]=imUtil.cut.mex.imageCutouts(R,X,Y,25);
    [a]=imUtil.cut.mex.mex_cutout(R,[X Y],25,0,0,0,1);
    
    tic;for i=1:100, [a]=imUtil.cut.mex.mex_cutout(R,[X Y],25,0,0,0,1);end,T1=toc;
    tic;for i=1:100,[a1]=imUtil.cut.mex.imageCutouts(R,X,Y,25);end,T2=toc;
    fprintf('imUtil.cut.mex.imageCutouts is x %f faster than imUtil.cut.mex.mex_cutout\n',T1./T2);


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
