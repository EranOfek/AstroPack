function [Result] = perfTest(X, Y, Args)
    % KperfTest for tools.hist.
    % Example: tools.hist.perfTest
    

    %% tools.hist.mex.hist1reg_mex
    X = rand(1e5,1);
    Nsim = 1e4;
    tic;for i=1:Nsim, [N]=matlab.internal.math.histcounts(X, (0:1./10:1)); end,T1=toc;
    tic;for i=1:Nsim, [N1,E,C] = tools.hist.mex.hist1reg_mex(X, [0 1], 10, 1,0);end,T2=toc;
    fprintf('tools.hist.mex.hist1reg_mex is x %f faster than matlab (internal) (10 bins)\n',T1./T2);

    tic;for i=1:Nsim, [N]=matlab.internal.math.histcounts(X, (0:1./500:1)); end,T1=toc;
    tic;for i=1:Nsim, [N1,E,C] = tools.hist.mex.hist1reg_mex(X, [0 1], 500, 1,0);end,T2=toc;
    fprintf('tools.hist.mex.hist1reg_mex is x %f faster than matlab (internal) (500 bins)\n',T1./T2);

    %% tools.hist.mex.hist2reg_mex
    X = rand(1e5,1);
    Y = rand(1e5,1);
    Nsim = 1e2;
    tic;for i=1:Nsim, [N]=histcounts2(X, Y, (0:1./100:1), (0:1./50:1)); end,T1=toc;
    tic;for i=1:Nsim, [N1] = tools.hist.mex.hist2reg_mex(X, Y, [0 1], [0 1], 100, 50, 1, 0);end,T2=toc;
    fprintf('tools.hist.mex.hist2reg_mex is x %f faster than matlab histcounts2\n',T1./T2);

    %% tools.hist.histcounts2regular_mex
    X=rand(1e6,1);
    Y=rand(1e6,1);
    E=(0:0.01:1);
    
    % speed
    tic;for I=1:1:1e2, N1=histcounts2(X,Y,E,E);end,T1=toc;
    tic;for I=1:1:1e2, N2=tools.hist.histcounts2regular_mex(X,Y,E,E);end,T2=toc;
    %tic;for I=1:1:1e2, N2=tools.hist.histcounts2regular_mex(X,Y,[0 1 0.01],[0 1 0.01],false);end,T3=toc;
    fprintf('tools.hist.histcounts2regular_mex is x %f faster than histcounts2\n',T1./T2);
    %fprintf('tools.hist.histcounts2regular_mex is x %f faster than histcounts2\n',T1./T3);

    


    %% tools.hist.mex.hist2d_VVtrans
    Xcat=rand(1e3,1).*1024; Ycat=rand(1e3,1).*1024; Xref=[Xcat+2;1]; Yref=[Ycat+1;2];
    FlipX=1; FlipY=1;
    RangeX=[-2000 2000]; 
    RangeY=[-1000 1000]; 
    StepX=400;
    StepY=400;

    Nsim = 10;
    tic;
    for i=1:Nsim
        Dx=Xcat-FlipX.*Xref.';
        Dy=Ycat-FlipY.*Yref.';
        %[H2] = histcounts2(Dy(:),Dx(:), (RangeY(1):StepY:RangeY(2)),(RangeX(1):StepX:RangeX(2)) );
        [H2] = histcounts2(Dx(:),Dy(:), (RangeX(1):StepX:RangeX(2)),(RangeY(1):StepY:RangeY(2)) );
    end
    T1=toc;
    tic;
    for i=1:Nsim
        %[H2b,VecYa,VecXa] = hist2d_VVtrans(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
        %[H2b,VecXa,VecYa] = tools.hist.mex.hist2d_VVtrans(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
        [H2b] = tools.hist.mex.hist2d_VVtrans(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
    end
    T2=toc;
    fprintf('tools.hist.mex.hist2d_VVtrans is x %f times faster than matlab\n',T1./T2);



end



