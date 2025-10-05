function [Result] = perfTest(X, Y, Args)
    % KperfTest for tools.hist.
    % Example: tools.hist.perfTest
    

     %%
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
        [H2b,VecXa,VecYa] = tools.hist.mex.hist2d_VVtrans(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
    end
    T2=toc;
    fprintf('tools.hist.mex.hist2d_VVtrans is x %f times faster than matlab\n',T1./T2);



end



