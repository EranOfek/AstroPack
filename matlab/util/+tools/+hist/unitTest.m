function [Result] = unitTest()
    % unitTest for tools.hist
    % Example: tools.hist.unitTest

    
    % tools.hist.histcounts2regular_mex
    X=rand(1e6,1);
    Y=rand(1e6,1);
    E=(0:0.01:1);
    N0 = histcounts2(X,Y,E,E);
    N  = tools.hist.histcounts2regular_mex(X,Y,E,E);
    N1 = tools.hist.histcounts2regular_mex(X,Y,[0 1 0.01],[0 1 0.01],false);
    
    if max(abs(double(N0) - double(N)),[],'all')>eps
        error('histtools.hist.histcounts2regular_mexcounts2regular_mex not consistent with histcounts2 (double)');
    end
    if max(abs(double(N0) - double(N1)),[],'all')>eps
        error('histcounts2regular_mex not consistent with histcounts2 (double)');
    end
    
    % speed
    tic;for I=1:1:1e2, N1=histcounts2(X,Y,E,E);end,toc
    tic;for I=1:1:1e2, N2=tools.hist.histcounts2regular_mex(X,Y,E,E);end,toc
    tic;for I=1:1:1e2, N2=tools.hist.histcounts2regular_mex(X,Y,[0 1 0.01],[0 1 0.01],false);end,toc
    
    %tic; for I=1:1:1e2, [Mat1,vx1,vy1,bx1,by1] = tools.array.hist2d_fast(X,Y, [0 1], [0 1], 0.01, 0.01); end, toc

    


    %          tic; for I=1:1:1000, [Mat1,vx1,vy1,bx1,by1] = tools.array.hist2d_fast(Xv,Yv, Xed, Yed); end, toc              
    %          tic; for I=1:1:1000, [Mat1,vx1,vy1,bx1,by1] = histcounts2(Xv,Yv, Xed, Yed); end, toc
    

    
    if 1==0
    Xs=single(X);
    Ys=single(Y);
    Es=single(E);
    N0 = histcounts2(Xs,Ys,Es,Es);
    N = tools.hist.histcounts2regular_mex(Xs,Ys,Es,Es);
    %N = tools.hist.histcounts2regular_mex(Xs,Ys,Es,Es);
    
    if max(abs(double(N0) - double(N)),[],'all')>eps
        error('histcounts2regular_mex not consistent with histcounts2 (double)');
    end
    end
    
    % tools.hist.mex.histcounts1regular_mex
    V=rand(1e4,1);        
    E=(0:0.01:1);      
    N=tools.hist.mex.histcounts1regular(V,0,0.01,100);
    
    N1=histcounts(V,E);
    if sum(abs(double(N(:))-double(N1(:))))>0
        error('histcounts1regular_mex inconsistent');
    end
    
    tic;for I=1:1:1e4, N=histcounts(V,E);end,toc                           
    tic;for I=1:1:1e4, N=tools.hist.mex.histcounts1regular(V,0,0.01,100);end,toc
    
    
    %%
    Xcat=rand(1e3,1).*1024; Ycat=rand(1e3,1).*1024; Xref=[Xcat+2;1]; Yref=[Ycat+1;2];
    FlipX=1; FlipY=1;
    RangeX=[-2000 2000]; 
    RangeY=[-1000 1000]; 
    StepX=400;
    StepY=400;

    Nsim = 1;
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
    %fprintf('tools.hist.mex.hist2d_VVtrans is x %f times faster than matlab\n',T1./T2);

    if max(abs(H2-H2b),[],'all')>0
        error('Problem with tools.hist.mex.hist2d_VVtrans');
    end



    
    
    Result = true;
end
