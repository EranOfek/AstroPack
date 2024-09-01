function [Result] = unitTest()
    % unitTest for tools.hist
    % Example: tools.hist.unitTest

    
    X=rand(1e6,1);
    Y=rand(1e6,1);
    E=(0:0.01:1);
    N0 = histcounts2(X,Y,E,E);
    N  = tools.hist.histcounts2regular_mex(X,Y,E,E);
    N1 = tools.hist.histcounts2regular_mex(X,Y,[0 1 0.01],[0 1 0.01],false);
    
    if max(abs(double(N0) - double(N)),[],'all')>eps
        error('histcounts2regular_mex not consistent with histcounts2 (double)');
    end
    if max(abs(double(N0) - double(N1)),[],'all')>eps
        error('histcounts2regular_mex not consistent with histcounts2 (double)');
    end
    
    % speed
    tic;for I=1:1:1e2, N1=histcounts2(X,Y,E,E);end,toc
    tic;for I=1:1:1e2, N2=tools.hist.histcounts2regular_mex(X,Y,E,E);end,toc
    tic;for I=1:1:1e2, N2=tools.hist.histcounts2regular_mex(X,Y,[0 1 0.01],[0 1 0.01],false);end,toc
    
    
    if 1==0
    Xs=single(X);
    Ys=single(Y);
    Es=single(E);
    N0 = histcounts2(Xs,Ys,Es,Es);
    N = tools.hist.histcounts2regular_mex(Xs,Ys,Es,Es);
    
    if max(abs(double(N0) - double(N)),[],'all')>eps
        error('histcounts2regular_mex not consistent with histcounts2 (double)');
    end
    end
    
    % tools.hist.mex.histcounts1regular_mex
    V=rand(1e4,1);        
    E=(0:0.01:1);      
    N=tools.hist.mex.histcounts1regular_mex(V,E);
    N1=histcounts(V,E);
    if sum(abs(N-N1))>0
        error('histcounts1regular_mex inconsistent');
    end
    
    tic;for I=1:1:1e4, N=tools.hist.mex.histcounts1regular_mex(V,E);end,toc
    tic;for I=1:1:1e4, N=histcounts(V,E);end,toc                           


    
    
    Result = true;
end
