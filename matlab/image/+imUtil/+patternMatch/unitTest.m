function Result = unitTest()
    % unitTest for imUtil.patternMatch
    % Example: imUtil.patternMatch.unitTest

	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
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
    toc
    tic;
    for i=1:Nsim
        %[H2b,VecYa,VecXa] = hist2d_VVtrans(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
        [H2b,VecXa,VecYa] = hist2d_VVtrans_fix(Xcat,Ycat,Xref,Yref,FlipX,FlipY,RangeX,StepX,RangeY,StepY);
    end
    toc   

    if max(abs(H2-H2b),[],'all')>0
        error('Problem with tools.hist.mex.hist2d_VVtrans');
    end

    %%


    tic;
    for i=1:Nsim
        Dx=Xcat-FlipX.*Xref.';
        Dy=Ycat-FlipY.*Yref.';
        %[H2a,VecY,VecX] = tools.array.hist2d_fast(Dy(:),Dx(:),RangeY,RangeX,StepY,StepX); 
        [H2a,VecX,VecY] = tools.array.hist2d_fast(Dx(:),Dy(:),RangeX,RangeY,StepX,StepY); 
    end
    toc  
    

    tic;
    for i=1:Nsim
        Dx=Xcat-FlipX.*Xref.';
        Dy=Ycat-FlipY.*Yref.';
        H2c=tools.hist.histcounts2regular_mex(Dy(:),Dx(:),[RangeX, StepX],[RangeY, StepY], false);
    end
    toc
    

    
    if max(abs(H2-single(H2c)),[],'all')>0
        error('Problem with tools.hist.histcounts2regular_mex');
    end
    if max(abs(H2-H2a),[],'all')>0
        %% tools.array.hist2d_fast is not fully consistent with histcounts2 but it is ok (edge effects)
        error('Problem with tools.array.hist2d_fast');
    end
    

    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

