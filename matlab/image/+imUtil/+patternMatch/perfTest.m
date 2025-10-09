function perfTest()
    % performences test for: imUtil.patternMatch
    % Example: imUtil.patternMatch.perfTest

    %% imUtil.patternMatch.mex.distAngPairs_mex

    N=1e3;
    CatX=rand(N,1).*1024;
    CatY=rand(N,1).*1024;
    MaxDist = 500;
    FlipX   = 1;
    FlipY   = 1;

    Nsim=100;
  
    tic;
    for i=1:Nsim
        CatXt = CatX.*FlipX;
        CatYt = CatY.*FlipY;
        CatDiffX = CatXt - CatXt.';
        CatDiffY = CatYt - CatYt.';
        % select withn Max Dist
        Fc = abs(CatDiffX(:))<MaxDist & abs(CatDiffY(:))<MaxDist;
        % find(Fc) is slower here...
        CatDiffX = CatDiffX(Fc);
        CatDiffY = CatDiffY(Fc);
        % all possible distances/angle between sources in Cat
        CatDist  = sqrt(CatDiffX.^2 + CatDiffY.^2);
        CatTan   = atan(CatDiffY./CatDiffX);
    
    end
    FF = CatDist<MaxDist;
    CatDist = CatDist(FF);
    CatTan  = CatTan(FF);
    T1=toc;

    tic;
    for i=1:Nsim
	    [a,b]=imUtil.patternMatch.mex.distAngPairs_mex(CatX,CatY,MaxDist, false, FlipX, FlipY);
    end
    T2=toc;

    tic;
    for i=1:Nsim
	    [a,b]=imUtil.patternMatch.mex.distAngPairs_mex(CatX,CatY,MaxDist, true, FlipX, FlipY);
    end
    T3=toc;

    fprintf('imUtil.patternMatch.mex.distAngPairs_mex is x %f faster than matlab (w/duplicates)\n', T1./T2);
    fprintf('imUtil.patternMatch.mex.distAngPairs_mex is x %f faster than matlab (wo/duplicates)\n', T1./T3);

    
  


end