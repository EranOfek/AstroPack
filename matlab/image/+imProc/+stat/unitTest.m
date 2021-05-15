function Result = unitTest()
    % unitTest for the +imProc.stat package
    % Example: Result = imProc.stat.unitTest

    AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
    [a,b,c] = imProc.stat.histcounts(AI);

    AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
    imProc.stat.hist(AI);
    imProc.stat.hist(AI,[],100);
    imProc.stat.hist(AI,[],{100,'Normalization','cdf'});

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.max(AI);
    [a,b] = imProc.stat.max(AI);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c,d] = imProc.stat.max(AI);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.mean(AI);
    [a,b] = imProc.stat.mean(AI);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.mean(AI);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.median(AI);
    [a,b] = imProc.stat.median(AI);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.median(AI);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.min(AI);
    [a,b] = imProc.stat.min(AI);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.min(AI);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.mode(AI);
    [a,b] = imProc.stat.mode(AI);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.mode(AI);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.moment(AI, 3);
    [a,b] = imProc.stat.moment(AI, 4);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.moment(AI, 5);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.quantile(AI, 0.2);
    [a,b] = imProc.stat.quantile(AI, 0.3);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.quantile(AI, 0.95);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.range(AI);
    [a,b] = imProc.stat.range(AI);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.range(AI);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.rstd(AI);
    [a,b] = imProc.stat.rstd(AI);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.rstd(AI);

    AI = AstroImage({randn(1000,1000), randn(1000,1000)});
    imProc.stat.std(AI);
    [a,b] = imProc.stat.std(AI);
    if abs(a(1)-1)>1e-2
        error('Problem with imProc.stat.std');
    end
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.std(AI);

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.var(AI);
    [a,b] = imProc.stat.var(AI);
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b,c] = imProc.stat.var(AI);
    
    Result = true;
    
end
