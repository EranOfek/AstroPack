function Result = unitTest()
    % unitTest for the +imProc.stat package
    % Example: Result = imProc.stat.unitTest

    AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
    [N,edges,bin] = imProc.stat.histcounts(AI);
    [Nmax,Imax] = max(N);
    if sum(bin==Imax,'all')~=Nmax
        error('Problem with imProc.stat.histcounts');
    end
    

    AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
    imProc.stat.hist(AI);
    imProc.stat.hist(AI,[],100);
    imProc.stat.hist(AI,'Back',{100,'Normalization','cdf'});
    close all;

    AI = AstroImage({rand(10,10), rand(10,10)});
    a = imProc.stat.max(AI);
    AI = AstroImage({ones(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    AI(1).Image(1,1)=4;
    [a,b] = imProc.stat.max(AI); 
    if a(1)~=4
        error('Problem with imProc.stat.max');
    end

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.mean(AI);
    a = imProc.stat.mean(AI);
    AI = AstroImage({ones(10,10), 2*ones(10,10)},'Back',{3*ones(10,10), 4*ones(10,10)});
    [a,b] = imProc.stat.mean(AI);
    if a(1)~=1 || a(2)~=2 || b(1)~=3 || b(2)~=4 
        error('Problem with imProc.stat.mean');
    end    

    AI = AstroImage({rand(10,10), rand(10,10)});
    imProc.stat.median(AI);
    a = imProc.stat.median(AI);
    AI = AstroImage({ones(10,10), 2*ones(10,10)},'Back',{3*ones(10,10), 4*ones(10,10)});
    AI(1).Image(1,1)=4;AI(2).Image(1,1)=4;AI(1).Back(1,1)=6;AI(2).Back(1,1)=6;
    [a,b] = imProc.stat.median(AI);   
    if a(1)~=1 || a(2)~=2 || b(1)~=3 || b(2)~=4 
        error('Problem with imProc.stat.median');
    end

    [a,b] = imProc.stat.min(AI);
    if a(1)~=1 || a(2)~=2 || b(1)~=3 || b(2)~=4 
        error('Problem with imProc.stat.min');
    end

    
    AI = AstroImage({ones(10,10), 2*ones(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b] = imProc.stat.mode(AI);
    if a(1)~=1 || a(2)~=2
        error('Problem with imProc.stat.mode');
    end

    AI = AstroImage({ones(10,10), 2*ones(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b] = imProc.stat.moment(AI, 5);
    AI(1).Image(:,6:10)=3;AI(2).Image(:,6:10)=6;
    [c,d] = imProc.stat.moment(AI, 2);
    if a(1)~=0 || a(2)~=0 || c(1)~=1 || c(2)~=4 
        error('Problem with imProc.stat.moment');
    end


    [a,b] = imProc.stat.quantile(AI, 0.25);
    if a(1)~=1 || a(2)~=2 
        error('Problem with imProc.stat.quantile');
    end

    [a,b] = imProc.stat.range(AI);
    if a(1)~=2 || a(2)~=4 
        error('Problem with imProc.stat.range');
    end    

    AI = AstroImage({ones(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});
    [a,b] = imProc.stat.rstd(AI);
    if a(1)~=0 
        error('Problem with imProc.stat.rstd');
    end 

    AI = AstroImage({randn(1000,1000),ones(10,10)},'Back',{rand(1000,1000), rand(10,10)});
    imProc.stat.std(AI);
    [a] = imProc.stat.std(AI);
    if abs(a(1)-1)>1e-2 || a(2)~=0
        error('Problem with imProc.stat.std');
    end
    

    AI = AstroImage({2*randn(1000,1000),ones(10,10)},'Back',{rand(1000,1000), rand(10,10)});
    [a,b] = imProc.stat.var(AI);   
    if abs(a(1)-4)>1e-2 || a(2)~=0
        error('Problem with imProc.stat.var');
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'imProc.stat test passed')
    Result = true;
    
end
