function Result = unitTest()
    % unitTest for tools.math.stat package
    % Example: tools.math.stat.unitTest
    
    % Package Unit-Test   
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    Nsim = 1000;
    Dd   = zeros(Nsim,1);
    Ds   = zeros(Nsim,1);
    for I=1:Nsim
        V = double(rand(1e4,1));
        Dd(I) = median(V) - tools.math.stat.median1(V);
        V = single(rand(1e4,1));
        Ds(I) = median(V) - tools.math.stat.median1(V);
    end
    if max(abs(Dd))>100.*eps
        error('tools.math.stat.median1 inconsistent with median (double input)');
    end
    if max(abs(Ds))>100.*eps
        error('tools.math.stat.median1 inconsistent with median (single input)');
    end
    
    % speed test
    V    = double(rand(1e6,1));
    Nsim = 10;
    tic;
    for Isim=1:Nsim
        R = median(V);
    end
    toc
    
    tic;
    for Isim=1:Nsim
        R = fast_median(V);
    end
    toc
    
    tic;
    for Isim=1:Nsim
        R = tools.math.stat.median1(V);
    end
    toc
    
    V    = single(rand(1e6,1));
    Nsim = 10;
    tic;
    for Isim=1:Nsim
        R = median(V);
    end
    toc
    
    tic;
    for Isim=1:Nsim
        R = fast_median(V);
    end
    toc
    
    tic;
    for Isim=1:Nsim
        R = tools.math.stat.median1(V);
    end
    toc
    

    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

