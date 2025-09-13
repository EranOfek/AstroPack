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
        Dd(I) = median(V) - tools.math.stat.mex.median1(V);
        V = single(rand(1e4,1));
        Ds(I) = median(V) - tools.math.stat.mex.median1(V);
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
    T=toc;
    fprintf('median on 1e6 double vector: %f\n',T);
    
    tic;
    for Isim=1:Nsim
        R = fast_median(V);
    end
    T=toc;
    fprintf('fast_median on 1e6 double vector: %f\n',T);
    
    tic;
    for Isim=1:Nsim
        R = tools.math.stat.mex.median1(V);
    end
    T=toc;
    fprintf('mex.median1 on 1e6 double vector: %f\n',T);
    
    V    = single(rand(1e6,1));
    Nsim = 10;
    tic;
    for Isim=1:Nsim
        R = median(V);
    end
    T=toc;
    fprintf('median on 1e6 single vector: %f\n',T);
    
    tic;
    for Isim=1:Nsim
        R = fast_median(V);
    end
    T=toc;
    fprintf('fast_median on 1e6 single vector: %f\n',T);
    
    tic;
    for Isim=1:Nsim
        R = tools.math.stat.mex.median1(V);
    end
    T=toc;
    fprintf('mex.median1 on 1e6 single vector: %f\n',T);

    tic;
    for Isim=1:Nsim
        R = tools.math.stat.mex.median(V,1);
    end
    T=toc;
    fprintf('mex.median on 1e6 single vector: %f\n',T);

    R = rand(1726,1726,20,'single');
    tic;
    for Isim=1:1:10
        M1 = tools.math.stat.mex.median(R,3,'omitnan');
    end
    T=toc;
    fprintf('mex.median on cube 3rd dim single : %f\n',T);

    tic;
    for Isim=1:1:10
        M2 = median(R,3,'omitnan');
    end
    T=toc;
    fprintf('median on cube 3rd dim single : %f\n',T);

    if max(abs(M1-M2),[],'all')>1e-6
        error('Problem with mex.median cube 3rd dim test');
    end


    
    % Test minmax
    R=randn(1700,1700);                                                 
    tic;
    for I=1:1:1000, [Min,MinInd]=min(R,[],'all','linear'); [Max, MaxInd]=max(R,[],'all','linear'); end
    T=toc;
    fprintf('min and max on 1e6 single vector: %f\n',T);
    
    tic;
    for I=1:1:1000, [Min1,Max1,MinInd1,MaxInd1]=tools.math.stat.mex.minmax(R);  end
    T=toc;
    fprintf('mex.minmax on 1e6 single vector: %f\n',T);
    if max(abs([Min-Min1, Max-Max1, MinInd-MinInd1, MaxInd-MaxInd1]))>0
        error('minmax not consistent');
    end


    % mex.std_madmean_mex
    A = rand(1726,1726,20,'single');
    tic;
    for i=1:10
        [a,b]=tools.math.stat.mex.std_madmean_mex(A,3,1);
    end
    T=toc;
    fprintf('mex.std_madmean_mex : %f\n',T);

    tic;
    for i=1:10
        a1=tools.math.stat.std_mad(A,0,3);
        b1=mean(A,3);
    end
    T=toc;
    fprintf('std_mad & mean : %f\n',T);

    if max(abs(a1-a),[],'all')>1e-7
        error('tools.math.stat.mex.std_madmean_mex mad error');
    end
    if max(abs(b1-b),[],'all')>1e-7
        error('tools.math.stat.mex.std_madmean_mex mean error');
    end


    % test tools.math.stat.mex.medianMeanStd
    A = rand(1726,1726,20);
    tic;
    for i=1:1e1
        M1a=median(A,3);
    end
    T=toc;
    fprintf('median on cube : %f\n',T);

    tic;
    for i=1:1e1
        M1a=median(A,3); M2a=mean(A,3); M3a=std(A,[],3);
    end
    T=toc;
    fprintf('median, mean, std on cube : %f\n',T);

    tic;
    for i=1:1e1
        [M1,M2,M3]=tools.math.stat.mex.medianMeanStd(A,3);
    end
    T=toc;
    fprintf('tools.math.stat.mex.medianMeanStd on cube : %f\n',T);

    if max(abs(M1-M1a),[],'all')>1e-15
        error('Problem with median in tools.math.stat.mex.medianMeanStd');
    end
    if max(abs(M2-M2a),[],'all')>1e-15
        error('Problem with mean in tools.math.stat.mex.medianMeanStd');
    end
    if max(abs(M3-M3a),[],'all')>1e-15
        error('Problem with std in tools.math.stat.mex.medianMeanStd');
    end


    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

