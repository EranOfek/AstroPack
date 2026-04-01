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


    %% tools.math.stat.mex.minGlobal_mex
    R = single(rand(1716,1716));
    [a,b]=min(R,[],'all');
    [a1,b1]=tools.math.stat.mex.minGlobal_mex(R);
    if abs(a-a1)>eps || (b-b1)~=0
        error('Problem with tools.math.stat.mex.minGlobal_mex');
    end

    %% tools.math.stat.mex.maxGlobal_mex
    R = single(rand(1716,1716));
    [a,b]=max(R,[],'all');
    [a1,b1]=tools.math.stat.mex.maxGlobal_mex(R);
    if abs(a-a1)>eps || (b-b1)~=0
        error('Problem with tools.math.stat.mex.maxGlobal_mex');
    end

    %% tools.math.stat.mex.minmaxGlobal_mex
    R = single(rand(1716,1716));
    [Min,MinInd]=min(R,[],'all');
    [Max,MaxInd]=max(R,[],'all');
    [Min1,Max1]=tools.math.stat.mex.minmaxGlobal_mex(R);
    if abs(Min-Min1)>eps || (Max-Max1)>eps
        error('Problem with tools.math.stat.mex.minmaxGlobal_mex');
    end
    

    %% tools.math.stat.mex.meanStd
    R = rand(1716,1716).*1e3;
    [a,b]=tools.math.stat.mex.meanStd(R,true);
    [a1]=mean(R,'all','omitnan'); b1=std(R,1,'all','omitnan');
    if abs(a-a1)>1e-11 || abs(b-b1)>1e-4
        error('Problem with tools.math.stat.mex.meanStd');
    end



    %% tools.math.stat.mex.std_madmean_mex
    A = rand(1726,1726,20,'single');
    [a,b]=tools.math.stat.mex.std_madmean_mex(A,3,1);
    a1=tools.math.stat.std_mad(A,0,3);
    b1=mean(A,3);
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

    %% test tools.math.stat.quantile1
    % speed test in unitPerf
    R = rand(1726,1726);
    q1=quantile(R(:),0.92);
    q2=tools.math.stat.mex.quantile1(R(:),0.92);

    if abs(q1-q2)>1e-6
        error('Problem with tools.math.stat.mex.quantile1');
    end    


    %% tools.math.stat.mex.wmedian_mex
    R = rand(1e3,3);
    W = rand(1e3,3)+2;
    [M]=tools.math.stat.mex.wmedian_mex(R,W);
    Err = 1./sqrt(W);
    [M1]=tools.math.stat.wmedian(R,Err);
    if max(abs(M-M1))>1e-8
        error('Problem with tools.math.stat.mex.wmedian_mex');
    end
    
    %% tools.math.stat.mex.wmedianStd_mex
    R = rand(1e3,3);
    W = rand(1e3,3)+2;
    [M,S]=tools.math.stat.mex.wmedianStd_mex(R,W);
    Err = 1./sqrt(W);
    [M1]=tools.math.stat.wmedian(R,Err);
    S1 = std(R);
    if max(abs(M-M1))>1e-8
        error('Problem with tools.math.stat.mex.wmedian_mex');
    end

    %% tools.math.stat.mex.wMeanStd_mex
    R = rand(1e3,3);
    W = rand(1e3,3)+2;
    [M,S,E]=tools.math.stat.mex.wMeanStd_mex(R,W);
    %[M,S,E]=wMeanStd_mex(R,W);
    Err = 1./sqrt(W);
    [M1,E1,S1]=tools.math.stat.wmean(R,Err);
    if max(abs(M-M1))>1e-8 || max(abs(S-S1))>1e-3 || max(abs(E-E1))>1e-4 
        error('Problem with tools.math.stat.mex.wMeanStd_mex');
    end
    
    %% tools.math.stat.mex.rstd_mex
    R = rand(1726,1726);
    r1=tools.math.stat.rstd(R,1);
    r2=tools.math.stat.mex.rstd_mex(R,[],1);
    if max(abs(r1-r2),[],'all')>3e-3
        error('Problem with tools.math.stat.mex.rstd_mex Dim=1');
    end
    r1=tools.math.stat.rstd(R,2);
    r2=tools.math.stat.mex.rstd_mex(R,[],2);
    if max(abs(r1-r2),[],'all')>3e-3
        error('Problem with tools.math.stat.mex.rstd_mex Dim=2');
    end

    %% Test: tools.math.stat.mex.chi2_sigmaclip
    A=randn(1e4,1); B=randn(1e4,1); C=randn(1e4,1).*0.5;
    Z=(A-B)./C; Is = (Z>-2 & Z<2);  Chi2=sum(( (A(Is)-B(Is))./C(Is)).^2); Nused=sum(Is);
    [Chi1,Nused1,Is1]=tools.math.stat.mex.chi2_sigmaclip(A,B,C,[2 2]);  
    %[Chi1,Nused1,Is1]=chi2_sigmaclip(A,B,C,[2 2]);      
    if abs(Chi1-Chi2)>1e-11 || Nused~=Nused1 || any(Is~=Is1)
        error('Problem with tools.math.stat.mex.chi2_sigmaclip')
    end


    %% Test: tools.math.stat.mex.sigma_clip_cube
    A1=randn(1716,1716,20);
    A2=A1;

    [M,N] = tools.math.stat.mex.sigma_clip_cube(A1,[2 2]);
    MA=mean(A2,3,'omitnan'); SA=std(A2,[],3,'omitnan'); Z= (A2-MA)./SA; Flag=Z<-2 | Z>2; A2(Flag)=NaN; M1=mean(A2,3,'omitnan'); N1=sum(~isnan(A2),3);
    if max(abs(M1-M),[],'all')>1e-14 || max(abs(N1-N),[],'all')~=0
        error('Problem with tools.math.stat.mex.sigma_clip_cube');
    end


    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

