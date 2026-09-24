function [Result] = unitTest()
    % unitTest for timeSeries.period
    % Example: timeSeries.period.unitTest
    

    %% timeSeries.period.mex.powerspec_mex

    T=(1:1:1000).'+randn(1000,1).*0.2;
    M=sin(2.*pi.*T./5.1)+randn(1000,1).*0.1;
    FreqVec=(0:1./2000:1);

    Nsim = 100;


    tic;
    for i=1:Nsim
        PS0=timeSeries.period.period_norm([T,M],FreqVec);
    end
    Time=toc;
    fprintf('timeSeries.period.period_norm run time: %f\n',Time);


    tic;
    for i=1:Nsim
        PS1=timeSeries.period.period_normnl([T,M],FreqVec);
    end
    Time=toc;
    fprintf('timeSeries.period.period_normnl run time: %f\n',Time);

    tic;
    for i=1:Nsim
        M=M-mean(M);
        PSm=timeSeries.period.mex.powerspec_mex(T,M,FreqVec);
        PSm=PSm./var(M);
    end
    Time=toc;
    fprintf('timeSeries.period.mex.powerspec_mex run time: %f\n',Time);

    if max(abs(PS0(:,2)-PS1(:,2)))>1e-10
        error('Problem with timeSeries.period.period_normnl');
    end
    if max(abs(PSm-PS0(:,2)))>1e-10
        error('Problem with timeSeries.period.mex.powerspec_mex');
    end
    

    %% timeSeries.period.mex.powerspecMatrix_mex
    fprintf('\n');
    T=(1:1:1000).'+randn(1000,1).*0.2;
    M=sin(2.*pi.*T./5.1)+randn(1000,1).*0.1;
    M = repmat(M, 1, 100);
    FreqVec=(0:1./2000:1);

    Nsim = 1;

    tic;
    for i=1:Nsim
        PS0 = zeros(numel(FreqVec),size(M,2));
        for J=1:100
            Temp=timeSeries.period.period_norm([T,M(:,J)],FreqVec);
            PS0(:,J) = Temp(:,2);
        end
    end
    Time=toc;
    fprintf('timeSeries.period.period_norm run time: %f\n',Time);

    tic;
    for i=1:Nsim
        M=M-mean(M);
        PSm=timeSeries.period.mex.powerspecMatrix_mex(T,M,FreqVec);
        PSm=PSm./var(M);
    end
    Time=toc;
    fprintf('timeSeries.period.mex.powerspec_mex run time: %f\n',Time);
    
    if max(abs(PSm-PS0),[],'all')>1e-10
        error('Problem with timeSeries.period.mex.powerspecMatrix_mex');
    end
    


    Result = true;

end
