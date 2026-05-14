function [Result] = unitTest()
   % unitTest for timeSeries.fit package


    %% timeSeries.fit.fitPiecewiseLinear

    T = (1:30).';
    Err = 0.1;
    Nsrc = 1000;
    Par1 = rand(2,Nsrc);
    Par2 = rand(2,Nsrc);
    Par3 = rand(2,Nsrc);
    M    = zeros(30,Nsrc);
    for Isrc=1:1:Nsrc
        M(1:11,Isrc) = polyval(Par1(:,Isrc), T(1:11)) + randn(11,1).*Err;
        M(12:20,Isrc) = polyval(Par2(:,Isrc), T(12:20)) + randn(9,1).*Err;
        M(21:30,Isrc) = polyval(Par3(:,Isrc), T(21:30)) + randn(10,1).*Err;
    end
    
    tic;
    Result = timeSeries.fit.obsolete.fitLinearSegmentsDP(T, M, Err, ...
        'Nseg', 3, ...
        'MinNpt', 3, ...
        'SortT', true);
    toc
    tic;
    [Result1,H0] = timeSeries.fit.fitPiecewiseLinear(T, M, Err, ...
        'Nseg', 3, ...
        'MinPts', 3);
    toc
    tic;
    [Result2,H0] = timeSeries.fit.mex.fitPiecewiseLinear(T, M, Err, ...
        'Nseg', 3, ...
        'MinPts', 3);
    toc

    if max(abs(Result1(1).Seg-Result(1).Seg),[],'all')>1e-11
        error('Problem with timeSeries.fit.fitPiecewiseLinear');
    end

    if max(abs(Result2(1).Seg-Result(1).Seg),[],'all')>1e-11
        error('Problem with timeSeries.fit.mex.fitPiecewiseLinear');
    end

    %%


   Result = true;
end
