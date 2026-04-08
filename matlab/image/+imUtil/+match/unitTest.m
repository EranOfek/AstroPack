function [Result] = unitTest()
    % unitTest for imUtil.match
    
    %%

    %% imUtil.match.mex.matchCatalogs / full test (list 1)
    RAD = 180./pi;
    
    RA0  = 100;
    Dec0 = 50;
    N1 = 1000;
    N11 = 100;
    Err = 0.1./3600;
    MatchDist = 1.5./3600;
    RA1  = rand(N1,1)+RA0;
    Dec1 = rand(N1,1)+Dec0;
    Use1 = true(N1,1);
    
    RA2  = RA1 + randn(N1,1).*Err./cosd(Dec0);
    Dec2 = Dec1 + randn(N1,1).*Err;
    RA2  = [RA2; RA2+3.05];
    Dec2 = [Dec2; Dec2-3.02];
    Origin2    = [ones(N1,1).*1; ones(N1,1).*2]; 
    [Dec2, SI] = sort(Dec2);
    RA2        = RA2(SI);
    Origin2    = Origin2(SI);
    
    RA1  = [RA1; RA1(1:N11)];
    Dec1 = [Dec1; Dec1(1:N11)+0.1];
    Origin1 = [ones(N1,1).*1; ones(N11,1).*2];
    
    %tic;
    %[Ind1, Ind2, Dist1, Dist2, Nmatch1, Nmatch2] = matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true);
    [Ind1, Dist1, Nmatch1, Ind2, Dist2, Nmatch2] = imUtil.match.mex.matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true);
    %toc;
    
    % debuging code
    N1 = numel(RA1);
    N2 = numel(Dec2);
    TestNmatch1 = zeros(N1,1);
    TestDist1   = nan(N1,1);
    TestInd1    = nan(N1,1);
    for I1=1:1:N1
        Dist = celestial.coo.sphere_dist_fast(RA1(I1)./RAD, Dec1(I1)./RAD, RA2./RAD, Dec2./RAD);
        IndMatch1 = find(Dist<(MatchDist./RAD));
        TestNmatch1(I1)  = numel(IndMatch1);
    
        [TmpDist, MinInd]    = min(Dist(IndMatch1).*RAD);
        if ~isempty(MinInd)
            TestDist1(I1) = TmpDist;
            TestInd1(I1) = IndMatch1(MinInd);
        end
    
    end
    
    TestVal = sum(abs(Nmatch1-TestNmatch1));
    if TestVal>eps
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogs / Nmatch1 output');
    end
    TestVal=max(abs(TestDist1-Dist1),[],1);
    if TestVal>1e-12
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogs / Dist1 output');
    end
    TestVal1 = max(abs(TestInd1-Ind1),[],1);
    TestVal2 = sum(isnan(TestInd1)~=isnan(Ind1));
    if TestVal1>eps || TestVal2>eps
        TestVal1
        TestVal2
        error('Problem with imUtil.match.mex.matchCatalogs / Ind1 output');
    end
    
    % imUtil.match.mex.matchCatalogs / full test (list 2)
    
    TestNmatch2 = zeros(N2,1);
    TestDist2   = nan(N2,1);
    TestInd2    = nan(N2,1);
    for I2=1:1:N2
        Dist = celestial.coo.sphere_dist_fast(RA2(I2)./RAD, Dec2(I2)./RAD, RA1./RAD, Dec1./RAD);
        IndMatch2 = find(Dist<(MatchDist./RAD));
        TestNmatch2(I2)  = numel(IndMatch2);
    
        [TmpDist, MinInd]    = min(Dist(IndMatch2).*RAD);
        if ~isempty(MinInd)
            TestDist2(I2) = TmpDist;
            TestInd2(I2) = IndMatch2(MinInd);
        end
    
    end
    
    TestVal = sum(abs(Nmatch2-TestNmatch2));
    if TestVal>eps
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogs / Nmatch2 output');
    end
    TestVal=max(abs(TestDist2-Dist2),[],1);
    if TestVal>1e-12
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogs / Dist2 output');
    end
    TestVal1 = max(abs(TestInd2-Ind2),[],1);
    TestVal2 = sum(isnan(TestInd2)~=isnan(Ind2));
    if TestVal1>eps || TestVal2>eps
        TestVal1
        TestVal2
        error('Problem with imUtil.match.mex.matchCatalogs / Ind2 output');
    end
    
    
    % imUtil.match.mex.matchCatalogs / full test (list 1) with Use argument
    RAD = 180./pi;
    
    RA0  = 100;
    Dec0 = 50;
    N1 = 1000;
    N11 = 100;
    Err = 0.1./3600;
    MatchDist = 1.5./3600;
    RA1  = rand(N1,1)+RA0;
    Dec1 = rand(N1,1)+Dec0;
    Use1 = true(N1,1);
    
    RA2  = RA1 + randn(N1,1).*Err./cosd(Dec0);
    Dec2 = Dec1 + randn(N1,1).*Err;
    RA2  = [RA2; RA2+3.05];
    Dec2 = [Dec2; Dec2-3.02];
    Origin2    = [ones(N1,1).*1; ones(N1,1).*2]; 
    [Dec2, SI] = sort(Dec2);
    RA2        = RA2(SI);
    Origin2    = Origin2(SI);
    
    RA1  = [RA1; RA1(1:N11)];
    Dec1 = [Dec1; Dec1(1:N11)+0.1];
    Origin1 = [ones(N1,1).*1; ones(N11,1).*2];
    
    N1 = numel(RA1);
    N2 = numel(RA2);
    Use1 = rand(N1,1)>0.2;
    Use2 = rand(N2,1)>0.2;
    
    %tic;
    %[Ind1, Ind2, Dist1, Dist2, Nmatch1, Nmatch2] = matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true, Use1, Use2);
    [Ind1, Dist1, Nmatch1, Ind2, Dist2, Nmatch2] = imUtil.match.mex.matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true, Use1, Use2);
    %toc;
    
    Use2NaN = nan(size(Use2));
    Use2NaN(Use2) = 1;
    
    % debuging code
    TestNmatch1 = zeros(N1,1);
    TestDist1   = nan(N1,1);
    TestInd1    = nan(N1,1);
    for I1=1:1:N1
        if Use1(I1)
            Dist = celestial.coo.sphere_dist_fast(RA1(I1)./RAD, Dec1(I1)./RAD, Use2NaN.*RA2./RAD, Use2NaN.*Dec2./RAD);
            IndMatch1 = find(Dist<(MatchDist./RAD));
            TestNmatch1(I1)  = numel(IndMatch1);
        
            [TmpDist, MinInd]    = min(Dist(IndMatch1).*RAD);
            if ~isempty(MinInd)
                TestDist1(I1) = TmpDist;
                TestInd1(I1) = IndMatch1(MinInd);
            end
        end
    end
    
    TestVal = max(abs(Nmatch1-TestNmatch1));
    if TestVal>eps
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogs / Nmatch1 output');
    end
    TestVal=max(abs(TestDist1-Dist1),[],1);
    if TestVal>1e-12
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogs / Dist1 output');
    end
    TestVal1 = max(abs(TestInd1-Ind1),[],1);
    TestVal2 = sum(isnan(TestInd1)~=isnan(Ind1));
    if TestVal1>eps || TestVal2>eps
        TestVal1
        TestVal2
        error('Problem with imUtil.match.mex.matchCatalogs / Ind1 output');
    end
    
    
    
    % imUtil.match.mex.matchCatalogs / full test (list 2) with Use argument
    RAD = 180./pi;
    
    RA0  = 100;
    Dec0 = 50;
    N1 = 1000;
    N11 = 100;
    Err = 0.1./3600;
    MatchDist = 1.5./3600;
    RA1  = rand(N1,1)+RA0;
    Dec1 = rand(N1,1)+Dec0;
    Use1 = true(N1,1);
    
    RA2  = RA1 + randn(N1,1).*Err./cosd(Dec0);
    Dec2 = Dec1 + randn(N1,1).*Err;
    RA2  = [RA2; RA2+3.05];
    Dec2 = [Dec2; Dec2-3.02];
    Origin2    = [ones(N1,1).*1; ones(N1,1).*2]; 
    [Dec2, SI] = sort(Dec2);
    RA2        = RA2(SI);
    Origin2    = Origin2(SI);
    
    RA1  = [RA1; RA1(1:N11)];
    Dec1 = [Dec1; Dec1(1:N11)+0.1];
    Origin1 = [ones(N1,1).*1; ones(N11,1).*2];
    
    N1 = numel(RA1);
    N2 = numel(RA2);
    Use1 = rand(N1,1)>0.2;
    Use2 = rand(N2,1)>0.2;
    
    %tic;
    %[Ind1, Ind2, Dist1, Dist2, Nmatch1, Nmatch2] = matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true, Use1, Use2);
    [Ind1, Dist1, Nmatch1, Ind2, Dist2, Nmatch2] = imUtil.match.mex.matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true, Use1, Use2);
    %toc;
    
    Use1NaN = nan(size(Use1));
    Use1NaN(Use1) = 1;
    
    % debuging code
    TestNmatch2 = zeros(N2,1);
    TestDist2   = nan(N2,1);
    TestInd2    = nan(N2,1);
    for I2=1:1:N2
        if Use2(I2)
            Dist = celestial.coo.sphere_dist_fast(RA2(I2)./RAD, Dec2(I2)./RAD, Use1NaN.*RA1./RAD, Use1NaN.*Dec1./RAD);
            IndMatch2 = find(Dist<(MatchDist./RAD));
            TestNmatch2(I2)  = numel(IndMatch2);
        
            [TmpDist, MinInd]    = min(Dist(IndMatch2).*RAD);
            if ~isempty(MinInd)
                TestDist2(I2) = TmpDist;
                TestInd2(I2) = IndMatch2(MinInd);
            end
        end
    end
    
    TestVal = max(abs(Nmatch2-TestNmatch2));
    if TestVal>eps
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogs / Nmatch2 output');
    end
    TestVal=max(abs(TestDist2-Dist2),[],1);
    if TestVal>1e-12
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogs / Dist2 output');
    end
    TestVal1 = max(abs(TestInd2-Ind2),[],1);
    TestVal2 = sum(isnan(TestInd2)~=isnan(Ind2));
    if TestVal1>eps || TestVal2>eps
        TestVal1
        TestVal2
        error('Problem with imUtil.match.mex.matchCatalogs / Ind2 output');
    end


    %%

    Result = true;

end
