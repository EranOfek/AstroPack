function [Result] = unitTest()
    % unitTest for imUtil.match
    
    %% imUtil.match.mex.matchTwoCats

    RAD  = 180./pi;
    
    N1   = 500;
    RA0  = 100;
    Dec0 = 50;
    Err  = 0.01./3600;  % arcsec
    RA1  = rand(N1,1) + RA0;
    Dec1 = rand(N1,1) + Dec0;
    SearchRad = 1.5./(RAD.*3600);
    
    [Dec1, SI] = sort(Dec1);
    RA1        = RA1(SI);
    
    N2   = 1000;
    RA2  = rand(N2,1) + RA0;
    Dec2 = rand(N2,1) + Dec0;
    RA2(1:N1)  = RA1(1:N1) + randn(N1,1).*Err;
    Dec2(1:N1) = Dec1(1:N1) + randn(N1,1).*Err;
    
    RA1  = RA1./RAD;
    Dec1 = Dec1./RAD;
    RA2  = RA2./RAD;
    Dec2 = Dec2./RAD;
    
    [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi([RA1, Dec1], RA2, Dec2, -SearchRad);
    [IndNearest2to1, DistNearest, Nmatch, IndAll] = imUtil.match.mex.matchTwoCats(RA1, Dec1, RA2, Dec2, SearchRad, false, false, false);
       
    if any((IndNearest2to1(~isnan(IndNearest2to1))~=[Ind.Ind].'))
        error('Problem with matchTwoCats');
    end
    
    if max(DistNearest(~isnan(DistNearest))-[Ind.Dist].')>3e-8
        error('Problem with matchTwoCats');
    end
    
    %% imUtil.match.mex.matchTwoCatsXY
    % this can fail sometimes (just do it again)
    
    RAD  = 180./pi;
    
    N1   = 200;
    RA0  = 100;
    Dec0 = 50;
    Err  = 0.01./3600;  % arcsec
    RA1  = rand(N1,1) + RA0;
    Dec1 = rand(N1,1) + Dec0;
    SearchRad = 1.5./(RAD.*3600);
    
    [Dec1, SI] = sort(Dec1);
    RA1        = RA1(SI);
    
    N2   = 500;
    RA2  = rand(N2,1) + RA0;
    Dec2 = rand(N2,1) + Dec0;
    RA2(1:N1)  = RA1(1:N1) + randn(N1,1).*Err;
    Dec2(1:N1) = Dec1(1:N1) + randn(N1,1).*Err;
    
    RA1  = RA1./RAD;
    Dec1 = Dec1./RAD;
    RA2  = RA2./RAD;
    Dec2 = Dec2./RAD;
  
    [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi([RA1, Dec1], RA2, Dec2, -SearchRad);
  
    [IndNearest2to1, DistNearest, Nmatch, IndAll] = imUtil.match.mex.matchTwoCatsXY(RA1, Dec1, RA2, Dec2, SearchRad, false, false, false);
 
    
    if any((IndNearest2to1(~isnan(IndNearest2to1))~=[Ind.Ind].'))
        error('Problem with matchTwoCats');
    end
    
    if max(DistNearest(~isnan(DistNearest))-[Ind.Dist].')>1e-7
        max(DistNearest(~isnan(DistNearest))-[Ind.Dist].')
        error('Problem with matchTwoCats');
    end
    
    
    
    %% imUtil.match.mex.matchSelfCat
    
    RAD  = 180./pi;
    
    N1   = 100;
    RA0  = 100;
    Dec0 = 50;
    Err  = 0.01./3600;  % arcsec
    SearchRad = 1./3600; % arcsec
    RA1  = rand(N1,1) + RA0;
    Dec1 = rand(N1,1) + Dec0;
    SearchRad = 1.5./(RAD.*3600);
    RA1(end+1) = RA1(1) - Err;
    Dec1(end+1) = Dec1(1) - Err;
    
    RA1  = RA1./RAD;
    Dec1 = Dec1./RAD;
    
    [Dec1, SI] = sort(Dec1);
    RA1 = RA1(SI);
    
    [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi([RA1, Dec1], RA1, Dec1, -SearchRad);
   
    [IndNearest2to1, DistNearest, Nmatch, IndAll] = imUtil.match.mex.matchSelfCat(RA1, Dec1, SearchRad, false, true, false);
   
    if sum(~isnan(IndNearest2to1))~=2 || sum(Nmatch)~=2
        error('Problem');
    end
    
    % remove duplicates
    [IndNearest2to1, DistNearest, Nmatch] = imUtil.match.mex.matchSelfCat(RA1, Dec1, SearchRad, false, false, false, true);

    if sum(~isnan(IndNearest2to1))~=1 || sum(Nmatch)~=1
        error('Problem');
    end

    %% imUtil.match.mex.matchSelfCatXY
    
    RAD  = 180./pi;
    
    N1   = 100;
    RA0  = 100;
    Dec0 = 50;
    Err  = 0.01./3600;  % arcsec
    SearchRad = 1./3600; % arcsec
    RA1  = rand(N1,1) + RA0;
    Dec1 = rand(N1,1) + Dec0;
    SearchRad = 1.5./(RAD.*3600);
    RA1(end+1) = RA1(1) - Err;
    Dec1(end+1) = Dec1(1) - Err;
    
    RA1  = RA1./RAD;
    Dec1 = Dec1./RAD;
    
    [Dec1, SI] = sort(Dec1);
    RA1 = RA1(SI);
    
    [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi([RA1, Dec1], RA1, Dec1, -SearchRad);
   
    [IndNearest2to1, DistNearest, Nmatch, IndAll] = imUtil.match.mex.matchSelfCatXY(RA1, Dec1, SearchRad, false, true, false);
   
    if sum(~isnan(IndNearest2to1))~=2 || sum(Nmatch)~=2
        error('Problem');
    end
    
    % remove duplicates
    [IndNearest2to1, DistNearest, Nmatch] = imUtil.match.mex.matchSelfCat(RA1, Dec1, SearchRad, false, false, false, true);

    if sum(~isnan(IndNearest2to1))~=1 || sum(Nmatch)~=1
        error('Problem');
    end



    %% VO.search.search_sortedlat_multi (different package but related)
    RAD  = 180./pi;
    
    N1   = 1000;
    RA0  = 100;
    Dec0 = 50;
    Err  = 0.1./3600;  % arcsec
    RA1  = rand(N1,1) + RA0;
    Dec1 = rand(N1,1) + Dec0;
    
    N2   = 2000;
    RA2  = rand(N2,1) + RA0 + 1;
    Dec2 = rand(N2,1) + Dec0 + 1;
    
    Nm = 3;  % manually match 3 sources
    [RA2(10),Imin] = min(RA1);
    Dec2(10)       = Dec1(Imin);
    [RA2(20),Imax] = max(RA1);
    Dec2(20)       = Dec1(Imax);
    RA2(30)        = RA1(100);
    Dec2(30)       = Dec1(100);
    
    %
    Cat = [RA2, Dec2];
    [Cat, SI] = sortrows(Cat,2);
    SearchRad = 1.5./(RAD.*3600);
    % convert to radians:
    Cat  = Cat./RAD;
    RA1  = RA1./RAD;
    Dec1 = Dec1./RAD;
    
    
    Nt=10;
    [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi(Cat,Cat(1:Nt,1),Cat(1:Nt,2),SearchRad);
    if sum([Ind.Nmatch])~=Nt
        sum([Ind.Nmatch])
        error('Problem with VO.search.search_sortedlat_multi');
    end
    
    [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi(Cat,RA1, Dec1,SearchRad);
    IndF = find([Ind.Nmatch]>0);
    if numel(IndF)~=Nm
        error('Problem with VO.search.search_sortedlat_multi');
    end
    if sum(FlagUnique)~=Nm
        error('Problem with VO.search.search_sortedlat_multi');
    end
    
    %
    % multiple coincidence
    
    RAD  = 180./pi;
    
    N1   = 10;
    RA0  = 100;
    Dec0 = 50;
    Err  = 0.01./3600;  % arcsec
    RA1  = rand(N1,1) + RA0;
    Dec1 = rand(N1,1) + Dec0;
    SearchRad = 1.5./(RAD.*3600);
    
    
    [Dec1, SI] = sort(Dec1);
    RA1        = RA1(SI);
    
    N2   = 20;
    RA2  = RA1(1) + randn(N2,1).*Err;
    Dec2 = Dec1(1) + randn(N2,1).*Err;
    Dist = celestial.coo.sphere_dist_fast(RA1(1)./RAD, Dec1(1)./RAD, RA2./RAD, Dec2./RAD);
    [~,MinInd] = min(Dist);
    
    
    [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi([RA1, Dec1]./RAD, RA2./RAD, Dec2./RAD,SearchRad);
    if sum([Ind.Nmatch]>0)~=N2
        error('Problem with VO.search.search_sortedlat_multi / many match');
    end
    
    [Dec2, SI] = sort(Dec2);
    RA2        = RA2(SI);
    [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi([RA2, Dec2]./RAD, RA1./RAD, Dec1./RAD,SearchRad);
    if sum([Ind.Nmatch]>0)~=1
        error('Problem with VO.search.search_sortedlat_multi / many match');
    end

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

    %% imUtil.match.mex.matchCatalogsXY

    % imUtil.match.mex.matchCatalogs / full test (list 1)
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
    
    %[Ind1, Ind2, Dist1, Dist2, Nmatch1, Nmatch2] = matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true);
    [Ind1, Dist1, Nmatch1, Ind2, Dist2, Nmatch2] = imUtil.match.mex.matchCatalogsXY(RA1, Dec1, RA2, Dec2, MatchDist, true);
    
    
    % debuging code
    N1 = numel(RA1);
    N2 = numel(Dec2);
    TestNmatch1 = zeros(N1,1);
    TestDist1   = nan(N1,1);
    TestInd1    = nan(N1,1);
    for I1=1:1:N1
        Dist = tools.math.geometry.plane_dist(RA1(I1), Dec1(I1), RA2, Dec2);
        IndMatch1 = find(Dist<(MatchDist));
        TestNmatch1(I1)  = numel(IndMatch1);
    
        [TmpDist, MinInd]    = min(Dist(IndMatch1));
        if ~isempty(MinInd)
            TestDist1(I1) = TmpDist;
            TestInd1(I1) = IndMatch1(MinInd);
        end
    
    end
    
    TestVal = sum(abs(Nmatch1-TestNmatch1));
    if TestVal>eps
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogsXY / Nmatch1 output');
    end
    TestVal=max(abs(TestDist1-Dist1),[],1);
    if TestVal>1e-12
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogsXY / Dist1 output');
    end
    TestVal1 = max(abs(TestInd1-Ind1),[],1);
    TestVal2 = sum(isnan(TestInd1)~=isnan(Ind1));
    if TestVal1>eps || TestVal2>eps
        TestVal1
        TestVal2
        error('Problem with imUtil.match.mex.matchCatalogsXY / Ind1 output');
    end
    
    % imUtil.match.mex.matchCatalogs / full test (list 2)
    
    TestNmatch2 = zeros(N2,1);
    TestDist2   = nan(N2,1);
    TestInd2    = nan(N2,1);
    for I2=1:1:N2
        Dist = tools.math.geometry.plane_dist(RA2(I2), Dec2(I2), RA1, Dec1);
        IndMatch2 = find(Dist<(MatchDist));
        TestNmatch2(I2)  = numel(IndMatch2);
    
        [TmpDist, MinInd]    = min(Dist(IndMatch2));
        if ~isempty(MinInd)
            TestDist2(I2) = TmpDist;
            TestInd2(I2) = IndMatch2(MinInd);
        end
    
    end
    
    TestVal = sum(abs(Nmatch2-TestNmatch2));
    if TestVal>eps
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogsXY / Nmatch2 output');
    end
    TestVal=max(abs(TestDist2-Dist2),[],1);
    if TestVal>1e-12
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogsXY / Dist2 output');
    end
    TestVal1 = max(abs(TestInd2-Ind2),[],1);
    TestVal2 = sum(isnan(TestInd2)~=isnan(Ind2));
    if TestVal1>eps || TestVal2>eps
        TestVal1
        TestVal2
        error('Problem with imUtil.match.mex.matchCatalogsXY / Ind2 output');
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
    
    %[Ind1, Ind2, Dist1, Dist2, Nmatch1, Nmatch2] = matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true, Use1, Use2);
    [Ind1, Dist1, Nmatch1, Ind2, Dist2, Nmatch2] = imUtil.match.mex.matchCatalogsXY(RA1, Dec1, RA2, Dec2, MatchDist, true, Use1, Use2);
    
    
    Use2NaN = nan(size(Use2));
    Use2NaN(Use2) = 1;
    
    % debuging code
    TestNmatch1 = zeros(N1,1);
    TestDist1   = nan(N1,1);
    TestInd1    = nan(N1,1);
    for I1=1:1:N1
        if Use1(I1)
            Dist = tools.math.geometry.plane_dist(RA1(I1), Dec1(I1), Use2NaN.*RA2, Use2NaN.*Dec2);
            IndMatch1 = find(Dist<(MatchDist));
            TestNmatch1(I1)  = numel(IndMatch1);
        
            [TmpDist, MinInd]    = min(Dist(IndMatch1));
            if ~isempty(MinInd)
                TestDist1(I1) = TmpDist;
                TestInd1(I1) = IndMatch1(MinInd);
            end
        end
    end
    
    TestVal = max(abs(Nmatch1-TestNmatch1));
    if TestVal>eps
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogsXY / Nmatch1 output');
    end
    TestVal=max(abs(TestDist1-Dist1),[],1);
    if TestVal>1e-12
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogsXY / Dist1 output');
    end
    TestVal1 = max(abs(TestInd1-Ind1),[],1);
    TestVal2 = sum(isnan(TestInd1)~=isnan(Ind1));
    if TestVal1>eps || TestVal2>eps
        TestVal1
        TestVal2
        error('Problem with imUtil.match.mex.matchCatalogsSY / Ind1 output');
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
    
    %[Ind1, Ind2, Dist1, Dist2, Nmatch1, Nmatch2] = matchCatalogs(RA1, Dec1, RA2, Dec2, MatchDist, true, Use1, Use2);
    [Ind1, Dist1, Nmatch1, Ind2, Dist2, Nmatch2] = imUtil.match.mex.matchCatalogsXY(RA1, Dec1, RA2, Dec2, MatchDist, true, Use1, Use2);
    
    
    Use1NaN = nan(size(Use1));
    Use1NaN(Use1) = 1;
    
    % debuging code
    TestNmatch2 = zeros(N2,1);
    TestDist2   = nan(N2,1);
    TestInd2    = nan(N2,1);
    for I2=1:1:N2
        if Use2(I2)
            Dist = tools.math.geometry.plane_dist(RA2(I2), Dec2(I2), Use1NaN.*RA1, Use1NaN.*Dec1);
            IndMatch2 = find(Dist<(MatchDist));
            TestNmatch2(I2)  = numel(IndMatch2);
        
            [TmpDist, MinInd]    = min(Dist(IndMatch2));
            if ~isempty(MinInd)
                TestDist2(I2) = TmpDist;
                TestInd2(I2) = IndMatch2(MinInd);
            end
        end
    end
    
    TestVal = max(abs(Nmatch2-TestNmatch2));
    if TestVal>eps
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogsXY / Nmatch2 output');
    end
    TestVal=max(abs(TestDist2-Dist2),[],1);
    if TestVal>1e-12
        TestVal
        error('Problem with imUtil.match.mex.matchCatalogsXY / Dist2 output');
    end
    TestVal1 = max(abs(TestInd2-Ind2),[],1);
    TestVal2 = sum(isnan(TestInd2)~=isnan(Ind2));
    if TestVal1>eps || TestVal2>eps
        TestVal1
        TestVal2
        error('Problem with imUtil.match.mex.matchCatalogsXY / Ind2 output');
    end



    %%

    Result = true;

end
