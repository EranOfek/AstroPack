function [Result] = perfTest()
    % unitTest for imUtil.match
    
    %% 

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
    
    Nsim = 100;
    tic;
    for i=1:Nsim,
        [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi([RA1, Dec1], RA1, Dec1, -SearchRad);
    end
    T1=toc;
    
    tic;
    for i=1:Nsim
        [IndNearest2to1, DistNearest, Nmatch, IndAll] = imUtil.match.mex.matchSelfCat(RA1, Dec1, SearchRad, false, true, false);
    end
    T2=toc;
    fprintf('matchTwoCats is x %f faster than VO.search.search_sortedlat_multi (with IndAll)\n',T1./T2);
    
    % without IndAll
    tic;
    for i=1:Nsim
        [IndNearest2to1, DistNearest, Nmatch] = imUtil.match.mex.matchSelfCat(RA1, Dec1, SearchRad, false, false, false);
    end
    T2=toc;
    fprintf('matchTwoCats is x %f faster than VO.search.search_sortedlat_multi (w/o IndAll)\n',T1./T2);
    
    
    if sum(~isnan(IndNearest2to1))~=2
        error('Problem');
    end
    
    % remove duplicates
    [IndNearest2to1, DistNearest, Nmatch] = imUtil.match.mex.matchSelfCat(RA1, Dec1, SearchRad, false, false, false, true);
    
    if sum(~isnan(IndNearest2to1))~=1
        error('Problem');
    end


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
    
    Nsim = 100;
    tic;
    for i=1:Nsim,
        [Ind,FlagUnique,FlagFound]=VO.search.search_sortedlat_multi([RA1, Dec1], RA2, Dec2, -SearchRad);
    end
    T1=toc;
    
    tic;
    for i=1:Nsim
        [IndNearest2to1, DistNearest, Nmatch, IndAll] = imUtil.match.mex.matchTwoCats(RA1, Dec1, RA2, Dec2, SearchRad, false, false, false);
    end
    T2=toc;
    fprintf('matchTwoCats is x %f faster than VO.search.search_sortedlat_multi (with IndAll)\n',T1./T2);
    
    % without IndAll
    tic;
    for i=1:Nsim
        [IndNearest2to1, DistNearest, Nmatch] = imUtil.match.mex.matchTwoCats(RA1, Dec1, RA2, Dec2, SearchRad, false, false, false);
    end
    T2=toc;
    fprintf('matchTwoCats is x %f faster than VO.search.search_sortedlat_multi (w/o IndAll)\n',T1./T2);    
    
    if any(sum(IndNearest2to1(~isnan(IndNearest2to1))~=[Ind.Ind].'))
        error('Problem with matchTwoCats');
    end
    
    if max(DistNearest(~isnan(DistNearest))-[Ind.Dist].')>3e-8
        error('Problem with matchTwoCats');
    end
    


    %% imUtil.match.mex.matchCatalogs / speed

    RA1  = rand(1000,1)+100;
    Dec1 = rand(1000,1)+50;
    RA2  = rand(2000,1)+100;
    Dec2 = rand(2000,1)+50;
    [Dec2, SI] = sort(Dec2);
    RA2        = RA2(SI);
    Use1 = []; %true(1000,1);
    Use2 = []; %true(2000,1);
    
    Nsim=300;
    tic;
    for i=1:Nsim,
        %[Ind1, Ind2, Dist1, Dist2, Nmatch1, Nmatch2] = matchCatalogs(RA1, Dec1, RA2, Dec2, 0.01, true,Use1,Use2, true);
        %[Ind1, Dist1, Nmatch1, Ind2, Dist2, Nmatch2] = imUtil.match.mex.matchCatalogs(RA1, Dec1, RA2, Dec2, 0.01, true,Use1,Use2, false);
        [Ind1, Dist1, Nmatch1] = imUtil.match.mex.matchCatalogs(RA1, Dec1, RA2, Dec2, 0.01, true,Use1,Use2, true);
    
    end
    T1=toc;
    
    AC1 = AstroCatalog({[RA1, Dec1]});
    AC1.ColNames = {'RA','Dec'};
    AC1.ColUnits = {'deg','deg'};
    
    AC2 = AstroCatalog({[RA2, Dec2]});
    AC2.ColNames = {'RA','Dec'};
    AC2.ColUnits = {'deg','deg'};
    
    tic;
    for i=1:Nsim,
        Result = imProc.match.matchReturnIndices(AC1,AC2,'Radius',0.01,'CooType','sphere','RadiusUnits','deg');
    end
    T2=toc;
    fprintf('imUtil.match.mex.matchCatalogs is x %f faster than imProc.match.matchReturnIndices\n',T2./T1);


    %%

    Result = true;

end
