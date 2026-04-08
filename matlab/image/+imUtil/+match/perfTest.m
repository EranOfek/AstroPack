function [Result] = perfTest()
    % unitTest for imUtil.match
    
    %%

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
