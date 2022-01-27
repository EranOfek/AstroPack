function Result = unitTest
    % unitTest for +imProc.match
    % Example: imProc.match.unitTest

    
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);

    
    % coneSearch
    
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    [NC, Flag, Dist] = imProc.match.coneSearch(AC,[1 1],'Radius',3600);
    [NC, Flag, Dist] = imProc.match.coneSearch(AC,[1 1; 0 0],'Radius',3600);  % search around two positions (merged results).

    % inPolygon
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    [InP, Flag] = imProc.match.inPolygon(AC,[1 1; 1.1 1.1; 0.5 0.1],'CooUnits','rad');
    InP.plotMapFun

    % match
    AC = AstroCatalog;
    AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
    AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
    AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
    AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
    [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');

    % match against catsHTM
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    imProc.match.coneSearch(AC,[1 1],'Radius',3600);
    [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = imProc.match.match_catsHTM(AC,'GAIADR2');
    % catsHTM is the ref catalog:
    [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = imProc.match.match_catsHTM(AC,'GAIADR2','catsHTMisRef',true);

    % matched2matrix
    AC = AstroCatalog;
    AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
    AC.ColNames = {'RA','Dec'};
    AC.ColUnits = {'rad','rad'};
    AC.getCooTypeAuto
    AC2 = AstroCatalog;
    AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
    AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
    AC2.getCooTypeAuto
    [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');
    [MC,UM,TUM] = imProc.match.match([AC;AC2; AC; AC2],AC2,'Radius',0.01,'RadiusUnits','rad');

    [Res, Summary, N_Ep] = imProc.match.matched2matrix(MC, 'RA')

    % flagSrcWithNeighbors
    AC = AstroCatalog({rand(100,2).*1024},'ColNames',{'X','Y'});
    Flag = imProc.match.flagSrcWithNeighbors(AC)

    % allSources
    AC=AstroCatalog({rand(10,3), rand(10,3), rand(10,3)},'ColNames',{'RA','Dec','Z'},'ColUnits',{'rad','rad',''});
    AC(1).Catalog = [AC(1).Catalog; AC(3).Catalog(1:5,:); AC(2).Catalog(1:2,:)];
    Result = imProc.match.unifiedSourcesCatalog(AC, 'CooType','sphere');
    
    cd(PWD);
    
    Result = true;
end
