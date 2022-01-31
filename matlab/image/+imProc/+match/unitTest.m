function Result = unitTest
    % unitTest for +imProc.match
    % Example: imProc.match.unitTest

    
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);

    
    % coneSearch
    
    %AC=AstroCatalog({'asu.fit'},'HDU',2);
    AC=AstroCatalog;
    cone_radius_arcsec= 3600;
    ra = (-0.5:0.001:0.5)';
    dec = (-0.5:0.001:0.5)';
    
    AC.Catalog=(zeros([numel(ra),1]));
    
    AC.insertCol(ra,1,'RAJ2000',{'rad'});
    AC.insertCol(dec,2,'DEJ2000',{'rad'});
    %AC.insertColumn('RAJ2000')
    [NC_1obj, Flag, Dist] = imProc.match.coneSearch(AC,[1 1],'Radius',cone_radius_arcsec);
    assert(isa(NC_1obj,'AstroCatalog'), 'imProc.match.coneSearch output is not AstroCatalog object');
    Col1 = AC.getCol(1);
    
    assert(numel(NC_1obj.getCol(1))==numel(Col1(Flag)), 'AstroCatalog(Flag) ~= new catalog, different number of rows');
    
    
    [NC_2obj, Flag, Dist] = imProc.match.coneSearch(AC,[1 1; 0 0],'Radius',cone_radius_arcsec);  % search around two positions (merged results).
    assert(numel(NC_1obj.getCol(1))<=numel(NC_2obj.getCol(1)),'bug? -matching for two objects resulted with less matched sources');
    

    % inPolygon
    %AC=AstroCatalog({'asu.fit'},'HDU',2);
    % Try inPolygon out of coordinates range;
    [InP, Flag] = imProc.match.inPolygon(AC,[max(ra)+0.1 max(dec)+0.1; max(ra)+0.5 max(dec)+0.1; max(ra)+0.1 max(dec)+0.6],'CooUnits','rad');
    assert(sum(Flag)==0,'InPolygon out of coordinates range as non zero matched sources')
    
    [InP, Flag] = imProc.match.inPolygon(AC,[0 max(dec)-0.1; max(ra)-0.5 0; max(ra)-0.1 max(dec)-0.6],'CooUnits','rad');
    assert(sum(Flag)~=0,'InPolygon inside coordinates range did not match sources');
    InP.plotMapFun

    % match
    % Test match w
    
    AC = AstroCatalog;
    AC.Catalog  = [0 0; 0.5 0.5; 1 1; 1.5 0.5; -1.5 1; -1.5 -1];
    randPos = ([rand(100,1),rand(100,1)] - 0.5)*4;
    for i =1:numel(AC.Catalog(:,1))
        D(:,i) = celestial.coo.sphere_dist_fast(AC.Catalog(i,1),AC.Catalog(i,2),randPos(:,1),randPos(:,2));
    end
    %AC.Catalog = ([rand(100,1),rand(100,1)] - 0.5)*2;
    
    AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
    AC2 = AstroCatalog; 
    
    AC2.Catalog  = randPos;
    searchRad_rad =0.2;
    AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
    [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',searchRad_rad,'RadiusUnits','rad');
    assert(sum(MC.getCol('Nmatch')) == sum(D(:)<=searchRad_rad),' imProc.match.match - number of matched sources do not agrees with distances');
    
    % match against catsHTM
    % Required: GaiaDR2 in catsHTM format to be on matlab path.
%     AC=AstroCatalog({'asu.fit'},'HDU',2);
%     imProc.match.coneSearch(AC,[1 1],'Radius',3600);
%     [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = imProc.match.match_catsHTM(AC,'GAIADR2');
%     % catsHTM is the ref catalog:
%     [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = imProc.match.match_catsHTM(AC,'GAIADR2','catsHTMisRef',true);

    % matched2matrix
    AC = AstroCatalog;
    AC.Catalog  = [2 0; 3 0; 4 0; 2.6 0 ; 2 0; 2.01 0];
    AC.ColNames = {'RA','Dec'};
    AC.ColUnits = {'rad','rad'};
    AC.getCooType;
    
    randPos = ([rand(100,1),rand(100,1)] - 0.5)*2;

    AC2 = AstroCatalog;
    %AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
    AC2.Catalog = randPos ;
    AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
    AC2.getCooType;
    [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');
    [MC,UM,TUM] = imProc.match.match([AC;AC2; AC; AC2],AC2,'Radius',0.01,'RadiusUnits','rad');

    [Res, Summary, N_Ep] = imProc.match.matched2matrix(MC, 'RA');
    
    % flagSrcWithNeighbors
    %AC = AstroCatalog({rand(100,2).*1024},'ColNames',{'X','Y'});
    %Flag = imProc.match.flagSrcWithNeighbors(AC)

    % allSources
    AC=AstroCatalog({rand(10,3), rand(10,3), rand(10,3)},'ColNames',{'RA','Dec','Z'},'ColUnits',{'rad','rad',''});
    AC(1).Catalog = [AC(1).Catalog; AC(3).Catalog(1:5,:); AC(2).Catalog(1:2,:)];
    Result = imProc.match.unifiedSourcesCatalog(AC, 'CooType','sphere');
    
    cd(PWD);
    
    Result = true;
end
