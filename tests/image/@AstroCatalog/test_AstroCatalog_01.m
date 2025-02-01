function Result = unitTest
    % unitTest for the AstroCatalog class
    %io.msgStyle(LogLevel.Test, '@start', 'AstroCatalog test started')

    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);

    % constructor

    % @FIX - @Eran - Where is this table? We need it also on
    % Windows - Need instructions where from to take it and where
    % to put it

    AC=AstroCatalog({'asu.fit'},'HDU',2);
    [Result] = isCooPix(AC);
    if Result
       error('AstroCatalog with no X/Y coordinates reported as having X/Y coordinates'); 
    end

%     AC=AstroCatalog({'asu.fit'},'HDU',2);
    [Result, Units] = isCooSphere(AC);
    if ~Result
       error('AstroCatalog with spherical coordinates reported as having none'); 
    end    
    
    AC=AstroCatalog({rand(100,2)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE'});    
    [Result] = isCooSphere(AC);
    if Result
       error('AstroCatalog with X/Y coordinates reported as having spherical coordinates'); 
    end

%     AC=AstroCatalog({'asu.fit'},'HDU',2);
    [Result, Units] = isCooPix(AC);
    if ~Result
       error('AstroCatalog with X/Y coordinates reported as having none'); 
    end
    
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    [CooType, Units] = getCooType(AC);   
    if char(CooType(1)) ~= 'Sphere'
       error('AstroCatalog Coordinates not interpreted correctly'); 
    end
    
    AC=AstroCatalog({rand(100,2)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE'});    
    [CooType, Units] = getCooType(AC);   
    if char(CooType(1)) ~= 'pix'
       error('AstroCatalog Coordinates not interpreted correctly'); 
    end
    
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    [ColX, ColY] = getColCooForCooType(AC, 'sphere');
    AC2=AstroCatalog({rand(100,2)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE'});  
    [ColX, ColY] = getColCooForCooType(AC2, 'pix');

    AC=AstroCatalog({'asu.fit'},'HDU',2);
    AC2=AstroCatalog({rand(100,6)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE','RAJ2000','DEJ2000','mag1','mag2'});  
    [IsSphereBoth, IsPixBoth, CooType] = getCommonCooType(AC, AC2);
    if ~IsSphereBoth || IsPixBoth || ~strcmp(CooType,'sphere')
        error('AstroCatalog Coordinates not interpreted correctly'); 
    end

    %io.msgLog(LogLevel.Test, 'testing AstroCatalog constructor');
    AC = AstroCatalog({'asu.fit','asu.fit'}, 'HDU',2);
    % obsolete: [CooType, NameX, NameY, IndInCellX, IndInCellY] = getCooTypeAuto(AC);
%             if AC(1).ColX~=AC(2).ColX
%                 error('ColX in the two elements should have been identical');
%             end
%             if AC(1).ColY~=AC(2).ColY
%                 error('ColY in the two elements should have been identical');
%             end

    % sort
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog sort');
    AC = AstroCatalog({'asu.fit','asu.fit'}, 'HDU',2);
    [~,~,~,ColY] = getCooType(AC);
    AC(1).SortByCol = ColY(1);
    AC(2).SortByCol = 'DEJ2000';
    AC.sortrows(ColY);
    if ~issorted(AC(1).Catalog(:,ColY(1)))
        error('catalog is not sorted');
    end

    % sort using the SortByCol property
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog SortByCol');
    AC = AstroCatalog({'asu.fit','asu.fit'}, 'HDU',2);
    [~,~,~,ColY] = getCooType(AC);
    AC(1).SortByCol = ColY(1);
    AC(2).SortByCol = 'DEJ2000';
    AC.sortrows;
    if ~issorted(AC(1).Catalog(:,ColY(1)))
        error('catalog is not sorted');
    end

    % bounding circle
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog boundingCircle');
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    AC.Catalog(12242:end,:) = []; % until readtable1 is fixed
    [Result] = imProc.match.coneSearch(AC, [10 1], 'Radius',3600.*10);
    [CircleX, CircleY, CircleRadius] = boundingCircle(Result);
    if abs(CircleX-10)>0.1 ||  abs(CircleY-1)>0.1 || abs(CircleRadius-10)>0.5
        error('Problem with catalogBoundingCircle');
    end

    % cropXY
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog cropXY');
    AC = AstroCatalog({rand(100,3).*100}, 'ColNames',{'XWIN','YWIN','Flux'});
    Result = cropXY(AC, [1 50 1 50]);
    Result = cropXY(AC, [81 100 41 70],'AddX',{'Flux'});
    Result = cropXY(AC, [81 100 41 70; 1 50 1 50]); % multiple crops of a single catalog

    % cropLonLatlnPoly
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog cropLatInPoly');
    AC=AstroCatalog({'asu.fit'},'HDU',2);
%     Result = cropLonLatInPoly(AC, [0.03 0.04], [30 40]);
    
    % getLonLat
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog getLonLat');
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    [Lon,Lat] = getLonLat(AC);
    [Lon,Lat] = getLonLat(AC,'rad');

    % getXY
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog getXY');
    AC=AstroCatalog({rand(100,2)},'ColNames',{'XWIN_IMAGE','YWIN_IMAGE'});
    [X,Y] = getXY(AC);

    % insertFlagColFromMask
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog insertFlagColFromMask');
    AC = AstroCatalog({rand(100,2).*1023}, 'ColNames',{'X','Y'});
    MI = MaskImage({uint32(ones(1024,1024).*5)});
    insertFlagColFromMask(AC, MI);

    % cone_search? what is this?
%             io.msgLog(LogLevel.Test, 'testing AstroCatalog/catsHTM cone_search?');
%             C=catsHTM.cone_search('GAIADR2',1,1,100,'OutType','astrocatalog'); % <--- doesn't work
%             [RA, Dec, PM_RA, PM_Dec, Plx, RV] = getRADecPM(C)


%             % TRANSFERED!!
%             % match (spherical)
%             AC = AstroCatalog;
%             AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
%             AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
%             AC.getCooTypeAuto
%             AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
%             AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
%             AC2.getCooTypeAuto
%             [M,UM,TUM] = match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');
%             if ~(sizeCatalog(M)==5 && sizeCatalog(UM)==3 && sizeCatalog(TUM)==2)
%                 error('Size of matched/unmatched catalog is wrong');
%             end
%             
%             % match (pixel)
%             AC = AstroCatalog;
%             AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
%             AC.ColNames = {'X','Y'}; AC.ColUnits = {'pix','pix'};
%             AC.getCooTypeAuto
%             AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
%             AC2.ColNames = {'X','Y'}; AC2.ColUnits = {'pix','pix'};
%             AC2.getCooTypeAuto
%             [M,UM,TUM] = match(AC,AC2,'Radius',0.02);
%             if ~(sizeCatalog(M)==5 && sizeCatalog(UM)==3 && sizeCatalog(TUM)==2)
%                 error('Size of matched/unmatched catalog is wrong');
%             end


    % plot
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog getCooTypeAuto');
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    %AC.getCooTypeAuto;
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog plotMapFun');
    AC.plotMapFun('aitoff',@plotm,{},'.','MarkerSize',1);
    
    % plotSources
    AT = AstroCatalog({rand(100,3)},'ColNames',{'X','Y','Mag'});
    AT.plotSources;   

    % convertCooUnits
    %io.msgLog(LogLevel.Test, 'testing AstroCatalog convertCooUnits');
    AC=AstroCatalog({'asu.fit','asu.fit'},'HDU',2);
    AC.convertCooUnits('deg');

    % getCoo - obsolete
%             io.msgLog(LogLevel.Test, 'testing AstroCatalog getCoo');
%             AC=AstroCatalog({'asu.fit'},'HDU',2);
%             [RA, Dec] = AC.getCoo('deg');
%             [RADec]   = AC.getCoo('rad');

    % sphere_dist
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    [Dist, PA] = sphere_dist(AC,1,1);

    cd(PWD);           
    %io.msgStyle(LogLevel.Test, '@passed', 'AstroCatalog test passed')
    
    close;
    
    Result = true;
end
