function Result = unitTest
    % unitTest for +imProc.match
    % Example: imProc.match.unitTest

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
    AC.getCooTypeAuto
    AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0]
    AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
    AC2.getCooTypeAuto
    [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');

    % match against catsHTM
    AC=AstroCatalog({'asu.fit'},'HDU',2);
    imProc.match.coneSearch(AC,[1 1],'Radius',3600);
    [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = imProc.match.match_catsHTM(AC,'GAIADR2');
    % catsHTM is the ref catalog:
    [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = imProc.match.match_catsHTM(AC,'GAIADR2','catsHTMisRef',true);


    % imProc.match.matchPattern
    % only shift
    Nstar = 1000;
    Ref = rand(Nstar,2).*2048 - 1024;
    %Ref = sortrows(Ref,2);
    Noverlap = 300;
    Cat = [Ref(1:Noverlap,1), Ref(1:Noverlap,2)];
    Cat = [Cat; rand(Nstar-Noverlap,2).*1024];
    Cat(:,1) = Cat(:,1) + 220 + randn(Nstar,1).*0.3;
    Cat(:,2) = Cat(:,2) + 130 + randn(Nstar,1).*0.3;
    Cat      = sortrows(Cat,2);
    Flip     = [1 -1];
    Ref      = Ref.*Flip;
    Result = imProc.match.matchPattern(Cat,Ref);
    
    if ~(Result.Sol.SN>30 && all(Result.Sol.Flip==Flip) && abs(Result.Sol.ShiftX-220)<3 && abs(Result.Sol.ShiftY-130)<3)
        error('Problem with matchPattern');
    end
    
    % shift and rotation
    Ns = 5000;
    Nm = 2000;
    Theta = 35;
    Ref = rand(Ns,2).*2048 - 1024;
    Cat = Ref(1:Nm,:) + rand(Nm,2).*0.1;
    Cat = [Cat; rand(Nm,2).*1024];
    % [Theta, Sclae, ShiftX, ShiftY, FlipX, FlipY];
    %Tr = [35, 1, 120, 45, 1, 1];
    Tr = [35, 1, 120, 45, 1, -1];
    [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
    Cat = [NewX, NewY];
    Result = imProc.match.matchPattern(Cat,Ref);

    % Shift rotation
    Ns = 5000;
    Nm = 2000;
    Ref = rand(Ns,2).*2048 - 1024;
    Cat = Ref(1:Nm,:) + rand(Nm,2).*0.1;
    %Tr = [-15, 1.0, -20, 45, 1, 1];  % ok
    %Tr = [-115, 1.0, -20, 45, 1, 1];  % ok
    %Tr = [15, 1.0, -20, 45, 1, 1];   % ok
    %Tr = [115, 1.0, -20, 45, 1, 1];  % ok
    %Tr = [-15, 1.0, -20, 45, -1, -1];  % ok
    Tr = [-115, 1.0, -20, 45, -1, -1];  % ok
    %Tr = [15, 1.0, -20, 45, -1, -1];  % ok
    %Tr = [115, 1.0, -20, 45, -1, -1];  % ok
    %Tr = [15, 1.0, -20, 45, 1, -1];  % ok
    %Tr = [115, 1.0, -20, 45, 1, -1];  % ok
    %Tr = [-115, 1.0, -20, 45, 1, -1];  % ok
    %Tr = [-15, 1.1, -20, 45, 1, -1];  % ok
    [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
    Cat = [NewX, NewY];
    Cat = [Cat; rand(Nm,2).*1024];
    Result = imProc.match.matchPattern(Cat,Ref);


    Result = true;
end
