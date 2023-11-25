function Result = unitTest()
    % OrbitalEl.unitTest
    
    %io.msgLog(LogLevel.Test, 'OrbitalEl test started');

    RAD = 180./pi;
    
    % selectFlag
    OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
    Res   = selectFlag(OrbEl, 1, true);
            
    % merge
    %OrbEl = celestial.OrbitalEl.loadSolarSystem;
    %O = merge(OrbEl);
    
    % meanMotion
    Result = meanMotion(OrbEl(1));
    
    % period
    Result = period(OrbEl(1));
    
    Result = eccAnom2radius(OrbEl(1), 1);  
    Result = trueAnom2radius(OrbEl(1), 1); 
    Result = eccAnom2radius(OrbEl(1), 1);  
    Result = trueAnom2eccAnom(OrbEl(1), 1);
    Result = nuDot(OrbEl(1), 1);           
    Result = rDot(OrbEl(1), 1);
    V=r2vel(OrbEl(1), 1); % [au/day]
    V=r2vel(OrbEl(1), 1, 's').*constant.au./1e5; %[km/s]

    OrbElA = celestial.OrbitalEl.loadSolarSystem;
    [Nu, R, E, Vel, M] = keplerSolve(OrbElA(1), 2451545);
    OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
    [Nu, R, E, Vel, M] = keplerSolve(OrbEl(1), 2451545+(1:1:10)');
    % Test parabolic orbit
    E1.Tp = celestial.time.julday([14 4 1998 0.4358]);
    E1.PeriDist = 1.487469;
    JD = celestial.time.julday([ 5 8 1998]);
   
    OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
    BodyPos = trueAnom2rectPos(OrbEl(1), 1, 1);
    [x,y,z] = trueAnom2rectPos(OrbEl(1), 1, 1);
    OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804);
    BodyPos = trueAnom2rectPos(OrbEl, [1;2], [1;2]);
            
    % ephem
    OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
    JD = celestial.time.julday([9 9 2021]);
    Cat = ephem(OrbEl, JD +(1:1:100)');

    OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
    Cat = ephem(OrbEl, JD);
    CatE = ephem(OrbEl, JD, 'GeoPos',[],'MaxIterLT',1,'IncludeMag',false);


    % compare to JPL
    TTmUTC = 70./86400;
    JD = celestial.time.julday([19 9 2021])+(0:1./24:5)';  
    Coo=[-116.865./RAD 33.3563./RAD 2000];
    OrbEl1 = celestial.OrbitalEl.loadSolarSystem([],9804);
    CatE = ephem(OrbEl1, JD + TTmUTC, 'GeoPos',Coo, 'OutUnitsDeg',false);
    [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',JD,'StopJD',JD+5,'StepSizeUnits','h','CENTER','675');
    
    % RA nd Dec diff between JPL and ephem:
    [CatE.Catalog.RA - CatJPL.Catalog(:,2), CatE.Catalog.Dec - CatJPL.Catalog(:,3)].*RAD.*3600;
    
    TTmUTC = 70./86400;
    JD = celestial.time.julday([19 9 2010])+(0:30:6000)';  
    Coo=[-116.865./RAD 33.3563./RAD 2000];
    OrbEl1 = celestial.OrbitalEl.loadSolarSystem([],9804);
    CatE = ephem(OrbEl1, JD + TTmUTC, 'GeoPos',Coo, 'OutUnitsDeg',false);
    [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',JD,'StopJD',JD+6000,'StepSize',30,'StepSizeUnits','d','CENTER','675');
    
    % RA nd Dec diff between JPL and ephem:
    [CatE.Catalog.RA - CatJPL.Catalog(:,2), CatE.Catalog.Dec - CatJPL.Catalog(:,3)].*RAD.*3600;
        
    % hyperbolic orbit
    OrbEl = celestial.OrbitalEl.loadSolarSystem('unnum','A/2017 U1');
    JD = celestial.time.julday([1 1 2018 0]);
    Cat = ephem(OrbEl, JD+(0:1./24:1), 'OutUnitsDeg',false);
    [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','A/2017 U1','StartJD',JD,'StopJD',JD+1,'StepSizeUnits','h','CENTER','399');
    %[Cat.Catalog(:,2) - CatJPL.Catalog(:,2), Cat.Catalog(:,3) - CatJPL.Catalog(:,3)].*RAD.*3600

    
    % ephem with several options
    OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
    JD = celestial.time.julday([9 9 2023]);
    Cat = ephem(OrbEl, JD );
    Cat2  = ephem(OrbEl, JD,'EarthEphem','inpop');

    assert(all(abs(1-Cat.Catalog{:,{'RA','Dec','R'}}./Cat2.Catalog{:,{'RA','Dec','R'}})<1e-3));

    %Cat = ephem(OrbEl, JD,'Integration',true);
    %Cat2  = ephem(OrbEl, JD,'Integration',true,'EarthEphem','inpop');
    %assert(all(abs(1-Cat.Catalog{:,{'RA','Dec','R'}}./Cat2.Catalog{:,{'RA','Dec','R'}})<1e-3));

    %% Testing against specific examples
    % Comet Encke (Example 33.b from Meeus 2009; p 232)
    E=celestial.OrbitalEl;
    E.Designation = 'Encke';
    E.Node        = 334.75006;
    E.W           = 186.23352;
    E.Incl        = 11.94524;
    E.Eccen       = 0.8502196;
    E.Tp          = celestial.time.julday([28 10 1990 0.54502]);
    E.A           = 2.2091404;
    E.populate;
    
    % expected values
    Ex.Nu  = -94.163310;  % true anomaly
    Ex.r   = 0.6524867;   % radius
    Ex.EA  = -34.026714;  % eccentric anomaly
    Ex.M   = -6.767367;   % mean anomaly
    Ex.X   = 0.2508066;   % equtorial position J2000
    Ex.Y   = 0.4849175;
    Ex.Z   = 0.3573373;
    
    Time          = celestial.time.julday([6 10 1990 0]);
    [Nu, R, EA, Vel, M]          = E.keplerSolve(Time);
    
    if abs(mod(Nu.*RAD - Ex.Nu, 360))>1e-6
        error('celestial.OrbitalEl/keplerSolve returns wrong Nu');
    end
    if abs(R - Ex.r)>1e-6
        error('celestial.OrbitalEl/keplerSolve returns wrong r');
    end
    if abs(mod(EA.*RAD - Ex.EA, 360))>1e-6
        error('celestial.OrbitalEl/keplerSolve returns wrong E');
    end
    if abs(mod(M.*RAD - Ex.M, 360))>1e-6
        error('celestial.OrbitalEl/keplerSolve returns wrong M');
    end
    
    BodyPos = trueAnom2rectPos(E, Nu, R);
    RotM    = celestial.coo.rotm_coo('E');  
    BodyPosEq = RotM*BodyPos';
    Diff    = BodyPosEq - [Ex.X, Ex.Y, Ex.Z]';  % equatorila
    if max(abs(Diff))>1e-7
        error('converion of nu, r to X/Y/Z equatorial is wrong');
    end
    
    % Sun Geocentric position: equatorial, J2000, rectangular
    IN = celestial.INPOP;
    IN.populateTables('all')
    
    EarPos = [IN.getPos('Ear',Time)-IN.getPos('Sun',Time)];
    Diff = [-EarPos] -[-0.9756732; -0.2003254; -0.0868566];
    if max(abs(Diff))>1e-7
        error('Sun geocentric position is wrong');
    end
    
    GeoPos = BodyPosEq - EarPos;
    Delta  = sqrt(sum(GeoPos.^2));
    if abs(Delta - 0.8243689)>1e-7
        error('Delta is wrong');
    end
    C = constant.c./constant.au.*86400;  % speed of light [AU/day]
    LightTime = Delta./C;
    [Nu, R, EA, Vel, M]          = E.keplerSolve(Time - LightTime);
    BodyPos = trueAnom2rectPos(E, Nu, R);
    RotM    = celestial.coo.rotm_coo('E');  
    BodyPosEq = RotM*BodyPos';
    GeoPos = BodyPosEq - EarPos;
    RA     = atan2d(GeoPos(2), GeoPos(1));
    Dec    = atand(GeoPos(3)./sqrt(  sum(GeoPos(1:2).^2)));
    if abs(RA - 158.558965)>1e-5 || abs(Dec-19.158496)>1e-5
        error('RA/Dec are wrong');
    end
    
    
    %% testing ephem against JPL
    E = celestial.OrbitalEl.loadSolarSystem([],9804);
    [X, V, JD0] = elements2pos(E, 'CooSys','ec');
    [T] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','VECTORS','TimeScale','TT', 'StartTime',JD0, 'StopTime',JD0+0.5,'CENTER','500@0');
    if any(abs([T.X;T.Y;T.Z]-X)>1e-6)
        error('Conversion orbital elements to position not consistent with JPL ephemeris');
    end
        
    
    % ephmeris comparisons (Kepler equation)
    TimeStep = 10;
    OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
    JD = OrbEl.Epoch - 100 + (0:TimeStep:200)';
    Cat = ephem(OrbEl, JD);
    [T] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','OBSERVER','TimeScale','TT', 'StartTime',JD(1), 'StopTime',JD(end), 'StepSize',TimeStep); 
    Diff = [[Cat.Catalog.RA - T.RA], [Cat.Catalog.Dec - T.Dec]].*3600;
    if any(abs(Diff))>2
        error('Large difference between JPL and ephem');
    end
    
    % Testing orbital integration
    OrbEl = celestial.OrbitalEl.loadSolarSystem([],[9804]);
    JD=ceil(OrbEl.Epoch)+0.5;
    IN = celestial.INPOP;
    IN.populateAll;
    CatInt = ephemIntegrateMultiTime1dir(OrbEl, JD+(0:100:5000).', 'INPOP',IN);
    Cat = ephemKeplerMultiTime(OrbEl, JD+(0:100:5000).');
    [T,~,U] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+5000,'StepSize',100);
    [(Cat.Catalog.RA-T.RA).*3600, (CatInt.Catalog.RA-T.RA).*3600, (CatInt.Catalog.Dec-T.Dec).*3600]
    if any((CatInt(:,2)-T.RA).*3600>1) || any((CatInt(:,2)-T.RA).*3600>1)
        error('Orbital integration diverges by more than 1 arcsec over 5000 days');
    end
    if any((CatInt.Catalog.SOT - T.SOT)>1)
        error('Error in SOT angle');
    end
    
    % Testing target position
    OrbEl1=celestial.OrbitalEl.loadSolarSystem('num',9804);
    JD = 2461000.5;
    [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(OrbEl1, JD+(0:1:10)','Integration',true, 'RefFrame','bary');
    % convert to ecliptic
    U_B_ec = celestial.coo.rotm_coo('e')*U_B;
    S_B_ec = celestial.coo.rotm_coo('e')*S_B;
    [T,~,U] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','VECTORS','TimeScale','TDB', 'StartTime',JD, 'StopTime',JD+10, 'StepSize',1, 'CENTER','500@0');
    if any(abs(T.X - U_B_ec(1,:)')>1e-6)
        error('Error in targetBaryPos');
    end

    % same with heliocentric position
    % Not working!
    % Test by obtaining the Sun barycentric position
    [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(OrbEl1, JD+(0:1:10)','Integration',true, 'RefFrame','bary');
    [Th,~,U] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','VECTORS','TimeScale','TDB', 'StartTime',JD, 'StopTime',JD+10.1, 'StepSize',1, 'CENTER','500@10');
    [TSB,~,U] = celestial.SolarSys.getJPL_ephem('500@10','EPHEM_TYPE','VECTORS','TimeScale','TDB', 'StartTime',JD, 'StopTime',JD+10.1, 'StepSize',1, 'CENTER','500@0');
    if any(abs(Th.X - (U_B_ec(1,:)' - S_B_ec(1,:)'))>1e-6)
        error('Error in targetBaryPos');
    end
    
     
     % integrateElements
     % Test the orbital elements propagation from one epoch to another via
     %direct integration
     OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9900]);
     OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801]);
     JD = 2460300.5;
     Result = integrateElements(OrbEl, JD);
     % compare with JPL
     [OrbEl_J] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','ELEMENTS','TimeScale','TDB','StartTime',JD,'StopTime',JD+0.5, 'OutType','OrbitalEl');
            
     [OrbEl_J.W - Result.W, OrbEl_J.Incl - Result.Incl]
     
     if abs(OrbEl_J.Node - OrbEl.Node)>1e-4 || ...
            abs(OrbEl_J.W - OrbEl.W)>1e-4 || ...
            abs(OrbEl_J.Incl - OrbEl.Incl)>1e-4 || ...
            abs(OrbEl_J.A - OrbEl.A)>1e-7 || ...
            abs(OrbEl_J.Eccen - OrbEl.Eccen)>1e-7
        error('Error in integrateElements');
     end
     
     
     
     
     
     
     
     
    % ephemMultiObj
    OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9900]);
    IN=celestial.INPOP;
    IN.populateAll;
    JD = 2460300.5;
    Result = ephemMultiObj(OrbEl, JD, 'INPOP',IN, 'Integration',true, 'MaxIterLT',1);
    % compare to JPL
    [T] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 
    [(Result.Catalog.Dec(1)-T.Dec(1)), (Result.Catalog.RA(1)-T.RA(1))].*3600

     
     
     
    
    % Topocentric position
%     Cat = ephem(OrbEl, JD, 'GeoPos',[35./RAD 30./RAD 415]);
%     [T] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','OBSERVER','TimeScale','TT',...
%                                                   'StartTime',JD(1), 'StopTime',JD(end), 'StepSize',TimeStep,...
%                                                   'GeoCoo',[35 50 0.415]);
%     Diff = [[Cat.Catalog.RA - T.RA], [Cat.Catalog.Dec - T.Dec]].*3600;
    
    %io.msgLog(LogLevel.Test, 'OrbitalEl test passed');
    Result = true;
end
