function Result = unitTest()
    % OrbitalEl.unitTest
    
    io.msgLog(LogLevel.Test, 'OrbitalEl test started');

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
    CatE = ephem(OrbEl, JD, 'GeoPos',[],'MaxIterLT',0,'IncludeMag',false);


    % compare to JPL
    TTmUTC = 70./86400;
    JD = celestial.time.julday([19 9 2021])+(0:1./24:5)';  
    Coo=[-116.865./RAD 33.3563./RAD 2000];
    OrbEl1 = celestial.OrbitalEl.loadSolarSystem([],9804);
    CatE = ephem(OrbEl1, JD + TTmUTC, 'GeoPos',Coo, 'OutUnitsDeg',false);
    [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',JD,'StopJD',JD+5,'StepSizeUnits','h','CENTER','675');
    
    % RA nd Dec diff between JPL and ephem:
    [CatE.Catalog.RA - CatJPL.Catalog(:,2), CatE.Catalog.Dec - CatJPL.Catalog(:,3)].*RAD.*3600
    
    TTmUTC = 70./86400;
    JD = celestial.time.julday([19 9 2010])+(0:30:6000)';  
    Coo=[-116.865./RAD 33.3563./RAD 2000];
    OrbEl1 = celestial.OrbitalEl.loadSolarSystem([],9804);
    CatE = ephem(OrbEl1, JD + TTmUTC, 'GeoPos',Coo, 'OutUnitsDeg',false);
    [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',JD,'StopJD',JD+6000,'StepSize',30,'StepSizeUnits','d','CENTER','675');
    
    % RA nd Dec diff between JPL and ephem:
    [CatE.Catalog.RA - CatJPL.Catalog(:,2), CatE.Catalog.Dec - CatJPL.Catalog(:,3)].*RAD.*3600
        
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

    assert(all(abs(1-Cat.Catalog{:,{'RA','Dec','R'}}./Cat2.Catalog{:,{'RA','Dec','R'}})<1e-3))

    Cat = ephem(OrbEl, JD,'Integration',true);
    Cat2  = ephem(OrbEl, JD,'Integration',true,'EarthEphem','inpop');
    assert(all(abs(1-Cat.Catalog{:,{'RA','Dec','R'}}./Cat2.Catalog{:,{'RA','Dec','R'}})<1e-3))

    
    io.msgLog(LogLevel.Test, 'OrbitalEl test passed');
    Result = true;
end
