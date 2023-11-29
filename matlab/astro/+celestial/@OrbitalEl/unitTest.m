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
        
    

    % same with heliocentric position
    % Not working!
    % Test by obtaining the Sun barycentric position
    OrbEl1 = celestial.OrbitalEl.loadSolarSystem('num',9804);
    [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(OrbEl1, JD+(0:1:10)','Integration',true, 'RefFrame','bary');
    RotM = celestial.coo.rotm_coo('e');
    U_B_ec = RotM * U_B;
    S_B_ec = RotM * S_B;
    [Th,~,U] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','VECTORS','TimeScale','TDB', 'StartTime',JD, 'StopTime',JD+10.1, 'StepSize',1, 'CENTER','500@10');
    if any(abs(Th.X - (U_B_ec(1,:)' - S_B_ec(1,:)'))>1e-6)
        error('Error in targetBaryPos');
    end
    
    %% Test: propagate2commonEpoch
    OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
    Result = propagate2commonEpoch(OrbEl);
    if numel(unique(Result.Epoch))~=1
        error('propagate2commonEpoch returned more than one epoch');
    end

    %% Test: integrateElements
    OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801]);   
    JD = OrbEl.Epoch + 1000;
    Result = integrateElements(OrbEl, JD, 'TolInt',1e-6);
    [T] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','ELEMENTS','TimeScale','TDB','StartTime',JD,'StopTime',JD+0.5, 'OutType','OrbitalEl');
    Resid = [Result.Node - T.Node, Result.W-T.W, Result.Incl-T.Incl, Result.A-T.A, Result.PeriDist-T.PeriDist, Result.Eccen-T.Eccen];
    if any(abs(Resid)>1e-4)
        error('Error in integrateElements');
    end
    
    %io.msgLog(LogLevel.Test, 'OrbitalEl test passed');
    Result = true;
end
