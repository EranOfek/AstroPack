% Unit-Test for celestial.SolarSys
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Unit-Test for celestial.SolarSys
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    %func_unitTest();
    
    % JPL horizons access
    [T,~,U] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','VECTORS','TimeScale','TT');  
    [T,~,U] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','ELEMENTS','TimeScale','TDB');
    [T,~,U] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','OBSERVER','TimeScale','TT'); 

    [RA,Dec,R,SL,EquationTime]=celestial.SolarSys.suncoo(2451545+[0:1:10]','j');
    
    [RA,Dec,HP]=celestial.SolarSys.mooncool(2451545+(0:1:10)',[1 1]);
    
    S = celestial.SolarSys.ple_xyzAll(2451545+(1:1:10));
    
    % test orbital integration w/JPL
    JD = 2460000;
    % ecliptic coordinates
    [T1] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','VECTORS','TimeScale','TT', 'StartTime',JD, 'StopTime',JD+0.5, 'CENTER','500@0');  
    [T2] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','VECTORS','TimeScale','TT', 'StartTime',JD+500, 'StopTime',JD+500+0.5, 'CENTER','500@0');  
    
    % convert to equatorial
    RotM = celestial.coo.rotm_coo('E');
    X1   = RotM * [T1.X;T1.Y;T1.Z];
    V1   = RotM * [T1.VX;T1.VY;T1.VZ];
    X2   = RotM * [T2.X;T2.Y;T2.Z];
    V2   = RotM * [T2.VX;T2.VY;T2.VZ];
    
    % integrate in equatorial coo
    IN = celestial.INPOP;
    IN.populateAll;
    [X,V] = celestial.SolarSys.orbitIntegration([JD JD+500], X1, V1, 'INPOP',IN);

    if any(abs([X-X2])>1e-5) || any(abs([V-V2])>1e-7)
        error('Error in orbital integration');
    end
    
    % Test the orbotal elements calculated from the orbital integration
    % convert barycentric equatorial to heliocentric equatorial
        
    S_B    = IN.getPos('Sun', JD+500);
    S_Bdot = IN.getVel('Sun', JD+500);
    X_H    = X - S_B;
    V_H    = V - S_Bdot;
    
    % convert X/V to ecliptic coordinates
    RotM = celestial.coo.rotm_coo('e');
    Xe    = RotM * X_H;
    Ve    = RotM * V_H;
    [Res,EE]=celestial.Kepler.xyz2elements(Xe,Ve, JD+500);
    [EL] = celestial.SolarSys.getJPL_ephem('299;','EPHEM_TYPE','ELEMENTS','TimeScale','TDB', 'StartTime',JD+500, 'StopTime',JD+500.5);
    
    if abs(EE.Node-EL.OM)>1e-6 || abs(EE.W-EL.W)>1e-3 || abs(EE.Eccen-EL.EC)>1e-8 || abs(EE.A-EL.A)>1e-5
        error('Error in xyz2elements or orbital integration');
    end
    
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	%io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

