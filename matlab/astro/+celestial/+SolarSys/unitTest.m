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

