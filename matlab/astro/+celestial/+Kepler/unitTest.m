function Result = unitTest()
    % unitTest for celestial.Kepler
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    % Test xyz2elements
    RAD = 180./pi;
    OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801]);
    OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9810]);
    [X,V]=OrbEl.elements2pos('CooSys','ec', 'RefFrame','helio');
    [Res,EE]=celestial.Kepler.xyz2elements(X,V, OrbEl.Epoch);
    if any(abs([Res.A.' - OrbEl.A, Res.Eccen.' - OrbEl.Eccen, Res.PeriDist.' - OrbEl.PeriDist])>1e-11,'all')
        error('Error in xyz2elements');
    end
    if any(abs([Res.Incl.' - OrbEl.Incl, Res.W.' - OrbEl.W, Res.Node.' - OrbEl.Node])>1e-9,'all')
        error('Error in xyz2elements');
    end
    if any(abs([Res.MeanMotion.' - OrbEl.meanMotion.*RAD])>1e-4,'all')
        error('Error in xyz2elements');
    end
    
    % Test xyz2elements against JPL
     JD = 2461300.5;
     [OrbEl_J] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','ELEMENTS','TimeScale','TDB','StartTime',JD,'StopTime',JD+0.5, 'OutType','OrbitalEl');
     [T] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','VECTORS','TimeScale','TDB','StartTime',JD,'StopTime',JD+0.5, 'CENTER','500@10');
     X = [T.X; T.Y; T.Z];
     V = [T.VX; T.VY; T.VZ];
     [~,EE]=celestial.Kepler.xyz2elements(X,V, JD);
     if abs(EE.Incl - OrbEl_J.Incl)>1e-10 || abs(EE.A - OrbEl_J.A)>1e-10 || abs(EE.Eccen - OrbEl_J.Eccen)>1e-10
         error('Error in xyz2elements');
     end
    

	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end
