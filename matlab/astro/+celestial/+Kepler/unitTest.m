function Result = unitTest()
    % unitTest for celestial.Kepler
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    %
    OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9810]);
    [X,V]=OrbEl.elements2pos('CooSys','ec', 'RefFrame','helio');
    Res=celestial.Kepler.xyz2elements(X,V, OrbEl.Epoch);
    if any(abs([Res.A.' - OrbEl.A, Res.Eccen.' - OrbEl.Eccen, Res.PeriDist.' - OrbEl.PeriDist])>1e-11,'all')
        error('Error in xyz2elements');
    end
    if any(abs([Res.Incl.' - OrbEl.Incl, Res.W.' - OrbEl.W, Res.Node.' - OrbEl.Node])>1e-9,'all')
        error('Error in xyz2elements');
    end
    if any(abs([Res.MeanMotion.' - OrbEl.meanMotion])>1e-4,'all')
        error('Error in xyz2elements');
    end


	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end
