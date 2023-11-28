function Result = unitTest()
    % unitTest for celestial.ephem
    % Example: R=celestial.ephem.unitTest();

    % load INPOP
    IN = celestial.INPOP;
    try
        IN.populateAll;
        InpopExist = true;
    catch
        InpopExist = false;
    end


    if InpopExist
        % run tests that requires celestial.INPOP


        %% Test: celestial.ephem.ephemMultiObj
        OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9900]);
        JD = OrbEl.Epoch(1)+1000;  % integrate to epoch+1000 days
        Result = celestial.ephem.ephemMultiObj(OrbEl, JD, 'INPOP',IN);
        % compare to JPL
        [T1] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 
        [T2] = celestial.SolarSys.getJPL_ephem('9802;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 
        Resid1 = [(Result.Catalog.Dec(1)-T1.Dec(1)), (Result.Catalog.RA(1)-T1.RA(1))].*3600;
        Resid2 = [(Result.Catalog.Dec(2)-T2.Dec(1)), (Result.Catalog.RA(2)-T2.RA(1))].*3600;
        if any(abs(Resid1)>0.1,'all') || any(abs(Resid2)>0.1,'all')
            error('Error in orbital integration - celestial.ephem.ephemMultiObj - residuals larger than 0.1 arcsec');
        end

        %% Test: celestial.ephem.ephemKeplerMultiObj
        OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
        JD = OrbEl.Epoch(1); % should be accurate only on epoch
        Result = celestial.ephem.ephemKeplerMultiObj(OrbEl, JD, 'INPOP',IN);
        [T] = celestial.SolarSys.getJPL_ephem('1;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 
        Resid1 = (Result.Catalog(1,:).RA - T.RA).*3600;
        Resid2 = (Result.Catalog(1,:).Dec - T.Dec).*3600;
        if any(abs(Resid1)>0.1,'all') || any(abs(Resid2)>0.1,'all')
            error('Error in kepler equation based ephemeris - celestial.ephem.ephemKeplerMultiObj - residuals larger than 0.1 arcsec');
        end
        

        %% Test: celestial.ephem.ephemKeplerMultiTime
        OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804);
        JD = OrbEl.Epoch;
        Result = celestial.ephem.ephemKeplerMultiTime(OrbEl, JD+(0:1:10)', 'INPOP',IN);
        [T] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+10.5); 
        Resid = [(Result.Catalog.Dec-T.Dec), (Result.Catalog.RA-T.RA)].*3600;
        if any(abs(Resid)>0.1,'all')
            error('Error in kepler equation based ephemeris - celestial.ephem.ephemKeplerMultiTime - residuals larger than 0.1 arcsec');
        end

        %% Test: celestial.ephem.ephemKepler
        OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804);
        JD = OrbEl.Epoch;
        Result = celestial.ephem.ephemKepler(OrbEl, JD+(0:1:10)');
        [T] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+10.5); 
        Resid = [(Result.Catalog.Dec-T.Dec), (Result.Catalog.RA-T.RA)].*3600;
        if any(abs(Resid)>0.1,'all')
            error('Error in kepler equation based ephemeris - celestial.ephem.ephemKeplerMultiTime - residuals larger than 0.1 arcsec');
        end

        OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
        JD = OrbEl.Epoch(1);
        Result = celestial.ephem.ephemKepler(OrbEl, JD);
        [T] = celestial.SolarSys.getJPL_ephem('1;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 
        Resid1 = (Result.Catalog(1,:).RA - T.RA).*3600;
        Resid2 = (Result.Catalog(1,:).Dec - T.Dec).*3600;
        if any(abs(Resid1)>0.1,'all') || any(abs(Resid2)>0.1,'all')
            error('Error in kepler equation based ephemeris - celestial.ephem.ephemKeplerMultiObj - residuals larger than 0.1 arcsec');
        end


        %%

        
    end



    Result = true;
end