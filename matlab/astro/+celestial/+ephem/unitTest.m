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
        JD = 2461000;
        Result = celestial.ephem.ephemMultiObj(OrbEl, JD, 'INPOP',IN);
        % compare to JPL
        [T1] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 
        [T2] = celestial.SolarSys.getJPL_ephem('9802;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+0.5); 
        Resid1 = [(Result.Catalog.Dec(1)-T1.Dec(1)), (Result.Catalog.RA(1)-T1.RA(1))].*3600;
        Resid2 = [(Result.Catalog.Dec(2)-T2.Dec(1)), (Result.Catalog.RA(2)-T2.RA(1))].*3600;
        if any(abs(Resid1)>0.1,'all') || any(abs(Resid2)>0.1,'all')
            error('Error in orbital integration - celestial.ephem.ephemMultiObj - residuals larger than 0.1 arcsec');
        end

        %%


    end



    Result = true;
end