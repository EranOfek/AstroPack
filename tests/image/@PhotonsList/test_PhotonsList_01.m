function Result = unitTest()

	Result = true;
    
    
    % To get data use:
    % VO.Chandra.wget_obsid(366)
    
    % Load event file
    P=PhotonsList('acisf00366N006_evt2.fits');
    % alternatively:
    P=PhotonsList.readPhotonsList1('acisf00366N006_evt2.fits');
    
    % Populate the bad times property:
    P.populateBadTimes;

    % count number of photons
    % [total, in good times]
    [a,b] = P.nphotons

    % select photons in energy
    P.selectEnergy([200 8000]);
    [a,b] = P.nphotons
    
    % populate the image field, X/Y correspinds to image [default is sky coo]
    P.constructImage; 
    
    % Add RA/Dec to catalog
    P.addSkyCoo
    
    % populate the scalar background:
    P.background
    
    
end

	