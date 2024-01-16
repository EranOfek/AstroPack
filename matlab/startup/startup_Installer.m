function startup_Installer
    % This function may be run once after the instellation of AstroPack.
    % Will install additional data packages:
    % 

    I = Installer;
    %I.install('VSOPE87');
    %I.install('GAIA_SpecTemplate');
    %I.install('CALSPEC');
    I.install('Atmosphere');
    
    % updates required
    I.install('Time');
    I.install('MinorPlanets');
    I.install('MinorPlanetsCT');
    I.install('INPOP');
    
end
