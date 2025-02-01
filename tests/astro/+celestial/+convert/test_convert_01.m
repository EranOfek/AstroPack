function [Result] = unitTest()
    % unitTest for celestial.convert
    

    %%
    % J2000.0 to true equinox of today date [deg]
    [OutRA1, OutDec1] = celestial.convert.precessCoo(180,-20);
    % same with sexagesimal coo:
    [OutRA2, OutDec2] = celestial.convert.precessCoo('12:00:00','-20:00:00',[],'OutMean',false);
    
    if abs(OutRA1-OutRA2)>1e-10 || abs(OutDec1-OutDec2)>1e-10
        error('Error in celestial.convert.precessCoo');
    end
    
    % J2000.0 to mean equinox of 2050 [input: sex, output: deg]
    [OutRA, OutDec] = celestial.convert.precessCoo('12:00:00','-20:00:00',[],'OutMean',true);
    
    % J2000.0 to mean equinox of 2050 [input: sex, output: deg]
    [OutRA, OutDec] = celestial.convert.precessCoo('12:00:00','+00:00:00',[],'OutMean',true, 'OutType','J', 'OutEquinox',2024);

    AcRA  = 180.307519;
    AcDec = -0.133613;
    if abs(OutRA - AcRA)>4e-5 || abs(OutDec - AcDec)>4e-5
        OutRA - AcRA
        OutDec - AcDec
       error('Error in celestial.convert.precessCoo');
    end 
    
    
    %%
    RA  = celestial.coo.convertdms('02:31:48.704','gH','d');
    Dec = celestial.coo.convertdms('+89:15:50.72','gD','d');
    
    [OutRA0, OutDec0] = celestial.convert.j2000_toApparent(RA, Dec, celestial.time.julday([1 1 2100]),'ApplyAberration',false,...
                      'OutMean',true,'PM_RA',44.48,'PM_Dec',-11.85,'ApplyRefraction',false);
    celestial.coo.convertdms(OutRA0, 'd','SH')
    celestial.coo.convertdms(OutDec0, 'd','SD')
    
    

    
    
    Result = true;
end
