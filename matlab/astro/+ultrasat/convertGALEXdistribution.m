function convertGALEXdistribution 
    % makes a .mat object containing ULTRASAT magnitudes 
    % for a set of source spectra and radii (on the detector) for a range
    % of GALEX NUX magnitudes 
    
    Wave    = 2000:11000;          % grid points in wavelength [A] 
    
    Ntemp   = 3;
    Temp    = [3500 5800 20000];   % grid points in temperature [K] 
    
    Nrad    = 25; 
    Rad     = linspace(0,10,Nrad); % grid points in radius [deg]   
    
    MagL = 13; MagH = 26; Delta_m = 0.2; % the distribution grid in Mag (GALEX NUV)
    Nmag = (MagH - MagL) / Delta_m; 
    
    MagNUV  = zeros(Nmag,1);
    MagU    = zeros(Ntemp, Nmag, Nrad);
    
    for iTemp = 1:Ntemp
        
        Spec = AstroSpec.blackBody(Wave', Temp(iTemp) ); % make a BB spectrum
                
        for iMag = 1:Nmag  % for each NUV magnitude and radial distance calculate the appropriate ULTRASAT magnitude 
            
            MagNUV(iMag) = MagL + (iMag-1) * Delta_m; 
            
            for iRad = 1:Nrad               
                RXX        = sprintf('R%d',iRad);
                Sp         = scaleSynphot(Spec, MagNUV(iMag), 'GALEX', 'NUV');
                MagU(iTemp, iMag, iRad) = astro.spec.synthetic_phot([Wave', Sp.Flux],'ULTRASAT',RXX,'AB');                
            end
            
        end
        
    end
    
    save('~/magu.mat','MagU', 'Temp', 'MagNUV', 'Rad');

end