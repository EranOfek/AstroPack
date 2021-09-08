# Package: astro.grb


### astro.grb.lorentz_from_flux

Lower limit on Lorentz factor GRB Package: AstroUtil.GRB Description: Given a GRB flux, estimate a lower limit on the Lorentz factor og a GRB, assuming a power law spectrum.


    
    Lower limit on Lorentz factor GRB  
    Package: AstroUtil.GRB  
    Description: Given a GRB flux, estimate a lower limit on the Lorentz  
    factor og a GRB, assuming a power law spectrum.  
    Input  : - Flux [erg/cm^2/s]  
    - DeltaT [s]  
    - Redshift  
    - Luminosity Dist [pc] (if empty use redshift).  
    - E_min [keV]  
    - E_max [keV]  
    - Alpha (spectral index).  
    Output : - Lower limit on lorentz factor assuming photon can  
    escape without annihilating other photons.  
    (case A in Lithwick & Sari).  
    - Lower limit on lorentz factor assuming e+ e- pairs  
    produced by photin annihilation are optically thin  
    (case B in Lithwick & Sari).  
    Reference : Lithwick & Sari 2001 ApJ 555, 540L  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Aug 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
