# Package: VO.HST


### VO.HST.hst_acs_zp_apcorr

Aperture correction for HST/ACS filter and aperture radius. Package: VO.HST Description: Given aperture radius for photometry, and Hubble Space Telecsope (HST) ACS-WFC filter name, return the fraction,


    
    Aperture correction for HST/ACS filter and aperture radius.  
    Package: VO.HST  
    Description: Given aperture radius for photometry, and Hubble Space  
    Telecsope (HST) ACS-WFC filter name, return the fraction,  
    and error in fraction, of the encircled energy within the  
    aperture.  
    Input  : - Aperture radius [arcseconds].  
    - HST/ACS/WFC filter name  
    Output : - Fraction of energy encircled within aperture.  
    - Error in the fraction of energy encircled within aperture.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Feb 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Sirianni et al. (2005)  
    Example: [Energy,EnergyErr]=VO.HST.hst_acs_zp_apcorr(0.3,'F475W');  
    Reliable: 2  
      
      
      
    Encircled Energy -  HST/ACS/WFC  (Sirianni et al. 2005, PASP, 117, 1049)  
    See: hst_acs_zp_apcorr.m utility function  
      
    Aper     F435W         F475W           F555W           F606W          F625W           F775W             F814W           F850LP          F892N  
    Radius  
    ["]  
