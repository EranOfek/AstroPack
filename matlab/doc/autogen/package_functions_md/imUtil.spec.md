# Package: imUtil.spec


### imUtil.spec.applyAtmosphericExtinction

Apply extinction to spectrum


    
    Apply extinction to spectrum  
    Input  : - A vector of wavelength [Ang].  
    - A matrix of spectrum (wavelength axis shuld be along the same  
    dimension as the vector of wavelength).  
    * ...,key,val,...  
    'AirMass'   - Airmass of requested output spectrum.  
    Default is 0.  
    'InAirMass' - Airmass of reference extinction curve.  
    Default is 1.  
    'AtmosphericExt' - Either a two column matrix of  
    [Wave[Ang], Extinction[mag]], or a string  
    indicating the extinction curve to load from:  
    cats.spec.AtmoExtinction  
    Default is 'VLT'.  
    'InterpMethod' - Interpolation method. Default is 'linear'.  
    Output : - Matrix of spectrum after applying the extinction correction.  
    Author : Eran Ofek (Mar 2021)  
    Example: ExtMatrix = imUtil.spec.applyAtmosphericExtinction((4001:1:5000)', ones(1000,3))  
    ExtMatrix = imUtil.spec.applyAtmosphericExtinction((4001:100:5000)', ones(10,3),'AirMass',2)  
      
### imUtil.spec.trace_spec

UNDER CONSTRUCTION


    
    UNDER CONSTRUCTION  
      
