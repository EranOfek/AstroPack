# Package: VO.NIST


### VO.NIST.get_scattering_cross_section

Get the attenuation and scattering cross-sections from the NIST website. Package: VO.NIST Description: Get the attenuation and scattering cross-sections from the NIST website. Calculate opacity and cross section for


    
    Get the attenuation and scattering cross-sections from the NIST website.  
    Package: VO.NIST  
    Description: Get the attenuation and scattering cross-sections from the  
    NIST website. Calculate opacity and cross section for  
    bound-free absorption.  
    Input  : - Atomic number (Z).  
    Output : - Structure with the following vectors.  
    .E - Energy [keV] (in the range ~0.01 to 430 keV).  
    .Kappa - Opacity [cm^2 g^-1]  
    .Sigma - Cross section per atom [cm^-2].  
    Reference: https://physics.nist.gov/PhysRefData/FFast/html/form.html  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data=VO.NIST.get_scattering_cross_section(1)  
    Reliable: 2  
      
      
      
