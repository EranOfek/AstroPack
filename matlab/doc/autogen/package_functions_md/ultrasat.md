# Package: ultrasat


### ultrasat.Cerenkov

Calculate the Cerenkov spectrum generated in a lens Package: telescope.Optics Description: Calculate the Cerenkov spectrum generated in a lens.


    
    Calculate the Cerenkov spectrum generated in a lens  
    Package: telescope.Optics  
    Description: Calculate the Cerenkov spectrum generated in a lens.  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'T' - Temperature [C]. Default is 20.  
    'Material' - Options are:  
    'si02_suprasil_2a' (fused silica) default.  
    'FluxOption' - Options:  
    DailyMin_MeanFlux:  
    DailyMax_MeanFlux:  
    DailyMin_95flux:  
    DailyMax_95flux:  - Default.  
    DailyMax_50flux:  
    DailyMax_75flux:  
    DailyMin_50flux:  
    DailyMin_75flux:  
    'Plot' - Default is false.  
    Output : - A structure containing the following fields:  
    'Lam' - Wavelength [Ang].  
    'Int' - Intensity of Cerenkov radaition generated in the lens.  
    Units: [count/cm^2/s/sr/micron]  
    'n'   - Refraction index at wavelength.  
    - A structure containing the following fields:  
    'E' - Electrons energy.  
    'F' - Electrons integrated flux with energy (>E)  
    [counts(>E)/cm^2/s].  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Res,ResEl]=ultrasat.Cerenkov  
      
      
### ultrasat.GEO_object_visibility

SHORT DESCRIPTION HERE Package: ultrasat Description:


    
    SHORT DESCRIPTION HERE  
    Package: ultrasat  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings  
    Output : -  
    License: GNU general public license version 3  
    By : Yossi Shvartzvald              Jan 2021  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: N1  = [220./RAD, 66./RAD];  
    S1  = [ 42./RAD,-66./RAD];  
    Coo = [N1;S1];  
    JD  = celestial.time.julday([1 1 2025 0]) + (0:0.1:365)';  
    GeoObjVis=ultrasat.GEO_object_visibility(JD,Coo);  
    Reliable:  
      
      
### ultrasat.ULTRASAT_restricted_visibility

SHORT DESCRIPTION HERE Package: ultrasat Description:


    
    SHORT DESCRIPTION HERE  
    Package: ultrasat  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Yossi Shvartzvald                    Jan 2021  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: N1  = [220./RAD, 66./RAD];  
    S1  = [ 42./RAD,-66./RAD];  
    Coo = [N1;S1];  
    JD  = celestial.time.julday([1 1 2025 0]) + (0:0.1:365)';  
    ULTRASAT_vis=ultrasat.ULTRASAT_restricted_visibility(JD,Coo);  
    Reliable:  
      
      
### ultrasat.dEdX_calc

Calculate dE/dX as a function of energy Package: ultrasat Description: Calculate the energy loss of electrons propagating in a material. This is used in order to estimate the Cernekov background.


    
    Calculate dE/dX as a function of energy  
    Package: ultrasat  
    Description: Calculate the energy loss of electrons propagating in a  
    material. This is used in order to estimate the Cernekov  
    background.  
    Input  : - Material: 'silica' | 'sapphire'. Default is 'silica'.  
    - Plot. Default is true.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : - Energy vector [MeV].  
    - dEdX as a function of energy [MeV/(g cm^-2)]  
    License: GNU general public license version 3  
    By : Yossi Shvartzvald               Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable: 2  
      
### ultrasat.event_rate

Calculate event rate for ULTRASAT given event and host abs. mag. Package: ultrasat Description: Calculate event rate for ULTRASAT given event and host abs. mag.


    
    Calculate event rate for ULTRASAT given event and host abs. mag.  
    Package: ultrasat  
    Description: Calculate event rate for ULTRASAT given event and host  
    abs. mag.  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    ''  
    Output : - Event rate per year in FoV.  
    - Redshift detection limit.  
    - Limiting magnitude as a function of z [z, LimMag].  
    - S/N structure for zLim/event rate.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Nov 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Rate,zLim,LimMag,SN]=ultrasat.event_rate  
    Reliable:  
      
      
### ultrasat.geostat_electrons_spec_flux

Return spectral model for electron flux in geostationary orbit Package: ultrasat Description: Return spectral model for electron flux in geostationary orbit.


    
    Return spectral model for electron flux in geostationary orbit  
    Package: ultrasat  
    Description: Return spectral model for electron flux in geostationary  
    orbit.  
    Input  :  * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Source' - Default is 'AE9' model.  
    'OutType' - 'AstCat' | 'mat'. Default is 'mat'.  
    Output : - Matrix or AstCat object containin spectra.  
    [counts(>E)/cm^2/s]  
    - ColCell  
    License: GNU general public license version 3  
    By : Yossi Shvartzvald               Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  [Out,ColCell]=ultrasat.geostat_electrons_spec_flux  
    [Out,ColCell]=ultrasat.geostat_electrons_spec_flux('OutType','astcat')  
    Reliable: 2  
      
      
      
      
### ultrasat.optic_comparison




    
      
      
      
### ultrasat.wget_goes_data

wget GOES partcles flux data Package: ultrasat Description:


    
    wget GOES partcles flux data  
    Package: ultrasat  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Out]=ultrasat.wget_goes_data;  
    Reliable:  
      
      
### ultrasat.zodiac_bck

Calculate the zodi calibrated spectrum and magnitude at a given position. Package: +ultrasat Description: Calculate the zodiacal calibrated spectrum and magnitude at a given sky position.


    
    Calculate the zodi calibrated spectrum and magnitude at a given position.  
    Package: +ultrasat  
    Description: Calculate the zodiacal calibrated spectrum and magnitude  
    at a given sky position.  
    Input  : - Vector of J2000.0 R.A., or longitude [radians].  
    - Vector of J2000.0 Dec, or latitude [radians].  
    - Date [D M Y F], or JD (column matrix). Default is empty.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Wave' - Vector of wavelength [Ang] in which to calculate  
    spectrum. If empty, then use original data.  
    Default is empty.  
    'OutType' - 'mat'|'astspec'. Default is 'mat'.  
    'CooSys'  - Input coordinate system. Default is 'j2000.0'.  
    Options include 'hec' - heliocentric ecliptic,  
    'e','g', etc.  
    'FilterFamily' - Default is 'SDSS'.  
    'FilterName'   - Default is 'g'.  
    'FilterSys'  - Default is 'AB'.  
    'InterpMethodSpec' - Default is 'linear'.  
    'InterpMethodCoo'  - Default is 'linear'.  
    Output : - A structure with the following fields:  
    'RA'  - Vector of RA/long corresponding to spectra.  
    'Dec' - Vector of Dec/latitude.  
    'JD'  - Vectoe of JD.  
    'Wave'- Vector of wavelength.  
    'Spec'- A matrix in which each column is the zodi spectrum  
    in units of erg/cm^2/s/Ang/arcsec^2.  
    Or a vector of AstSpec objects with the spectra.  
    'Mag' - Vector of magnitude in requested band.  
    'MagV'- Vector of Vega V-band magnitudes.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Oct 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Spec]=ultrasat.zodiac_bck(1,1,2451545);  
    [Spec]=ultrasat.zodiac_bck(1,1,2451545,'CooSys','g')  
    [Spec]=ultrasat.zodiac_bck(1,1,[],'CooSys','hec')  
    Reliable: 2  
      
      
      
      
### ultrasat.zodiac_bck_V

Get the Zodiac V light surface brightness as a function of coordinates Package: @AstSpec Description: Calculate the zodiac V vand Vega magnitude for a sky.


    
    Get the Zodiac V light surface brightness as a function of coordinates  
    Package: @AstSpec  
    Description: Calculate the zodiac V vand Vega magnitude for a sky.  
    Input  : - Either Ecliptic longitude or Helio-ecliptic longitude  
    (i.e., L - L_sun) in radians  
    (see convertdm.m for additional options).  
    If date is not given, this is assumed to be Helio-ecliptic  
    longitude.  
    - Ecliptic latitude [radians].  
    (see convertdm.m for additional options).  
    - Date [D M Y] or JD at which to calculate the Helio-ecliptic  
    longitude. Defaut is empty. If empty than treat the first  
    input argument as Helio-ecliptic longitude.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'InterpMethod' - Default is 'linear'.  
    'ReplaceNaNVal' - Value to replace NaNs. Default is 21.5.  
    Output : - Zodiacal light V-vabd Vega surface magnitude.  
    Tested : Matlab R2014a  
    By : Ilan Sagiv                      Sep 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: https://hst-docs.stsci.edu/display/WFC3IHB/9.7+Sky+Background#id-9.7SkyBackground-9.7.19.7.1ZodiacalLight,EarthShine,andLOW-SKY  
    Example: [ZodiVmag]=ultrasat.zodiac_bck_V(Long,Lat,Date)  
    Reliable: 2  
      
      
### ultrasat.zodiac_spectrum

Get the Zodiac light spectrum Package: @AstSpec Description: Return the zodiac spectrum as adopted from the HST STIS handbook. The high zodiacal ligh is defined where V=22.1 mag/arcsec^-2.


    
    Get the Zodiac light spectrum  
    Package: @AstSpec  
    Description: Return the zodiac spectrum as adopted from the HST STIS  
    handbook. The high zodiacal ligh is defined where V=22.1  
    mag/arcsec^-2.  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Wave' - Vector of wavelength [Ang] in which to calculate  
    spectrum. If empty, then use original data.  
    Default is empty.  
    'BackType' - Background type. Default is 'zodi'.  
    'OutType'  - 'mat'|'astspec'. Default is 'mat'.  
    'InterpMethod' - Default is 'linear'.  
    Output : - Zodiacal ligh spectrum  
    [wavelength(Ang), Flux(erg/cm^2/s/A/arcsec^2)]  
    Reference: https://hst-docs.stsci.edu/display/STISIHB/6.6+Tabular+Sky+Backgrounds  
    but there is a discrepency with:  
    http://www.stsci.edu/hst/wfc3/design/documents/handbooks/currentIHB/c09_exposuretime08.html#389841  
    According to the HST help desk the STIS table should be used.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Nov 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: S=ultrasat.zodiac_spectrum;  
    to verify normalization: synphot(Spec,'Johnson','V','Vega')  
    S=ultrasat.zodiac_spectrum;  
    Reliable: 1  
      
      
      
