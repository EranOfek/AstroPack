# Package: astro.spec


### astro.spec.accretion_disk

theoretical spectrum of a optically thick, thin accretion disk Package: AstroUtil.spec Description: Calculate the theoretical spectrum of a optically thick, thin accretion disk given the mass of


    
    theoretical spectrum of a optically thick, thin accretion disk  
    Package: AstroUtil.spec  
    Description: Calculate the theoretical spectrum of a optically thick,  
    thin accretion disk given the mass of  
    the central object, disk radius, and the accretion rate.  
    Input  : - Mass of central object [Solar Mass].  
    - Accreation rate (Mdot) [Solar Mass per Year].  
    - Inner cutof radius of accretion disk [R_{s}],  
    default is 10.  
    If empty the use default.  
    - Outer cutof radius of accretion disk [R_{s}],  
    default is 1000.  
    If empty the use default.  
    - Wavelength in which to calculate Flux [Ang], default is  
    logspace(0,4,1000)  
    If empty the use default.  
    - Number of steps in accretion disk radius, default is 1000.  
    If empty the use default.  
    Output : - Total emittance [erg/sec/cm^2/cm(lambda)] from accretion  
    disk as function of wavelength, [Wavelengh[Ang], Lum].  
    - Vector of radii in the accretion disk [cm].  
    - Vector of temperature corresponding to eacg radius [K].  
    - The R_s radius of the central mass.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Spec,VecR,VecT,Rs]=AstroUtil.spec.accretion_disk(1e6,0.1,10,100);  
    Reliable: 2  
      
### astro.spec.accretion_disk_mag_c

Optically thick, thin accretion disk magnitudes Package: astro.spec Description: Calculate the magnitude, in a given filter, of a optically-thick thin accretion disk model.


    
    Optically thick, thin accretion disk magnitudes  
    Package: astro.spec  
    Description: Calculate the magnitude, in a given filter, of a  
    optically-thick thin accretion disk model.  
    Input  : - Matrix of accretion disk parameters:  
    [M Mdot Rin Rout]. See accretion_disk.m for details.  
    - Filter system, see get_filter.m for details,  
    - Filter name, see get_filter.m for details,  
    - Magnitude system, 'AB' or 'Vega'.  
    - Distance [pc], default is 10pc.  
    Output : - Black body observed magnitude.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                       Feb 2007  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Mag=astro.spec.accretion_disk_mag_c([1 1e-8 5 100],'SDSS','u','AB',10);  
    Reliable: 2  
      
### astro.spec.band_spectrum

Band psectrum Package: astro.spec Description: Calculate an un-normalized Band-spectrum (Band et al. 1993).


    
    Band psectrum  
    Package: astro.spec  
    Description: Calculate an un-normalized Band-spectrum (Band et al. 1993).  
    Input  : - Vector of Energy [keV].  
    - E0 [keV].  
    - Alpha, default is -1.  
    - Beta, defaukt is -2.  
    Output : - Number of photons (unnormalized) per cm^2/s/keV  
    To normalize spectrum, multiply by a normalization factor.  
    - Energy [keV] (unnormalized) per cm^2/s/keV  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Feb 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### astro.spec.black_body

Black body spectrum Package: astro.spec Description: Black body spectrum. OBSOLETE: Use AstSpec.blackbody instead.


    
    Black body spectrum  
    Package: astro.spec  
    Description: Black body spectrum.  
    OBSOLETE: Use AstSpec.blackbody instead.  
    Input  : - Temperature [K].  
    - Vector of wavelength [Ang].  
    - Calculation type:  
    'P'  - planck formula, default.  
    'RJ' - Rayleigh-Jeans approximation  
    'W'  - Wein spectrum.  
    Output : - Emittance [erg/sec/cm^2/cm(lambda)]  
    - Emittance [erg/sec/cm^2/Hz]  
    - Emittance [erg/sec/cm^2/Ang(lambda)]  
    - Emittance [mJy] (i.e., (erg/sec/cm^2/Hz)/1e-26)  
    - Number of photons [photons/sec/cm^2/Ang(lambda)]  
    See also: AstSpec.blackbody  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2003  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  [Il,In,IlA,ImJy,Ip]=astro.spec.black_body(10000,logspace(1,4,100).');  
    Reliable: 1  
      
### astro.spec.blackbody_bolmag

Bolometric magnitude of black body spectrum Package: astro.spec Description: Calculate the bolometric magnitude of a black body spectrum, given its temperature, radius and distance.


    
    Bolometric magnitude of black body spectrum  
    Package: astro.spec  
    Description: Calculate the bolometric magnitude of a black body spectrum,  
    given its temperature, radius and distance.  
    Input  : - Vector of black body temperature [K].  
    - Black body radius [cm], default is 1cm.  
    - Distance [pc], default is 10pc.  
    Output : - Black body bolometric magnitude.  
    Normalized such m_bol=0 = 2.48e-5 erg cm^-2 s^-1.  
    Sun bol lum = 3.845e33 erg s^-1 = bol mag +4.74.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Feb 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See Also: blackbody_mag_c.m  
    Example: BolMag=AstroUtil.spec.blackbody_bolmag([5700;6000],696000e5,10);  
    Reliable: astro  
      
### astro.spec.blackbody_flux

Flux of blackbody in some wavelength range Package: AstroUtil.spec Description: Calculate the flux, in a given wavelength range, of a black-body, given its temperature, radius and distance.


    
    Flux of blackbody in some wavelength range  
    Package: AstroUtil.spec  
    Description: Calculate the flux, in a given wavelength range, of a  
    black-body, given its temperature, radius and distance.  
    Input  : - Vector of black body temperature [K].  
    - Wavelength range [Min Max] in Ang.  
    Use convert.energy.m to convert from other units.  
    - Black body radius [cm], default is 1cm.  
    - Distance [pc], default is 10pc.  
    Output : - Black body observed flux [erg cm^-2 s^-1].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                       Feb 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See Also: blackbody_mag.m, blackbody_bolmag.m  
    Reliable: 2  
      
### astro.spec.blackbody_mag_c

Blackbody magnitude Package: astro.spec Description: Calculate the magnitude, in a given filter, of a black-body, given its temperature, radius and distance.


    
    Blackbody magnitude  
    Package: astro.spec  
    Description: Calculate the magnitude, in a given filter, of a black-body,  
    given its temperature, radius and distance.  
    Input  : - Vector of black body temperature [K].  
    - Filter system, see get_filter.m for details.  
    Alternatively, this can be a filter transmission curve  
    [Wave, Trans].  
    - Filter name, see get_filter.m for details.  
    - Magnitude system, 'AB' or 'Vega'.  
    - Black body radius [cm], default is 1cm.  
    - Distance [pc], default is 10pc.  
    - Eb-v - Apply extinction with E_{B-V}. Default is 0.  
    Output : - Black body observed magnitude.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Feb 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See Also: blackbody_mag.m, blackbody_bolmag.m  
    Example : [Mag]=astro.spec.blackbody_mag_c([5770;8000],'Johnson','V','Vega',696000e5);  
    Reliable: 1  
      
### astro.spec.brightness_temp

Brightness temperature Package: AstroUtil.spec Description: Calculate the brightness temperature.


    
    Brightness temperature  
    Package: AstroUtil.spec  
    Description: Calculate the brightness temperature.  
    Input  : - Specific flux [erg cm^-2 s^-1 Hz^-1]  
    (note that 1mJy = 1e-26 ergs cm^-2 s^-1 Hz^-1).  
    - Frequency [Hz].  
    - Luminosity distance [pc].  
    - Source radius [cm].  
    Output : - Brightness temperature [K].  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Nov 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: T=AstroUtil.spec.brightness_temp(1e-26,1.4e9,10,1e6);  
    Reliable: 1  
      
      
### astro.spec.calibrate_spec_using_phot

calibrate_spec_using_phot function                             AstroSpec Description: Given a spectrum and a light curve in one or more bands, applay synthetic photometry to the spectrum to calibrate


    
      
    calibrate_spec_using_phot function                             AstroSpec  
    Description: Given a spectrum and a light curve in one or more bands,  
    applay synthetic photometry to the spectrum to calibrate  
    it against the light curves. Optionally, if RA, Dec are  
    provided, dereddned the spectrum for Galactic extinction.  
    Furthermore, if redshift is provided then the spectrum  
    will be converted to the rest frame, and if additional  
    host extinction is given then another deredenning at the  
    rest frame wavelength will be applied.  
    If more than one filter is provided than the spectrum  
    calibration will be performed using a polynomial fitting  
    when the deg of the polynom is N-1, where N is the good  
    filters.  
    Input  : - Spectrum [Wave(Ang), Flux(per unit wavelength)]  
    Alternatively this can be a file name containing the  
    spectrum name.  
    - Date [Day Month Year Hour Min Sec] or JD at which the  
    spectrum was obtained.  
    - Calibrated light curve of the source [JD Mag].  
    Optionally if a cell array then each cell contains the  
    light curve in a different band.  
    If a string or a cell array of strings is provided then  
    the program will attempt to use the read_mark_lc.m program  
    to read the file into a light curve.  
    - String of filter family (see AstFilter.get.m for options).  
    e.g., 'PTF', 'SDSS'.  
    If the light curve input argument is a cell array then  
    this a cell array of filter families should be provided.  
    Alternatively, this can be an AstFilter class object.  
    - String or cell array of filter names (e.g., 'R').  
    - String or cell array of the magnitude system for the  
    light curve (i.e., 'AB','Vega').  
    * Arbitrrary number of pairs of input arguments: ...,key,val,...  
    The following keywords are available:  
    'RA'    - J2000.0 Right Ascension [radians] of the source.  
    Default is empty matrix.  
    If empty matrix then Galactic extinction is not  
    applied, unless the 'Ebv' keyword is provided.  
    'Dec'   - J2000.0 Declination [radians] of the source.  
    Default is empty matrix.  
    If empty matrix then Galactic extinction is not  
    applied, unless the 'Ebv' keyword is provided.  
    'Ebv'   - E_{B-V} [mag] at the source position.  
    Default is 0, unless 'RA' and 'Dec' are provided.  
    'z'     - Redshift of the source. Default is 0.  
    'Ebv_z' - Additional E_{B-V} extinction at the redshift of  
    the source to applay to the rest frame spectrum.  
    Default is 0.  
    'Rv'    - Galactic R_{V}. Default is 3.08.  
    'Rv_z'  - Host galaxy R_{V}. Default is 3.08.  
    'MaxFlag'- maximum fraction of filter outside spectrum range.  
    Default is 0.05.  
    'PolyDeg'- Degree of polynomial to fit to the syn. phot.  
    minus interpolated phot. Default is number of  
    good filters - 1.  
    'InterpMethod' - Light curve interpolation method.  
    See interp1.m for options. Default is 'linear'.  
    Output : - Output calibrated observed frame spectrum  
    [Wave(Ang), Flux(erg/cm^2/s/Ang)]  
    i.e., only syn. phot. calibration is applied.  
    - Output calibrated spectrum with all requested corrections  
    applied (rest frame).  
    - Structure of various parameters.  
    Tested : Matlab R2013A  
    By : Eran O. Ofek                   July 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [ObsRestSpec,CorrSpec,Par]=calibrate_spec_using_phot(Spec,...  
    [5 6 2013 0.5],LC,'PTF','R','AB');  
    [ObsRestSpec,CorrSpec,Par]=calibrate_spec_using_phot(Spec,...  
    2461545.3,'PTF10cwx.out_PTF48R','PTF','R','AB');  
    [ObsRestSpec,CorrSpec,Par]=calibrate_spec_using_phot(Spec,...  
    2461545.3,'PTF10cwx.out_PTF48R','PTF','R','AB',...  
    'z',0.8,'RA',1,'Dec',0.2,'Rv_z',2.6);  
    Reliable: 2  
      
      
### astro.spec.cat_fit_template2phot

SHORT DESCRIPTION HERE Package: AstroUtil Description:


    
    SHORT DESCRIPTION HERE  
    Package: AstroUtil  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=AstroUtil.spec.cat_fit_template2phot(S)  
    Reliable:  
      
      
### astro.spec.chi2_bb_photometry

Given photometric observations calculate \chi^2 for BB with a given T. Package: astro.spec Description: Given photometric observations calculate \chi^2 for a black body with a given effective temperature and angular


    
    Given photometric observations calculate \chi^2 for BB with a given T.  
    Package: astro.spec  
    Description: Given photometric observations calculate \chi^2 for  
    a black body with a given effective temperature and angular  
    radius.  
    Input  : - [T(K),AngRad(radians)].  
    - A structure array with photometry. An element per band, with  
    the following fields:  
    .Family  
    .Band   - Band name.  
    .System - System name.  
    .Mag    - Magnitude.  
    .MagErr - Magnitude error.  
    - Eb-v [mag].  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Oct 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Chi2,Dof,AngRad,AngRadErr]=astro.spec.chi2_bb_photometry([11000 1],tRes)  
    Reliable:  
      
      
      
    Phot contains:  
    Phot().Band  
    Phot().System  
    Phot().Mag  
    Phot().MagErr  
      
      
      
### astro.spec.color2temp

Usage: temp = color2temp(color, filter1, filter2, filter_system='GAIA', mag_system='AB') Use the difference between two color magnitudes to estimate the temperature, using Eran's AstroUtil.spec.blackbody_mag_c.


    
    Usage: temp = color2temp(color, filter1, filter2, filter_system='GAIA', mag_system='AB')  
    Use the difference between two color magnitudes to estimate the  
    temperature, using Eran's AstroUtil.spec.blackbody_mag_c.  
      
    This function tries to reverse the blackbody by minimizing the difference  
    between the given color term and the one you get from blackbody_mag_c.  
      
    Inputs: -The color magnitude difference, e.g., B-V  
    -The first filter (in the above example, B).  
    -The second filter (in the above example, V).  
    -Filter system (see get_filter.m for details). Default is GAIA.  
    -The magnitude system used for the input color. Default is AB.  
      
    Output: the temperature that most closely resembles the color given.  
      
    Example : temp = color2temp(1.3, 'B', 'V', 'GAIA', 'AB');  
      
    Written by: Guy Nir 31/10/2019  
      
### astro.spec.conv_vargauss

Convolution with a Gaussian with a wavelngth dependent width Package: AstroUtil Description: Convolution with a Gaussian with a wavelngth dependent width.


    
    Convolution with a Gaussian with a wavelngth dependent width  
    Package: AstroUtil  
    Description: Convolution with a Gaussian with a wavelngth dependent  
    width.  
    Input  : - A spectrum [Wavelength, Intensity]  
    - A resolution vector, with the same length of the spectrum.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'ResolutionType' - 'lambda' | 'R'. Default is 'R'.  
    'InterpMethod'   - Default is 'cubic'  
    'FilterSizeMaxSigma' - Default is 3.  
    Output : - Convolved spectrum.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Conv]=AstroUtil.spec.conv_vargauss  
    Reliable:  
      
      
      
      
### astro.spec.eq_sampling

Resample two [X,Y] lists to have the same sampling (x). Package: astro.spec Description: Given two lists, each contains [X,Y], equalize the sampling frequency of the two lists by interpolating both lists to


    
    Resample two [X,Y] lists to have the same sampling (x).  
    Package: astro.spec  
    Description: Given two lists, each contains [X,Y], equalize the sampling  
    frequency of the two lists by interpolating both lists to  
    at a specified X.  
    Input  : - First list, in which the first column is the independent  
    varaible, and the second column is the value for each point.  
    If this is a cell array, then the first cell contains  
    a column vector of the independent variable, and  
    the second cell contain a matrix of the dependt variable,  
    and the interpolation is preformed for each column separately.  
    - Second list, (like the first list).  
    - New sampling rate, if a scalar is then taken as a constant  
    sampling rate. In case that a vector is given, it is taken as  
    the new independent variable. In case that empty matrix ([])  
    is given then the minimum sampling rate of the two lists  
    is taken. (default).  
    - Interpolation method:  
    'linear'  - linear interpolation (default).  
    'nearest' - nearest neighbor interpolation  
    'spline'  - cubic spline interpolation  
    'cubic'   - cubic interpolation  
    Output : - The first list with the new sampling.  
    If the input was a cell arry then the output is a cell  
    array too.  
    - The second list with the new sampling.  
    If the input was a cell arry then the output is a cell  
    array too.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### astro.spec.eq_temp

Eqilibrium temperature of a body illuminated by a black-body radiation. Package: astro.spec Description: Calculate the eqilibrium temperature of a body illuminated by a black-body radiation.


    
    Eqilibrium temperature of a body illuminated by a black-body radiation.  
    Package: astro.spec  
    Description: Calculate the eqilibrium temperature of a body  
    illuminated by a black-body radiation.  
    Input  : - Radiating source radius [cm].  
    - Radiating source temperature [K].  
    - Distance between radiating source and object [cm].  
    - Geometric albedo of object.  
    Output : - Equlibrium temperature of object [K].  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Oct 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Teq=astro.spec.eq_temp(696000e5,5700,1.5e13,0.3)  
    Reliable: 2  
      
      
### astro.spec.extinction

Extinction in band from E_{B-V} Package: astro.spec Description: Given the E_{B-V} and the wavelength or filter name calculate the extinction in magnitude.


    
    Extinction in band from E_{B-V}  
    Package: astro.spec  
    Description: Given the E_{B-V} and the wavelength or filter name calculate  
    the extinction in magnitude.  
    The program works in the 0.1-2 micron range.  
    The program is using the Cardelli, Clayton, Mathis (1989)  
    model.  
    See also old version: optical_extinction.m.  
    Input  : - E_{B-V} [mag]  
    - Filter effective wavelength [microns] or filter family name  
    (e.g., 'SDSS'). See get_filter.m for details.  
    - Filter name (e.g., 'r').  
    - R (scalar) Av/E(B-V), default value is 3.08.  
    Output : - Extinction [mag]  
    Tested : MATLAB 5.1  
    By : Eran O. Ofek                    Feb 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: optical_extinction.m  
    Reference : Cardelli, Clayton, Mathis 1989 ApJ 345, 245  
    Example:  to calculate The extinction in 'U' band given E(B-V)=0.1  
    A_U = astro.spec.extinction(0.1,'Johnson','U')  
    Reliable: 1  
      
### astro.spec.find_shift_scale_spec

Find best scale and additive shift to match two spectra Package: AstroUtil.spec Description: Given a spectrum and a reference spectrum [Wavelength, Flux] find the shift (a constant added to the data) and/or a


    
    Find best scale and additive shift to match two spectra  
    Package: AstroUtil.spec  
    Description: Given a spectrum and a reference spectrum [Wavelength, Flux]  
    find the shift (a constant added to the data) and/or a  
    scaling (multiply the data by a constant) that relats the  
    data and the reference by minimizing the rms.  
    Note that the shift is applaied first.  
    Alternatively, the program can find a factor as function  
    of wavelength that such: Ref*Factor = Spec.  
    This is done by fitting a polynomial to the log of the ratio  
    between the spectra and reference.  
    Input  : - Spectrum [Wavelength, Intensity]  
    - Reference spectrum [Wavelength, Intensity]  
    - Shift method:  
    'none'  
    'mean'  
    'median'   (default).  
    'std'  
    'min'  
    'max'  
    'fit'      - find shift and scale using a LSQ wavelength independent fit.  
    - Scale method:  
    'none'  
    'mean'  
    'median'    (default).  
    'std'  
    'range'  
    'min'  
    'max'  
    'fit'       - find shift and scale using a LSQ wavelength independent fit.  
    - Sigma clipping (for the fit option) in std units (default is NaN).  
    if NaN then no sigma clipping.  
    - Match Spec and Ref by devision and polynomial fit.  
    If 0 or not given then use Shift and Scale method,  
    else use this method instead of Shift and Scale methods. Default is 0.  
    Output : - Shift, or alternativel if Poly>0, then:  
    return [Wavelength, Factor],  
    in which wavelength is taken from the Ref, and Ref*Factor = Spec.  
    - Scale  
    - Correlation between spectra and reference in overlap region.  
    - The number of spectral points in the Spectra within the overlap region.  
    - RMS of spectra and reference residual in overlap region.  
    - Vector of residuals between spectra and reference in overlap region.  
    - Scaled reference spectra  
    See also: scale_spectrum.m  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### astro.spec.fit_bb

Fit a blackbody to spectrum Package: astro.spec Description: Fit a black body spectrum to a list of spectral measurments, spectrum or photometric measurments.


    
    Fit a blackbody to spectrum  
    Package: astro.spec  
    Description: Fit a black body spectrum to a list of spectral measurments,  
    spectrum or photometric measurments.  
    Input  : - The spectral points to fit [Wavelength, Flux, [Err]].  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'FitMethod' - One of the following fit methods:  
    'lscov' - use lscov. Default.  
    '\'  
    'ratio_chi2'  
    'ratio_rms'  
    'ratio_relrms'  
    'BadRanges' - Two column matrix of bad ranges to remove from  
    spectrum before fitting. Line per bad range.  
    This can be used to mask lines.  
    Default is [].  
    'WaveUnits' - Wavelength units. Default is 'Ang'.  
    See convert.units.m for options.  
    'IntUnits'  - Flux units. Default is 'erg*cm^-2*s^-1*Ang^-1'.  
    See convert.flux.m for options.  
    E.g., 'AB', 'STmag','mJy','ph/A',...  
    'Trange'    - Temperature range [K] in which to search for  
    solution. Default is [100 5e6].  
    'Tpoint'    - Number of points in each search range.  
    Default is 5.  
    'Thresh'    - Convergence threshold. Default is 0.01;  
    'ColW'      - Column index in spectrum input containing  
    the wavelength. Default is 1.  
    'ColF'      - Column index in spectrum input containing  
    the flux. Default is 2.  
    'ColE'      - Column index in spectrum input containing  
    the error in flux. Default is 3.  
    'RelErrFlux' - If the error column is empty then will se the  
    error to be the flux multiplied by this factor.  
    If this factor is empty then will set the errors  
    to 1. Default is 0.05.  
    'FitFun'    - Function for ratio calculation in initial  
    iterative search. Default is @mean.  
    'Nsigma'    - Return the errors for Nsigma confidence  
    interval. Default is 1.  
    'Plot'      - plot specectra and best fit. Default is false.  
    Output : - A structure containing the best fit black body results.  
    - AstSpec class object containing the calculated best fit  
    black-body spectrum.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Feb 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Res,SpecBB]=astro.spec.fit_bb([AS(1).Wave,AS(1).Int]);  
    plot(AS(1)); hold on; plot(SpecBB);  
    Reliable: 2  
      
      
### astro.spec.fit_bb_photometry

SHORT DESCRIPTION HERE Package: astro Description:


    
    SHORT DESCRIPTION HERE  
    Package: astro  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Oct 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Chi2=astro.spec.fit_bb_photometry(tRes)  
    Reliable:  
      
      
      
### astro.spec.fit_specline

Fit and measure flux of spectral line Package: AstroUtil.spec Description: Fit multiple profiles and measure flux of spectral line


    
    Fit and measure flux of spectral line  
    Package: AstroUtil.spec  
    Description: Fit multiple profiles and measure flux of spectral line  
    Input  : - Spectrum to fit [Wavelength, Flux].  
    If empty matrix, then will attempt to read the spectrum from  
    the current figure.  
    - Function to fit.  
    If empty matrix than do not fit a function, but calculate  
    line flux and equivalent width.  
    * Arbitrary number of pairs of ...,key,val,... arguments.  
    The following keywords are available:  
    'Back'   - Method of background subtractuin:  
    'none' - use fitting function only.  
    'man'  - Interactive subtraction of background  
    (default).  
    Alternatively, this can be a vector that specify  
    from where to obtain the background:  
    [X1, X2, X3, X4].  
    Where X1 and X2 mark the position of the left  
    background region and X3 and X4 is for the right  
    background region.  
    'Poly'   - Polynomial order of manual background subtraction.  
    Default is 1.  
    'Plot'   - Plot fitted lines {'y'|'n'}. Default is 'y'.  
    'PlotPar'- Cell array of additional parameters to pass to the  
    fitted lines ploting function.  
    Default is {'','Color',[0.8 0.8 0.8]}.  
    'Par0'   - Vector of guess parameters.  
    If equal 'guess' then will attempt to call the  
    fitted function Fun('guess',Spec) which suppose  
    to return a guess.  
    'Nsim'   - Number of simulations within the specified  
    background ranges. Default is 1.  
    'MinBkg' - Minimum bkg range in terms of fraction of the defined  
    range. Default is 0.3 (30).  
      
    Output :   - Vector of Fit structure.  
    If Nsim=1 then the first Fit values (Flux,EW) are according to the original  
    background ranges. The rest are according to the different  
    randomly-selected bkg ranges, but always the range from  
    which the flux is calculated (Iall) is of the inner edges of  
    the original bkg (supplied or by interactive selection).  
    Tested : Matlab 7.13  
    By : Eran Ofek / Ofer Yaron          Jul 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Fit=fit_specline(Spec,@fun_gauss);  
    Fun = @(Par,X) fun_gauss(Par(1:3),X)+fun_gauss(Par(4:6),X)  
    Fit=fit_specline(Spec,Fun,'Par0',[1 6630 10 1 6630 100]);  
    Fun = @(Par,X) fun_gauss(Par(1:3),X)+fun_lorentzian(Par(4:6),X)  
    Fit=AstroUtil.spec.fit_specline(Spec,Fun,'Par0',[1 6630 10 6630 100 10]);  
    Fit=fit_specline(Spec,[],'Back',bkg,'Nsim',100,'MinBkg',0.5);  
    Reliable: 2  
      
      
### astro.spec.fit_template2phot

Fit a set of spectral templates to photometric observations of a source. Package: astro Description: Fit a set of spectral templates to photometric observations of a single source. Return the spectral template that best


    
    Fit a set of spectral templates to photometric observations of a source.  
    Package: astro  
    Description: Fit a set of spectral templates to photometric observations  
    of a single source. Return the spectral template that best  
    fit the photometric data.  
    Input  : - Photometric data of a single source to which to fit spectral  
    templates.  
    This is one of the following:  
    Cell array of {Mag, Err, BandFamily, BandName, MagType}  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Template' - An AstSpec object of spectral templates to fit.  
    Default is AstSpec.get_pickles([],'V').  
    'SelectionMethod' - Method by which to select best fit  
    template. {'rms' | 'chi2'}. Default is 'rms'.  
    'IgnorePartial' - A flag indicating if to ignore synthetic  
    magnitude which is based on extrapolation.  
    Default is true.  
    'SynMag' - A matrix of synthetic magnitude.  
    Default is empty.  
    This is useful in order to avoid re-calculating the  
    synthetic magnideu multiple times.  
    'ExtrapFlag' - Like SynMag but for the extrapolation flag.  
    Output : - A structure with the following fields:  
    'BestRMS' - RMS for best template.  
    'BestChi2' - Chi^2 for best template.  
    'NfiltUse' - Number of filter used for fitting each template.  
    'BestTempInd' - Index of best fitted template.  
    - Matrix of synthetic magnitude [template, filter]  
    - Matrix of syn.mag interpolation flag (0 if ok)  
    [template, filter].  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
### astro.spec.fun_gauss

1-D Gaussian Package: astro.spec Description: Calculating a Gaussian function of the form: Y = Amplitude*exp( (X-W0)^2/(2*Sigma^2) )


    
    1-D Gaussian  
    Package: astro.spec  
    Description: Calculating a Gaussian function of the form:  
    Y = Amplitude*exp( (X-W0)^2/(2*Sigma^2) )  
    Optionaly, convolve the the result with a Gaussian.  
    This function can be used by fitting functions like  
    nlinfit_my.m and fit_specline.m  
    Input  : - Vector of free parameters of the Gaussian  
    [Integral, W0, Sigma]  
    Alternatively, if this parameter is 'guess' then the program  
    will attempt to guess (i.e., find initial values for fitting  
    functions) the free parameters, based on the position of  
    the highest value.  
    - X positions at which to calculate the Gaussian.  
    If the first argument is 'guess' then this parameter should  
    be [X,Y], and the program will attempt to guess the free  
    parameters.  
    Alternatively, this parameter can be 'int' and in this case  
    the program will calculate the integral of the Gaussian.  
    - Width (in sigma) of convolution kernel.  
    This can be used only if the X position is evenly spaced.  
    Default is 0.  
    Output : - The Gaussian value at X position.  
    Alternatively, if the first input argument is 'guess' then  
    will return the best guess parameters.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek        Apr 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=[4000:1:9000].'; Y=astro.spec.fun_gauss([1 6000 100],X);  
    Y=astro.spec.fun_gauss([1 6000 100],X,100);  
    GyessPar=astro.spec.fun_gauss('guess',[X Y]);  find initial parameters  
    Int = astro.spec.fun_gauss([1 6000 100],'int')  
    Reliable: 2  
      
      
### astro.spec.fun_lorentzian

1-D Lorentzian Package: astro.spec Description: Calculating a Lorentzian function of the form: Y = D.*Gamma./(pi.*( (X-X0).^2 + Gamma.^2 ))


    
    1-D Lorentzian  
    Package: astro.spec  
    Description: Calculating a Lorentzian function of the form:  
    Y = D.*Gamma./(pi.*( (X-X0).^2 + Gamma.^2 ))  
    Optionaly, convolve the the result with a Gaussian.  
    This function can be used by fitting functions like  
    nlinfit_my.m and fit_specline.m  
    Input  : - Vector of free parameters of the Gaussian  
    [X0, Gamma, D] which are  
    the Line Center, Line Width, Line depth, respectively.  
    Alternatively, if this parameter is 'guess' then the program  
    will attempt to guess (i.e., find initial values for fitting  
    functions) the free parameters, based on the position of  
    the highest value.  
    - X positions at which to calculate the Gaussian.  
    If the first argument is 'guess' then this parameter should  
    be [X,Y], and the program will attempt to guess the free  
    parameters.  
    Alternatively, this parameter can be 'int' and in this case  
    the program will calculate the integral of the Gaussian.  
    - Width (in sigma) of convolution kernel.  
    This can be used only if the X position is evenly spaced.  
    Default is 0.  
    Output : - The Gaussian value at X position.  
    Alternatively, if the first input argument is 'guess' then  
    will return the best guess parameters.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek        Apr 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=[4000:1:9000].'; Y=AstroUtil.spec.fun_lorentzian([6000 100 10],X);  
    GyessPar=astro.spec.fun_lorentzian('guess',[X Y]);  find initial parameters  
    Int = astro.spec.fun_lorentzian([6000 100 10],'int')  
    Reliable: 2  
      
      
### astro.spec.fun_pcyg

1-D simplistic P-Cygni line model Package: AstroUtil.spec Description: Calculating a P-Cygni profile composed of two Gaussians of the form:


    
    1-D simplistic P-Cygni line model  
    Package: AstroUtil.spec  
    Description: Calculating a P-Cygni profile composed of two Gaussians  
    of the form:  
    Y = AmplitudeEmi*exp( (X-W0)^2/(2*SigmaEmi^2) ) -  
    AmplitudeAbs*exp( (X-W0)^2/(2*SigmaAbs^2) ) for X<W0  
    and only the first term for X>=W0.  
    Optionaly, convolve the the result with a Gaussian.  
    This function can be used by fitting functions like  
    nlinfit_my.m and fit_specline.m  
    Input  : - Vector of free parameters of the Gaussian  
    [IntegralEmi, W0, SigmaEmi, HalfIntegralAbs, SigmaAbs, N]  
    Alternatively, if this parameter is 'guess' then the program  
    will attempt to guess (i.e., find initial values for fitting  
    functions) the free parameters, based on the position of  
    the highest value.  
    - Width (in sigma) of convolution kernel.  
    This can be used only if the X position is evenly spaced.  
    Default is 0.  
    Output : - The P-Cygni profile function value at X position.  
    Alternatively, if the first input argument is 'guess' then  
    will return the best guess parameters.  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    May 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=[4000:1:9000].'; Y=AstroUtil.spec.fun_pcyg([1 6000 100 0.5 200 2],X,10);  
    Reliable:  
      
      
### astro.spec.fun_voigt

1-D Voight profile Package: AstroUtil.spec Description: Calculate a Voigt line profile (convolution of a Gaussian and a Lorntzian).


    
    1-D Voight profile  
    Package: AstroUtil.spec  
    Description: Calculate a Voigt line profile (convolution of a Gaussian  
    and a Lorntzian).  
    Input  : - Vector of parameters [Line center, Line width, Line depth,  
    Gaussian sigma].  
    - Vector of wavelength [A] in which to calculate the Voigt  
    profile.  
    Output : - Line intensity as a function of input wavelngth [power A^-1]  
    Tested : Matlab 7.13  
    By : Eran O. Ofek                        Oct 2012  
    URL : http://www.weizmann.ac.il/home/eofek/matlab/  
      
      
      
### astro.spec.hydrogen_lines

The vacum wavelength of Hydrogen lines Package: AstroUtil.spec Description: Calculate the vacum wavelength of Hydrogen lines, given their shell numbers.


    
    The vacum wavelength of Hydrogen lines  
    Package: AstroUtil.spec  
    Description: Calculate the vacum wavelength of Hydrogen lines, given their  
    shell numbers.  
    Input  : - Inner shell number (scalar or a vector).  
    (e.g., 2 for Balmer series).  
    - Outer shell number (scalar or a vector).  
    Output : - Wavelengths of the corresponding Balmer lines [A].  
    Tested : Matlab 7.3  
    By : eran O. Ofek                    Feb 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: L=hydrogen_lines(2,3);   Balmer H\alpha line  
    Reliable: 1  
      
      
### astro.spec.interp_mag

Find best fit spectra from magnitudes and interpolate to other bands. Package: AtroUtil.spec Description: Given magnitude of an object find the best fit spectra (from a library of templates), and calculate the magnitude of


    
    Find best fit spectra from magnitudes and interpolate to other bands.  
    Package: AtroUtil.spec  
    Description: Given magnitude of an object find the best fit spectra  
    (from a library of templates), and calculate the magnitude of  
    the spectra in additional bands.  
    Input  : - Input measured magnitudes, and optional errors [Mag, Err].  
    If single column is given, then give equal weight  
    for all measurments.  
    - Cell array of filters normalized transmission,  
    corresponding to the input magnitudes.  
    - Cell array of magnitude types for input mag 'AB' | 'Vega'.  
    - Cell array of filters normalized transmission,  
    in which to calculate the output magnitudes.  
    - Cell array of magnitude types for input mag 'AB' | 'Vega'.  
    - Vector of redshifts to test, default is 0.  
    - Vector of E_{B-V} extinctions to test, default is 0.  
    - Types of templates to use {'Star' | 'StarMS' | 'QSO' | 'Gal'},  
    default is 'Star'.  
    - Two elements vector containing the airmass of atmospheric  
    extinction to applay to a spectrum while  
    calculating the input magnitudes (first element), and  
    while calculating the output magnitudes (second element).  
    Default is [0 0] - no extinction.  
    - Two elements vector containing the strength of the  
    Telluric absorptions (see get_spectra.m)  
    to applay to a spectrum while calculating the input  
    magnitudes (first element), and while calculating  
    the output magnitudes (second element).  
    Default is [0 0] - no Telluric absorption.  
    Output : - Vector of output magnitudes.  
    - Best RMS [mag].  
    - Best fit template name.  
    - RMS matrix for all templates.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Needed: Spectra library  
    Example:  Fit template and interpolate magnitudes  
    InABV  = {'AB','AB'}; OutABV = {'AB','AB','AB'};  
    MeasuredMag=[19.69;19.24];  
    F475W=get_filter('HST-ACS','F475W');  
    F814W=get_filter('HST-ACS','F814W');  
    g=get_filter('SDSS','g');  
    r=get_filter('SDSS','r');  
    i=get_filter('SDSS','i');  
    [BestOutMag,MinRMS,BestFit,RMS]=interp_mag(MeasuredMag,{F475W.nT{1},F814W.nT{1}},InABV,{g.nT{1},r.nT{1},i.nT{1}},OutABV,0,0)  
    Reliable: 1  
      
### astro.spec.interpolant_mag

Interpolant from a time series of photometric observations in one band Package: AstroUtil.spec Description: Given a time series of observations take at a single band return an interpolant that allows to calculate the magnitude


    
    Interpolant from a time series of photometric observations in one band  
    Package: AstroUtil.spec  
    Description: Given a time series of observations take at a single band  
    return an interpolant that allows to calculate the magnitude  
    in each time within observations range.  
    Input  : - [Time, Mag, Err]  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'MaxTimeNoObs' - Maximum time without observations.  
    If gap larger than this exist in the data the  
    interpolant will not be valid in the gap.  
    Default is 5.  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Interp=AstroUtil.spec.interpolant_mag(Data)  
    Reliable:  
      
      
### astro.spec.ionization_potential

Return ionization potential for elemnt and ionization level. Package: astro.spec Description: Returm the ionization potential for a given element and ionization level.


    
    Return ionization potential for elemnt and ionization level.  
    Package: astro.spec  
    Description: Returm the ionization potential for a given element and  
    ionization level.  
    Input  : - Atomic number (Z) or name (e.g., 'He').  
    - Vector of ionization level. If empty (e.g., []), then will  
    return all the ionization potential available in the DB  
    for the specific element.  
    Default is empty matrix.  
    Output : - List of ionization potentials [eV].  
    - Structure containing the entire database of ionization  
    potentials.  
    Tested : Matlab 2012a  
    By : Eran O. Ofek                    Sep 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: http://srdata.nist.gov/gateway/gateway?dblist=0  
    http://www.physics.ohio-state.edu/~lvw/handyinfo/ips.html  
    Example: [P,IP]=astro.spec.ionization_potential('He',2);  
    [P,IP]=astro.spec.ionization_potential(2,2);  
    [P,IP]=astro.spec.ionization_potential('Ti',[2 3]);  
    Reliable: 2  
      
      
### astro.spec.is_rrlyr_sdss_colors

Is RR Lyr star candidate based on SDSS colors Package: astro.spec Description: Select RR Lyr star candidates based on their SDSS magnitudes in the u, g, and r-bands.


    
    Is RR Lyr star candidate based on SDSS colors  
    Package: astro.spec  
    Description: Select RR Lyr star candidates based on their SDSS magnitudes  
    in the u, g, and r-bands.  
    Based on  
    Input  : - u mag  
    - g mag  
    - r mag  
    Output : - Flag indicating of RR Lyr star:  
    0 - no; 1 - 6; 2 - 28; 3 - 61  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: astro.spec.is_rrlyr_sdss_colors(21,20,20)  
    Reliable: 2  
      
      
      
    DefV. =  
    InPar = InArg.populate_keyval(DefV,varargin,mfilename);  
      
### astro.spec.kcorr

Calculate k-correction. Package: AstroUtil.spec Description: Calculate k-correction. Given a spectrum two filters, and their redshifts calculate the k-correction of the first


    
    Calculate k-correction.  
    Package: AstroUtil.spec  
    Description: Calculate k-correction. Given a spectrum two filters, and  
    their redshifts calculate the k-correction of the first  
    filter minus the second filter. This is calculated by  
    shifting the spectrum to z1, measuring the synthetic  
    magnitude in the first filter, then shiting to z2  
    and measuring the synthetic magnitude in the second filter.  
    Input  : - Spectrum [Wavelength(Ang), Flux(F_lambda)] of source.  
    Assume the spectrum is at z=0.  
    See convert.flux.m to transform from general units to F_lambda.  
    - Vector of redshift in which to apply the first filter.  
    - First filter family. See get_filter.m for details.  
    - First filter name. See get_filter.m for details.  
    - First magnitude system. See get_filter.m for details.  
    - Vector of redshift in which to apply the second filter.  
    This vector should be of the same length as z1.  
    - Second filter family. See get_filter.m for details.  
    - Second filter name. See get_filter.m for details.  
    - Second magnitude system. See get_filter.m for details.  
    Output : - The magnitude difference between the first and second filters,  
    applay to the spectrum at different redshifts.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Apr 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Oke & Sandage 1968 ApJ, 154, 21  
    Example: Kcorr=AstroUtil.spec.kcorr(Spec,2,'GALEX','NUV','AB',1,'2MASS','J','Vega')  
    Reliable: 2  
      
      
### astro.spec.lines_db

Search spectral line by name or wavelength and add ionization potential Package: AstroUtil.spec Description: Search spectral line by name or wavelength and add ionization potential information.


    
    Search spectral line by name or wavelength and add ionization potential  
    Package: AstroUtil.spec  
    Description: Search spectral line by name or wavelength and add  
    ionization potential information.  
    The DB list of lines contains 46663 spectral lines  
    (Reader et al. 1980; 1981) for 99 atomic species.  
    Neutral through quadruply ionized atoms are tabulated.  
    The Main list of lines is a smaller list of prominant lines.  
    Input  : - Line wavelength, or name (e.g., 'H I').  
    If empty then upload entire LinesDB.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'SearchWidth' - Wavelength search semi width.  
    Default is 10 Ang.  
    'SortBy'      - Sort output by one of the following columns:  
    'Z'|'Intensity'|'Wave'|'Name'|'IonPotential'.  
    Default is 'Z'.  
    'AddIonPot'   - Add ionization potential to table.  
    Default is true.  
    'q'           - Apply a query to catalog.  
    E.g., 'IonPotential>10 & Z<10'.  
    Default is empty.  
    'DB'          - Lines database to use. Options are:  
    'DB' - Main database of over 46,000 lines.  
    'Main' - List of main prominant lines.  
    Default is 'DB'.  
    Output : - An AstCat object containing the requested lines.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Ans=AstroUtil.spec.lines_db('H I');  
    Ans=AstroUtil.spec.lines_db(6564,'SearchWidth',2);  
    Ans=AstroUtil.spec.lines_db;  
    Ans=AstroUtil.spec.lines_db(6564,'q','IonPotential>10 & Z<10','SortBy','IonPotential');  
    Reliable: 2  
      
      
### astro.spec.luptitude

luptitude function                                             AstroSpec Description: Convert flux to luptitudes (asinh magnitudes). OBSOLETE: Use convert.luptitude instead.


    
      
    luptitude function                                             AstroSpec  
    Description: Convert flux to luptitudes (asinh magnitudes).  
    OBSOLETE: Use convert.luptitude instead.  
    Input  : - Flux.  
    - Reference flux (Flux0), default is 1.  
    Note that this parameter should equal to 10.^(0.4.*ZP);  
    - Softening parameter (B), default is 1e-10.  
    Output : - Luptitude.  
    Tested : Matlab 7.13  
    By : Eran O. Ofek                    Jul 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### astro.spec.matchspec

A GUI utility to inspect and match spectrum with templates. Package: AstroUtil.spec Description: A GUI utility to inspect and match spectrum with templates.


    
    A GUI utility to inspect and match spectrum with templates.  
    Package: AstroUtil.spec  
    Description: A GUI utility to inspect and match spectrum with templates.  
    Input  : - Spectra to inspect [Wavelength, Intensity, Error]  
    Error is optional.  
    - Method to plot spectra: {'stairs' | 'plot' | 'errorxy'},  
    default is 'stairs'.  
    - Template spectra [Wavelength, Intensity, Error].  
    - Template color, default is 'r'.  
    Output : null  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.spec.matchspec(Spec);  
    Reliable: 2  
      
      
    MATCHSPEC M-file for matchspec.fig  
    MATCHSPEC, by itself, creates a new MATCHSPEC or raises the existing  
    singleton*.  
      
    H = MATCHSPEC returns the handle to a new MATCHSPEC or the handle to  
    the existing singleton*.  
      
    MATCHSPEC('CALLBACK',hObject,eventData,handles,...) calls the local  
    function named CALLBACK in MATCHSPEC.M with the given input arguments.  
      
    MATCHSPEC('Property','Value',...) creates a new MATCHSPEC or raises the  
    existing singleton*.  Starting from the left, property value pairs are  
    applied to the GUI before matchspec_OpeningFunction gets called.  An  
    unrecognized property name or invalid value makes property application  
    stop.  All inputs are passed to matchspec_OpeningFcn via varargin.  
      
    *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one  
    instance to run (singleton)".  
      
    See also: GUIDE, GUIDATA, GUIHANDLES  
      
    Copyright 2002-2003 The MathWorks, Inc.  
      
    Edit the above text to modify the response to help matchspec  
      
    Last Modified by GUIDE v2.5 17-Jul-2005 19:25:41  
      
    Begin initialization code - DO NOT EDIT  
### astro.spec.scale_spectrum

Scale spectrum by shift and stretch. Package: AstroUtil.spec Description: Scale spectrum by shift and stretch or a wavelength dependent factor (See also: find_shift_scale_spec.m).


    
    Scale spectrum by shift and stretch.  
    Package: AstroUtil.spec  
    Description: Scale spectrum by shift and stretch or a wavelength  
    dependent factor (See also: find_shift_scale_spec.m).  
    Input  : - Spectrum to scale [Wavelength, Intensity, Error].  
    Error is optional.  
    - Scale parameters:  
    if two element vector is given then: [Shift Stretch]  
    else regarded as a matrix of [wavelength, Factor]  
    in which factor is multiplied by the intensity  
    to get the new spectrum.  
    Output : - Scaled spectrum [Wavelength, Intensity, Error].  
    NewSpec = (Spec+Shift)*Stretch.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: find_shift_scale_spec.m  
    Reliable: 1  
      
### astro.spec.shift2vel

Calculate the velocity from the red/blue shift (z). Package: AstroUtil.spec Description: Calculate the velocity from the red/blue shift (z).


    
    Calculate the velocity from the red/blue shift (z).  
    Package: AstroUtil.spec  
    Description: Calculate the velocity from the red/blue shift (z).  
    Input  : - Vector of red/blue shift (z).  
    Output : - Vector of Velocities in km/sec.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: vel2shift.m  
    Example: shift2vel(1.0)  
    Reliable: 1  
      
### astro.spec.shift_spec

Transform a spectrum from the observed frame to the rest frame. Package: astro.spec Description: Transform a spectrum from the observed frame to the rest frame. If the redshift is negative then transform from the rest


    
    Transform a spectrum from the observed frame to the rest frame.  
    Package: astro.spec  
    Description: Transform a spectrum from the observed frame to the rest frame.  
    If the redshift is negative then transform from the rest  
    frame to the observed frame.  
    Input  : - Spectrum, in which the first column is  
    the wavelength/frequency/energy, and the second  
    is the specific flux.  
    - The redshift Z.  
    If the redshift is negative then transform from the rest  
    frame to the observed frame.  
    - Working method in: wavelength/frequency/energy  
    'w' - wavelength [lam_rest = lam_obs /(1+Z)] -default.  
    'f' - frequency.  
    'e' - energy.  
    - Specific flux conversion method:  
    'w' - f_{lambda} -> f_rest=f_obs * (1+Z)^3 - default.  
    'f' - f_{nu}     -> f_rest=f_obs * (1+Z)  
    'wi'- f_{lambda} -> f_obs=f_rest * (1+Z)^-3  
    'fi'- f_{nu}     -> f_obs=f_rest * (1+Z)^-1  
    'n' - do nothing.  
    Output : - The new shifted spectrum.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek / Ofer Yaron       May 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
      
### astro.spec.sky_ebv

Get Galactic extinction for a list of coordinates Package: AstroUtil.spec Schlegel, Finkbeiner & Davis (1998) extinction maps. Use AstroUtil.spec.extinction to calculate the extinction.


    
    Get Galactic extinction for a list of coordinates  
    Package: AstroUtil.spec  
    Schlegel, Finkbeiner & Davis (1998) extinction maps.  
    Use AstroUtil.spec.extinction to calculate the extinction.  
    Input  : - J2000.0 RA or longitude [H M S] or [RAD] or sexagesimal.  
    - J2000.0 Dec or latitude [Sign D M S] or [RAD] or sexagesimal.  
    - Coordinates type:  
    'eq' : J2000.0 equatorial (default).  
    'g'  : Galactic.  
    'ec'  : ecliptic.  
    - true|false Correct Schlegel et al. E(B-V) when >0.1,  
    using the Adams et al. (2013) correction.  
    I.e., at large E(B-V) values Schlegel et al. is probably  
    overestimating the extinction.  
    Default is true.  
    Output : - E(B-V) [mag].  
    The E(B-V) is returned from the point in the map which is  
    the nearest to the input coordinates.  
    - [A_U, A_B, A_V, A_R, A_I, A_J, A_H, A_K] (assuming R=3.08).  
    Reference: Schlegel, Finkbeiner & Davis (1998; ApJ 500, 525).  
    Needed : coco.m  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : hhtp://weizmann.ac.il/home/eofek/matlab/  
    Example: [Ebv]=AstroUtil.spec.sky_ebv(1,1);  
    Reliable: 2  
      
      
### astro.spec.spec_photon_counts

Spectrum to photon counts. OBSOLETE: Use telescope.sn.spec2photons. Package: astro.spec Description: Given a spectrum and the effective area of an instrument as a function of wavelength, calculate the the total recieved


    
    Spectrum to photon counts. OBSOLETE: Use telescope.sn.spec2photons.  
    Package: astro.spec  
    Description: Given a spectrum and the effective area of an instrument as  
    a function of wavelength, calculate the the total recieved  
    flux and the photons count rate in the instrument.  
    OBSOLETE: Use telescope.sn.spec2photons instead.  
    Input  : - Spectra of the source (emitted from a cm^2 on the source):  
    [Wavelengh(Ang), Emmitence(erg cm^-2 s^-1 A^-1)].  
    Alternatively, a scalar representing a black-body  
    temperature [K].  
    - Filter [Wavelength(Ang), EffectiveAreaOfInstrument(cm^2)].  
    If two element vector than assumes it to be the wavelength  
    range [Ang] of a top-hat filter with eefective area of 1 cm^2.  
    - Transmission law [Wavelength(Ang), Transmission(frac)].  
    Where the extinction is measured in fraction  
    (e.g., 1 means no extinction; 0.5 means half the photons  
    are absorbed).  
    If empty matrix (i.e., []), then assumes no extinction.  
    - Source radius [cm], default is 1cm.  
    - Source distance [pc], default is 10pc.  
    Output : - Observed flux [erg s^-1] on instrument.  
    - Observed photon count rate on instrument.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Nov 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See Also: blackbody_mag_c.m, blackbody_flux.m  
    Example: [Flux,Counts]=telescope.sn.spec2photons(5770,[100 300000],[],696000e5,10);  
    Flux.*4.*pi.*(10.*3.08e18).^2;    Bolometric luminosity of the Sun  
    Reliable: 2  
      
### astro.spec.synphot

Synthetic photometry of spectrum Package: astro.spec Description: Calculate synthetic photometry of a spectrum OBSOLETE: use AstroUtil.spec.synthetic_phot.m


    
    Synthetic photometry of spectrum  
    Package: astro.spec  
    Description: Calculate synthetic photometry of a spectrum  
    OBSOLETE: use AstroUtil.spec.synthetic_phot.m  
    Input  : - Spectrum [wavelength(Ang), Flux(F_{\lambda})].  
    - Filter normalized transmission curve,  
    or a string containing filter familiy name,  
    or AstFilter class object.  
    See AstFilter.get for details.  
    Filter transmission curve override filter name.  
    - Filter name, see AstFilter.get.m for details.  
    - Magnitude system: {'Vega' | 'AB'}  
    - Algorithm used:  
    'Poz' - D. Poznanski, basic_synthetic_photometry.m  
    'cos' - transmission curve interpolated on spectrum.  
    Default.  
    'soc' - spectrum interpolated on transmission curve.  
    If empty matrix, then use default.  
    - E_{B-V} extinction to apply to spectrum before calculating  
    the synthetic photometry. Default is 0.  
    This function is using the Cardelli et al. model implemented  
    in extinction.m  
    - R_v of extinction. Default is 3.08.  
    - Device to be use:  
    'Bol' - Bolometric device which measure the total energy  
    within the band.  
    'photon' - Photon Counting device which measure the number  
    of photons within the band  
    Output : - Synthetic magnitude.  
    - The fraction of flux that was extrapolated in case of  
    partial coverage between spectrum and filter.  
    0 - means no extrapolation.  
    - Filter effective wavelength [Ang].  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    May 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Comments: The 'Poz' option requires basic_synthetic_photometry.m  
    by: Dovi Poznanski.  
    Example: [Mag,Flag]=synphot(Spec,'SDSS','r','AB');  
    [Mag,Flag]=synphot(Spec,'SDSS','r','AB',[],0.1);  apply extinction  
    Reliable: 1  
      
### astro.spec.synthetic_phot

Synthetic photometry of spectra Package: astro.spec Description: Synthetic photometry of spectra. Can calculate synthetic photometry in a single band to


    
    Synthetic photometry of spectra  
    Package: astro.spec  
    Description: Synthetic photometry of spectra.  
    Can calculate synthetic photometry in a single band to  
    multiple spectra.  
    Input  : - Spectrum [Wave, Flux, [Flux, Flux, ...]]  
    matrix with two or more columns. The first column is the  
    wavelength, while the rest are the flux of varius spectra  
    sampled at the same wavelengths.  
    - Either:  
    Filter family name - e.g., 'SDSS'. or  
    Matrix of transmission [Wave[Ang], Transmission], or  
    AstFilter object.  
    - Filter name (relevant only if previus argument is filter  
    family).  
    - Magnitude system. 'AB'|'Vega'. Default is 'AB'.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Device' - 'photon' | 'bol'. Default is 'photon'.  
    'SpecFluxUnits' - Default is 'cgs/A'.  
    'SpecWaveUnits' - Default is 'A'.  
    'ColW'          - Wavelength colum. Default is 1.  
    'ColF'          - Flux column. Default is '2end'.  
    if '2end' then select all columns except 1.  
    'InterpMethod'  - Interpolation method. Default is 'linear'.  
    Output : - Magnitude  
    - Coverage factor of the transmission integral. Scalar  
    representing the first spectrum.  
    - Flux.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: S=astro.get_pickles;  
    SS=astro.blackbody(5770,(2000:1:10000)');  
    SS.Int = SS.Int.*4.*pi.*constant.SunR.^2./(4.*pi.*(10.*constant.pc).^2);  
    [Mag,Cover,F]=astro.spec.synthetic_phot([SS.Wave, SS.Int],'SDSS','g','AB');  
    Reliable: 2  
      
      
### astro.spec.vel2shift

Calculate the red/blue shift (z) from velocity. Package: AstroUtil.spec Description: Calculate the red/blue shift (z) from velocity.


    
    Calculate the red/blue shift (z) from velocity.  
    Package: AstroUtil.spec  
    Description: Calculate the red/blue shift (z) from velocity.  
    Input  : - Matrix of Velocities (default units are km/s).  
    - String of units of velocity. Default is 'km/s'.  
    Output : - Matrix of red/blue shift (Z).  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: shift2vel.m  
    Example: vel2shift(179875.4748)   return redshift = 1  
    Reliable: 1  
      
### astro.spec.wein

Wein law Package: AstroUtil.spec Description: Apply Wein law - return the peak wavelength of a black body at a given temperature.


    
    Wein law  
    Package: AstroUtil.spec  
    Description: Apply Wein law - return the peak wavelength of a  
    black body at a given temperature.  
    Input  : - Temperature.  
    - Units of temperature,  
    {'erg'|'J'|'Hz'|'A'|'eV'|'T'|'me'|'mp'|'cal'|'Btu'|  
    'kWh'|'TNT','gr'}, default is 'T' (Kelvin),  
    see convert_energy.m for options.  
    Output : - Wavelength of peak intensity of black body spectrum [Ang].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Feb 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: PeakWave=wein(1,'eV'); PeakWave=wein(5770,'T');  
    Reliable: 1  
      
      
### astro.spec.xray_abs

Bound-free absorption from neutral hydrogen column density. Package: AstroUtil.spec Description: Given the neutral Hydrogen column density, calculate the bound-free attenuation of X-rays as a function of wavelength


    
    Bound-free absorption from neutral hydrogen column density.  
    Package: AstroUtil.spec  
    Description: Given the neutral Hydrogen column density, calculate the  
    bound-free attenuation of X-rays as a function of wavelength  
    in the ISM.  
    The program assumes abundences from Ebihara (1982).  
    Absorption is due to neutral species only.  
    Adopted from Zombeck (1990).  
    Input  : - Energy [keV] in range 0.03 to 10.0 keV.  
    - Column density [cm^-2].  
    Output : - The fraction of transmitted X-rays (1-no attanuation,  
    0-full attanuation)  
    - Optical depth.  
    - Cross section.  
    Tested : Matalb 7.3  
    By : Eran O. Ofek                    Nov 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Zombeck (1990)  
    http://ads.harvard.edu/cgi-bin/bbrowse?book=hsaa&chap=6&page=0202  
    From: Morrison & McCammon (1983; ApJ 270, 119)  
    Example: [Tran,Tau,CrossSec]=astro.spec.xray_abs([0.03:0.01:10]',1e20);  
    Reliable: 2  
      
### astro.spec.zodiac_bck

Zodiac background light (OBSOLETE). Package: astro.spec Description: Calculate the zodiac magnitude and flux in a given filter and sky position. The zodiac spectrum and position


    
    Zodiac background light (OBSOLETE).  
    Package: astro.spec  
    Description: Calculate the zodiac magnitude and flux in a given filter  
    and sky position. The zodiac spectrum and position  
    dependent flux are adopted from the HST WFC3 handbook.  
    OBSOLETE: Use AstSpec.zodiac_bck instead.  
    Input  : - Either Ecliptic longitude or Helio-ecliptic longitude  
    (i.e., L - L_sun) in radians  
    (see convertdm.m for additional options).  
    By default this should be the Helio-ecliptic longitude.  
    - Ecliptic latitude [radians].  
    (see convertdm.m for additional options).  
    - Date [D M Y] or JD at which to calculate the Helio-ecliptic  
    longitude. Defaut is empty. If empty than treat the first  
    input argument as Helio-ecliptic longitude.  
    - Filter family (e.g., 'GALEX'). See get_filter.m for  
    options. Default is 'SDSS'.  
    - Filter band name (e.g., 'NUV'). See get_filter.m for  
    options. Default is 'r'.  
    - Mag system {'AB'|'Vega'}. Default is 'AB'.  
    Output : - Zodiacal light magnitude in filter and position.  
    - Zodiacal light V-band magnitude in position.  
    - Zodiacal light flux in filter [erg cm^-2 s^-1 arcsec^2]  
    - Zodiacal light photon rate [photons cm^-2 s^-1 arcsec^2]  
    Tested : Matlab R2014a  
    By : Ilan Sagiv                      Sep 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [F,V,Flux,Counts]=astro.spec.zodiac_bck(45./RAD,80./RAD,[],'LIM','NUV')  
    Reliable: NEED TO VERIFY zodi spec  
      
