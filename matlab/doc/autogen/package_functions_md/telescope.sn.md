# Package: telescope.sn


### telescope.sn.back_comp

Get spectra of background components Package: telescope Description: Get spectra of background components for S/N calculation. All the components are in [erg/cm^2/s/Ang/arcsec^2].


    
    Get spectra of background components  
    Package: telescope  
    Description: Get spectra of background components for S/N calculation.  
    All the components are in [erg/cm^2/s/Ang/arcsec^2].  
    The background components are:  
    zodi, cerenkov, host galaxy, and sky  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'PsfEffAreaAS' - A mandatory parameter indicating the  
    effective area of the PSF 4*pi*(FWHM/2.35)^2.  
    Default is Inf. Inf will behave like no host  
    galaxy contribution to the background.  
    'ZodiMagV'     -  
    Output : - An AstSpec object in which each element corresponds to  
    a background components.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Nov 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [BackSpec]=telescope.sn.back_comp  
    Reliable:  
      
### telescope.sn.optimal_phot_aperture

Optimal aperture radius for aperture photometry of a Gaussian PSF. Package: telescope.sn Description: Given a Gaussian symmetric PSF and image background and readout noise. Estimate the radius of the photometric


    
    Optimal aperture radius for aperture photometry of a Gaussian PSF.  
    Package: telescope.sn  
    Description: Given a Gaussian symmetric PSF and image background and  
    readout noise. Estimate the radius of the photometric  
    aperture that maximize the S/N for an aperture photometry,  
    for a given target S/N. Return also the signal of the  
    source.  
    Calculate under the assumption that the number of pixels  
    in the background is continuous and not discrete.  
    Also calculate the S/N for a PSF photometry.  
    Input  : * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Sigma'    - Gaussian sigma. Default is 3.  
    'TargetSN' - Tahrget S/N. Default is 3.  
    'B'        - Background [e-]. Default is 100.  
    'RN'       - Readout noise [e-]. Default is 5.  
    'Min_r'    - Minimum allowed aperture radius. Default is 1.  
    'Guess_r'  - Initial guess for optimal aperture. Default is 2.  
    'Thresh'   - Threshold for convergence. Default is 1e-3.  
    Output : - A structure with the following fields:  
    .r  - radius of optimal aperture photometry.  
    .S  - Signal in source [e-] that will produce the target S/N.  
    .SN - S/N at convergence.  
    See also: telescope.sn.sn_psf_phot.m  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Sep 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=telescope.sn.optimal_phot_aperture;  
    Reliable: 2  
      
      
### telescope.sn.sn_calc

A signal-to-noise calculator for astronomical telescopes. Package: telescope.sn Description: A signal-to-noise calculator for astronomical telescopes. Calculate S/N or limiting magnitude and field of view


    
    A signal-to-noise calculator for astronomical telescopes.  
    Package: telescope.sn  
    Description: A signal-to-noise calculator for astronomical telescopes.  
    Calculate S/N or limiting magnitude and field of view  
    properties.  
    Input  : * Arbitrary number of pairs of ...,key,val,... arguments.  
    The following keywords are available:  
    'Mag'   - Magnitude column vector at which to calculate S/N.  
    If the 'SN' field is provided, then this input  
    is ignored.  
    'SN'    - Column vector of S/N at which to calculate  
    limiting magnitude.  
    Default is empty []. If empty, then calculate the  
    S/N at the magnitude given in 'Mag'.  
    'AM'    - Airmass of observation. If empty than assume  
    there is no atmospheric extinction. Default is 1.3.  
    'AtmExtFile' - Atmospheric extinction file.  
    See atmospheric_ext.m for details.  
    Default is 'KPNO_atmospheric_extinction.dat'.  
    'ScatL' - Fraction of scattered light from the entire  
    star light in the field of view. Default is 0.0.  
    'Tel'   - Name of telescope system with predefined  
    parameters {'ULTRASAT','PTF','ZTF','Wise100cm'}.  
    If empty than don't use telescope.  
    NOT YET AVAILABLE.  
    'Aper'  - Telescope diameter [cm]. Default is 60 cm.  
    'FocalLength' - Telescope effective focal length [cm].  
    Default is 95 cm.  
    'Family'- Observation band family (e.g., 'SDSS').  
    See get_astfilter.m for options.  
    'Band'  - Observation band name (e.g., 'r').  
    See get_astfilter.m for options.  
    'MagType'- Magnitude type {'AB','Vega'}. Default is 'AB'.  
    'QOE'   - Total quantum and optical efficiency of the  
    telescope including obscurations. Default is 0.5.  
    'FWHM'  - Effective FWHM [arcsec]. Default is 2.5.  
    'Bin'   - Binning size. Default is 1.  
    'PixSize'- Pixel size [microns]. Default is 6.5.  
    'SizeCCD'- CCD size in pixels. Default is [2048 2048].  
    'Nccd'  - Number of CCDs. Default is 1.  
    'ReadTime'- CCD readout time [s]. Default is 1.  
    'RN'    - Readout noise [e-]. Default is 2.  
    'DC'    - Dark current [e-/s/pix]. Default is 0.016.  
    'WD'    - CCD well depth. Default is 1e5.  
    'Gain'  - Detector gain [e-/ADU]. Default is 1.5.  
    'CTE'   - Detector CTE. Default is 0.99999.  
    'PosCCD'- Position of source on CCD to calculate CTE losses.  
    [pix pix]. Default is [1024 1024].  
    'ExpTime'- Exposure time [s]. Default is 1.  
    'CRsplit'- Number of images. Default is 1.  
    'Back'  - Background [mag/arcsec^2]. Default is 20.3.  
    'BackUnits' - Background units. Default is 'mag/arcsec^2'.  
    'PhotRad'- Row vector of photometric aperture radius, to use  
    in the search for the optimal photometry radius.  
    Defaults is (1.5:0.24).  
    'MagVecSN'- Vector of magnitudes in which to look for  
    limiting magnitude given S/N.  
    Default is (5:0.5:30).'.  
    'Temp'  - Effective black body temperature of source [K].  
    Default is 5770 K.  
    'InterpMethod' - Interpolation method. Default is 'cubic'.  
    See interp1.m for options.  
    'RA'    - J2000.0 R.A. in which to calculate the properties  
    of stars brighter than the magnitude.  
    Default is '10:00:00.0'.  
    See convertdms.m for options.  
    'Dec'   - J2000.0 Dec. Default is '+20:00:00.0'.  
    See convertdms.m for options.  
    'FOV'   - Field of view in arcsec in which to query the USNO-B  
    catalog. Default is 1000.  
    Output : - Structure containing the S/N properties. The following fields  
    are available:  
    .Mag    - Magnitude or limiting mag.  
    .SN     - S/N.  
    .VarRatio - Contribution of the various noise to the total  
    variance.  
    .BestPhotRad - Optimal photometric aperture radius [arcsec].  
    .FracInCentralPix - Fraction of flux in central pixel.  
    .SaturationMag - Saturation magnitude.  
    .SNrad - S/N as a function of photometric radius.  
    .PhotRad - photometric radius [arcsec].  
    - Structure containing additional parameters.  
    - Structure containing the stellar density in the field.  
    This is calculated using the USNO-B1 R2 mag, and the colors  
    are B2-R2.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [SN,OP,S]=telescope.sn.sn_calc('ExpTime',1./30,'SN',10); SN.Mag, OP.PixScale, OP.totFOVarea, OP.totFOVarea.*S.StarDensity  
    [SN,OP]=telescope.sn.sn_calc('Mag',[16;17]);  
    [SN,OP]=telescope.sn.sn_calc('SN',[5;10]);  
    Reliable: 2  
      
      
      
### telescope.sn.sn_det2signal

Convert detection S/N of a Gaussian PSF to signal Package: telescope.sn Description: Given detection S/N calculate the PSF signal.


    
    Convert detection S/N of a Gaussian PSF to signal  
    Package: telescope.sn  
    Description: Given detection S/N calculate the PSF signal.  
    Input  : - S/N for detection  
    - Width of Gaussian PSF (sigma in pix).  
    - Background [e-]  
    - Readnoise [e-].  
    Output : - Signal [e-]  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Sep 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: S=telescope.sn.sn_det2signal(10,2,100,0)  
    Reliable: 2  
      
      
### telescope.sn.sn_det_psf

Calculate S/N for detection of a Gaussian PSF Package: telescope.sn Description: Calculate the S/N (signal-to-noise ratio) for a point source with a symmetric Gaussian profile for PSF (optimal)


    
    Calculate S/N for detection of a Gaussian PSF  
    Package: telescope.sn  
    Description: Calculate the S/N (signal-to-noise ratio) for a point  
    source with a symmetric Gaussian profile for PSF (optimal)  
    detection.  
    Note this is different than PSF photometry (see  
    sn_psf_phot.m).  
    Input  : - Source signal [electrons].  
    - Sigma of PSF [pixels].  
    - Background [e/pix].  
    - Readout noise [e].  
    - PSF radius [pix]. Default is 20.  
    Output : - Theoretical S/N of PSF photometry (with radius  
    from 0 to infinity).  
    - S/N of PSF photometry with PSF truncated at Radius.  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Aug 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: SN=telescope.sn.sn_det_psf(1000,1,500,10)  
    Reliable: 2  
      
      
### telescope.sn.sn_phot2signal

Convert photometry S/N of a Gaussian source to signal Description: Given a target S/N, image background, readnoise and PSF Gaussian sigma, calculate the total count in PSF that will give the target S/N for PSF photometry.


    
    Convert photometry S/N of a Gaussian source to signal  
    Description: Given a target S/N, image background, readnoise and PSF  
    Gaussian sigma, calculate the total count in PSF that  
    will give the target S/N for PSF photometry.  
    Input  : - Target S/N.  
    - Sigma of PSF [pixels].  
    - Background [e/pix].  
    - Readout noise [e].  
    - PSF radius [pix]. Default is 20.  
    Output : - Source signal [electrons].  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Apr 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: S=telescope.sn.sn_phot2signal(5,2,300,5)  
    Reliable: 2  
      
      
### telescope.sn.sn_phot_aper

Calculate the aperture photometry S/N for a Gaussain source Package: telescope.sn Description: Calculate the S/N (signal-to-noise ratio) for a point source with a symmetric Gaussian profile for aperture


    
    Calculate the aperture photometry S/N for a Gaussain source  
    Package: telescope.sn  
    Description: Calculate the S/N (signal-to-noise ratio) for a point  
    source with a symmetric Gaussian profile for aperture  
    photometry.  
    Input  : - Source signal [electrons].  
    - Sigma of PSF [pixels].  
    - Background [e/pix].  
    - Readout noise [e].  
    - Aperture photometry radius [pix].  
    Output : - Theoretical S/N of aperture photometry.  
    See also: sn_psf_phot.m, optimal_phot_aperture.m  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Apr 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: SN=telescope.sn.sn_phot_aper(1000,2,100,10,5)  
    Reliable: 2  
      
      
### telescope.sn.sn_phot_psf

Calculate photometry S/N of a Gaussian PSF Description: Calculate the S/N (signal-to-noise ratio) for a point source with a symmetric Gaussian profile for PSF (optimal) photometry.


    
    Calculate photometry S/N of a Gaussian PSF  
    Description: Calculate the S/N (signal-to-noise ratio) for a point  
    source with a symmetric Gaussian profile for PSF (optimal)  
    photometry.  
    Input  : - Source signal [electrons].  
    - Sigma of PSF [pixels].  
    - Background [e/pix].  
    - Readout noise [e].  
    - PSF radius [pix]. Default is 20.  
    Output : - Theoretical S/N of PSF photometry (with radius  
    from 0 to infinity).  
    - S/N of PSF photometry with PSF truncated at Radius.  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Mar 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: SN=telescope.sn.sn_phot_psf(1000,1,500,10)  
    Reliable: 2  
      
      
### telescope.sn.sn_phot_psfn

Calculate photometry S/N of a numerical PSF Description: Calculate the S/N (signal-to-noise ratio) for a numerical PSF (optimal) photometry.


    
    Calculate photometry S/N of a numerical PSF  
    Description: Calculate the S/N (signal-to-noise ratio) for a numerical  
    PSF (optimal) photometry.  
    Input  : - Flux normalization.  
    - PSF stamp normalized to 1.  
    Alternatively, this can be a cell array of PSFs.  
    - The background variance (including background and readnoise).  
    If PSF is a cell array of PSFs, this need to be a cell array  
    of variances of the same length.  
    Output : - Theoretical S/N of PSF photometry.  
    See also: sn_psf_phot.m  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Apr 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: SN=telescope.sn.sn_phot_psfn(1000,VecPSF{1},VecStd(1).^2)  
    Reliable: 2  
      
      
### telescope.sn.sn_spec

S/N calculator for long-slit spectra Package: telescope.sn Description: Simulate long-slit spectral observations and estimate the S/N per resolution element.


    
    S/N calculator for long-slit spectra  
    Package: telescope.sn  
    Description: Simulate long-slit spectral observations and estimate the  
    S/N per resolution element.  
    Input  : - Spectrum [Wave(Ang), Flux(cgs/A)] or an AstSpec object  
    containing spectrum. Default is 'QSO_SDSS' at mag 20.0  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'RN'      - Readout noise [e-]. Default is 10.  
    'Gain'    - CCD gain [e-/ADU] (for digitization noise).  
    Default is 1.5.  
    'ExpTime' - Total exposure time [s]. Default is 600.  
    'SubExp'  - Number of sub exposure within total exposure time.  
    Default is 1.  
    'DC'      - CCD dark curent [e-/pix/s]. Default is 1e-3.  
    'Back'    - A background spectrum [Wave, Flux(cgs/A/arcsec^2)]  
    Either an AstSpec object a matrix or background  
    catalog name in the cats.spec.SkyBack pacakge.  
    Default is 'Gemini_SkyBack_dark'.  
    'PixScale' -Pixel scale in spatial direction [arcsec/pix].  
    Default is 0.6.  
    'Seeing'   -Seeing FWHM [arcsec]. Default is 1.2.  
    'SpecRes'  -  
    'SpecSampling' -  
    'AirMass'  -  
    'AtmosphericExt'  
    'Aper'  
    'TelTh'  
    'SpecTh'  
    'QE'  
    'ExtractionSemiWidth'  
    'SpecRange'  
    'SpecFluxUnits'        = 'cgs/A';  
    'BackFluxUnits'        = 'cgs/A';  
    'SpecWaveUnits'        = 'A';  
    'BackWaveUnits'        = 'A';  
    'ColW'                 = 1;  
    'ColF'                 = 2;  
    'InterpMethod'         = 'linear';  
    Output : - A structure array with the following fields:  
    'Wave' - A vector of wavelength (Ang) at which the S/N is  
    calculated. This vector is is given by the spectral  
    sampling.  
    'SNperResEl' - S/N per resolution element as extracted using  
    optimal extraction. Note that this is not the Horne  
    et al. extraction.  
    'ResEl' - Resolution element (Ang).  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Spec = AstSpec.get_galspec('Gal_E');  
    Spec = scale2mag(Spec,20.5);  
    Spec.synphot('SDSS','r','AB')  
    SN=telescope.sn.sn_spec(Spec)  
    Reliable: Under tests  
      
      
### telescope.sn.snr

Signal-to-Noise ratio calculator using full spectral components. Package: telescope.sn Description: Given the spectra of the source and background components, calculate the S/N for PSF detection.


    
    Signal-to-Noise ratio calculator using full spectral components.  
    Package: telescope.sn  
    Description: Given the spectra of the source and background components,  
    calculate the S/N for PSF detection.  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Nov 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [SN]=telescope.sn.snr  
    Reliable: 2  
      
      
### telescope.sn.spec2photons

Spectrum to photon counts in band. Package: telescope.sn Description: Given a spectrum and the effective area of an instrument as a function of wavelength, calculate the the total recieved


    
    Spectrum to photon counts in band.  
    Package: telescope.sn  
    Description: Given a spectrum and the effective area of an instrument as  
    a function of wavelength, calculate the the total recieved  
    flux and the photons count rate in the instrument.  
    Input  : - Spectra of the source (emitted from a cm^2 on the source):  
    [Wavelengh(Ang), Emmitence(erg cm^-2 s^-1 A^-1)].  
    Alternatively, a scalar representing a black-body  
    temperature [K].  
    Alternatively this can be an AstSpec object.  
    - Filter [Wavelength(Ang), EffectiveAreaOfInstrument(cm^2)].  
    If two element vector than assumes it to be the wavelength  
    range [Ang] of a top-hat filter with efective area of 1 cm^2.  
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
      
### telescope.sn.volumetric_rate_sn

SHORT DESCRIPTION HERE Package: telescope.sn Description:


    
    SHORT DESCRIPTION HERE  
    Package: telescope.sn  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [d]=telescope.sn.volumetric_rate_sn(varargin)  
    Reliable:  
      
      
      
      
