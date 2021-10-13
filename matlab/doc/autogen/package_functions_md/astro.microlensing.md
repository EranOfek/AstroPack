# Package: astro.microlensing


### astro.microlensing.chi2_microlensing

chi^2 between microlensing observations and model Package: AstroUtil.microlensing Description: Calculate a \chi^2 for between a microlensing event light curve and a theoretical model light curve given microlensing parameters.


    
    chi^2 between microlensing observations and model  
    Package: AstroUtil.microlensing  
    Description: Calculate a \chi^2 for between a microlensing event  
    light curve and a theoretical model light curve  
    given microlensing parameters.  
    Input  : - Parameters [T0, Beta, V, Alpha, BaseMag, SourceSize].  
    Where SourceSize is optional source size in Einstein  
    radius units. If given, the use psfs_microlensing.m  
    else use ps_microlensing.m.  
    - Light curve [JD, Mag, Err].  
    - Optional: Number of elements on the radius of the finite source,  
    in which to divide it when integrating its luminosity.  
    - Function, L=f(R,...), that given matrix R (radii) in units  
    of the Einstein radius and additional parameters, return the  
    luminosity of the finite source. The luminosity returned  
    by this function should be normalized such that the  
    integrated luminosity from the finite source is equal to  
    the base magnitude.  
    Default is to use a flat luminosity circular finite source.  
    * Arbitrary number of additional parameters to be passed to  
    the function that calculate the luminosity as a function  
    of position of the finite source.  
    Output : - Chi^2  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                   October 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    -  
      
### astro.microlensing.microlens_ps

Microlening light curve for a point source as a function of time Package: AstroUtil.microlensing Description: Calculate the microlensing light curve for a point source, as a function of time (assuming linear motion).


    
    Microlening light curve for a point source as a function of time  
    Package: AstroUtil.microlensing  
    Description: Calculate the microlensing light curve for a point source,  
    as a function of time (assuming linear motion).  
    Input  : - Either vector of free parameters  
    [T0,Beta,V,Alpha,BaseMag]  
    or a matrix of the same free parameters,  
    or a structure array with these fields and parameters.  
    T0 is time of min. impact parameters [days].  
    Beta is the impact parameter in units of the Einstein radius.  
    V is the relative velocity (e.g., Einstein radius per day).  
    Alpha is the blending parameter, 0<Alpha<=1, Alpha=1 if no blending.  
    and BaseMag is the base line magnitude [mag].  
    Default is [0, 0.05, 0.1, 1, 18].  
    - Vector of times. Default is (-20:0.1:20).'.  
    Output : - Magnitude for each time.  
    - Structure array of additional parameters for each time.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Feb 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mag,Res]=AstroUtil.microlensing.microlens_ps; plot(Res.T,Mag);  
    Reliable: 2  
      
      
### astro.microlensing.microlens_psb

Microlensing lightcurve for a point source as a function of angular dist Package: AstroUtil.microlensing Description: Calculate the microlensing light curve for a point source, in a list of a given source-lens distances.


    
    Microlensing lightcurve for a point source as a function of angular dist  
    Package: AstroUtil.microlensing  
    Description: Calculate the microlensing light curve for a point source,  
    in a list of a given source-lens distances.  
    Input  : - Either vector of free parameters  
    [Alpha,BaseMag]  
    or a matrix of the same free parameters,  
    or a structure array with these fields and parameters.  
    Alpha is the blending parameter, 0<Alpha<=1, Alpha=1 if no blending.  
    and BaseMag is the base line magnitude [mag].  
    Default is [1, 18].  
    - Vector of lens-source distances in units of the Einstein  
    radius. Default is (-2:0.01:2).'.  
    Output : - Magnitude for each time.  
    - Structure array of additional parameters for each time.  
    .BetaT  - The source-lens distances for which the parameters  
    were calculated (Einstein radius units).  
    .Mu     - Total magnification.  
    .Mu1    - 1st image magnification.  
    .Mu2    - 2nd image magnification.  
    .Theta1 - The position of the 1st image in units of the  
    Einstein radius, as measured relative to the  
    lens.  
    .Theta2 - The position of the 2nd image in units of the  
    Einstein radius, as measured relative to the  
    lens.  
    .Theta  - The flux weighted combined image position.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mag,Res]=microlens_psb; plot(Res.BetaT,Mag);  
    Reliable: 2  
      
      
      
### astro.microlensing.microlens_psfs

Microlening light curve with finite source effect Package: AstroUtil.microlensing Description: Calculate the microlensing light curve for a point source.


    
    Microlening light curve with finite source effect  
    Package: AstroUtil.microlensing  
    Description: Calculate the microlensing light curve for a point source.  
    Input  : - Either vector of free parameters  
    [T0,Beta,V,Alpha,BaseMag, FS]  
    or a matrix of the same free parameters,  
    or a structure array with these fields and parameters.  
    T0 is time of min. impact parameters [days].  
    Beta is the impact parameter in units of the Einstein radius.  
    V is the relative velocity (e.g., Einstein radius per day).  
    Alpha is the blending parameter, 0<Alpha<=1, Alpha=1 if no blending.  
    BaseMag is the base line magnitude [mag].  
    FS is the finite source radius in units of the Einstein  
    radius.  
    Default is [0, 0.05, 0.1, 1, 18, 0.1].  
    - Vector of times. Default is (-20:0.1:20).'.  
    - Function handle for a limb darkning function.  
    I=Fun(Radius); where Radius is between 0 and 1, and I  
    is the intensity in the range of 0 and 1.  
    Default is flat function (@(r)1).  
    - Number of integration grid point along the radius.  
    Default is 10.  
    Output : - Magnitude for each time.  
    - Structure array of additional parameters for each time.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Feb 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mag,Res]=microlens_psfs; plot(Res.T,Mag);  
    Reliable: 2  
      
      
### astro.microlensing.microlens_pspar

Microlening light curve including annual parallax Package: AstroUtil.microlensing Description: Calculate the microlensing light curve for a point source, as a function of time include annual parallax effects.


    
    Microlening light curve including annual parallax  
    Package: AstroUtil.microlensing  
    Description: Calculate the microlensing light curve for a point source,  
    as a function of time include annual parallax effects.  
    Input  : - A vector of free parameters  
    [T0,ER,RA,Dec,Beta_ra,Beta_dec,Mu_ra,Mu_dec,Par_ls,Alpha,BaseMag].  
    T0 - the JD of minimum Barycentric impact parameter.  
    ER - Einstein radius in consistent units (e.g., mas).  
    RA - J2000 RA of source [rad].  
    Dec - J2000 Dec of source [rad].  
    Beta_ra - Minimum impact parameter in the RA axis in  
    consistent units (e.g., mas).  
    Beta_dec - Minimum impact parameter in the Dec axis in  
    consistent units (e.g., mas).  
    Mu_ra - Proper motion in ra in consistent units  
    (e.g., mas/day).  
    Mu_dec - Proper motion in dec in consistent units  
    (e.g., mas/day).  
    Par_ls - parallax of lens - parallax of source in consistent  
    units (e.g., mas).  
    Alpha - the blending parameter, 0<Alpha<=1, Alpha=1 if no blending.  
    BaseMag - the base line magnitude [mag].  
    - Vector of JDs in which to calculate the light curve.  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'EpemType' - Which formulae to use in order to calculate  
    the observer Barycentric position.  
    'ple' - use ple_earth.m (default).  
    Output : - Magnitude for each time.  
    - Structure array of additional parameters for each time.  
    .BetaT  - The source-lens distances for which the parameters  
    were calculated (Einstein radius units).  
    .Mu     - Total magnification.  
    .Mu1    - 1st image magnification.  
    .Mu2    - 2nd image magnification.  
    .Theta1 - The position of the 1st image in units of the  
    Einstein radius, as measured relative to the  
    lens.  
    .Theta1 - The position of the 2nd image in units of the  
    Einstein radius, as measured relative to the  
    lens.  
    .Theta  - The flux weighted combined image position.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: JD = 2451545+[-10:0.01:10].';  
    [Mag,Res]=AstroUtil.microlensing.microlens_pspar([2451545 0.001 4.71 -0.35 0.0001 0.0001 0.0001 0.0001 0.0001 1 18],JD);  
    [Mag1,Res1]=AstroUtil.microlensing.microlens_pspar([2451545 0.001 4.71 -0.35 0.0001 0.0001 0.0001 0.0001 0.000001 1 18],JD);  
    plot(JD,Mag); hold on; plot(JD,Mag1,'r-'); plot.invy  
    Reliable: 2  
      
      
### astro.microlensing.microlens_template

SHORT DESCRIPTION HERE Package: AstroUtil Description:


    
    SHORT DESCRIPTION HERE  
    Package: AstroUtil  
    Description:  
    Input  : - Vector of times.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    't0' - Vector of all possible t0 for which to generate  
    templates.  
    'tE' - Vector of all possible Einstein radius crossing times  
    for which to generate templates.  
    'beta' - Vector of all possible beta (minimal impact  
    parameters) for which to generate templates.  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### astro.microlensing.ml_filterbank

Generate microlensing template bank Package: AstroUtil Description: Generate microlensing template bank


    
    Generate microlensing template bank  
    Package: AstroUtil  
    Description: Generate microlensing template bank  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    't' - Vector of times. Default is (-100:1:100).'.  
    'VecBeta' - Vector of minmal impact parameters.  
    Default is (0.1:0.05:1).^2.'.  
    'VecV'    - Vector of velocty in Einstein radius per day.  
    Default is logspace(-3,-0.5,25).'.  
    'OutMag'  - Output is in magnitude scaling. Default is true.  
    Output : - Matrix of templates bank.  
    Each column correspond to one template.  
    - Structure containing value of parameters in each column of the  
    template bank.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [F,ParsML]=AstroUtil.microlensing.ml_filterbank;  
    Reliable:  
      
      
      
      
### astro.microlensing.pointsource_lens

Microleninsg magnification, images positions and time delays (OBSOLETE) Package: AstroUtil.microlensing Description: Calculate the magnification and images positions for a point source lens, given the lens properties and impact parameter. OBSOLETE, use ps_lens instead.


    
    Microleninsg magnification, images positions and time delays (OBSOLETE)  
    Package: AstroUtil.microlensing  
    Description: Calculate the magnification and images positions for a  
    point source lens, given the lens properties and impact  
    parameter. OBSOLETE, use ps_lens instead.  
    Input  : - Deflector mass [solar mass].  
    - D_l [pc] or z_l.  
    - D_s [pc] or z_s.  
    - D_ls [pc] or z_ls.  
    - Impact parameter [radians].  
    - 0 (default) if distances are given and 1 for redshift.  
    Output : - Einstein radius [radians].  
    - Theta_1 [radians] in respect to the lens.  
    - Theta_2 [radians] in respect to the lens.  
    - Mu_1  
    - Mu_2  
    - Time delay 1  
    - Time delay 2  
    - Theta center of mass of images.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [ER,T1,T2,Mu1,Mu2,TD1,TD2,Tcm]=AstroUtil.microlensing.pointsource_lens(1,5000,10000,5000,1./(1000.*RAD.*3600));  
    -  
### astro.microlensing.ps_lens

Calculate deflection, magnification and time delay for point mass lens Package: astro Description: Calculate deflection, magnification and time delay for point mass lens.


    
    Calculate deflection, magnification and time delay for point mass lens  
    Package: astro  
    Description: Calculate deflection, magnification and time delay for point  
    mass lens.  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Mass' - Lens mass. Default is 1.  
    'MassUnits' - Lens mass units. Default is 'SunM'  
    'Dl'   - Lens distance or redshift. Default is 5000;  
    'Ds'   - Source distance or redshift. Default is 1e4;  
    'DistUnits' - Distance units. Default is 'pc'.  
    'Beta' - Impact parameter. Default is logspace(-3,3,7)'.  
    'BetaUnits' - Impact parameter units. Default is 'ThetaE'.  
    'BetaMin' - Minimum impact parameter. Default is 0.  
    If provided than the lens source distance is  
    calculated from sqrt(Beta^2 + BetaMin^2). BetaMin  
    has the units specified in BetaUnits.  
    'OutUnits' - Units of angular output. Default is 'arcsec'.  
    Output : - A structure containing the microlensing properties.  
    - A structure of useful functions.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=astro.microlensing.ps_lens  
    Reliable: 2  
      
      
      
### astro.microlensing.radial_astrometric_microlensing

The astrometric deflection of the primary microleneing image. Package: AstroUtil.microlensing Description: Calculate the astrometric deflection of the primary microlensing image as a function of time.


    
    The astrometric deflection of the primary microleneing image.  
    Package: AstroUtil.microlensing  
    Description: Calculate the astrometric deflection of the primary  
    microlensing image as a function of time.  
    Input  : - Deflector mass [solar mass].  
    - D_l [pc].  
    - D_s [pc].  
    - D_ls [pc].  
    - Minimum impact parameter at T0: Beta0 [arcsec].  
    - Proper motion [arcsec/unit time].  
    - Vector of times [unit time].  
    - T0 [unit time].  
    Output : - Shifts in arcsec as a function of time T.  
    - A structure array with the following fields:  
    'ER' - The Einstein radius [arcsec].  
    'BetaT' - Vector of deflector source distance [arcsec].  
    'T'  - Vector of times.;  
      
      
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [S,Res]=AstroUtil.microlensing.radial_astrometric_microlensing  
    Reliable: 2  
      
      
### astro.microlensing.shapiro_delay

Calculate the Shapiri time delay approximation (beta>ThetaE) Package: AstroUtil.microlensing Description: Calculate the Shapiri time delay approximation assuming the angle between the lens and the source is much larger than the Einstein radius.


    
    Calculate the Shapiri time delay approximation (beta>ThetaE)  
    Package: AstroUtil.microlensing  
    Description: Calculate the Shapiri time delay approximation assuming  
    the angle between the lens and the source is much larger  
    than the Einstein radius.  
    Input  : - Angle (radians) between source and lens.  
    - Lens mass [solar mass]. Default is 1.  
    Output : - Time delay [seconds]  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Dt=AstroUtil.microlensing.shapiro_delay(0.001);  
    Reliable: 2  
      
      
### astro.microlensing.shapiro_delay_pm

The Shaprio time delay given proper motion between two stars. Package: AstroUtil.microlensing Description: The Shaprio time delay given proper motion between two stars.


    
    The Shaprio time delay given proper motion between two stars.  
    Package: AstroUtil.microlensing  
    Description: The Shaprio time delay given proper motion between two stars.  
    Input  : - time [Julian years].  
    - time of minimum impact parameter [Julian years].  
    - minimum impact parameter [arcsec].  
    - relative proper motion [mas/yr].  
    - Lens mass [solar mass].  
    Output : - Time delay [s].  
    - 1st time derivative of time delay.  
    - 2nd time derivative.  
    - 3rd time derivative.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [Dt,Dt_dt1,Dt_dt2,Dt_dt3]=AstroUtil.microlensing.shapiro_delay_pm(t,t0,bm,mu,M)  
    Reliable:  
      
### astro.microlensing.stoch_time_delay

SHORT DESCRIPTION HERE Package: AstroUtil.microlensing Description:


    
    SHORT DESCRIPTION HERE  
    Package: AstroUtil.microlensing  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
      
