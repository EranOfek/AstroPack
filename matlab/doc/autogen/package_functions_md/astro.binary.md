# Package: astro.binary


### astro.binary.astrometric_binary

Predict astrometric binary position Package: AstroUtil.binary Description: Given orbital elements of an astrometric binary, predicts its sky position as a function of time.


    
    Predict astrometric binary position  
    Package: AstroUtil.binary  
    Description: Given orbital elements of an astrometric binary, predicts  
    its sky position as a function of time.  
    Input  : - JD or dates (see julday.m for options) in which to calculate  
    the binary position.  
    - Binary orbital elements. This is either a vector of  
    [Period (days), a (arcsec), e, T (JD),...  
    Omega (rad),omega (rad) ,i (rad)],  
    or a structure containing the following fields:  
    .Period, .a, .e, .T, .Omega, .omega, .i.  
    Default is [365 1 0.5 1 1 1 1].  
    Output : - Structue containing the orbit position as a function of time.  
    The following fields are available:  
    .JD - JD  
    .X, .Y, .Z - positions.  
    - Structure containing the following additional parameters:  
    .Nu   - true anomaly  
    .R    - radius vector  
    .E    - Eccentric anomaly  
    .dNudt- d\nu/dt  
    Tested : Maylab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Orbit,Par]=AstroUtil.binary.astrometric_binary; plot(Orbit.X,Orbit.Y,'.');  
    Reliable: 2  
      
      
### astro.binary.binary_reflection_effect

reflection effect from a star with unit illumination on mirror Package: AstroUtil.binary Description: Calculate the reflection effect from a star with unit illumination on mirror (star) with radius R2.


    
    reflection effect from a star with unit illumination on mirror  
    Package: AstroUtil.binary  
    Description: Calculate the reflection effect from a star with unit  
    illumination on mirror (star) with radius R2.  
    Input  : - Mirror radius [consistent length unit].  
    - Distance between stars [consistent length unit].  
    - Phase angle, observer-mirror-star, [radians].  
    - Model parameters vector:  
    Output : - The fraction of reflected luminosity (from L1, by L2).  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
      
      
### astro.binary.binary_rv

Binary star radial velocity (RV) Package : AstroUtil.binary Description: Calculate binary star radial velocity as a function of time.


    
    Binary star radial velocity (RV)  
    Package : AstroUtil.binary  
    Description: Calculate binary star radial velocity as a function of  
    time.  
    Input  : - Time (days).  
    - Period (days)/  
    - Time of periastron (days).  
    Default is 0.  
    - Periastron distance [q=a*(1-e)] (AU). Default is 1.  
    - Orbital eccentricity (0<=e<1). Default is 0.  
    - Orbital inclination (radians). Default is pi./2.  
    - Longitude of the periastron (radians). Default is 0.  
    Output : - Radial velocity [cm/s].  
    - K2 (secondary amplitude) [AU/day].  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: t=[0:1:365].'; RV=AstroUtil.binary.binary_rv(t,365);  
    e=0.3; a=1; RV=AstroUtil.binary.binary_rv(t,365,0,a./(1-e),e,85./RAD,100);  
    Reliable: 2  
      
      
      
### astro.binary.chi2_astrometric_binary

Find astrometric binary elements from observations Package: AstroUtil.binary Description: Given the astrometric observations of a binary and its eccentricity, Time of periastron and period, fit linearly the remaining four orbital elements (omega, Omega, i, a).


    
    Find astrometric binary elements from observations  
    Package: AstroUtil.binary  
    Description: Given the astrometric observations of a binary and its  
    eccentricity, Time of periastron and period, fit linearly  
    the remaining four orbital elements (omega, Omega, i, a).  
    This function is used by fit_astrometric_binary.m  
    Input  : - Two elements vector of [e, T].  
    - Astrometric binary trial period.  
    - Vector of time of observations (days).  
    If parallax is fitted than this parameter must be in JD.  
    - Column vector of observed X positions.  
    - Coloum vector of observed Y positions.  
    - Column vector of errors in X positions. Default is empty [].  
    - Column vector of errors in Y positions. Default is empty [].  
    - Fit proper motion {true|false}. Default is false.  
    - Fit parallax {true|false}. Default is false.  
    - Mean J2000 RA [rad]. Required if fitting parallax.  
    - Mean J2000 Dec [rad]. Required if fitting parallax.  
    - Earth barycentric [X, Y, Z] position [au] in ecliptic rectangular  
    coordinates, referred to equinox and ecliptic of J2000.0.  
    Output : - Chi2 of best fit.  
    - Structure of best fit parameters.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: fit_astrometric_binary.m; astrometric_binary.m  
    Example: [Orbit,Par]=AstroUtil.binary.astrometric_binary;  
    [Chi2,Res]=AstroUtil.binary.chi2_astrometric_binary([365,0.5,1],Orbit.JD,Orbit.X,Orbit.Y,[],[]);  
    Reliable: 2  
      
### astro.binary.coalescence_gw_time

Calculate the coalescence time for a binary stars due to GW emission Package: +AstroUtil.binary


    
    Calculate the coalescence time for a binary stars due to GW emission  
    Package: +AstroUtil.binary  
    Input  : - M1 [Solar mass]  
    - M2 [Solar mass]  
    - a [cm]  
    - e. Default is 0. (Correct for low e only).  
    Output : - Coalescensc time [s]  
    - da/dt [cm/s]  
    - de/dt [s^-1]  
    Example: [t_coal,da_dt,de_dt]=AstroUtil.binary.coalescence_gw_time(M1,M2,a,e)  
      
### astro.binary.eb_demo

GUI Eclipsing Binary light-curve demo. Package: AstroUtil.binary Description: GUI Eclipsing Binary light-curve demo.


    
    GUI Eclipsing Binary light-curve demo.  
    Package: AstroUtil.binary  
    Description: GUI Eclipsing Binary light-curve demo.  
    Input  : null  
    Output : null  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: eb_demo  
    Reliable: 2  
      
      
### astro.binary.eb_light_curve

Eclipsing binary light curve as a function of time. Package: AstroUtil.binary Description: Calculate eclipsing binary light curve as a function of time.


    
    Eclipsing binary light curve as a function of time.  
    Package: AstroUtil.binary  
    Description: Calculate eclipsing binary light curve as a function of time.  
    Input  : - Stars luminosities, [L1, L2].  
    - Stars mass, [M1, M2]. in solar mass.  
    - Stars radii, [R1, R2]. in au.  
    - Orbital elements : [T, q, e, Om, w, i]  
    Units : days, au, [], radians, radians, radinas respectively.  
    - Limb-darkening function, default is 'limb_darkening'.  
    - Parameters for Primary-star limb-darkening pars.  
    Default is {'Milne',1}.  
    - Parameters for Secondary-star limb-darkening pars.  
    Default is {'Milne',1}.  
    - Parameters for Primary-star reflection-effect pars.  
    Default is [0;0;0].  
    - Parameters for Secondary-star reflection-effect pars.  
    Default is [0;0;0].  
    Output : - Light curve, two column matrix [JD, LC].  
    LC is given in relative magnitude.  
    - The period in days, as calculated from the input parameters.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [LC,Period]=AstroUtil.binary.eb_light_curve([100 20],[1 0.6],[0.01 0.01],[0 0.3 0.2 0 0 pi./2],'AstroUtil.binary.limb_darkening');  
    Reliable: 2  
      
### astro.binary.equipot

Calculate the gravitational potential of a binary star on a grid. Package: AstroUtil.stars Description: Calculate two body equipotanials map.


    
    Calculate the gravitational potential of a binary star on a grid.  
    Package: AstroUtil.stars  
    Description: Calculate two body equipotanials map.  
    Input  : - m1 : first star mass in solar mass.  
    - m2 : second star mass in solar mass.  
    - a : the separation between the two stars. in meters.  
    - s : scaling the result from min to max in units of  
    the staller separation.  
    - n : number of point in each axis. default is 15.  
    - z : the surface to work on. default is z=0 (orbital plane)  
    Output : - grid of x coordinate.  
    - grid of y coordinate.  
    - matrix of potential defined by the x/y grids.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [x,y,q]=AstroUtil.binary.equipot(1,0.7,0.7.*1e9,3,50);  
    mesh(x,y,q);  
    -  
      
### astro.binary.fit_astrometric_binary

Fit an elliptical-orbit binary orbit to astrometric data. Package: AstroUtil.binary Description: Fit an elliptical-orbit binary orbit to astrometric data. The fit has 7 free parameters, 4 of which (omega, Omega, i, and a) are fitted linearly while two (T, e) are fitted


    
    Fit an elliptical-orbit binary orbit to astrometric data.  
    Package: AstroUtil.binary  
    Description: Fit an elliptical-orbit binary orbit to astrometric data.  
    The fit has 7 free parameters, 4 of which (omega, Omega, i,  
    and a) are fitted linearly while two (T, e) are fitted  
    non-linaerly and one (Period) is scanned.  
    Input  : - Column vector of times of the observations (days).  
    If parallax is fitted than this parameter must be in JD.  
    - Column vector of observed X positions.  
    - Coloum vector of observed Y positions.  
    - Column vector of errors in X positions. Default is empty [].  
    - Column vector of errors in Y positions. Default is empty [].  
    - Vector frequencies (1/days) in which to search for  
    binary-period solution. Default is (0.001:0.0001:0.01).'  
    corresponding to search between 100 to 1000 days.  
    - Mean J2000 RA [rad]. Required if fitting parallax.  
    - Mean J2000 Dec [rad]. Required if fitting parallax.  
    Output : - Structure containing the best fit parameters. The following  
    fields are available:  
    .Par  - Linear fitted parameters:  
    [a, omega, Omega, i, MeanPos_x, PM_x, MeanPos_y, PM_y,  
    Plx].  
    .ParErr - Error in linear fitted parameters.  
    .RefTime - Reference epoch of the proper motion.  
    .El   - Structure of the binary orbital elements.  
    .Chi2 - Chi2 of best fit  
    .Dof  - Number of degrees of freedom.  
    .Npar - Number of free parameters in the model.  
    .ResidX  - Vector of X-axis residuals.  
    .ResidY  - Vector of Y-axis residuals.  
    .VecChi2 - Vector of Chi2 for each trial frequency.  
    .Freq    - Vector of (input) trial frequencies.  
    - Fit proper motion {true|false}. Default is false.  
    - Fit parallax {true|false}. Default is false.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: astrometric_binary.m; chi2_astrometric_binary.m  
    Example: [Orbit,Par]=astrometric_binary;  
    Freq  = 1./(330:1:400).';  
    BestFit=AstroUtil.binary.fit_astrometric_binary(Orbit.JD,Orbit.X,Orbit.Y,[],[],Freq);  
    Reliable: 2  
      
      
### astro.binary.fit_rv_ellipse

Fit radial velocity to ellipse as a function of period Package: AstroUtil.binary Description: Given radial velocity as a function of time and a trial period, fit an ellipse and calculate the RMS as a function of trial period.


    
    Fit radial velocity to ellipse as a function of period  
    Package: AstroUtil.binary  
    Description: Given radial velocity as a function of time and a trial  
    period, fit an ellipse and calculate the RMS as a function  
    of trial period.  
    Input  : - Vector of time.  
    - Vector of radial velocity  
    - Vector of trial periods.  
    Output : - Structure array of results. For each trial period return  
    the rms of the fit.  
    - Vector of frequencies (i.e., 1/period).  
    - Structure of best fit parameters.  
    Reference: Bhattacharyya & Nityanada (2008; MNRAS 387, 273-278)  
    By : Eran O. Ofek            Dec 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Res,FreqVec]=AstroUtil.binary.fit_rv_ellipse;  simulation mode  
    plot(FreqVec,[Res.RMS])  
    Reliable: 2  
      
### astro.binary.limb_darkening

Limb darkening function Package: AstroUtil.binary Description: Calculate the star luminosity per unit area as a function of its radius, and given the limb-darkening parameters.


    
    Limb darkening function  
    Package: AstroUtil.binary  
    Description: Calculate the star luminosity per unit area as a function  
    of its radius, and given the limb-darkening parameters.  
    Input  : - Radius [stellar radius unit].  
    - Limb darkning model. Options are:  
    'Milne' - Milne (1921)  [1 parameter]  
    Lum  = 1-Pars(1).*(1-Mu);  
    'Wade'  - Wade & Rucinski (1985)  [3 parameters]  
    Lum  = 1-Pars(1).*(1-Mu)-Pars(2).*(1-Mu).^Pars(3);  
    'Kling' - Klingesmith & Sobieski (1970)  [2 parameters]  
    Lum  = 1-Pars(1).*(1-Mu)-Pars(2).*Mu.*log(Mu);  
    'Diaz'  - Diaz-Cordoves & Gimenez (1992) [2 parameters]  
    Lum  = 1-Pars(1).*(1-Mu)-Pars(2).*(1-sqrt(Mu));  
    - Vector of parameters for the model.  
    Output : - Luminosity per unit area at radius, R. [consistent lum. unit].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Claret et al. 1995, A&ASS, 114, 247  
    Example: Lum=AstroUtil.binary.limb_darkening(0.1,'Diaz',[1 1]);  
    Reliable: 2  
      
### astro.binary.obstruction

Stellar obstruction due to the eclipse Package: AstroUtil.binary Description: Calculate stellar obstruction due to the eclipse given the stars radii, distance and limb darkening function of the background star.


    
    Stellar obstruction due to the eclipse  
    Package: AstroUtil.binary  
    Description: Calculate stellar obstruction due to the eclipse given the  
    stars radii, distance and limb darkening function of the  
    background star.  
    Input  : - Vector of distances between primary and secondary  
    [consistent length unit].  
    - Radius of background star [length unit].  
    - Radius of forground star [length unit].  
    - Number of steps in integration.  
    - limb-darkening function, returning the luminosity per unit area  
    as function of radius.  
    - Cell array of additional optional parameters for the  
    LimbFun function. Default is {'Milne',1}.  
    Output : - Vector of total obstraction of background star, for each distance  
    in distance vector [background-star luminosity].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Obs=AstroUtil.binary.obstruction(D,R1,R2,100,'limb_darkening',{'Kling',[1 0.5]});  
    Reliable: 2  
    -  
### astro.binary.plot_eb_lc

Plot eclipsing binary light curve as a function of time. Package: AstroUtil.binary Description: Plot eclipsing binary light curve as a function of time.


    
    Plot eclipsing binary light curve as a function of time.  
    Package: AstroUtil.binary  
    Description: Plot eclipsing binary light curve as a function of time.  
    Input  : - Stars luminosities, [L1, L2].  
    - Stars mass, [M1, M2]. in solar mass.  
    - Stars radii, [R1, R2]. in au.  
    - Orbital elements : [T, q, e, Om, w, i]  
    Units : days, au, [], radians, radians, radinas respectively.  
    - Limb-darkening function, default is 'limb_darkening'.  
    - Parameters for Primary-star limb-darkening pars.  
    Default is {'Milne',1}.  
    - Parameters for Secondary-star limb-darkening pars.  
    Default is {'Milne',1}.  
    - Parameters for Primary-star reflection-effect pars.  
    Default is [0;0;0].  
    - Parameters for Secondary-star reflection-effect pars.  
    Default is [0;0;0].  
    Output : - Light curve, two column matrix [JD, LC].  
    LC is given in relative magnitude.  
    - The period in days, as calculated from the input parameters.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.binary.plot_eb_lc([100 20],[1 0.6],[0.01 0.01],[0 0.3 0.2 0 0 pi./2],'AstroUtil.binary.limb_darkening');  
    Reliable: 2  
    -  
### astro.binary.plot_eb_lc_ph

Plot eclipsing binary light curve as a function of phase. Package: AstroUtil.binary Description: Plot eclipsing binary light curve as a function of phase.


    
    Plot eclipsing binary light curve as a function of phase.  
    Package: AstroUtil.binary  
    Description: Plot eclipsing binary light curve as a function of phase.  
    Input  : - Stars luminosities, [L1, L2].  
    - Stars mass, [M1, M2]. in solar mass.  
    - Stars radii, [R1, R2]. in au.  
    - Orbital elements : [T, q, e, Om, w, i]  
    Units : days, au, [], radians, radians, radinas respectively.  
    - Limb-darkening function, default is 'limb_darkening'.  
    - Parameters for Primary-star limb-darkening pars.  
    Default is {'Milne',1}.  
    - Parameters for Secondary-star limb-darkening pars.  
    Default is {'Milne',1}.  
    - Parameters for Primary-star reflection-effect pars.  
    Default is [0;0;0].  
    - Parameters for Secondary-star reflection-effect pars.  
    Default is [0;0;0].  
    Output : - Light curve, two column matrix [JD, LC].  
    LC is given in relative magnitude.  
    - The period in days, as calculated from the input parameters.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AstroUtil.binary.plot_eb_lc_ph([100 20],[1 0.6],[0.01 0.01],[0 0.3 0.2 0 0 pi./2],'AstroUtil.binary.limb_darkening');  
    Reliable: 2  
    -  
### astro.binary.rv2ellipse

Convert radial velocity as a function of time to ellipse Package: AstroUtil.binary Description: Given radial velocity as a function of time and a trial period convert it to a RV_Odd vs RV_Even (ellipse). I.e., for a good period plot(RV_Even,RV_Odd,'.') should be


    
    Convert radial velocity as a function of time to ellipse  
    Package: AstroUtil.binary  
    Description: Given radial velocity as a function of time and a trial  
    period convert it to a RV_Odd vs RV_Even (ellipse).  
    I.e., for a good period plot(RV_Even,RV_Odd,'.') should be  
    an ellipse.  
    Input  : - Vector of time.  
    - Vector of radial velocity  
    - Trial period  
    Output : - RV_Even  
    - RV_Odd  
    - Corresponding phase  
    - Corresponding time  
    Reference: Bhattacharyya & Nityanada (2008; MNRAS 387, 273-278)  
    By : Eran O. Ofek            Dec 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
      
    calculate phase [fraction]  
### astro.binary.total_light

Primary total light given its radius and limb darkening. Package: AstroUtil.binary Description: Calculate the primary total light given its radius and limb darkening function or constant luminosity per unit area.


    
    Primary total light given its radius and limb darkening.  
    Package: AstroUtil.binary  
    Description: Calculate the primary total light given its radius and  
    limb darkening function or constant luminosity per unit area.  
    Input  : - Primary star radius [consistent length unit].  
    - Name of limb-darkening function: L(r,P1,P2,...),  
    where r is distance from  
    the star center in units of its radius, and L is the luminosity  
    per unit area in this radius, r.  
    P1,P2,... are optional parameters of LimbFun function  
    If constant number is given then it is taken as a constant  
    luminosity per unit area.  
    - Number of steps in numerical integration,  
    in case of non-constant LimbFun.  
    Default is 100.  
    - Cell array of optional parameters for limb-darkening function.  
    Default is {'Milne',1}.  
    Output : - Total luminosity of star [consistent lum. unit].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: TotLum=AstroUtil.binary.total_light(1,'AstroUtil.binary.limb_darkening',100,{'Wade',[1 1 1]});  
    Reliable: 2  
    -  
