# Package: astro.cosmo


### astro.cosmo.ad_dist

Calculate the filled beam angular diameter distance between two redshifts Package: astro.cosmo Description: Calculate the filled beam angular diameter distance between two redshifts along the line of sight.


    
    Calculate the filled beam angular diameter distance between two redshifts  
    Package: astro.cosmo  
    Description: Calculate the filled beam angular diameter distance  
    between two redshifts along the line of sight.  
    Input  : - Two, or one columns matrix containing redshifts [z1 z2].  
    For each row, the angular diameter distance is  
    calculated between z1 and z2. If one column is given [z2],  
    then calculate the angular diameter distance from  
    z1=0 to z2.  
    - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'planck' (see cosmo_pars.m).  
    If a scalar is givne then assume its H_0 [km/s/Mpc].  
    - Optional Omega_{m}.  
    - Optional Omega_{\Lambda}.  
    Output : - Angular diameter distance [pc].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: astro.cosmo.ad_dist([1 2;1.5 2],'wmap3');  
    Reliable: 1  
      
### astro.cosmo.ad_dr_dist

Calculates the Dyer-Roeder angular diameter distance for the empty beam Package: astro.cosmo Description: Calculates the Dyer-Roeder angular diameter distance, for the empty beam case (for filled beam use: ad_dist.m), between two redshifts on the line of sight.


    
    Calculates the Dyer-Roeder angular diameter distance for the empty beam  
    Package: astro.cosmo  
    Description: Calculates the Dyer-Roeder angular diameter distance,  
    for the empty beam case (for filled beam use: ad_dist.m),  
    between two redshifts on the line of sight.  
    Input  : - Two, or one columns matrix containing redshifts [z1 z2].  
    For each row, the angular diameter distance is  
    calculated between z1 and z2. If one column is given [z2],  
    then calculate the angular diameter distance from  
    z1=0 to z2.  
    - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap9' (see cosmo_pars.m).  
    Output : - Vector of Angular diameter distance, in parsecs.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Dist=astro.cosmo.ad_dr_dist(0.6);  
      
      
### astro.cosmo.ad_q_dist

Angular diamater distance with quintessence Package: astro.cosmo Description: Compute filled-beam angular-diameter distance to an object in a flat Universe with constant equation of state (p=w\rho; i.e., quintessence).


    
    Angular diamater distance with quintessence  
    Package: astro.cosmo  
    Description: Compute filled-beam angular-diameter distance to an object  
    in a flat Universe with constant equation of state  
    (p=w\rho; i.e., quintessence).  
    Input  : - Vector of redshifts, if two column matrix is given,  
    then integrate the distance from the value in the first column  
    to the value in the second column.  
    - Cosmological parameters : [H0, \Omega_{m}, w],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    Output : - Angular-diameter distance [parsec].  
    Reference : Perlmutter et al. 1997 ApJ, 483, 565  
    Jain et al. (2002), astro-ph/0105551  
    Notes  : Previously called: ad_fq_dist.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2002  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: astro.cosmo.ad_q_dist(1);  
    Reliable: 2  
      
### astro.cosmo.cdt_dz

Calculate the differential cdt/dz in the FLRW geometry. Package: astro.cosmo Description: Calculate the differential cdt/dz in the FLRW geometry.


    
    Calculate the differential cdt/dz in the FLRW geometry.  
    Package: astro.cosmo  
    Description: Calculate the differential cdt/dz in the FLRW geometry.  
    Input  : - Vector of redshifts.  
    - Cosmological parameters: [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    Output : - The quantity cdt/dz in the FLRW geometry [cm].  
    Reference : Fukugita et al. 1992, ApJ 393, 3  
    Tested : Matlab 5.1  
    By : Eran O. Ofek             Jul 2001  
    URL : http://weizmann.ac.il/home/eofek/  
    Example: Diff=astro.cosmo.cdt_dz([1;2]);  
    Reliable: 1  
      
### astro.cosmo.comoving_dist

Comoving distance Package astro.cosmo Description: Calculate the line of sight comoving distance.


    
    Comoving distance  
    Package astro.cosmo  
    Description: Calculate the line of sight comoving distance.  
    Input  : - Redshift.  
    - Cosmological parameters:  
    - Cosmological parameters :  
    [H0, \Omega_{m}, \Omega_{\Lambda}, \Omega_{radiation}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    Default for Omega_radiation is 0.  
    Output : - Comoving distance [pc].  
    Reference : http://nedwww.ipac.caltech.edu/level5/Hogg/frames.html  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: ComovDist=astro.cosmo.comoving_dist([1;2]);  
    Reliable: 2  
      
      
### astro.cosmo.comoving_volume

Calculate the differential and comoving volume Package: astro.cosmo Description: Calculate the differential comoving volume (d\Omega dz) at redshift z and the total comoving volume from redshift 0 to z.


    
    Calculate the differential and comoving volume  
    Package: astro.cosmo  
    Description: Calculate the differential comoving volume (d\Omega dz)  
    at redshift z and the total comoving volume from redshift  
    0 to z.  
    Input  : - Redshift.  
    - Cosmological parameters:  
    [H0, \Omega_{m}, \Omega_{\Lambda}, \Omega_{radiation}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    Default for Omega_radiation is 0.  
    Output : - Differential comoving volume (d\Omega dz) at redshift z [pc^3]  
    - Comoving volume from redshift 0 to z [pc^3].  
    Reference : http://nedwww.ipac.caltech.edu/level5/Hogg/frames.html  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Aug 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [dVc,Vc]=astro.cosmo.comoving_volume([1;2]);  
    Reliable: 2  
      
      
### astro.cosmo.cosmo_pars

Cosmological parameters as measured by various experiments. Package: astro.cosmo Description: Return the cosmological parameters as measured by various experiments.


    
    Cosmological parameters as measured by various experiments.  
    Package: astro.cosmo  
    Description: Return the cosmological parameters as measured by  
    various experiments.  
    Input  : - Cosmological parameter source, options are:  
    'wmap3'    : WMAP+SNLS (Spergel et al. 2007).  
    'wmap5'    : WMAP+SN+BO (Komatsu et al. 2008).  
    'wmap9'    : WMAP9+BAO+H0 (Hinshaw et al. 2013).  
    'planck'   : planck full mission (Ade et al. 2015).  
    or alternatively a three or four element vector  
    containing: [H0, OmegaM, OmegaL OmegaRad], where  
    default for OmegaRad is 0.  
    If this parameter is a structure, return the structure  
    as output.  
    Output : - Structure containing cosmological parameters.  
    - Structure containing cosmological parameters errors  
    The tructure is identical to the parameter structure,  
    but containing two values for each parameter  
    [1sigma lower error, 1sigma upper error].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                     March 2007  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: [Par,ErrPar]=astro.cosmo.cosmo_pars('wmap3');  
    Reliable: 2  
      
      
### astro.cosmo.crit_surface_density

The critical surface density for gravitational lensing Package: astro.cosmo Description: Calculates the critical surface density for gravitational lensing, given the cosmology and redshifts of the lens and source.


    
    The critical surface density for gravitational lensing  
    Package: astro.cosmo  
    Description: Calculates the critical surface density for gravitational  
    lensing, given the cosmology and redshifts of the lens  
    and source.  
    Input  : - Redshifts vectors [[z_{l}], [z_{s}]], or distances in parsecs.  
    - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    - Distances type:  
    'norm' : normal angular diam. dist. - default.  
    'dr'   : Dyer-Roeder ang. diam. distance (empty beam case).  
    'pc'   : flat space (nearby sources) - In this case the input  
    is distances in parsecs (instead of redshift).  
    Output : - Critical surface density for gravitational lensing [gr/cm^2].  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                    Jul 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: SigmaCrit=astro.cosmo.crit_surface_density([1 2],'wmap3');  
    Reliable: 2  
      
### astro.cosmo.delta_vir_z

Calculate the virial overdensity \Delta_{vir} Package: astro.cosmo Description: Calculate the virial overdensity \Delta_{vir}, as a function of redshift z, and cosmological paramaeters.


    
    Calculate the virial overdensity \Delta_{vir}  
    Package: astro.cosmo  
    Description: Calculate the virial overdensity \Delta_{vir}, as a  
    function of redshift z, and cosmological paramaeters.  
    Input  : - Redshift vector.  
    - Parameters at zero redshift.  
    [\Omega_{m}, \Omega_{\Lambda}]  
    Output : - Vector of \Delta_{vir}(Z) corresponding to redshift vector.  
    Reference : Bryan & Norman 1998 ApJ 495, 80  
    Bullock et al. 2000 - astro-ph/9908159  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [DeltaVirZ]=astro.cosmo.delta_vir_z(0,[0.3 0.7]);  
    Reliable: 2  
      
      
### astro.cosmo.disp_measure

Calculate the dispersion measure along a cosmological line of sight Package: astro.cosmo Description: Calculate the dispersion measure along a cosmological line of sight as a function of redshift. Valid below redshift 3 (He re-ionization).


    
    Calculate the dispersion measure along a cosmological line of sight  
    Package: astro.cosmo  
    Description: Calculate the dispersion measure along a cosmological line  
    of sight as a function of redshift. Valid below redshift 3  
    (He re-ionization).  
    Input  : - Vector of redshifts.  
    - An optional structure of cosmological parameters (see  
    astro.cosmo.cosmo_pars). Alternatively a string of  
    parameters names. Default is 'planck'.  
    Output : - Vector of dispersion measures [cm^-3 pc].  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Zheng, Z. et al. 2014 ApJ 797, 71  
    Example: DM=astro.cosmo.disp_measure([0.1;1])  
    Reliable: 2  
      
      
### astro.cosmo.dist_mod2dist

Convert distance modulous to luminosity distance and redshift Package: astro.cosmo Description: Convert distance modulous to luminosity distance and redshift.


    
    Convert distance modulous to luminosity distance and redshift  
    Package: astro.cosmo  
    Description: Convert distance modulous to luminosity distance and  
    redshift.  
    Input  : - Distance modulous [mag].  
    Output : - Luminosity distance [pc].  
    - Redshift.  
    See also: astro.cosmo.inv_lum_dist  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Dist,Z]=astro.cosmo.dist_mod2dist(35)  
    Reliable: 2  
      
      
      
### astro.cosmo.e_z

E(z) cosmological function Package: astro.cosmo Description: Calculate E(z) cosmological function, which is proportional to the time derivative of the logarithm of the scale factor.


    
    E(z) cosmological function  
    Package: astro.cosmo  
    Description: Calculate E(z) cosmological function, which is  
    proportional to the time derivative of the logarithm  
    of the scale factor.  
    Input  : - Redshift.  
    - Cosmological parameters vector:  
    [Omega_M Omega_Lambda Omega_Radiation],  
    default for Omega_radiation is 0.  
    Output : - E(z).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Ez=astro.cosmo.e_z([1;2],[0.3 0.7]);  
    Reliable: 2  
      
      
### astro.cosmo.growth_linear_perturbation

alculate the growth function of linear perurbations Package: astro.cosmo Description: Calculate the growth function of linear perurbations in various cosmological models.


    
    alculate the growth function of linear perurbations  
    Package: astro.cosmo  
    Description: Calculate the growth function of linear perurbations in  
    various cosmological models.  
    Input  : - Vector of redshifts.  
    - Omega parameters, [\Omega_{m}, \Omega_{\Lambda}].  
    Output : - Growth speed vector for each Z.  
    normalized to 1 at z=0.  
    - Vector of input redshifts.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Li & Ostriker, 2000 astro-ph/0010432  
    Peebles 1980, The large scale structure of the universe.  
    Example: [D]=astro.cosmo.growth_linear_perturbation(1,[0.3 0.7]);  
      
      
### astro.cosmo.hubble_z

The Hubble parameter as a function of redshift Package: astro.cosmo Description: Compute the Hubble parameter as a function of redshift. (Assuming matter dominated universe - Z<1000).


    
    The Hubble parameter as a function of redshift  
    Package: astro.cosmo  
    Description: Compute the Hubble parameter as a function of redshift.  
    (Assuming matter dominated universe - Z<1000).  
    Input  : - Vector of redshifts. [z].  
    - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap9' (see cosmo_pars.m).  
    Output : - Hubble parameter per each redshift [km/sec/Mpc].  
    Reference : Lahav et al. 1991, MNRAS, 251, 128  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                    Jul 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: H=astro.cosmo.hubble_z(1,[70,0.3,0.7]);  
    Reliable: 1  
      
### astro.cosmo.inv_comoving_volume

Convert cosmological volume to redshift Package: AStroUtil.cosmo Description: Use the cosmological volume to calculate the corresponding redshift.


    
    Convert cosmological volume to redshift  
    Package: AStroUtil.cosmo  
    Description: Use the cosmological volume to calculate the corresponding  
    redshift.  
    Input  : - Vector of volumes [pc^3].  
    - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap9' (see cosmo_pars.m).  
    Output : - Redshift.  
    Reference : http://nedwww.ipac.caltech.edu/level5/Hogg/frames.html  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Dec 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [~,V]=astro.cosmo.comoving_volume(2); [Z]=astro.cosmo.inv_comoving_volume(V)  
    Reliable: 2  
    -  
### astro.cosmo.inv_disp_measure

Convert cosmological line of sight dispersion measure to redshift Package: astro.cosmo Description: Convert cosmological line of sight dispersion measure to redshift.


    
    Convert cosmological line of sight dispersion measure to redshift  
    Package: astro.cosmo  
    Description: Convert cosmological line of sight dispersion measure to  
    redshift.  
    Input  : - Vector of DM [cm^-3 pc].  
    - An optional structure of cosmological parameters (see  
    AstroUtil.cosmo.cosmo_pars). Alternatively a string of  
    parameters names. Default is 'planck'.  
    Output : - Vector of redshifts.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Z=astro.cosmo.inv_disp_measure(1000);  
    Reliable: 2  
      
      
### astro.cosmo.inv_e_z

1/E(z) cosmological function Package: astro.cosmo Description: Calculate 1/E(z) cosmological function, in which E(z) is proportional to the time derivative of the logarithm of the scale factor.


    
    1/E(z) cosmological function  
    Package: astro.cosmo  
    Description: Calculate 1/E(z) cosmological function, in which E(z)  
    is proportional to the time derivative of the  
    logarithm of the scale factor.  
    Input  : - Redshift.  
    - Cosmological parameters vector:  
    [Omega_M Omega_Lambda Omega_Radiation],  
    default for Omega_radiation is 0.  
    Output : - 1/E(z).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Ez=e_z([1;2],[0.3 0.7]);  
    Reliable: 2  
      
      
### astro.cosmo.inv_lum_dist

Distance modulus to redshift Package: astro.cosmo Description: Given the distance modulus, use the luminosity distance to calculate the corresponding redshift.


    
    Distance modulus to redshift  
    Package: astro.cosmo  
    Description: Given the distance modulus, use the luminosity distance  
    to calculate the corresponding redshift.  
    Input  : - Vector of distance modulus or luminosity distance.  
    - Type of first input argument:  
    'DM'  - distance modulus  
    'LD'  - luminosity distance (default).  
    - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    Output : - Redshift.  
    Reference : Perlmutter et al. 1997 ApJ, 483, 565  
    Oke & Sandage 1968 ApJ, 154, 21  
    Peterson, B.M., 1997, AGN, p.165  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                  March 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Z=astro.cosmo.inv_lum_dist(35,'dm');     distance modulus to redshift  
    Z=astro.cosmo.inv_lum_dist(150e6,'ld');  lum. dist. to redshift  
    Reliable: 2  
      
### astro.cosmo.lookback_time

Compute the cosmological lookback time Package: astro.cosmo Description: Compute the cosmological lookback time, between two events in redshift z1 and z2, and given the cosmology. (Assuming matter dominated universe - Z<1000).


    
    Compute the cosmological lookback time  
    Package: astro.cosmo  
    Description: Compute the cosmological lookback time, between two events  
    in redshift z1 and z2, and given the cosmology.  
    (Assuming matter dominated universe - Z<1000).  
    Input  : - Matrix of redshifts. [[z1], [z2]], if only one column  
    is given then takes z1=0.  
    - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    Ignores \Omega_{\radiation}.  
    Output : - Lookback time [seconds].  
    Reference : Lahav et al. 1991, MNRAS, 251, 128  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                    Jul 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: T=astro.cosmo.lookback_time([0 1; 1 2; 2 3],[70 0.1 0.9])./(3.1e7.*1e9)  
    Reliable: 1  
      
### astro.cosmo.lum_dist

Luminosity distance Package: astro.cosmo Description: Compute luminosity distance from redshift and cosmological parameters. Given the object spectra, calculate also the K-correction.


    
    Luminosity distance  
    Package: astro.cosmo  
    Description: Compute luminosity distance from redshift and cosmological  
    parameters. Given the object spectra, calculate also the  
    K-correction.  
    Input  : - Vector of redshifts.  
    - Cosmological parameters : [H0, \Omega_{m}, \Omega_{\Lambda}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    Output : - Luminosity distance [parsec].  
    - Distance modulus [mag].  
    Reference : Perlmutter et al. 1997 ApJ, 483, 565  
    Oke & Sandage 1968 ApJ, 154, 21  
    Peterson, B.M., 1997, AGN, p.165  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                    Jul 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [DL,DM]=astro.cosmo.lum_dist([0.1;0.2])  
    [DL,DM]=astro.cosmo.lum_dist([0.1;0.2],[71 0.3 0.7])  
    Reliable: 2  
      
### astro.cosmo.matter_density

The mean matter density in the Universe. Package: astro.cosmo Description: Calculate the mean matter density in the Universe.


    
    The mean matter density in the Universe.  
    Package: astro.cosmo  
    Description: Calculate the mean matter density in the Universe.  
    Input  : - OmegaM, or cosmological parameteters. See cosmo_pars.m  
    for details. Default is 'wmap9'.  
    - If the first parameter is OmegaM than this should be H0.  
    Output : - Density [gr/cm^3]  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Rho=astro.cosmo.matter_density(0.3,70); Rho=astro.cosmo.matter_density;  
    Reliable: 2  
      
      
### astro.cosmo.omega_m_lambda_lines

Selected lines in Omega_m-Omega_lambda diagram Package: astro.cosmo Description: Given a universe with \Omega_{m} and \Omega_{\Lambda} contributions, and given \Omega_{m} vector, find for each value of \Omega_{m}: (i) the value of


    
    Selected lines in Omega_m-Omega_lambda diagram  
    Package: astro.cosmo  
    Description: Given a universe with \Omega_{m} and \Omega_{\Lambda}  
    contributions, and given \Omega_{m} vector, find for  
    each value of \Omega_{m}: (i) the value of  
    \Omega_{\Lambda} for which the universe will expand  
    forever; (ii) The \Omega_{\Lambda} criterion for which  
    there have been no singularity in the past (rather than  
    Big Bang its early history consisted of a period of  
    gradually slowing contraction to a minimum radius before  
    begining its current expansion).  
    Input  : - Vector of \Omega_{m}.  
    Output : - Vector of lower(>=) \Omega_{\Lambda} (for each \Omega_{m}),  
    for which the universe will expand forever.  
    (in the \Omega_{\Lambad} vs. \Omega_{m} plane, the  
    big-crunch, is to the left of this critical line).  
    - Vector of lower(>=) \Omega_{\Lambda} (for each \Omega_{m}),  
    for which there was no singularity in the past.  
    (in the \Omega_{\Lambad} vs. \Omega_{m} plane, the  
    no-big-bang, is to the right of this critical line).  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                    Jul 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    References : Carroll, S.M. 2000, astro-ph/0004075  
    Example: OmegaM=[0:0.01:3]';   define a vector of Omega matter  
    [OmL_EF,OmL_NS]=astro.cosmo.omega_m_lambda_lines(OmegaM);  
    plot(OmegaM,OmL_EF); hold on; plot(OmegaM,OmL_NS,'r');  
    xlabel('\Omega_{m}'); ylabel('\Omega_{\Lambda}');  
    axis([0 3 -1 4.5]);  
    text(1.5,-0.2,'Expand forever'); text(0.5,3.5,'No singularity');  
    Reliable: 2  
      
      
### astro.cosmo.omega_z

Omega_m as a function of redshift Package: astro.cosmo Description: Calculate \Omega_{m} as a function of redshift z.


    
    Omega_m as a function of redshift  
    Package: astro.cosmo  
    Description: Calculate \Omega_{m} as a function of redshift z.  
    Input  : - Redshift vector.  
    - Parameters at zero redshift.  
    [\Omega_{m}, \Omega_{\Lambda}]  
    Output : - Vector of \Omega_{m}(Z) corresponding to redshift vector.  
    Reference : Porciani & Madau 2000 ApJ 532, 679.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [OmegaMz]=astro.cosmo.omega_z(1,[0.3 0.7]);  
    Reliable: 1  
      
      
### astro.cosmo.sfr

Estimate the cosmic star formation rate as a function of redshift Description: Fit the measured star formation rate as a function of redshift and return its interpolated value in some requested redshifts.


    
    Estimate the cosmic star formation rate as a function of redshift  
    Description: Fit the measured star formation rate as a function  
    of redshift and return its interpolated value in some  
    requested redshifts.  
    Input  : - An array of redshifts at which to return the SFR.  
    Output : - The star formation rate [SolarMass yr^-1 Mpc^-3]  
    - Best for polynomial coef.  
    - Table of measurments.  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [S,P,Table]=astro.cosmo.sfr((0:0.01:8)')  
    Reliable: 2  
      
### astro.cosmo.tran_comoving_dist

Transverse comoving distance Package: astro.cosmo Description: Calculate the transverse comoving distance. Given the transverse comoving distance (D_M), the comoving distance between two events at the same redshift, but separated on


    
    Transverse comoving distance  
    Package: astro.cosmo  
    Description: Calculate the transverse comoving distance. Given the  
    transverse comoving distance (D_M), the comoving distance  
    between two events at the same redshift, but separated on  
    the sky by some angle \delta\theta is: D_M\delta\theta.  
    Input  : - Redshift.  
    - Cosmological parameters:  
    [H0, \Omega_{m}, \Omega_{\Lambda}, \Omega_{radiation}],  
    or cosmological parmeters structure, or a string containing  
    parameters source name, default is 'wmap3' (see cosmo_pars.m).  
    Default for \Omega_{radiation} is 0.  
    Output : - Comoving distance [pc].  
    Reference: http://nedwww.ipac.caltech.edu/level5/Hogg/frames.html  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    Example: D_M=astro.cosmo.tran_comoving_dist([1;2]);  
    -  
      
