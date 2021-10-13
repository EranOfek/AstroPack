# Package: astro.supernova


### astro.supernova.fit_sn_rise

Fit rise-time of SN light curve Package: astro.supernova Description: Fit various functions appropriate for the rise of SN light curve. The fitted functions are: L_max*(1-((t-t_max)/t_rise)^2)


    
    Fit rise-time of SN light curve  
    Package: astro.supernova  
    Description: Fit various functions appropriate for the rise of SN  
    light curve. The fitted functions are:  
    L_max*(1-((t-t_max)/t_rise)^2)  
    L_max.*(1-exp(-(t-t_start)./t_rise))  
    L_max.*erfc(sqrt(t_diff./(2.*(t-t_start))))  
    Input  : - Supernova light curve [time(day), mag, err]  
    * Arbitrary number of pair of arguments, ...,key,val,...  
    The following keywords are available:  
    'Type'  - LC type {'mag','lum'}. Default is 'mag'.  
    If 'lum', then the first inpt arguments is  
    [time(day), Lum(erg/s), err(erg/s)]  
    'DM'    - Distance modulus [mag] to use in conversion from  
    magnitude to luminosity. Default is 0.  
    'A'     - Extinction in band [mag] to use in conversion from  
    magnitude to luminosity. Default is 0.  
    'SunAbsMag' - Sun abs. mag. in band. Default is 4.66 (PTF R).  
    'Nsim'  - Number of bootstrap simulations for error  
    estimation. Default is 100.  
    'Options' - optimset structure to pass to the non linear  
    fitting.  
    Output : - A structure with the following fields:  
    .t2   - a t^2 law fit  
    .exp  - an exp fit  
    .erfc - an erfc fit  
    Each field contains the following seb fields:  
    .Par     - Best fit parameters  
    in .t2 these are [L_max, t_max, t_rise]  
    in .exp these are [L_max, t_start, t_rise]  
    in .erfc these are [L_max, t_diff, t_start]  
    .ParErrB - Bootstrap errors in best fit parameters  
    .Chi2    - \chi^2 of best fit  
    .Dof     - number of degrees of freedom  
    Tested : Matlab R2013a  
    By : Eran O. Ofek                    Jul 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=astro.supernova.fit_sn_rise(LC);  
    Reliable: 2  
      
      
### astro.supernova.interacting_sn_prop

Calculate the properties of CSM-ejecta interacting SN Package: astro.supernova Description: Calculate the properties of CSM-ejecta interacting SN based on the Ofek et al. (2014) formulae.


    
    Calculate the properties of CSM-ejecta interacting SN  
    Package: astro.supernova  
    Description: Calculate the properties of CSM-ejecta interacting SN based  
    on the Ofek et al. (2014) formulae.  
    Input  : - L0  
    - Alpha - Bolometric light curve power-law index slope.  
    - m - stellar envelope power law index  
    * Arbitary number of pairs of arguments: ...,key,val,...  
    The following keywords are available:  
    'tbo' - Wind shock breakout time scale [s].  
    'Eps' - Efficiency.  
    'kappa'  
    'mu_p'  
    'Te_shock'  
    'Te_csm'  
    'Z'  
    'Nu'  
    'tI'  
    Output : - Structure pf output parameters.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data=astro.supernova.interacting_sn_prop(1e44,-1./3,8)  
      
    DefV.m        = 10;  
    DefV.alpha    = -0.3;  
### astro.supernova.kilonova_multizone

Calculate the Waxman et al. analytic multizone kilonova model Package: astro.supernova Description: Calculate the Waxman et al. analytic multizone kilonova model.


    
    Calculate the Waxman et al. analytic multizone kilonova model  
    Package: astro.supernova  
    Description: Calculate the Waxman et al. analytic multizone kilonova  
    model.  
    Input  : - time [seconds]. If empty or not provided, then default is  
    logspace(2,6,10000).  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Case' - Select predefined private cases:  
    '1a' - XM<0 & s>0  
    '1b' - XM<0 & s<0  
    '2a' - XM>0 & s>0  
    '2b' - XM>0 & s<0  
    'eConf'- Electron confinment (true). If false than assume free  
    electrons escape. Default is true.  
    'M'    - Ejecta mass [solar mass].  
    'vM'   - Ejecta v_M velocity [speed of light].  
    'Alpha'- Ejecta velocity distribution: v(m)=vM m^-Alpha.  
    'Beta' - Heating power-law decay.  
    'Gamma'- Evolution in kappa.  
    't0'   - t0.  
    'kM'   - kappa(t=tM) [cm^2/gr]  
    'kG'   - kappa_\gamma [cm^2/gr]  
    'epsdot0' - Energy deposition rate at t0 [erg/s/gr].  
    Output : - Luminosity [erg/s].  
    - Structure array with additional parameters.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Oct 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: t=logspace(1,7,30)'; [L,Res]=astro.supernova.kilonova_multizone([]);  
    Reliable: 2  
      
      
### astro.supernova.mildrel_synchrotron

SHORT DESCRIPTION HERE Package: astro.supernova Description:


    
    SHORT DESCRIPTION HERE  
    Package: astro.supernova  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
### astro.supernova.nickel56_decay

Energy production of Nickel56->Cobalt->Iron Package: astro.supernova Description: Calculate the energy production of Nicel56->Cobalt->Iron radioactive decay as a function of time.


    
    Energy production of Nickel56->Cobalt->Iron  
    Package: astro.supernova  
    Description: Calculate the energy production of Nicel56->Cobalt->Iron  
    radioactive decay as a function of time.  
    Input  : - Time [day].  
    - t0. Default is 30 days.  
    - n. Default is 3.  
    Output : A structure with the following fields:  
    'E' - Total energy [erg/s/gr] generated in gamma and positorons  
    from both Ni and Co decay.  
    'Qgamma' - Total energy [erg/s/gr] generated in gammas.  
    'Qpos'   - Total energy [erg/s/gr] generated in positrons.  
    'Q_Ni'   - Total energy from Ni decay.  
    'Q_Co'   - Total energy from Co decay.  
    'f_dep'  - Fraction of energy deposited from the gammas.  
    'Edep'   - Total radiated energy (Qgamma.*f_dep + Qpos).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Aug 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference : Kulkarni (2006), Eq. 14, 44.  
    Sharon & Kushnir (2020)  
    Example: [Res]=astro.supernova.nickel56_decay([1;2]);  
    Reliable: 2  
      
      
### astro.supernova.sn_cooling_msw

sn_cooling_sw function                                           General Description: Calculate the shock cooling light curve (following the shock breakout) of a supernova, based on the Sapir & Waxman (2017) model (ApJ 838:130).


    
      
    sn_cooling_sw function                                           General  
    Description: Calculate the shock cooling light curve (following the  
    shock breakout) of a supernova, based on the  
    Sapir & Waxman (2017) model (ApJ 838:130).  
    Input  : - Vector of times [days] in which to calculate the shock  
    cooling luminosity in the obeserver frame.  
    Default is logspace(log10(1./24),log10(10),30).';  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Type' - {'rsg','bsg','wr'}. Default is 'rsg'.  
    'Vs' -   The shock velocity paramaeter in units of cm/s.  
    Default is 10^8.5 cm/s (see eq. 2,3).  
    'E51'  - Energy in units of 10^51 erg. Default is 1.  
    If empty will use Vs field, otherwise will override  
    the Vs field. Default is empty.  
    'Rs'   - Progenitor radius in units of the solar radii.  
    Default is 500.  
    'Ms'   - Ejecta mass in solar masses. Default is 10.  
    'f_rho'- f_{\rho} parameter. If empty use 1 for 'rsg',  
    0.05 for 'bsg', and 0.1 for 'wr'.  
    'kappa'- Opacity. Default is 0.34 cm^2 gr^-1.  
    'Tcorr'- Temperature correction factor.  
    Default is 1.1 for RSG and 1.0 for BSG.  
    'redshift' - SN redshift. Default is 0.023061.  
    'Dist' - SN distance [Mpc].  
    If empty will use the redshift field, otherwise  
    will override the redshift field. Default is empty.  
    'Ebv'  - Extinction E(B-V). Default is 0.  
    'Rv'   - Selective extinction (R_V). Default is 3.08.  
    'FiltFam' - Filter family (see get_filter.m for options).  
    Default is 'LIM'.  
    If FiltFam is a struct in the format returned by  
    get_filter.m, the input filter is used instead of  
    calling get_filter.m  
    'FiltName'- Filter name (see get_filter.m for options).  
    Default is 'JPL'.  
    'FiltSys' - Filter system {'AB','Vega'}. Default is 'AB'.  
    'Wave'    - Wavelength [A] of AB monochromatic magnitude to  
    calculate. If provided the returned magnitude will  
    corresponds to this wavelength (instead of the filter  
    family and name). Default is empty.  
    'Model'- Determine weither to use MSW20, SK17 or RW11 model.  
    Default is 'MSW'. Other options are 'SW' and 'RW'.  
      
    Output : - Intrinsic luminosity [erg/s] as a function of time.  
    - Intrinsic color temperature [K] as a function of time.  
    - photospheric effective radius [cm] as a function of time  
    (using T_col and not T_ph).  
    - t_d [days] in the rest frame.  
    - Observed apparnt Magnitude as a function of time.  
    Corrected for redshift.  
    - t_min [days]  - the earliest time the model is valid given in  
    observer frame.  
    - t_max [days]  - the latest time the dynamical model is valid  
    given in observer frame.  
    - model_times   - a structure contanis the model internal times  
    (given in the SN frame):  
    - t_min [days] - the time the progenitor envelope has  
    expanded enough and RW and SW models becomes  
    valid.  
    - t_opac [days] - the time T_ph = 0.7eV and the constant  
    opacity approximation breaks. The time is  
    given in rest frame.  
    - t_tr [days]   - the transperancy time at which the  
    enveleope is almost tranparent and is the  
    timescale for the SW17 luminosity suppression.  
    - t_delta [days]- the time where the RW11 small delta  
    approximation breaks (delta ~ 0.1) and the  
    emmision becomes dependent in the envelope  
    density profile.  
    The time is given in rest frame.  
    - t_min_sw [days] - The time the breakout is well described  
    by its assymptotic behaviour and MSW becomws  
    valid.  
      
    Comment: The funtion support calculation for multiple cases in one call.  
    Input parameters on the same dimension are considered as a set  
    for a single case (e.g. different pairs of progenitor radius and  
    and redshifts). Input parameters on different dimensions are  
    not considered as a part of a set and all the cases results from  
    the combinations of the different dimensions are calculated.  
    see the second example.  
      
    Tested : Matlab R2017a  
    By : Noam Ganot                    Jan 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  [L,Tc,R] = astro.supernova.sn_cooling_msw((3000:3000:90000)/86400);  
    [L,Tc,R,t_d,Mag,t_min,t_max,t_opac,t_tr,t_delta]=...  
    astyro.supernova.sn_cooling_sw((3000:3000:90000)/86400,'Rs',...  
    [500;1000],'Vs',shiftdim([0.5;1;2]*10^8.5,-2),...),...  
    'redshift',[0.01;0.07],'FiltFam','Galex','FiltName','NUV')  
    Reliable: 1  
      
      
### astro.supernova.sn_cooling_rw

Shock cooling light curve (Rabinak & Waxman 2011) Package: astro.supernova Description: Calculate the shock cooling light curve (following the shock breakout) of a supernova, based on the Rabinak & Waxman (2011) model.


    
    Shock cooling light curve (Rabinak & Waxman 2011)  
    Package: astro.supernova  
    Description: Calculate the shock cooling light curve (following the  
    shock breakout) of a supernova, based on the  
    Rabinak & Waxman (2011) model.  
    Input  : - Vector of times [days] in which to calculate the shock  
    cooling luminosity.  
    Default is logspace(log10(1./24),log10(10),30).';  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Type' - {'rsg','bsg','wr'}. Default is 'rsg'.  
    'E51'  - Energy in units of 10^51 erg. Defaukt is 1.  
    'Ms'   - Ejecta mass in solar masses. Default is 10.  
    'Rs'   - Progenitor radius in units of the solar radii.  
    Default is 500.  
    'f_rho'- f_{\rho} parameter. If empty use 0.1 for 'rsg',  
    0.05 for 'bsg', and 0.1 for 'wr'.  
    'kappa'- Opacity. Default is 0.34 cm^2 gr^-1.  
    'Tcorr'- Temperature correction factor. Default is 1.2.  
    'Z'    - He abundabnce for WR models (Z=0 He; Z=1 no He).  
    Default is 0.  
    'redshift' - SN redshift. Default is 0.023061.  
    'Dist' - SN distance [Mpc]. Default is 100.  
    If empty will use the redshift field, otherwise  
    will override the redshift field. Default is empty.  
    'Ebv'  - Extinction E(B-V). Default is 0.  
    'Rv'   - Selective extinction (R_V). Default is 3.08.  
    'FiltFam' - Filter family (see get_filter.m for options).  
    Default is 'LIM'.  
    'FiltName'- Filter name (see get_filter.m for options).  
    Default is 'JPL'.  
    'FiltSys' - Filter system {'AB','Vega'}. Default is 'AB'.  
    'Wave'    - Wavelength [A] of AB monochromatic magnitude to  
    calculate. If provided the returned magnitude will  
    corresponds to this wavelength (instead of the filter  
    family and name). Default is empty.  
    Output : - Intrinsic luminosity [erg/s] as a function of time.  
    - Intrinsic effective temperature [K] as a function of time.  
    - photospheric radius [cm] as a function of time.  
    - Time [days].  
    - Observed apparnt Magnitude as a function of time.  
    Corrected for redshift.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Dec 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,Tc,R,Time,Mag]=astro.supernova.sn_cooling_rw_my;  
    Reliable: 2  
      
### astro.supernova.sn_cooling_sw

sn_cooling_sw function                                           General Description: Calculate the shock cooling light curve (following the shock breakout) of a supernova, based on the Sapir & Waxman (2017) model (ApJ 838:130).


    
      
    sn_cooling_sw function                                           General  
    Description: Calculate the shock cooling light curve (following the  
    shock breakout) of a supernova, based on the  
    Sapir & Waxman (2017) model (ApJ 838:130).  
    Input  : - Vector of times [days] in which to calculate the shock  
    cooling luminosity in the obeserver frame.  
    Default is logspace(log10(1./24),log10(10),30).';  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Type' - {'rsg','bsg','wr'}. Default is 'rsg'.  
    'Vs' -   The shock velocity paramaeter in units of cm/s.  
    Default is 10^8.5 cm/s (see eq. 2,3).  
    'E51'  - Energy in units of 10^51 erg. Default is 1.  
    If empty will use Vs field, otherwise will override  
    the Vs field. Default is empty.  
    'Rs'   - Progenitor radius in units of the solar radii.  
    Default is 500.  
    'Ms'   - Ejecta mass in solar masses. Default is 10.  
    'f_rho'- f_{\rho} parameter. If empty use 1 for 'rsg',  
    0.05 for 'bsg', and 0.1 for 'wr'.  
    'kappa'- Opacity. Default is 0.34 cm^2 gr^-1.  
    'Tcorr'- Temperature correction factor.  
    Default is 1.1 for RSG and 1.0 for BSG.  
    'redshift' - SN redshift. Default is 0.023061.  
    'Dist' - SN distance [Mpc].  
    If empty will use the redshift field, otherwise  
    will override the redshift field. Default is empty.  
    'Ebv'  - Extinction E(B-V). Default is 0.  
    'Rv'   - Selective extinction (R_V). Default is 3.08.  
    'FiltFam' - Filter family (see get_filter.m for options).  
    Default is 'LIM'.  
    If FiltFam is a struct in the format returned by  
    get_filter.m, the input filter is used instead of  
    calling get_filter.m  
    'FiltName'- Filter name (see get_filter.m for options).  
    Default is 'JPL'.  
    'FiltSys' - Filter system {'AB','Vega'}. Default is 'AB'.  
    'Wave'    - Wavelength [A] of AB monochromatic magnitude to  
    calculate. If provided the returned magnitude will  
    corresponds to this wavelength (instead of the filter  
    family and name). Default is empty.  
    'Model'- Determine weither to use SK17 extension to the  
    original RW11 model by supressing the bolometric  
    luminocity or not. Extending the model beyond the  
    small delta approximation introduces dependencies on  
    the progenitor's structure. Default is 'RW'.  
    Use 'SW' to extend.  
    Output : - Intrinsic luminosity [erg/s] as a function of time.  
    - Intrinsic color temperature [K] as a function of time.  
    - photospheric effective radius [cm] as a function of time  
    (using T_col and not T_ph).  
    - t_d [days] in the rest frame.  
    - Observed apparnt Magnitude as a function of time.  
    Corrected for redshift.  
    - t_min [days]  - the earliest time the model is valid given in  
    rest frame.  
    - t_max [days]  - the latest time the dynamical model is valid  
    given in rest frame.  
    - t_opac [days] - the time T_ph = 0.7eV and the constant opacity  
    approximation breaks. The time is given in  
    rest frame.  
    - t_tr [days]   - the transperancy time which is the timescale  
    for the luminosity suppression.  
    - t_delta [days]- the time where the small delta approximation  
    breaks (delta ~ 0.1) and the emmision becomes  
    dependent in the envelope density profile.  
    The time is given in rest frame.  
    - R_BO [cm]     - An upper limit for the photosphere radius  
    when the model is not valid before t_min.  
      
    Comment: The funtion support calculation for multiple cases in one call.  
    Input parameters on the same dimension are considered as a set  
    for a single case (e.g. different pairs of progenitor radius and  
    and redshifts. Input parameters on different dimensions are  
    not considered as a part of a set and all the cases results from  
    the combinations of the different dimensions are calculated.  
    see the second example.  
      
    Tested : Matlab R2017a  
    By : Noam Ganot                    Jan 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  [L,Tc,R] = astro.supernova.sn_cooling_sw((3000:3000:90000)/86400);  
    [L,Tc,R,t_d,Mag,t_min,t_max,t_opac,t_tr,t_delta]=...  
    astro.supernova.sn_cooling_sw((3000:3000:90000)/86400,'Rs',...  
    [500;1000],'Vs',shiftdim([0.5;1;2]*10^8.5,-2),...),...  
    'redshift',[0.01;0.07],'FiltFam','Galex','FiltName','NUV')  
    Reliable: 1  
      
      
### astro.supernova.sync_mildly_relativistic

Synchrotron emission from mildly relativistic ejecta interacting with ISM Package: astro.supernova Description:


    
    Synchrotron emission from mildly relativistic ejecta interacting with ISM  
    Package: astro.supernova  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: nuLnu=astro.supernova.sync_mildly_relativistic  
    Reliable: X  
      
      
### astro.supernova.synchrotron

SHORT DESCRIPTION HERE Package: astro.supernova Description:


    
    SHORT DESCRIPTION HERE  
    Package: astro.supernova  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
