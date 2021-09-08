# Package: celestial.pm


### celestial.pm.fitMultiProperMotion

Simultanoulsy (fast) fit proper motion and stationary model to observations The fit is done in RA/Dec space, and no cos(delta) is applied Perform hypothesis testing between the proper motion and stationary models.


    
    Simultanoulsy (fast) fit proper motion and stationary model to observations  
    The fit is done in RA/Dec space, and no cos(delta) is applied  
    Perform hypothesis testing between the proper motion and stationary  
    models.  
    Input  : - A vector of times.  
    - An array of RA [Epoch X Source]  
    - An array of Dec [Epoch X Source]  
    - An array of errors in RA, in the same units as RA.  
    Default is 1./(3600.*100).  
    - An array of errors in Dec, in the same units as Dec.  
    Default is 1./(3600.*100).  
    * ...,key,val,...  
    'MinNobs' - minimum number of data points required for fit  
    (used only when the number of observations is not  
    the same for all sources).  
    Default is 5.  
    'Prob' - Vector of probabilities for which to calculate  
    the probably difference between H1 and H0.  
    Default is [1e-3 1e-5].  
    'Units' - Units of RA/Dec. This is used only for the the  
    checking that the RA data is not crossing zero.  
    Default is 'deg'.  
    'RenormErr' - A logical flag inducating if to normalize  
    the chi2 for the H1 hypothesis to 1.  
    Default is true.  
    Output : - A structure with the following fields:  
    .MeanT - Mean epoch relative to which the fit is done.  
    .RA.ParH1 - Parameters for H1 [pos; vel] for each source.  
    .RA.ParH0 - Parameters for H0 [pos] for each source.  
    .RA.Chi2_H1 - \chi^2 for H1  
    .RA.Chi2_H0 - \chi^2 for H0  
    .RA.Nobs - Number of observations for each source.  
    .RA.DeltaChi2 - \Delta\chi^2 between H0 and H1.  
    .RA.FlagH1 - A logical indicating if the source has a  
    prefreed H1 model over H0. each line, for each  
    requested proabability.  
    .RA.StdResid_H1 - std of H1 residuals.  
    .RA.StdResid_H0 - std of H0 residuals.  
    .Dec. - the same as .RA, but for the Dec axis.  
    Author : Eran Ofek (May 2021)  
    Example: Time=(1:1:20)'; RA = randn(20,1e5)./(3600.*100); Dec=randn(20,1e5)./(3600.*200);  
    RA(:,1) = (0.1:0.1:2)'./(3600);  
    Result = celestial.pm.fitMultiProperMotion(Time, RA, Dec);  
      
### celestial.pm.searchCatForKnownPM

Given a catalog of sources positions and times, and a specific position+proper motion, look for sources in the catalog that are found in the proper motion trajectory.


    
    Given a catalog of sources positions and times, and a specific  
    position+proper motion, look for sources in the catalog that  
    are found in the proper motion trajectory.  
    Input  : - Catalog of observations (at least, RA, Dec, Epoch).  
    - A six or eight columns array of  
    [Epoch_RA_JD, RA_rad, PM_RA, Epoch_Dec_JD, Dec_rad, PM_Dec, Parallax_mas, RV_kms]  
    If eight columns, then apply parallax.  
    PM is in mas/yr.  
    * ...,key,val,...  
    'ColRA' - Column index of RA in catalog. Default is 1.  
    'ColDec' - Column index of Dec in catalog. Default is 2.  
    'ColEpoch' - Column index of Epoch in catalog. Default is 3.  
    'CatCooUnits' - catalog coo units. Default is 'rad'.  
    'CatEpochUnits' - Catalog epoch units ['J'|'B'|'JD']. Default is 'JD'.  
    'SearchRadius' - Search radius for sources in trajectory.  
    Default is 1.  
    'SearchRadiusUnits' - Search radius units.  
    Default is 'arcsec'.  
    Output : - A structure array of results. One element per tested PM.  
    The following fields are available:  
    'Nfound' - Number of sources found within search radius.  
    'Std' - Std of angular distance of sources in search  
    radius.  
    'Flag' - A vector of logicals indicating if a sources is  
    found within search radius.  
    'RStdAll' - Robust std for all sources.  
    Author : Eran Ofek (Jul 2021)  
    Example: JD_OUT=celestial.time.julday([1 1 2020])+[-365.*5:100:365.*5]'  
    [PredRA, PredDec] =celestial.coo.proper_motion(JD_OUT, JD_OUT(1), JD_OUT(1), 1,1,100,100)  
    Cat = [PredRA, PredDec+rand(37,1).*3./RAD./3600, JD_OUT];  
    Cat = [Cat; [1.1, 1.2, JD_OUT(1)]]  
    Result = celestial.pm.searchCatForKnownPM(Cat, [JD_OUT(1), 1, 100, JD_OUT(1), 1, 100])  
      
