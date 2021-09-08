# Package: celestial.scheduling


### celestial.scheduling.LAST_scheduler




    
      
      
      
      
    get TargetList including history  
      
      
    run night simulation in order to estimate the footprints of fast cadence  
      
    prep TargetList for hig cadence  
      
    prep TargetList for low cadence  
      
    prep Dynamic TargetList (ToO) - set to empty  
      
    start with most limited telescopes (1,2)  
    for each telescope  
    For each telescope mount call target_selection  
      
    telescopes are pre-assigned for low/high cadence  (????)  
    1 3 5 7 9  11  
    2 4 6 8 10 12  
    by default 5,6,9,10 are assigned to fast cadence  
      
    For each telescope mount call validate_coo.m  
    end  
    output is list of possible targets, target category (fast/slow), and  
    which telescope can observe the target  
      
      
    Select target for telescope  
    and mark the selected targets  
      
### celestial.scheduling.LAST_simulator

Simulate LAST targets scheduling Package: +celestial.schedling Description:


    
    Simulate LAST targets scheduling  
    Package: +celestial.schedling  
    Description:  
    Input  : - JD of first night to simulate.  
    * Arbitrary number of pairs of ...,key,val,... arguments.  
    The following keywords are available:  
    'DecRange' - Declination range of targets.  
    Default is [-30 90].  
    'Ntel' - Number of telescopes to schedule. Default is 4.  
    'Nnight' - Number of nights to simulate. Default is 10.  
    'Lon' - Observatory East longitude [rad]. Default is 35./RAD.  
    'Lat' - Observatory North Latitude [rad]. Default is 30./RAD.  
    'TimeStep' - Time step [day] for time visibility table.  
    Default is 5./1440.  
    'SunAltLimit' - Sun altitude limit [rad]. Default is -12./RAD.  
    'MaxAM' - Maximum AM limit. Default is 2.  
    'AzAltLimit' - Table of allowed altitude vs. Az [deg].  
    Default is [0 15;90 15; 180 15; 270 15; 360 15]  
    Must start at Az 0 and end at Az 360.  
    'MinMoonDistIllum' - Minimum angular distance from the moon  
    [deg] as a afunction of Moon illumination.  
    Default is  
    [0 0; 0.1 1; 0.2 1; 0.3 1; 0.4 2; 0.5 3; 0.6 5;0.7 10;0.8 15; 0.9 30; 1.0 30]  
    'MinVisibilityTime' - Minimum required target time visibility  
    during the night [day]. Default is 1./24.  
    'FactorVisibilityAM' - Factor by which to multiply the  
    visibility time after adjusting it to new AM.  
    Default is true.  
    'MainCadence' - The main cadence of the survey [day]  
    Default is 1.4.  
    'NightCadence' - The nightly cadence [day].  
    Default is 40./1440.  
    'Nfast' - Number of epochs per night in the nightly cadence.  
    Default is 2.  
    'MainWFun' - Weight function for main cadence.  
    Default is @celestial.scheduling.fermiexp  
    'MainWFunPar' - Parameters for weight function for main  
    cadence.  
    Default is {1.4, 1, 0.03, 1, 0.5}.  
    'NightWFun' - Weight function for nightly cadence.  
    Default is @celestial.scheduling.fermiexp  
    'NightWFunPar' - Parameters for weight function for nightly  
    cadence.  
    Default is {40./1440, 1, 0.003, 1.5, 0.5}.  
    'InterpMethod' - Tables interpolation. Default is 'linear'  
    'Plot' - Plot observed target as a function of time.  
    Default is true.  
    Output : - TimeHistory - A structure array with element per epoch,  
    with the list of targets observed on each epoch.  
    - History - A structure arry with element per target.  
    containing the target observing history.  
    - A structure with the followig fields:  
    .AllDiff - Vector of all time differences between targets,  
    seperated with NaN between targets.  
    .AllAM - Vector of all observed AM.  
    By: Eran Ofek                      Oct 2020  
    Example: [TimeHistory,History,ResS]=celestial.scheduling.LAST_simulator(celestial.time.julday([1 3 2021]),'Plot',false)  
    histogram(ResS.AllAM,[1:0.05:2]')   histogram of AM distribution  
    plot number of vists per field  
    axesm ('aitoff', 'Frame', 'on', 'Grid', 'on');  
    scatterm(ResS.TargetList(:,2).*RAD,ResS.TargetList(:,1).*RAD,45,TL,'filled');  
    H=colorbar;  
    H.Label.String='Epochs/yr';  
    H.Label.Interpreter='latex';  
    G=celestial.coo.coco([(0:1:360)',zeros(361,1)],'g','j2000.0');  
    hold on;  
    plotm(G(:,2).*RAD,G(:,1).*RAD,'k.')  
    axis off  
      
      
      
### celestial.scheduling.assign_targets2telescopes




    
      
      
      
      
      
### celestial.scheduling.coo_visibility

Calculate the visibility of celestial coordinates Package: +celestial.scheduling Description: For a given night, and a list of targets, calculate the matrix of target visibility in 5-min (default) steps.


    
    Calculate the visibility of celestial coordinates  
    Package: +celestial.scheduling  
    Description: For a given night, and a list of targets, calculate the  
    matrix of target visibility in 5-min (default) steps.  
    The visibility criteria, includes SunAltLimit, MaxAM,  
    Moon distance as a function of illumination, and minimum  
    visibility time. By default it will also calculate the AM  
    limit above the target is found for  
    MinVisibilityTime*FactorVisibilityAM.  
    Input  : - JD. will choose the nearest night (unless AllNightVisibility  
    is false).  
    - Vector of targets J2000.0 RA [rad].  
    - Vector of targets J2000.0 Dec [rad].  
    * Arbitrary number of pairs of ...,key,val,... arguments.  
    The following keywords are available:  
    'AllNightVisibility' - If true, then calculate the all night  
    visibility of the targets 9e.g., every TimeStep).  
    If false, then calculate the visibility only at the  
    current requested time.  
    Default is true.  
    'Lon' - Observatory East longitude [rad]. Default is 35./RAD.  
    'Lat' - Observatory North Latitude [rad]. Default is 30./RAD.  
    'TimeStep' - Time step [day] for time visibility table.  
    Default is 5./1440.  
    'SunAltLimit' - Sun altitude limit [rad]. Default is -12./RAD.  
    'MaxAM' - Maximum AM limit. Default is 2.  
    'AzAltLimit' - Table of allowed altitude vs. Az [deg].  
    Default is [0 15;90 15; 180 15; 270 15; 360 15]  
    Must start at Az 0 and end at Az 360.  
    'MinMoonDistIllum' - Minimum angular distance from the moon  
    [deg] as a afunction of Moon illumination.  
    Default is  
    [0 0; 0.1 1; 0.2 1; 0.3 1; 0.4 2; 0.5 3; 0.6 5;0.7 10;0.8 15; 0.9 30; 1.0 30]  
    'MinVisibilityTime' - Minimum required target time visibility  
    during the night [day]. Default is 1./24.  
    'FactorVisibilityAM' - Factor by which to multiply the  
    visibility time after adjusting it to new AM.  
    'CleanTime' - Remove times in which Sun is above alt limit.  
    Default is true.  
    'InterpMethod' - Tables interpolation. Default is 'linear'  
    Output : - A structure with the following fields:  
    .VecJD - A vector of JD for each line in the visibility  
    tables.  
    .Flag - A matrix of logicals (Time,Target) indicating if the  
    target is visible in each time step.  
    .RA - Vector of J2000.0 RA [rad].  
    .Dec - Vector of J2000.0 Dec [rad].  
    .TimeVisible - Vector indicating the total time during the  
    night in which the target is visible [day].  
    .NtargetVisible - Vector of te number of targets visible on  
    each time.  
    .AM - Matrix of AM for each target and time.  
    .LST - LST [rad]  
    .MoonDist - Matrix of moon distances [rad].  
    .FlagLim - The same as Flag, but after applying the new AM  
    limit per target.  
    .TimeVisibleLim - The same as TimeVisible, but after applying the new AM  
    limit per target.  
    .NtargetVisibleLim - The same as NtargetVisible, but after applying the new AM  
    limit per target.  
    - A vector of AM limits per target as calculated by requireing  
    the MinVisibilityTime for the target  
    By: Eran Ofek                      Oct 2020  
    Example: [TileList,TileArea]=celestial.coo.tile_the_sky(56,42);  
    [Res,LimitAM]=celestial.scheduling.coo_visibility(2451556,TileList(:,1),TileList(:,2))  
    [Res]=celestial.scheduling.coo_visibility(2451556,TileList(:,1),TileList(:,2),'MaxAM',LimitAM)  
      
      
### celestial.scheduling.fermiexp

Fermi rise - Exp decay weight function Package: +celestial.scheduling


    
    Fermi rise - Exp decay weight function  
    Package: +celestial.scheduling  
    Input  : - time  
    * t0, DecayExp, SoftFermi, BaseW, ExtraW  
    Output : - Weights  
    Example: W=celestial.scheduling.fermiexp(t,1,1,0.05,1,0.5);  
      
### celestial.scheduling.target_selection

Select a single target for one telescope Package: +celestial.scheduling Example: Res=celestial.scheduling.target_selection


    
    Select a single target for one telescope  
      
    Package: +celestial.scheduling  
    Example: Res=celestial.scheduling.target_selection  
      
### celestial.scheduling.validate_coo

Validate HA, Dec within Az,Alt and HA,Dec ranges. Package: +celestial.scheduling Description: Given a vectors of HA/Dec check for each coordinate if it complies with Az,Alt limit and withing HA/Dec ranges.


    
    Validate HA, Dec within Az,Alt and HA,Dec ranges.  
    Package: +celestial.scheduling  
    Description: Given a vectors of HA/Dec check for each coordinate if it  
    complies with Az,Alt limit and withing HA/Dec ranges.  
    Input  : - Matrix of HA [rad].  
    - Matrix of Dec [rad].  
    * Arbitrary number of pairs of ...,key,val,... arguments.  
    The following keywords are available:  
    'Lon' - Observatory East longitude [rad]. Default is 35./RAD.  
    'Lat' - Observatory North Latitude [rad]. Default is 30./RAD.  
    'AzAltLimit' - A two column matrix of [Az Alt] limits [deg].  
    First line must be for Az=0 and last line for Az=360.  
    Default is [0 15; 90 15; 180 15; 270 15; 360 15]  
    'RangeHA' - Range of valid HA (-180 to 180) [deg].  
    Default is [-120 120].  
    'RangeDec' - Range of valid Dec [deg].  
    Default is [-30 90].  
    'InterpMethod' - Table interpolation method.  
    Default is 'linear'.  
    Output : - A matrix of logicals indicating if target is in range (true).  
    Example: Flag=celestial.scheduling.validate_coo(HA,Dec)  
      
      
### celestial.scheduling.weight_cadence

Calculate the cadence weight for a list of targets. Package: +celestial.scheduling Description: The cadence weight is a combination of two weights, based on the last time the target was observed during the night, and


    
    Calculate the cadence weight for a list of targets.  
    Package: +celestial.scheduling  
    Description: The cadence weight is a combination of two weights, based on  
    the last time the target was observed during the night, and  
    the last time it was observed before the current night.  
    This is calculated using the current JD, the JD of last  
    observation, and the nightly counter.  
    Input  : - The current JD.  
    - A vector of JD on which each target was last observed.  
    - A vector of night counter. The number of times each target was  
    observed on the current night.  
    * Arbitrary number of pairs of ...,key,val,... arguments.  
    The following keywords are available:  
    'MainCadence' - The main cadence of the survey [day]  
    Default is 2.4.  
    'NightCadence' - The nightly cadence [day].  
    Default is 40./1440.  
    'Nfast' - Number of epochs per night in the nightly cadence.  
    Default is 2.  
    'MainWFun' - Weight function for main cadence.  
    Default is @celestial.scheduling.fermiexp  
    'MainWFunPar' - Parameters for weight function for main  
    cadence.  
    Default is {2.4, 1, 0.03, 1, 0.5}.  
    'NightWFun' - Weight function for nightly cadence.  
    Default is @celestial.scheduling.fermiexp  
    'NightWFunPar' - Parameters for weight function for nightly  
    cadence.  
    Default is {40./1440, 1, 0.003, 1.5, 0.5}.  
    Output : - Vector of weights for each target.  
    - A structure with additional information.  
    The following fields are available:  
    .TimeSinceLastObs  
    By: Eran Ofek                      Oct 2020  
    Example :  
      
      
      
      
### celestial.scheduling.weights_fun

'exp' - A + B*exp(-t/C) 'fermi' - A + B/(1+ exp(-(t-C)/D))


    
      
    'exp' - A + B*exp(-t/C)  
    'fermi' - A + B/(1+ exp(-(t-C)/D))  
      
    Example: W=celestial.scheduling.weights_fun('fermiexp',(0:0.01:10).',[1,0,1.5,1,0.1,1,0.5,1])  
      
      
