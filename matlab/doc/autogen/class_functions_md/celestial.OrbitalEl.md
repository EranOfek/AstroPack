# Class: celestial.OrbitalEl



    
    OrbitalEl  
      
      
      
      

### Functions List

    OrbitalEl - Constractor for OrbitalEl class
    eccAnom2radius - Eccentric anomaly to radius vector
    eccAnom2trueAnom - Eccentric Anomaly to True Anomaly
    ephem - Calculate epjemerides for OrbitalEl object. For each orbital-element or time, return the Geocentric or topocentric ephemerides of the target. For definitions and formulae, see Explanatory Supplement to the Astronomical
    get.A - getter for A (semi-major axis)
    get.PeriDist - getter for A (semi-major axis)
    get.Tp - getter for periapsis time [JD]
    keplerSolve - Solve the Kepler equation for OrbitalEl object. For elliptic, parabolic, and hyperbolic orbits Simoultanously solves multiple orbtial elemsnts for a single time or for vector of times of the same length, or a single orbital element at multiple times.
    loadSolarSystem - Load the JPL Solar System orbital elements from local disk To install the orbotal elements use the Installer class.
    magnitude - Calculate magnitude for an OrbitalEl object
    meanMotion - Return the mean motion [deg/day]
    merge - Merge the orbital elements in several elements of the OrbitalEl object. This function is custom made for merging the JPL epehmerides, and may fail in other cases.
    nuDot - Calculate the time derivative of the true anomaly Description: Calculate the time derivative of the true anomaly. Correct only for e<1
    numEl - Return the number or orbital elements in each OrbitalEl element.
    period - Return the orbital period
    r2vel - Calculate orbital velocity from radius vector Description: Calculate orbital velocity from radius vector Correct only for e<1
    rDot - Calculate the time derivative of the radius vector Description: Calculate the time derivative of the radius vector. correct only for e<1
    searchMinorPlanetsNearPosition - Search all minor planets/comets near position on a specific date. Given an OrbitalEl object with multiple elements, in which each elements contains vectors of multiple orbital elements, generate epehmerides and search for all minor planets and comets that are near position (cone search).
    selectFlag - Select specific orbital-elements (targets) from an OrbitalEl object.
    semiLatusRectum - Return the semilatus rectum
    table - Generate a matlab table or orbital elements from OrbitalEl object.
    thiele_innes - Convert orbital elements to Thiele-Innes elements Description: Convert orbital elements to Thiele-Innes orbital elements.
    trueAnom2eccAnom - True Anomaly to Eccentric Anomaly
    trueAnom2radius - True anomaly to radius vector
    trueAnom2rectPos - True anomaly and radius vector to rectangular position Description: True anomaly to rectangular position
    unitTest - OrbitalEl.unitTest

### OrbitalEl

Constractor for OrbitalEl class


    
    Constractor for OrbitalEl class  
    Input  * ...,key,val,...  
    Any of the OrbitalEl properties folloed by value.  
    Defaults 'MagType='HG', K=0.017202098950000.  
    Output : - An OrbitalEl object  
    Author : Eran Ofek (Sep 2021)  
    Example: Obj = celestial.OrbitalEl  
      


### eccAnom2radius

Eccentric anomaly to radius vector


    
    Eccentric anomaly to radius vector  
    Input  : - A single element OrbitalEl object.  
    - Eccentric Anomaly.  
    - Units of Eccentric Anomaly. Default is 'rad'.  
    Output : - Radius vector.  
    Author : Eran Ofek (Sep 2021)  
    Example: Result = eccAnom2radius(OrbEl, 1);  
      


### eccAnom2trueAnom

Eccentric Anomaly to True Anomaly


    
    Eccentric Anomaly to True Anomaly  
    Input  : - A single element OrbitalEl object.  
    - Eccentric anomaly.  
    - Input and output units. Default is 'rad'.  
    Output : - True anomaly.  
    Author : Eran Ofek (Sep 2021)  
    Example: Result = eccAnom2trueAnom(OrbEl(1), 1)  
      


### ephem

Calculate epjemerides for OrbitalEl object. For each orbital-element or time, return the Geocentric or topocentric ephemerides of the target. For definitions and formulae, see Explanatory Supplement to the Astronomical


    
    Calculate epjemerides for OrbitalEl object.  
    For each orbital-element or time, return the Geocentric or  
    topocentric ephemerides of the target.  
      
    For definitions and formulae, see Explanatory Supplement to the Astronomical  
    Alamanac (Seidelmann 2006), chapter 3.313, p. 148.  
    Input  : - A single element OrbitalEl object.  
    This object may include multiple orbital elements  
    in vectors of parameters.  
    - A vector of JD in the TDT time scale.  
    If the input OrbitalEl object contains multiple  
    orbital elements, then the length of the vector of  
    times may be 1 or equal to the number of orbital  
    elements. In this case, different times corresponds  
    to different orbital elements.  
    Alternatively, if the input OrbitalEl object  
    contains a sungle orbital element, then it will be  
    calculated at the different times.  
    * ...,key,val,...  
    'Tol' - Tolerance [rad] for solving the Kepler  
    equation. Default is 1e-8.  
    'TolLT' - Tolerance [day] for the light-time  
    correction iterations. Default is 1e-6.  
    'OutUnitsDeg' - A logical indicating if to list  
    the RA and Dec in degrees. If false list in  
    radians. Default is true.  
    'Aberration' - A logical indicating if to include  
    aberration of light. Default is false.  
    Note that for the default (false) the  
    output is in an "astrometric" reference  
    frame (i.e., relative to the stars).  
    'GeoPos' - Geodetic position of the observer (on  
    Earth). [Lon (rad), Lat (rad), Height (m)].  
    If empty, then calculate geocentric  
    positions. Default is [].  
    'RefEllipsoid' - Reference ellipsoid for the  
    geodetic positions. Default is 'WGS84'.  
    'OutType' - Output type:  
    'mat' - a matrix  
    'AstroCatalog' - An AstroCatalog object.  
    Default is 'AstroCatalog'  
    'MaxIterLT' - Maximum numbre of iterations for  
    light-time corrections. Default is 5.  
    0 will force to no ligh-time correction  
    (e.g., for quick calculation).  
    'IncludeMag' - A logical indicating if to include  
    magnitude in output catalog.  
    Default is true.  
    'AddDesignation' - A logical indicating if to add  
    the asteroid designation (in the last  
    column) to the output.  
    If true, then the output will be in a  
    format of table instead of a matrix.  
    Default is true.  
    Output : - Output ephemerides with the following columns:  
    {'JD', 'RA', 'Dec', 'R', 'Delta','SOT','STO', 'Mag'}  
    and units:  
    {'day','deg','deg', 'au','au','deg','deg','mag'}.  
    Author : Eran Ofek (Sep 2021)  
    Example: OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);  
    JD = celestial.time.julday([9 9 2021])  
    Cat = ephem(OrbEl, JD +(1:1:100)')  
      
    OrbEl = celestial.OrbitalEl.loadSolarSystem('num');  
    Cat = ephem(OrbEl, JD);  
    tic;CatE = ephem(OrbEl, JD, 'GeoPos',[],'MaxIterLT',0,'IncludeMag',false);toc  
      
    compare to JPL  
    JD = celestial.time.julday([19 9 2021])+(0:1./24:1)';  
    Coo=[-116.865./RAD 33.3563./RAD 2000]  
    OrbEl1 = celestial.OrbitalEl.loadSolarSystem([],9804);  
    CatE = ephem(OrbEl1, JD, 'GeoPos',Coo, 'OutUnitsDeg',false)  
    [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',JD,'StopJD',JD+1,'StepSizeUnits','h','CENTER','675')  
    RA nd Dec diff between JPL and ephem:  
    [CatE.Catalog(:,2) - CatJPL.Catalog(:,2), CatE.Catalog(:,3) - CatJPL.Catalog(:,3)].*RAD.*3600  
    hyperbolic orbit  
    OrbEl = celestial.OrbitalEl.loadSolarSystem('unnum','A/2017 U1');  
    JD = celestial.time.julday([1 1 2018 0]);  
    Cat = ephem(OrbEl, JD+(0:1./24:1), 'OutUnitsDeg',false);  
    [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','A/2017 U1','StartJD',JD,'StopJD',JD+1,'StepSizeUnits','h','CENTER','399')  
    [Cat.Catalog(:,2) - CatJPL.Catalog(:,2), Cat.Catalog(:,3) - CatJPL.Catalog(:,3)].*RAD.*3600  
      


### get.A

getter for A (semi-major axis)


    
    getter for A (semi-major axis)  
      


### get.PeriDist

getter for A (semi-major axis)


    
    getter for A (semi-major axis)  
      


### get.Tp

getter for periapsis time [JD]


    
    getter for periapsis time [JD]  
      


### keplerSolve

Solve the Kepler equation for OrbitalEl object. For elliptic, parabolic, and hyperbolic orbits Simoultanously solves multiple orbtial elemsnts for a single time or for vector of times of the same length, or a single orbital element at multiple times.


    
    Solve the Kepler equation for OrbitalEl object.  
    For elliptic, parabolic, and hyperbolic orbits  
    Simoultanously solves multiple orbtial elemsnts for a  
    single time or for vector of times of the same length,  
    or a single orbital element at multiple times.  
    Input  : - A single element OrbitalEl object.  
    - Vector or scalar of times (e.g. JD).  
    The time of periastron will be subtracted from  
    this time.  
    * ...,key,val,...  
    'Tol' - Tolerance. Default is 1e-8 (radians).  
    'K' - Gaussian gravitational constant.  
    If empty, then use OrbitalEl object default.  
    Default is [].  
    'SubTp' - A logical indicating if to subtract the  
    time of periapsis. Default is true.  
    Output : - Vctor of True anomaly [rad].  
    - Vecor of radius vector [au].  
    - Vector of Eccentric anomaly [rad].  
    - Vector of velocity [au/day].  
    - Vector of Mean anomaly [rad]. NaN for parabolic or  
    hyperbolic.  
    Author : Eran Ofek (Sep 2021)  
    Example: OrbElA = celestial.OrbitalEl.loadSolarSystem;  
    [Nu, R, E, Vel, M] = keplerSolve(OrbElA(1), 2451545)  
    OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);  
    [Nu, R, E, Vel, M] = keplerSolve(OrbEl(1), 2451545+(1:1:10)')  
    Test parabolic orbit  
    E1.Tp = celestial.time.julday([14 4 1998 0.4358]);  
    E1.PeriDist = 1.487469;  
    JD = celestial.time.julday([ 5 8 1998]);  
    E1.W=1; E1.Incl=1; E1.Node=1; E1.Eccen=1;  
    [Nu, R, E, Vel, M] = keplerSolve(E1, JD);  
    Nu should be 66.78862 deg, R=2.133911  
      


### loadSolarSystem

Load the JPL Solar System orbital elements from local disk To install the orbotal elements use the Installer class.


    
    Load the JPL Solar System orbital elements from local disk  
    To install the orbotal elements use the Installer class.  
    Input  : - Type: [] - read all | 'num' | 'unnum' | 'comet'  
    Default is [].  
    - Minor planets designation (string) or number.  
    If empty, return all. Default is [].  
    Output : - OrbitalEl object.  
    Number of elements equal to the number of files  
    read, and in each elements there may be multiple  
    entries.  
    AUthor : Eran Ofek (Sep 2021)  
    Example: OrbEl = celestial.OrbitalEl.loadSolarSystem  
    OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804)  
    OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804)  
    OrbEl = celestial.OrbitalEl.loadSolarSystem([],'Ceres')  
      


### magnitude

Calculate magnitude for an OrbitalEl object


    
    Calculate magnitude for an OrbitalEl object  
    Input  : - A single element OrbitalEl object.  
    - R [au] Sun-target distance  
    - Delta [au] Observer-target distance  
    - Phase angle (Sun-Target-Observer angle.  
    * ...,key,val,...  
    'MagType' - MagType (e.g., 'HG'). If empty, use  
    object MagType. Default is [].  
    'MagPar' - Mag parameters (e.g., [H, G]). If empty  
    use object MagPar. Default is [].  
    'PhaseUnits' - Units of phase angle.  
    Default is 'deg'.  
    Output : - Magnitudes  
    Author : Eran Ofek (Sep 2021)  
    Example: OrbElA = celestial.OrbitalEl.loadSolarSystem('num');  
    Mag = magnitude(OrbElA, 1, 1, 0)  
      


### meanMotion

Return the mean motion [deg/day]


    
    Return the mean motion [deg/day]  
    Input  : - A single element OrbitalEl object  
    - AngularUnits. Default is 'rad'.  
    Output : - Mean motion [rad/day]  
      


### merge

Merge the orbital elements in several elements of the OrbitalEl object. This function is custom made for merging the JPL epehmerides, and may fail in other cases.


    
    Merge the orbital elements in several elements of the OrbitalEl object.  
    This function is custom made for merging the JPL  
    epehmerides, and may fail in other cases.  
    Input  : - An OrbitalEl object, with multiple elements.  
    Output : - A merged OrbitalEl objt with a single element.  
    This is always a new copy.  
    Example: OrbEl = celestial.OrbitalEl.loadSolarSystem;  
    O = merge(OrbEl);  
      


### nuDot

Calculate the time derivative of the true anomaly Description: Calculate the time derivative of the true anomaly. Correct only for e<1


    
    Calculate the time derivative of the true anomaly  
    Description: Calculate the time derivative of the true anomaly.  
    Correct only for e<1  
    Input  : - A single element OrbitalEl object.  
    - True anomaly.  
    - Input and output units. Default is 'rad'.  
    Output : - dNu/dt  
    Author : Eran Ofek (Sep 2021)  
    Example: Result = nuDot(OrbEl(1), 1)  
      


### numEl

Return the number or orbital elements in each OrbitalEl element.


    
    Return the number or orbital elements in each OrbitalEl  
    element.  
      


### period

Return the orbital period


    
    Return the orbital period  
    Input  : - A single element OrbitalEl object  
    - Units. Default is 'day'  
    Output : - Orbital period  
    Example: Result = period(OrbEl)  
      


### r2vel

Calculate orbital velocity from radius vector Description: Calculate orbital velocity from radius vector Correct only for e<1


    
    Calculate orbital velocity from radius vector  
    Description: Calculate orbital velocity from radius vector  
    Correct only for e<1  
    Input  : - A single elements OrbitalEl object.  
    - Radius vector  
    - TimeUnits. Default is 'day'  
    Output : - Velocity (default units au/day)  
    Author : Eran Ofek (Sep 2021)  
    Example: V=r2vel(OrbEl(1), 1);  [au/day]  
    V=r2vel(OrbEl(1), 1, 's').*constant.au./1e5; [km/s]  


### rDot

Calculate the time derivative of the radius vector Description: Calculate the time derivative of the radius vector. correct only for e<1


    
    Calculate the time derivative of the radius vector  
    Description: Calculate the time derivative of the radius  
    vector.  
    correct only for e<1  
    Input  : - A single element OrbitalEl object.  
    - True anomaly [rad].  
    Output : - dr/dt  
    Author : Eran Ofek (Sep 2021)  
    Example: Result = rDot(OrbEl(1), 1)  
      


### searchMinorPlanetsNearPosition

Search all minor planets/comets near position on a specific date. Given an OrbitalEl object with multiple elements, in which each elements contains vectors of multiple orbital elements, generate epehmerides and search for all minor planets and comets that are near position (cone search).


    
    Search all minor planets/comets near position on a specific date.  
    Given an OrbitalEl object with multiple elements, in which  
    each elements contains vectors of multiple orbital  
    elements, generate epehmerides and search for all minor  
    planets and comets that are near position (cone search).  
    The search is done in two steps. In the first iteration,  
    rough positions are calculated for all orbital elements,  
    while in the second iteration accurate positions are  
    calculated for the objects near the position.  
    Input  : - An OrbitalEl object (multiple elements supported).  
    - JD in TDB time scale.  
    - J2000.0 R.A. of the position to search.  
    Units are either 'deg'|'rad' (controlled via the  
    'CooUnits' key/val).  
    - J2000.0 Dec. of the position to search.  
    - Search Radius. Default is 1000.  
    * ...,key,val,...  
    'SearchRadiusUnits' - Search Radius units.  
    Default is 'arcsec'.  
    'CooUnits' - Search coordinate units.  
    Default is 'deg'.  
    'MagLimit' - Magnitude limit. Default is Inf.  
    'GeoPos' - Geodetic position of the observer (on  
    Earth). [Lon (rad), Lat (rad), Height (m)].  
    If empty, then calculate geocentric  
    positions. Default is [].  
    'RefEllipsoid' - Reference ellipsoid for the  
    geodetic positions. Default is 'WGS84'.  
    'OutUnitsDeg' - A logical indicating if the output  
    objects coordinates are in degrees (true)  
    or radians (false). Default is true.  
    'coneSearchArgs' - A cell array of additional  
    arguments to pass to imProc.match.coneSearch  
    Default is {}.  
    'AddDesignation' - A logical indicating if to add  
    the asteroid designation (in the last  
    column) to the output.  
    If true, then the output will be in a  
    format of table instead of a matrix.  
    Default is true.  
    'QuickSearchBuffer' - In the first iteration the  
    search radius is increased by this amount.  
    Default is 500 (units given by the  
    'SearchRadiusUnits' key/val).  
    Output : - An AstroCatalog object with the ephemerides of the  
    minor planets / comets found near the search  
    position. The number of elements are equal to the  
    number of elements in the input OrbitalEl object.  
    You can merge the results using AstroTable/merge.  
    - A structure array (element per Result element)  
    with the selected minor planets 'Number' and 'Designation'.  
    Author : Eran Ofek (Sep 2021)  
    Example: OrbEl= celestial.OrbitalEl.loadSolarSystem;  
    [Result, Names] = searchMinorPlanetsNearPosition(OrbEl, 2451545, 0, 0, 1000)  
      
      


### selectFlag

Select specific orbital-elements (targets) from an OrbitalEl object.


    
    Select specific orbital-elements (targets) from an OrbitalEl object.  
    Input  : - A single element OrbitalEl object that may contain  
    multiple orbital elements.  
    - A vector of logical flags, or indices to select  
    from the OrbitalEl input object.  
    - Indicate if to create a new deep copy of the object.  
    [], true, false.  
    If true, create new deep copy  
    If false, return pointer to object  
    If [] and Nargout0 then do not create new copy.  
    Otherwise, create new copy.  
    Output : - An OrbitalEl object with the selected orbits.  
    Author : Eran Ofek (Sep 2021)  
    Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('num');  
    Res   = selectFlag(OrbEl, 1, true);  
      


### semiLatusRectum

Return the semilatus rectum


    
    Return the semilatus rectum  
      


### table

Generate a matlab table or orbital elements from OrbitalEl object.


    
    Generate a matlab table or orbital elements from OrbitalEl object.  
    Input  : - An OrbitalEl object.  
    Output : - A table.  
    Author : Eran Ofek (Sep 2021)  
    Example: E=celestial.OrbitalEl.loadSolarSystem;  
    table(E)  
      


### thiele_innes

Convert orbital elements to Thiele-Innes elements Description: Convert orbital elements to Thiele-Innes orbital elements.


    
    Convert orbital elements to Thiele-Innes elements  
    Description: Convert orbital elements to Thiele-Innes  
    orbital elements.  
    Input  : - OrbitalEl object  
    Output : - Structure array with Thiele-Innes elements.  
    See also: celestial.Kepler.thiele_innes2el.m  
    Example: TI=thiele_innes(OrbEl(1));  
      


### trueAnom2eccAnom

True Anomaly to Eccentric Anomaly


    
    True Anomaly to Eccentric Anomaly  
    Input  : - A single element OrbitalEl object.  
    - True anomaly.  
    - Input and output units. Default is 'rad'.  
    Output : - Eccentric anomaly.  
    Author : Eran Ofek (Sep 2021)  
    Example: Result = trueAnom2eccAnom(OrbEl(1), 1)  
      


### trueAnom2radius

True anomaly to radius vector


    
    True anomaly to radius vector  
    Input  : - A single element OrbitalEl object.  
    - True Anomaly.  
    - Units of True Anomaly. Default is 'rad'.  
    Output : - Radius vector.  
    Author : Eran Ofek (Sep 2021)  
    Example: Result = trueAnom2radius(OrbEl, 1);  
      


### trueAnom2rectPos

True anomaly and radius vector to rectangular position Description: True anomaly to rectangular position


    
    True anomaly and radius vector to rectangular position  
    Description: True anomaly to rectangular position  
    Input  : - OrbitalEl object.  
    - True anomaly [rad].  
    - Optional radius vector. If not given will be  
    calculated from the True anaomaly.  
    - Angilar units. Default is 'rad'.  
    Output : * Either a single 3 column matrix of [X,Y,Z] or  
    X,Y,Z. Units the same as the radius vector units.  
    Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('num');  
    BodyPos = trueAnom2rectPos(OrbEl(1), 1, 1)  
    [x,y,z] = trueAnom2rectPos(OrbEl(1), 1, 1)  
    OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804);  
    BodyPos = trueAnom2rectPos(OrbEl, [1;2], [1;2])  
      


### unitTest

OrbitalEl.unitTest


    
    OrbitalEl.unitTest  
      


