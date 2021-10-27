# Package: celestial.SolarSys


### celestial.SolarSys.aberrationSolarSystem




    
      
      
    Here:  
    u_B - The Barycentric position of the object.  
    E_B - The Barycentric position of the earth.  
    S_B - The Barycentric position of the Sun.  
    E_H - The Heliocenric position of the Earth (E_B - S_B)  
    U = u_B(t-tau) - E_B(t)  
    Q = u_B(t-tau) - S_B(t-tau)  
    For definitions and formulae, see Explanatory Supplement to the Astronomical  
    Alamanac (Seidelmann 2006), chapter 3.315, p. 148.  
    Vel should be in the Barycentric system, but here we  
    approximate it in the Heliocentric system  
      
    Example: U2 = celestial.SolarSys.aberrationSolarSystem(U, E_dotH, Delta)  
      
### celestial.SolarSys.asteroid_magnitude

Calculate the magnitude of minor planets in the HG system Package: celestial.SolarSys Description: Calculate the magnitude of minor planets in the HG system. Valid for phase angles (Beta) in range 0 to 120 deg.


    
    Calculate the magnitude of minor planets in the HG system  
    Package: celestial.SolarSys  
    Description: Calculate the magnitude of minor planets in the HG system.  
    Valid for phase angles (Beta) in range 0 to 120 deg.  
    Input  : - MP-Sun distance in au.  
    - MP-observer distance in au.  
    - Phase angle in radians (Sun-Target-Observer angle).  
    - The mean absolute visual magnitude (H).  
    - The slope parameter (G), default is 0.15.  
    If G is NaN, then G is ignored and only H is used.  
    Output : - The minor planet visual magnitude.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Mag=celestial.SolarSys.asteroid_magnitude(3,2,0,15,0.15)  
    Reliable: 2  
      
      
### celestial.SolarSys.calc_all_planets_lun_occ

Lunar occultations of planets


    
    Lunar occultations of planets  
      
### celestial.SolarSys.calc_vsop87

Planetary coordinates based on the VSOP87 theory Package: celestial.SolarSys Description: Calculate planetary coordinates using the VSOP87 theory.


    
    Planetary coordinates based on the VSOP87 theory  
    Package: celestial.SolarSys  
    Description: Calculate planetary coordinates using the VSOP87 theory.  
    Input  : - Column vector of JD in the TT time scale.  
    - Object name:  
    'Mercury' : Mercury  
    'Venus'   : Venus  
    'Earth'   : Earth  
    'EMB'     : Earth-Moon barycenter  
    'Mars'    : Mars  
    'Jupiter' : Jupiter  
    'Saturn'  : Saturn  
    'Uranus'  : Uranus  
    'Neptune' : Neptune  
    'Sun'     : Sun  
    - Type of VSOP87 series to use, VSOP_Type option:  
    'a' : Heliocentric; ec. rectangular; equinox and ecliptic of J2000.0  
    'b' : Heliocentric; ec. spherical; equinox and ecliptic of J2000.0  
    'c' : Heliocentric; ec. rectangular; equinox and ecliptic of date  
    'd' : Heliocentric; ec. spherical; equinox and ecliptic of date  
    'e' : Barycentric; ec. rectangular; equinox and ecliptic of J2000.0  
    - Type of output coordinates:  
    'd' : default coordinates as given by the series type (default).  
    'E' : rectangular, Equatorial FK5 J2000.0 coordinate system,  
    possible in the case of VSOP_Type='A'|'B'|'E'.  
    Output : - Coordinates matrix, column per time.  
    In each column [X;Y;Z] or [L;B;R]. X,Y,Z,R in au. L,B in radinas.  
    - Velocity matrix, column per time.  
    In each column [Xt;Yt;Zt] or [Lt;Bt;Rt]. Xt,Yt,Zt,Rt in au/day.  
    Lt,Bt in radinas/day.  
    Referebce: Bretagnon P., Francou G. 1988 A&A 202, 309  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Coo,Vel]=celestial.SolarSys.calc_vsop87(2451545+(0:1:100)', 'Uranus', 'e', 'E');  
    Reliable: 2  
      
      
### celestial.SolarSys.earthShadowCoo

Calculate the J2000.0 equatorial coordinates of the Earth shadow at a given height


    
    Calculate the J2000.0 equatorial coordinates of the Earth shadow at a given height  
    Input  : - JD (UT1 time scale).  
    - Topocentric distance to point in shadow for which to  
    calculate the position. Default is 42164 km.  
    * ...,key,val,...  
    'DistUnits' - Default is 'km'.  
    'GeoPos' - [Lon, Lat, Height] must be in [rad, rad, m].  
    Default is [35 32 0]./(180./pi);   [rad rad m]  
    'RefEllipsoid' - Default is 'WGS84'.  
    'OutUnitsDeg' - Output is in degrees. Default is true.  
    Output : - J2000.0 RA of shadow point.  
    - J2000.0 Dec of shadow point.  
    - J2000.0 RA of anti Sun direction.  
    - J2000.0 Dec of anti Sun direction.  
    Author : Eran Ofek (Oct 2021)  
    Example: JD = 2451545 + (0:0.1:365)';  
    [RA, Dec, RAas, Decas] = celestial.SolarSys.earthShadowCoo(JD, 'OutUnitsDeg',false);  
    plot(JD, RAD.*celestial.coo.sphere_dist_fast(RA,Dec,RAas,Decas))  
      
### celestial.SolarSys.earth_vel_ron_vondrak

Earth barycentric velocity Package: celestial.SolarSys Description: Calculate the Earth barycentric velocity in respect to the mean equator and equinox of J2000.0, using a version of the Ron & Vondrak (1986) trigonometric series.


    
    Earth barycentric velocity  
    Package: celestial.SolarSys  
    Description: Calculate the Earth barycentric velocity in respect to the  
    mean equator and equinox of J2000.0, using a version  
    of the Ron & Vondrak (1986) trigonometric series.  
    Input  : - Vector of JD.  
    - Output units. Options are:  
    'cgs'  - for [cm].  
    'SI'   - for [m].  
    'agd'  - for [AU] - default.  
    Output : - Earth barycentric velocity vector refereed to the equatorial  
    system and true equinox of date. This is a three column  
    matrix [V_X, V_Y, V_Z] in which each row corresponds to one  
    epoch.  
    Tested : matlab 7.10  
    By : Eran O. Ofek                    Oct 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: EarthVel=celestial.SolarSys.earth_vel_ron_vondrak(2451545)  
    Reliable: 2  
      
      
### celestial.SolarSys.ec_longlat2cart

Convert Heliocentric ecliptic long/lat/rad referred to mean equinox of date to cartesian coordinates.


    
    Convert Heliocentric ecliptic long/lat/rad referred to mean equinox of date to cartesian coordinates.  
    Input  : - Heliocentric ecliptic long refered to mean equinox of date.  
    - Heliocentric ecliptic lat refered to mean equinox of date.  
    - Heliocentric ecliptic rad refered to mean equinox of date.  
    - Vector of JD (one per Long/lat/rad).  
    - Output type (always cartesian):  
    'Ecdate' - Ecliptic of date.  
    'Eqdate' - Equatorial mean equinox of date.  
    'EqJ2000' - Equatorial mean equinox of J2000.  
    Output : - Vector of X.  
    - Vector of Y.  
    - Vector of Z.  
    Author : Eran Ofek (Oct 2021)  
    Example: [X,Y,Z] = celestial.SolarSys.ec_longlat2cart([1 2],[0 1],[1 2],2451545+[100, 200])  
      
### celestial.SolarSys.equinox_solstice

Approximate time of Equinox and Solstice Package: celestial.SolarSys Description: Calculate the approximate time of Equinox and Solstice for a given list of years. Accurate to about 100s between year -1000 to 3000.


    
    Approximate time of Equinox and Solstice  
    Package: celestial.SolarSys  
    Description: Calculate the approximate time of Equinox and Solstice  
    for a given list of years. Accurate to about 100s between  
    year -1000 to 3000.  
    Input  : - Vector of years.  
    Output : - Structure array with 4 fields, each one contains the JD  
    of the March Equinox, June Solstice, September Equinox  
    and December Solstice, respectively.  
    Reference: Meeus 1991  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Eq=celestial.SolarSys.equinox_solstice([1962;2000])  
    Reliable: 2  
      
### celestial.SolarSys.get_horizons

Get an ephemerides for a solar system body from JPL horizons get_horizons function                                              ephem Description: get an ephemerides for a solar system body from the JPL horizons system.


    
    Get an ephemerides for a solar system body from JPL horizons  
      
    get_horizons function                                              ephem  
    Description: get an ephemerides for a solar system body from the JPL  
    horizons system.  
    OBSOLETE: Use jpl_horizons.m instead  
    Input  : - Start JD or date [D M Y Frac], [D M Y H M S], [D M Y].  
    Default time scale is UTC. Use 'TimeScale' argument for  
    other time scales (e.g., 'TT').  
    - End JD or date.  
    - Object name:  
    'Mercury', 'Venus', 'Mars',  
    'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto'  
    'Moon','Sun'  
    Alternatively, a vector of orbital elements  
    [e, q, T, Long_AscNode, Arg_Peri, Inc] (au, JD, deg)  
    or  
    ...  
      
    * Arbitrary number of pairs of input arguments:  
    ...,'keyword','value',...  
    The following keywords are available:  
    'Geod'      - Geodetic coordinates.  
    If 'Geocentric' (default), the calculate  
    geocentric coordinates.  
    Otherwise [Long, Lat, Height].  
    Where Long and Lat are in deg, and Height in km.  
    Coordinates are given relative to WGS84.  
    'StepSize'  - Tabulation step size in days, default is 1 day.  
    'StepUnit'  - {'d' | 'h' | 'm'}, default is 'd'.  
    'TimeScale' - Time scale {'UT' | 'TT'}, default is 'UT'.  
    'Type'      - planet name type {'mp' | 'p'}, default is 'mp'.  
      
      
    Output : - Structure containing the object ephemerides:  
    .JD        - Julian day  
    .J_RA      - J2000.0 astrometric R.A. [rad]  
    .J_Dec     - J2000.0 astrometric Dec. [rad]  
    .J_RA_sex  - J2000.0 astrometric R.A. [sex]  
    .J_Dec_sex - J2000.0 astrometric Dec [sex]  
    .a_RA      - Apparent R.A. with respect to true Equinox of date [rad].  
    Corrected for light-time, the gravitational deflection  
    of light, stellar aberration, precession and nutation.  
    .a_Dec     - Apparent Dec. with respect to true Equinox of date [rad].  
    .a_RA_sex  - Apparent R.A. with respect to true Equinox of date [sex].  
    .a_Dec_sex - Apparent Dec. with respect to true Equinox of date [sex].  
    .Mag       - Apparent V-band magnitude.  
    .SB        - Surface magnitude [mag/arcsec^2]  
    .Illum     - Illuminated fraction  
    .AngDiam   - Angular diamater ["]  
    .NP_PA     - PA of north pole [rad].  
    .NP_Dist   - Ang. distance to north pole ["].  
    Negative value indicate NP on hidden hemisphere.  
    .r         - Target-Sun distance [au]  
    .Delta     - Target-Observer distance [au].  
    .AngSOT    - Sun-Observer-Target angle [rad]  
    Apparent solar elongation, if negative the target center  
    behind the Sun.  
    .AngSOTd   - +1 tragets leads the Sun (morning sky)  
    -1 tragets trail the Sun (evening sky)  
    .AngSTO    - Sun-Target-Observer abgle [rad]  
    Apparent phase angle of target.  
    .AngTOM    - Target-Observer-Moon angle [rad]  
    .MoonIllum - Moon illuminated fraction  
    .Const     - Constellation  
    Reference: http://ssd.jpl.nasa.gov/horizons_batch.cgi  
    http://ssd.jpl.nasa.gov/?horizons_doc#specific_quantities  
    ftp://ssd.jpl.nasa.gov/pub/ssd/horizons_batch_example.long  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                   Jul 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data=celestial.SolarSys.get_horizons([1 1 2000],[1 2 2000],'Mars');  
    Data=celestial.SolarSys.get_horizons([1 1 2000],[1 2 2000],'5003B','Type','p');  
    Reliable: 2  
      
### celestial.SolarSys.get_moon

Get Moon position (low accuracy) Package: celestial.SolarSys Description: Get Moon position (low accuracy).


    
    Get Moon position (low accuracy)  
    Package: celestial.SolarSys  
    Description: Get Moon position (low accuracy).  
    Input  : - JD, or date (see jd.m for available formats).  
    - Geodetic coordinates [Long, Lat] in radians.  
    Output : - Structure containing Moon position, with the following fields:  
    .RA   - RA [radians]  
    .Dec  - Dec [radians]  
    .Az   - Azimuth [radians]  
    .Alt  - Altitude [radians]  
    .IllF - Illuminated fraction  
    .Phase- Moon phase angle [radians]  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jun 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Moon=celestial.SolarSys.get_moon(2451545,[1 1])  
    Reliable: 1  
      
      
### celestial.SolarSys.get_orbit_files

Get asteroids/comets orbital elements from JPL, save locally and read. Package: celestial.SolarSys OBSOLETE: see Installer/install and Installer.readElementsFileJPL Description: Get asteroids and comets orbital elements from JPL and read into a matlab structure.


    
    Get asteroids/comets orbital elements from JPL, save locally and read.  
    Package: celestial.SolarSys  
    OBSOLETE: see Installer/install and Installer.readElementsFileJPL  
    Description: Get asteroids and comets orbital elements from JPL  
    and read into a matlab structure.  
    Input  : - 'wget','get' - get the latest orbital elements file from JPL  
    'load','use' - use local orbital elements file (default).  
    - Output type: 'struct'|'orbitalel'. Default is 'orbitalel'  
    Output : - Data structure containing the following fields:  
    .Cat    : 1 - num. asteroid; 2 - unnum asteroid; 3 - comet.  
    .Number : asteroid number  
    .Name   : Designation  
    .Epoch  : Epoch [JD]  
    .a      : semi major axis [AU]  
    .q      : perihelion distance [AU]  
    .e      : eccentricity  
    .i      : J2000 Inclination [rad]  
    .w      : J2000 Argument of perihelion [rad]  
    .Om     : J2000 Longitude of ascending node [rad]  
    .M      : Mean anomaly at Epoch [rad]  
    .P      : Period [s]  
    .n      : Mean motion [deg/day]  
    .Tp     : Time of perihelion [JD]  
    .H      : Asteroid abs. mag. [mag]  
    .G      : Magnitude slope parameter  
    .Ref    : Reference  
    Tested : Matlab 7,8  
    By : Eran O. Ofek                    Nov 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Files source: http://ssd.jpl.nasa.gov/?sb_elem  
    Example: Data=celestial.SolarSys.get_orbit_files('get');  
    Reliable: 1  
      
### celestial.SolarSys.get_sun

Get Sun position (low accuracy) Package: celestial.SolarSys Description: Get Sun position (low accuracy).


    
    Get Sun position (low accuracy)  
    Package: celestial.SolarSys  
    Description: Get Sun position (low accuracy).  
    Input  : - JD, or date (see jd.m for available formats).  
    - Geodetic coordinates [Long, Lat] in radians.  
    Output : - Structure containing Sun position, with the following fields:  
    .RA     - RA [radians]  
    .Dec    - Dec [radians]  
    .Az     - Azimuth [radians]  
    .Alt    - Altitude [radians]  
    .dAzdt  - dAz/dt [radians/s]  
    .dAltdt - dAlt/dt [radians/s]  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jun 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Sun=celestial.SolarSys.get_sun(2451545,[1 1]);  
    Reliable: 1  
      
### celestial.SolarSys.jpl_horizons

Get JPL horizons ephemeris for a solar system body. Package: celestial.SolarSys Description: Get JPL horizons ephemeris for a solar system body.


    
    Get JPL horizons ephemeris for a solar system body.  
    Package: celestial.SolarSys  
    Description: Get JPL horizons ephemeris for a solar system body.  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Cat]=celestial.SolarSys.jpl_horizons;  
    [Cat]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',celestial.time.julday([14 6 2018]),'StopJD',  celestial.time.julday([20 6 2018]));  
    Reliable: 2  
      
      
### celestial.SolarSys.jup_meridian

Low accuracy formula for Jupiter central meridian Package: celestial.SolarSys Description: Low accuracy formula for Jupiter central meridian.


    
    Low accuracy formula for Jupiter central meridian  
    Package: celestial.SolarSys  
    Description: Low accuracy formula for Jupiter central meridian.  
    Input  : - Vector of JDs in TDT.  
    Output : - matrix of central meridian (in deg.) [System_I, System_II]  
    The central meridian is calculated for the geometric disk.  
    - matrix of central meridian (in deg.) [System_I, System_II]  
    The central meridian is calculated for the illuminated disk.  
    - The planetocentric declination of the Sun. (deg.)  
    - The planetocentric declination of the Earth. (deg.)  
    Reference: Meeus, J. 1991 in: Astronomical Algorithms.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Web example: http://astroclub.tau.ac.il/ephem/JovMap/index.php#JovMap  
    Example: [CMg,CMi,Ds,De]=celestial.SolarSys.jup_meridian(2451545);  
    Reliable: 2  
      
      
### celestial.SolarSys.jup_satcurve

Plot monthly curves of the position of the Galilean satellites Package: celestial.SolarSys Description: Plot monthly curves of the relative position of the Galilean satellites of Jupiter.


    
    Plot monthly curves of the position of the Galilean satellites  
    Package: celestial.SolarSys  
    Description: Plot monthly curves of the relative position of the  
    Galilean satellites of Jupiter.  
    Input  : - Month  
    - Year  
    Output : null  
    plot   : - Jovian Satellites curve for one month.  
    Reference : Meeus, J. 1991 in: Astronomical Algorithms.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Web example: http://astroclub.tau.ac.il/ephem/JovSat/  
    Example: celestial.SolarSys.jup_satcurve(1,2015)  
    Reliable: 1  
      
### celestial.SolarSys.jupiter_map

Plot Jupiter image as observed from Earth at a given time Package: celestial.SolarSys Description: Plot Jupiter image as observed from Earth at a given time.


    
    Plot Jupiter image as observed from Earth at a given time  
    Package: celestial.SolarSys  
    Description: Plot Jupiter image as observed from Earth at a given time.  
    Input  : - If two elements vector then:  
    [long_of_Sys_I, long_of_Sys_II]  
    else JD (scalar), or date vector (single date;  
    see julday.m for options). Date in TT time scale.  
    Output : null  
    Plot   : Jupiter RGB image refer to Jupiter System II, illuminated disk.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jan 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Needed : Map of Jupiter JupiterVoyagerMap.jpg (source: Voyager)  
    Web example: http://astroclub.tau.ac.il/ephem/JovMap/  
    Example: celestial.SolarSys.jupiter_map(2451545);  
    Reliable: 2  
      
### celestial.SolarSys.kuiper_check

Parallax due to Earth and object motion of a solar system object Package: celestial.SolarSys Description: Given a date and coordinates of a celestial object, calculate the parallax due to the Earth motion (Par_E) and the parallax due to the object motion (Par_K),


    
    Parallax due to Earth and object motion of a solar system object  
    Package: celestial.SolarSys  
    Description: Given a date and coordinates of a celestial object,  
    calculate the parallax due to the Earth motion (Par_E)  
    and the parallax due to the object motion (Par_K),  
    assuming the object is in e=0 orbit around the Sun,  
    its Sun distance is >>1au.  
    If (Par_E-Par_K)>Object_Proper_Motion then the object is  
    located outside the solar system.  
    Input  : - Date [Day, Month, Year, fraction_of_day]  
    or [JD].  
    - RA [H M S] or [rad] or sexagesimal.  
    - Dec [Sign D M S] or [rad] or sexagesimal.  
    - sun-object distance [au], (default is 40au).  
    Output : - Parallax due to Earth motion [arcsec/day].  
    - Parallax due to object motion [arcsec/day].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Par_E,Par_K]=celestial.SolarSys.kuiper_check([11 3 1999 0],[ 16 36 02],[+1 66 12 34]);  
    Reliable: 2  
      
### celestial.SolarSys.moon_elp82

ELP2000-82 ecliptic coordinates of the Moon Package: celestial.SolarSys Description: Calculate accurate ELP2000-82 ecliptic coordinates of the Moon, referred to the inertial mean ecliptic and equinox of date. This function was previously called moonpos.m.


    
    ELP2000-82 ecliptic coordinates of the Moon  
    Package: celestial.SolarSys  
    Description: Calculate accurate ELP2000-82 ecliptic coordinates of the  
    Moon, referred to the inertial mean ecliptic and equinox of  
    date. This function was previously called moonpos.m.  
    Input  : - Vector of julian days in TDT time scale.  
    - Otput coordinates type:  
    'q2000' : equatorial rectangular coordinates referred  
    to the FK5 equator and equinox  
    (i.e. mean equator and rotational mean  
    equinox of J2000).   - (default).  
    'qdate' : equatorial rectangular coordinates referred  
    to the true equator and equinox of date.  
    'e2000' : ecliptic rectangular coordinates referred to  
    the inertial mean ecliptic and equinox J2000.0  
    'elp82' : ecliptic rectangular coordinates referred to  
    the inertial mean ecliptic of date and  
    departure point \gamma_{2000}'.  
    'elpdt' : ecliptic rectangular coordinates referred to  
    the inertial mean ecliptic and equinox of date.  
    Output : - X [km].  
    - Y [km].  
    - Z [km].  
    See Also: mooncool.m  
    Reference : ELP2000-82B (Chapront-Touze & Chapront 1982).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jun 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [X,Y,Z]=celestial.SolarSys.moon_elp82(2451545+(1:1:100).');  
    Reliable: 2  
      
      
### celestial.SolarSys.moon_ephem

ELP2000-82 Moon ephemeris Package: celestial.SolarSys Description: Calculate very accurate ELP2000-82 apparent coordinates of the moon as observed from one of the solar system planet's. Assuming no light deflection.


    
    ELP2000-82 Moon ephemeris  
    Package: celestial.SolarSys  
    Description: Calculate very accurate ELP2000-82 apparent coordinates of  
    the moon as observed from one of the solar system planet's.  
    Assuming no light deflection.  
    Input  : - Column vector of JD in the TT time scale.  
    (If needed in UTC time scale add DeltaT directly to JD).  
    - Reference frame:  
    'J2000'   : Equatorial, Mean Equinox and Equator of  
    J2000.0 (FK5) (default).  
    Not corrected for light deflection and abberation,  
    but corrected for light-time and retardation of light.  
    'date'    : Equatorial, True Equinox and Equator of date  
    'ecliptic': Ecliptic, apparent True Equinox and ecliptic of date.  
    - Obsever position :  
    'Earth'   : Earth, Geocentric (default)  
    'Topo'    : Earth, Topocentric  
    - Observer WGS84 Geodetic position (for Topocentric coordinates).  
    [East Long. (rad), North Lat. (rad), Height (m)]  
    - Column vector of Delta T (=TT-UT1) in days,  
    needed for topocentric coordinates.  
    - The X coordinate of the celestial ephemeris pole  
    with respect to the terrestrial pole measured along  
    the 0 meridian (radians). - default is 0.  
    (one element per time).  
    - The Y coordinate of the celestial ephemeris pole  
    with respect to the terrestrial pole measured along  
    the 270 meridian (radians). default is 0.  
    (one element per time).  
    Output : - Moon's coordinates:  
    [RA (rad), Dec (rad)]  
    - Moon's distances:  
    [Delta (au), R (au), obs-sun (au), Tau (day)]  
    where R is the Sun-Planet distance  
    and Delta is the Planet-Observer distance.  
    Tau is the observer-planet light time correction.  
    - Moon's angles (approximate):  
    [Theta (rad), Phi (rad), Elon (rad)]  
    Theta is the Observer-Sun-Moon angle.  
    Phi is the observer-planet-sun angle.  
    Elon is the sun-observer-planet angle.  
    - [K], Moon's illuminated fraction of the disk.  
    Reference : ELP2000-82B (Chapront-Touze & Chapront 1982).  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                   Sep 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: UNKNOWN  
    -  
      
### celestial.SolarSys.moon_illum

Low accuracy Moon illuminated fraction Package: celestial.SolarSys Description: Low accuracy Moon illuminated fraction


    
    Low accuracy Moon illuminated fraction  
    Package: celestial.SolarSys  
    Description: Low accuracy Moon illuminated fraction  
    Input  : - JD or date (see julday.m for options).  
    Output : - Illuminated fraction of the moon.  
    - Moon Phase angle [radians].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Aug 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Illum,Ph]=celestial.SolarSys.moon_illum(2451545)  
    Reliable: 1  
      
### celestial.SolarSys.moon_librations

Moon's librations Package: celestial.SolarSys Description: Calculate the Moon's librations and P.A., including physical librations.


    
    Moon's librations  
    Package: celestial.SolarSys  
    Description: Calculate the Moon's librations and P.A., including  
    physical librations.  
    Input  : - Vector of JD.  
    - Observer geodetic position, [East long, Lat, height],  
    in radians and meters.  
    NaN for geocentric librations.  
    - Optinal parameter : [Longitude, Latitude, HP] in radians.  
    where Longitude is a column vector of apparent geocentric  
    longitude of the Moon including the effect of the nutation.  
    where Latitude is a column vector of apparent geocentric  
    latitude of the Moon. Here, HP is a column vector of the  
    Moon's geocentric horiz. parallax. If not given it will  
    be calculated. If topocentric librations are needed then  
    topocentric coordinates should be given.  
    Output : - [Libration in long, Libration in lat, P.A. of axis] in radians.  
    When the libration in longitude is positive, the the west limb  
    is exposed, when the libration in latitude is positive,  
    the north limb is exposed,  
    Reference: Astro. Algo. J. Meeus 1991  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: BUGS  
      
### celestial.SolarSys.moon_phases

Return a list of moon phases in range of dates Package: celestial.SolarSys Description: Return a list of moon phases in range of dates.


    
    Return a list of moon phases in range of dates  
    Package: celestial.SolarSys  
    Description: Return a list of moon phases in range of dates.  
    Input  : - Minimum date [D M Y] or JD.  
    - Maximum date [D M Y] or JD.  
    Output : - List of Moon phases [Phase, JD].  
    Where Phase are: 0,1,2,3 for new moon, first quarter,  
    full moon, and last quarter, respectively.  
    JD is given in the TT time scale.  
    Reference: Meeus 1991  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jul 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: MP=celestial.SolarSys.moon_phases([1 1 2015],[1 1 2016]);  
    Reliable: 2  
      
      
### celestial.SolarSys.moon_sky_brightness

Krisciunas & Schaefer (1991) sky brightness model due to the Moon Package: celestial.SolarSys Description: Given the date, object equatorial coordinates, and observer geodetic position, calculate the excess in sky brightness (V-band) in the object celestial position.


    
    Krisciunas & Schaefer (1991) sky brightness model due to the Moon  
    Package: celestial.SolarSys  
    Description: Given the date, object equatorial coordinates, and  
    observer geodetic position, calculate the excess in  
    sky brightness (V-band) in the object celestial position.  
    The function utilize the algorithm by  
    Krisciunas & Schaefer (1991).  
    Input  : - Date [day, month, year, frac_day] or JD.  
    if one element is given than assumed to be JD.  
    - Object apparent equatorial  
    coordinates [RA, Dec] in radians.  
    - Observer geodetic position [East_Long, Lat, Height],  
    radians and meters above ref ellips.  
    default is wise observatory position.  
    - Extinction coef. in V. (default is 0.3mag/airmass).  
    - Sky brightness in V. (default is 21.7 mag/sq. arcsec.).  
    Output : - The change in the V-band sky brightness caused by moonlight.  
    - Moon elongation, radians.  
    - Object-Moon distance, radians.  
    - Moon illuminated fraction.  
    Reference : Krisciunas, K. and Schaefer, B. 1991 PASP 103, 1033.  
    See also: moon_sky_brightness1.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [DeltaV,D,ObjMoonDist,K]=celestial.SolarSys.moon_sky_brightness(2451545,[1 1],[1 1],0.2,21)  
    Reliable: 2  
      
### celestial.SolarSys.mooncool

Low-accuracy topocentric equatorial coordinates of the Moon Package: celestial.SolarSys Description: Calculate low-accuracy topocentric equatorial coordinates of the Moon, referred to the equinox of date.


    
    Low-accuracy topocentric equatorial coordinates of the Moon  
    Package: celestial.SolarSys  
    Description: Calculate low-accuracy topocentric equatorial coordinates  
    of the Moon, referred to the equinox of date.  
    Input  : - matrix od dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    - [East_Long, North_Lat] of observer in radians.  
    If NaN or empty then calculate geocentric position.  
    - Algorithm:  
    'l' : very low accuracy (default).  
    0.3 deg in pos. (apparent coordinates).  
    0.003 deg. in horizontal parallax.  
    'b' : low accuracy ~1' in position.  
    Output : - vector of RA, in radians.  
    - vector of Dec. in radians.  
    - Vector of horizontal parallax.  
    r = 1/sin(HP)  SD = 0.2725.*HP  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [RA,Dec,HP]=celestial.SolarSys.mooncool(2451545+(0:1:10)',[1 1]);  
    Reliable: 1  
      
### celestial.SolarSys.moonecool

Low-accuracy geocentric ecliptical coordinate of the Moon Package: celestial.SolarSys Description: Calculate low accuracy geocentric ecliptical coordinates of the Moon, referred to the mean equinox of date. Accuracy: in longitude and latitude ~1', distance ~50km


    
    Low-accuracy geocentric ecliptical coordinate of the Moon  
    Package: celestial.SolarSys  
    Description: Calculate low accuracy geocentric ecliptical coordinates  
    of the Moon, referred to the mean equinox of date.  
    Accuracy: in longitude and latitude ~1', distance ~50km  
    To get apparent longitude add nutation in longitude.  
    Input  : - matrix od dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Longitude [radians].  
    - Latitude [radians].  
    - radis vector [km]  
    - HP [radians].  
    See Also: mooncool.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Eaxmple: [Lon,Lat,Rad,HP]=celestial.SolarSys.moonecool(2451545+(0:1:10)')  
    Reliable: 2  
      
      
### celestial.SolarSys.moonlight

Calculate the Moon illumination in Lux on horizontal surface Package: celestial.SolarSys Description: Calculate the Moon illumination in Lux on horizontal surface as a function of the Moon altitude, horizontal parallax and Elongation.


    
    Calculate the Moon illumination in Lux on horizontal surface  
    Package: celestial.SolarSys  
    Description: Calculate the Moon illumination in Lux on horizontal  
    surface as a function of the Moon altitude, horizontal  
    parallax and Elongation.  
    Input  : - Vector of Altitude in radians.  
    - Vector of Horizontal Parallax in radians.  
    - Vector of Elongation in radians.  
    Output : - Illumination in Lux on horiz. surface.  
    See also: sunlight.m, skylight.m  
    Tested : Matlab 5.2  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Illum=celestial.SolarSys.moonlight(1,0.01,1)  
    Reliable: 2  
      
### celestial.SolarSys.orbEl2ephem




    
      
      
### celestial.SolarSys.orbelem2ephem

SHORT DESCRIPTION HERE Package: celestial Description:


    
    SHORT DESCRIPTION HERE  
    Package: celestial  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
### celestial.SolarSys.pl_rotation

Calculate planetary rotation parameters Package: celestial.SolarSys Description: Calculate planetary rotation parameters.


    
    Calculate planetary rotation parameters  
    Package: celestial.SolarSys  
    Description: Calculate planetary rotation parameters.  
    Input  : - Date or JD.  
    If one column vector is given then one JD per line.  
    If 3 columns matrix is given then [D M Y] per line. (for UT=0).  
    If 4 columns matrix is given then [D M Y frac_day] per line.  
    - Planet name:  
    'Sun' | 'Mercury' | 'Venus' | 'Earth' | 'Mars' | 'JupiterI' |  
    'JupiterII' | 'JupiterIII' | 'Saturn' |  
    'Uranus' | 'Neptune' | 'Pluto'  
    Output : - Location of the prime meridian (W) [radians] measured along  
    the planet's equator in an easterly direction with respect  
    to the planet's north pole from the node (loacated at right  
    asc. pi/2+RA_0) of the planet's equator on the standard  
    equator. If W increases with time, the planet has direct  
    rotation, otherwise its retrograde.  
    - [RA_0, Dec_0], standard equatorial coordinates with equinox  
    J2000 at epoch J2000 in radians.  
    - Aproximate (oscilating)  longitude of ascending node  
    of the planet' orbit referred tomean equinox of date  
    in radians  
    (In the case of the Sun return NaN).  
    Notes  : W for Jupiter, Saturn and Uranus refer to the rotation of  
    their magnetic fields (system III). On Jupiter system I refers  
    to the mean atmospheric equatorial rotation. System II referes  
    to the mean atmospheric rotation north of the south component  
    of the north eq. belt, and south of the north comp. of the  
    south eq. belt.  
    Reference: Explanatory supplement P. 705  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [W,Coo,AN]=celestial.SolarSys.pl_rotation([1 1 2000],'JupiterII');  
    Reliable: 2  
    -  
      
### celestial.SolarSys.planar_sundial

Calculate and plot a planar sundial Package: celestial.SolarSys Description: Calculate and plot a planar sundial.


    
    Calculate and plot a planar sundial  
    Package: celestial.SolarSys  
    Description: Calculate and plot a planar sundial.  
    Input  : - Latitude of sundial [radians].  
    - The azimuth of the perpendicular to the sundial  
    plane, measured from the north, eastward [radians].  
    - The zenith distance of the direction defined by  
    the sundial straight stylus [radians].  
    - The length of the straight stylus [length units].  
    Output : null  
    Plot   : plot a sundial  
    The x-axis is the horizonatal axis;  
    the y-axis coincides with the line of greatest slope of  
    the sundial plane; and the red + mark the position of  
    the stylus footprint.  
    The line at the top mark the stylus length.  
    Reference: Astronomical Algo. Meeus.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Aug 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.SolarSys.planar_sundial(1,1,1,1)  
    Reliable: 2  
      
      
### celestial.SolarSys.planet_ephem

planet_ephem function                                                ephem Description: Planetary ephemerids generator based on VSOP87.


    
      
    planet_ephem function                                                ephem  
    Description: Planetary ephemerids generator based on VSOP87.  
    Input  : - Column vector of JD in the TT time scale, or date.  
    (If needed in UTC time scale add DeltaT directly to JD).  
    - Object name:  
    'Mercury' : Mercury  
    'Venus'   : Venus  
    'Earth'   : Earth  
    'Mars'    : Mars  
    'Jupiter' : Jupiter  
    'Saturn'  : Saturn  
    'Uranus'  : Uranus  
    'Neptune' : Neptune  
    - Obsever position :  
    'Mercury' : Mercury  
    'Venus'   : Venus  
    'Earth'   : Earth, Geocentric (default)  
    'Topo'    : Earth, Topocentric  
    'Mars'    : Mars  
    'Jupiter' : Jupiter  
    'Saturn'  : Saturn  
    'Uranus'  : Uranus  
    'Neptune' : Neptune  
    'Sun'     : Sun (Heliocentric)  
    - Reference frame:  
    'J2000'   : Equatorial, Mean Equinox and Equator of  
    J2000.0 (FK5) (default).  
    Not corrected for light deflection and abberation,  
    but corrected for light-time and retardation of light.  
    'date'    : Equatorial, True Equinox and Equator of date  
    'ecliptic': Ecliptic, apparent True Equinox and ecliptic of date.  
    - Observer WGS84 Geodetic position (for Topocentric coordinates).  
    [East Long. (rad), North Lat. (rad), Height (m)]  
    - Column vector of Delta T (=TT-UT1) in days,  
    needed for topocentric coordinates.  
    - The X coordinate of the celestial ephemeris pole  
    with respect to the terrestrial pole measured along  
    the 0 meridian (radians). - default is 0.  
    (one element per time).  
    - The Y coordinate of the celestial ephemeris pole  
    with respect to the terrestrial pole measured along  
    the 270 meridian (radians). default is 0.  
    (one element per time).  
    Output : - Planet's coordinates:  
    [RA (rad), Dec (rad)]  
    - Planet's distances:  
    [Delta (au), R (au), obs-sun (au), Tau (day)]  
    where R is the Sun-Planet distance  
    and Delta is the Planet-Observer distance.  
    Tau is the observer-planet light time correction.  
    - Planet's angles (approximate):  
    [Theta (rad), Phi (rad), Elon (rad)]  
    Theta is the Observer-Sun-Planet angle.  
    Phi is the observer-planet-sun angle.  
    Elon is the sun-observer-planet angle  
    The elongation is measured eastward.  
    - [Mag, K], Planet's magnitude and  
    Illuminated fraction of the disk of a planet.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Coo,Dist,Ang,Mag]=planet_ephem(2451545+[0:1:10].',...  
    'Venus',...  
    'Earth',...  
    'J2000');  
      
### celestial.SolarSys.planet_lowephem

Low-accuracy ephemeris of the planets (~1 arcmin) Package: celestial.SolarSys Description: Low accuracy ephemeris of the planets. Accurate to about ~1'.


    
    Low-accuracy ephemeris of the planets (~1 arcmin)  
    Package: celestial.SolarSys  
    Description: Low accuracy ephemeris of the planets.  
    Accurate to about ~1'.  
    Input  : - If single column vector then Julian day.  
    If four column vector then [D M Y FracDay].  
    If six column vector then [D M Y Hour Min Sec].  
    - Planet name:  
    {'Mercury' | 'Venus' | 'Earth' | 'Mars' | 'Jupiter' |  
    'Saturn'  | 'Uranus' | 'Neptune' | 'Sun'}  
    - Observer position within the solar system:  
    {'Mercury' | 'Venus' | 'Earth' | 'Mars' | 'Jupiter' |  
    'Saturn'  | 'Uranus' | 'Neptune' | 'Sun'},  
    default is 'Earth' (geocentric).  
    - Coordinate type:  
    'RectEcl'    - Rectangular ecliptic [au, au, au]  
    'RectEq'     - Rectangular equatorial [au, au, au]  
    'SphericEcl' - Spherical ecliptic [rad, rad, au]  
    'SphericEq'  - Spherical equatorial [rad, rad, au] - default.  
    - Equinox :  
    'J2000' (not supported in this version)  
    'date'   - default  
    Output : - Matrix of coordinates [X, Y, Z] or [RA, Dec, RadVec]  
    row per each date.  
    - Matrix of distances:  
    [Delta(observer-object) RadiusVector(Sun-Object), (Sun-Observer)]  
    - Matrix of angles:  
    [Sun-Observer-Planet,  Sun-Planet-Observer] in radians.  
    - [Magnitude, IlluminatedFraction]  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Pos,Dist,Ang,Mag]=celestial.SolarSys.planet_lowephem(2451545+(0:1:100)','Mars','Earth','SphericEq','date');  
    Reliable: 2  
      
### celestial.SolarSys.planet_obj_conj

planets_obj_conj function                                            ephem Description:   Calculate planet-object conjunctions/occultations, In which two planets are occult or found within DistThresh from each other as observed from


    
      
    planets_obj_conj function                                            ephem  
    Description:   Calculate planet-object conjunctions/occultations,  
    In which two planets are occult or found within  
    DistThresh from each other as observed from  
    ObserverPlanet.  
    Input  : - Start of search date or JD.  
    If one column vector is given then one JD.  
    If 4 columns matrix is given then [D M Y frac_day] per line.  
    - End of search date or JD.  
    If one column vector is given then one JD.  
    If 4 columns matrix is given then [D M Y frac_day] per line.  
    - Maximum angular distance threshold in arcsec, default is 3600.  
    - Observer's planet:  
    'Mercury' | 'Venus' | 'Earth' |  'Mars' |  
    'Jupiter' | 'Saturn' | 'Uranus' | 'Neptune'  
    Default is 'Earth'.  
    - Planet:  
    'Mercury' | 'Venus' | 'Earth' |  'Mars' |  
    'Jupiter' | 'Saturn' | 'Uranus' | 'Neptune'  
    - Second object J2000.0 coordinates [RA, Dec] in radians.  
    Output : - General info about conjunction/occultation  
    [JD_ExMin, Date, MinDist.*3600.*RAD, PA_MinDist.*RAD, RA, Dec, Elon.*RAD, Delta, HP.*RAD.*3600, Occ]  
    Tested : Matlab 5.3  
    By : Eran O. Ofek          December 2002  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### celestial.SolarSys.planet_radius

Planet radius and flattening factor and angular diameter. Package: celestial.SolarSys Description: Get planetary radius and flattening factor, and calculate planet angular diameter.


    
    Planet radius and flattening factor and angular diameter.  
    Package: celestial.SolarSys  
    Description: Get planetary radius and flattening factor, and calculate  
    planet angular diameter.  
    Input  : - Planet name:  
    'Mercury' | 'Venus' | 'Earth' | 'Mars' | 'Jupiter' |  
    'Saturn' | 'Uranus' | 'Neptune' | 'Pluto' | 'Sun' | 'Moon'  
    - (optional) Observer-planet distance [au].  
    If the distance is given, the angular diameter is also  
    calculated.  
    - Planetocentric latitude of the observer, Latitude  
    of subobserver [radians]. (default is 0).  
    Output : - [Planetary equatorial radius (km),  
    Planet polar radius (km),  
    Flattening factor]  
    - [Angular equatorial radius, (radians)  
    Angular polar radius, (radians)],  
    The angular equatorial radius is given only if the distance  
    is given, and the ang. polar radius is given only if the  
    latitude sub-observer is given.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### celestial.SolarSys.planets_lunar_occultations

Search and calculates for lunar occultations of the planets Package: celestial.SolarSys Description: Calculate local circumstences for lunar occultations of Planets and asteroids. Only events in which the planet is above the local horizon


    
    Search and calculates for lunar occultations of the planets  
    Package: celestial.SolarSys  
    Description: Calculate local circumstences for lunar occultations of  
    Planets and asteroids.  
    Only events in which the planet is above the local horizon  
    will be selected.  
    Input  : - Search start date [JD] or [D M Y].  
    - Search end date [JD] or [D M Y].  
    - Cell array of planets to search.  
    Alternatively a string indicating one of the following options:  
    'planets' - search all planets {'Mercury',..,'Neptune'}  
    'all'     - search all planets and 4 primary asteroids and 4  
    Jovian satellites.  
    - Geodetic position [Long, Lat, Height],  
    where Long and Lat are in deg, and Height in km.  
    - Minimum seperation conjunctions to find [deg].  
    Output : - Matrix of occultations with the following columns:  
    (1) - Disappereance (1) or Reapperaence (2).  
    (2) - Planet index in the planet names cell array (see next output argument).  
    (3) - JD for outer contact [UTC]  
    (4) - JD for inner contact [UTC]  
    (5) - Planet J2000 RA [deg]  
    (6) - Planet J2000 Dec [deg]  
    (7) - PA of occultations [deg] relative to Moon center  
    (8) - Planet Azimuth [deg]  
    (9) - Planet Altitude [deg]  
    (10)- Planet Magnitude  
    (11)- Planet angular diameter [arcsec]  
    (12)- Moon Illuminated fraction []  
    (13) - Solar Elongation [deg]  
    - Cell array of correspondinf planet names  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Dec 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: RAD = 180./pi;  
    [OccList,PlanetNames]=planets_lunar_occultations(2451545,2451545+100,{'Venus'},[35 32 0]./RAD,1);  
    Reliable: 2  
    -  
      
### celestial.SolarSys.planets_magnitude

Calculate the planets apparent magnitude Package: celestial.SolarSys Description: Calculate the planets apparent magnitude.


    
    Calculate the planets apparent magnitude  
    Package: celestial.SolarSys  
    Description: Calculate the planets apparent magnitude.  
    Input  : - Planet name:  
    'Mercury' | 'Venus' | 'Earth' | 'Mars' | 'Jupiter' |  
    'Saturn' | 'Uranus' | 'Neptune' | 'Pluto'  
    - Sun-planet distance in au.  
    - Observer-planet distance in au.  
    - Sun observer distance in au.  
    - Band (filter) type:  
    'V' : visual (default)  
    'U' : UV  
    'B' : blue  
    - The difference between the Saturnicentric longitudes of the Sun  
    and the Earth (radians),  
    relavant only for Saturn.  
    - Saturnocentric sub-Earth latitude (radians),  
    relavant only for Saturn.  
    Output : - The planet magnitude  
    Reference: A&A supp.  
    Tested : matlab 5.3  
    By : Eran O. Ofek                    May 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: M=celestial.SolarSys.planets_magnitude('Mars',1.5,0.6,1,'V');  
    Reliable: 2  
      
### celestial.SolarSys.planets_rotation

Planet north pole, rotation rate and the primery meridian Package: celestial.SolarSys Description: Return Planet north pole, rotation rate and the primery meridian.


    
    Planet north pole, rotation rate and the primery meridian  
    Package: celestial.SolarSys  
    Description: Return Planet north pole, rotation rate and the primery  
    meridian.  
    Input  : - PlanetName {'Sun' | 'Mercury' | 'Venus' | 'Earth' |  
    'Mars' | 'Jupiter' | 'Saturn' | 'Uranus' |  
    'Neptune' | 'Pluto'}  
    - Vector of Julian days.  
    - Jupiter rotation system {'I' | 'II' | 'III'},  
    default is 'I'.  
    Output : - Alpha0 : J2000.0 Right Ascension of the north pole [radians].  
    - Delta0 : J2000.0 Declination of the north pole [radians].  
    - W : location of the prime meridian measured along the  
    planet's equator in an easterly direction with  
    respect to the planet's north pole from the node  
    (located at RA 90deg+Alpha0) of the planet's  
    equator on the standard equator [radians].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Dec 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Alpha0,Delta0,W]=celestial.SolarSys.planets_rotation('Jupiter',2451545,'II')  
    Reliable: 2  
      
      
### celestial.SolarSys.plants_object_conjunctions

Local circumstences for conjunctions of planets with a given object Package: celestial.SolarSys Description: Calculate local circumstences for conjunctions of planets with a given object.


    
    Local circumstences for conjunctions of planets with a given object  
    Package: celestial.SolarSys  
    Description: Calculate local circumstences for conjunctions of planets  
    with a given object.  
    Input  : - Search start date [JD] or [D M Y].  
    - Search end date [JD] or [D M Y].  
    - J2000 [RA, Dec] of object in radians.  
    - Cell array of planets to search.  
    Alternatively a string indicating one of the following options:  
    'planets' - search all planets {'Mercury',..,'Neptune'}  
    'all'     - search all planets and 4 primary asteroids and 4  
    Jovian satellites.  
    - Geodetic position [Long, Lat, Height],  
    where Long and Lat are in deg, and Height in km.  
    - Minimum seperation conjunctions to find [deg].  
    Output : - Matrix of occultations with the following columns:  
    (1) - Planet index in the planet names cell array (see next output argument).  
    (2) - JD of conjunction [UTC]  
    (3) - Min distance [deg]  
    (4) - PA of conjunction [deg] relative to object  
    (5) - Solar-object angular distance [deg].  
    - Cell array of correspondinf planet names  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Dec 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [ConjList,PlanetNames]=celestial.SolarSys.plants_object_conjunctions(2451545,2451545+10,[1 0],'planets',[1 1 0],1);  
    Reliable: 2  
    -  
### celestial.SolarSys.ple_earth

Low-accuracy planetray ephemeris for Earth Package: celestial.SolarSys Description: Low accuracy planetray ephemeris for Earth. Calculate Earth heliocentric longitude, latitude and radius vector referred to the mean ecliptic and equinox of date.


    
    Low-accuracy planetray ephemeris for Earth  
    Package: celestial.SolarSys  
    Description: Low accuracy planetray ephemeris for Earth. Calculate  
    Earth heliocentric longitude, latitude and radius  
    vector referred to the mean ecliptic and equinox of date.  
    Accuarcy: Better than 1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Ecliptic Longitude in radians.  
    - Ecliptic Latitude in radians.  
    - Radius vector in au.  
    Reference: VSOP87  
    See also: ple_planet.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_earth([1 1 2000 0])  
    Reliable: 2  
      
      
      
### celestial.SolarSys.ple_force

Calculate the net Sun+planets force on a solar system body.


    
    Calculate the net Sun+planets force on a solar system body.  
    Input  : - Target body positiopn vector.  
    This is a 3 X Ntimes matrix, where 3 is the numbr of  
    coordinates, and Ntimes the number of times.  
    - Vector of JD.  
    - Output type (always cartesian):  
    'Ecdate' - Ecliptic of date.  
    'Eqdate' - Equatorial mean equinox of date.  
    'EqJ2000' - Equatorial mean equinox of J2000.  
    Default is 'EqJ2000'  
    - A logical indicating if to include the Sun force.  
    Default is true.  
    Output : - A 3 X Ntimes matrix of force vectors on the body in each  
    epoch (JD). Units: Solar-mass * au / day^2  
    Author : Eran Ofek (Oct 2021)  
    Example: Force = celestial.SolarSys.ple_force([2 2 2]', 2451545)  
      
### celestial.SolarSys.ple_jupiter

Low accuracy planetray ephemeris for Jupiter. Package: celestial.SolarSys Description: Low accuracy planetray ephemeris for Jupiter. Calculate Jupiter heliocentric longitude, latitude and radius vector referred to mean ecliptic and equinox of date.


    
    Low accuracy planetray ephemeris for Jupiter.  
    Package: celestial.SolarSys  
    Description: Low accuracy planetray ephemeris for Jupiter. Calculate  
    Jupiter heliocentric longitude, latitude and radius  
    vector referred to mean ecliptic and equinox of date.  
    Accuarcy: ~1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Longitude in radians.  
    - Latitude in radians.  
    - Radius vector in au.  
    Reference: VSOP87  
    See also: ple_planet.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_jupiter(2451545)  
    Reliable: 2  
      
      
      
### celestial.SolarSys.ple_mars

Low-accuracy planetray ephemeris for Mars Package: celestial.SolarSys Description: Low accuracy planetray ephemeris for Mars. Calculate Mars heliocentric longitude latitude and radius vector referred to mean ecliptic and equinox of date.


    
    Low-accuracy planetray ephemeris for Mars  
    Package: celestial.SolarSys  
    Description: Low accuracy planetray ephemeris for Mars. Calculate  
    Mars heliocentric longitude latitude and radius vector  
    referred to mean ecliptic and equinox of date.  
    Accuarcy: Better than 1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Longitude in radians.  
    - Latitude in radians.  
    - Radius vector in au.  
    Reference: VSOP87  
    See also: ple_planet.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_mars(2451545)  
    Reliable: 2  
      
      
      
### celestial.SolarSys.ple_mercury

Low accuracy ephemerides for Mercury Package: celestial.SolarSys Description: Low accuracy ephemerides for Mercury. Calculate Mercury heliocentric longitude, latitude and radius vector referred to mean ecliptic and equinox of date.


    
    Low accuracy ephemerides for Mercury  
    Package: celestial.SolarSys  
    Description: Low accuracy ephemerides for Mercury. Calculate  
    Mercury heliocentric longitude, latitude and radius  
    vector referred to mean ecliptic and equinox of date.  
    Accuarcy: better than 1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Longitude in radians.  
    - Latitude in radians.  
    - Radius vector in au.  
    Reference: VSOP87  
    See also: ple_planet.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_mercury(2451545+(0:1:10)')  
    Reliable: 2  
      
      
      
### celestial.SolarSys.ple_neptune

Low accuracy planetray ephemeris for Neptune Package: celestial.SolarSys Description: Low accuracy planetray ephemeris for Neptune. Calculate Neptune heliocentric longitude, latitude and radius vector referred to mean ecliptic and equinox of date.


    
    Low accuracy planetray ephemeris for Neptune  
    Package: celestial.SolarSys  
    Description: Low accuracy planetray ephemeris for Neptune. Calculate  
    Neptune heliocentric longitude, latitude and radius  
    vector referred to mean ecliptic and equinox of date.  
    Accuarcy: Better than 1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Longitude in radians.  
    - Latitude in radians.  
    - Radius vector in au.  
    Reference: VSOP87  
    See also: ple_planet.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_neptune(2451545+(0:1:10)')  
    Reliable: 2  
      
      
      
### celestial.SolarSys.ple_planet

Low accuracy ephemeris for the main planets Package: celestial.SolarSys Description: Low accuracy ephemeris for the main planets. Given a planet name calculate its heliocentric coordinates referred to mean ecliptic and equinox of date.


    
    Low accuracy ephemeris for the main planets  
    Package: celestial.SolarSys  
    Description: Low accuracy ephemeris for the main planets. Given a  
    planet name calculate its heliocentric coordinates  
    referred to mean ecliptic and equinox of date.  
    Accuarcy: Better ~1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    - Planet name:  
    {'Mercury' | 'Venus' | 'Earth' |  'Mars' |  
    'Jupiter' | 'Saturn' | 'Uranus' | 'Neptune'}  
    - Coordinate system:  
    'LBR' : [L, B, R] in (rad, rad, au), default.  
    'XYZ' : [X, Y, Z] in au and ecliptic systems.  
    Output : - Heliocentric longitude in radians, or X in au.  
    - Heliocentric latitude in radians, or Y in au.  
    - Heliocentric radius vector in au, or Z in au.  
    Reference: VSOP87  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_planet(2451545+[1:1:100]','Mercury');  
    Reliable: 2  
      
### celestial.SolarSys.ple_saturn

Low accuracy planetray ephemeris for Saturn Package: celestial.SolarSys Description: Low accuracy planetray ephemeris for Saturn. Calculate Saturn heliocentric longitude, latitude and radius vector referred to mean ecliptic and equinox of date.


    
    Low accuracy planetray ephemeris for Saturn  
    Package: celestial.SolarSys  
    Description: Low accuracy planetray ephemeris for Saturn. Calculate  
    Saturn heliocentric longitude, latitude and radius  
    vector referred to mean ecliptic and equinox of date.  
    Accuarcy: ~1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Longitude in radians.  
    - Latitude in radians.  
    - Radius vector in au.  
    Reference: VSOP87  
    See also: ple_planet.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_saturn([1 1 2000 0])  
    Reliable: 2  
      
      
      
### celestial.SolarSys.ple_uranus

Low accuracy ephemeris for Uranus Package: celestial.SolarSys Description: Low accuracy ephemeris for Uranus. Calculate Uranus heliocentric longitude, latitude and radius vector referred to mean ecliptic and equinox of date.


    
    Low accuracy ephemeris for Uranus  
    Package: celestial.SolarSys  
    Description: Low accuracy ephemeris for Uranus. Calculate Uranus  
    heliocentric longitude, latitude and radius vector  
    referred to mean ecliptic and equinox of date.  
    Accuarcy: ~1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Longitude in radians.  
    - Latitude in radians.  
    - Radius vector in au.  
    Reference: VSOP87  
    See also: ple_planet.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_uranus([1 1 2000 0])  
    Reliable: 2  
      
      
      
### celestial.SolarSys.ple_venus

Low accuracy ephemeris for Venus Package: celestial.SolarSys Description: Low accuracy ephemeris for Venus. Calculate Venus heliocentric longitude, latitude and radius vector referred to mean ecliptic and equinox of date.


    
    Low accuracy ephemeris for Venus  
    Package: celestial.SolarSys  
    Description: Low accuracy ephemeris for Venus. Calculate  
    Venus heliocentric longitude, latitude and radius  
    vector referred to mean ecliptic and equinox of date.  
    Accuarcy:  Better than 1' in long/lat, ~0.001 au in dist.  
    Input  : - matrix of dates, [D M Y frac_day] per line,  
    or JD per line. In TT time scale.  
    Output : - Longitude in radians.  
    - Latitude in radians.  
    - Radius vector in au.  
    Reference: VSOP87  
    See also: ple_planet.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [L,B,R]=celestial.SolarSys.ple_venus(2451545)  
    Reliable: 2  
      
      
      
### celestial.SolarSys.ple_xyzAll

Get approximate cartesian coordinates of major planets in a matrix. Return a matrix of 3 X Nplanets X Ntimes of cartesian coordinates.


    
    Get approximate cartesian coordinates of major planets in a matrix.  
    Return a matrix of 3 X Nplanets X Ntimes of cartesian coordinates.  
    Input  : - A vector of JD.  
    - Output type (always cartesian):  
    'Ecdate' - Ecliptic of date.  
    'Eqdate' - Equatorial mean equinox of date.  
    'EqJ2000' - Equatorial mean equinox of J2000.  
    Default is 'EqJ2000'.  
    Output : - A matrix of 3 X Nplanets X Ntimes of cartesian coordinates.  
    Author : Eran Ofek (Oct 2021)  
    Example: S = celestial.SolarSys.ple_xyzAll(2451545+(1:1:10));  
      
### celestial.SolarSys.read_mpc_packed_epoch

Convert the MPC packed date format to JD Package: celestial.SolarSys Description: Convert the MPC packed date format to JD.


    
    Convert the MPC packed date format to JD  
    Package: celestial.SolarSys  
    Description: Convert the MPC packed date format to JD.  
    Input  : - Cell array in which each cell containing string of packed  
    date.  
    Output : - Vector of JD.  
    Reference: http://www.cfa.harvard.edu/iau/info/PackedDates.html  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jan 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### celestial.SolarSys.rise_set

Calculate rise/set times Package: celestial.SolarSys Description: Given an object coordinates and observer position, calculate rise/set/transit times, and azimuth and altitude. The times are in the UT1 (not UTC) system.


    
    Calculate rise/set times  
    Package: celestial.SolarSys  
    Description: Given an object coordinates and observer position,  
    calculate rise/set/transit times, and azimuth and altitude.  
    The times are in the UT1 (not UTC) system.  
    Input  : - Object position [RA, Dec] in radians.  
    If object is changing is position, then give:  
    [RA(d-1), Dec(d-1); RA(d), Dec(d); RA(d+1), Dec(d+1)]  
    where RA(x) and Dec(x) are the RA/Dec in:  
    (d-1) day-1 at 0 TDT,  
    (d) day at 0 TDT,  
    (d+1) day+1 at 0 TDT.  
    - Greenwich apparent sidereal time at 0 UT on day (d),  
    in fraction of days.  
    - The geometric altitude of the needed event (radians).  
    e.g., -0.5667/RAD for stars and planets (default).  
    -0.8333/RAD for the Sun  
    0.7275*HorizPar - 0.5667/RAD for the Moon.  
    -6,-12,-18 for twilight.  
    - Observer geodetic position [East Long, North Lat],  
    in radians.  
    Default is the Wise obs. position [  34.763, 30.596]/RAD.  
    - Delta T (=UT-UTC) [fraction of day], default is 0.  
    Output : - Time of [Transit; Rise; Set] in fraction of day, for day (d).  
    NaN values for no event. The time is in U.T.  
    - Transit altitude in radians.  
    - Rise/Set azimuth in radians.  
    Reference: Meeus J., 1991 in Astronomical Algorithms  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Sep 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### celestial.SolarSys.saturn_rings

Calculate the orientation angles for Saturn's rings Package: celestial.SolarSys Description: Calculate the orientation angles for Saturn's rings.


    
    Calculate the orientation angles for Saturn's rings  
    Package: celestial.SolarSys  
    Description: Calculate the orientation angles for Saturn's rings.  
    Input  : - Time of observation, JD, TT time scale.  
    Output : - The saturnicentric latitude of the observer referred to the  
    plane of the ring, positive towereds the north, [radians].  
    - Saturnicentric latitude of the Sun referred to the  
    plane of the ring; positive towerds the north, [radians].  
    - Difference between saturnicentric longitude of sun  
    and observer, [radians].  
    - The geocentric position angle of the northern semi minor axis  
    of the apparent ellipse of the ring, measured from the north  
    towerds the east, [radians].  
    - Major axis of ring [arcsec].  
    Columns 1 to 5 are for:  
    Outer edge of outer ring  
    Inner edge of outer ring  
    Outer edge of inner ring  
    Inner edge of inner ring  
    Inner edge of dusky ring  
    - Minor axis of ring [arcsec].  
    Columns 1 to 5 are for:  
    Outer edge of outer ring  
    Inner edge of outer ring  
    Outer edge of inner ring  
    Inner edge of inner ring  
    Inner edge of dusky ring  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 2001  
    Web example: http://astroclub.tau.ac.il/ephem/Saturn/index.php  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [SLO,SLS,DU,P,Major,Minor]=celestial.SolarSys.saturn_rings(2451545);  
    Reliable: 2  
    -  
### celestial.SolarSys.search_conj

Celestial conjunctions between moving objects Package: celestial.SolarSys Description: Search for conjunctions on the celestial sphere between two moving objects given thier coordinates as a function of time.


    
    Celestial conjunctions between moving objects  
    Package: celestial.SolarSys  
    Description: Search for conjunctions on the celestial sphere between  
    two moving objects given thier coordinates as a function  
    of time.  
    Input  : - Column vector of JD.  
    - First list of coordinates [radians] and other properties  
    for moving source.  
    - Columns indices of RA and Dec in first source list.  
    If empty matrix (i.e., []), then set it to [1 2].  
    - Vector of flags indicating the type of column in the first  
    source list: 0 - non angle, or 1 - angle property.  
    Angle properties (0..2pi) are interpolated using  
    interp_diff_ang.m. If empty matrix (i.e., []), then  
    assumes onlt the RA column is an angle property.  
    - List of coordinates [radians] and other properties for  
    second moving source.  
    - Columns indices of RA and Dec in second source list.  
    If empty matrix (i.e., []), then set it to [1 2].  
    - Vector of flags indicating the type of column in the second  
    source list: 0 - non angle, or 1 - angle property.  
    Angle properties (0..2pi) are interpolated using  
    interp_diff_ang.m. If empty matrix (i.e., []), then  
    assumes onlt the RA column is an angle property.  
    - Minimum seperation to look for [radians].  
    Output : - Structure of all the conjunction between the bodies in  
    the two lists. The structure contains the following fields:  
    .MinJD    - Vector of JD of conjunctions.  
    .MinDist  - Vector of minimum separation [radians] in  
    each conjunction.  
    .MinPA    - Vector of PA [radians] in each conjunction  
    (at .MinJD)  
    .List1    - Matrix of List1 interpolated to the time  
    of each conjunction (line per conjunction).  
    .List2    - Matrix of List2 interpolated to the time  
    of each conjunction (line per conjunction).  
    - Indices of conjunctions that their .MinDist is smaller  
    than the minimum required separation. Note that the  
    conjunction structure may include some events with  
    minimum distance larger than the required minimum  
    separation.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Web example: http://astroclub.tau.ac.il/ephem/PlanetsConj/  
    http://astroclub.tau.ac.il/ephem/AsteroidsPlanetsConj/  
    http://astroclub.tau.ac.il/ephem/LunarOcc/PlanetsConj/  
    http://astroclub.tau.ac.il/ephem/LunarOcc/Planets/  
    Example: search_conj(JD,List1,[1 2],[],List2,[1 2],[],1./RAD);  
    Reliable: 2  
      
### celestial.SolarSys.search_conj_sm

Celestial conjunctions on the between moving and stationary objects Package: celestial.SolarSys Description: Search for conjunctions on the celestial sphere between a list of stationary points and a moving object given the coordinates of the moving object as a function of time.


    
    Celestial conjunctions on the between moving and stationary objects  
    Package: celestial.SolarSys  
    Description: Search for conjunctions on the celestial sphere between a  
    list of stationary points and a moving object given the  
    coordinates of the moving object as a function of time.  
    Input  : - Column vector of JD, refering to the moving object.  
    - First list of coordinates [radians] and other properties  
    for moving source.  
    - Columns indices of RA and Dec in first source list.  
    If empty matrix (i.e., []), then set it to [1 2].  
    - Vector of flaga indicating the type of column in the first  
    source list: 0 - non angle, or 1 - angle property.  
    Angle properties (0..2pi) are interpolated using  
    interp_diff_ang.m. If empty matrix (i.e., []), then  
    assumes onlt the RA column is an angle property.  
    - List of coordinates [radians] and other properties for  
    stationary points.  
    - Columns indices of RA and Dec in second source list.  
    If empty matrix (i.e., []), then set it to [1 2].  
    - Vector of flaga indicating the type of column in the first  
    source list: 0 - non angle, or 1 - angle property.  
    Angle properties (0..2pi) are interpolated using  
    interp_diff_ang.m. If empty matrix (i.e., []), then  
    assumes onlt the RA column is an angle property.  
    - Minimum seperation to look for [radians].  
    Output : - Structure of all the conjunction between the bodies in  
    the two lists. The structure contains the following fields:  
    .MinJD    - Vector of JD of conjunctions.  
    .MinDist  - Vector of minimum separation [radians] in  
    each conjunction.  
    .MinPA    - Vector of PA [radians] in each conjunction  
    (at .MinJD)  
    .List1    - Matrix of List1 interpolated to the time  
    of each conjunction (line per conjunction).  
    .List2    - Matrix of List2 interpolated to the time  
    of each conjunction (line per conjunction).  
    - Indices of conjunctions that their .MinDist is smaller  
    than the minimum required separation. Note that the  
    conjunction structure may include some events with  
    minimum distance larger than the required minimum  
    separation.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Web example: http://astroclub.tau.ac.il/ephem/PlanetsGC/  
    Example: celestial.SolarSys.search_conj(JD,List1,[1 2],[],List2,[1 2],[],1./RAD);  
    Reliable: 2  
      
### celestial.SolarSys.skylight

Calculate the total sky illumination in Lux on horizontal surface Package: celestial.SolarSys Description: Calculate the total sky illumination due to the Sun, Moon, stars and air-glow, in Lux on horizontal surface as a function of time.


    
    Calculate the total sky illumination in Lux on horizontal surface  
    Package: celestial.SolarSys  
    Description: Calculate the total sky illumination due to the Sun, Moon,  
    stars and air-glow, in Lux on horizontal surface as a  
    function of time.  
    Input  : - vector of JD.  
    - Geodetic position [East_Long, North_Lat] in radians.  
    Output : - Total illumination in Lux on horiz. surface.  
    - Sun+sky illumination in Lux on horiz. surface.  
    - Moon illumination in Lux on horiz. surface.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Tot_Illum, Sun_Illum, Moon_Illum]=skylight(2451545,[1 1])  
    Reliable: 2  
      
      
### celestial.SolarSys.solarlong2jd

Time in which the Sun is in a given solar longitude Package: celestial.SolarSys Description: Calculate time in which the Sun is in a given solar longitude, using low accuracy formulae (15 min. precision).


    
    Time in which the Sun is in a given solar longitude  
    Package: celestial.SolarSys  
    Description: Calculate time in which the Sun is in a given solar  
    longitude, using low accuracy formulae (15 min. precision).  
    Input  : - Vector of solar longitude (radians).  
    - Matrix of Approximate date of solar longitude [Year, Month].  
    - Equinox for input longitude:  
    'g' - True Geometric referred to the mean equinox of date.  
    'a' - Apparent, referred to the true equinox of date.  
    'j' - J2000, referred to the J2000.0 equinox. (default).  
    - Algorithm:  
    'l' - low accuracy, default.  
    'e' - Exact solution.  
    Output : - Vector of JDs  
    See Also : suncoo1.m; suncoo.m  
    Reference : Ofek, E. 2000, JIMO, 28, 176  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Sep 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: OutJD=celestial.SolarSys.solarlong2jd(0,[2000 3] ,'a','l')  
    Reliable: 2  
      
### celestial.SolarSys.sun_ephem

Sun ephemeris POackage: celestial.SolarSys Description:  Sun ephemerids generator


    
    Sun ephemeris  
    POackage: celestial.SolarSys  
    Description:  Sun ephemerids generator  
    Input  : - Column vector of JD in the TT time scale, or date (see julday.m).  
    - Obsever position :  
    'Mercury' : Mercury  
    'Venus'   : Venus  
    'Earth'   : Earth, Geocentric (default)  
    'Topo'    : Earth, Topocentric   < BUG  
    'Mars'    : Mars  
    'Jupiter' : Jupiter  
    'Saturn'  : Saturn  
    'Uranus'  : Uranus  
    'Neptune' : Neptune  
    'Sun'     : Sun (Heliocentric)  
    - Reference frame:  
    'J2000'   : Equatorial, Mean Equinox and Equator of  
    J2000.0 (FK5) (default).  
    Not corrected for light deflection and abberation,  
    but corrected for light-time and retardation of light.  
    'date'    : Equatorial, True Equinox and Equator of date  
    'ecliptic': Ecliptic, apparent True Equinox and ecliptic of date.  
    - Observer WGS84 Geodetic position (for Topocentric coordinates).  
    [East Long. (rad), North Lat. (rad), Height (m)]  
    - Column vector of Delta T (=TT-UT1), needed for topocentric coordinates.  
    - The X coordinate of the celestial ephemeris pole  
    with respect to the terrestrial pole measured along  
    the 0 meridian (radians). - default is 0.  
    (one element per time).  
    - The Y coordinate of the celestial ephemeris pole  
    with respect to the terrestrial pole measured along  
    the 270 meridian (radians). default is 0.  
    (one element per time).  
    Output : - Sun's coordinates:  
    [RA (rad), Dec (rad)]  
    - Sun's distances: [R (au), Tau (day)]  
    Tau is the observer-planet light time correction.  
    - [Mag], Sun apparent magnitude.  
    Tested : Matlab 5.3 - in process  
    By : Eran O. Ofek                    May 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
### celestial.SolarSys.sun_rise_set

Calculate Sun rise/set Package: celestial.SolarSys Description: Given the coordinates and observer position, calculate rise/set/transit/twilight times and azimuth and altitude for the Sun. The accuracy depends on the function used


    
    Calculate Sun rise/set  
    Package: celestial.SolarSys  
    Description: Given the coordinates and observer position, calculate  
    rise/set/transit/twilight times and azimuth and altitude  
    for the Sun. The accuracy depends on the function used  
    for calculating the solar position. With the default  
    sun-position function, the geometric accuracy is about  
    a few seconds.  
    Input  : - Date [D M Y] or julian day [JD], one date/jd per line.  
    - Observer geodetic position [East Long, North Lat, Height (meters)],  
    in radians.  
    Default is the Wise obs. position [  34.763, 30.596]/RAD.  
    - East Time Zone, default is 2 [hours].  
    - Delta T (=UT-UTC) [fraction of day], default is 0.  
    Output : - [Morning Astronomical Twilight,  
    Morning Nautical Twilight,  
    Morning Civil Twilight,  
    Rise,  
    Transit,  
    Set,  
    Evening Civil Twilight,  
    Evening Nautical Twilight,  
    Evening Astronomical Twilight]  
    Line per each date. In fraction of day.  
    The time is relative to Local Time  
    (=UT + Time Zone) [hours].  
    - [Rise Az, Transit Alt, Set Az]  
    Line per each date. In radians.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Sep 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Time,Ang]=celestial.SolarSys.sun_rise_set(2451545+(0:5:365)',[35 32 0].*pi./180,0,0)  
    Reliable: 2  
      
### celestial.SolarSys.suncoo

Low-accuracy position of the Sun (0.01 deg in long). Package: celestial.SolarSys Description: Calculate the Sun equatorial coordinates using low accuracy formale. Accuracy : 0.01 deg. in long.


    
    Low-accuracy position of the Sun (0.01 deg in long).  
    Package: celestial.SolarSys  
    Description: Calculate the Sun equatorial coordinates using low  
    accuracy formale. Accuracy : 0.01 deg. in long.  
    Input  : - Vector of JDs.  
    - Equinox for output coordinates:  
    'g' - True Geometric referred to the mean equinox of date.  
    'a' - Apparent, referred to the true equinox of date. (default).  
    'j' - J2000, referred to the J2000.0 equinox.  
    Output : - vector of RA, in radians.  
    - vector of Dec. in radians.  
    - Vector of radius vectors, in AU.  
    - Solar longitude in the same ref. frame as RA/Dec. (radians).  
    - Equation of Time [Minuts of time]  
    See Also : suncoo1; mooncoo  
    Tested : matlab 5.3  
    By : Eran O. Ofek                    Sep 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [RA,Dec,R,SL,EquationTime]=celestial.SolarSys.suncoo(2451545+[0:1:10]','j');  
    Reliable: 1  
      
### celestial.SolarSys.suncoo1

Low-accuracy coordinates of the Sun (1950-2050 range) Package: celestial.SolarSys Description: Calculate the Sun equatorial coordinates using low accuracy formaulae for the range 1950 to 2050. Accuracy : 0.01 deg. in long, 0.1m in Equation of Time


    
    Low-accuracy coordinates of the Sun (1950-2050 range)  
    Package: celestial.SolarSys  
    Description: Calculate the Sun equatorial coordinates using  
    low accuracy formaulae for the range 1950 to 2050.  
    Accuracy : 0.01 deg. in long, 0.1m in Equation of Time  
    Input  : - Vector of JDs  
    Output : - vector of RA, in radians.  
    - vector of Dec. in radians.  
    - Vector of radius vectors, in AU.  
    Tested : Matlab 5.2  
    By : Eran O. Ofek                    August 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: suncoo.m, mooncoo.m  
    Example: [RA,Dec,RadVec]=celestial.SolarSys.suncoo1(julday([1 1 2000]));  
    Reliable: 1  
      
### celestial.SolarSys.sunlight

Calculate the Sun illumination in Lux on horizontal surface Package: celestial.SolarSys Description: Calculate the Sun illumination in Lux on horizontal surface as a function as its altitude in radians.


    
    Calculate the Sun illumination in Lux on horizontal surface  
    Package: celestial.SolarSys  
    Description: Calculate the Sun illumination in Lux on horizontal  
    surface as a function as its altitude in radians.  
    Input  : - vector of Altitude in radians.  
    Output : - Illumination in Lux on horiz. surface.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Illum=celestial.SolarSys.sunlight(-0.1)  
    Reliable: 2  
      
