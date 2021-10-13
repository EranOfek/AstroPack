# Package: celestial.coo


### celestial.coo.aberration

Apply aberration of light to source position Package: celestial.coo Description: Calculate the position of a star corrected for aberration of light. Rigoursly, these is applied after accounting for the light


    
    Apply aberration of light to source position  
    Package: celestial.coo  
    Description: Calculate the position of a star corrected for aberration  
    of light.  
    Rigoursly, these is applied after accounting for the light  
    travel time effect and light deflection and before the  
    precession and nutation are being applied.  
    Input  : - Matrix containing three columns (cosine directions).  
    Each row represent the object position in a given epoch.  
    If two columns are given the assume J2000.0 [RA, Dec] in radians.  
    - The observer velocity in the barycentric reference frame,  
    in the same format as the previous parameter (Equatorial J2000.0).  
    If only one column is specified, then this is assumed  
    to be the JD and Geocentric observer. In this case the progarm  
    will calculate the Geocentric velocity vectors for each epoch  
    using the Ron & Vondrak (1986) expressions.  
    - Input/output vector units - options are:  
    'cgs'  - for [cm].  
    'SI'   - for [m].  
    'agd'  - for [AU] - default.  
    Output : - The position of the object corrected for abberation of light  
    and given in the observer reference frame in the same  
    format as the input parameters.  
    Reference: Explanatory supplement to the Astronomical Almanac, p. 150.  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Oct 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: U2=celestial.coo.aberration([celestial.coo.convertdms([2 44 12.9747],'H','r'), celestial.coo.convertdms([1 49 13 39.896],'D','R')],celestial.time.julday([13.19 11 2028]));  
    Reliable: 2  
      
      
### celestial.coo.add_offset

Offset a position by angular distance and position angle Package: celestial.coo Description: Add an offset specified by angular distance and position angle to celestial coordinates.


    
    Offset a position by angular distance and position angle  
    Package: celestial.coo  
    Description: Add an offset specified by angular distance and position  
    angle to celestial coordinates.  
    Input  : - RA [radians].  
    - Dec [radians].  
    - Offset (angular distance) [radians].  
    - Position angle [radians].  
    Output : - RA [radians].  
    - Dec [radians].  
    See also: sphere_offset.m; sphere_dist.m  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                    Mar 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [OutRA,OutDec]=celestial.coo.add_offset(1,1,0.01,1)  
    Reliable: 1  
      
      
### celestial.coo.airmass

Airmass from time and object and observer position Package: celestial.coo Description: Given the JD, object celestial coordinates, and observer Geodetic coordinates, calculating the airmass of the object.


    
    Airmass from time and object and observer position  
    Package: celestial.coo  
    Description: Given the JD, object celestial coordinates, and observer  
    Geodetic coordinates, calculating the airmass of the  
    object.  
    Input  : - Matrix in which the first column is JD (the other columns  
    are ignored).  
    - Object RA in radians or sexagesimal coordinates  
    (see convertdms.m for possible options)  
    - Object Dec in radians or sexagesimal coordinates  
    (see convertdms.m for possible options)  
    - Observer coordinates: [East Long, North Lat] in radians.  
    Output : - Vector of airmass.  
    - [AZ, Alt] of object in radians.  
    See Also : hardie.m; hardie_inv.m; horiz_coo.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Apr 2011  
    URL : http://wiezmann.ac.il/home/eofek/matlab/  
    Example: [AirMass,AzAlt]=celestial.coo.airmass(2451545+[0;1],1,1,[1 1]);  
    Reliable: 2  
      
      
### celestial.coo.alt2ha

Convert altitude and declnation to hour angle Package: celestial.coo Description: Given an object altitude and declination and the observer latitude, return the corresponding Hour Angle.


    
    Convert altitude and declnation to hour angle  
    Package: celestial.coo  
    Description: Given an object altitude and declination and the observer  
    latitude, return the corresponding Hour Angle.  
    Input  : - Altitude [radians].  
    - Declination [radians].  
    - Geocentric latitude [radians].  
    - Input/output units 'rad' or 'deg'. Default is 'rad'.  
    Outpuoutputt is always in radians.  
    Output : - Hour Angle [radians].  
    Note that there are two solutions at +/-HA, but only  
    the positive HA is returned.  
    Tested : Matlab R2013a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: HA=celestial.coo.alt2ha(1,1,1)  
    Reliable: 2  
      
      
      
### celestial.coo.altha2dec

Convert altitude and hour angle to declination Package: celestial.coo Description: Given Altitude and Hour Angle of an object and the observer latitude, calculate the object Declination. There may be up to two solutions for the Declination.


    
    Convert altitude and hour angle to declination  
    Package: celestial.coo  
    Description: Given Altitude and Hour Angle of an object and the observer  
    latitude, calculate the object Declination.  
    There may be up to two solutions for the Declination.  
    Input  : - Altitude [radians].  
    - Hour Angle [radians].  
    - Observer geocentric latitude [radians].  
    - Input/output units 'rad' or 'deg'. Default is 'rad'.  
    Output : - First Declination solution [radians].  
    NaN if no solution.  
    - Second Declination solution [radians].  
    NaN if no solution.  
    Tested : Matlab R2013a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Alt=40;  Phi=32; HA= (-180:1:180)';  
    [Dec1,Dec2]=celestial.coo.altha2dec(Alt,HA,Phi,'deg')  
    plot(HA,Dec1);hold on; plot(HA,Dec2,'r-')  
    Reliable: 2  
      
      
### celestial.coo.angle_in2pi

Convert an angle to the 0 to 2*pi range Package: celestial.coo Description: Convert an angle to the range 0 to 2.*pi.


    
    Convert an angle to the 0 to 2*pi range  
    Package: celestial.coo  
    Description: Convert an angle to the range 0 to 2.*pi.  
    Input  : - Matrix of angles.  
    - Period of angles. Default is 2.*pi.  
    Output : - Angle in the allowed range.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    May 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.coo.angle_in2pi([-370;-350;-10;10;350;370;730],360)  
    Reliable: 2  
      
      
### celestial.coo.area_sphere_polygon

Area of a polygon on a sphere Package: celestial.coo Description: Calculate the area of a polygon on a sphere, where the polygon sides are assumed to be great circles. If the polygon is not closed (i.e., the first point is identical to the last


    
    Area of a polygon on a sphere  
    Package: celestial.coo  
    Description: Calculate the area of a polygon on a sphere, where the  
    polygon sides are assumed to be great circles. If the polygon  
    is not closed (i.e., the first point is identical to the last  
    point) then, the program close the polygon.  
    Input  : - Column vector of longitudes for polygon.  
    Units are [rad] or [H M S] or sexagesimal.  
    - Column vector of latitude for polygon.  
    Units are [rad] or [Sign D M S] or sexagesimal.  
    Output : - Area of spherical polygon which its sides are great circles  
    [sr].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Mar 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### celestial.coo.azalt2hadec

Convert Az/Alt to HA/Dec Package: +celestial.coo


    
    Convert Az/Alt to HA/Dec  
    Package: +celestial.coo  
    Input  : - Az [rad]  
    - Alt [rad]  
    - Lat [rad]  
    - CooUnits {'rad' | 'deg'}. Default is 'rad'.  
    Output : - HA [same as input]  
    - Dec [same as input]  
    By : Eran O. Ofek                Aug 2020  
    Example: Az=1; Alt=1; Lat=[30;31]; [HA,Dec]=celestial.coo.azalt2hadec(Az,Alt,Lat,'deg')  
      
### celestial.coo.boundingCircle

fit the smallest-radius bounding circle to set of X, Y points


    
    fit the smallest-radius bounding circle to set of X, Y points  
    Input  : - An array containing X coordinates [radians].  
    - An array containing Y coordinates (corresponding to the X  
    coordunates) [radians].  
    Output : - A two element vector of best circle position [X,Y] in radians.  
    - The minimum radius around the best center than encompass all  
    the data points [radians].  
    Author : Eran Ofek (Apr 2021)  
    Example: X = rand(10,1)+1; Y = rand(10,1);  
    [BestCoo, BestRadius] = celestial.coo.boundingCircle(X,Y);  
    axesm('aitoff', 'Frame', 'on', 'Grid', 'on');  
    plotm(Y.*RAD,X.*RAD,'+'); hold on;  
    [Lat,Lon]=reckon(BestCoo(2).*RAD, BestCoo(1).*RAD, BestRadius.*RAD, (0:1:360));  
    plotm(Lat,Lon,'k-')  
      
### celestial.coo.calc_pm

calc_pm function Description: Calculate the proper motion of a star from a set of measurments. OBSOLETE:


    
      
    calc_pm function  
    Description: Calculate the proper motion of a star from a set of  
    measurments.  
    OBSOLETE:  
    Input  : - List of positions [Time, RA, Dec] per line  
    (coordinates in radians).  
    - Error matrix: [RA_Err, Dec_Err] per line (in radians).  
    - Time units:  
    'j'   - Julian years  (default).  
    'jd'  - Julian days  
    Output : - PM_RA (RA*cos(Dec)) [Value, left_err, right_err] ["/year]  
    - PM_Dec              [Value, left_err, right_err] ["/year]  
    - PM_Tot              [Value, left_err, right_err] ["/year]  
    - PM_PA               [Value, left_err, right_err] [deg]  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                      January 2001  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### celestial.coo.cel_annulus_area

Area within a celestial annulus Package: celestial.coo Description: Calculate the area within a celestial annulus defined by an inner radius and outer radius of two concentric small circles.


    
    Area within a celestial annulus  
    Package: celestial.coo  
    Description: Calculate the area within a celestial annulus defined by an  
    inner radius and outer radius of two concentric small circles.  
    Input  : - List of Inner radii [radians].  
    - List of Outer radii [radians].  
    - Output Units:  
    'sr'   - sterradians (default).  
    'deg'  - deg^2  
    'am'   - arcmin^2  
    'as'   - arcsec^2  
    Output : - List of areas for each pole and radii in output units.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jan 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Area=celestial.coo.cel_annulus_area(0,pi,'deg');  area of the celestial sphere  
    Reliable: 2  
      
### celestial.coo.celestial_circ

Grid of coordinates on a small spherical circle Package: celestial.coo Description: Calculate grid of longitude and latitude of a small circle on the celestial sphere.


    
    Grid of coordinates on a small spherical circle  
    Package: celestial.coo  
    Description: Calculate grid of longitude and latitude of a small circle  
    on the celestial sphere.  
    Input  : - Longitude of small circle center [radians].  
    - Latitude of small circle center [radians].  
    - Radius of small circle [radians].  
    - Number of points to calculate, default is 100.  
    Output : - Longitude of points on the small circle.  
    - Latitude of points on the small circle.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Dec 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Lon,Lat]=celestial.coo.celestial_circ(1,1,1,500);  
    Reliable: 2  
      
### celestial.coo.center2corners

Return field corners given its center and size Package: celestial.coo Description: Given a field/s center and size calculate the field/s corners.


    
    Return field corners given its center and size  
    Package: celestial.coo  
    Description: Given a field/s center and size calculate the field/s  
    corners.  
    Input  : - Field center RA [radians].  
    This should be a column vector.  
    - Field center Dec [radians].  
    This should be a column vector.  
    - Half the field of view in RA [radians].  
    - Half the field of view in Dec [radians].  
    Output : - Corners RA [radians] - 4 corners pear line.  
    - Corners Dec [radians] - 4 corners per line.  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                    Mar 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [OutRA,OutDec]=celestial.coo.center2corners(1,1,0.01,0.01)  
    Reliable: 2  
      
      
### celestial.coo.coco

Convert between different coordinates (OBSOLETE: use convert_coo) Package: celestial.coo Description: General coordinate convertor. Convert/precess coordinate from/to Equatorial/galactic/Ecliptic coordinates.


    
    Convert between different coordinates (OBSOLETE: use convert_coo)  
    Package: celestial.coo  
    Description: General coordinate convertor. Convert/precess  
    coordinate from/to Equatorial/galactic/Ecliptic  
    coordinates.  
    Input  : - Matrix of input coordinates.  
    First column for Long/RA and second column for lat/Dec.  
    - Type of input coordinates.  
    'j####.#' - equatorial, mean of date  
    (default 'j2000.0'). [Julian].  
    'J####.#' - equatorial, true of date  
    (default 'j2000.0'). [Julian].  
    'g' - galactic.  
    'S' - Super galactic.  
    'c' - CMB dipole (see rotm_coo.m for details)  
    'e' - Ecliptic with J2000.0 equinox.  
    - Type of outpt coordinates.  
    'j####.#' - equatorial, mean of date  
    (default 'j2000.0'). [Julian].  
    'J####.#' - equatorial, true of date  
    (default 'j2000.0'). [Julian].  
    'g' - galactic. (default)  
    'S' - Super galactic.  
    'c' - CMB dipole (see rotm_coo.m for details)  
    'e' - Ecliptic with J2000.0 equinox.  
    - Units for input coordinates.  
    'r' - radians. (default)  
    'd' - degrees.  
    'h' - hours/deg.  
    - Units for outpu coordinates.  
    'r' - radians. (default)  
    'd' - degrees.  
    'h' - hours/deg.  
    Output : - Matrix of output coordinates.  
    - Total rotation matrix.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Feb 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: C=celestial.coo.coco(rand(10,2),'j2000.0','e')  
    Reliable: 1  
      
### celestial.coo.convert2equatorial

Convert coordinates/name to apparent equatorial coordinates. Package: celestial Description: Given a coordinates in some coordinate system or equinox, or an object name, convert it to euatorial coordinates that includes the atmospheric refraction correction and optional


    
    Convert coordinates/name to apparent equatorial coordinates.  
    Package: celestial  
    Description: Given a coordinates in some coordinate system or equinox,  
    or an object name, convert it to euatorial coordinates that  
    includes the atmospheric refraction correction and optional  
    telescope distortion model (T-point model).  
    Input  : - Longitude in some coordinate system, or object name.  
    Longitude can be either sexagesimal coordinates or numeric  
    calue in degress (or radians if InputUnits='rad').  
    Object name is converted to coordinates using either SIMBAD,  
    NED or JPL horizons.  
    - Like the first input argument, but for the latitude.  
    If empty, or not provided, than the first argument is assumed  
    to be an object name.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    However, if only one parameter is provided, it is treated as  
    the value of 'InCooType'.  
    'InCooType'  - Input coordinates frame:  
    'a' - Az. Alt.  
    'g' - Galactic.  
    'e' - Ecliptic  
    'ha' - Hour Angle/Declination at true equinox  
    of date.  
    'jha' - Hour Angle/Declination at J2000.0.  
    - A string start with J (e.g., 'J2000.0').  
    Equatorial coordinates with mean equinox of  
    date, where the year is in Julian years.  
    -  A string start with t (e.g., 't2020.5').  
    Equatorial coordinates with true equinox of  
    date.  
    Default is 'J2000.0'  
    'OutCooType' - 'tdate' | 'J2000.0'. Default is 'J2000.0'.  
    'NameServer' - ['simbad'] | 'ned' | 'jpl'.  
    'JD'         - Julian day. This is used for horizontal  
    coordinates and H.A.  
    Default is now (i.e., celestial.time.julday)  
    'ObsCoo'     - Observer Geodetic position.  
    [East Long (deg), Lat (deg), Height (meters)]  
    'HorizonsObsCode' - In case solar system ephemerides is  
    requested, then this is the Horizons observatory  
    code. Default is '500' (geocentric observer).  
    'DistFun'    - Distortion function handle.  
    The function is of the form:  
    [DistHA,DistDec]=@Fun(HA,Dec), where all the  
    input and output are in degrees.  
    Default is empty. If not given return [0,0].  
    'InputUnits' - Default is 'deg'.  
    'OutputUnits'- Default is 'deg'  
    'ApplyRefraction' - Default is true.  
    'Temp'       - Default is 15 C.  
    'Wave'       - Default is 5500 Ang.  
    'PressureHg' - Default is 760 mm Hg.  
    Output : - Apparent R.A. (or HA, if InCooType is 'ha', or 'jha')  
    - Apparent Dec.  
    - A structure containing the intermidiate values.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [DistRA,DistDec,Aux]=celestial.coo.convert2equatorial(1,1)  
    celestial.coo.convert2equatorial('12:00:00','+20:00:00');  
    celestial.coo.convert2equatorial('M31');  
    celestial.coo.convert2equatorial('9804;',[],'NameServer','jpl')  
    celestial.coo.convert2equatorial('12:00:00','+20:00:00','InCooType','J2000.0');  
    celestial.coo.convert2equatorial('12:00:00','+20:00:00','J2000.0');  
    Reliable: 2  
      
      
### celestial.coo.convert_coo

Convert between different coordinates Package: celestial.coo Description: General coordinate convertor. Convert/precess coordinate from/to Equatorial/galactic/Ecliptic coordinates.


    
    Convert between different coordinates  
    Package: celestial.coo  
    Description: General coordinate convertor. Convert/precess  
    coordinate from/to Equatorial/galactic/Ecliptic  
    coordinates.  
    Input  : - Matrix of longitudes [rad].  
    - Matrix of latitudes [rad].  
    - Type of input coordinates.  
    'J####.#' - equatorial, mean equinox of date  
    (default 'j2000.0'). [Julian years].  
    't####.#' - equatorial, true equinox of date  
    'tdate' - Equinox of provided JD.  
    'g','gal','galactic' - galactic.  
    'S' - Super galactic.  
    'c' - CMB dipole (see rotm_coo.m for details)  
    'e','ec','ecl','ecliptic' - Ecliptic with J2000.0 equinox.  
    'h','hor','azalt' - horizontal coordinates  
    - Type of outpt coordinates.  
    Like input. Default is 'g'.  
    - JD (for horizontal coordinates). Default is now.  
    - Observatory [Long,Lat] [rad] (for horizontal coordinates).  
    Output : - Matrix of output longitude.  
    - Matrix of output latitude.  
    - Total rotation matrix.  
    Tested : Matlab 2019b                    Feb 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Lon,Lat,RM]=celestial.coo.convert_coo(rand(10,4),rand(10,4),'J2000.0','e')  
    [Lon,Lat]=celestial.coo.convert_coo(1.1,-0.2,'J2000.0','J2015.4')  
    Reliable: 1  
      
      
### celestial.coo.convertdms

Convert between various representations of coordinates and time Package: celestial.coo Description: Convert between various representations of coordinates and time as sexagesimal coordinates, degrees and radians.


    
    Convert between various representations of coordinates and time  
    Package: celestial.coo  
    Description: Convert between various representations of coordinates and  
    time as sexagesimal coordinates, degrees and radians.  
    Input  : - Input data in one of the following formats:  
    degrees, radians, [H M S.S], [Sign D M S.S],  
    [H M.M], [Sign D M.M],  
    'HH:MM:SS.S', '+DD:MM:SS.S'.  
    - Input type:  
    'r'  - radians.  
    'd'  - degrees.  
    'D'  - [Sign D M S.S] format.  
    'DM' - [Sign D M.M] format.  
    'h'  - hours.  
    'H'  - [H M S.S] format.  
    'HM' - [H M.M] format.  
    'f'  - fraction in range [0,1].  
    'SH' - String of sexagesimal hours 'HH:MM:SS.SSS'  
    In this case the input can be strings or cell array  
    of strings.  
    'SHb'- like 'SH', but with blanks instead of ":".  
    'SHh'- String of sexagesimal hours 'HHhMMmSS.SSSs'  
    In this case the input can be strings or cell array  
    of strings.  
    'SD' - String of sexagesimal degrees '+DD:MM:SS.SS'  
    In this case the input can be strings or cell array  
    of strings.  
    'SDb'- like 'SH', but with blanks instead of ":".  
    'gH' - Automatic identification of hours,  
    [rad], [H M S], [sexagesimal string].  
    'gD' - Automatic identification of degrees,  
    [rad], [Sign D M S], [sexagesimal string].  
    - Output type:  
    'r'  - radians, in range [0,2*pi] (default).  
    'R'  - radians, in range [-pi,pi].  
    'd'  - degrees.  
    'h'  - hours.  
    'f'  - fraction in range [0,1].  
    'D'  - [Sign D M S.S] format.  
    'H'  - [H M S.S] format.  
    'DM' - [Sign D M.M] format.  
    'HM' - [H M.M] format.  
    'SH' - String of sexagesimal hours 'HH:MM:SS.SSS'  
    'SD' - String of sexagesimal degrees '+DD:MM:SS.SS'  
    'SHn'- String of sexagesimal hours 'HHMMSS.SSS'  
    'SHh'- String of sexagesimal hours 'HHhMMmSS.SSSs'  
    'SDn'- String of sexagesimal degrees '+DDMMSS.SS'  
    'SHb'- like 'SH', but with blanks instead of ":".  
    'SDb'- like 'SH', but with blanks instead of ":".  
    Output : - Requested output.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.coo.convertdms([1;2;3],'gH','d');  
    celestial.coo.convertdms([-1;0.1;1],'gD','SD');  
    Reliable: 2  
      
### celestial.coo.convertdms1

convertdms1 function                                               ephem Description:


    
      
    convertdms1 function                                               ephem  
    Description:  
    Input  : -  
    Output : -  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Feb 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
### celestial.coo.coo2box

Calculate box vertices around coordinates (OBSOLETE: use coo2box) Package: celestial Description: Given a list of RA/Dec coordinates, and box half size, calculate the approximnate positions of the box vertices around the coordinates. Do not fix RA/Dec jumps.


    
    Calculate box vertices around coordinates (OBSOLETE: use coo2box)  
    Package: celestial  
    Description: Given a list of RA/Dec coordinates, and box half size,  
    calculate the approximnate positions of the box vertices  
    around the coordinates. Do not fix RA/Dec jumps.  
    Input  : - Set of RA [radians].  
    - Set of Dec [radians].  
    - Box half size [RA, Dec] in radians.  
    - Output units {'rad','deg'}. Default is 'rad'.  
    Output : - RA1 [radians]  
    - RA2 [radians]  
    - Dec1 [radians]  
    - Dec2 [radians]  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Dec 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.coo.coo2box(1,1,0.01)  
    Reliable: 2  
      
      
### celestial.coo.coo2cosined

Coordinates to cosine directions Package: celestial.coo Description: Convert coordinates to cosine directions in the same reference frame. See also: cosined.m, cosined2coo.m


    
    Coordinates to cosine directions  
    Package: celestial.coo  
    Description: Convert coordinates to cosine directions in the same  
    reference frame. See also: cosined.m, cosined2coo.m  
    Input  : - Matrix of longitudes [radians].  
    - Matrix of latitudes [radians].  
    Output : - Matrix of first cosine directions.  
    - Matrix of second cosine directions.  
    - Matrix of third cosine directions.  
    Tested : Matlab 7.10  
    By :  Eran O. Ofek                   Oct 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [CD1,CD2,CD3]=celestial.coo.coo2cosined(1,1);  
    Reliable: 1  
      
      
### celestial.coo.coo_resolver

Resolve coordinates or target name into RA/Dec Package: celestial Description: Given coordinates (Lon/Lat) in any coordinate system or format, or a target name convert the coordinates into RA/Dec in some specific equinox and in deg/radians unuts.


    
    Resolve coordinates or target name into RA/Dec  
    Package: celestial  
    Description: Given coordinates (Lon/Lat) in any coordinate system or  
    format, or a target name convert the coordinates into  
    RA/Dec in some specific equinox and in deg/radians unuts.  
    Input  : - Longitude in any format (e.g., deg, sexagesimal,...)  
    or system (e.g., equatorial, ecliptic,...).  
    If numeric than assume input is in degrees (unless 'InUnits'  
    is set to 'rad', for radians). If string and Latitute is not  
    empty, than assume input is in sexagesimal format  
    (e.g., 'HH:MM:SS.FFF' or 'HH MM SS.FFF'). If string and  
    Latitude is empty, or not given, than assume this is an object  
    name and try to use name resolver (e.g., NED/SIMBAD) to  
    convert the name to coordinates.  
    Available coordinate systems are listed in the 'InSys'  
    parameter. Default is J2000 equatorial.  
    - Latitude in any format or system.  
    If empty or not provided than assume Longitude is an opbject  
    name.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'InSys'    - Input coordinate system. Options are  
    'j2000','el','gal' (see celestial.coo.coco for  
    more options).  
    Default is 'j2000'.  
    'InUnits'  - Input units in case of a numeric input  
    coordinates {'deg'|'rad'}. Default is 'deg'.  
    'NameServer' - Name server function:  
    @VO.name.server_ned [default].  
    @VO.name.server_simbad  
    'OutUnits' - Output units {'deg'|'rad'}. Default is 'rad'.  
    Output : - J2000.0 R.A.  
    - J2000.0 Dec.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  RA/Dec [deg] to RA/Dec in radians  
    [OutRA,OutDec]=celestial.coo.coo_resolver(1,1);  
    RA/Dec [rad] to RA/Dec in radians  
    [OutRA,OutDec]=celestial.coo.coo_resolver(1,1,'InUnits',rad');  
    use NED name server (requires internet)  
    [OutRA,OutDec]=celestial.coo.coo_resolver('Deneb','NameServer',@VO.name.server_simbad)  
    [OutRA,OutDec]=celestial.coo.coo_resolver('M81',[])  
    [OutRA,OutDec]=celestial.coo.coo_resolver('M81','OutUnits','deg')  
    input in sexagesimal  
    [OutRA,OutDec]=celestial.coo.coo_resolver('15:00:11.1','-13:01:11.1')  
    input in galactic coordinates  
    [OutRA,OutDec]=celestial.coo.coo_resolver('15:00:11.1','-13:01:11.1','InSys','gal')  
    Reliable: 2  
      
      
### celestial.coo.cosined

Convert between coordinates and cosine directions Package: celestial.coo Description: Cosine direction transformation. Convert longitude and latitude to cosine direction and visa versa. See also: coo2cosined.m, cosined2coo.m


    
    Convert between coordinates and cosine directions  
    Package: celestial.coo  
    Description: Cosine direction transformation. Convert longitude  
    and latitude to cosine direction and visa versa.  
    See also: coo2cosined.m, cosined2coo.m  
    Input  : - Column matrix, with 2 or 3 colums.  
    If 2 colums are given then [longitude latitude] in  
    radians, and the output is the cosine direction.  
    If three columns are given, then assume each line  
    contains cosine direction [X Y Z], and the output will  
    be the longitude and latitude in radians.  
    Output : - Cosine direction or longitude and latitude [radian].  
    Each raw in the output corresponds to each raw in  
    the input.  
    Tested : Matlab 7.0  
    See also: cosined2coo, coo2cosined  
    By :  Eran O. Ofek                   Jul 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.coo.cosined(rand(5,3));     Lon/lat from cosine-dir.  
    celestial.coo.cosined([0 0;1 1]);     cosine-dir from long/lat  
    Reliable: 1  
      
### celestial.coo.cosined2coo

Cosine direction to coordinates Package: celestial.coo Description: Convert cosine directions to coordinates in the same reference frame. See also: cosined.m, coo2cosined.m


    
    Cosine direction to coordinates  
    Package: celestial.coo  
    Description: Convert cosine directions to coordinates in the same  
    reference frame. See also: cosined.m, coo2cosined.m  
    Input  : - Matrix of first cosine directions.  
    - Matrix of second cosine directions.  
    - Matrix of third cosine directions.  
    Output : - Matrix of longitudes [radians].  
    - Matrix of latitudes [radians].  
    Tested : Matlab 7.10  
    By :  Eran O. Ofek                   Oct 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [RA,Dec]=celestial.coo.cosined2coo(0.1,0,1)  
    Reliable: 1  
      
      
### celestial.coo.dome_az

dome_az function                                                   ephem Description:


    
      
    dome_az function                                                   ephem  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    - Additional parameters  
    Any additional key,val, that are recognized by one of the  
    following programs:  
    Output : -  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    May 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [DomeAz,Az,Alt]=dome_az(0,0,32./RAD,5,1)  
    Reliable:  
      
      
### celestial.coo.ecliptic2helioecliptic

Ecliptic longitude to Helio-ecliptic longitude Package: celestial.coo Description: Transform ecliptic longitude to Helio-ecliptic longitude.


    
    Ecliptic longitude to Helio-ecliptic longitude  
    Package: celestial.coo  
    Description: Transform ecliptic longitude to Helio-ecliptic longitude.  
    Input  : - Ecliptic longitude [radians]. See convert_dms.m for  
    additional options.  
    - Date [Day Mounth Year FracDay], or JD column vector.  
    See julday.m for options.  
    - Solar longitude algorithm:  
    'low' - low accuracy (0.01 deg). Default.  
    Output : - Helio-ecliptic longitude [radians].  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Nov 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: HelioEcLong=celestial.coo.ecliptic2helioecliptic(1,[21 3 2000])  
    Reliable: 2  
      
      
### celestial.coo.fit_proper_motion

SHORT DESCRIPTION HERE Package: celestial Description:


    
    SHORT DESCRIPTION HERE  
    Package: celestial  
    Description:  
    Input  : - Time (days)  
    - RA [rad]  
    - Dec [rad];  
    - ErrRA ["]  
    - ErrDec ["]  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Res]=celestial.coo.fit_proper_motion(T,RA,Dec,ErrRA,ErrDec,Epoch)  
    Reliable:  
      
      
### celestial.coo.fit_scircle

Example: celestial.coo.fit_scircle


    
      
    Example: celestial.coo.fit_scircle  
      
      
### celestial.coo.geocentric2lsr

Geocentric or heliocentric velocity to velocity relative to the LSR Package: celestial.coo Description: Approximate conversion of geocentric or heliocentric velocity to velocity relative to the local standard of rest (LSR).


    
    Geocentric or heliocentric velocity to velocity relative to the LSR  
    Package: celestial.coo  
    Description: Approximate conversion of geocentric or heliocentric  
    velocity to velocity relative to the local standard of  
    rest (LSR).  
    Input  : - Column vector of JD.  
    - Column vector of measured speed  
    (+ for moving in observer direction).  
    - Column vector of error in measured speed.  
    - Two column matrix of Galactic coordinates [Long, Lat]  
    in radians.  
    - Conversion type:  
    'G' - Geocentric to LSR/Galactocentric.  
    'H' - Heliocentric to LSR/Galactocentric.  
    - The velocity of the Sun relative to the LSR  
    [u, v, w], in km/sec.  
    Default is [-9, 12, 7].  
    - Errors in the velocity of the Sun relative to the LSR.  
    Default is [0.5, 0.5, 0.5].  
    Output : - Speed relative to the LSR.  
    - Error in speed relative to the LSR.  
    - Galactocentric speed.  
    Reference: Allen's astrophysicl quantities  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Sep 2003  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [V,Ve,GV]=celestial.coo.geocentric2lsr(2451545,20,1,[1 1],'G')  
    Reliable: 2  
      
### celestial.coo.get_skytile_coo

- get_skytile_coo function                                    Catalogue Description: Assuming some sky tileing (see tile_the_sky.m) and optional sub tileing for each tile, search for all the tiles which their centers found within some distance


    
    -  
    get_skytile_coo function                                    Catalogue  
    Description: Assuming some sky tileing (see tile_the_sky.m) and  
    optional sub tileing for each tile, search for all the  
    tiles which their centers found within some distance  
    from a given celestial coordinates.  
    Input  : - Coordinate to search [RA, Dec], in radians.  
    - Search radius in radians.  
    - Number of tiles in RA direction.  
    - Number of tiles in Dec direction.  
    - Number of subtiles in RA and Dec, within each tile.  
    Output : - Tiles centers [RA, Dec], in radians.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek        September 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See also: tile_the_sky.m  
    Example: List=get_skytile_coo([1 1.4],0.2./RAD,360,180,[5 5]);  
    -  
      
### celestial.coo.ha2alt

Hour angle to altitude and airmass Package: celestial.coo Description: Given Hour Angle as measured from the meridian, the source declination and the observer Geodetic latitude, calculate the source altitude above the horizon and its airmass.


    
    Hour angle to altitude and airmass  
    Package: celestial.coo  
    Description: Given Hour Angle as measured from the meridian, the source  
    declination and the observer Geodetic latitude, calculate  
    the source altitude above the horizon and its airmass.  
    Input  : - Hour Angle [radians].  
    - Declination [radians].  
    - Latitude [radians].  
    - Input/output units. 'rad' | 'deg'. Default is 'rad'.  
    Output : - Altitude [radians].  
    - Airmass.  
    See also: horiz_coo.m, ha2az.m  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Aug 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [A,AM]=celestial.coo.ha2alt(1,1,0.5)  
    Reliable: 1  
      
      
### celestial.coo.ha2az

Convert hour angle and declination to azimuth, altitude and airmass Package: celestial.coo Description: Given Hour Angle as measured from the meridian, the source declination and the observer Geodetic latitude, calculate the horizonal source azimuth


    
    Convert hour angle and declination to azimuth, altitude and airmass  
    Package: celestial.coo  
    Description: Given Hour Angle as measured from the meridian, the source  
    declination and the observer Geodetic latitude, calculate  
    the horizonal source azimuth  
    Input  : - Hour Angle [radians].  
    - Declination [radians].  
    - Latitude [radians].  
    - Input/output units. 'rad' | 'deg'. Default is 'rad'.  
    Output : - Azimuth [radians].  
    - Altitude [radians].  
    - Airmass.  
    See also: horiz_coo.m, ha2alt.m  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Aug 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Az,Alt,AM]=celestial.coo.ha2az(1,1,1)  
    Reliable: 1  
      
      
      
### celestial.coo.hadec2azalt

Convert HA/Dec to Az/Alt Package: +celestial.coo


    
    Convert HA/Dec to Az/Alt  
    Package: +celestial.coo  
    Input  : - HA [rad]  
    - Dec [rad]  
    - Lat [rad]  
    - Input/Output coordinates units {'rad' | 'deg'}. Default is 'rad'.  
    Output : - Az [same as input]  
    - Alt [same as input]  
    By : Eran O. Ofek                Aug 2020  
    Example: [Az,Alt]=celestial.coo.hadec2azalt(HA,Dec,Lat)  
      
### celestial.coo.hardie

The Hardie airmass formula Package: celestial.coo Description: Calculate airmass using the Hardie formula.


    
    The Hardie airmass formula  
    Package: celestial.coo  
    Description: Calculate airmass using the Hardie formula.  
    Input  : - Matrix of zenith distances [radians].  
    - Algorith: {'hardie','csc'}. Default is 'hardie'.  
    Output : - Air mass.  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Jan 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: airmass.m; hardie_inv.m  
    Example: AM=celestial.coo.hardie([1;1.1]);  
    Reliable: 1  
      
      
### celestial.coo.hardie_inv

Convert hardie airmass to altitude Package: celestial.coo Description: Inverse Hardie airmass function. Convert airmass to zenith distance.


    
    Convert hardie airmass to altitude  
    Package: celestial.coo  
    Description: Inverse Hardie airmass function. Convert airmass to zenith  
    distance.  
    Input  : - Air Mass  
    Output : - Zenith distance [radians].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Sep 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: ZD=celestial.coo.hardie_inv(2);  
    Reliable: 2  
      
      
### celestial.coo.horiz_coo

Celestial equatorial coordinates to horizontal coordinates Package: celestial.coo Description: Convert Right Ascension and Declination to horizontal coordinates or visa versa.


    
    Celestial equatorial coordinates to horizontal coordinates  
    Package: celestial.coo  
    Description: Convert Right Ascension and Declination to horizontal  
    coordinates or visa versa.  
    Input  : - Two columns matrix of coordinates,  
    (Long & Lat) | (Az & Alt) in radians  
    - vector of JDs + UT fraction of day,  
    if scalar value is given, then it duplicated  
    for all coordinates.  
    - Geodetic Coordinates, east long & north lat in radians  
    if scalar value is given then it is duplicate for  
    all coordinates.  
    - Direction,  
    'h' - from equatorial to horizontal (default).  
    'e' - from horizontal to equatorial.  
    Output : - two column matrix of output coordinates.  
    * If two output arguments are requested than return the two  
    coordinates seperatly.  
    Tested : matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: OutCoo=celestial.coo.horiz_coo([1 1],celestial.time.julday([1 1 2015]),[1 1])  
    Reliable: 1  
      
### celestial.coo.in_box

Check if celestial coordinates are in a box (approximate). Package: celestial Description: Check if celestial coordinates are in a box defined by four corners and its sides are great circles. See also: celestial.htm.in_polysphere


    
    Check if celestial coordinates are in a box (approximate).  
    Package: celestial  
    Description: Check if celestial coordinates are in a box defined by four  
    corners and its sides are great circles.  
    See also: celestial.htm.in_polysphere  
    Input  : - Vector of longitude to check [rad].  
    - Vector of latitude to check [rad].  
    - [Longitude,Latitiude] of Box center [rad].  
    - [HalfSizeLon, HalfSizeLat] of box [rad].  
    Output : - A vector of logical flags indicating if coordinates is in box.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Flag=celestial.coo.in_box(rand(100,1),rand(100,1),[0 0],[0.5 0.5])  
    Reliable:  
      
      
      
      
### celestial.coo.inside_celestial_box

Check if coorduinates are within box Package: celestial.coo Description: Given a list of celestial coordinates, and a box center, width and height, where the box sides are parallel to the coorinate system (i.e., small circles),


    
    Check if coorduinates are within box  
    Package: celestial.coo  
    Description: Given a list of celestial coordinates, and a box center,  
    width and height, where the box sides are parallel to the  
    coorinate system (i.e., small circles),  
    check if the coordinates are within the box.  
    Input  : - Vector of celestial longitudes [radians] to test.  
    - Vector of celestial latitudes [radians] to test.  
    - Vector of longitudes of center of boxes [radians].  
    - Vector of latitudes of center of boxes [radians].  
    - Vector of full widths of boxes [radians].  
    - Vector of full heights of boxes [radians].  
    Output : - Flag indicating if coordinate is inside corresponding box  
    (1) or not (0).  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                    Mar 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: I=celestial.coo.inside_celestial_box(1,1,1,1,0.1,0.1)  
    Reliable: 2  
      
      
### celestial.coo.interp_coo

Interpolate celestial coordinates as a function of time Package: celestial.coo Description: Interpolate on celestial ccordinates as a function of time. Use the built in matlab interpolation functions.


    
    Interpolate celestial coordinates as a function of time  
    Package: celestial.coo  
    Description: Interpolate on celestial ccordinates as a function of time.  
    Use the built in matlab interpolation functions.  
    Input  : - Vector of Times (e.g., JD).  
    - Vector of longitudes [radians].  
    - Vector of latitudes [radians].  
    - Vector of new times in which to interpolate position.  
    - Algorithm (see interp1.m for details), default is 'pchip'.  
    Output : - Vector of longitudes interpolated to the to the vector  
    of new times.  
    - Vector of latitudes interpolated to the to the vector  
    of new times.  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                    Mar 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Time=(1:1:11)';  
    RA = (0.1:0.01:0.2)'; Dec = (0.1:0.01:0.2).';  
    NewTime = [2.3;3.4];  
    [NewRA,NewDec]=celestial.coo.interp_coo(Time,RA,Dec,NewTime);  
    Reliable: 2  
      
      
### celestial.coo.is_coordinate_ok

Check that coordinates satisfy some observability conditions Package: celestial.coo Description: Check that J2000 equatorial coordinates satisfy some observability conditions including Az, Alt, HA.


    
    Check that coordinates satisfy some observability conditions  
    Package: celestial.coo  
    Description: Check that J2000 equatorial coordinates satisfy some  
    observability conditions including Az, Alt, HA.  
    Input  : - J2000.0 R.A. [radians].  
    - J2000.0 Dec. [radians].  
    - JD. If emepty than use current JD.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Lon'  
    '  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Apr 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Flag,FlagRes]=celestial.coo.is_coordinate_ok(1,0)  
    Reliable:  
      
      
      
### celestial.coo.light_abberation

light_abberation function                                          ephem Description: Given an object observer-centric direction, corrected for light deflection in the natural frame (P1), calculate the proper direction of the object (P2) in


    
      
    light_abberation function                                          ephem  
    Description: Given an object observer-centric direction, corrected  
    for light deflection in the natural frame (P1),  
    calculate the proper direction of the object (P2) in  
    the observer-centric inertial frame that is moving  
    with instantaneous velocity of the observer (V)  
    relative to the natural frame.  
    Input  : - (P1) object observer-centric direction, corrected  
    for light deflection in the natural frame. [au]  
    (3-element column vector).  
    If more than one column is given, then the proper direction  
    is calculated for each vector. In that case Q and E  
    should contain the same number of columns.  
    - (Et) Observer velocity in the barycentric frame. [au/day]  
    Output : - The object proper direction (P2) in the  
    observer-centric inertial frame that is moving  
    with instantaneous velocity of the observer (V)  
    relative to the natural frame.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
### celestial.coo.light_deflection

light_deflection function                                              ephem Description: Calculate the observer-centric direction of a planet, corrected for light deflection in the natural frame. Note: for the stellar case Q=P.


    
      
    light_deflection function                                              ephem  
    Description: Calculate the observer-centric direction of a planet,  
    corrected for light deflection in the natural frame.  
    Note: for the stellar case Q=P.  
    Input  : - 3 by n matrix (P) of planet observer-centric position vector.  
    3-element column vector per epoch.  
    If more than one column is given, then the light deflection  
    is calculated per column vector. In that case Q and E  
    should contain the same number of columns.  
    Position vector units should be AU.  
    - (Q) Planet helicentric position vector. [au]  
    - (E) Observer helicentric position vector. [au]  
    Output : - Observer-centric direction (P1) of the object, corrected  
    for light deflection in the natural frame. [au]  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                       May 2001  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: E=[1;0;0]; Q=[0;100;0]; P=[0;99;0];  
    P1=light_deflection(P,Q,E);  
      
### celestial.coo.nearest_coo

nearest_coo function                                                   ephem Description: Given a list of coordinates (with arbitrary number of dimensions), search for the coordinate in list which is the nearest to a given (single) coordinate.


    
      
    nearest_coo function                                                   ephem  
    Description: Given a list of coordinates (with arbitrary number of  
    dimensions), search for the coordinate in list which is  
    the nearest to a given (single) coordinate.  
    Input  : - List of coordinates: [X] or [X Y] or [X Y Z],...  
    - Coordinate to search [X] or [X Y],...  
    - Coordinate type: {'plane' | 'sphere'}, default is 'plane' -  
    'sphere' is available only for 2-D [long, lat].  
    If 'sphere' then coordinates must be given in radians.  
    Output : - Coordinate of nearest point in the list.  
    - Index of nearest point in the list.  
    - Distanace to all the points in list (in radians for 'sphere').  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Dec 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [NearCoo,NearInd,DistAll]=nearest_coo([1 1;2 2;3 1],[1 1]);  
    [NearCoo,NearInd,DistAll]=nearest_coo([1 1;2 2;3 1],[1 1.1],'sphere');  
    Reliable: 2  
      
### celestial.coo.nutation

Intermidiate accuracy IAU 1984 nutation Package: celestial.coo Description: Calculate the Nutation in longitude and latitude, and the nutation rotation matrix. This is a low accuracy version based on the IAU 1984 nutation


    
    Intermidiate accuracy IAU 1984 nutation  
    Package: celestial.coo  
    Description: Calculate the Nutation in longitude and latitude, and the  
    nutation rotation matrix.  
    This is a low accuracy version based on the IAU 1984 nutation  
    series. See also: nutation1984.m  
    Input  : - Column vector of julian day.  
    - Type of nutation matrix:  
    'f' : full precision, (default).  
    'l' : linearized matrix.  
    Output : - Matrix listing nutation in longitude and obliquity,  
    line per JD [radians], in the first and second columns,  
    respectively.  
    - Nutation rotation matrix.  
    In case that more then one JD is asked,  
    then this is a cube.  
    Reference : Explanatory Supplement to the Astronomical Almanac (1992)  
    See also: nutation1984.m, nutation2rotmat.m  
    Tested : Matlab 5.2  
    By : Eran O. Ofek                    Feb 2000  
    URL : htpp://weizmann.ac.il/home/eofek/matlab/  
    Example: [N,NM]=celestial.coo.nutation(2451545);  
    Reliable: 2  
      
### celestial.coo.nutation1980

nutation1984 function                                              ephem Description: Calculate the IAU 1980 Nutation series for a set of JDs.


    
      
    nutation1984 function                                              ephem  
    Description: Calculate the IAU 1980 Nutation series for a set of JDs.  
    Input  : - Column vector of JDs.  
    - Output units {'rad','deg','as','mas'}. Default is 'rad'.  
    - Type of nutation matrix (see nutation2rotmat.m):  
    'f' : full precision, (default).  
    'l' : linearized matrix.  
    Output : - A two column matrix of nutation in [long., obliq].  
    - A cube of nutation rotation matrices (see nutation2rotmat.m).  
    Reference: http://hpiers.obspm.fr/eop-pc/models/nutations/nut.html  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Nut,NutMatrix]=nutation1984(2451545+[1:1:100].');  
    Reliable: 2  
      
      
### celestial.coo.nutation2rotmat

nutation2rotmat function                                           ephem Description: Given nutation in longitude and obliquity (in radians) and JD, return the Nutation rotation matrix.


    
      
    nutation2rotmat function                                           ephem  
    Description: Given nutation in longitude and obliquity (in radians)  
    and JD, return the Nutation rotation matrix.  
    Input  : - A two column matrix of nutation in [Long, Obliq].  
    - JD for which the nutations were calculated.  
    - Type of nutation matrix:  
    'f' : full precision, (default).  
    'l' : linearized matrix.  
    Output : - A cube of Nutation rotation matrices. The 3rd Dim is for  
    various times.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jun 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: NutMatrix=nutation2rotmat(Nut,JD);  
    Reliable: 2  
      
      
### celestial.coo.nutation_lowacc

nutation_lowacc function                                           ephem Description: Low accuracy (~1") calculation of the nutation.


    
      
    nutation_lowacc function                                           ephem  
    Description: Low accuracy (~1") calculation of the nutation.  
    Input  : - Matrix of Julian days.  
    Output : - Matrix of nutation in longitude [rad].  
    - Matrix of nutation in obliquity [rad].  
    See also: nutation.m, nutation1984.m, nutation2rotmat.m  
    Reference: Seidelmann (1992)  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [DLon,DObl]=nutation_lowacc(2451666);  
    Reliable: 2  
      
### celestial.coo.obliquity

Calculate the obliquity of the Earth ecliptic. Package: celestial.coo Description: Calculate the obliquity of ecliptic, with respect to the mean equator of date, for a given julian day.


    
    Calculate the obliquity of the Earth ecliptic.  
    Package: celestial.coo  
    Description: Calculate the obliquity of ecliptic, with respect to the  
    mean equator of date, for a given julian day.  
    Input  : - Vector of Julian Days.  
    - Caqlculation type:  
    'L' - IAU 1976, good from 1000-3000 AD,  
    default.  
    'H' - Laskar expression, more accurate.  
    Output : - Obliquity of ecliptic of date in radians.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Obl=celestial.coo.obliquity(2451545+[0:1:5]).';  
    Reliable: 1  
      
      
### celestial.coo.parallactic2ha

Convert parallactic angle and declinatio to hour angle Package: celestial.coo Description: Convert parallactic angle, declination and latitude to hour angle. Note that there are two solutions, and the function will return both.


    
    Convert parallactic angle and declinatio to hour angle  
    Package: celestial.coo  
    Description: Convert parallactic angle, declination and latitude to  
    hour angle. Note that there are two solutions, and the  
    function will return both.  
    Input  : - Parallactic angle [rad].  
    - Declination [rad].  
    - Observer latitude [rad].  
    Output : - Hour angle first solution [rad].  
    - Hour angle second solution [rad].  
    Tested : Matlab R 2011b  
    By : Eran O. Ofek                    Nov 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [HA1,HA2]=celestial.coo.parallactic2ha(1,1,1);  
    Reliable: 2  
      
      
### celestial.coo.parallactic_angle

parallactic_angle function                                             ephem Description: Calculate the parallactic angle of an object. The parallactic is defined as the angle between the local zenith, the object and the celestial north pole measured


    
      
    parallactic_angle function                                             ephem  
    Description: Calculate the parallactic angle of an object.  
    The parallactic is defined as the angle between the local  
    zenith, the object and the celestial north pole measured  
    westwerd (e.g., negative before, and positive after the  
    passage through the southern meridian).  
    Input  : * Set of three input argument: [RA, Dec], LST, Lat  
    or alternatively four input argument: RA, Dec, LST, Lat.  
    Where RA, Dec and Lat are in radians, and LST in  
    fraction of days. Lat is observer the geodetic latitude.  
    LST can be either a scalar, matrix of the same size as  
    RA and Dec, or a vector which have a common dimension  
    as RA and Dec.  
    Output : - Parallactic angle  
    If object in the zenith then NaN.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                   October 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
      
### celestial.coo.parseCooInput

Parse RA/Dec coordinates


    
    Parse RA/Dec coordinates  
    Input  : - RA in [rad], [deg], [sexagesinmal], or [object name]  
    - Dec in [rad], [deg], [sexagesinmal. If empty, then RA is  
    object name.  
    * ...,key,val,...  
    'InUnits' - Default is 'deg'.  
    'OutUnits' - Default is 'deg'.  
    'NameServer' - Default is 'simbad'.  
    Output : - RA  
    - Dec  
    Author : Eran Ofek (Oct 2021)  
    Example: [RA, Dec] = celestial.coo.parseCooInput(1, 1, 'InUnits','rad', 'OutUnits','deg')  
      
### celestial.coo.pm2space_motion

pm2space_motion function                                           ephem Description: Convert proper motion, radial velocity and parralax to space motion vector in the equatorial system.


    
      
    pm2space_motion function                                           ephem  
    Description: Convert proper motion, radial velocity and parralax to  
    space motion vector in the equatorial system.  
    Input  : - [RA] in radians.  
    - [Dec] in radians.  
    - PM in RA [mas/yr]  
    - PM in Dec [mas/yr]  
    - Parallax [mas]  
    - Radial velocity [km/s]  
    Output : - Space motion vector [X Y Z] in au/day, in the equatorial  
    system.  
    - Space position vector in au, in the equatorial system.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Seidelmann 1992 p.121  
    Reliable: 2  
      
### celestial.coo.pm_eq2gal

SHORT DESCRIPTION HERE Package: celestial Description:


    
    SHORT DESCRIPTION HERE  
    Package: celestial  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [mu_l,mu_b]=celestial.coo.pm_eq2gal(alpha,delta,mu_alpha,mu_delta)  
    Reliable:  
      
      
### celestial.coo.pm_vector

pm_vector function                                                 ephem Description: Return the space motion vector given proper motion, parallax and radial velocity. Obsolete: use pm2space_motion instead.


    
      
    pm_vector function                                                 ephem  
    Description: Return the space motion vector given proper motion, parallax  
    and radial velocity.  
    Obsolete: use pm2space_motion instead.  
    Input  : - R.A. in radians.  
    - Dec. in radians.  
    - Proper motion in R.A. in radians/century.  
    - Proper motion in Dec. in radians/century.  
    - Radial velocity (V) in au/century (1km/sec = 21.095 au/century),  
    measured positively away frm the Earth.  
    Default is 0.  
    - Parallax in radians. Default is 0.  
    Output : - Space motion vector (m) of the star expressed  
    in radians per cenury.  
    Referred to the standard equator and the coordinates equinox.  
    If the input parameters are vectors, then the result  
    is a matrix: each column for each vector element.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: M=pm_vector(1,1,1e-9,1e-9,10,1e-9);  
    Reliable: 2  
      
### celestial.coo.polar_alignment

Calculate the RA/Dec drift due to equatorial polar alignemnt error. Package: celestial Description: Given a set of Declination-drift observations over several hour angle and declinations, calculate the deviation of the polar mount from the true North celestial pole.


    
    Calculate the RA/Dec drift due to equatorial polar alignemnt error.  
    Package: celestial  
    Description: Given a set of Declination-drift observations over several  
    hour angle and declinations, calculate the deviation of the  
    polar mount from the true North celestial pole.  
    Input  : - Vector of HA of targets [rad].  
    - Vector Dec of targets [rad].  
    - Vector of measured Declination drifts ["/s].  
    - Vector of measured Right Asecnsion drifts ["/s].  
    - True altitude of NCP (Geodetic latitude of observer) [rad].  
    - Plot rms vs. beta and psi. Default is false.  
    - UseOnlyDec. Default is false.  
    Output : - Structure of results.  
    .BestAlt - Alt shift in [deg] needed to fix polar alignment.  
    Positive upward.  
    .BestAz - Az shift in [deg] needed to fix polar alignment  
    Positive Eastward.  
    .BestPsi - Best fit hour angle of telescope pole relative to  
    NCP [deg], measured westward from the superior merdian.  
    .BestBeta - Best fit angular distance [deg] between telescope  
    pole and NCP.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Res]=celestial.coo.polar_alignment  
    Reliable:  
      
      
      
      
### celestial.coo.polar_alignment_drift

Calculate the RA/Dec drift due to equatorial polar alignemnt error. Package: celestial Description:


    
    Calculate the RA/Dec drift due to equatorial polar alignemnt error.  
    Package: celestial  
    Description:  
    Input  : - HA of target [rad].  
    - Dec of target [rad].  
    - True altitude of NCP (Geodetic latitude of observer) [rad].  
    - HA of telescope pole [rad]  
    - Distance between NCP and equaltorial pole [rad]  
    Output : - Tracking error in HA ["/s]  
    - Tracing error in Dec ["/s]  
    - Tracking error in RA ["/s]  
    - Az needed to move the mount in order to coorect alignement (positive Eastward)  
    - Alt needed to move the mount in order to correct the alignement (positive upward)  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [H_dot,D_dot,R_dot]=celestial.coo.polar_alignment_drift(10./RAD,0,32./RAD,45./RAD,1./RAD)  
    Reliable:  
      
      
### celestial.coo.polar_alignment_tracking_error

Package: celestial.coo Description:


    
      
    Package: celestial.coo  
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
      
      
      
### celestial.coo.pole_from2points

Find pole of a great circle defined by two points on the sphere. Package: celestial.coo Description: Given two points on the celestial sphere (in any system) describing the equator of a coordinate system, find one of the poles of this coordinate system.


    
    Find pole of a great circle defined by two points on the sphere.  
    Package: celestial.coo  
    Description: Given two points on the celestial sphere (in any system)  
    describing the equator of a coordinate system,  
    find one of the poles of this coordinate system.  
    Input  : - RA of 1st point [radians].  
    - Dec of the 1st point [radians]  
    - RA of the 2nd point [radians]  
    - Dec of the 2nd point [radians]  
    Output : - RA of one of the poles.  
    - Dec of the pole.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jul 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [RA3,Dec3]=celestial.coo.pole_from2points(RA1,Dec1,RA2,Dec2)  
    Reliable: 2  
      
      
      
### celestial.coo.precession

Calculate the Earth precession parameters Package: celestial.coo Description: Calculate the Earth precssion parameters as a function of JD.


    
    Calculate the Earth precession parameters  
    Package: celestial.coo  
    Description: Calculate the Earth precssion parameters as a function of  
    JD.  
    Input  : - JD  
    Output : - ZetaA [radians].  
    - ZA [radians]  
    - ThetaA [radians]  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Nov 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [ZetaA,ZA,ThetaA]=celestial.coo.precession(2451545+[0:1:5]');  
    Reliable: 1  
      
      
### celestial.coo.proper_motion

Applay proper motion to a catalog Package: celestial.coo Description: Applay proper motion to a catalog


    
    Applay proper motion to a catalog  
    Package: celestial.coo  
    Description: Applay proper motion to a catalog  
    Input  : - Final epoch (days). Either a scalar or a vector.  
    - Catalog initial epoch (days) of RA.  
    Either a scalar or a vector.  
    - Catalog initial epoch (days) of Dec. If empty then will  
    use the EpochIn of RA.  
    Either a scalar or a vector.  
    - RA at initial epoch [radians].  
    - Dec at initial epoch [radians].  
    - Proper motion in RA [mas/yr].  
    - Proper motion in Dec [mas/yr].  
    - Parallax [mas], default is 1e-4;  
    - Radial velocity [km/s], default is 0.  
    Output * Either 2 or 3 output arguments.  
    If two output arguments, these are the final RA and Dec  
    in radians.  
    [RA,Dec] = proper_motion(...);  
    If three output arguments, these are the X,Y,Z cosine directions  
    in units of AU.  
    [X,Y,Z] = proper_motion(...);  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jan 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### celestial.coo.proper_motion_parallax

Applay proper motion and parallax to a catalog Package: celestial.coo Description: Applay proper motion to a catalog


    
    Applay proper motion and parallax to a catalog  
    Package: celestial.coo  
    Description: Applay proper motion to a catalog  
    Input  : - Final epoch (days). Either a scalar or a vector.  
    - Catalog initial epoch (days) of RA.  
    Either a scalar or a vector.  
    - Catalog initial epoch (days) of Dec. If empty then will  
    use the EpochIn of RA.  
    Either a scalar or a vector.  
    - RA at initial epoch [radians].  
    - Dec at initial epoch [radians].  
    - Proper motion in RA [mas/yr].  
    - Proper motion in Dec [mas/yr].  
    - Parallax [mas], default is 1e-4;  
    - Radial velocity [km/s], default is 0.  
    Output * Either 2 or 3 output arguments.  
    If two output arguments, these are the final RA and Dec  
    in radians.  
    [RA,Dec] = proper_motion(...);  
    If three output arguments, these are the X,Y,Z cosine directions  
    in units of AU.  
    [X,Y,Z] = proper_motion(...);  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jan 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### celestial.coo.refraction

Estimate atmospheric refraction, in visible light. Package: celestial.coo Description: Estimate atmospheric refraction, in visible light.


    
    Estimate atmospheric refraction, in visible light.  
    Package: celestial.coo  
    Description: Estimate atmospheric refraction, in visible light.  
    Input  : - Vector of altitude [radians].  
    - [T (C), P (mb), Rh ()]. default is [20, 1000, 0.8]  
    Relative humidity (Rh) is used only in 'Sa' formula.  
    - Formula type:  
    'AA' - Astronomical almanach, low accuracy for Alt>15deg,  
    default.  
    'Sa' - Saastamoinen's formula for Alt>20deg.  
    Output : - Refraction correction in radians. (Add to Alt).  
    Reference : AA 2001  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                   Nov 2000  
    URL : http://weizmann.ac.il/home/eran/matlab/  
    See also: refraction_wave.m  
    Example: [R]=celestial.coo.refraction([20;30]./RAD);  
    Reliable: 1  
      
### celestial.coo.refraction_coocor

Atmospheric refraction correction for equatorial coordinates. Package: celestial.coo Description: Calculate the correction in equatorial coordinates due to atmospheric refraction.


    
    Atmospheric refraction correction for equatorial coordinates.  
    Package: celestial.coo  
    Description: Calculate the correction in equatorial coordinates due to  
    atmospheric refraction.  
    Input  : - Vector of R.A. in radians, [H M S] or sexagesimal format.  
    See convertdms.m for details.  
    - Vector of Dec. in radians, [Sign D M S] or sexagesimal format.  
    - Refraction in radians. See refraction.m.  
    - Parallactic angle in radians. See parallactic_angle.m.  
    Alternatively, if additional input argument are specified  
    then this is assumed to be the JD.  
    - Optional Geodetic position [Long, Lat], in radians.  
    If specified then the previous input argument is assumed  
    to be JD.  
    Output : - Offset in R.A. needed to be added to true position in  
    order to get the apparent position [radians].  
    - Offset in Dec. needed to be added to true position in  
    order to get the apparent position [radians].  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                     Oct 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Examples: [DelAlpha,DelDelta]=celestial.coo.refraction_coocor(RA,Dec,Ref,ParAng);  
    [DelAlpha,DelDelta]=celestial.coo.refraction_coocor(RA,Dec,Ref,JD,GeodPos);  
    Reliable: 1  
      
      
### celestial.coo.refraction_wave

refraction_wave function                                           ephem Description: Calculate the wavelength-dependent atmospheric refraction and index of refraction based on Cox (1999) formula.


    
      
    refraction_wave function                                           ephem  
    Description: Calculate the wavelength-dependent atmospheric refraction  
    and index of refraction based on Cox (1999) formula.  
    Input  : - Altitude [radians].  
    - Wavelength [Ang]. Default is 5000 Ang.  
    - Temperature [C]. Default is 15 C .  
    - Pressure [hPa]. Default is 760 mm Hg.  
    - Partial vapour pressure. Default is 8 mm Hg.  
    Output : - Atmospheric refraction [radians].  
    - Index of refraction.  
    Reference: Filippenko 1982 (PASP 94, 715)  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                   Nov 2013  
    URL : http://weizmann.ac.il/home/eran/matlab/  
    See also: refraction_wave.m  
    Example: [R,N]=celestial.coo.refraction_wave([20;30]./RAD,7000);  
    Reliable: 2  
      
      
### celestial.coo.rotm_coo

Rotation matrix for coordinate conversion Package: celestial.coo Description: Generate a rotation matrix for coordinate conversion and precession.


    
    Rotation matrix for coordinate conversion  
    Package: celestial.coo  
    Description: Generate a rotation matrix for coordinate conversion  
    and precession.  
    Input  : - Type of rotation matrix  
    'e'  - equatorial with given equinox  
    to ecliptic with the same ecliptic and equinox .  
    Default is J2000.0  
    'E'  - ecliptic with given ecliptic and equinox  
    to the equatorial with the same equinox.  
    Default is J2000.0  
    'G'  - galactic to equatorial with mean equinox of J2000.0  
    'g'  - equatorial with mean equinox of J2000.0 to galactic.  
    'p'  - precession matrix from mean equinox  
    of date to mean equinox of J2000.0.  
    'P'  - precession matrix from mean equinox  
    J2000.0 to mean equinox of date.  
    'pd' - precession matrix from true equinox  
    of date to mean equinox of J2000.0.  
    'Pd' - precession matrix from mean equinox  
    J2000.0 to true equinox of date.  
    'gSG'- Equatorial J2000.0 to Super Galactic.  
    'SGg'- Super Galactic to Equatorial J2000.0.  
    'gCMB'-Egalactic to WMAP 3rd year, CMB dipole.  
    'CMBg'- CMB dipole to Galactic.  
    - Equinox of coordinates (in Julian Day),  
    used only in the case of 'p' | 'P' | 'pd' | 'Pd' | 'ed' | 'Ed'  
    In case of 'E' or 'q' if this parameter is  
    not given it is taken as 2451545.0 (=J2000.0)  
    Output : - Rotation matrix  
    Reference : Ex. Supp. to the Astronomical Almanac.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: R=celestial.coo.rotm_coo('E');  
    convert ecliptic coordinate [1 1] to equatorial:  
    celestial.coo.cosined([R*celestial.coo.cosined([1 1]).'].');  
    Reliable: 1  
      
      
### celestial.coo.sky_area_above_am

Calculate sky area observable during the night above a specific airmass. Package: celestial.coo Description: Calculate sky area observable during the night above a specific airmass, and assuming each field is observable for at least TimeVis hours.


    
    Calculate sky area observable during the night above a specific airmass.  
    Package: celestial.coo  
    Description: Calculate sky area observable during the night above  
    a specific airmass, and assuming each field is observable  
    for at least TimeVis hours.  
    Input  : - JD.  
    - Latitute [rad].  
    - Airmass.  
    - Time visibility [hours].  
    Output : - Sky area [deg^2]  
    - Length of night [fraction of day].  
    Tested : Matlab R2013a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: for I=1:1:366, [SkyArea16(I),LON(I)]=celestial.coo.sky_area_above_am(VecJD(I),Lat,AM,TimeVis); end  
    Reliable: 2  
      
### celestial.coo.solve_alignment6dof_problem




    
      
      
      
    DOF:  
    Phi   - HA of the mount-axis as measured Eastward from the NCP  
    Theta - HA of the telescope-axis as measured Eastward from the axis  
    D     - NCP-Axis angular distance  
    R     - Telescope-Axis angular distance  
    L0    - encoder Dec zero  
    H0    - encoder HA zero  
      
    additional parameters  
    H     - HA of pointing relative to axis  
    Ht    - Ht = H - H0  
    L     - pointing-axis angular distance  
    Lt    - Lt = L - L0  
      
    Input:  
    HA_Tel, Dec_Tel - from astrometry  
    HA_Axis, Dec_Axis - from mount  
      
### celestial.coo.sphere_dist

angular distance and position angle between two points on the sphere Package: celestial.coo Description: Calculate the angular distance and position angle between two points on the celestial sphere.


    
    angular distance and position angle between two points on the sphere  
    Package: celestial.coo  
    Description: Calculate the angular distance and position angle between  
    two points on the celestial sphere.  
    Input  : - Column vector of "long." for the first point  
    [rad] or [H M S] or sexagesimal string.  
    see convertdms.m for details.  
    - Column vector of "lat." for the first point  
    [rad] or [Sign D M S] or sexagesimal string.  
    - Column vector of "long." for the second point  
    [rad] or [H M S] or sexagesimal string.  
    - Column vector of "lat." for the second point  
    [rad] or [Sign D M S] or sexagesimal string.  
    - Units of input parameters {'g'|'rad'|'deg'}, default is 'g'.  
    'g' - radians or sexagesimal.  
    'rad' - radians only.  
    'deg' - degrees only.  
    Output is always in radians.  
    Output : - Vector of distances between points [radian].  
    - Vector of position angle between points [radian].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Feb 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: sphere_dist_fast.m, sphere_dist_cosd.m (and built in distance.m).  
    Example: [D,P]=celestial.coo.sphere_dist([1.1;1],[0.2;0.1],[10 0 0],[1 40 0 0]);  
    Reliable: 1  
      
### celestial.coo.sphere_dist_cosd

Angular distance between a set of two cosine vector directions. Package: celestial.coo Description: Calculate the angular distance between a set of two cosine vector directions. This should be used instead of sphere_dist_fast.m only


    
    Angular distance between a set of two cosine vector directions.  
    Package: celestial.coo  
    Description: Calculate the angular distance between a set of two  
    cosine vector directions.  
    This should be used instead of sphere_dist_fast.m only  
    if you have the cosine vectors.  
    Input  : - A 3 column matrix of cosine directions, row per vector.  
    - A 3 column matrix of cosine directions, row per vector.  
    Output : - Vector of distances between points [radian].  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: sphere_dist.m, sphere_dist_fast.m (and built in distance.m).  
    Example: D=sphere_dist_cosd(CD1,CD2);  
    Reliable: 1  
      
      
### celestial.coo.sphere_dist_fast

sphere_dist_fast function                                          ephem Description: Calculate the angular distance between two points on the celestial sphere. See sphere_dist.m (and built in distance.m) for a more general function. This function is ~10 time


    
      
    sphere_dist_fast function                                          ephem  
    Description: Calculate the angular distance between two points on the  
    celestial sphere. See sphere_dist.m (and built in distance.m)  
    for a more general function. This function is ~10 time  
    faster than sphere_dist.m, but it works only with radians  
    and calculate only the distance.  
    Input  : - Matrix of logitudes for the first point [radian].  
    - Matrix of latitudes for the first point [radian].  
    - Matrix of logitudes for the second point [radian].  
    - Matrix of latitudes for the second point [radian].  
    Output : - Matrix of distances between points [radian].  
    - Matrix of position angles between points [radian].  
    Measured westward. Take 2*pi-PA to get the PA measured  
    Eastward.  
    - Matrix of P.A. of the first point relative to the second point  
    (eastwrad from the north).  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Feb 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: sphere_dist.m, sphere_dist_cosd.m (and built in distance.m).  
    Example: D=celestial.coo.sphere_dist_fast(RA1,Dec1,RA2,Dec2);  
    Reliable: 1  
      
      
### celestial.coo.sphere_dist_fast_thresh

sphere_dist_fast_thresh function                                   ephem Description: Calculate the angular distance between two points on the celestial sphere. See sphere_dist.m (and built in distance.m) for a more general function. This function is ~10 time


    
      
    sphere_dist_fast_thresh function                                   ephem  
    Description: Calculate the angular distance between two points on the  
    celestial sphere. See sphere_dist.m (and built in distance.m)  
    for a more general function. This function is ~10 time  
    faster than sphere_dist.m, but it works only with radians  
    and calculate only the distance.  
    Input  : - Matrix of logitudes for the first point [radian].  
    - Matrix of latitudes for the first point [radian].  
    - Matrix of logitudes for the second point [radian].  
    - Matrix of latitudes for the second point [radian].  
    Output : - Matrix of distances between points [radian].  
    - Matrix of position angles between points [radian].  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Feb 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: sphere_dist.m (and built in distance.m).  
    Example: D=sphere_dist_fast_thresh(RA1,Dec1,RA2,Dec2);  
    Reliable:  
      
      
### celestial.coo.sphere_dist_thresh

sphere_dist_thresh function                                        ephem Description: Given Long and Lat coordinates and a reference coordinates (in radians) return a flag indicating if each point is within a spherical distance from a reference point.


    
      
    sphere_dist_thresh function                                        ephem  
    Description: Given Long and Lat coordinates and a reference coordinates  
    (in radians) return a flag indicating if each point is  
    within a spherical distance from a reference point.  
    Input  : - Matrix of Longitude coordinates [rad].  
    - Matrix of Latitude coordinates [rad].  
    - Reference Longitude coordinates [rad].  
    - Reference Latitude coordinates [rad].  
    - Distance threshold [rad] (radius, or box full width).  
    - Search shape {'box'|'circ'}. Default is 'circ'.  
    Output : - Flag indicating if the input Long/Lat points are in the  
    requested region.  
    - Distance.  
    - Position angle [radians].  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [Flag,Dist,PA]=sphere_dist_thresh(rand(100,1),rand(100,1),0.5,0.5,0.2,'box')  
    Reliable: 2  
      
### celestial.coo.sphere_move

Applay offset to RA and Dec Package: +celestial.coo


    
    Applay offset to RA and Dec  
    Package: +celestial.coo  
    Input  : - Longitude  
    - Latitude  
    - Offset in longitude  
    - Offset in latitude  
    - Input and output units. Default is 'rad'.  
    Output : - Output longitude  
    - Output latitude  
      
      
### celestial.coo.sphere_offset

sphere_offset function                                             ephem Description: Calculate the offset needed to move from a point on the celesial sphere to a second point on the celestial sphere, along longitide (small circle) and latitude (great circle).


    
      
    sphere_offset function                                             ephem  
    Description: Calculate the offset needed to move from a point on the  
    celesial sphere to a second point on the celestial sphere,  
    along longitide (small circle) and latitude (great circle).  
    The needed offsets depends in which axis the offset is done  
    first (longitude or latitude - 'rd' and 'dr' options,  
    respectively).  
    Input  : - Column vector of "long." for the first point  
    [rad] or [H M S] or sexagesimal string.  
    - Column vector of "lat." for the first point  
    [rad] or [Sign D M S] or sexagesimal string.  
    - Column vector of "long." for the second point  
    [rad] or [H M S] or sexagesimal string.  
    - Column vector of "lat." for the second point  
    [rad] or [Sign D M S] or sexagesimal string.  
    - Units of input parameters {'rad'|'deg'}, default is 'rad'.  
    Output is always in radians.  
    - Type of offset:  
    'rd'   : Move in longitude (e.g., RA) followed by move  
    in latitude (e.g., Dec.). Default.  
    'dr'   : First move in latitude, then in longitude.  
    Output : - Vector of offsets in longitude [rad].  
    - Vector of offsets in latitude [rad].  
    - Vector of distances between the two points (along great  
    circle; see sphere_dist.m).  
    - Vector of position angles between the two points.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [OffsetLong,OffsetLat,Dist,PA]=sphere_offset([10 0 0],[1 60 0 0],[10 0 15],[1 60 2 56],[],'rd');  
    Reliable: 1  
      
      
### celestial.coo.spherical_tri_area

spherical_tri_area function                                         AstroMap Description: Given three coordinates on a sphere, calculate the area of a spherical triangle defined by these three points.


    
      
    spherical_tri_area function                                         AstroMap  
    Description: Given three coordinates on a sphere, calculate the area  
    of a spherical triangle defined by these three points.  
    Input  : - [Long, Lat] in radians of first point on a sphere.  
    - [Long, Lat] in radians of second point on a sphere.  
    - [Long, Lat] in radians of third point on a sphere.  
    Output : - Area of spherical triangles.  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                  December 2008  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
      
### celestial.coo.spherical_triangle_circum_circle

Calculate the radius of the circum circle of a spherical triangle Package: celestial.coo


    
    Calculate the radius of the circum circle of a spherical triangle  
    Package: celestial.coo  
    Input  : - Method by which to calculate the radius. The following methods  
    are supported:  
    'sides' - calculate the radius from the sides a,b,c.  
    'angles'- calculate the radius from the angles A,B,C.  
    'vertex'- calculate the radius from the triangle vertces.  
    * 3 or 6 arguments describing the spherical triangles.  
    For 'sides', the arguments are arrays of a,b,c.  
    For 'angles', the arguments are arrays of A,B,C.  
    For 'vertex', the arguments are arrays of  
    Long1,Lat1,Long2,Lat2,Long3,Lat3.  
    All arguments are in radians.  
    Output : - An array of the radii of the circum circles.  
    Reference: Todhunter & Leathem  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: R=celestial.coo.spherical_triangle_circum_circle('sides',1,1,1)  
    Reliable: 2  
      
      
### celestial.coo.spherical_triangle_inscribed_circle

Calculate the radius of the inscribed circle of a spherical triangle Package: celestial.coo


    
    Calculate the radius of the inscribed circle of a spherical triangle  
    Package: celestial.coo  
    Input  : - Method by which to calculate the radius. The following methods  
    are supported:  
    'sides' - calculate the radius from the sides a,b,c.  
    'angles'- calculate the radius from the angles A,B,C.  
    'vertex'- calculate the radius from the triangle vertces.  
    * 3 or 6 arguments describing the spherical triangles.  
    For 'sides', the arguments are arrays of a,b,c.  
    For 'angles', the arguments are arrays of A,B,C.  
    For 'vertex', the arguments are arrays of  
    Long1,Lat1,Long2,Lat2,Long3,Lat3.  
    All arguments are in radians.  
    Output : - An array of the radii of the inscribed circles.  
    Reference: Todhunter & Leathem  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: r=celestial.coo.spherical_triangle_inscribed_circle('sides',1,1,1)  
    Reliable: 2  
      
      
### celestial.coo.star_conjunctions

Calculate conjuctions between stars given their proper motion. Package: celestial.coo Description: Given a star with its coordinates, proper motion and optionally parallax and radial velocity, and a list of multiple stars at the same sky region, calculate possible


    
    Calculate conjuctions between stars given their proper motion.  
    Package: celestial.coo  
    Description: Given a star with its coordinates, proper motion and  
    optionally parallax and radial velocity, and a list of  
    multiple stars at the same sky region, calculate possible  
    conjunctions between the star in the first list and all the  
    stars in the second list. For each possible conjunction,  
    calculate also the microlensing properties of the event  
    including the astrometric microlensing.  
    Input  : - A matrix containing one line, or a an AstCat object containing  
    a single star.  
    - A matrix in which each line correspond to a star, or an AstCat  
    object.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    ''  
    Output : - A structure containing all possible conjunctions.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.coo.proper_motion_nearest_approach  
    Reliable: 2  
      
      
### celestial.coo.star_conjunctions_montecarlo

SHORT DESCRIPTION HERE Package: celestial Description:


    
    SHORT DESCRIPTION HERE  
    Package: celestial  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: QuantD=celestial.coo.star_conjunctions_montecarlo  
    Reliable:  
      
      
### celestial.coo.tile_the_sky

Tile the celestial sphere Package: celestial.coo Description: Tiling the celestial sphere with approximately equal area tiles.


    
    Tile the celestial sphere  
    Package: celestial.coo  
    Description: Tiling the celestial sphere with approximately equal  
    area tiles.  
    Input  : - Number of tiles along the celestial equator.  
    - Number of tiles along a meridian.  
    Output : - List of tiles:  
    [CenterRA,CenterDec,MinRA,MaxRA,MinDec,MaxDec], in radians.  
    - Area of tiles [sr]  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [TileList,TileArea]=celestial.coo.tile_the_sky(360.*4,180.*4);  
    Reliable: 2  
      
      
### celestial.coo.topocentricVector

Calculate the topocentric vector of an observer.


    
    Calculate the topocentric vector of an observer.  
    Input  : - JD in UT1 time system.  
    - Geodetic position. If [], then assume geocentric position  
    and return zeros. Otherwise should be [Long, Lat, Height]  
    in [rad, rad, m].  
    * ...,key,val,...  
    'RefEllipsoid' - Reference ellipsoid. Default is 'WGS84'.  
    'Convert2ecliptic' - A logical indicating if to convert  
    the results to eclitic coordinates.  
    Otherwise, the output is in equatorial coordinates.  
    'Equinox' - Equinox of output: 'date' | 'J2000'.  
    Default is 'date'.  
    'OutUnits' - Output units. Default is 'm'.  
    'Xp' - The angle of the celestial epheerius pole of the  
    Earth with respect to the terrestial pole [rad].  
    Along longitude 0. Default is [].  
    'Yp' - Like Xp but for long of 270 (East). Default is [].  
    Output : - The position vector of the topocentric observer relative  
    to the Earth center.  
    - The radius vector time derivative [rad/s].  
    Author : Eran Ofek (Sep 2021)  
    Example: G = celestial.coo.topocentricVector(celestial.time.julday([21 3 2000]), [35 32 0]./RAD)  
      
### celestial.coo.topocentric_vec

topocentric_vec function                                           ephem OBSOLETE: use celestial.coo.topocentricVector instead Description: Calculate the topocentric position and velocity vectors of an observer, with respect to the true equator and


    
      
    topocentric_vec function                                           ephem  
    OBSOLETE: use celestial.coo.topocentricVector instead  
    Description: Calculate the topocentric position and velocity vectors  
    of an observer, with respect to the true equator and  
    equinox of date. In otder to transform the vectors to a  
    coodinates system in respect to the Earth's mean equator  
    and equinox, use inv(P)*inv(N)*G and inv(P)*inv(N)*Gt,  
    where P and N are the precession and nutation rotation  
    matrices (see rotm_coo.m).  
    Input  : - Column vector of JD in UT1 time scale,  
    or date [D M Y Frac] | [D M Y H M S] | [D M Y].  
    - Geodetic coordinates. This is three column matrix of the  
    type: [Longitude, Latitude, Height], in case that two  
    column matrix is given, than the height is taken as zero.  
    Units: Longitude and latitude measured in radians while  
    the height is measured in meters above reference ellipsoid.  
    - Reference ellipsoid:  
    'Merit1983' - Merit 1983           a=6378137  1/f=298.257  
    'GRS80'     - GRS 80 (IUGG 1980)     6378137      298.257222  
    'GRS67'     - GRS 67 (IUGG 1967)     6378160      298.247167  
    'IAU1976'   - IAU 1976               6378140      298.257  
    'IAU1964'   - IAU 1964               6378160      298.25  
    'WGS84'     - WGS 1984 (default)  
    - The X coordinate of the celestial ephemeris pole  
    with respect to the terrestrial pole measured along  
    the 0 meridian (radians). - default is 0.  
    - The Y coordinate of the celestial ephemeris pole  
    with respect to the terrestrial pole measured along  
    the 270 meridian (radians). default is 0.  
    Output : - The topocentric position vector with respect to the  
    true equator and equinox of date. [meters]  
    Each column for one time.  
    - The topocentric velocity vector with respect to the  
    true equator and equinox of date. [meters/sec]  
    Each column for one time.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek             May 2001  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
      
