# Package: celestial.Earth


### celestial.Earth.earth_gravity_field

Calculate the Earth gravity field for a set of locations. Package: celestial.Earth Description: Calculate the Earth gravity field for a set of locations. For both rotating and non rotating Earth.


    
    Calculate the Earth gravity field for a set of locations.  
    Package: celestial.Earth  
    Description: Calculate the Earth gravity field for a set of locations.  
    For both rotating and non rotating Earth.  
    Mean IRTF pole is assumed.  
    Input  : - Column vector of radii from the Earth center at which to  
    calculate the Earth potential [cm].  
    - Column vector Geocentric latitude of at which to  
    calculate the Earth potential [rad].  
    Note that these are Geocentric rather than Geodetic  
    coordinates.  
    - Column vector Geocentric longitude of at which to  
    calculate the Earth potential [rad].  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Model' - Earth gravitational potential model:  
    'DGM-1S'       - DGM-1s model (deg 250; default)  
    'EIGEN-6C3stat'- EIGEN-6C3stat model (deg 1949)  
    More models are available from:  
    http://icgem.gfz-potsdam.de/ICGEM/  
    'EarthPeriod' - The Earth rotation period, used for  
    calculating the potential on the moving surface of  
    the Earth. Default is 86164.09890369732 s.  
    Output : - The non-rotating Earth potential at the specified  
    locations [cgs units].  
    - The rotating Earth potential at the specified  
    locations [cgs units].  
    - A structure contains the Earth Gravity Model used.  
    References: http://icgem.gfz-potsdam.de/ICGEM/  
    Seidelmann  (1992)  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [V,W,EGM]=celestial.Earth.earth_gravity_field(6731e5,1,1);  
    Reliable: 2  
      
      
### celestial.Earth.geoc2geod

Convert Geocentric coordinates to Geodetic coordinates Description: Convert Geocentric coordinates to Geodetic coordinates using specified reference ellipsoid.


    
    Convert Geocentric coordinates to Geodetic coordinates  
    Description: Convert Geocentric coordinates to Geodetic coordinates  
    using specified reference ellipsoid.  
    Input  : - Geocentric coordinates. This is three column matrix of the  
    type: [Longitude, Latitude, Radius]  
    where Radius is the distance from the sphere center.  
    Units: Longitude and latitude measured in radians while  
    the radius is measured in meters above reference ellipsoid.  
    - Reference ellipsoid:  
    'Merit1983' - Merit 1983           a=6378137  1/f=298.257  
    'GRS80'     - GRS 80 (IUGG 1980)     6378137      298.257222  
    'GRS67'     - GRS 67 (IUGG 1967)     6378160      298.247167  
    'IAU1976'   - IAU 1976               6378140      298.257  
    'IAU1964'   - IAU 1964               6378160      298.25  
    'WGS84'     - WGS 1984 (default)  
    - Convergence accuracy in arcsec, (default is 0.01 arcsec).  
    (Iterative algorithm).  
    Output : - Geodetic coordinates matrix of the type:  
    [longitude, latitude, Height]  
    where longitude and latitude are measured in radians  
    and Height measured in meters above the reference ellipsoid.  
    Reference : Astronomical Almnach  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Geod=celestial.Earth.geoc2geod([1 1 7000]);  
    Reliable: 2  
      
### celestial.Earth.geod2geoc

Convert Geodetic coordinates to Geocentric coordinates Package: celestial.Earth Description: Convert Geodetic coordinates to Geocentric coordinates using specified reference ellipsoid.


    
    Convert Geodetic coordinates to Geocentric coordinates  
    Package: celestial.Earth  
    Description: Convert Geodetic coordinates to Geocentric coordinates  
    using specified reference ellipsoid.  
    Input  : - Geodetic coordinates. This is three column matrix of the  
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
    Output : - Geocentric coordinates matrix of the type:  
    [longitude, latitude, radius]  
    where longitude and latitude are measured in radians  
    and radius measured in meters from the reference ellipsoid center.  
    - Geocentric cartesian coordinates [x, y, z] in meters.  
    Reference : Astronomical Almnach  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Geoc,GeocCart]=celestial.Earth.geod2geoc([1 1 100]);  
    Reliable: 2  
      
### celestial.Earth.obs_coo

SHORT DESCRIPTION HERE Package: celestial Description:


    
    SHORT DESCRIPTION HERE  
    Package: celestial  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    May 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### celestial.Earth.refellipsoid

Return data for a given reference ellipsoid of Earth. Package: celestial.Earth Description: Return data for a given reference ellipsoid of Earth.


    
    Return data for a given reference ellipsoid of Earth.  
    Package: celestial.Earth  
    Description: Return data for a given reference ellipsoid of Earth.  
    Input  : - Reference ellipsoid:  
    'Merit1983' - Merit 1983           a=6378137  1/f=298.257  
    'GRS80'     - GRS 80 (IUGG 1980)     6378137      298.257222  
    'GRS67'     - GRS 67 (IUGG 1967)     6378160      298.247167  
    'IAU1976'   - IAU 1976               6378140      298.257  
    'IAU1964'   - IAU 1964               6378160      298.25  
    'WGS84'     - WGS 1984 (default)     6378137      298.257223563  
    'IERS1989'  - IERS 1989              6378136      298.257  
    Output : - Data matrix:  
    [Equatorial radius (meters),  
    Flattening factor].  
    Reference : Astronomical Almnach  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data=celestial.Earth.refellipsoid('WGS84')  
    Reliable: 2  
      
      
### celestial.Earth.satellite_mag

Satellite apparent magnitude Package: celestial Description: Satellite apparent magnitude


    
    Satellite apparent magnitude  
    Package: celestial  
    Description: Satellite apparent magnitude  
    Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Dist' - Satellite distance [km]. Default is 500.  
    'Area' - Satellite area [cm^2]. Default is 1.  
    'Albedo' - Albedo. Default is 0.1.  
    'SunAbsMag' - Sun abs. mag. Default is -26.74 (V Vega).  
    Output : - Structure containing the following fields:  
    'Mag' - Satellite apparent magnitude.  
    'AngV' - Satellite angular velicity assuming Keplerian orbit  
    with no projection [arcsec/s].  
    'MagResEl' - Magnitude per resolution element (FWHM-length).  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Dec 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=celestial.Earth.satellite_mag  
    Reliable: 2  
      
      
