# Package: celestial.stars


### celestial.stars.constellation

Find the constellations in which celestial coordinates are located. Package: celestial.stars Description: Find the constellations in which celestial coordinates are located.


    
    Find the constellations in which celestial coordinates are located.  
    Package: celestial.stars  
    Description: Find the constellations in which celestial coordinates are  
    located.  
    Input  : - Matrix of [R.A., Dec.].  
    If two column matrix is given, then the first column is  
    the R.A. and the second is the Dec. (radians).  
    If Seven column matrix is given, then it is assumed  
    to be in sexagesimal format [H M S, Sign D M S].  
    - Equinox of coordinates, in JD (true eq. of date)  
    or 'J2000.0' for mean eq. of J2000.0.  
    'default is 'J2000.0'.  
    Output : - Cell array with constellation names.  
    Reference: Roman N.G., 1987, PASP 99, 695.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [PosConst]=celestial.stars.constellation([1 1;2 2]);  
    Reliable: 2  
      
### celestial.stars.nearest_bright_star

Select N bright stars near a given coordinate Package: +celestial.stars


    
    Select N bright stars near a given coordinate  
    Package: +celestial.stars  
    Input  : - J2000.0 RA [deg] or sexagesimal string.  
    - J2000.0 Dec [deg] or sexagesimal string.  
    - Mag. limit. Default is 2.  
    - Number of stars (sorted by distance). Default is 3.  
    Example : celestial.stars.nearest_bright_star(1,1,2)  
    By : Eran Ofek              Mar 2021  
      
### celestial.stars.star_apparent_place

star_apparent_place function                                           ephem Description: Compute the apparent place of stars at a given epoch, given the star mean place, proper motion, parallax, radial velocity, at a reference epoch T0.


    
      
    star_apparent_place function                                           ephem  
    Description: Compute the apparent place of stars at a given epoch,  
    given the star mean place, proper motion, parallax,  
    radial velocity, at a reference epoch T0.  
    Input  : - Star catalog:  
    [RA, Dec, Mu_RA, Mu_Dec, Parallax, RV].  
    where RA, Dec are the equatorial coordinates of the stars in  
    radians.  
    Mu_RA, Mu_Dec are the proper motion in arcsec per julian century  
    (NaN if unknown).  
    Parallax is the star parallax in arcsec (NaN if unknown).  
    RV is the star radial velocity in km/sec (NaN if unknown).  
    - Catalog equinox (JD or 'J2000' | 'J1991.25' | 'B1950.0').  
    - Catalog epoch (JD or 'J2000' | 'J1991.25' | 'B1950.0')  
    - Epoch of observation: JD in the TT time scale or date  
    [D M Y frac_day]  
    - Observer geodetic coordinates: [Long, Lat, Height] in  
    (rad, rad, meters).  
    Default is [NaN, NaN, NaN] in this case diurnal aberration  
    is neglected.  
    Output : - [R.A., Dec.] in radians referred to equinox and epoch of  
    observation.  
    Reference: Explanatory supplement P. 121, 152  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                   October 2001  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
