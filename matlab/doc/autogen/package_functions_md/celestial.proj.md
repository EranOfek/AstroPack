# Package: celestial.proj


### celestial.proj.pr_aitoff

Project coordinates using equal area Aitoff projection Package: celestial.proj Description: Project coordinates (longitude and latitude) using equal area Aitoff projection.


    
    Project coordinates using equal area Aitoff projection  
    Package: celestial.proj  
    Description: Project coordinates (longitude and latitude) using equal  
    area Aitoff projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [X,Y]=celestial.proj.pr_aitoff(1,1,1)  
    Reliable: 1  
      
### celestial.proj.pr_albers

Albers Equal-Area projection. Package: celestial.proj Description: Project coordinates (longitude and latitude) using the Albers Equal-Area projection. The coordinates are projected on ellipse with axis ratio of 2:1.


    
    Albers Equal-Area projection.  
    Package: celestial.proj  
    Description: Project coordinates (longitude and latitude) using the  
    Albers Equal-Area projection. The coordinates are projected on  
    ellipse with axis ratio of 2:1.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    - The origin point of the Cartesian coordinates  
    [Long_0, Lat_0], default is [0 0].  
    - The standart parallels [Lat_1, Lat_2],  
    default is [-pi/2 pi/2].  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jul 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### celestial.proj.pr_azimuthal_equidist

Azimuthal equidistant projection. Package: celestial.proj Description: Project longitude and latitude using Azimuthal equidistant projection (constant radial scale).


    
    Azimuthal equidistant projection.  
    Package: celestial.proj  
    Description: Project longitude and latitude using Azimuthal equidistant  
    projection (constant radial scale).  
    Input  : - Longitude [rad].  
    - CoLatitude [rad].  
    - Sphere radius, default is 1.  
    Output : - X  
    - Y  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Aug 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
      
### celestial.proj.pr_bonne

Bonne projection. Package: celestial.proj Description:  Project coordinates (longitude and latitude) using the Bonne projection.


    
    Bonne projection.  
    Package: celestial.proj  
    Description:  Project coordinates (longitude and latitude) using the  
    Bonne projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    - [Central_Longitude, Standard_Parallel] default [0 pi/4]  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### celestial.proj.pr_cassini

Cassini projection. Package: celestial.proj Description: Project coordinates (longitude and latitude) using the Cassini projection.


    
    Cassini projection.  
    Package: celestial.proj  
    Description: Project coordinates (longitude and latitude) using the  
    Cassini projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    - Central longitude, defauly=t is 0.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jul 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### celestial.proj.pr_conic

Conic projection. Package: celestial.proj Description: Project coordinates (longitude and latitude) using the Conic projection.


    
    Conic projection.  
    Package: celestial.proj  
    Description: Project coordinates (longitude and latitude) using the  
    Conic projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    - Height of apex above sphare center, in sphare radius units.  
    default is 2.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### celestial.proj.pr_cylindrical

pr_cylindrical function                                             AstroMap Description: Project coordinates (longitude and latitude) using a general cylindrical projection.


    
      
    pr_cylindrical function                                             AstroMap  
    Description: Project coordinates (longitude and latitude) using a general  
    cylindrical projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    - Standard coordinates. [Stand_Long, Stand_Lat]. (radians)  
    Speicial cases of cylindrical equal-area projections  
    Stand_Lat    Projection name  
    0 deg.       Lambert Cylindrical Equal-Area (default)  
    30 deg.      Behrmann Cylindrical Equal-Area  
    37.383 deg.  Tristan Edwards Projection  
    44.138 deg.  Peters Projection  
    45 deg.      Gall Orthographic projection  
    50 deg.      Balthasart projection  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                      July 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
### celestial.proj.pr_gnomonic

pr_gnomonic function                                            AstroMap Description: Project coordinates (longitude and latitude) using the Gnomonic non conformal projection This is a nonconformal projection from a sphere center in


    
      
    pr_gnomonic function                                            AstroMap  
    Description: Project coordinates (longitude and latitude) using the  
    Gnomonic non conformal projection  
    This is a nonconformal projection from a sphere center in  
    which orthodromes are stright lines.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    - Central coordinate vector [Long_center,Lat_center]  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jul 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: pr_ignomonic.m  
    Reliable: 2  
      
      
### celestial.proj.pr_hammer

pr_hammer function                                                  AstroMap Description: Project coordinates (longitude and latitude) using the Hammer projection. The coordinates are projected on an ellipse with axis ratio of 2:1.


    
      
    pr_hammer function                                                  AstroMap  
    Description: Project coordinates (longitude and latitude) using the  
    Hammer projection. The coordinates are projected on an ellipse  
    with axis ratio of 2:1.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    August 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 1  
      
### celestial.proj.pr_hammer_aitoff

pr_hammer_aitoff function                                           AstroMap Description: Project coordinates (longitude and latitude) using equal area Hammer-Aitoff projection used in the FITS/WCS standard.


    
      
    pr_hammer_aitoff function                                           AstroMap  
    Description: Project coordinates (longitude and latitude) using equal area  
    Hammer-Aitoff projection used in the FITS/WCS standard.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    For FITS WCS this parameter should be 180./pi.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                 September 2012  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reference: Calabretta & Greisen 2002 A\&A 395, 1077  
    Reliable: 1  
      
### celestial.proj.pr_ignomonic

roject coordinates using the inverse Gnomonic non conformal projection Package: celestial.proj Description: Project coordinates (X and Y) using the inverse Gnomonic non conformal projection,


    
    roject coordinates using the inverse Gnomonic non conformal projection  
    Package: celestial.proj  
    Description: Project coordinates (X and Y) using the  
    inverse Gnomonic non conformal projection,  
    Input  : - Vector of X, in radians.  
    - Vector of Y, in radians.  
    - Central coordinate vector [Long_center,Lat_center],  
    Output : - Vector of longitude in radians.  
    - Vector of latitude in radians.  
    See also: pr_gnomonic.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Long,Lat]=celestial.proj.pr_ignomonic(1,1,[0 0])  
    Reliable: 2  
      
      
### celestial.proj.pr_ihammer_aitoff

pr_ihammer_aitoff function                                          AstroMap Description: Project coordinates (longitude and latitude) using the inverse of the equal area Hammer-Aitoff projection used in the FITS/WCS standard.


    
      
    pr_ihammer_aitoff function                                          AstroMap  
    Description: Project coordinates (longitude and latitude) using the inverse  
    of the equal area Hammer-Aitoff projection used in the  
    FITS/WCS standard.  
    Input  : - Vector of X position.  
    - Vector of X position.  
    - Scale radius, default is 1.  
    For FITS WCS this parameter should be 180./pi.  
    Output : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                 September 2012  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 1  
      
### celestial.proj.pr_mercator

- pr_mercator function                                               AstroMap Description: Project coordinates (longitude and latitude) using the Mercator projection.


    
    -  
    pr_mercator function                                               AstroMap  
    Description: Project coordinates (longitude and latitude) using the  
    Mercator projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    - Central longitude, defauly is 0.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                      July 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
    -  
### celestial.proj.pr_mollweide

pr_mollweide function                                               AstroMap Description: Project coordinates (longitude and latitude) using the equal area Mollweide projection.


    
      
    pr_mollweide function                                               AstroMap  
    Description: Project coordinates (longitude and latitude) using the  
    equal area Mollweide projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    August 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 1  
      
### celestial.proj.pr_parabolic

pr_parabolic function                                               AstroMap Description: Project coordinates (longitude and latitude) using the Parabolic projection.


    
      
    pr_parabolic function                                               AstroMap  
    Description: Project coordinates (longitude and latitude) using the  
    Parabolic projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                      July 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
### celestial.proj.pr_planis

planisphere projection. Package: celestial.proj Description: Project longitude and latitude using a 'planisphere projection'.


    
    planisphere projection.  
    Package: celestial.proj  
    Description: Project longitude and latitude using a  
    'planisphere projection'.  
    Input  : - Longitude [rad].  
    - Latitude [rad].  
    - Latitude [rad] defining projection point, default is 32./RAD.  
    - Sphere radius, default is 1.  
    Output : - X  
    - Y  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Aug 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
      
### celestial.proj.pr_polar

pr_polar function                                                   AstroMap Description: Project coordinates (longitude and latitude) using the polar projection (from north pole).


    
      
    pr_polar function                                                   AstroMap  
    Description: Project coordinates (longitude and latitude) using the  
    polar projection (from north pole).  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    August 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
      
### celestial.proj.pr_sin

Slant ortographic (SIN) projection Package: celestial.proj Description: Slant ortographic (SIN) projection


    
    Slant ortographic (SIN) projection  
    Package: celestial.proj  
    Description: Slant ortographic (SIN) projection  
    Input  : - Longitude [rad].  
    - Latitude [rad].  
    - [Long, Lat] of projection center [rad].  
    Output : - X  
    - Y  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [X,Y]=celestial.proj.pr_sin(1,1,[1 1])  
    Reliable: 2  
      
### celestial.proj.pr_sinusoidal

pr_sinusoidal function                                              AstroMap Description: Project coordinates (longitude and latitude) using the Sinusoidal projection.


    
      
    pr_sinusoidal function                                              AstroMap  
    Description: Project coordinates (longitude and latitude) using the  
    Sinusoidal projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                      July 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
### celestial.proj.pr_stereographic

pr_stereographic function                                           AstroMap Description: Project coordinates (longitude and latitude) using the Stereographic projection. This is a map projection in which great circles and Loxodromes


    
      
    pr_stereographic function                                           AstroMap  
    Description: Project coordinates (longitude and latitude) using the  
    Stereographic projection.  
    This is a map projection in which great circles and Loxodromes  
    are logarithmic spirals.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    - [Central_Longitude, Standard_Parallel] default [0 0]  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    August 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
### celestial.proj.pr_stereographic_polar

Project coordinates using the Stereographic polar projection Package: celestial.proj Description: Project coordinates (longitude and latitude) using the Stereographic polar projection. This projection preservs angles.


    
    Project coordinates using the Stereographic polar projection  
    Package: celestial.proj  
    Description: Project coordinates (longitude and latitude) using the  
    Stereographic polar projection.  
    This projection preservs angles.  
    Input  : - Vector of Azimuth, in radians.  
    - Vector of Zenith-distance, in radians.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                     Nov 2004  
    URL : http://weizmnann.ac.il/home/eofek/matlab/  
    Example: [X,Y]=celestial.proj.pr_stereographic_polar(1,1)  
    Reliable: 2  
      
### celestial.proj.pr_xy

X-Y projection (no transformation). Package: celestial.proj Description: Project coordinates (longitude and latitude) to X-Y projection (no transformation).


    
    X-Y projection (no transformation).  
    Package: celestial.proj  
    Description: Project coordinates (longitude and latitude) to X-Y  
    projection (no transformation).  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Scale radius, default is 1.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.2  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### celestial.proj.projectcoo

projectcoo function                                             AstroMap Description: Project coordinates from longitude and latitude to X/Y using a specified projection.


    
      
    projectcoo function                                             AstroMap  
    Description: Project coordinates from longitude and latitude  
    to X/Y using a specified projection.  
    Input  : - Vector of Longitude, in radians.  
    - Vector of Latitude, in radians.  
    - Projection Type  
    'a' - Aitoff (default)  
    'm' - Mollweide equal-area  
    'h' - Hammer  
    'p' - Parabolic  
    's' - Sinusoidal  
    'l' - Lambert equal-area cylindrical  
    'b' - Behrmann equal-area cylindrical  
    't' - Tristan Edwards cylindrical  
    'P' - Peters cylindrical  
    'G' - Gall Orthographic cylindrical  
    'B' - Balthasart cylindrical  
    'c' - General cylindrical, opt par 1 is [Stand_Long, Stand_Lat] (radians)  
    'C' - Cassini  
    'x' - XY projection, no transformation.  
    'r' - polar projection (from north pole).  
    'A' - Albers equal-area, Par 1 is CenCoo and 2 is for ParLat (radians)  
    'g' - Gnomonic nonconformal projection. Par1 is [Long_cen, Lat_cen]  
    'M' - Mercator projection. Par1 is long_cen.  
    'o' - Bonne projection. Par1 is [central_long, standard_parallel]  
    'S' - Stereographic projection. Par1 is [central_long, standard_parallel]  
    #.# - Conic projection. where #.# is height of apex.  
    - Radius scale parameters, default is 1.  
    - Optional parameters 1.  
    - Optional parameters 2.  
    Output : - Vector of X position  
    - Vector of Y position  
    Tested : Matlab 5.2  
    By : Eran O. Ofek                    Jul 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
