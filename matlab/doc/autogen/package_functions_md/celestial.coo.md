# Package: celestial.coo


### celestial.coo.aberration

Apply aberration of light to source position Package: celestial.coo Description: Calculate the position of a star corrected for aberration of light.


### celestial.coo.add_offset

Offset a position by angular distance and position angle Package: celestial.coo Description: Add an offset specified by angular distance and position angle to celestial coordinates.


### celestial.coo.airmass

Airmass from time and object and observer position Package: celestial.coo Description: Given the JD, object celestial coordinates, and observer Geodetic coordinates, calculating the airmass of the


### celestial.coo.alt2ha

Convert altitude and declnation to hour angle Package: celestial.coo Description: Given an object altitude and declination and the observer latitude, return the corresponding Hour Angle.


### celestial.coo.altha2dec

Convert altitude and hour angle to declination Package: celestial.coo Description: Given Altitude and Hour Angle of an object and the observer latitude, calculate the object Declination.


### celestial.coo.angle_in2pi

Convert an angle to the 0 to 2*pi range Package: celestial.coo Description: Convert an angle to the range 0 to 2.*pi.


### celestial.coo.area_sphere_polygon

Area of a polygon on a sphere Package: celestial.coo Description: Calculate the area of a polygon on a sphere, where the polygon sides are assumed to be great circles. If the polygon


### celestial.coo.azalt2hadec

Convert Az/Alt to HA/Dec Package: +celestial.coo


### celestial.coo.boundingCircle

fit the smallest-radius bounding circle to set of X, Y points


### celestial.coo.calc_pm

calc_pm function Description: Calculate the proper motion of a star from a set of measurments.


### celestial.coo.cel_annulus_area

Area within a celestial annulus Package: celestial.coo Description: Calculate the area within a celestial annulus defined by an inner radius and outer radius of two concentric small circles.


### celestial.coo.celestial_circ

Grid of coordinates on a small spherical circle Package: celestial.coo Description: Calculate grid of longitude and latitude of a small circle on the celestial sphere.


### celestial.coo.center2corners

Return field corners given its center and size Package: celestial.coo Description: Given a field/s center and size calculate the field/s corners.


### celestial.coo.coco

Convert between different coordinates (OBSOLETE: use convert_coo) Package: celestial.coo Description: General coordinate convertor. Convert/precess coordinate from/to Equatorial/galactic/Ecliptic


### celestial.coo.convert2equatorial

Convert coordinates/name to apparent equatorial coordinates. Package: celestial Description: Given a coordinates in some coordinate system or equinox, or an object name, convert it to euatorial coordinates that


### celestial.coo.convert_coo

Convert between different coordinates Package: celestial.coo Description: General coordinate convertor. Convert/precess coordinate from/to Equatorial/galactic/Ecliptic


### celestial.coo.convertdms

Convert between various representations of coordinates and time Package: celestial.coo Description: Convert between various representations of coordinates and time as sexagesimal coordinates, degrees and radians.


### celestial.coo.convertdms1

convertdms1 function                                               ephem Description:


### celestial.coo.coo2box

Calculate box vertices around coordinates (OBSOLETE: use coo2box) Package: celestial Description: Given a list of RA/Dec coordinates, and box half size, calculate the approximnate positions of the box vertices


### celestial.coo.coo2cosined

Coordinates to cosine directions Package: celestial.coo Description: Convert coordinates to cosine directions in the same reference frame. See also: cosined.m, cosined2coo.m


### celestial.coo.coo_resolver

Resolve coordinates or target name into RA/Dec Package: celestial Description: Given coordinates (Lon/Lat) in any coordinate system or format, or a target name convert the coordinates into


### celestial.coo.cosined

Convert between coordinates and cosine directions Package: celestial.coo Description: Cosine direction transformation. Convert longitude and latitude to cosine direction and visa versa.


### celestial.coo.cosined2coo

Cosine direction to coordinates Package: celestial.coo Description: Convert cosine directions to coordinates in the same reference frame. See also: cosined.m, coo2cosined.m


### celestial.coo.dome_az

dome_az function                                                   ephem Description:


### celestial.coo.ecliptic2helioecliptic

Ecliptic longitude to Helio-ecliptic longitude Package: celestial.coo Description: Transform ecliptic longitude to Helio-ecliptic longitude.


### celestial.coo.fit_proper_motion

SHORT DESCRIPTION HERE Package: celestial Description:


### celestial.coo.fit_scircle

Example: celestial.coo.fit_scircle


### celestial.coo.geocentric2lsr

Geocentric or heliocentric velocity to velocity relative to the LSR Package: celestial.coo Description: Approximate conversion of geocentric or heliocentric velocity to velocity relative to the local standard of


### celestial.coo.get_skytile_coo

- get_skytile_coo function                                    Catalogue Description: Assuming some sky tileing (see tile_the_sky.m) and optional sub tileing for each tile, search for all the


### celestial.coo.ha2alt

Hour angle to altitude and airmass Package: celestial.coo Description: Given Hour Angle as measured from the meridian, the source declination and the observer Geodetic latitude, calculate


### celestial.coo.ha2az

Convert hour angle and declination to azimuth, altitude and airmass Package: celestial.coo Description: Given Hour Angle as measured from the meridian, the source declination and the observer Geodetic latitude, calculate


### celestial.coo.hadec2azalt

Convert HA/Dec to Az/Alt Package: +celestial.coo


### celestial.coo.hardie

The Hardie airmass formula Package: celestial.coo Description: Calculate airmass using the Hardie formula.


### celestial.coo.hardie_inv

Convert hardie airmass to altitude Package: celestial.coo Description: Inverse Hardie airmass function. Convert airmass to zenith distance.


### celestial.coo.horiz_coo

Celestial equatorial coordinates to horizontal coordinates Package: celestial.coo Description: Convert Right Ascension and Declination to horizontal coordinates or visa versa.


### celestial.coo.in_box

Check if celestial coordinates are in a box (approximate). Package: celestial Description: Check if celestial coordinates are in a box defined by four corners and its sides are great circles.


### celestial.coo.inside_celestial_box

Check if coorduinates are within box Package: celestial.coo Description: Given a list of celestial coordinates, and a box center, width and height, where the box sides are parallel to the


### celestial.coo.interp_coo

Interpolate celestial coordinates as a function of time Package: celestial.coo Description: Interpolate on celestial ccordinates as a function of time. Use the built in matlab interpolation functions.


### celestial.coo.is_coordinate_ok

Check that coordinates satisfy some observability conditions Package: celestial.coo Description: Check that J2000 equatorial coordinates satisfy some observability conditions including Az, Alt, HA.


### celestial.coo.light_abberation

light_abberation function                                          ephem Description: Given an object observer-centric direction, corrected for light deflection in the natural frame (P1),


### celestial.coo.light_deflection

light_deflection function                                              ephem Description: Calculate the observer-centric direction of a planet, corrected for light deflection in the natural frame.


### celestial.coo.nearest_coo

nearest_coo function                                                   ephem Description: Given a list of coordinates (with arbitrary number of dimensions), search for the coordinate in list which is


### celestial.coo.nutation

Intermidiate accuracy IAU 1984 nutation Package: celestial.coo Description: Calculate the Nutation in longitude and latitude, and the nutation rotation matrix.


### celestial.coo.nutation1980

nutation1984 function                                              ephem Description: Calculate the IAU 1980 Nutation series for a set of JDs.


### celestial.coo.nutation2rotmat

nutation2rotmat function                                           ephem Description: Given nutation in longitude and obliquity (in radians) and JD, return the Nutation rotation matrix.


### celestial.coo.nutation_lowacc

nutation_lowacc function                                           ephem Description: Low accuracy (~1") calculation of the nutation.


### celestial.coo.obliquity

Calculate the obliquity of the Earth ecliptic. Package: celestial.coo Description: Calculate the obliquity of ecliptic, with respect to the mean equator of date, for a given julian day.


### celestial.coo.parallactic2ha

Convert parallactic angle and declinatio to hour angle Package: celestial.coo Description: Convert parallactic angle, declination and latitude to hour angle. Note that there are two solutions, and the


### celestial.coo.parallactic_angle

parallactic_angle function                                             ephem Description: Calculate the parallactic angle of an object. The parallactic is defined as the angle between the local


### celestial.coo.pm2space_motion

pm2space_motion function                                           ephem Description: Convert proper motion, radial velocity and parralax to space motion vector in the equatorial system.


### celestial.coo.pm_eq2gal

SHORT DESCRIPTION HERE Package: celestial Description:


### celestial.coo.pm_vector

pm_vector function                                                 ephem Description: Return the space motion vector given proper motion, parallax and radial velocity.


### celestial.coo.polar_alignment

Calculate the RA/Dec drift due to equatorial polar alignemnt error. Package: celestial Description: Given a set of Declination-drift observations over several hour angle and declinations, calculate the deviation of the


### celestial.coo.polar_alignment_drift

Calculate the RA/Dec drift due to equatorial polar alignemnt error. Package: celestial Description:


### celestial.coo.polar_alignment_tracking_error

Package: celestial.coo Description:


### celestial.coo.pole_from2points

Find pole of a great circle defined by two points on the sphere. Package: celestial.coo Description: Given two points on the celestial sphere (in any system) describing the equator of a coordinate system,


### celestial.coo.precession

Calculate the Earth precession parameters Package: celestial.coo Description: Calculate the Earth precssion parameters as a function of JD.


### celestial.coo.proper_motion

Applay proper motion to a catalog Package: celestial.coo Description: Applay proper motion to a catalog


### celestial.coo.proper_motion_parallax

Applay proper motion and parallax to a catalog Package: celestial.coo Description: Applay proper motion to a catalog


### celestial.coo.refraction

Estimate atmospheric refraction, in visible light. Package: celestial.coo Description: Estimate atmospheric refraction, in visible light.


### celestial.coo.refraction_coocor

Atmospheric refraction correction for equatorial coordinates. Package: celestial.coo Description: Calculate the correction in equatorial coordinates due to atmospheric refraction.


### celestial.coo.refraction_wave

refraction_wave function                                           ephem Description: Calculate the wavelength-dependent atmospheric refraction and index of refraction based on Cox (1999) formula.


### celestial.coo.rotm_coo

Rotation matrix for coordinate conversion Package: celestial.coo Description: Generate a rotation matrix for coordinate conversion and precession.


### celestial.coo.sky_area_above_am

Calculate sky area observable during the night above a specific airmass. Package: celestial.coo Description: Calculate sky area observable during the night above a specific airmass, and assuming each field is observable


### celestial.coo.solve_alignment6dof_problem




### celestial.coo.sphere_dist

angular distance and position angle between two points on the sphere Package: celestial.coo Description: Calculate the angular distance and position angle between two points on the celestial sphere.


### celestial.coo.sphere_dist_cosd

Angular distance between a set of two cosine vector directions. Package: celestial.coo Description: Calculate the angular distance between a set of two cosine vector directions.


### celestial.coo.sphere_dist_fast

sphere_dist_fast function                                          ephem Description: Calculate the angular distance between two points on the celestial sphere. See sphere_dist.m (and built in distance.m)


### celestial.coo.sphere_dist_fast_thresh

sphere_dist_fast_thresh function                                   ephem Description: Calculate the angular distance between two points on the celestial sphere. See sphere_dist.m (and built in distance.m)


### celestial.coo.sphere_dist_thresh

sphere_dist_thresh function                                        ephem Description: Given Long and Lat coordinates and a reference coordinates (in radians) return a flag indicating if each point is


### celestial.coo.sphere_move

Applay offset to RA and Dec Package: +celestial.coo


### celestial.coo.sphere_offset

sphere_offset function                                             ephem Description: Calculate the offset needed to move from a point on the celesial sphere to a second point on the celestial sphere,


### celestial.coo.spherical_tri_area

spherical_tri_area function                                         AstroMap Description: Given three coordinates on a sphere, calculate the area of a spherical triangle defined by these three points.


### celestial.coo.spherical_triangle_circum_circle

Calculate the radius of the circum circle of a spherical triangle Package: celestial.coo


### celestial.coo.spherical_triangle_inscribed_circle

Calculate the radius of the inscribed circle of a spherical triangle Package: celestial.coo


### celestial.coo.star_conjunctions

Calculate conjuctions between stars given their proper motion. Package: celestial.coo Description: Given a star with its coordinates, proper motion and optionally parallax and radial velocity, and a list of


### celestial.coo.star_conjunctions_montecarlo

SHORT DESCRIPTION HERE Package: celestial Description:


### celestial.coo.tile_the_sky

Tile the celestial sphere Package: celestial.coo Description: Tiling the celestial sphere with approximately equal area tiles.


### celestial.coo.topocentric_vec

topocentric_vec function                                           ephem Description: Calculate the topocentric position and velocity vectors of an observer, with respect to the true equator and


