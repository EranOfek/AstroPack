# Package: celestial.Kepler


### celestial.Kepler.apsides_precession

First order estimation of the GR precession of the line of apsides Package: celestial.Kepler Description: First order estimation of the GR precession of the line of apsides.


    
    First order estimation of the GR precession of the line of apsides  
    Package: celestial.Kepler  
    Description: First order estimation of the GR precession of the line  
    of apsides.  
    Input  : - Central object mass [gram].  
    - Semi major axis [cm].  
    - Eccentricity.  
    Output : - apsidel motion in radians per orbit.  
    Reference: Danby 1989, Celestial Mechanics, p.68  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2002  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: DW=celestial.Kepler.apsides_precession(2e33,1e11,0.1)  
    Reliable: 2  
      
      
### celestial.Kepler.dnu_dt

Calculate dnu/dt and dr/dt for elliptical orbit Package: celestial.Kepler Description: Calculate dnu/dt and dr/dt for elliptical orbit using the Kepler Equation.


    
    Calculate dnu/dt and dr/dt for elliptical orbit  
    Package: celestial.Kepler  
    Description: Calculate dnu/dt and dr/dt for elliptical orbit using  
    the Kepler Equation.  
    Input  : - Mean motion [rad/day].  
    - Eccentricity.  
    - Semi major axis [au].  
    - Eccentric anomaly [rad].  
    Output : - dnu/dt - the time derivative of the the true anomaly  
    [rad/day].  
    - dr/dt - the time derivative of the radius vector [au/day].  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [dnudt,drdt]=celestial.Kepler.dnu_dt(0.017,0.2,1,0)  
    Reliable: 2  
      
      
### celestial.Kepler.eccentric2true_anomaly

Convert Eccentric anomaly to true anomaly Package: celestial.Kepler Description: Convert Eccentric anomaly to true anomaly.


    
    Convert Eccentric anomaly to true anomaly  
    Package: celestial.Kepler  
    Description: Convert Eccentric anomaly to true anomaly.  
    Input  : - Eccentric anomaly [radians].  
    - Eccentricity.  
    Output : - True anomaly [radians].  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Dec 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Nu=celestial.Kepler.eccentric2true_anomaly(1,0.1)  
    Reliable: 2  
      
      
### celestial.Kepler.elements2position

Convert orbital elements and date to position and velocity vectors. Package: celestial.Kepler Description: Convert orbital elements and date to position vector and velocity vector at a give epoch.


    
    Convert orbital elements and date to position and velocity vectors.  
    Package: celestial.Kepler  
    Description: Convert orbital elements and date to position vector and  
    velocity vector at a give epoch.  
    Input  : - Time of observations, one time per row.  
    If one column is given then taken as JD.  
    if four colum is given then taken as [D M Y frac_day].  
    - Orbital elements : [q e T i \Omega \omega], in au, days, radians.  
    - Gaussian gravitational constant.  
    Default is: 0.017202098950000  
    - GM [au^3 day^-2]. Default is: 2.959122084294439e-004  
    Output : - Position matrix, [X;Y;Z] per column.  
    in au relative to ecliptic and equinox.  
    - Velocity matrix, [X;Y;Z] per column.  
    in au/day relative to ecliptic and equinox.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.Kepler.elements2position(2451545,[1 0.1 2451545 0 0 0])  
    Reliable: 2  
      
      
### celestial.Kepler.elements_1950to2000

B1950.0 FK4 orbital elements to J2000.0 FK5 Package: celestial.Kepler Description: Convert solar system orbital elements given in the B1950.0 FK4 reference frame to the J2000.0 FK5 reference frame.


    
    B1950.0 FK4 orbital elements to J2000.0 FK5  
    Package: celestial.Kepler  
    Description: Convert solar system orbital elements given in the B1950.0  
    FK4 reference frame to the J2000.0 FK5 reference frame.  
    Input  : - B1950.0 Orbital inclination [rad].  
    - B1950.0 Longitude of asecnding node [rad].  
    - B1950.0 Longitude of periastron [rad].  
    Output : - J2000.0 Orbital inclination [rad].  
    - J2000.0 Longitude of asecnding node [rad].  
    - J2000.0 Longitude of periastron [rad].  
    Reference: Seidelmann (1992), p. 314  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [IN,OmegaN,omegaN]=celestial.Kepler.elements_1950to2000(1,1,1)  
    Reliable: 2  
      
      
      
### celestial.Kepler.gauss_grav_const

Gaussian gravitational constant for a system Package: celestial.Kepler Description: Get the analog of the Gaussian gravitational constant for a system with a given primary mass, secondary mass and


    
    Gaussian gravitational constant for a system  
    Package: celestial.Kepler  
    Description: Get the analog of the Gaussian gravitational constant for  
    a system with a given primary mass, secondary mass and  
    unit distance. This program is useful in order to apply  
    kepler.m for non-solar system cases.  
    Input  : - Secondary mass [solar mass units], default is 0.  
    - Primary mass [solar mass units], default is 1.  
    - Unit distance in [au], default is 1.  
    - Unit time in [day=86400 SI sec], default is 1.  
    Output : - The equivalent of the Gaussian gravitational constant for  
    the chosen system. By default (K=gauss_grav_const), the  
    program return the Gaussian gravitational constant  
    (i.e., 0.017202098950000).  
    See also : get_constant.m, kepler.m  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: K=celestial.Kepler.gauss_grav_const(0,1,1,1);  get the Gaussian gravitational constant  
    Reliable: 2  
      
### celestial.Kepler.kepler3law

Kepler 3rd law Package: celestial.Kepler Description: Calculate the velocity, semi-major axis and period of a system using the Kepler third law.


    
    Kepler 3rd law  
    Package: celestial.Kepler  
    Description: Calculate the velocity, semi-major axis and period of  
    a system using the Kepler third law.  
    Input  : - System mass [gr].  
    - Type of given property:  
    'p' - period [s].  
    'a' - semi-major axis [cm].  
    'v' - velocity [cm/s].  
    - Propery value.  
    Output : - A structure containing the following fields:  
    .p  - period [s].  
    .a  - semi-major axis [cm].  
    .v  - velocity [cm/s].  
    Tested : Matlab R2012a  
    By : Eran O. Ofek                    Nov 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=celestial.Kepler.kepler3law(1.9891e33,'a',1.5e13);  
    Reliable: 1  
    -  
      
### celestial.Kepler.kepler_elliptic

Solve Kepler equation for elliptic orbit Package: celestial.Kepler Description: Solve Kepler equation (M = E - e sin E) and find the true anomaly and radius vector for elliptical orbit (i.e., 0<=e<1).


    
    Solve Kepler equation for elliptic orbit  
    Package: celestial.Kepler  
    Description: Solve Kepler equation (M = E - e sin E) and find the true  
    anomaly and radius vector for elliptical orbit (i.e., 0<=e<1).  
    The function requires the time since periastron (t-T), the  
    periastron distance (q) and the Gaussian constant for the  
    system and units (k; see gauss_grav_const.m).  
    Input  : - Time since periastron (t-T) in units specified by chosen  
    Gaussian gravitational constant (default is days).  
    If the Gaussian gravitational constant is NaN, then assumes  
    this is the mean anomaly.  
    - Periastron distance (q) in units specified by chosen  
    Gaussian gravitational constant (default is au).  
    The periastron distance is related to the semi major  
    major axis (a), through q=a*(1-e).  
    - Orbital eccentricity.  
    - [K,M] Gaussian constant for the system (k) and Mass [SolarMass].  
    Default is :  
    k=0.017202098950000 , M=1 [SolarMass];  
    (appropriate for the solar system;  
    negligible mass object orbiting the Sun; time units  
    are days and distance units are au). See gauss_grav_const.m  
    If NaN, then use the first argument as the mean anomaly,  
    instead of time since periastron.  
    - Tolerance, default is 1e-8.  
    Output : - True anomaly [radians].  
    - Radius vector in units of distance, for the default case units  
    are au.  
    - Eccentric anomaly [radians].  
    - Orbital velocity [distance units per time units], for default  
    Gaussian gravitational constant this is [au/day].  
    - Velocity is not defined if Gaussian grav. constant is NaN.  
    - The mean anomaly [radians].  
    Notes  : The mean anomaly (M) is related to the time since periastron  
    and q through: M = n*(t-T), where the mean motion (n) is  
    given by: k*a^(3/2), where a=q/(1-e) and k is the Gaussian  
    gravitational constant.  
    See also: kepler_parabolic.m, kepler_hyperbolic.m, gauss_grav_const.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Nu,R,E]=celestial.Kepler.kepler_elliptic(10,0.5,0.96);  
    [Nu,R,E]=celestial.Kepler.kepler_elliptic(1, 0.5,0.96,NaN);  
    Reliable: 2  
      
### celestial.Kepler.kepler_elliptic_fast

Solve Kepler equatin (fast version) Package: celestial.Kepler Description: Fast solver for the Kepler equation for elliptic orbit. This is a simpler version of kepler_elliptic.m


    
    Solve Kepler equatin (fast version)  
    Package: celestial.Kepler  
    Description: Fast solver for the Kepler equation for elliptic orbit.  
    This is a simpler version of kepler_elliptic.m  
    Input  : - Vector of Mean anomaly [radians].  
    - Vecor of Eccentric anomaly [radians].  
    - Tolerance (e.g., default is 1e-8).  
    Output : - Eccentric anomaly.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Dec 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    see alos: kepler_elliptic.m  
    Example: M=rand(1e6,1).*2.*pi; e=rand(1e6,1);  
    tic;E=celestial.Kepler.kepler_elliptic_fast(M,e,1e-8);toc  
    Reliable: 2  
      
      
### celestial.Kepler.kepler_hyperbolic

SOlve Kepler equation for hyperpolic orbit Package: celestial.Kepler Description: Solve Kepler equation (M = e sinh H - H) and find the true anomaly and radius vector for hyperbolic orbit (i.e., e>1).


    
    SOlve Kepler equation for hyperpolic orbit  
    Package: celestial.Kepler  
    Description: Solve Kepler equation (M = e sinh H - H) and find the true  
    anomaly and radius vector for hyperbolic orbit (i.e., e>1).  
    The function requires the time since periastron (t-T), the  
    periastron distance (q) and the Gaussian constant for the  
    system and units (k; see gauss_grav_const.m).  
    Input  : - Time since periastron (t-T) in units specified by chosen  
    Gaussian gravitational constant (default is days).  
    - Periastron distance (q) in units specified by chosen  
    Gaussian gravitational constant (default is au).  
    - Orbital eccentricity.  
    - Gaussian constant for the system (k).  
    Default is : 0.017202098950000 (appropriate for the solar  
    system; negligible mass object orbiting the Sun; time units  
    are days and distance units are au). See gauss_grav_const.m  
    - Tolerance, default is 1e-8.  
    Output : - True anomaly [radians].  
    - Radius vector in units of distance, for the default case units  
    are au.  
    - Hyperbolic eccentric anomaly, H [radians].  
    - Orbital velocity [distance units per time units], for default  
    Gaussian gravitational constant this is [au/day].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Nu,R,H,Vel]=celestial.Kepler.kepler_hyperbolic(1,1,1.1);  
    Reliable: 2  
      
### celestial.Kepler.kepler_lowecc

A low eccentricity serise solution for the Kepler equation Package: celestial.Kepler Description: Solve the Kepler Equation for low eccentricity using a series approximation. This is typically better than 1"


    
    A low eccentricity serise solution for the Kepler equation  
    Package: celestial.Kepler  
    Description: Solve the Kepler Equation for low eccentricity using  
    a series approximation. This is typically better than 1"  
    for e<0.1.  
    Input  : - Mean anomaly [rad].  
    - Eccentricity (e).  
    - Periastron distance (q).  
    Output : - True anomaly [rad].  
    Reference: Meeus (1991)  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jul 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Nu]=celestial.Kepler.kepler_lowecc(1,0.01)  
    [Nu,R]=celestial.Kepler.kepler_lowecc(1,0.01,1);  
    Reliable: 2  
      
      
    Nu = M + 2.*e.*sin(M) + 5./4.*e.^2.*sin(2.*M) + ...  
    e.^3./12 .*(13.*sin(3.*M)-3.*sin(M)) +...  
    e.^4./96.*(103.*sin(4.*M) - 44.*sin(2.*M));  
      
### celestial.Kepler.kepler_parabolic

Solve the Kepler equation for Parabolic orbit Package: celestial.Kepler Description: Solve Kepler equation (M = E - e sin E) and find the true anomaly (related to the eccentric anomaly, E) and radius


    
    Solve the Kepler equation for Parabolic orbit  
    Package: celestial.Kepler  
    Description: Solve Kepler equation (M = E - e sin E) and find the true  
    anomaly (related to the eccentric anomaly, E) and radius  
    vector for parabolic orbit (i.e., e=1). The function  
    requires the time since periastron (t-T), the periastron  
    distance (q) and the Gaussian constant for the system and  
    units (k; see gauss_grav_const.m).  
    Input  : - Time since periastron (t-T) in units specified by chosen  
    Gaussian gravitational constant (default is days).  
    - Periastron distance (q) in units specified by chosen  
    Gaussian gravitational constant (default is au).  
    - Gaussian constant for the system (k).  
    Default is : 0.017202098950000 (appropriate for the solar  
    system; negligible mass object orbiting the Sun; time units  
    are days and distance units are au). See gauss_grav_const.m  
    Output : - True anomaly [radians].  
    - Radius vector in units of distance, for the default case units  
    are au.  
    - S (=2qS^2 = D^2) [radians].  
    - Orbital velocity [distance units per time units], for default  
    Gaussian gravitational constant this is [au/day].  
    See also: kepler_elliptic.m, kepler_hyperbolic.m, kepler.m,  
    gauss_grav_const.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Nu,R,S]=celestial.Kepler.kepler_parabolic([7.0;7.8],0.1);  
    Reliable: 2  
      
### celestial.Kepler.position2elements

Convert position/velocity vectors to orbital elements. Package: celestial.Kepler Description: Convert date, position vector and velocity vector at a give ephoch to orbital elements at the same epoch.


    
    Convert position/velocity vectors to orbital elements.  
    Package: celestial.Kepler  
    Description: Convert date, position vector and velocity vector at a give  
    ephoch to orbital elements at the same epoch.  
    Input  : - Time of observations, one time per raw.  
    If one column is given then taken as JD.  
    if four colum is given then taken as [D M Y frac_day].  
    - Position matrix, [X;Y;Z] per column.  
    in au relative to ecliptic and equinox.  
    - Velocity matrix, [X;Y;Z] per column.  
    in au/day relative to ecliptic and equinox.  
    - Gaussian gravitational constant.  
    Default is: 0.017202098950000  
    - GM [au^3 day^-2]. Default is: 2.959122084294439e-004  
    Output : - [q, e, T, i, \Omega, \omega], distance in au, time in days,  
    angles in radians.  
    - [a, \nu, EccentricAnomaly, MeanAnomaly, MeanLongitude, n], distance  
    in au, time in days, angles in radians.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: NOT READY  
      
      
### celestial.Kepler.thiele_innes

Calculate the Thiele-Innes orbital elements Package: celestial.Kepler Description: Calculate the Thiele-Innes orbital elements.


    
    Calculate the Thiele-Innes orbital elements  
    Package: celestial.Kepler  
    Description: Calculate the Thiele-Innes orbital elements.  
    Input  : - a (semi major axis).  
    - omega (rad)  
    - Omega (rad)  
    - i (rad)  
    Output : - Structure containing the 6 Thiele-Innes elements  
    (fields are A, B, C, F, G, H).  
    See also: thiele_innes2el.m  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: TI=celestial.Kepler.thiele_innes(1,1,1,1);  
    Reliable: 2  
      
      
### celestial.Kepler.thiele_innes2el

Thiele-Innes to orbital elements Package: celestial.Kepler Description: Calculate the orbital elements from the Thiele-Innes orbital elements.


    
    Thiele-Innes to orbital elements  
    Package: celestial.Kepler  
    Description: Calculate the orbital elements from the Thiele-Innes  
    orbital elements.  
    Input  : - A or a structure with the A,B,C,F,G,H fields.  
    - F  
    - B  
    - G  
    - C (optional)  
    - H (optional)  
    Output : - Structure containing the 6 Thiele-Innes elements  
    (fields are A, B, C, F, G, H).  
    See also: thiele_innes2.m  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: TI=celestial.Kepler.thiele_innes(1,1,1,1);  
    El=celestial.Kepler.thiele_innes2el(TI);  
    Reliable: 2  
      
      
### celestial.Kepler.true2eccentric_anomaly

True anomaly to eccentric anomaly Package: celestial.Kepler Description: Convert true anomaly to eccentric anomaly for elliptic orbit.


    
    True anomaly to eccentric anomaly  
    Package: celestial.Kepler  
    Description: Convert true anomaly to eccentric anomaly for elliptic  
    orbit.  
    Input  : - True anomaly  
    - Eccentricity.  
    Output : - Eccentric anomaly.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Dec 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: E=celestial.Kepler.true2eccentric_anomaly(1,0.1);  
    Reliable: 2  
      
      
### celestial.Kepler.trueanom2pos

True anomaly, radius vector and orbital elements to cartezian position Package: celestial.Kepler Description: Given an object true anomaly, radius vector, time and orbital elements and time, calculate its orbital position


    
    True anomaly, radius vector and orbital elements to cartezian position  
    Package: celestial.Kepler  
    Description: Given an object true anomaly, radius vector, time and  
    orbital elements and time, calculate its orbital position  
    in respect to the orbital elements reference frame.  
    Input  : - Vector of radius vector, [length unit].  
    - Vector of True anomaly, [radians].  
    - Argument of Ascending node, [radians].  
    - Longitude of periastron, [radians].  
    - Inclination, [radians].  
    Output : * If one output argument is requested then the program returns  
    a single matrix containing the [X,Y,Z].  
    If three output arguments are requested them the program  
    returns the X, Y, Z position vectors in ecliptic coordinate  
    system.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: trueanom2vel.m  
    Example: [X,Y,Z] = celestial.Kepler.trueanom2pos(1,[0;1],1,1,1);  
    [XYZ] = celestial.Kepler.trueanom2pos(1,[0;1],1,1,1);  
    [XYZ] = celestial.Kepler.trueanom2pos([1;2],[0;1],1,1,1);  
    [XYZ] = celestial.Kepler.trueanom2pos([1;2],[0;1],1,1,[1;0.1]);  
    Reliable: 1  
      
      
### celestial.Kepler.trueanom2vel

True anomaly, radius vector and orbital elements to position and velocity Package: celestial.Kepler Description: Given an object true anomaly, radius vector, their derivatives and orbital elements and time, calculate its


    
    True anomaly, radius vector and orbital elements to position and velocity  
    Package: celestial.Kepler  
    Description: Given an object true anomaly, radius vector, their  
    derivatives and orbital elements and time, calculate its  
    orbital position and velocity in respect to the orbital  
    elements reference frame.  
    Input  : - Vector of radius vector, [length unit].  
    - Vector of True anomaly, [radians].  
    - Vector of radius vector derivative [radians/time].  
    - Vector of true anomaly derivative [radians/time].  
    - Argument of Ascending node, [radians].  
    - Longitude of periastron, [radians].  
    - Inclination, [radians].  
    Output : - X  
    - Y  
    - Z  
    - X dot  
    - Y dot  
    - Z dot  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: trueanom2pos.m  
    Example: [X,Y,Z,Xd,Yd,Zd] = celestial.Kepler.trueanom2vel(1,[0;1],0.1,0.1,1,1,1);  
    Reliable: 2  
      
