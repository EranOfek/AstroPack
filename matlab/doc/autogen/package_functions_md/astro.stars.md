# Package: astro.stars


### astro.stars.ang_radius_from_color

Estimate angular radius and color temperature from a set of magnitudes Package: +AstroUtil.stars


    
    Estimate angular radius and color temperature from a set of magnitudes  
    Package: +AstroUtil.stars  
    Input  : - A matrix of magnitudes in several bands. Source per line.  
    - A cell array of filter family names for the columns in the  
    marix of magnitudes.  
    - A cell array of band names.  
    - A cell array of mag sys. types.  
    Output : - ANgular radius ["]  
    - Temperature [K].  
    Example:  
    MagMat=[15.886 16.465 15.168];  
    FamilyCell={'GAIA','GAIA','GAIA'};  
    BandCell={'Bp','Rp','G'};  
    SystemCell={'Vega','Vega','Vega'};  
    [AR,T]=AstroUtil.stars.ang_radius_from_color(MagMat,FamilyCell,BandCell,SystemCell)  
      
      
### astro.stars.ang_radius_from_temp

Calculate the angular size of a star given its mag, extinction and temp Package: +AstroUtil.stars


    
    Calculate the angular size of a star given its mag, extinction and temp  
    Package: +AstroUtil.stars  
    Input  : - Apparent Magnitude  
    - Effective temperature  
    - Extinction in band. Default is 0.  
    - Filter family. Default is 'GAIA'.  
    - Filter name. Default is 'G'.  
    - Magnitude system. Default is 'Vega'.  
    Output : - Angular radius of star [arcsec]  
    Example: [Cat,ColCell]=catsHTM.cone_search('GAIADR2',0,0,2031.1);  
    AngRad=AstroUtil.stars.ang_radius_from_temp(Cat(:,16),Cat(:,24),Cat(:,27))  
      
### astro.stars.equipot

Calculate the gravitational potential of a binary star on a grid. Package: AstroUtil.stars Description: Calculate two body equipotanials map.


    
    Calculate the gravitational potential of a binary star on a grid.  
    Package: AstroUtil.stars  
    Description: Calculate two body equipotanials map.  
    Input  : - m1 : first star mass in solar mass.  
    - m2 : second star mass in solar mass.  
    - a : the separation between the two stars. in meters.  
    - s : scaling the result from min to max in units of  
    the staller separation.  
    - n : number of point in each axis. default is 15.  
    - z : the surface to work on. default is z=0 (orbital plane)  
    Output : - grid of x coordinate.  
    - grid of y coordinate.  
    - matrix of potential defined by the x/y grids.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [x,y,q]=AstroUtil.stars.equipot(1,0.7,0.7.*1e9,3,50);  
    mesh(x,y,q);  
    -  
      
### astro.stars.star_ang_rad

Calculate empirical angular radii of stars from magnitude and colors. Package: AstroUtil.stars Description: Empirical angular radii of stars based on their magnitude and colors.


    
    Calculate empirical angular radii of stars from magnitude and colors.  
    Package: AstroUtil.stars  
    Description: Empirical angular radii of stars based on their magnitude  
    and colors.  
    Input  : - Magnitude.  
    - Color.  
    - Band in which magnitude is provided. Default is 'g'.  
    - Color name in which color is provided. Default is 'g-r'.  
    Output : - Angular radii of stars [mas].  
    - Flag indicating if angular radii is in range (1) or out of  
    range (0) and therefore unreliable.  
    Reference: http://arxiv.org/abs/1311.4901  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Theta,FlagInRange]=AstroUtil.stars.star_ang_rad(10,0.4,'g','g-i');  
    Reliable: 2  
      
      
      
    http://arxiv.org/abs/1311.4901  
### astro.stars.star_sptype_color

Spectral type to color Package: AstroUtil.star Description: Given a star spectral type and luminosity class, get the star color between any two filters.


    
    Spectral type to color  
    Package: AstroUtil.star  
    Description: Given a star spectral type and luminosity class, get the star  
    color between any two filters.  
    Input  : - Spectral type (e.g., 'A4').  
    - Luminosity class (e.g., 'V').  
    - The familiy name for the first filter (e.g., 'SDSS').  
    - The filter name for the first filter (e.g., 'g').  
    - Magnitude system for the first filter (e.g., 'AB').  
    - The familiy name for the second filter (e.g., 'SDSS').  
    - The filter name for the second filter (e.g., 'r').  
    - Magnitude system for the second filter (e.g., 'AB').  
    Output : - Color.  
    - Rough interpolation error.  
    Tested : Matlab 7.13  
    By : Eran O. Ofek                    Oct 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [C,E]=AstroUtil.stars.star_sptype_color('A3','IV','SDSS','g','AB','SDSS','r','AB');  
    [C,E]=AstroUtil.stars.star_sptype_color('A3','IV','Johnson','B','Vega','Johnson','V','Vega');  
    Reliable: 2  
      
      
      
### astro.stars.stellar_imf

The stellar initial mass function Package: AstroUtil.stars Description: Return the stellar initial mass function in a given mass range.


    
    The stellar initial mass function  
    Package: AstroUtil.stars  
    Description: Return the stellar initial mass function in a given  
    mass range.  
    Input  : - Vector of mass [solar mass].  
    - Intial mass function type:  
    'Salpeter'  - Salpeter (1955)  
    'Scalo'     - Scalo (1986)  
    'SM1979'    - Scalo & Miller (1979)  
    Or alternativel, a matrix containing the IMF:  
    [M1 M2 Power_Law_Index; M1 M2 ...]  
    Output : - Vector of dN/dM for each mass.  
    - Cumulative number of stars from 0 to Mass.  
    - Total fraction of number of stars within the range  
    min(Mass) to max(Mass), relative to the total mass in  
    the 0..Inf range.  
    - Cumulative mass of stars from 0 to Mass.  
    - Total fraction of stellar mass within the range  
    min(Mass) to max(Mass), relative to the total mass in  
    the 0..Inf range.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Apr 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [DnDm,CumFracN,TotalFracN,CumFracM,TotalFracM]=AstroUtil.stars.stellar_imf(5,'Salpeter');  
    Reliable: 2  
      
### astro.stars.stellar_tracks

Geneva stellar tracks as a function of time. Package: AstroUtil.stars Description: Given an initial mass and metllicity return the Geneva stellar tracks as a function of time.


    
    Geneva stellar tracks as a function of time.  
    Package: AstroUtil.stars  
    Description: Given an initial mass and metllicity return the Geneva  
    stellar tracks as a function of time.  
    Input  : - Initial mass [solar mass].  
    Can be one of the followings:  
    [0.4 | 0.5 | 0.6 | 0.7 | 0.8 | 0.9 | 1.0 |  
    1.25 | 1.5 | 1.7 | 2 | 2.5 | 3 | 4 | 5 | 7 | 9 | 10 |  
    12 | 15 | 20 | 25 | 40 | 60 | 85 | 120].  
    - Metallicity [mass fraction].  
    Can be one of the followings:  
    [0.0004 | 0.001 | 0.004 | 0.008 | 0.02 | 0.04 | 0.1]  
    - Model type ["c" | "e" | "l" | "m" | "p" | "o"],  
    see table1.dat for details. Default is "c".  
    Output : - Evolution structure, contains the following fields:  
    .Number - grid point number  
    .Age    - age [yr]  
    .Mass   - actual mass [solar mass]  
    .LogL   - log(Luminosity) [log(solar lum)]  
    .LogT   - log(Effective Temp.) [log(K)]  
    .H; .He; .C12; C13; .N14; .O16; .O17; .O18; .Ne20; .Ne22  
    - surface abundance [mass fraction] of these elements.  
    .CoreT  - Stellar core temperature for Wolf-Rayet stars [K].  
    .ML     - Mass loss rate [solMass/yr].  
    - UBV structure, contains the following fields:  
    .Number - grid point number  
    .Age    - age [yr]  
    .Mass   - actual mass [solar mass]  
    .LogL   - log(Luminosity) [log(solar lum)]  
    .LogT   - log(Effective Temp.) [log(K)]  
    .Logg   - log(Surface gravity) [log(cm/s^2)]  
    .U; .B; .V; .R; .I; .J; .H; .K; .L; .Lt; .M  
    - Abs. magnitude in the respective bands.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Feb 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Ev,UBV]=AstroUtil.stars.stellar_tracks(1,0.04,'c');  
    Reliable: 2  
      
