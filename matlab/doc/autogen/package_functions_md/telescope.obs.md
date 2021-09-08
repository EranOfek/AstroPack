# Package: telescope.obs


### telescope.obs.daily_observability

Observability plot fot a specific coordinates Package: telescope.obs Description: Plot the observability of a given object from a give location on Earth during one night.


    
    Observability plot fot a specific coordinates  
    Package: telescope.obs  
    Description: Plot the observability of a given object from a give location  
    on Earth during one night.  
    This program will plot the object Alt during the night,  
    along with the Sun/Moon alt and the excess in sky  
    brightness due to the Moon.  
    Input  : - Observer Geodetic position, [E-Long, N-Lat] in radians.  
    or give string to choose from list (see observatory_coo.m):  
    'Wise' - wise observatory        [  34.763, 30.596]/RAD.  
    'KPNO' - Keat Peak National obs. [-111.60 , 31.980]/RAD.  
    'Keck' - Keck observatory.       [-155.478, 19.828]/RAD.  
    'APO'  - Apache Point obs.       [-105.82 , 32.780]/RAD.  
    'CA'   - Calar-Alto obs.         [   2.546, 37.224]/RAD.  
    'MMT'  - MMT obs. (Mt. Hopkins)  [-110.885, 31.688]/RAD.  
    'Paran'- ESO Paranal obs.        [ -70.403,-24.625]/RAD.  
    'LaPal'- La Palma Island obs.    [ -18.881, 28.760]/RAD.  
    'SAO'  - Special Astrop. obs.    [  41.442, 43.653]/RAD.  
    'Palom'- Palomar observatory     [-116.863, 33.357]/RAD.  
    'AAO'  - Anglo-Australian obs.   [ 149.067,-31.277]/RAD.  
    'CTIO' - Cerro Tololo Inter-Am.  [ -70.815,-30.165]/RAD.  
    'ESO'  - ESO La Silla obs.       [ -70.730,-29.257]/RAD.  
    - Date [JD] or [D M Y].  
    - Object R.A. [H M S] or [radians].  
    - Object Dec. [Sign D M S] or [radians].  
    - Coordinates equinox, default is 2000.0  
    Output : - Structure of plotted parameters, including the following fields:  
    .JD   - Julian day.  
      
    Plot   : Object altitude and airmass during the night, along with  
    the excess in sky brightness, in the object position,  
    due to the Moon illumination.  
    Note   : Historically called: obj_obs_cond.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: telescope.obs.daily_observability('Wise',[27 8 2001],[18 0 0],[1 67 0 0]);  
    Reliable: 2  
      
### telescope.obs.keck_obs_limits

Rise/set time for object in Keck observatory given telescope limits. Package: telescope.obs Description: Given a date and object celestial positions, calculate the rise and set time of an object in Keck observatory, given


    
    Rise/set time for object in Keck observatory given telescope limits.  
    Package: telescope.obs  
    Description: Given a date and object celestial positions, calculate the  
    rise and set time of an object in Keck observatory, given  
    the Nasmyth mount limits.  
    Input  : - Observatory:  
    {'KeckI' | 'KeckII'}  
    - Date [D M Y] or [JD].  
    - List of target's RA [H M S], ['HH:MM:SS.S'] or [rad].  
    see convertdms.m for more details.  
    - List of target's Dec [Sign D M S], ['+DD:MM:SS.S'] or [rad].  
    see convertdms.m for more details.  
    Output : - Crossing the visibility limits [Rise, Transit, Set] in UT  
    fraction of day.  
    - Airmass at [Rise, Transit, Set].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Oct 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [RTS,AM]=keck_obs_limits('KeckI',[1 7 2011],[18 00 00],[-1 30 0 0])  
    Reliable: 2  
      
      
### telescope.obs.observatory_coo

Geodetic coordinates of selected observatories Package: telescope.obs observatory_coo function                                           ephem Description: Return geodetic coordinates of an observatory.


    
    Geodetic coordinates of selected observatories  
    Package: telescope.obs  
    observatory_coo function                                           ephem  
    Description: Return geodetic coordinates of an observatory.  
    Input  : - Observatory name:  
    'Wise' - wise observatory        [  34.763, 30.596]/RAD.  
    'Kraar'- Kraar observatory (WIS) [34.812711,31.908083]./RAD  
    'KPNO' - Keat Peak National obs. [-111.60 , 31.980]/RAD.  
    'Keck' - Keck observatory.       [-155.478, 19.828]/RAD.  
    'APO'  - Apache Point obs.       [-105.82 , 32.780]/RAD.  
    'CA'   - Calar-Alto obs.         [   2.546, 37.224]/RAD.  
    'MMT'  - MMT obs. (Mt. Hopkins)  [-110.885, 31.688]/RAD.  
    'Paran'- ESO Paranal obs.        [ -70.403,-24.625]/RAD.  
    'LaPal'- La Palma Island obs.    [ -18.881, 28.760]/RAD.  
    'SAO'  - Special Astrop. obs.    [  41.442, 43.653]/RAD.  
    'Palom'- Palomar observatory     [-116.863, 33.357]/RAD.  
    'AAO'  - Anglo-Australian obs.   [ 149.067,-31.277]/RAD.  
    'CTIO' - Cerro Tololo Inter-Am.  [ -70.815,-30.165]/RAD.  
    'ESO'  - ESO La Silla obs.       [ -70.730,-29.257]/RAD.  
    'Camp' - Las Campanas obs.       [ -70.700,-29.008]/RAD.  
    'Strom'- Mount Stromlo obs.      [ 149.008,-35.320]/RAD.  
    'Lowel'- Lowell obs. Flagstaff   [-111.665, 35.203]/RAD.  
    Output : - Observatory geodetic position [Longitude, Latitude]  
    in radians.  
    - Matrix of ID strings. Line per observatory.  
    - Matrix of obs. names strings. Line per observatory.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [ObsCoo]=telescope.obs.observatory_coo('ESO');  
    Reliable: 2  
      
### telescope.obs.obspl

GUI observations planner Package: telescope.obs Description: GUI Observation Planer. Plot Alt/Airmass and moon sky brightness as a function of time in night, or yearly


    
    GUI observations planner  
    Package: telescope.obs  
    Description: GUI Observation Planer. Plot Alt/Airmass and moon sky  
    brightness as a function of time in night, or yearly  
    visibility plot per celestial object.  
    Input  : not documented  
    Output : null  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Aug 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: telescope.obs.obspl;  
    Reliable: 2  
    -  
      
### telescope.obs.yearly_observability

Plot yearly observability chart Pacakge: telescope.obs Description: Plot a yearly observability chart for an object.


    
    Plot yearly observability chart  
    Pacakge: telescope.obs  
    Description: Plot a yearly observability chart for an object.  
    Input  : - Year (scalar).  
    - Matrix of objects coordinates: [[RA], [Dec]],  
    where [RA] is a column vector of R.A. in radians,  
    where [Dec] is a column vector of Dec. in radians.  
    If this matrix has 7 columns, then the 3 first  
    columns are taken as the R.A. [H M S],  
    and the next columns taken as the Dec. [Sign D M S].  
    One object per line.  
    - Observatory coordinates [East Long, North Lat] in radians,  
    or observatory name. For options of observatory names.:  
    see observatory_coo.m  
    Default is 'Wise'.  
    - East Time Zone [hours], default is 2.  
    - Minimum airmass lines [Airmass].  
    cuve for the times the object will cross (upper and lower),  
    the specified airmass.  
    If NaN, then no airmass line is plotted.  
    Default is NaN.  
    - Delta T (=TDT-UT1) in fraction of day.  
    Default is [1/1440].  
    Output : - The maximum (abs. value) of the Hour Angle [hours], for which  
    the object is found in the needed AirMass.  
    Plot   : Rise/set (red solid lines); 6/12/18/Twilight (red dotted lines);  
    midnight (green dotted); object crosses airmass (blue dashed)  
    See also: daily_observability.m, obspl.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Sep 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: telescope.obs.yearly_observability(2001,[10 0 0 -1 20 0 0],'Wise',2,2.5);  
    Bug    : telescope.obs.yearly_observability(2001,[10 0 0 +1 20 0 0],'SAO',0,2.5);  
    Reliable: 2  
      
