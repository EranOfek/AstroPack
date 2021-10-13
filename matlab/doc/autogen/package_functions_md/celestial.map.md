# Package: celestial.map


### celestial.map.amapproj

Old map projection function (not supported) Package: celestial.map Description: Plot a list of coordinates (as dots/lines) on a map with a chosen projection.


    
    Old map projection function (not supported)  
    Package: celestial.map  
    Description: Plot a list of coordinates (as dots/lines) on a map with  
    a chosen projection.  
    Input  : - matrix of coordinates.  
    [Longitude (rad), Latitude (rad), Magnitude, Spectral-type].  
    Magnitude and Spectral-type are optional.  
    In case that the "all sky map" option is selected, the  
    coordinates should be in range [-pi pi -pi/2 pi/2].  
    Magnitude cotrol the symbol size.  
    The spectral-type control the symbol color and it can be  
    one of the following:  
    1  -  Blueish [0 0.5 0.9]  
    2  -  Cyan  
    3  -  Green  
    4  -  Yellow  
    5  -  Orange [1 0.65 0]  
    6  -  Reddish    [1 0 0.5]  
    7  -  Black    [0 0 0]  
    8  -  Gray    [0.8 0.8 0.8]  
    9  -  Blcak  
    * In case of seven columns matrix, the columns  
    are assumed to be [H M S DecSign D M S]  
    - projection type:  
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
    'C' - Cassini  
    'x' - XY, no projection  
    'r' - polar  
    'g' - Gnomonic  
    'o' - Bonne  
    'S' - Stereographic  
    #.# - (number), conic projection with height #.#  
    - area vector  
    'a' - for all sky globe. (default) or  
    [Long_Min, Long_Max, Lat_Min, Lat_Max]  
    - object symbol and color  
    '.', 'o', '>', '<', 'd', '*', ...  
    default is '.'  
    - grid symbol and color  
    'N' - no grid.  
    default is 'r:'  
    - 3x3 rotation matrix of secondary grid lines.  
    default is no secondary grid. recomended only  
    with AreaVec='a'.  
    - Secondary grid format, default is 'r.'.  
    default is no secondary grid.  
    Output : - The projected X position of the data points.  
    - The projected Y position of the data points.  
    Tested : Matlab 5.2  
    By : Eran O. Ofek                    Aug 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: NOT WORKING ANYMORE  
      
### celestial.map.plot_monthly_smap

Plot a monthly sky map Package: celestial.map Description: Plot a monthly sky map with a naked eye stars for a given time and observer Geodetic position. Optionaly mark planets position, constellations and the milky way.


    
    Plot a monthly sky map  
    Package: celestial.map  
    Description: Plot a monthly sky map with a naked eye stars for a given  
    time and observer Geodetic position. Optionaly mark planets  
    position, constellations and the milky way.  
    Input  : - Date [D M Y H M S]  |  
    Date [D M Y Frac] |  
    JD.  
    * arbitrary number of keyword and properties, where keywods are:  
    'GeodPos'    - Geodetic poistion  
    [East Long, North Lat, Height(m)]  
    in radians, default is [35 32 0]./RAD.  
    'Cat'        - Star Catalog, default is mag6.mat  
    'ColRA'      - RA column in star catalog, default is 1.  
    'ColDec'     - RA column in star catalog, default is 2.  
    'ColMag'     - Mag column in star catalog, default is 3.  
    'ColCol'     - Color (B-V) column in star catalog,  
    default is 4.  
    'ColPMRA'    - PM RA*cos(dec) column in star catalog,  
    default is 5 [mas/yr].  
    'ColPMDec'   - PM Dec column in star catalog,  
    default is 6 [mas/yr].  
    'MW'         - Plot MilkyWay {'yes' | 'no'}, default is 'yes'.  
    'StarSizeScale'-Scale factor for stars size (scales linearly  
    'MagFun'), default is 0.7.  
    'MagFun'     - Two column matrix relating star mag (first  
    column) to symbol size (second column).  
    'StarType    - Star symbols {'black-edge' | 'same-edge'},  
    default is 'black-edge'.  
    'ConLines'   - Plot Constellation lines {'yes' | 'no'},  
    default is 'yes'.  
    'ConLinesColor'- Constellation lines color,  
    default is [0 0 1].  
    'ConLinesWidth'- Constellation lines width, default is 0.5.  
    'ConLabels'  - Constellation labels {'yes' | 'no'},  
    default is 'yes'.  
    'ConLabelsColor'- Constellations labels color, default is 'k'.  
    'ConLabelsOrient'- Constellations labels orientation:  
    {'south' | 'radial'}, default is 'south'.  
    'MagLimit'   - Mag limit, default is 5.  
    'PM'         - Applay proper motion to star catalog  
    {'yes' | 'no'}, default is 'yes'.  
    'Precess'    - Precess coordinates, {'yes' | 'no'},  
    default is 'yes'.  
    'Equinox'    - Star Catalog mean equinox in JD,  
    default is 2451545.5  
    'Epoch'      - Star Catalog epoch in JD, default is 2451545.5  
    'ColorBrightMW' - Color scheme for Milky way bright patches,  
    default is [0.8 0.8 0.8].  
    'ColorDarkMW' - Color scheme for Milky way dark patches,  
    default is [1 1 1].  
    'Planets'    - Show planets, the following options are available:  
    If string:  
    'no'/'off' - don't show planets - default.  
    'ALL' - show Mercury..Neptune + Moon & Sun.  
    'All' - show Mercury..Neptune.  
    'EYE' - Show Mecury..Saturn + Moon & Sun.  
    'Eye' - Show Mecury..Saturn.  
    Or vector of numbers, one number for each planets  
    according to the following codes:  
    1-Mercury; 2-Venus; 3-Moon; 4-Mars;  
    5-Jupiter; 6-Saturn; 7-Uranus; 8-Neptune;  
    0-Sun.  
    'PlJD'       - Vector of JD in which to show planets position  
    (e.g., multiple epochs), default is the single  
    epoch given in Date parameter.  
    'PlMarker'   - Planet marker type, default is 'o'.  
    'PlLine'     - Planet position connecting line style,  
    default is '-'.  
    'PlMarkSize' - Planet marker size, default is 'Mag'.  
    (if 'Mag' then size according to magnitude scheme  
    of stars.)  
    If 'Icon', then use single size planets icons.  
    'PlColor'    - Planets color, default is [0 0 0].  
    'MoonPhases' - Plot Moon phases calendar for current month,  
    {'y','n'}, default is 'n'.  
    'ObjectList' - Matrix of additional coordinates/objects to plot:  
    [RA, Dec, Equinox(JD), LineType, SymbolType,  
    Mag, SymbolSize,  
    SymbolColor(3columns), TailPA, TailLength].  
    where different set of objects may be entered  
    with a lines of NaNs seprating between them.  
    LineType   : 0-no line, 1-solid, 2-dashed,  
    3-dash-dot, 4-dpineotted  
    SymbolType : 0-no symbol, 1-'o', 2-'^', 3-'s',  
    4-'p', 5-'h',  
    6-comet, 7-meteor shower  
    Mag        : plot symbol size according to  
    magnitude.  
    SymbolSize : If symbol size not NaN then used  
    the value in  
    SymbolSize column instead of Mag.  
    SymbolColor: Three column vector of symbol color  
    TailPA     : Comet tail P.A. in radians.  
    TailLength : Comet tail length in radians.  
    'ColorOut'   - Color outside the map, default is [1 1 1].  
    'ColorIn'    - Color inside the map, default is [1 1 1].  
    'ColorScheme'- Select a predefined color scheme  
    (override user selected colors):  
    'C'   - Colored stars, black planets,  
    white background,  
    gray milky way, default.  
    'BW'  - Black stars and planets, white bacground,  
    gray milky way.  
    'CB'  - Color stars, white planets,  
    black background,  
    blue milky way.  
    'Copyright'  - {'off' | 'EO' | 'TAU'}, default is 'TAU'.  
    'Legend'     - {'off' | 'Mag'}, default is 'Mag'.  
    'Visible'    - {'on' | 'off'}, default is 'off'  
    (for plot visibility).  
    'Ecliptic'   - plot ecliptic {'yes'|'no'}, default is 'no'.  
    Output : null  
    Plot   : Plot a monthly sky map.  
    Tested : Matlab 6.5  
    By : Eran O. Ofek                    Nov 2004  
    Web example: http://astroclub.tau.ac.il/skymaps/monthly/  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: celestial.map.plot_monthly_smap([1 1 2010 18./24]);  
    Reliable: 1  
      
### celestial.map.plot_smap

Given a star catalog plot star map Package: celestial.map Description: Given a star catalog plot star map with optional magnitude/color/type/proper motion ('/cy).


    
    Given a star catalog plot star map  
    Package: celestial.map  
    Description: Given a star catalog plot star map with optional  
    magnitude/color/type/proper motion ('/cy).  
    Input  : - Catalog to plot  
    * arbitrary number of keyword and properties, where keywods are:  
    'FOV'      - Field of view [fovRA, fovDec], in [arcmin],  
    default is field of view.  
    'Center'   - Field center [RA, Dec], default is mid coordinates.  
    'ColRA'    - Catalog column number containing RA,  
    if three columns are given then assumed to  
    be [H M S], otherwise [Radians].  
    Default is 1.  
    'ColDec'   - Catalog column number containing Dec,  
    if four columns are given then assumed to  
    be [Sign D M S], otherwise [Radians].  
    Default is 2.  
    'ColMag'   - Catalog column number containing magnitude.  
    Default is 3.  
    NaN magnitude size is set to 0.01  
    'ColMagMax'- Catalog column number containing max magnitude  
    (e.g., variable stars), default is NaN.  
    'ColColor' - Catalog column number containing color index.  
    If one column is given then the color index  
    is converted to color via the 'ColorMap'  
    property. If three columns are given then  
    used as [r g b] color.  
    Default is NaN (dont plot color).  
    'ColPMRA'  - Catalog column number containing PM in RA [mas/yr]  
    Default is NaN (dont plot PM).  
    'ColPMDec' - Catalog column number containing PM in Dec [mas/yr]  
    Default is NaN (dont plot PM).  
    'MinPM'    - Minimum proper motion [mas/yr] value to plot (as an arrow),  
    default is NaN (don't plot PM).  
    'ColTypeE' - Catalog column number containing object edge color,  
    default is NaN (e.g., EdgeColor is black).  
    where object tye are:  
    0 - EdgeColor is the same as object color.  
    1 - EdgeColor is black.  
    2 - EdgeColor is white.  
    If a string with one of the above numbers is  
    given, then set the symbol to the one given  
    by this number.  
    'ColTypeS' - Catalog column number containing object symbol type,  
    default is NaN (e.g., filled circles).  
    where symbols type are:  
    0  - filled circles  
    10 - empty circles.  
    11 - filled ellipse  
    1  - empty ellipse  
    2  - filled box  
    12 - empty box  
    3  - filled triangle  
    13 - empty triangle  
    4  - +  
    14 - + inside colored circle  
    24 - + inside white circle  
    5  - X  
    15 - X inside colored circle  
    25 - X inside white circle  
    94 - dotted line  
    If a string with one of the above numbers is  
    given, then set the symbol to the one given  
    by this number.  
    'ColEll_A' - Catalog column number containing semi major axis of  
    ellipse [radians], default is NaN.  
    'ColEll_B' - Catalog column number containing semi minor axis of  
    ellipse [radians], default is NaN.  
    'ColEll_E' - Catalog column number containing ellipticity of  
    ellipse [radians], default is NaN.  
    'ColEll_PA'- Catalog column number containing PA of  
    ellipse [radians], default is NaN.  
    'Labels'   - cell array containing labels (one per object),  
    default is NaN.  
    'MagRange' - Magnitude range to map into the 'MagSize' property.  
    Default is [min(Mag) max(Mag)].  
    'MagSize'  - Star sizes to map from star magnitude.  
    Default is [3 30].  
    'MagFun'   - Star magnitude-size mapping table, if given  
    overrid MagSize.  
    Two column matrix [Mag, Size] and use linear  
    interpolation in between.  
    Default is NaN.  
    'ColorMap' - Color map [plot_color, min_color, max_color],  
    default is : [0 1 1 -Inf 0.00  
    0 0 1 0.00 0.75  
    0 1 0 0.75 1.50  
    1 1 0 1.50 2.25  
    1 0 0 2.25 3.00  
    1 0 1 3.00 +Inf  
    0 0 0 NaN  NaN  
    1 1 1 Inf  Inf]  
    'PlotTitle'- Plot figure title {'yes' | 'no'}, default is 'yes'.  
    'Units'    - labels units : {'rad' | 'deg' | 'arcmin' | 'arcsec' | 'Natural'},  
    default is 'arcmin'.  
    'Project ' - map projection : {'natural'  | 'stereographic' | 'gnomonic'    |  
    'mercator' | 'cassini'       | 'conic'       |  
    'albers'   | 'bonne'         | 'cylindrical' |  
    'parabolic'| 'polar'         | 'sinusoidal'  |  
    'aitoff'   | 'hammer'        | 'mollweide'   |  
    'stereographic_polar' | 'azimuthal'}  
    Output : null  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Mar 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
### celestial.map.prep_dss_fc

- prep_dss_fc function                                               AstroMap Description: Prepare DSS finding charts, with labels, compass, and slits.


    
    -  
    prep_dss_fc function                                               AstroMap  
    Description: Prepare DSS finding charts, with labels, compass, and slits.  
    Input  : * Arbitrary number of pairs of parameters (...,keyword,value,...):  
    Possible keywords:  
    'ListOpt1'  - List file name containing [RA,Dec,Equinox,Name],  
    where RA and Dec are in sexagesimal format,  
    Equinox in Years, SlitPA in deg, SlitWidth and  
    SlitLength in arcsec.  
    'ListOpt2'  - List file names containing:  
    [RA,Dec,Equinox,Name,SlitPA,SlitWidth,SlitLength].  
    'ListOptW'  - List file names containing:  
    [#,RA,Dec,Equinox,Name,...]  
    'Inv'       - Inverse colormap, {'y' | 'n'}, default is 'y'.  
    'PlotSlit'  - {'y' | 'n'}, default is 'y' (if available).  
    'PlotObj'   - {'n','bs','ro',...}. Plot circle/box around  
    centeral coordinates, default is 'bo'.  
    'PlotSize'  - Marker size, default is 16.  
    'PlotCirc'  - {'n' | [R G B]}. Plot circle, default is 'n'.  
    'CircRad'   - Circle radius in arcsec, default is 30.  
    'PlotName'  - Plot name on finding chart {'y' | 'n'},  
    default is 'y'.  
    'PlotCoo'   - Plot Object coordinates {'y' | 'n'},  
    default is 'y'.  
    'PlotComp'  - Plot Compass {'y' | 'n'}, default is 'y'.  
    'PlotScale' - Scale length [arcsec], default is NaN  
    (don't plot scale).  
    'RA'        - Vector of RA [radians], [H M S] ore sexagesimal.  
    'Dec'       - Vector of Dec [radians], [Sign D M S], or  
    sexagesimal.  
    'FOV'       - Field of view [arcmin arcmin], default is [12 12].  
    'Equinox'   - Vector of Equinox [Years], default is [2000],  
    'Name'      - Cell array of names.  
    'SlitPA'    - Vector of Slit PA [deg], default is [],  
    'SlitWidth' - Vector of Slit width [arcsec], default is 0  
    'SlitLength'- Vector of Slit length [arcsec], default is 200.  
    'CLF'       - Clear figure after plot {'y' | 'n'}, default is 'n'.  
    'Save'      - Save finding chart using "name":  
    {'n' | 'jpg' | 'eps'}, default is 'jpg'.  
    'Filter'    - POSS filter/epoch, see get_dss.m for options,  
    default is '2'.  
    'Color'     - Text/Scale/Compass color, default is 'b'.  
    'SaveFITS'  - Svae FITS image {'y' | 'n'}, default is 'n';  
    'CompassPos'- Compass relative position, default is [0.1 0.1].  
    'CompassLength'-Compass relative length, default is 0.05.  
    'CooPos'    - Coordinates text relative position,  
    default is [0.1,0.95].  
    'CooFontSize'-Coordinates text font size, default is 20.  
    'NamePos'   - Name text position, default is [0.1,0.85].  
    'NameFontSize'-Name text font size, default is 20.  
    'ScalePos'  - Scale relative position, default is [0.8,0.1].  
    'ScaleFontSize'- Scale text font sizem, default is 18.  
    'Z1_Per'    - Z1 scaling (percentile), default is 0.50.  
    'Z2_Per'    - Z2 scaling (percentile), default is 0.99.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jul 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: prep_dss_fc('ListOpt1','FC1.list','PlotObj','b+');  
    Name{1}='Example';  
    prep_dss_fc('RA',[10 04 00],'Dec',[1 41 0 0],'Name',Name,'PlotScale',120,'SlitPA',45,'SlitWidth',30,'Color','c','Inv','n');  
    Reliable: 2  
    -  
      
      
      
### celestial.map.usnob1_map

Plot a finding chart using a local copy of the USNO-B2.0 Package: celestial.map Description: Plot a finding chart using a local copy of the USNO-B2.0 catalog. The user can select between b/w stars or color stars (with their O-E color index color coded).


    
    Plot a finding chart using a local copy of the USNO-B2.0  
    Package: celestial.map  
    Description: Plot a finding chart using a local copy of the USNO-B2.0  
    catalog. The user can select between b/w stars or color  
    stars (with their O-E color index color coded).  
    If O-E is not available then stars are plotted in black.  
    The user can overplot known galaxies from the PGC catalog.  
    In the color option the edge of probable stellar objects  
    is marked by black circle. The user can overplot known  
    bright stars (VT<11) for which spikes and saturation  
    artifact can be seen.  
    Input  : - RA (J2000.0), [HH MM SS] or [Radians]  
    - Dec (J2000.0), [Sign, DD MM SS] or [Radians]  
    - Field of view [arcsec], default is 300 arcsec.  
    - Color option: 'bw'-black & white; 'sp'-spectral type (default)  
    - Add PGC galaxies, default is 'y'.  
    - Add known bright stars with an estimat of their artifact region.  
    - Magnitude range, default is [-2 22].  
    - Symbol plot size range [3, 30].  
    - Galaxy face color, ('w' - will delete stars found behind the galaxy),  
    default is 'none'.  
    Output : - Field range in deg [RAmin, RAmax, Decmin, Decmax].  
    Plot   : Finding chart.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                     March 2004  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See Also: apmread.m, get_usnob1sel.m, pgc.dat, tyc2_11.dat  
    Need: ubcone (binary file!) + USNO-B2.0 catalog on disk  
    Reliable: NOT WORKING ANYMORE  
      
