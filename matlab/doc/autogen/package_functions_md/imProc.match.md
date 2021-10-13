# Package: imProc.match


### imProc.match.coneSearch

cone search(s) on AstroCatalog/AstroImage object


    
    cone search(s) on AstroCatalog/AstroImage object  
    Input  : - An AstroCatalog/AstroImage object. If multiple elements  
    then will perform the cone search on any one of  
    the elements.  
    - A two column matrix of [RA, Dec] or [X, Y]  
    to search.  
    If more then one row, then the results of the  
    search will be merged.  
    * ...,key,val,...  
    'CooType' - Which Coo system to use 'pix' (for X/Y),  
    or 'sphere' (for RA/Dec).  
    If empty, will look for 'sphere', and if not exist  
    will use 'pix'. Default is [].  
    'Radius'  - Search radius. Default is 5.  
    'RadiusUnits' - Search radius units (if spherical  
    coordinates search). Default is 'arcsec'.  
    'Shape' - Shape search. Default is 'circle'.  
    'CooUnits' - Units of (spherical) coordinates  
    (second input argument). Default is 'deg'.  
    'AddDistCol' - Add column distance to outout  
    catalog. Default is true.  
    'DistUnits' - Distance units. Default is 'arcsec'.  
    'DistColName' - Distance column name.  
    Default is 'Dist'.  
    'DistColPos' - Position of Distance column in  
    output catalog. Default is Inf (i.e., last  
    column).  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    Output : - An AstroCatalog object with the found sources.  
    - A vector of logicals with the same length as the  
    number of rows in the input object. True if object  
    is in cone. If the input is an object with  
    multiple elements, then this vector corresponds to  
    the last element in the object array.  
    - A vector of distances of the found objects from  
    the cone search center. If the input is an object with  
    multiple elements, then this vector corresponds to  
    the last element in the object array.  
    Author : Eran Ofek (Apr 2021)  
    Example: AC=AstroCatalog({'asu.fit'},'HDU',2);  
    [NC, Flag, Dist] = imProc.match.coneSearch(AC,[1 1],'Radius',3600)  
    [NC, Flag, Dist] = imProc.match.coneSearch(AC,[1 1; 0 0],'Radius',3600);   search around two positions (merged results).  
      
### imProc.match.flagSrcWithNeighbors

Flag sources in AstroCatalog which have neighbors within a radius Optionaly, remove sources with neighboors.


    
    Flag sources in AstroCatalog which have neighbors within a radius  
    Optionaly, remove sources with neighboors.  
    Input  : - A multi-element AstroCatalog object.  
    The object must be sorted, by 'Y'.  
    * ...,key,val,...  
    'CooType' - ['pix'] | 'spere'.  
    'pix' will work on cartesian coordinates, while 'sphere',  
    on spherical coordinates.  
    'Radius' - Search radius. Default is 10.  
    'RadiusUnits' - Search radius units (for  
    'CooType'='sphere'). Default is 'arcsec'.  
    'ColNamesX' - A cell array of dictionary names for the X  
    coordinates (first to appear will be selected).  
    Default is AstroCatalog.DefNamesX.  
    'ColNamesY' - A cell array of dictionary names for the Y  
    coordinates (first to appear will be selected).  
    Default is AstroCatalog.DefNamesY.  
    'ColNamesRA' - A cell array of dictionary names for the RA  
    coordinates (first to appear will be selected).  
    Default is AstroCatalog.DefNamesRA.  
    'ColNamesDec' - A cell array of dictionary names for the  
    Dec coordinates (first to appear will be selected).  
    Default is AstroCatalog.DefNamesDec.  
    Outout : - A vector of logical indicating sources with neighboors  
    within search radius. If multi-element AstroCatalog, then  
    only the vector corresponding to last object is returned.  
    - Modified object (original copy is modified!).  
    The AstroCatalog object after removing sources with  
    neighboors. If nargout<2 then the original object is not  
    modified.  
    Author : Eran Ofek (Jul 2021)  
    Example: AC = AstroCatalog({rand(100,2).*1024},'ColNames',{'X','Y'});  
    Flag = imProc.match.flagSrcWithNeighbors(AC)  
      
### imProc.match.inPolygon

Return sources inside polygon


    
    Return sources inside polygon  
    Input  : - An AstroCatalog/AstroImage object. If multiple elements  
    then will perform the inPolygon search on any one of  
    the elements.  
    - A two column matrix of [Long, Lat] or [X,Y] that  
    defines the verteces of the polygon.  
    * ...,key,val,...  
    'CooType' - Which Coo system to use 'pix' (for X/Y),  
    or 'sphere' (for RA/Dec).  
    If empty, will look for 'sphere', and if not exist  
    will use 'pix'. Default is [].  
    'CooUnits' - Units of (spherical) coordinates  
    (second input argument). Default is 'deg'.  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    Output : - An AstroCatalog object with the found sources.  
    - A vector of logicals with the same length as the  
    number of rows in the input object. True if object  
    is in polygon. If the input is an object with  
    multiple elements, then this vector corresponds to  
    the last element in the object array.  
    Author : Eran Ofek (Apr 2021)  
    very similar to coneSearch  
    Example: AC=AstroCatalog({'asu.fit'},'HDU',2);  
    [InP, Flag] = imProc.match.inPolygon(AC,[1 1; 1.1 1.1; 0.5 0.1],'CooUnits','rad')  
      
### imProc.match.insertCol_matchIndices

Insert Dist/Nmatch columns to a single element AstroCatalog based on ResInd. This is a utility function for users of imProc.match.matchReturnIndices. The function gets the input of imProc.match.matchReturnIndices (called here ResInd), and produce two AstroCatalog objects:


    
    Insert Dist/Nmatch columns to a single element AstroCatalog based on ResInd.  
    This is a utility function for users of  
    imProc.match.matchReturnIndices. The function gets the input of  
    imProc.match.matchReturnIndices (called here ResInd), and  
    produce two AstroCatalog objects:  
    1. The original AstroCatalog with possibly Dist and Nmatch  
    columns.  
    2. Selected lines in the original AstroCatalog which has  
    matches, with possibly Dist and Nmatch  
    columns.  
    Input  : - A single element AstroCatalog object.  
    - ResInd. This is the output of imProc.match.matchReturnIndices  
    * ...,key,val,...  
    'AddColDist' - Default is true.  
    'ColDistPos' - Default is Inf.  
    'ColDistName' - Default is 'Dist'.  
    'ColDistUnits' - Default is 'arcsec'.  
    'AddColNmatch' - Default is true.  
    'ColNmatchPos' - Default is Inf.  
    'ColNmatchName' - Default is 'Nmatch'.  
    Output : - The input catalog with added columns for the nearest match  
    in the catsHTM catalog.  
    - Select lines only from the input catalog. Only sources  
    with matches are selected.  
    Author : Eran Ofek (Apr 2021)  
    Example:  
      
### imProc.match.match

Match two catalogs in AstroCatalog objects This functin returens: a matched source catalog, and an unmatched source catalog. The matched catalog result has the same number of sources as in the Obj2 catalog, and for each Obj2 source,


    
    Match two catalogs in AstroCatalog objects  
    This functin returens: a matched source catalog, and an  
    unmatched source catalog.  
    The matched catalog result has the same number of  
    sources as in the Obj2 catalog, and for each Obj2 source,  
    the nearest source in Obj1 is listed. If there is no  
    source within the search radius, then the entire line  
    contains NaNs.  
    The sources in Obj1 that doesn't have counterparts in  
    Obj2 are listed in the unmatched catalog.  
    The angular distance and index of the source in Obj1 may be  
    added to the matched catalog.  
    Also return a catalog of TruelyUnMatchedObj.  
    This exclude not only the nearest source within the  
    search radius, but all the sources in Obj1 which have  
    counterparts within the search radius.  
    Input  : - An AstroCatalog/AstroImage object.  
    If multiple elements then each element will be  
    matched against the corresponding element (or a  
    single element) in the second object.  
    If this object is not sorted, then the object will be  
    sorted (and modified).  
    - A second AstroCatalog object - The function will  
    attempt to match every source in this catalog with  
    objects in the first input catalog.  
    * ..., key, val,..  
    'Radius'  - Search radius. Default is 5.  
    'RadiusUnits' - Search radius units (if spherical  
    coordinates search). Default is 'arcsec'.  
    'AddIndInRef' - A logical indicating if to add a  
    column to Obj1 that include the index of  
    the source in the reference catalog (Obj2).  
    Default is true.  
    'IndInRefColName' - The column name of the Index  
    in reference. Default is 'IndInRef'.  
    'IndInRefColPos' - Position of the IndInRef column  
    name to add to Obj1. Default is Inf (i.e.,  
    last column).  
    'DeleteExistIndInRef' - A logical indicating if to  
    delete existing IndRefColName, before  
    inserting the new column.  
    Default is true.  
    'AddDistCol' - Add column distance to outout  
    catalog. Default is true.  
    'DistUnits' - Distance units. Default is 'arcsec'.  
    'DistColName' - Distance column name.  
    Default is 'Dist'.  
    'DistColPos' - Position of Distance column in  
    output catalog. Default is Inf (i.e., last  
    column).  
    'CooType' - CooType (i.e., 'pix','sphere').  
    If empty, will use what is available in the catalog  
    with preference for 'sphere'. Default is empty.  
    'ColCatX' - If CooType is not empty, this is the column  
    names/index from which to select the catalog X  
    coordinate. Default is [].  
    'ColCatY' - Like 'ColCatX', but for the Y coordinate.  
    'ColRefX' - Like 'ColCatX', but for te ref catalog.  
    'ColRefY' - Like 'ColRefX', but for the Y coordinate.  
    Output : - An AstroCatalog object of matched sources.  
    Numeber of sources equal to that of the number  
    of sources in the second object (Obj2).  
    Data is taken from Obj1.  
    Entire line is NaN if no source found.  
    - An AstroCatalog object of unmatched sources.  
    Include all the sources in the first object that  
    are not matched.  
    - An AstroCatalog object of truely unmatched  
    sources.  
    Include all the sources in the first object that  
    are not found in the search radius.  
    Author : Eran Ofek (Apr 2021)  
    Example : AC = AstroCatalog;  
    AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];  
    AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};  
    AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0]  
    AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};  
    [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad')  
      
### imProc.match.match2solarSystem

Match sources in AstroCatalog object to Solar System objects. This is done using the celestial.OrbitalEl class. For each source in the AstroCatalog, search for minor planets at the same position and epoch. Returns the lines from the AstroCatalog that are matched with


    
    Match sources in AstroCatalog object to Solar System objects.  
    This is done using the celestial.OrbitalEl class.  
    For each source in the AstroCatalog, search for minor planets  
    at the same position and epoch.  
    Returns the lines from the AstroCatalog that are matched with  
    minor planets, and add the angular distance of the match and  
    the minor planet designation.  
    Input  : - An AstroCatalog, or AstroImage object (multi-elements supported).  
    Note that unless 'CreateNewObj'=true, this ibject may be  
    modified by the function.  
    * ...,key,val,...  
    'JD' - Scalar Julian Day (JD), corresponding to the epoch of the  
    catalog. If empty, and input is an AstroImage, then  
    will attempt to read the JD from the image header.  
    Default is [].  
    'OrbEl' - An OrbitalEl class with the orbital elements of  
    all the minor planets to match against the sources.  
    If empty, then will load the JPL orbital elements  
    from disk. For instellation of the JPL orbital  
    elements file see the Installer class.  
    Default is [].  
    'SearchSingleEpoch' - A logical indicating which search  
    option to use:  
    If false [default], then each image/catalog is  
    search seperatly.  
    If true, then all the images must have a common  
    epoch (provided in input JD), and they are within  
    ImageRadius from ImageRA, ImageDec.  
    'ImageRadius' - Big image Radius, for the  
    SearchSingleEpoch=true option. Default is 4.  
    'ImageRA' - Big image RA, for the  
    SearchSingleEpoch=true option. Default is [].  
    'ImageDec' - Big image Dec, for the  
    SearchSingleEpoch=true option. Default is [].  
    'ImageCooUnits' - ImageRadius, ImageRA, ImageDec units.  
    Default is 'deg'.  
    'AddPlanets' - Match sources also against planets.  
    THIS OPTION IS NOT YET AVAILABLE.  
    Default is false.  
    'SearchRadius' - Matching search radius between source and  
    minor planets. Deafult is 5.  
    'SearchRadiusUnits' - Units for the SearchRadius argument.  
    Default is 'arcsec'.  
    'MagLimit' - Magnitude limit for minor planets. Default is Inf.  
    'KeyLon' - Obs. Longitude main keyword dictionary  
    name. Default is 'OBSLON'.  
    'KeyLat' - Obs. Latitude main keyword dictionary  
    name. Default is 'OBSLAT'.  
    'KeyAlt' - Obs. Altitude main keyword dictionary  
    name. Default is 'OBSALT'.  
    'IsInputAlt' - IsInputAlt argument to pass to  
    getVal. If true, will search keyword name  
    in alternate names list. Default is false.  
    'GeoPos' - Geodetic position of the observer (on  
    Earth). [Lon (rad), Lat (rad), Height (m)].  
    This parameter superceeds KeyLat,KeyLon.  
    If 'geo' then calculate geocentric position.  
    Default is [].  
    'RefEllipsoid' - Reference ellipsoid for the  
    geodetic positions. Default is 'WGS84'.  
    'AddColDist' - Adding match angular distance column to  
    output catalog. Default is true.  
    'ColDistPos' - Position in which to add ang. dist column.  
    Default is Inf.  
    'ColDistName' - Name of ang. dist column.  
    Default is 'DistMP'.  
    'ColDistUnits' - Units of ang. dist column.  
    Default is 'arcsec'.  
    'AddColNmatch' - Adding number of matches column to  
    output catalog. Default is true.  
    'ColNmatchPos' - Position in which to add Nmatch column.  
    Default is Inf.  
    'ColNmatchName' - Name of Nmatch column.  
    Default is 'NmatchMP'.  
    'AddColDesignation' -Adding minor planet designation column to  
    output catalog. If true, then the output 'Catalog'  
    field in the AStroCatalog will be of table class.  
    Default is true.  
    'ColDesigPos' - Position in which to add designation column.  
    Default is Inf.  
    'ColDesigName' - Name of designation column.  
    Default is 'Designation'.  
    'CreateNewObj' - {false|true} Indicating if to create a  
    new copy of the input Astrocatalog object.  
    Default is false.  
    If nargout>1, then add, for each  
    source in the input AstroCatalog object, the  
    angular distance to the nearest minor planet (NaN if no match).  
    'SourcesColDistPos' - The position of the ang. dist.  
    column that will be added to the input AstroCatalog  
    object. Default is Inf.  
    'SourcesColDistName' - The name of the ang. dist.  
    column that will be added to the input AstroCatalog  
    object. Default is 'DistMP.  
    'SourcesColDistUnits' - The units of the ang. dist. added  
    to the input AstroCatalog object.  
    Default is 'arcsec'.  
    Output : - An AstroCatalog object containing only the sources in the  
    input AstroCatalog that are matched with minor planets.  
    Possibly adding ang. dist, Nmatch, and minor planet  
    designation to the output.  
    - If a second output is requested, then the input  
    AstroCatalog object will be modified (see 'CreateNewObj'  
    argument). The modified object may be sorted and may  
    include additional information on the angular distance to  
    the nearest minor planet.  
    Author : Eran Ofek (Sep 2021)  
    Example: Cat = ephem(OrbEl, JD, 'AddDesignation',false);  
    R = rand(10,8); R(:,2:3) = R(:,2:3) + [82 11];  
    Cat1 = AstroCatalog({R});  
    Cat2  = merge([Cat;Cat1]);  
    Cat2  = deleteCol(Cat2, 'JD');  
    Result = imProc.match.match2solarSystem(Cat2, 'JD',JD, 'GeoPos',[]);  
    O = celestial.OrbitalEl.loadSolarSystem;  
    Result = imProc.match.match2solarSystem(Cat2, 'JD',JD, 'GeoPos',[], 'OrbEl',O);  
    [Result, CatOut] = imProc.match.match2solarSystem(Cat2, 'JD',JD, 'GeoPos',[], 'OrbEl',O);  
      
### imProc.match.matchReturnIndices

Match two catalogs in AstroCatalog objects and return the matched indices. This is a basic utility function that returns the two-directional indices of the matched sources. This function is used by the more advanced matching programs.


    
    Match two catalogs in AstroCatalog objects and return the matched indices.  
    This is a basic utility function that returns the two-directional  
    indices of the matched sources.  
    This function is used by the more advanced matching programs.  
    Input  : - An AstroCatalog/AstroImage object.  
    If multiple elements then each element will be  
    matched against the corresponding element (or a  
    single element) in the second object.  
    If this object is not sorted, then the object will be  
    sorted (and modified, unlse CreateNewObj=true).  
    - A second AstroCatalog object - The function will  
    attempt to match every source in this catalog with  
    objects in the first input catalog.  
    * ..., key, val,..  
    'Radius'  - Search radius. Default is 5.  
    'RadiusUnits' - Search radius units (if spherical  
    coordinates search). Default is 'arcsec'.  
    'CreateNewObj' - A logical indicating if to create a new  
    copy of Obj1 if it is not sorted. If false, then  
    Obj1 will be modified. Default is false.  
    'CooType' - CooType (i.e., 'pix','sphere').  
    If empty, will use what is available in the catalog  
    with preference for 'sphere'. Default is empty.  
    'ColCatX' - If CooType is not empty, this is the column  
    names/index from which to select the catalog X  
    coordinate. Default is [].  
    'ColCatY' - Like 'ColCatX', but for the Y coordinate.  
    'ColRefX' - Like 'ColCatX', but for te ref catalog.  
    'ColRefY' - Like 'ColRefX', but for the Y coordinate.  
    Output : - A structure array (element per Obj1/Obj2 matching) with  
    the following fields:  
    'Obj2_IndInObj1' - A vector, for each source in Obj2, of  
    its matched indices in Obj1. NaN if no match.  
    'Obj2_Dist' - A vector, for each source in Obj2, of the  
    angular distance ['rad' if 'sphere'] between the  
    matched sources.  
    'Obj2_NmatchObj1' - A vector, for each source in Obj2, of the  
    number of matches within search radius.  
    'Obj1_IndInObj2' - A vector, for each source in Obj1, of  
    its matched indices in Obj2. NaN if no match.  
    'Obj1_FlagNearest' - A vector, for each source in Obj1,  
    of logicals indicating if this source is the neaest  
    match to a source is Obj2.  
    'Obj1_FlagAll' - A vector, for each source in Obj1,  
    of logicals indicating if this source is a  
    match (within search radius) to a source is Obj2.  
    Author : Eran Ofek (Sep 2021)  
    Example : AC = AstroCatalog;  
    AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];  
    AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};  
    AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0]  
    AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};  
    Result = imProc.match.matchReturnIndices(AC,AC2,'Radius',0.01,'CooType','sphere','RadiusUnits','rad')  
      
### imProc.match.match_catsHTM

Match an AstroCatalog object with catsHTM catalog


    
    Match an AstroCatalog object with catsHTM catalog  
    Input  : - An AstroCatalog or an AstroImage object (multi  
    elements supported). The AStroCatalog object will  
    be matched against a catsHTM catalog.  
    - catsHTM catalog name (e.g., 'GAIADR2').  
    See catsHTM.catalogs for possible options.  
    * ...,key,val,...  
    'Coo' - [RA, Dec] of coordinates to search.  
    If empty, then will attempt to find this  
    from the catalog itself. DEfault is [].  
    'CooUnits' - Units of coordinates. Object default  
    is 'deg'.  
    'Radius' - Matching radius. Default is 3.  
    'RadiusUnits' - Matchin radius units.  
    Default is 'arcsec'.  
    'CatRadius' - The search radius of the catsHTM  
    catalog. If not given this is taken as the  
    bounding circle radius of the inout  
    AstroCatalog. Default is [].  
    'CatRadiusUnits' - CatRadius units.  
    Default is 'arcsec'.  
    'Con' - A cell array of additional  
    constraints to apply to output catalog.  
    See catsHTM.cone_search for options.  
    E.g., {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}  
    Default is {}.  
    'catsHTMisRef' - A logical indicating if the  
    catsHTM catalog is treated as the reference  
    catalog. Default is false.  
    If true, then the output is the same but for the catsHTM catalog.  
    'AddColDist' - Default is true.  
    'ColDistPos' - Default is Inf.  
    'ColDistName' - Default is 'Dist'.  
    'ColDistUnits' - Default is 'arcsec'.  
    'AddColNmatch' - Default is true.  
    'ColNmatchPos' - Default is Inf.  
    'ColNmatchName' - Default is 'Nmatch'.  
    Output : - The input catalog with added columns for the nearest match  
    in the catsHTM catalog.  
    - Select lines only from the input catalog. Only sources  
    with matches are selected.  
    Author : Eran Ofek (Apr 2021)  
    Example: AC=AstroCatalog({'asu.fit'},'HDU',2);  
    M = imProc.cat.Match;  
    M.coneSearch(AC,[1 1],'Radius',3600);  
    [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = M.match_catsHTM(AC,'GAIADR2')  
      
### imProc.match.matched2matrix

A matched AstroCatalog object into a matrix of epochs by index AstCat object to a matrix of matched sources. Description: Given an AstroCatalog object containing multiple elements, in which each element containing the same number of rows (e.g., the output of Match/match.m), return a matrix


    
    A matched AstroCatalog object into a matrix of epochs by index  
    AstCat object to a matrix of matched sources.  
    Description: Given an AstroCatalog object containing multiple elements, in  
    which each element containing the same number of rows  
    (e.g., the output of Match/match.m), return a matrix  
    that contains, for selected columns, the requested column  
    in all the AstroCatalog elements. E.g., the number of columns  
    in the matrix is equal to the number of AstroCatalog elements  
    (column per element) and the number of rows is equal to  
    the number of rows in each AstroCatalog element.  
    Input  : - An AstroCatalog object containing multiple element, in  
    which each element containing the same number of rows  
    (e.g., the output of AstCat/match.m)  
    - Column indices or column names (string or a cell array of  
    strings) for which to prepare the array.  
    - A logical indicating if the ouput matrix is  
    Epoch by Ind (true), or Ind by Epoch (false).  
    Default is true.  
    Output : - A structure in which each field name corresponds to a  
    requested column name and contains the matrix of all the  
    column entries in all the AStroCatalog object elements.  
    - A structure array containing a summary.  
    The following fields are available:  
    .Nsrc - Number of sources (size of the 1st dimension of the  
    output matrix).  
    .Nepoch - Number of epochs (size of the 2nd dimension of the  
    output matrix).  
    .Nnn - number of not NaNs for each source in the first Column.  
    - Vector of length equal to the number of epochs. The value in  
    each element is the number of stars that appears in exactly  
    I epochs.  
    Author : Eran O. Ofek (May 2016)  
    Example: AC = AstroCatalog;  
    AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];  
    AC.ColNames = {'RA','Dec'};  
    AC.ColUnits = {'rad','rad'};  
    AC.getCooTypeAuto  
    AC2 = AstroCatalog;  
    AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];  
    AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};  
    AC2.getCooTypeAuto  
    [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');  
    [MC,UM,TUM] = imProc.match.match([AC;AC2; AC; AC2],AC2,'Radius',0.01,'RadiusUnits','rad');  
      
    [Res, Summary, N_Ep] = imProc.match.matched2matrix(MC, 'RA')  
      
      
### imProc.match.unifiedSourcesCatalog

Match multiple catalogs and create a catalog of all unique (by position) sources. i.e., generate a list of all unique positions that appears in all the AstroCatalog object.


    
    Match multiple catalogs and create a catalog of all unique (by position) sources.  
    i.e., generate a list of all unique positions that appears in all the AstroCatalog object.  
    Input  : - A multi-element AstroCatalog object.  
    Each element must contain a catalog which have the same  
    coordinate system as all the other catalogs, and the  
    coordinates have the same column names.  
    * ...,key,val,...  
    'CooType' - Coordinate system by which to match the catalogs  
    'sphere' | 'pix'. Default is 'sphere'.  
    'Radius'  - Search radius. Default is 5.  
    'RadiusUnits' - Search radius units (if spherical  
    coordinates search). Default is 'arcsec'.  
    'ColNamesX' - A cell array of dictionary for X column name.  
    Default is AstroCatalog.DefNamesX.  
    'ColNamesY' - A cell array of dictionary for Y column name.  
    Default is AstroCatalog.DefNamesY.  
    'ColNamesRA' - A cell array of dictionary for RA column name.  
    Default is AstroCatalog.DefNamesRA.  
    'ColNamesDec' - A cell array of dictionary for Dec column name.  
    Default is AstroCatalog.DefNamesDec.  
    Output : - An AstroCatalog object containing a single element catalog  
    with X and Y positions.  
    Author : Eran Ofek (Sep 2021)  
    Example: AC=AstroCatalog({rand(10,3), rand(10,3), rand(10,3)},'ColNames',{'RA','Dec','Z'},'ColUnits',{'rad','rad',''});  
    AC(1).Catalog = [AC(1).Catalog; AC(3).Catalog(1:5,:); AC(2).Catalog(1:2,:)];  
    Result = imProc.match.unifiedSourcesCatalog(AC, 'CooType','sphere');  
      
### imProc.match.unitTest

unitTest for +imProc.match Example: imProc.match.unitTest


    
    unitTest for +imProc.match  
    Example: imProc.match.unitTest  
      
    coneSearch  
