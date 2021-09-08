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
      
### imProc.match.match

Match two catalogs in AstroCatalog objects This functin returens: a matched source catalog, and an unmatched source catalog. The matched catalog result has the same number of


    
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
    column to  Obj1 that include the index of  
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
    If true, then the number of sources of the matched  
    catalog is like the catsHTM object, while  
    false the size is like the input object.  
    Output : - A matched AstroCatalog object. See Match.match.  
    - An unatched AstroCatalog object. See Match.match.  
    - A truely unatched AstroCatalog object. See Match.match.  
    - The catsHTM AstroCatalog object.  
    Author : Eran Ofek (Apr 2021)  
    Example: AC=AstroCatalog({'asu.fit'},'HDU',2);  
    M = imProc.cat.Match;  
    M.coneSearch(AC,[1 1],'Radius',3600);  
    [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = M.match_catsHTM(AC,'GAIADR2')  
      
### imProc.match.matched2matrix

A matched AstroCatalog object into a matrix of epochs by index AstCat object to a matrix of matched sources. Description: Given an AstroCatalog object containing multiple elements, in which each element containing the same number of rows


    
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
