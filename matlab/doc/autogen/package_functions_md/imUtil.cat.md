# Package: imUtil.cat


### imUtil.cat.affine2d_transformation

Applay a 2D affine transformation to [X,Y] coordinates Package: +imUtil.cat Description: Applay a 2D affine transformation to [X,Y] coordinates


    
    Applay a 2D affine transformation to [X,Y] coordinates  
    Package: +imUtil.cat  
    Description: Applay a 2D affine transformation to [X,Y] coordinates  
    Input  : - Catalog for which to transform the coordinates.  
    If empty, then return only the affine matrix.  
    - Either a 2D affine matrix, or a vector of rotation and shift.  
    If a matrix, this is a 3x3 affine matrix  
    e.g., [cos(Theta), -sin(Theta), ShiftX  
    sin(Theta),  cos(Theta), ShiftY  
    0            0           1]  
    Otherwise a vector of [Theta, Scale, ShiftX, ShiftY]  
    or [Theta, Sclae, ShiftX, ShiftY, FlipX, FlipY]  
    - Direction of rotation:  
    '+' will rotate the coordinates in respect to the reference  
    frame, while '-' will rotate the reference frame.  
    Default is '+'.  
    * Pairs of ...,key,val,... The following keywords are avaialble:  
    'ThetaUnits' - If the AffineMatrix (second input argument) is  
    a vector of three elements then here you can specify  
    the units of the rotation angle Theta.  
    Default is 'deg'.  
    'ColX' - Catalog column that contains the X axis. Default is 1.  
    'ColY' - Catalog column that contains the Y axis. Default is 2.  
    Output : - A vector of new X coordinates.  
    - A vector of new Y coordinates.  
    - A 3x3 affine matrix for 2D transformation.  
    - A 3x3 affine matrix for 2D transformation, but without the  
    flip.  
    By: Eran O. Ofek                         May 2020  
    Example: Cat = rand(10,2);  
    [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,[0 10 10])  
      
### imUtil.cat.astrometric_cat

Get astrometric catalog, corrected for proper motion Package: +imUtil.cat


    
    Get astrometric catalog, corrected for proper motion  
    Package: +imUtil.cat  
    Input  : - J2000.0 R.A. [deg].  
    - J2000.0 Dec. [deg].  
    * Pairs of ...,key,val,... arguments.  
    The possible keywords are:  
    'Cat' - Catalog. Default is 'GAIADR2'.  
    'CatFun' - Method by which to extract catalog.  
    Default is 'catsHTM'.  
    'OmitNoPM' - Omit stars with no PM. Default is false.  
    'Con' - constrainf function to applay to catalog.  
    Default is {{'ExcessNoise',[0 10]},{'Mag_G',[10 20]}}.  
    'Size' - Extraction size [semi width, semi height]  
    or search radius. Default is [1 1].  
    'Shape' - Extraction shape: 'circ'|'box'. Default is 'box'.  
    'SizeUnits' - Units for size argument. Default is 'deg'.  
    'EpochOut'  - Default is 2451545.  
    'EpochUnits' - Units of EpochOut. Default is 'JD'.  
    'CooUnits' - Input search coordinates inits.  
    Defaut is 'deg'.  
    'ObsGeoPos' - Observatory Geodetic position [Lon, Lat] in deg.  
    This is needed for adding the AirMass and Paralactic  
    Angle. If not provided, then will not be added.  
    Defualt is [].  
    'AddGeo' - Add [AirMass, ParallacticAngle, Az, Alt] which  
    depands on Geodetic position and time.  
    'OutClass' - Output object. Options are:  
    'mat' - A matrix.  
    'catCl' - A catCl object.  
    'AstCat' - An AstCat object.  
    'table' - A table object.  
    Default is 'mat'.  
    Output : - An astrometric catalog with [RA, Dec, Mag, Color, AstErr] columns.  
    - A cell array of colum names.  
    - A cell array of column units.  
    By: Eran O. Ofek                         Apr 2020  
    Example: [Cat,ColCell,ColUnits]=imUtil.cat.astrometric_cat(1,1,'EpochOut',2021)  
    [Cat,ColCell,ColUnits]=imUtil.cat.astrometric_cat(1,1,'EpochOut',2021,'ObsGeoPos',[35 32])  
      
      
### imUtil.cat.dilute_cat_by_mag

Remove faint sources from a catalog to have a specific surface density Package: +imUtil.cat Description: Dilute sources from a acatalog based on sources magnitude. Remove the faintest sources and sources which have NaN magnitude, such that the surface density of the diluted


    
    Remove faint sources from a catalog to have a specific surface density  
    Package: +imUtil.cat  
    Description: Dilute sources from a acatalog based on sources magnitude.  
    Remove the faintest sources and sources which have NaN  
    magnitude, such that the surface density of the diluted  
    catalog will be equal to some target surface density.  
    Input  : - The catalog, in which one of the columns is a magnitude  
    column.  
    - A cell array of column names. If empty, then the next input  
    argument must be numeric.  
    - The index of the magnitude column name in the input catalog,  
    or the magnitude column name (string).  
    The name will be searched in the cell array of column names,  
    first using strcmp, and if not sucessful using strfind.  
    - The surface density of the input catalog.  
    - The target (required) density of the catalog. The surafce  
    density of the selected sources will be equal to this surface  
    density.  
    Output : - A vector of logical flags of selected sources (rows).  
    - The diluted catalog.  
    By: Eran O. Ofek                         Apr 2020  
    Example: [Flag,Cat]=imUtil.cat.dilute_cat_by_mag(rand(100,3),{'RA','Dec','Mag_G'},'Mag',100,20)  
      
### imUtil.cat.flag_overdense

Flag sources that are found in overdense (box shaped) regions Package: +imUtil.cat Description: Given a catalog of [X,Y] positions, flag sources that are found in regions which their density is above a threshold. The function uses two algorithms:


    
    Flag sources that are found in overdense (box shaped) regions  
    Package: +imUtil.cat  
    Description: Given a catalog of [X,Y] positions, flag sources that are  
    found in regions which their density is above a threshold.  
    The function uses two algorithms:  
    1. source number in box above threshold number.  
    2. source number in box is Nsigma above background.  
    One or both algorithms can be used.  
    Input  : - [X,Y] catalog.  
    * Pairs of ...,key,val,... The following keywords are avaialble:  
    'MaxNinBox' - The threshold of the number of sources found in  
    a box of size [StepX, StepY] above which to flag the  
    source.  
    If empty, then the first algorithm (source number in  
    box above threshold number) will not be used.  
    Default is 10.  
    'Nsigma' - This is the threshold for the second algorithm  
    (source number in box is Nsigma above background), in  
    units of the number of sigmas above background.  
    If empty. then this algorithm will not be used.  
    Default is 10.  
    'MinStd' - Minimum std in the second method, in order to  
    avoide zero std. Default is 0.5.  
    'MeanFun' - Function handle to use in the background  
    estimation. Default is @median.  
    'MeanFunPar' - A cell array of parameters to passt to the  
    'MeanFun'. Default is {'all','omitnan'}.  
    'StdFun' - Function handle to use in the std  
    estimation. Default is @imUtil.background.rstd.  
    'StdFunPar' - A cell array of parameters to passt to the  
    'MeanFun'. Default is {}.  
    'StepX' - X box size for source counting.  
    Default is 10.  
    'StepY' - Y box size for source counting.  
    Default is 10.  
    'CCDSEC' - [Xmin Xmax Ymin Ymax] of image in which to do the  
    source counting. This region must include all sources.  
    If empty, then use min/max of coordinates -/+ 'Delta'.  
    Default is [].  
    'ColX' - X column in catalog. Default is 1.  
    'ColY' - Y column in catalog. Default is 2.  
    'Delta' - The Delta to add to min/max coordinates in case  
    CCDSEC is empty.  
    Default is 0.5.  
    Output : - A logical flag indicating if the corresponding source in the  
    input catalog is in overdense region.  
    - An integer indicating to which box the source belongs.  
    0 if source is not in an overdense region.  
    Note that if both algorithms are used then the flagging from  
    the second algorith will dominate.  
    By: Eran O. Ofek                         May 2020  
    Example: Cat=rand(100,2).*1000; Cat=[Cat;rand(20,2).*10]; Cat=[Cat;rand(20,2).*10+[50 100]];  
    Flag=imUtil.cat.flag_overdense(Cat)  
      
      
### imUtil.cat.flag_overdense_colrow

Flag sources that are found in overdense (box shaped) regions Package: +imUtil.cat Description: Given a catalog of [X,Y] positions, flag sources that are found in regions which their density is above a threshold. The function uses two algorithms:


    
    Flag sources that are found in overdense (box shaped) regions  
    Package: +imUtil.cat  
    Description: Given a catalog of [X,Y] positions, flag sources that are  
    found in regions which their density is above a threshold.  
    The function uses two algorithms:  
    1. source number in row/column above threshold number.  
    2. source number in row/column is Nsigma above background.  
    One or both algorithms can be used.  
    Input  : - [X,Y] catalog.  
    * Pairs of ...,key,val,... The following keywords are avaialble:  
    'Dim' - Dimension overwhich to calculate the histogram.  
    if 1, then willl look for overdensity in columns, if 2  
    in rows. Default is 1.  
    'MaxNinBox' - The threshold of the number of sources found in  
    a column/row of size [Step] above which to flag the  
    source.  
    If empty, then the first algorithm (source number in  
    box above threshold number) will not be used.  
    Default is [].  
    'Nsigma' - This is the threshold for the second algorithm  
    (source number in box is Nsigma above background), in  
    units of the number of sigmas above background.  
    If empty. then this algorithm will not be used.  
    Default is 10.  
    'MinStd' - Minimum std in the second method, in order to  
    avoide zero std. Default is 0.5.  
    'MeanFun' - Function handle to use in the background  
    estimation. Default is @median.  
    'MeanFunPar' - A cell array of parameters to passt to the  
    'MeanFun'. Default is {'all','omitnan'}.  
    'StdFun' - Function handle to use in the std  
    estimation. Default is @imUtil.background.rstd.  
    'StdFunPar' - A cell array of parameters to passt to the  
    'MeanFun'. Default is {}.  
    'Step' - histogram step size.  
    Default is 3.  
    'CCDSEC' - [Xmin Xmax Ymin Ymax] of image in which to do the  
    source counting. This region must include all sources.  
    If empty, then use min/max of coordinates -/+ 'Delta'.  
    Default is [].  
    'ColX' - X column in catalog. Default is 1.  
    'ColY' - Y column in catalog. Default is 2.  
    'Delta' - The Delta to add to min/max coordinates in case  
    CCDSEC is empty.  
    Default is 0.5.  
    Output : - A logical flag indicating if the corresponding source in the  
    input catalog is in overdense line/row.  
    - An integer indicating to which line/row the source belongs.  
    0 if source is not in an overdense region.  
    Note that if both algorithms are used then the flagging from  
    the second algorith will dominate.  
    By: Eran O. Ofek                         May 2020  
    Example: Cat=rand(100,2).*1000; Cat=[Cat;rand(50,2).*[1 1000]+[10 0] ];  
    [Flag,Ind]=imUtil.cat.flag_overdense_colrow(Cat)  
      
      
### imUtil.cat.match_sources

Match sources in two catalogs with the same coordinates systems. Package: imUtil.cat Description: Match a catalog and reference catalog which are on the same 2-D coordinate system. The catalogs may have spherical or planner coordinates.


    
    Match sources in two catalogs with the same coordinates systems.  
    Package: imUtil.cat  
    Description: Match a catalog and reference catalog which are on the  
    same 2-D coordinate system.  
    The catalogs may have spherical or planner coordinates.  
    Input  : - A catalog. Coordinate units are defined in 'CooUnits'.  
    - A reference catalog. Unless 'IsSortedRef' is false, this  
    catalog must be sorted by latitude/Y coordinates.  
    Coordinate units are defined in 'CooUnits'.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'CooUnits' - Units of coordinates (for the IsSpherical=true  
    case).  
    Default is 'rad'.  
    'SearchRadius' - Search radius for matching.  
    Default is 3.  
    'SearchRadiusUnits' - In case the 'IsSpherical' argument is  
    true, then this indicats the units for the SearchRadius  
    argument.  
    Default is 'arcsec'.  
    'IsSpherical' - Indicating if catalogs are in sphereical  
    coordinates [Long,Lat] (true) or planner (false).  
    Default is true.  
    'IsSortedRef' - Is the refrence catalog is sorted.  
    Default is true.  
    'ColCatX' - Column for X/Long coordinates in Cat.  
    Default is 1.  
    'ColCatY' - Column for Y/Lat coordinates in Cat.  
    Default is 2.  
    'ColRefX' - Column for X/Long coordinates in Ref.  
    Default is 1.  
    'ColRefY' - Column for Y/Lat coordinates in Ref.  
    Default is 2.  
    'MatchOnly1' - If true then select only sources with a single  
    match within search radius. Default is true.  
    Output : - Matched catalog (line by line to MatchedRef).  
    - Matched reference catalog (MatchedRef).  
    - A structure with the following fields:  
    'Ind'      - A strucure array in which the number of elements equal to the  
    number of coordinates in Cat, and with the following  
    fields:  
    'Ind' - Vector of indices of matched sources.  
    'Ind' - index of source in Ref.  
    'Nmatch' - number of matches.  
    'Nmatch' - Number of matched sources.  
    'Dist' - Distance between sources. This is provided only if  
    the input search radius is negative.  
    'Nmatches' - vector of number of matches for each source in  
    Cat to sources in Ref.  
    'DeltaX' - Cat-Ref X/Long difference (on great circle).  
    'DeltaY' - Cat-Ref Y/Lat diffrence.  
    'Dist'   - Cat/Ref distance.  
    'PA'     - Position angle.  
    'DeltaRMS' - rms of [DeltaX, DeltaY].  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Cat = rand(1000,2).*1000; Ref = Cat + randn(1000,2).*0.3;  
    Ref = sortrows(Ref,2);  
    [MatchedCat,MatchedRef,Res]=imUtil.cat.match_sources(CatRef = sortrows(Ref,2);,Ref,'IsSpherical',false)  
    Reliable: 2  
      
      
### imUtil.cat.match_sources_xy

Match sources between Cat and Ref catalogs using their X/Y (planar) coordinates. Package: +imUtil.cat Description: Matched the sources in Cat to sources in Ref by planar X/Y coordinates.


    
    Match sources between Cat and Ref catalogs using their X/Y (planar) coordinates.  
    Package: +imUtil.cat  
    Description: Matched the sources in Cat to sources in Ref by planar X/Y  
    coordinates.  
    Input  : - Catalog. A matrix in which two of the columns contains X/Y.  
    - Referece catalog. A matrix in which two of the columns contains X/Y.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'SearchRadius' - Search radius. Default is 3.  
    'IsCatSorted' - Is Cat sorted by Y column. Default is false.  
    'ColCatX' - Column of X coordinate in Cat. Default is 1.  
    'ColCatY' - Column of Y coordinate in Cat. Default is 2.  
    'ColRefX' - Column of X coordinate in Ref. Default is 1.  
    'ColRefY' - Column of Y coordinate in Ref. Default is 2.  
    Output : - A structure containing the following fields:  
    .Ind - Ind as returned by VO.search.search_sortedY_multi  
    .MatchedInd - MatchedInd as returned by VO.search.search_sortedY_multi  
    - Matched catalog. Each source is matched to a source in Ref  
    (line by line). NaN if source doesnt exist in Ref.  
    - Matched Ref. (the input Ref catalog).  
    By : Eran Ofek                     Aug 2020  
    Example: Ref=rand(2000,2).*1024;  
    Cat=Ref(1:1000,:) + randn(1000,2).*0.5; Cat=[Cat;rand(500,2).*1024];  
    [ResM,MatchedCat,Ref] = imUtil.cat.match_sources_xy(Cat,Ref)  
      
### imUtil.cat.surface_density

Estimate surface density in catalog, optionally using convex hull Package: +imUtil.cat Description: Estimate the surface density and area that a catalog is covering. The area is estimated using either the box size, circle radius, and if not provided using the convex hull.


    
    Estimate surface density in catalog, optionally using convex hull  
    Package: +imUtil.cat  
    Description: Estimate the surface density and area that a catalog is  
    covering. The area is estimated using either the box size,  
    circle radius, and if not provided using the convex hull.  
    Input  : - A catalog (row per source). If convex hull is used then the  
    first two columns of the catalog are [Long, Lat].  
    - Either radius or [HalfWidth, HalfHeight] that the catalog  
    covers. If not provided, then will use the convex hull  
    algorithm to estimate he area.  
    Output : - Surface density of sources in catalog.  
    - Area that is being covered by the catalog. The arae unist are  
    the radius/HalfSize area units or the units of the Long/Lat  
    coordinates.  
    By: Eran O. Ofek                         Apr 2020  
    Example: [Density,Area]=imUtil.cat.surface_density(rand(1000,2))  
      
