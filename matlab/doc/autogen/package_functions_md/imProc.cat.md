# Package: imProc.CAT


### imProc.CAT.applyProperMotion

Apply proper motion and parallax to sources in AstroCatalog object


    
    Apply proper motion and parallax to sources in AstroCatalog object  
    Input  : - An AstroCatalog object  
    - Catalog epoch (e.g., in Julian years or JD).  
    If empty, will attempt to use the catalog epoch column.  
    - Output epoch.  
    * ...,key,val,...  
    'ColEpochIn' - If catalog epoch is not provided (empty)  
    then this is the column name from which to read the input  
    epoch.  
    'EpochInUnits' - Units of input epoch ('mjd','jd','j').  
    Default is 'jd'.  
    'EpochOutUnits' - Units of output epoch ('mjd','jd','j').  
    Default is 'jd'.  
    'ApplyPlx' - A logical indicating if to apply parallax.  
    Requires the VSOP87 data directory.  
    Defaut is false.  
    'ColRA' - Default is Obj(1).DefNamesRA  
    'ColDec' - Default is Obj(1).DefNamesDec  
    'ColPM_RA' - Default is Obj(1).DefNamesPMRA  
    'ColPM_Dec' - Default is Obj(1).DefNamesPMDec  
    'ColRV' - Default is Obj(1).DefNamesRV  
    'ColPlx' - Default is Obj(1).DefNamesPlx  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    Output : - An AstroCatalog object with the RA/Dec at the new epoch.  
    Author : Eran Ofek (May 2021)  
    Requires: VSOP87 data directory  
    Example: C = catsHTM.cone_search('GAIADR2',1,1,1000,'OutType','astrocatalog');  
    Result = imProc.cat.applyProperMotion(C, 2015,2021,'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',0)  
      
      
### imProc.CAT.filterForAstrometry

Given two catalogs, match their surface density and filter sources. Description: Given two catalogs (e.g., Cat and Ref), clean the catalogs by removing NaN coordinates, imUtil.cat.flag_overdense_colrow, imUtil.cat.flag_overdense, estimate their density using imUtil.cat.surface_density, and


    
    Given two catalogs, match their surface density and filter sources.  
    Description: Given two catalogs (e.g., Cat and Ref), clean the catalogs  
    by removing NaN coordinates,  
    imUtil.cat.flag_overdense_colrow, imUtil.cat.flag_overdense,  
    estimate their density using imUtil.cat.surface_density, and  
    equalize their surface density by removing the faintest  
    sources in one of the catalogs using  
    imUtil.cat.dilute_cat_by_mag  
    Input  : - A catalog with [X,Y,Mag] columns.  
    Either a matrix, or AstroCatalog or AstroImage object.  
    - A Ref catalog wiyh [X,Y,Mag] columns.  
    Either a matrix, or AstroCatalog or AstroImage object.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'CatRemoveNaN' - A logical indicating if to remove NaN  
    coordinates from the Cat. Default is true.  
    'CatRemoveBadColRow' - A logical indicating if to remove  
    source in overdense columns/rows  
    from the Cat. Default is true.  
    'CatRemoveOverDense' - A logical indicating if to remove  
    source in overdense regions  
    from the Cat. Default is true.  
    'RefRemoveNaN'  - A logical indicating if to remove NaN  
    coordinates from the Ref. Default is false.  
    'RefRemoveBadColRow' - A logical indicating if to remove  
    source in overdense columns/rows  
    from the Ref. Default is true.  
    'RefRemoveOverDense' - A logical indicating if to remove  
    source in overdense regions  
    from the Ref. Default is false.  
    'EqualizeDensity' - A logical indicating if to equalize the  
    surface density of the two catalogs.  
    'DiluteThreshold' - If the surface density of the Ref minus  
    Cat divided by Cat (abs value) is larger than this  
    factor then applay source diluation.  
    Default is 0.5.  
    'ColRowPar' - A cell array of addotional parameters to pass to  
    imUtil.cat.flag_overdense_colrow.  
    Default is {}.  
    'OverdensePar' - A cell array of addotional parameters to pass to  
    imUtil.cat.flag_overdense  
    Default is {}.  
    'CatHalfSize' - Either radius, or [half width, half height] of  
    the Cat catalog. If empty, then estimate area using  
    convex hull. Default is empty.  
    'RefHalfSize' - Either radius, or [half width, half height] of  
    the Ref catalog. If empty, then estimate area using  
    convex hull. Default is empty.  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    'ColCatX' - X coordinates column index/name in the Cat.  
    Default is AstroCatalog.DefNamesX.  
    'ColCatY' - Y coordinates column index/name in the Cat.  
    Default is AstroCatalog.DefNamesY.  
    'ColCatMag' - Mag column index/name in the Cat.  
    Default is AstroCatalog.DefNamesMag.  
    'ColRefX' - X coordinates column index/name in the Ref.  
    Default is AstroCatalog.DefNamesX.  
    'ColRefY' - Y coordinates column index/name in the Ref.  
    Default is AstroCatalog.DefNamesY.  
    'ColRefMag' - Mag column index/name in the Ref.  
    Default is AstroCatalog.DefNamesMag.  
    Output : - Cleaned Cat.  
    - Cleaned Ref.  
    - Summary of number of sources survived after each step.  
    Author : Eran Ofek (Jun 2021)  
    Example: [Cat,Ref]=imProc.cat.filterForAstrometry(rand(100,3).*1000,rand(200,3).*1000);  
      
### imProc.CAT.fitPeakMultipleColumns

Given N columns with some property (e.g., S/N) fit a parabola to the S/N values as a function of some parameter (e.g., FWHM), and return the peak S/N value and position. The best fitted S/N and position, as well as the best S/N column value and index are optionally wrotten to the AstroCatalog.


    
    Given N columns with some property (e.g., S/N) fit a parabola  
    to the S/N values as a function of some parameter (e.g., FWHM), and  
    return the peak S/N value and position.  
    The best fitted S/N and position, as well as the best S/N column  
    value and index are optionally wrotten to the AstroCatalog.  
    If the S/N is outside of bounderies or have minimum instead of  
    maximum, return NaN (default).  
    Input  : - Either an AstroCatalog, AstroImage (with AstroCatalog), or  
    a matrix.  
    * ...,key,val,...  
    'Cols' - Column names from which to get the values.  
    Default is {'SN_1','SN_2','SN_3'}.  
    'Pos' - A vector of positions, correspinding to 'Cols'.  
    This paramaeter must be provided and length must  
    equal to the numbre of 'Cols'. Default is [].  
    'FlagBad' - A logical indicating if to put NaNs in  
    MaxFitPos which is minima instaed of maxima, and to  
    replace the MaxFitPos by the nearest bound if the position  
    is out of 'Pos' bounds. Default is true.  
    'InsertBestVal' - Insert Best (max) value. Default is true.  
    'InsertBestInd' - Insert the index of the column  
    corresponding to the best (max) value.  
    Default is true.  
    'InsertMaxFitVal' - Inset fitted max value.  
    Default is true.  
    'InsertMaxFitPos' -  Inset fitted max pos.  
    Default is true.  
    'InsertErrMaxFitPos' - Insert error in fitted pos.  
    Default is true.  
    'ColNameBestVal' - Best val column name.  
    Default is 'BestSN'.  
    'ColNameBestInd' - Best column index.  
    Default is 'IndBestSN'.  
    'ColNameMaxVal' - Fitted max value column name.  
    Default is 'FitBestSN'.  
    'ColNameMaxPos' - Fitted max pos column name.  
    Default is 'FotPosBestSN'.  
    'ColNameMaxPosErr' - Fitted max error pos column name.  
    Default is 'FitPosErrBestSN'.  
    'Pos' - Position in which to insert the new columns.  
    Default is Inf.  
    'CreateNewObj' - A logical indicating if to create a new  
    copy of the AstroCatalog (only) object.  
    Default is false.  
    Output : - A structure array with the following fields:  
    'MaxFitPos' - Fitted pos.  
    'MaxFitVal' - Fitted val.  
    'ErrFitPos' - Error in fitted pos. Nan for maxima.  
    'BestVal' - Best val.  
    'BestInd' - Best ind.  
    'Flag' - A structure with out of bound/min/max flags.  
    - The same type as the input (but only AstroCatalog |  
    AstroImage are supported). These contains the new columns.  
    If CreateNewObj=true, only the Astrocatalog content is  
    copied.  
    Author : Eran Ofek (Oct 2021)  
    Example: X=rand(100,3);  
    [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(X, 'Pos',[1 2 3])  
    AC = AstroCatalog({X}, 'ColNames',{'SN_1','SN_2','SN_3'});  
    [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(AC, 'Pos',[1 2 3])  
    AI=AstroImage('PTF_Cropped.fits');  
    imProc.sources.findMeasureSources(AI, 'PsfFunPar',{[0.1; 1.2; 3]});  
    [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(AI, 'Pos',[0.1 1.2 3])  
      
      
### imProc.CAT.getAstrometricCatalog

Get Astrometric catalog from local/external database and optionally apply proper motion, parallax and units conversions.


    
    Get Astrometric catalog from local/external database  
    and optionally apply proper motion, parallax and units conversions.  
    Input  : - J2000.0 R.A. [rad, deg, [H M S], or sexagesimal string]  
    - J2000.0 Dec. [rad, deg, [Sign D M S], or sexagesimal string]  
    * ...,key,val,...  
    'CatName' - Catalog name. Default is 'GAIAEDR3'.  
    If AstroCatalog, then will return the catalog as  
    is.  
    'CatOrigin' - Catalog origin. Default is 'catsHTM'.  
    'Radius' - Search radius. Default is 1000.  
    'RadiusUnits' - Search radius units. Default is 'arcsec'.  
    'CooUnits' - Search RA/Dec units (this isused only if  
    RA/Dec are numerical scalars). Default is 'deg'.  
    'Shape' - Search shape. Not implemented. Currently will  
    return all sources in cone.  
    'OutUnits' - Output catalog units. Default is 'rad'.  
    'Con' - Search constraings for catsHTM.  
    E.g., {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}.  
    Default is {}.  
    'UseIndex' - UseIndex paramter for catsHTM.  
    Default is false.  
    'EpochOut' - Output epoch. Default units is 'JD' (see  
    imProc.cat.applyProperMotion for more options).  
    If empty, will not apply proper motion and  
    parallax.  
    Default is [].  
    'EpochIn' - If given, then will override catalog epoch.  
    Default units are 'JD'.  
    'argsProperMotion' - A cell array of additional arguments  
    to pass to imProc.cat.applyProperMotion.  
    Default is {}.  
    'ColNameMag' - Column name containing mag.  
    Default is {'Mag_BP','Mag'}.  
    'RangeMag' - Magnitude range to retrieve.  
    Default is [12 19.5].  
    'ColNamePlx' - Parallax column name.  
    Default is {'Plx'}.  
    'RangePlx' - Parllax range to retrieve.  
    Default is [-Inf 50].  
    'OutRADecUnits' - Output units for the RA and Dec output  
    arguments. Default is 'rad'.  
    'RemoveNeighboors' - A logical indicating if to remove  
    sources with close neighboors. Default is true.  
    'flagSrcWithNeighborsArgs' - A cell array of additional  
    arguments to pass to flagSrcWithNeighbors.  
    Default is {}.  
    Output : - An AstroCatalog object with the astrometric catalog.  
    - The input RA [units from 'OutRADecUnits'].  
    - The input Dec [units from 'OutRADecUnits'].  
    Author : Eran Ofek (Jun 2021)  
    Example: Result = imProc.cat.getAstrometricCatalog(1,1);  
      
      
### imProc.CAT.insertAzAlt

Calculate and insert Az, Alt, AirMass, ParAng columns to AstroCatalog object


    
    Calculate and insert Az, Alt, AirMass, ParAng columns to AstroCatalog object  
    Input  : - An AstroCatalog or AstroImage object.  
    * ...,key,val,...  
    'JD' - A vector of JDs (per image/catalog). If empty, will  
    attempt to get the JD from the image header.  
    Default is [].  
    'ObsCoo' - A two column matrix of obs. coordinates  
    [Lon Lat]. If empty, will attempt to get  
    coordinates from header.  
    'ObsCooUnits' - Default is 'deg'.  
    'InsertAzAlt' - Insert Az and Alt. Default is true.  
    'InsertAM' - Insert AirMass. Default is true.  
    'InsertPA' - Insert Paralactic angle. Default is true.  
    'AirMassAlgo' - 'csc'|['hardie].  
    'ColPos' - Column position at which to adde the new  
    columns. Default is Inf.  
    'ColNameAzAlt' - Names of the new Az/Alt columns.  
    Default is {'Az','Alt'}.  
    'ColUnitsAzAlt' - Units for Az/Alt colums.  
    Default is 'deg'.  
    'ColNameAM' - Name of new AirMass column. Default is 'AM'.  
    'ColNamePA' - Name of the new paralactic angle column.  
    Default is 'ParAng'.  
    'ColUnitsPA' - Units of PA column. Default is 'deg'.  
    'juldayArgs' - Cell array of additional arguments to pass  
    to the julday function.  
    'getObsCooArgs' - Add. arguments to pass to getObsCoo.  
    'getLonLatArgs' - Add. arguments o pass yo getLonLat.  
    'TypeLST' - LST type. ['m'] - mean; 'a'-apparent.  
    'CreateNewObj' - [], true, false.  
    If true, create new deep copy  
    If false, return pointer to object  
    If [] and Nargout0 then do not create new copy.  
    Otherwise, create new copy.  
    Default is [].  
    Output : - An updated or new copy of the input catalog with the new  
    columns.  
    Author : Eran Ofek (Sep 2021)  
    Example: AC = AstroCatalog({rand(100,2)},'ColNames',{'RA','Dec'},'ColUnits',{'rad','rad'});  
    imProc.cat.insertAzAlt(AC, 'JD',2451545, 'ObsCoo',[35 32]);  
      
### imProc.CAT.unitTest

unitTest for imProc.cat Example: imProc.cat.unitTest


    
    unitTest for imProc.cat  
    Example: imProc.cat.unitTest  
      
