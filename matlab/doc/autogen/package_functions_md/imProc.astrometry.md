# Package: imProc.astrometry


### imProc.astrometry.addCoordinates2catalog

Add or update RA/Dec coordinates in catalogs in AstroImage/Astrocatalog


    
    Add or update RA/Dec coordinates in catalogs in AstroImage/Astrocatalog  
    Input  : - An AstroImage or AstroCatalog object containing a catalog.  
    * ...,key,val,...  
    'WCS' - An AstroWCS object. If not given will taken from  
    the AstroImage.WCS data. Default is [].  
    'UpdateCoo' - A logical indicating if to update the RA/Dec  
    columns if already exist. Default is false.  
    'OutUnits' - Output RA/Dec units. Default is 'rad'.  
    'includeDistortion' - Include distortions. Default is true.  
    'useTran2D' - Use Tran2D object. Default is false.  
    'ColNameRA' - RA column name to insert. Default is 'RA'.  
    'ColNameDec' - Dec column name to insert. Default is 'Dec'.  
    'Pos' - Column poistion. Default is Inf.  
    'CreateNewObj' - Create a new copy of the object.  
    Default is false.  
    'DicNamesX' - Dictionary X column name.  
    Default is AstroCatalog.DefNamesX.  
    'DicNamesY' - Dictionary Y column name.  
    Default is AstroCatalog.DefNamesY.  
    'DicNamesRA' - Dictionary RA column name.  
    Default is AstroCatalog.DefNamesRA.  
    'DicNamesDec' - Dictionary Dec column name.  
    Default is AstroCatalog.DefNamesDec.  
    Output : - An AstroImage or AstroCatalog object with the updataed  
    coordinates.  
    Author : Eran Ofek (Aug 2021)  
    Example: Out = imProc.astrometry.addCoordinates2catalog(AI,'WCS',Result.WCS,'UpdateCoo',true)  
      
### imProc.astrometry.assessAstrometricQuality

Collect information regarding quality of astrometric solution and return a sucess flag.


    
    Collect information regarding quality of astrometric solution and  
    return a sucess flag.  
    Input  : - The ResFit structure returned by Tran2D/fitWCS  
    * ...,key,val,...  
    'MinNumSrc' - Min numbr of sources used in solution.  
    Default is 10.  
    'MinUsedSrcFrac' - Min fraction of sources used for  
    solution. Default is 0.2.  
    'MinRMS' - Min RMS of solution [deg].  
    Default is 0.3./3600.  
    'MinAssymRMS' - Min AssymRMS of solution [deg].  
    Default is 0.2./3600.  
    'MinErrOnMean' - Min error on the maem for solution [deg].  
    Default is 0.05./3600.  
    Output : - A vector of sucess flags (one per ResFit element).  
    - A structure array of collected astrometric quality  
    paramseters.  
    Author : Eran Ofek (Aug 2021)  
    Example: [SucessFlag, QualitySummary] = imProc.astrometry.assessAstrometricQuality(Result.ResFit)  
      
### imProc.astrometry.astrometryCheck

Compare the astrometry of a catalog with a reference astrometric catalog. Return statistics regarding the matched sources, rms, rms as a function of position and mag.


    
    Compare the astrometry of a catalog with a reference astrometric catalog.  
    Return statistics regarding the matched sources, rms, rms as a  
    function of position and mag.  
    Input  : - An AstroCatalog or AstroImage object (with AstroCatalog).  
    * ...,key,val,...  
    'WCS' - An AstroWCS object containing the WCS of the  
    catalog. If given, will override the AstroWCS on  
    the AstroImage. Default is [].  
    'CatName' - Catalog name. Default is 'GAIAEDR3'.  
    If AstroCatalog, then will return the catalog as  
    is.  
    'getAstrometricCatalogArgs' - A cell array of additional  
    arguments to pass to imProc.cat.getAstrometricCatalog  
    'Radius' - Matching radius between the input catalog and  
    the astrometric catalog. Default is 5.  
    'RadiusUnits' - Units for matching radius.  
    Default is 'arcsec'.  
    'IncludeDistortions' - A logical indicating if to use the  
    WCS including the distortions (true), or not (false).  
    Default is true.  
    'MaxNmtach' - When calculating the rms and median of the residuals,  
    do not use sources with number of matches larger than  
    this value. Default is 1.  
    'Nbin' - When calculating the rms vs. X/Y position, this  
    is the number of bins in X and Y. Default is 3.  
    'ColNamesX' - A cell array of dictionary names for X coordinates  
    in the input catalog. Default is AstroCatalog.DefNamesX  
    'ColNamesY' - Like 'ColNamesX', but for the Y axis.  
    Default is AstroCatalog.DefNamesY  
    'ColNamesMag' - Like 'ColNamesX', but for the magnitude  
    Default is AstroCatalog.DefNamesMag  
    Output : - A structure array with an element per input catalog, with  
    the statistical information regarding the matching.  
    The following fields are available:  
    'Nsrc' - Numbre of sources.  
    'NmatchedSrc' - Number of sources with Nmatch>0  
    'VecDist' - Vector of matched distances [arcsec].  
    'VecDeltaRA' - RA diff [arcsec].  
    'VecDeltaDec' - Dec diff [arcsec].  
    'VecNmatch' - Number of matched per source  
    'RMS_RA' - rms RA [arcsec]  
    'RMS_Dec' - rms Dec [arcsec]  
    'RRMS_RA' - robust rms RA [arcsec]  
    'RRMS_Dec' - robust rms Dec [arcsec]  
    'MedDelta_RA' - median of RA diff [arcsec]  
    'MedDelta_Dec' - median of Dec diff [arcsec]  
    'BinN' - Matrix of number of matches in positional bin.  
    'BinMean' - mean dist in positional bin.  
    'BinMedian' - median dist in positional bin.  
    'MagResid' - Output of imUtil.calib.resid_vs_mag  
    Author : Eran Ofek (Jul 2021)  
    Example: AstrometricCat = catsHTM.cone_search('GAIAEDR3',1,1,1000);  
    or load AstrometricCat_PTF_Cropped.mat  
    R=imProc.astrometry.astrometryCheck(AstrometricCat,'CatName',AstrometricCat)  
      
### imProc.astrometry.astrometryCore

A core function for astrometry. Match pattern and fit transformation. The function is designed to solve the astrometry of an image in a single shoot (no partitioning). A new copy of the catalog is always created.


    
    A core function for astrometry. Match pattern and fit transformation.  
    The function is designed to solve the astrometry of an image in  
    a single shoot (no partitioning).  
    A new copy of the catalog is always created.  
    Input  : - An AstroImage with populated CatData or an AstroCatalog object,  
    with sources X, Y positions.  
    This can be a multiple element object. In this case, the  
    RA/Dec coordinate refers to all the images.  
    * ...,key,val,...  
    'RA' - A single J2000.0 RA coordinates  
    in rad, deg, or sexagesimal string.  
    This is a mandatory argument.  
    If first input is an AstroImage this can also be an  
    header keyword name. Default is 'RA'.  
    'Dec' - A single J2000.0 Dec coordinates  
    in rad, deg, or sexagesimal string.  
    This is a mandatory argument.  
    If first input is an AstroImage this can also be an  
    header keyword name. Default is 'DEC'.  
    'CooUnits' - RA/Dec coordinates units ('deg','rad').  
    This is ignored if RA/Dec are sexagesimal.  
    Default is 'deg'.  
    'CatName' - Either an astrometric catalog name (char  
    array) to query around the requested coordinates,  
    or an AstroCatalog object containing such a  
    catalaog.  
    Default is 'GAIAEDR3'.  
    'CatOrigin' - Catalog origin (relevant if CatName is a  
    char array).  
    Default is 'catsHTM'.  
    'CatRadius' - Catalog query radius.  
    If empty will attempt to estimate automatically  
    from the diagonal of the image in pixels, and the  
    max(scale).  
    Default is 1400.  
    'CatRadiusUnits' - CatRadius units.  
    Default is 'arcsec'.  
    'Con' - Additional constraints for the catalog query.  
    See catsHTM.cone_search. Default is {}.  
    'RefColNameMag' - Column name containing mag in the  
    astrometric reference catalog.  
    Default is {'Mag_BP','Mag'}.  
    'RefRangeMag' - Magnitude range to retrieve.  
    Default is [12 19.5].  
    'RefColNamePlx' - Parallax column name in the  
    astrometric reference catalog.  
    Default is {'Plx'}.  
    'RefRangePlx' - Parllax range to retrieve.  
    Default is [-Inf 50].  
    'EpochOut' - Output epoch. Default units is 'JD' (see  
    imProc.cat.applyProperMotion for more options).  
    If empty, will not apply proper motion and  
    parallax.  
    Default is [].  
    'argsProperMotion' - A cell array of additional arguments  
    to pass to imProc.cat.applyProperMotion.  
    Default is {}.  
    'argsFilterForAstrometry' - A cell array of additional  
    arguments to pass to imProc.cat.filterForAstrometry  
    Default is {}.  
    'argsFitPattern' - A cell array of additional arguments to  
    pass to imProc.trans.fitPattern  
    Default is {}.  
    'ProjType' - Projection type. See imProc.trans.projection.  
    Default is 'TPV'.  
    'ImageCenterXY' - [X,Y] in pixels of image center  
    corresponding to the guess coordinates.  
    If empty, will take the 0.5.*max(XY), where XY are  
    the source pixel coordinates.  
    Note that this arguments is in the Result output.  
    Default is [].  
    'Scale' - Value, or range of scale [arcse/pix].  
    Default is 1.0  
    'RotationRange' - Range of rotations to test [deg].  
    Default is [-90, 90].  
    'RotationStep' - Rotation step. Default is 0.2 [deg].  
    'RangeX' - Range of X shift to test [pixels].  
    Default is [-1000 1000].  
    'RangeY' - Range of Y shift to test [pixels].  
    Default is [-1000 1000].  
    'StepX' - Step in X shift histogram. Default is 4.  
    'StepY' - Step in Y shift histogram. Default is 4.  
    'Flip' - A two column matrix of [X, Y] flips to test.  
    Default is [1 1; 1 -1;-1 1;-1 -1].  
    'SearchRadius' - Matching search radius [pixels].  
    Default is 5.  
    'FilterSigma' - Width [sigma units] of Gaussian filter with  
    which to cross-correlate the H2 (hits for shifts) matrix.  
    If empty, no filtering is applied. Default is 3.  
    'Tran' - A Tran2D object describing the 2D transformation  
    to fit.  
    Default is Tran2D.  
    'MaxSol2Check' - Maximum number of solution candidates to  
    test (solutions from fitPattern). Default is 3.  
    'OutCatCooUnits' - Units of RA/Dec added to catalog.  
    Default is 'deg'.  
    'OutCatColRA' - RA Column name added to catalog.  
    Default is 'RA'.  
    'OutCatColDec' - Dec Column name added to catalog.  
    Default is 'Dec'.  
    'OutCatColPos' - Position of RA/Dec columns added to catalog.  
    Default is Inf.  
    'CatColNamesX' - A cell array dictionary of input catalog  
    X column name. Default is AstroCatalog.DefNamesX.  
    'CatColNamesY' - A cell array dictionary of input catalog  
    Y column name. Default is AstroCatalog.DefNamesY.  
    'CatColNamesMag' - A cell array dictionary of input catalog  
    magnitude column name. Default is AstroCatalog.DefNamesMag.  
    'RefColNamesRA' - A cell array dictionary of reference astrometric catalog  
    RA column name. Default is AstroCatalog.DefNamesRA.  
    'RefColNamesDec' - A cell array dictionary of reference astrometric catalog  
    Dec column name. Default is AstroCatalog.DefNamesDec.  
    Output : - A structure array of results.  
    Element per input catalog.  
    Available fields are:  
    'ImageCenterXY' - The [X, Y] image center [pix] used in the solution.  
    'ResPattern' - The structure output with candidate  
    solutions from fitPattern.  
    'Nsolutions' - Number of solutions found by fitPattern.  
    If Nsolutions>0 then the following fields are available  
    'Tran' - A Tran2D object per solution, containing the  
    transformation (after projection).  
    'Res' - A structure array (per solution) with the best  
    fit transformation information, including rms and  
    number of matches.  
    'ErrorOnMean' - A vector of assymptoticRMS/sqrt(Ngood)  
    for each solution.  
    'BestInd' - Index of solution with minimal ErrorOnMean.  
    - An handle to the original input catalog, after adding the  
    RA/Dec columns for all the sources.  
    The input catalog is modified only if two or more output  
    arguments are requested.  
    - An AstroImage or AstroCatalog object containing the astrometric catalog  
    used, after applying proper motions, and queryRange.  
    If the input is AstroImage, this is an AstroImage with the  
    WCS and Headers updated with the new WCS.  
    Author : Eran Ofek (Jul 2021)  
    Example: Result = imProc.astrometry.astrometryCore(AI.CatData, 'RA',149.1026601, 'Dec',69.4547688, 'CatColNamesMag','MAG_CONV_2');  
      
### imProc.astrometry.astrometryImage




    
      
      
      
### imProc.astrometry.astrometryRefine

Refine an astrometric solution of an AstroCatalog object This function may work on images which have either an approximate WCS (either in AstroHeader or AstroWCS), or a catalog with RA/Dec coordinates. The coordinates should be good to a few arcseconds. For no solutions use imProc.astrometry.astrometryCore.


    
    Refine an astrometric solution of an AstroCatalog object  
    This function may work on images which have either an approximate  
    WCS (either in AstroHeader or AstroWCS), or a catalog with RA/Dec  
    coordinates. The coordinates should be good to a few arcseconds.  
    For no solutions use imProc.astrometry.astrometryCore.  
    A new copy of the catalog is always created.  
    Input  : - An AstroCatalog or AstroImage object (multiple elements supported).  
    * ...,key,val,...  
    'SearchRadius' - Sources search radius [arcsec].  
    Default is 3.  
    'IncludeDistortions' - A logical indicating if to include  
    the fitted distortions in estimating the RA/Dec of  
    the sources in the catalog. Default is true.  
    'Header' - An AstroHeader object corresponding to the  
    AstroCatalog. If exist, then will use the  
    AstroWCS.header2wcs function to generate an  
    AstroWCS. If empty, will attempt to use the user  
    supplied AstroWCS in the 'WCS' function key.  
    Default is [].  
    'WCS' - An AstroWCS object. If empty, will look for 'RA',  
    and 'Dec' columns in the AstroCatalog object.  
    Otherwise the RA and Dec will be calculated from  
    the AstroWCS object.  
    Default is [].  
    'RA' - RA corrsponding to catalog/image center. This will be  
    used as the coordinate for the astrometric catalog  
    query and the CRVAL argument.  
    If first input is an AstroImage, this can be a char  
    array containing header keyword name (e.g., 'RA').  
    If empty, and WCS is given then will estimate from  
    WCS. If empty, then will estimate using  
    boundingCircle on the catalog. Default is [].  
    'Dec' - Like Dec, but for the RA. Default is [].  
    'CooUnits' - Units for the RA and Dec function keys.  
    Default is 'deg'.  
    'Scale' - Catalog/image plate scale ["/pix].  
    If empty, will estimate from catalog.  
    Default is [].  
    'ProjType' - Projection type. See imProc.trans.projection.  
    Default is 'TPV'.  
    'TranMethod' - imProc.astrometry.fitWCS  
    transformation method. Default is 'TPV'.  
    'Tran' - A Tran2D object that defines the fitted  
    transformation. Default is Tran2D.  
    'ErrPos' - Error in positions [pix]. Default is 0.01.  
    'ExtraData' - Data for additional fitted parameters.  
    Matrix of e.g., [AM, PA, Color] with the same  
    number of rows as the catalog.  
    Default is [].  
    'Niter' - Number of fitting iterations. Default is 2.  
    'FitMethod' - Fitting method for Tran2D/fitAstrometricTran  
    Default is 'lscov'.  
    'MaxResid' - Maximum residual to use in fit.  
    Default is 0.5.  
    'MagRange' - [Min Max] max range. Default is [13 19].  
    'BinMethod' - Method to use:  
    'poly' - polynomial fit.  
    'bin' - binning the data.  
    Default is 'bin'  
    'PolyDeg' - Polynomial degree for the polynomial fit.  
    Default is 3.  
    'BinSize' - Bin size for binning. Default is 1 (mag).  
    'FunMean' - A function handle to use when calculating the mean  
    of the data in each bin.  
    Default is @tools.math.stat.nanmedian.  
    'FunStd' - A function handle to use when calculating the std  
    of the data in each bin, or when calculating the global  
    std after the polynomial fit.  
    Default is @imUttil.background.rstd.  
    'InterpMethod' - Interpolation method. Default is 'linear'.  
    'ThresholdSigma' - Threshold in sigmas (std) for flagging good  
    data. Default is 3.  
    'CatName' - Either an astrometric catalog name (char  
    array) to query around the requested coordinates,  
    or an AstroCatalog object containing such a  
    catalaog.  
    Default is 'GAIAEDR3'.  
    'CatOrigin' - Catalog origin (relevant if CatName is a  
    char array).  
    Default is 'catsHTM'.  
    'CatRadius' - Catalog query radius.  
    If empty will attempt to estimate automatically  
    from the diagonal of the image in pixels, and the  
    max(scale).  
    Default is 1400.  
    'CatRadiusUnits' - CatRadius units.  
    Default is 'arcsec'.  
    'Con' - Additional constraints for the catalog query.  
    See catsHTM.cone_search. Default is {}.  
    'RefColNameMag' - Column name containing mag in the  
    astrometric reference catalog.  
    Default is {'Mag_BP','Mag'}.  
    'RefRangeMag' - Magnitude range to retrieve.  
    Default is [12 19.5].  
    'RefColNamePlx' - Parallax column name in the  
    astrometric reference catalog.  
    Default is {'Plx'}.  
    'RefRangePlx' - Parllax range to retrieve.  
    Default is [-Inf 50].  
    'EpochOut' - Output epoch. Default units is 'JD' (see  
    imProc.cat.applyProperMotion for more options).  
    If empty, will not apply proper motion and  
    parallax.  
    Default is [].  
    'argsProperMotion' - A cell array of additional arguments  
    to pass to imProc.cat.applyProperMotion.  
    Default is {}.  
    'flagSrcWithNeighborsArgs' - A cell array of additional  
    arguments to pass to the flagSrcWithNeighbors fun.  
    Default is {}.  
    'ReuseAstrometricCat' - A logical indicating if to reuse  
    the astrometric catalog from the first query. This  
    is possible only when all the images/catalogs  
    corresponds to the same sky location.  
    Default is false.  
    'RemoveNeighboors' - A logical indicating if to remove  
    sources with near neighboors from the astrometric  
    catalog. Default is true.  
    'CreateNewObj' - A logical indicating if to create a new  
    copy of the input AstroCatalog object.  
    Default is true.  
    'OutCatCooUnits' - Units of RA/Dec added to catalog.  
    Default is 'deg'.  
    'OutCatColRA' - RA Column name added to catalog.  
    Default is 'RA'.  
    'OutCatColDec' - Dec Column name added to catalog.  
    Default is 'Dec'.  
    'OutCatColPos' - Position of RA/Dec columns added to catalog.  
    Default is Inf.  
    'CatColNamesX' - A cell array dictionary of input catalog  
    X column name. Default is AstroCatalog.DefNamesX.  
    'CatColNamesY' - A cell array dictionary of input catalog  
    Y column name. Default is AstroCatalog.DefNamesY.  
    'CatColNamesMag' - A cell array dictionary of input catalog  
    magnitude column name. Default is AstroCatalog.DefNamesMag.  
    'RefColNamesRA' - A cell array dictionary of reference astrometric catalog  
    RA column name. Default is AstroCatalog.DefNamesRA.  
    'RefColNamesDec' - A cell array dictionary of reference astrometric catalog  
    Dec column name. Default is AstroCatalog.DefNamesDec.  
    Output : - A structure array with the following fields (each element  
    corresponds to an AstroCatalog elelemt):  
    'ParWCS' - The WCS parameters.  
    'Tran' - The fitted Tran2D object.  
    'ResFit' - The best fit results summary.  
    'WCS' - An updated WCS object with the best fit solution.  
    - The input AstroCatalog objct with new and updated  RA/Dec  
    columns. The columns are added only if the second output  
    argument is requested.  
    - An AstroCatalog containing the AstrometricCat catalog.  
    Author : Eran Ofek (Aug 2021)  
    Example: RR = imProc.astrometry.astrometryRefine(AI.CatData, 'WCS',Result.WCS, 'CatName',AstrometricCat, 'RA',149.1026601, 'Dec',69.4547688);  
      
### imProc.astrometry.astrometrySubImages

Solve astrometry for sub images of a single contigious image The solution is done by executing astrometryCore for a limited number of sub images, and astrometryRefine for all the rest, based on the solution from astrometryCore.


    
    Solve astrometry for sub images of a single contigious image  
    The solution is done by executing astrometryCore for a limited  
    number of sub images, and astrometryRefine for all the rest,  
    based on the solution from astrometryCore.  
    Input  : - An AstroImage object with multiple sub images of a  
    contigous field of view.  
    * ...,key,val,...  
    'CCDSEC' - A mandatory argument. This is a 4 column matrix  
    of [Xmin, Xmax, Ymin, Ymax] of the CCDSEC fir each  
    sub image.  
    This is typically obtained from the second output  
    argument of imProc.image.image2subimages.  
    'CenterXY' -  
    'RA'  
    'Dec'  
    'CooUnits'  
    'Scale'  
    'Tran'  
    'EpochOut'  
    'CreateNewObj'  
    'CatName'  
    'astrometryCoreArgs'  
    'astrometryRefineArgs'  
    Output : -  
    Author : Eran Ofek (Aug 2021)  
    Example:  
      
### imProc.astrometry.fitWCS

Perform the Tran2D.fitAstrometricTran and prepare the WCS info This is an auxilary function that performs the fitting stage between an astrometric catalog and an image catalog, and return the Tran2D object as well as the information needed for the WCS (e.g., CRPIX, CRVAL, etc.).


    
    Perform the Tran2D.fitAstrometricTran and prepare the WCS info  
    This is an auxilary function that performs the fitting stage  
    between an astrometric catalog and an image catalog, and return the  
    Tran2D object as well as the information needed for the WCS (e.g.,  
    CRPIX, CRVAL, etc.).  
    Input  : - Xcat - Vector of catalog X coordinates  
    - Ycat - Vector of catalog Y coordinates  
    - Xref - Vector of external reference projected X  
    coordinates  
    - Yref - Vector of external reference projected Y  
    coordinates  
    - Mag - Vector of magnitudes  
    - RAdeg - RA [deg] of the center for projection.  
    - Decdeg - Dec [deg] of the center for projection.  
    * ...,key,val,...  
    'ImageCenterXY'  
    'Scale' - Scale ["/pix]  
    'Flip'  - [X Y] flip operated already on the coordinates.  
    Default is [1 1].  
    'ProjType' - Projection type. See imProc.trans.projection.  
    Default is 'TPV'.  
    'TranMethod' - ['TPV'] | 'tran2d'  
    This dictates the fitting scheme.  
    'Tran' - A Tran2D object for the transformation to fit.  
    Default is Tran2D.  
    'ExtraData' - Additional columns to pass to the Tran2D  
    transformation. Default is [].  
    'ErrPos' - Error in positions [pix].  
    'Niter' - Number of fitting iterations.  
    'FitMethod' - Fitting method for Tran2D/fitAstrometricTran  
    Default is 'lscov'.  
    'MaxResid' - Maximum residual to use in fit.  
    Default is 1.  
    'MagRange' - [Min Max] max range. Default is [].  
    'BinMethod' - Method to use:  
    'poly' - polynomial fit.  
    'bin' - binning the data.  
    Default is 'bin'  
    'PolyDeg' - Polynomial degree for the polynomial fit.  
    Default is 3.  
    'BinSize' - Bin size for binning. Default is 1 (mag).  
    'FunMean' - A function handle to use when calculating the mean  
    of the data in each bin.  
    'FunStd' - A function handle to use when calculating the std  
    of the data in each bin, or when calculating the global  
    std after the polynomial fit.  
    Default is @imUttil.background.rstd.  
    'InterpMethod' - Interpolation method. Default is 'linear'.  
    'ThresholdSigma' - Threshold in sigmas (std) for flagging good  
    data. Default is 3.  
    'TestNbin' - Number of binx in each dimensions, when  
    counting the number of good sources as a function of  
    position in the image. Default is 3.  
    Output : - The Tran2D object with the fitted transformations  
    - (ParWCS) A structure with some of the parameters required  
    in order to build the WCS.  
    - (ResFit) A structure containing information about the fit  
    quality. See Tran2D/fitAstrometricTran for details.  
    Author : Eran Ofek (Jul 2021)  
    Example: imProc.astrometry.fitWCS  
      
### imProc.astrometry.unitTest

unitTest for +imProc.astrometry Example: imProc.astrometry.unitTest


    
    unitTest for +imProc.astrometry  
    Example: imProc.astrometry.unitTest  
      
      
