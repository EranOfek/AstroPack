# Package: imProc.trans


### imProc.trans.fitPattern

Match two catalogs using stars pattern and return approximate transformation Description: Given two catalogs that coordinadte systems are related by flip, scale, rotation and shift, search the the approximate affine transformation that is required in order to align the coordinate systems of


    
    Match two catalogs using stars pattern and return approximate transformation  
    Description: Given two catalogs that coordinadte systems are related  
    by flip, scale, rotation and shift, search the the approximate  
    affine transformation  
    that is required in order to align the coordinate systems of  
    the two catalogs. The search is done by matching patterns in  
    one catalog to the other.  
    Input  : - An AstroImage containing AstroCatalog or an AstroCatalog,  
    object, or a two column matrix with [X,Y] positions.  
    - A reference catalog (like the first input argument).  
    Both first and second arguments may be a single or  
    multiple element objects.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'ColNamesX' - Cell array of dictioray for X column names  
    for both catalogs.  
    Default is AstroCatalog.DefNamesX  
    'ColNamesY' - Cell array of dictioray for Y column names  
    for both catalogs.  
    Default is AstroCatalog.DefNamesY  
    'Scale' - The scale to apply to the referepnce catalog in order  
    to convert it to the input catalog. If this is a  
    scalar, then will not attempt to search for the best  
    scaleing. If a two element vector, then these are the  
    [min, max] scale range to search.  
    'HistDistEdgesRotScale' - [MinDist, MaxDist, NumberOf points] for  
    distance histogram.  
    Default is [10 600 300].  
    'HistRotEdges' - Edges for angle axis of the histogram.  
    If a scalar is provided then this rotation will be  
    assumed.  
    Default is (-90:0.2:90).'.  
    'RangeX' - [Min Max] range of X shifts to test.  
    If empty, then will select automaticall the maximal  
    possible range.  
    Alternatively, this can be a vector of X-axis  
    histogram edges.  
    Default is [-1000 1000].  
    'RangeY' - [Min Max] range of Y shifts to test.  
    If empty, then will select automaticall the maximal  
    possible range.  
    Alternatively, this can be a vector of Y-axis  
    histogram edges.  
    Default is [-1000 1000].  
    'StepX' - X-axis step size for the histogram calculation.  
    If empty, then will use RangeX as a vector of edges.  
    Default is 3.  
    'StepY' - Y-axis step size for the histogram calculation.  
    If empty, then will use RangeY as a vector of edges.  
    Default is 3.  
    'Flip' - A two column matrix of all possible flips to test.  
    Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].  
    Default is [1 1].  
    'SearchRadius' - Searchj radius for final source matching  
    [pix]. Default is 4.  
    'MaxMethod' - he method by which the 2D histogram peaks will  
    be selected. The following options are available:  
    'thresh' - Select maxima larger than some threshold.  
    'max1' - Select the highest maxima.  
    'maxall' - Select all the maxima.  
    'max_fracmax' - Select all the maxima above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    'thresh_fracmax' - Select all the maxima above the  
    threshold and above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    Alternatively this can be a positive integer (N).  
    In this case will return the N highest maxima.  
    Default is 'thresh_fracmax'  
    'Threshold' - Detection threshold.  
    If PeakMethod is 'sn' then this has units of S/N.  
    Otherwise, this is the number of matches.  
    Default is 8.  
    'Conn' - local maxima finding connectivity parameter.  
    For details see imUtil.image.local_maxima.  
    Default is 8.  
    'FracOfMax' - The parameter that that used in 'max1frac' and  
    'sn' PeakMethod, for selecting peaks.  
    Only peaks that above the maximal peak multiplied by  
    this parameter will be returned.  
    'BackFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @median.  
    'BackFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {'all','omitnan'}.  
    'VarFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @imUtil.background.rvar.  
    'VarFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {}.  
    'SubSizeXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [128 128].  
    'OverlapXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [16 16].  
    'MinVariance' - The minimum variance in in the 2D histogram,  
    That is used to calculate the S/N.  
    Default is 1.  
    'FilterSigma' - Width [sigma units] of Gaussian filter with  
    which to cross-correlate the H2 (hits for shifts) matrix.  
    If empty, no filtering is applied. Default is 3.  
    Output : - A structure of possible solutions for matching between the two  
    catalogs. Follwoing fields are available:  
    .SN  
    .MaxVal  
    .Flip  
    .Rot  
    .Scale  
    .ShiftX - The shift in X one need to add to Ref in order to  
    get Cat.  
    .ShiftY - The shift in Y one need to add to Ref in order to  
    get Cat.  
    .AffineTran - A cell array of affine matrix transformations  
    Apply this transformation to Ref using imUtil.cat.affine2d_transformation  
    In order to get Cat.  
    This is the rotation transformation for the reference frame  
    and not the reference coordinates.  
    - A structure of matching steps, including ResShift and ResRot.  
    ResShift is documented in  
    imUtil.patternMatch.find_shift_pairs, while ResRot in imUtil.patternMatch.find_rot_pairs  
    or imUtil.patternMatch.find_scalerot_pairs.  
    - A structure containing the matched sources for each solution.  
    The following fields are available:  
    'MatchedCat - [X,Y] of the sources in Cat matched to  
    Author : Eran Ofek (May 2021)  
    Example: Result = imProc.trans.fitPattern(Obj1, Obj2, Args)  
      
### imProc.trans.fitTransformation

Fit an exact transformation between two matched catalogs The returned transformation is from the reference to the catalog.


    
    Fit an exact transformation between two matched catalogs  
    The returned transformation is from the reference to the catalog.  
    Input  : - Catalog in one of the following formats:  
    1. AstroImage object containing an AstroCatalog.  
    2. AstroCatalog object  
    3. A 3-column matrix [X,Y,Mag].  
    The fit is done one to many or many to many.  
    - Like catalog, but for the reference.  
    * ...,key,val,...  
    'Tran' - A Tran2D object containing the required  
    transformation to fit. Defaut is Tran2D.  
    'MaxIter' - Maximum number of fitting iterations.  
    In the second iteration, the observations are  
    weighted by their mean residual as a function of  
    magnitude. Defaut is 2.  
    'ErrPos' - The positional errors to be used per source  
    in the first iteration [pixels]. In the second iteration,  
    these errors will be added in quadrature to the  
    mean residuals as a function of magnitude.  
    Default is 1e-5.  
    'Norm'  - empty - do nothing, NaN - auto, or [centerX, RangeX, centerY, RangeY]  
    Default is [].  
    'FitMethod' - Options are 'lscov' | '\'. Default is 'lscov'.  
    'Algo' - Options are 'orth' | 'chol'. Default is 'chol'.  
    resid_vs_mag:  
    'MagRange' - Mag range. If empty no limit. Default is [].  
    'MaxResid' - Maximum allowed residuals [pix]. Default is 1.  
    'BinMethod' - Binning method: 'bin'|'poly'. Default is 'bin'  
    'PolyDeg' - Poly order. Default is 3.  
    'BinSize' - Bin size [mag]. Default is 1.  
    'FunMean' - Function handle for mean in in bin.  
    Defaut is @nanmedian  
    'FunStd' - Function handle for std in bin.  
    Default is @imUtil.background.rstd  
    'InterpMethod' - Interpolation method. Default is 'linear'  
    'ThresholdSigma' - Threshold in sigmas. Default is 3.  
    column names:  
    'ColCatX  - X column name in Catalog. Default is AstroCatalog.DefNamesX  
    'ColCatY' - Y column name in Catalog. Default is AstroCatalog.DefNamesY  
    'ColCatM' - Mag column name in Catalog. Default is {'MAG','mag','MAG_PSF','MAG_CONV'}  
    'ColRefX' - X column name in Reference. Default is AstroCatalog.DefNamesX  
    'ColRefY' - Y column name in Reference. Default is AstroCatalog.DefNamesY  
    'ColRefM' - Mag column name in Reference. Default is {'MAG','mag','MAG_PSF','MAG_CONV'}  
    'ColRefC' - Color column name in Reference. Default is {'Color'}  
    'ColRefAM' - AirMass column name in Reference. Default is {'AIRMASS'}  
    'ColRefPA' - Par Ang. column name in Reference. Default is {'ParAng'}  
    'ColRefModX' - mod(X) column name in Reference. Default is {'ModX'}  
    'ColRefModY' - mod(Y) column name in Reference. Default is {'ModY'};  
    Output : - A structure array of fitted parameters.  
    - A structure array of results.  
    - A Tran2D object for each image.  
    Example: Nsrc = 1000;  
    Cat = rand(Nsrc,3).*[1024 1024 10];  
    Ref = Cat + 0.1.*randn(Nsrc,3);  
    Ref = [Ref, rand(Nsrc,1).*2];  
    T   = Tran2D;  
    [Param, Res] = imProc.trans.fitTransformation(Cat, Ref, 'Tran',T);  
      
      
### imProc.trans.projection

project Lon/Lat to X/Y using specified projection


    
    project Lon/Lat to X/Y using specified projection  
    Input  : - A matrix, AstroImage with catalog, or AstroCatalog object.  
    This is the input catalog containing some  
    Longitude/Latitude data to be projected.  
    - (Lon0) - The center longitude of projection.  
    - (Lat0) - The center latitude of projection.  
    - (Scale) - Scale radius (for e.g., 'gnomonic projection').  
    Default is 1.  
    - Projection type - options are:  
    Gnomonic projection: {'tan','gnomonic','tpv','tan-sip','sip'}  
    ...  
    * ...,key,val,...  
    'Coo0Units' - Units for the Lon0/Lat0 coordinates.  
    Default is 'deg'.  
    'AddNewCols' - A cell array of column names to add with  
    the projected X/Y coordinates. If empty, then the  
    projected coordinates will be stored in the Lon/Lat  
    input coordinates. Default is {'X','Y'}.  
    'Pos' - Position of new columns. Default is Inf.  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    'ColLon' - A cell array of column names in which to look  
    for the longitude. If input is a matrix, then this  
    will be always the first column. Default is  
    AstroCatalog.DefNamesRA.  
    'ColLat' - A cell array of column names in which to look  
    for the latitude. If input is a matrix, then this  
    will be always the first column. Default is  
    AstroCatalog.DefNamesDec.  
    'ColUnits' - A cell array of units for the lon/lat  
    columns. This is used only in the input is a matrix.  
    Default is {'deg','deg'}.  
    Output : - An AstroCatalog or AstroImage object in which the catalog  
    is updated with the projected coordinates.  
    Author : Eran Ofek (Jun 2021)  
    Example: Result = imProc.trans.projection(rand(100,2), 0.5, 0.5, 1, 'tan', 'Coo0Units','rad', 'ColUnits',{'rad','rad'})  
    Result = imProc.trans.projection(rand(100,2), 0.5, 0.5, 180./pi, 'tan', 'Coo0Units','deg', 'ColUnits',{'deg','deg'})  
      
### imProc.trans.projectionInv

project X/Y to Lon/Lat using specified projection


    
    project X/Y to Lon/Lat using specified projection  
    Input  : - A matrix, AstroImage with catalog, or AstroCatalog object.  
    This is the input catalog containing some  
    X/Y data to be projected.  
    - (Lon0) - The center longitude of projection.  
    - (Lat0) - The center latitude of projection.  
    - (Scale) - Scale radius (for e.g., 'gnomonic projection').  
    Default is 1.  
    - Projection type - options are:  
    Gnomonic projection: {'tan','gnomonic','tpv','tan-sip','sip'}  
    ...  
    * ...,key,val,...  
    'Coo0Units' - Units for the Lon0/Lat0 coordinates.  
    Default is 'deg'.  
    'AddNewCols' - A cell array of column names to add with  
    the projected X/Y coordinates. If empty, then the  
    projected coordinates will be stored in the Lon/Lat  
    input coordinates.  
    'Pos' - Position of new columns. Default is Inf.  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    'ColLon' - A cell array of column names in which to look  
    for the longitude. If input is a matrix, then this  
    will be always the first column. Default is  
    AstroCatalog.DefNamesRA.  
    'ColLat' - A cell array of column names in which to look  
    for the latitude. If input is a matrix, then this  
    will be always the first column. Default is  
    AstroCatalog.DefNamesDec.  
    'OutUnits'  
    - A cell array of units for the lon/lat  
    columns. This is used only in the input is a matrix.  
    Default is {'deg','deg'}.  
    Output : - An AstroCatalog or AstroImage object in which the catalog  
    is updated with the projected coordinates.  
    Author : Eran Ofek (Jun 2021)  
    Example: Result = imProc.trans.projectionInv(rand(100,2), 0.5, 0.5, 1, 'tan', 'Coo0Units','rad');  
    Result = imProc.trans.projectionInv(rand(100,2), 0.5, 0.5, 180./pi, 'tan', 'Coo0Units','deg');  
      
      
### imProc.trans.tranAffine

Apply affine transformation to an AstroCatalog object


    
    Apply affine transformation to an AstroCatalog object  
    Input  : - An AstroCatalog or AstroImage (with a catalog) object or  
    a matrix.  
    If a matrix, then assumes that it contains two columns of  
    [x, y] coordinates.  
    - An affine transformation in one of the following formats:  
    1. A 3x3 affine transformation matrix.  
    2. An affine2d class.  
    3. [ShiftX, ShiftY]  
    4. [ShiftX, ShiftY, Rotation]  
    5. [ShiftX, ShiftY, Rotation, Scale]  
    6. [ShiftX, ShiftY, Rotation, Scale, FlipX, FlipY]  
    - A logical indicating if to perform a forward  
    transformation (true) or backward (false).  
    Default is true (forward transformation).  
    * ...,key,val,...  
    'RotUnits' - Units for the rotation angle (if provided).  
    Default is 'rad'.  
    'ColX' - A cell array of column nnames for the X  
    coordinates. Will choose the first to appear.  
    Default is AstroCatalog.DefNamesX.  
    'ColY' - A cell array of column nnames for the Y  
    coordinates. Will choose the first to appear.  
    Default is AstroCatalog.DefNamesY.  
    'CreateNewObj' - - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    Output : - An AstroCatalog object in which the coordinates are  
    transformed.  
    Author : Eran Ofek (Jun 2021)  
    Example: Result = imProc.trans.tranAffine(rand(100,2), [1 1], true)  
      
      
### imProc.trans.unitTest

unitTest for +imProc.match Example: imProc.trans.unitTest


    
    unitTest for +imProc.match  
    Example: imProc.trans.unitTest  
      
    imProc.trans.matchPattern  
    only shift  
