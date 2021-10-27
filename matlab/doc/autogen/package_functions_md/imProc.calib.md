# Package: imProc.calib


### imProc.calib.gainCorrect

Divide image by gain and update header.


    
    Divide image by gain and update header.  
    Input  : - An AstroImage object.  
    - A char array of Gain header keyword name, or a numeric  
    array of gain values (scalar, or one per image).  
    Default is 'GAIN';  
    * ...,key,val,...  
    'CreateNewObj' - [], true, false. Default is [].  
    'getValArgs' - A cell array of additional arguments to  
    pass to AstroHeader/getVal. Default is {}.  
    'DataProp' - AstroImage data properties which to divide by  
    gain. Default is {'Image','Var','Back'}.  
    'replaceValArgs' - A cell array of additional arguments to  
    pass to AstroHeader/replaceVal. Default is {}.  
    'OrigGainKey' - An header keyword name in which to write the  
    original gain. Default is 'ORIGGAIN'  
    'DefaultGain' - The default gain in case can't find gain  
    in header. Default is 1.  
    Output : - An AstroImage object with gain=1 and updated header.  
    NOTE: The catalog is not modified.  
    Author : Eran Ofek (Jul 2021)  
    Example: AI = AstroImage({rand(10,10)});  
    imProc.calib.gainCorrect(AI)  
      
### imProc.calib.photometricZP

Calculate an absolute photometric calibration to AstroCatalog Given an AstroCatalog or AstroImage with a catalog, match the sources against a photometric catalog, and calculate the zero point (ZP) of the catalog.


    
    Calculate an absolute photometric calibration to AstroCatalog  
    Given an AstroCatalog or AstroImage with a catalog, match the  
    sources against a photometric catalog, and calculate the zero  
    point (ZP) of the catalog.  
    Input  : - An AstroImage or AstroCatalog object.  
    * ...,key,val,...  
    'Radius' - Matching radius between the catalog and  
    reference catalog. Default is 3.  
    'RadiusUnits' - Units for the 'Radius' argument.  
    Default is 'arcsec'.  
    'Method' - Zero point calculation method:  
    'Simple' - Fit a ZP + color term + width term  
    model.  
    'UseOnlyMainSeq' - A logical indicating if to use only  
    Main Sequence stars. Default is false.  
    'MaxErr' - Max error of stars to use in solution.  
    Default is 0.02 mag.  
    'MaxSN' - Max S/N of sources to use.  
    Default is 1000.  
    'CatColNameMag' - Mag. column name in Catalog.  
    This magnitude will be calibrated.  
    Default is 'MAG_CONV_3'.  
    'CatColNameMagErr' - Mag. error column name in Catalog.  
    Default is 'MAGERR_CONV_3'.  
    'CatColNameSN' - S/N column name in Catalog.  
    Default is 'SN_3'.  
    'LimMagSN' - S/N for lim. mag. calculation.  
    Default is 5.  
    'LimMagColor' - Color in which to calculate the lim. mag.  
    Default is 1.  
      
    'RefColNameMag' - Mag. column name in reference catalog.  
    Default is 'Mag_BP'.  
    'RefColNameMagErr' - Mag. error column name in ref. catalog.  
    Default is 'ErrMag_BP'.  
    'RefColNameMagBands' - A cell array of mag column names  
    from which to calculate the colors.  
    If a single column then, Color is calculated  
    from 'RefColNameMag' - 'RefColNameMagBands'  
    If multiple columns than take the diff along the  
    second dimension. Default is {'Mag_RP','Mag_G'}.  
    'RefColNameMagBandsErr' - Column names of mag. errors  
    corresponding to 'RefColNameMagBands'.  
    Default is {'ErrMag_RP','ErrMag_G'}.  
      
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
    'UseIndex' - UseIndex paramter for catsHTM.  
    Default is false.  
      
    'RangeMag' - Magnitude range to retrieve.  
    Default is [12 19.5].  
    'ColNamePlx' - Parallax column name.  
    Default is {'Plx'}.  
    'RangePlx' - Parllax range to retrieve.  
    Default is [-Inf 50].  
      
    'UpdateMagCols' - A logical indicating if to update the  
    magnbitude columns with the ZP.  
    NOTE: This will only add the ZP (without the other  
    terms) - we call this the telescope natural mag.  
    system. Default is true.  
    'MagColName2update' - Either a char array containing a  
    string to match to all other columns, and columns containing  
    this string will be updated, or a cell array of  
    column names to update. Default is 'MAG_'.  
      
    'matchReturnIndicesArgs' - A cell array of additional  
    arguments to pass to imProc.match.matchReturnIndices  
    Default is {}.  
    'CreateNewObj' - A logical indicating if to copy the input  
    object. Default is false.  
    'Plot' - A logical indicating if to plot  
    Output : - The input object, with the (possible) mag. column names  
    updated.  
    - A structure array with the calibration fit results.  
    - An AstroCatalog object containing all the retrieved  
    photometric catalogs.  
    Author : Eran Ofek (Oct 2021)  
    Example: [Result, ZP, PhotCat] = imProc.calib.photometricZP(SI(1))  
    [Result, ZP, PhotCat] = imProc.calib.photometricZP(SI,'UseOnlyMainSeq',0,'Plot',1, 'CreateNewObj',1);  
      
### imProc.calib.selectMainSequenceFromGAIA

Select main sequence stars from GAIA catalog in AstroCatalog object.


    
    Select main sequence stars from GAIA catalog in AstroCatalog object.  
    Input  : - An AstroCatalog object containing a GAIA EDR3 catalog  
    (e.g., using catsHTM).  
    * ...,key,val,...  
    'CreateNewObj' - true|false. Create a new copy of the input catalog.  
    Default is true.  
    Selection parameters  
    'PlxLimit' - Default is 1 mas  
    'PlxSNLimit' - Default is 10.  
    'ExcessNoiseLimit' - Default is 500 micro arcsec.  
    'ExcessNoiseSigLimit' - Default is 2.  
    Fitting parameters  
    'ParMS' - A vector of main-sequence abs mag vs. color  
    polynomials. If empty, will fit polynomial.  
    Default is [].  
    'PolyOrder' - Polynomial order to fit. Default is 9.  
    'DiffClip' - A vector of difference clipping.  
    In each iteration will clip sources with residuals  
    larger than the corresponding value.  
    Default is [2 1 0.3].  
    'ColorLimit' - Color boundries. Default is [0.5 2].  
    'Plot - A logical indicating if to plot the best fit data.  
    Output : - An AstroCatalog object with the selected main sequence  
    stars.  
    - As tructure array containing a 'Flag' field for the  
    selected rows per catalog.  
    - A structure array with the best fitted polynomial  
    parameters.  
    Author : Eran Ofek (Sep 2021)  
    Example: DataSampleDir = tools.os.getTestDataDir; cd(DataSampleDir);  
    GM = io.files.load2('AstrometricCat_PTF_Cropped.mat');  
    [Result, FlagRes, Par] = imProc.calib.selectMainSequenceFromGAIA(GM)  
      
### imProc.calib.unitTest

unitTest for imProc.calib Example: imProc.calib.unitTest


    
    unitTest for imProc.calib  
    Example: imProc.calib.unitTest  
      
