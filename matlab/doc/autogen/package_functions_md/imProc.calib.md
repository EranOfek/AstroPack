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

Apply an absolute photometric calibration to AstroCatalog Given an AstroCatalog or AstroImage with a catalog, match the sources against a photometric catalog, and calculate the zero point (ZP) of the catalog.


    
    Apply an absolute photometric calibration to AstroCatalog  
    Given an AstroCatalog or AstroImage with a catalog, match the  
    sources against a photometric catalog, and calculate the zero  
    point (ZP) of the catalog.  
    Inout  : -  
    * ...,key,val,...  
      
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
      
    'ColNameMag' - Column name containing mag.  
    Default is {'Mag_BP','Mag'}.  
    'RangeMag' - Magnitude range to retrieve.  
    Default is [12 19.5].  
    'ColNamePlx' - Parallax column name.  
    Default is {'Plx'}.  
    'RangePlx' - Parllax range to retrieve.  
    Default is [-Inf 50].  
      
    Output :  
    Author :  
    Example:  
      
### imProc.calib.unitTest

unitTest for imProc.calib Example: imProc.calib.unitTest


    
    unitTest for imProc.calib  
    Example: imProc.calib.unitTest  
      
