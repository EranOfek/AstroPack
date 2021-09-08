# Package: imProc.flat


### imProc.flat.deflat

Divide by flat (and construct if needed) from a list of images Treat a single Filter images only.


    
    Divide by flat (and construct if needed) from a list of images  
    Treat a single Filter images only.  
    Input  : - An AstroImage object. Either containing images  
    from which to divide the flat, or all the images  
    including the flat images.  
    All the images are assumed to be bias/dark  
    subtracted.  
    - A flat image. If empty, will attempt to construct  
    the flat image. Default is [].  
    * ...,key,val,...  
    'BitDictinaryName' - A BitDictionary name.  
    If empty, will use existing BitDictionary.  
    Note, that if BitDictionary doesn't exist  
    and not provided, the function will fail.  
    Default is 'BitMask.Image.Default' (located  
    in the config/ directory).  
    'IsFlat' - A function handle for a function that  
    selects and validates flat images  
    (e.g., @imProc.flat.isFlat).  
    Alternatively, a vector of logicals  
    indicating which image is a flat  
    image. If empty use all images.  
    Default is @imProc.flat.isFlat.  
    'FlatArgs' - A cell array of additional arguments  
    to pass to the flat method.  
    Default is {}.  
    'CCDSEC' - A CCDSEC on which to operate the bias  
    subtraction (will be used both on the bias  
    and target images). If empty, use the  
    entire image. Default is [].  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    Output : - An AstroImage object containing the non-flat  
    images after division by flat and mask propagation.  
    - A flat image.  
    - A vector of logicals indicating flat images.  
    - A vector of logicals indicating non-flat images.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage('LAST.*_dark.fits');  
    Bias = imProc.dark.bias(AI);  
    AI = AstroImage('LAST.*_twflat.fits');  
    imProc.flat.isFlat(AI)  
    imProc.dark.debias(AI,Bias);   note that with CreateNewObj it fails  
    [Flat,IsFlat,CoaddN]=imProc.flat.flat(AI);  
    A = AstroImage('LAST.2.1.2_20200821.020129.546_clear_0_science.fits');  
    imProc.dark.debias(A, Bias);  
    imProc.flat.deflat(A, Flat);  
      
### imProc.flat.flat

Generate a super flat image from a set of flat images. A flat image will be generated for each Filter


    
    Generate a super flat image from a set of flat images.  
    A flat image will be generated for each Filter  
    Input  : - An AstroImage object with multiple images.  
    * ...,key,val,...  
    'BitDictinaryName' - A BitDictionary name.  
    If empty, will use existing BitDictionary.  
    Note, that if BitDictionary doesn't exist  
    and not provided, the function will fail.  
    Default is 'BitMask.Image.Default' (located  
    in the config/ directory).  
    'IsFlat' - A function handle for a function that  
    selects and validates flat images  
    (e.g., @imProc.flat.isFlat).  
    Alternatively, a vector of logicals  
    indicating which image is a flat  
    image. If empty use all images.  
    The function must be a method of Flat.  
    Default is @imProc.flat.isFlat.  
    'IsBiasArgs' - A cell array of arguments to pass  
    to the IsFlat function. Default is {}.  
    'StackMethod' - For options, see  
    imProc.image.Stack.coadd).  
    Default is 'sigmaclip'.  
    'StackArgs' - A cell array of arguments to pass to the  
    method function. Default is  
    {'MeanFun',@nanmean, 'StdFun','std', 'Nsigma',[5 5], 'MaxIter',1}.  
    'EmpiricalVarFun' - Default is @var.  
    'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.  
    'DivideEmpiricalByN' - A logical indicating if to divide  
    CoaddVarEmpirical by N. Default is false.  
    'FilterKey' - A string of Filter main dictionary name.  
    This is used in order to group the images by  
    filter. If empty, then use all images.  
    Default is 'FILTER'.  
    'getValArgs' - A cell array of arguments to pass  
    to the Header/getVal function. Default is {}.  
    'FlatHighStd_BitName' - FlatHighStd bit name.  
    Default is 'FlatHighStd'.  
    'FlatHighStd_Threshold' - Threshold value for  
    FlatHighStd, in units of the MeanFun value.  
    'FlatHighStd_MeanFun' - Either a function handle  
    or a value. The function handle is used for  
    the calculation of the mean of the image std map.  
    The threshold is in units of this mean  
    value. Default is 1e-2.  
    'FlatLowVal_BitName' - A FlatLowVal bit name.  
    Default is 'FlatLowVal'.  
    'FlatLowVal_Threshold' - Flat low value  
    threshold below to flag as FlatLowVal.  
    Default is 0.5.  
    'NaN_BitName' - A NaN bit name.  
    Default is 'NaN';  
    'AddHeader' - A 3 column cell array to add to  
    header. Default is {}.  
    'AddHeaderPos' - Position of the added header.  
    Default is 'end'.  
    'SumExpTime' - A logical indicating if to sum  
    (true) or take the mean (false) of the  
    EXPTIME header keyword. Default is false.  
    Output : - An arrray of AstroImage containing the Flat image per  
    filter.  
    - A vector of logical indicating which images were  
    used.  
    - A matrix of the number of images used in each  
    pixel.  
    If multiple filters, then this corresponds to the last  
    filter.  
    Example: AI = AstroImage('LAST.*_dark.fits')  
    Bias = imProc.dark.bias(AI);  
    AI = AstroImage('LAST.*_twflat.fits');  
    imProc.flat.isFlat(AI)  
    imProc.dark.debias(AI,Bias);   note that with CreateNewObj it fails  
    [Flat,IsFlat,CoaddN]=imProc.flat.flat(AI);  
      
### imProc.flat.isFlat

Check and validate that a set of images in an AstroImage object are flat images


    
    Check and validate that a set of images in an AstroImage object are flat images  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'Template' - A template image with the same size  
    of the input image. This can be either a  
    matrix or an AstroImage object.  
    If this is an AstroImage it may include a  
    variance image.  
    'TemplateVar' - A variance image. If provided, and  
    the template is an AstroImage, this will  
    override the content of the variance image  
    in the AstroImage.  
    'Nsigma' - Threshold in units of number of sigmas.  
    Defualt is 5.  
    'MaxFracBadPixels' - If the fraction of pixels  
    which value deviates from the template by  
    more/less than Nsigma is larger than this  
    threshold then the output FlagBad will be  
    set to true. Default is 0.0001.  
    'UseImageVar' - A logical indicating if to add the  
    AstroImage variance to the template  
    variance. If false, use only the template  
    variance. Default is true.  
    'ImTypeKeyName' - IMTYPE header keyword name.  
    Default is 'IMTYPE'.  
    'FilterKey' - A string of Filter main dictionary name.  
    This is used in order to generate the cell of filter names.  
    Default is 'FILTER'.  
    Additional parameters to pass yo isImType.  
    Output : - A vector of logical indicating if an  
    image is a validate flat image.  
    - A structure containing vector of logicals for  
    individaul tests.  
    - A cell array of filters (per image).  
    Author : Eran Ofek (May 2021)  
    Example: A=AstroImage('LAST.*_dark.fits');  
    [Result,Flag] = imProc.flat.isFlat(A)  
      
### imProc.flat.unitTest

unitTest for imProc.image.Flat


    
    unitTest for imProc.image.Flat  
    Example : Result = imProc.image.Flat.unitTest;  
      
      
