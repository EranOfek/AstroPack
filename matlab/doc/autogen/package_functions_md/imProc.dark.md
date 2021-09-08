# Package: imProc.dark


### imProc.dark.bias

Generate a super bias image from a s et of bias images.


    
    Generate a super bias image from a s et of bias images.  
    Input  : - An AstroImage object with multiple images.  
    * ...,key,val,...  
    'BitDictinaryName' - A BitDictionary name.  
    If empty, will use existing BitDictionary.  
    Note, that if BitDictionary doesn't exist  
    and not provided, the function will fail.  
    Default is 'BitMask.Image.Default' (located  
    in the config/ directory).  
    'IsBias' - A function handle for a function that  
    selects and validates bias/dark images  
    (e.g., @imProc.dark.isBias, @isDark).  
    Alternatively, a vector of logicals  
    indicating which image is a bias/dark  
    image. If empty use all images.  
    The function must be a method of Dark.  
    Default is @imProc.dark.isBias.  
    'IsBiasArgs' - A cell array of arguments to pass  
    to the  IsBias function. Default is {}.  
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
    'getValArgs' - A cell array of arguments to pass  
    to the Header/getVal function. Default is {}.  
    'LowRN_BitName' - LowRN bit name.  
    This bit flag pixels which variance is  
    larger than Threshold*RN^2, where RN is the  
    ReadNoise.  
    Default is 'LowRN'.  
    'LowRN_Threshold' - Threshold value.  
    Default is 0.05.  
    'LowRN_MeanFun' - A string, a function handle or  
    numerical value. If string then this is an  
    header keyword name, and will attempt to  
    look for this keyword (using the getVal  
    function). If numerical value, than assume  
    this is the RN. If a function handle than  
    this function will be applied on the  
    variance image to estimate the RN.  
    Default is @median.  
    'HighRN_BitName'  
    This bit flag pixels which variance is  
    smaller than Threshold*RN^2, where RN is the  
    ReadNoise.  
    Default is 'HighRN'.  
    'HighRN_Threshold' - Threshold value.  
    Default is 10.  
    'HighRN_MeanFun' - Like LowRN, buit for the  
    HighRN bit.  
    Default is @median.  
    'DarkHighVal_BitName' - Bit name for high  
    dark/bias values. Defined as image values  
    larger than Threshold*Mean, where Mean is  
    the image mean.  
    Default is 'DarkHighVal'.  
    'DarkHighVal_Threshold' - Threshold value.  
    Default is 2.  
    DarkLowVal_BitName' - Bit name for low  
    dark/bias values. Defined as image values  
    smaller than Threshold*Mean, where Mean is  
    the image mean.  
    Default is 'DarkLowVal'.  
    'DarkLowVal_Threshold' - Threshold value.  
    Default is 0.2.  
    'BiasFlaring_BitName' - Bit name for flaring  
    pixels identified using  
    identifyFlaringPixels.  
    'BiasFlaring_Threshold' - A threshold value  
    (number of sigma above mean). Default is 20.  
    'BiasFlaringArgs' - A cell array of additional  
    arguments to pass to identifyFlaringPixels.  
    Default is {}.  
    'AddHeader' - A 3 column cell array to add to  
    header. Default is {}.  
    'AddHeaderPos' - Position of the added header.  
    Default is 'end'.  
    'SumExpTime' - A logical indicating if to sum  
    (true) or take the mean (false) of the  
    EXPTIME header keyword. Default is false.  
    Output : - An AstroImage containing the bias/dark image.  
    - A vector of logical indicating which images were  
    used.  
    - A matrix of the number of images used in each  
    pixel.  
    Example: A=AstroImage('LAST.*_dark.fits')  
    Bias = imProc.dark.bias(A)  
      
### imProc.dark.compare2template

Compare AstroImage to a template and variance and flag image which are different than the template.


    
    Compare AstroImage to a template and variance and flag image  
    which are different than the template.  
    Input  : - An AstroImage object containing images.  
    The comparison is done 1 to 1, 1 to many, or many  
    to 1.  
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
    'DataProp' - Data property in the AstroImage.  
    Default is 'Image'.  
    'VarProp' - Variance property in the template  
    image. Default is 'Var'.  
    Output : - A column vector of logicals indicating if the  
    fraction of bad pixels (above or below threshold)  
    is above MaxFracBadPixels.  
    - A column vector of the fraction of bad pixels in  
    each image.  
    - An ImageComponent containing the Z image.  
    (i.e., Image-Template)/sqrt(Var).  
    Author : Eran Ofek (Apr 2021)  
    Example: AI = AstroImage({2.*randn(10,10)});  
    Template = AstroImage({0},'Var',{4});  
    [FlagBad, FracbadPixels, Z] = imProc.dark.compare2template(AI, 'Template',Template)  
      
### imProc.dark.debias

Subtract bias (and construct if needed) from a list of images


    
    Subtract bias (and construct if needed) from a list of images  
    Input  : - An AstroImage object. Either containing images  
    from which to subtract the bias, or all the images  
    including the bias images.  
    - A bias image. If empty, will attempt to construct  
    the bias/dark image. Default is [].  
    * ...,key,val,...  
    'BitDictinaryName' - A BitDictionary name.  
    If empty, will use existing BitDictionary.  
    Note, that if BitDictionary doesn't exist  
    and not provided, the function will fail.  
    Default is 'BitMask.Image.Default' (located  
    in the config/ directory).  
    'IsBias' - A function handle for a function that  
    selects and validates bias/dark images  
    (e.g., @imProc.dark.isBias, @isDark).  
    Alternatively, a vector of logicals  
    indicating which image is a bias/dark  
    image. If empty use all images.  
    Default is @imProc.dark.isBias.  
    'BiasArgs' - A cell array of additional arguments  
    to pass to the bias method.  
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
    Output : - An AstroImage object containing the non-bias  
    images after bias subtraction and mask propagation.  
    - A bias image.  
    - A vector of logicals indicating bias images.  
    - A vector of logicals indicating non-bias images.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage('LAST.2.1.2_20200821.015445.457_clear_0_science.fits');  
    AB = imProc.dark.debias(AI,Bias);  
      
### imProc.dark.identifyFlaringPixels

Identify flaring pixels in a cube of images Searched by looking at (Cube-Mean)/Std>Threshold


    
    Identify flaring pixels in a cube of images  
    Searched by looking at (Cube-Mean)/Std>Threshold  
    Input  : - A cube of images. Usulally the image index is in  
    the 3rd dimension.  
    * ...,key,val,...  
    'MeanFun' - Either a number, a matrix or a  
    function handle by which to calculate the  
    mean function. Default is @median.  
    'MeanFunArgs' - A cell array of arguments to pass  
    to the mean function.  
    Default is {3,'omitnan'}.  
    'MaxFun' - A function handle by which to calculate  
    the max of the data. Default is @max.  
    'MaxFunArgs' - A cell array of arguments to pass  
    to the max function.  
    Default is {[],3}.  
    'StdFun' - Either a number, a matrix or a  
    function handle by which to calculate the  
    mean function. Default is @imUtil.background.rstd  
    'StdFunArgs' - A cell array of arguments to pass  
    to the std function.  
    Default is {3}.  
    'Threshold' - Threshold above to flag the pixel.  
    Default is 10.  
    Output : - A matrix of logicals indicating pixels that are  
    above the flaring threshold.  
    - A matrix of the mean values.  
    - A matrix of the std values.  
    - A matrix of the max values.  
    Author : Eran Ofek (May 2021)  
    Example: Cube = randn(100,100,10); Cube(1,1,1)=30;  
    [Result,Mean,Std,Max] = imProc.dark.identifyFlaringPixels(Cube);  
    [Result,Mean,Std,Max] = imProc.dark.identifyFlaringPixels(Cube,'MeanFunArgs',{'all'});  
      
### imProc.dark.identifySimilarImages

Search for sucessive images with a fraction of identical pixel values This is useful in order to identify problems with the detector firmware (i.e., some regions of the detector in two surcessive images are identical due to a readout


    
    Search for sucessive images with a fraction of identical pixel values  
    This is useful in order to identify problems with the  
    detector firmware (i.e., some regions of the detector in  
    two surcessive images are identical due to a readout  
    problem).  
    Input  : - An AstroImage or ImageComonent object.  
    * ...,key,val,...  
    'DataProp' - Data property containing the image.  
    Default is 'Image'.  
    'MaxAllowedFrac' - The fraction of identical  
    pixels above to set the output argument  
    PassedThreshold to true.  
    Default is 0.2.  
    Output : - PassedThreshold. True if the fraction of identical  
    pixels in two sucessive images is larger than the  
    MaxAllowedFrac threshold.  
    - An array of the fraction of identical pixels in  
    and image and the sucessive image.  
    The last value is always NaN.  
    Author : Eran Ofek  
    Example: AI=AstroImage({rand(1000,1000), rand(1000,1000)});  
    [a,b]=imProc.dark.identifySimilarImages(AI)  
      
### imProc.dark.isBias

Check and validate that a set of images in an AstroImage object are bias images


    
    Check and validate that a set of images in an AstroImage object are bias images  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'MaxAllowedFrac' - The fraction of identical  
    pixels above to set the output argument  
    PassedThreshold to true.  
    This parameter is passed to identifySimilarImages  
    Default is 0.2.  
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
    Additional parameters to pass yo isImType.  
    Output : - A vector of logical indicating if an  
    image is a validate bias/dark image.  
    - A structure containing vector of logicals for  
    individaul tests.  
    Author : Eran Ofek (May 2021)  
    Example: A=AstroImage('LAST.*_dark.fits');  
    D=imProc.image.Dark;  
    [Result,Flag] = D.isBias(A)  
      
### imProc.dark.isDark

Check and validate that a set of images in an AstroImage object are dark images


    
    Check and validate that a set of images in an AstroImage object are dark images  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'MaxAllowedFrac' - The fraction of identical  
    pixels above to set the output argument  
    PassedThreshold to true.  
    This parameter is passed to identifySimilarImages  
    Default is 0.2.  
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
    Additional parameters to pass yo isImType.  
    Output : - A vector of logical indicating if an  
    image is a validate bias/dark image.  
    - A structure containing vector of logicals for  
    individaul tests.  
    Author : Eran Ofek (May 2021)  
    Example: A=AstroImage('LAST.*_dark.fits');  
    [Result,Flag] = imProc.dark.isDark(A)  
      
### imProc.dark.overscan

Create overscan images and optionally subtract from images


    
    Create overscan images and optionally subtract from images  
    Input  : - An AstroImage object of images that contains an  
    overscan region.  
    * ...,key,val,...  
    'OverScan' - Either an header keyword containing  
    the overscan region, or an [Xmin Xmax Ymin Ymax]  
    vector for the overscan.  
    Default is 'OVERSCAN'.  
    'Subtract' - A logical indicating if to subtract  
    the overscan from the image. Default is true.  
    'RemoveOverScan' - A logical indicating if to crop  
    out the overscan region. Default is true.  
    'RemoveOthers' - a logical indicating if to crop  
    also the Back, Var, and Mask fields.  
    Default is true (independent of RemoveOverScan).  
    'OverScanDir' - Indicating the direction of the overscan:  
    'x'|'y'|1|2| [].  
    'x', or 2 means that the overscan is read along  
    the x-axis (meaning the overscan is in  
    columns).  
    'y', or 1 means that the overscan is read along  
    the y-axis.  
    [] - choose the direction automatically,  
    where the smaller dimension of the overscan  
    region is assumed to be the direction.  
    Default is [].  
    'Method' - Method by which to calculate the overscan:  
    'globalmedian' - global median (default).  
    'globalmean' - global mean.  
    'median' - median long the overscan  
    'mean' - mean along the overscan  
    'medmedfilt' - median along the overscan  
    followed by median filtering  
    smoothing. Smoothing default is {50}.  
    'medsgolayfilt' - median along the overscan  
    followed by Svitzk-Golay filtering.  
    Smoothing default is {3 50}.  
    'MethodArgs' - A cell array of additional  
    arguments to pass to the method.  
    (Defaults are defined for each medthod).  
    'UpdateCCDSEC' - A logical indicating if to update  
    the CCDSEC header keyword in the header.  
    Will also update NAXIS* keywords.  
    Default is true.  
    'KeyCCDSEC' - Header keyword CCDSEC. Default is 'CCDSEC'.  
    'DataProp' - Data property on which to operate.  
    Defaultis 'ImageData'.  
    'DataPropIn' - Data property, in the image component, on which to operate.  
    Defaultis 'Image'.  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    Output : - An AstroImage object with the overscan subtracted  
    (and croped) images.  
    - OverScanAI is an AstroImage object that stores the  
    overscan image in the 'ImageData' field, and the  
    calculated (global or line) overscan in the  
    'BackData' field.  
    Author : Eran Ofek (May 2021)  
    Example: [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[1 10 1 9600])  
    [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[6379 6388 1 9600])  
    [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[6379 6388 1 9600],'Method','medmedfilt')  
      
