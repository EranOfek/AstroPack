# Package: imProc.background


### imProc.background.background

Calculate background and variance of an AstroImage object.


    
    Calculate background and variance of an AstroImage object.  
    Input  : - An AstroImage object multi elements supported).  
    * ...,key,val,...  
    'BackFun' - A function handle for the background (and  
    optionally variance) estimation.  
    The function is of the form:  
    [Back,[Var]]=Fun(Matrix,additional parameters,...),  
    where the output Variance is optional.  
    The additional parameters are provided by the  
    'BackFunPar' keyword (see next keyword).  
    Default is @median [other example:  
    @imUtil.background.mode]  
    'BackFunPar' - A cell array of additional parameters to pass  
    to the BackFun function.  
    Default is {[1 2]} [other example: {true,true,0.1}]  
    'VarFun' - A function handle for the background estimation.  
    The function is of the form:  
    [Var]=Fun(Matrix,additional parameters,...).  
    The additional parameters are provided by the  
    'VarFunPar' keyword (see next keyword).  
    If NaN, then will not calculate the variance.  
    If empty, then will assume the variance is returned as  
    the second output argument of 'BackFun'.  
    If a string then will copy Back value into the Var.  
    Default is empty (i.e., @imUtil.background.rvar returns  
    the robust variance as the second output argument).  
    'VarFunPar' - A cell array of additional parameters to pass  
    to the VarFun function.  
    Default is {}.  
    'SubSizeXY' - The [X,Y] size of the partitioned sub images.  
    If 'full' or empty, use full image.  
    Default is [128 128].  
    'Overlap' - The [X,Y] additional overlaping buffer between  
    sub images to add to each sub image.  
    Default is 16.  
    'ExtendFull' - A logical indicating if to extend the  
    background map into a full-size image. Default is true.  
      
    'SubBack' - A logical indicating if to subtract the  
    background from the image. Default is false.  
    'KeepScaled' - A logical indicating if to rescale the  
    background and variance images to the full size.  
    I.e., this corresponds to how the images are stored  
    in the ImageComponent class.  
    Default is false.  
    'ReCalcBack' - A logical indicating if to recalculate the  
    background even if exist. Default is true.  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    'AddHeaderInfo' - A logical indicating if to add header  
    keywords with background and variance statistics.  
    Including the following keys: {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR'};  
    Default is true.  
    There are some additional hidden arguments.  
    Example: AI = AstroImage({rand(1024,1024)});  
    Result = imProc.background.background(AI);  
      
### imProc.background.filterSources

Generate a background image filtered from sources using sucessive filtering The image is populated in the Back field or returned as an IBackImage object. Description: This routine filter out sources from an image and


    
    Generate a background image filtered from sources using sucessive filtering  
    The image is populated in the Back field or returned as an  
    IBackImage object.  
    Description: This routine filter out sources from an image and  
    generate an approximate background image.  
    The following steps are implemented:  
    1. Filtering the image against template bank of sources.  
    The first guess back and var will be taken from the Back  
    and Var fields.  
    2. Set the value around sources found by step 1 to NaN.  
    3. Use inpaint_nans to interpolate over NaNs.  
    4. Smooth the image using another filter (default is annulus).  
    5. if Niter>1 go to step 1, and find sources this time using  
    the newly estimated background.  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'DataProp' - Data property on which to filter the sources.  
    Default is 'Image'.  
    'UpdateBack' - A logical indicating if to update the  
    'Back' property in the AstroImage with the filtered  
    image. Default is true.  
    'UpdateVar' - A logical indicating if to update the  
    'Var' property in the AstroImage with the filtered  
    image. Default is true.  
    Note that this is meaningful, only if gain was set  
    to 1.  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    'Threshold' - Sources detection threshold (sigmas).  
    Default is 3.  
    'BackFun' - Function handle for background estimation (if  
    not available in the Back field).  
    Default is @median.  
    'BackFunArgs' - A cell array of additional arguments to  
    pass to the background estimation function.  
    Default is {[1 2],'omitnan'}.  
    'VarFun' - Function handle for variance estimation (if not  
    avaialble in the Var field).  
    Default is @imUtil.background.rvar.  
    'VarFunArgs' - A cell array of additional arguments to  
    pass to the variance estimation function.  
    Default is {}.  
    'Template' - A template for filtering in the source  
    finding step, or a function handle for generating  
    templates. The template is a matrix or a cube of  
    templates (all will be filtered).  
    Default is imUtil.kernel2.gauss.  
    'TemplateArgs' - A cell array of arguments to pass to the  
    Template filter function. Default is {[0.1 2 3].'}.  
    'RadiusNaN' - Radius in pixels which will be replaced with  
    NaN around all the found sources (i.e., pixels with  
    S/N>Threshold). Default is 3.  
    'InpaintMethod' - inpaint_nans method (see inpaint_nans  
    for details). Default is 2.  
    'SmoothFilter' - A final smoothing filter.  
    Either a 2D matrix, or a function handle to generate  
    the filter, or empty.  
    If empty, skip this step and do not smooth the  
    background image.  
    Default is @imUtil.kernel2.annulus  
    'SmoothFilterArgs' - A cell array of arguments to pass to  
    the SmoothFilter funcion. Default is {[16 32]}.  
    'PadMethod' - Padding before convolution method.  
    Default is 'symmetric'.  
    'VarFromBack' - If true, for the next iteration use Var=Back.  
    Otherwise, use the origibal Var image.  
    'Niter' - Number of iterations. Default is 1.  
    Output : - An updated AstroImage object.  
    - An optional BackImage object with the smoothed background  
    images.  
    Author : Eran Ofek (Jul 2021)  
    Example: AI=AstroImage('PTF_Cropped.fits');  
    AI=cast(AI,'double'); AI.Image = AI.Image.*1.6;  
    imProc.background.filterSources(AI)  
      
### imProc.background.fitSurface

Fit a surface to a 2D image, with sigma clipping The X/Y coordinates are normalized prior to fitting.


    
    Fit a surface to a 2D image, with sigma clipping  
    The X/Y coordinates are normalized prior to fitting.  
    Input  : - An AstroImage or ImageComponent object.  
    * ...,key,val,...  
    'DataProp' - data property on which to operate.  
    Default is 'Image'.  
    'VecX' - Vector of X positions of the pixels in the image.  
    If empty, then use (1:StepXY:SizeIJ(2)).  
    Default is empty.  
    'VecY' - Vector of Y positions of the pixels in the image.  
    If empty, then use (1:StepXY:SizeIJ(1)).  
    Default is empty.  
    'SizeIJ' - [I, J] size of full image. If empty, then use  
    size(Image). Default is empty.  
    'StepXY' - Steps on values to fit. Default is 1.  
    'Fun' - A cell array of 2D functionals to fit:  
    Default is { @(x,y)ones(size(x)), @(x,y)x, @(x,y)y, @(x,y)2.*x.^2-1, @(x,y)2.*y.^2-1, @(x,y)x.*y }  
    'Niter' - Number of iterations. Use >1 for sigma clipping.  
    Default is 2.  
    'SigmaClip' - SIgma clipping value. Default is 3.  
    Output : - A structure array of the results including  
    the functionals (Fun), the fitted parameters (Par), and  
    the RMS.  
    The parameters are for the coeficients of the functionals  
    after coordinate normalization.  
    - An ImageComponent of surface values.  
    Author : Eran Ofek (Jun 2021)  
    Example: [MatX, MatY] = meshgrid( (1:1:1000), (1:1:1000) );  
    Z = 2+MatX +MatY + MatX.*MatY;  
    AI = AstroImage({Z});  
    [Result, Surface] = imProc.background.fitSurface(AI)  
    VecX = (11:10:990).';  
    VecY = VecX;  
    [MatX, MatY] = meshgrid( VecX, VecY);  
    Z = 2+MatX +MatY + MatX.*MatY;  
    AI = AstroImage({Z});  
    [Result, Surface] = imProc.background.fitSurface(AI, 'SizeIJ',[1000 1000], 'VecX',VecX, 'VecY',VecY);  
      
      
### imProc.background.unitTest

unitTest for the +imProc.background package Example: imProc.background.unitTest


    
    unitTest for the +imProc.background package  
    Example: imProc.background.unitTest  
      
    background  
