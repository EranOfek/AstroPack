# Package: imProc.stack


### imProc.stack.applyUnaryFun

Applay scalar-unary function (e.g., function that returns a scalar) on AstroImage


    
    Applay scalar-unary function (e.g., function that returns a scalar) on AstroImage  
    Input  : - An AstroImage object.  
    - An AstroImage object, or a cell array of matrices  
    (images) or scalars, or a vector of scalars, or a  
    function_handle.  
    If an array, then will be converted to a cell of  
    scalars using num2cell. Each cell element will be  
    applied (using the operator) on the AstroImage. If one cell  
    element then will be applied on all images.  
    If function_handle (unary function) then it will  
    be used to calculate the offset using this  
    function.  
    - Operator to use - e.g., subtracting the offset or  
    dividing the factor. Default is @minus).  
    * ...,key,val,...  
    'OpArgs' - A cell array of arguments to pass to  
    the Offset operator (if operator is provided).  
    'PreDivide' - A logical indicating if to take  
    the inverse of the Offset prior to applying  
    the operator. This can be used to speed up  
    performences (in division). Default is false.  
    'DataProp' - The data property in the AstroImage  
    (both input and offset) that contains the data.  
    Default is 'ImageData'.  
    'DataPropIn' - The data property in the  
    ImageComponent that contains the image  
    data. Default is 'Data'.  
    'CreateNewObj' - A logical indicating if the  
    output is a new object. If empty, then will  
    check the number of output argumnets. If  
    zero (e.g., C.subtractOffset(...)) then  
    will update the input image object. Else,  
    will create a new object.  
    Output : - An AstroImage object.  
    Author : Eran Ofek (Apr 2021)  
    Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});  
    R  = imProc.stack.applyUnaryFun(AI,1);  
    R  = imProc.stack.applyUnaryFun(AI,[1 2]);  
    R  = imProc.stack.applyUnaryFun(AI,{1 2});  the same  
    R  = imProc.stack.applyUnaryFun(AI,AI);  
    R  = imProc.stack.applyUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});  
    R  = imProc.stack.applyUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});  
      
      
### imProc.stack.coadd

Coadd images in AstroImage object including pre/post normalization


    
    Coadd images in AstroImage object including pre/post normalization  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'CCDSEC' - CCDSEC on which to operate:  
    [Xmin, Xmax, Ymin, Ymax].  
    Use [] for the entire image.  
    If not [], then DataPropIn/Out will be  
    modified to 'Image'.  
    'DataPropIn' - The data property that contains the  
    the data in the ImageComponent.  
    Default is 'Data'.  
    'Offset' - Either a function handle, a vector, or  
    empty. If function handle, then will apply  
    it to the ImageData to calculate an offset  
    per image. This offset will be subtracted  
    from each image. If vector, then this is a  
    offset value per image. If empty, do not  
    apply offset. Default is [].  
    'OffsetArgs' - A cell array of additional  
    arguments to pass to the offset function.  
    Default is {}.  
    'PreNorm' - Like offset, but for the  
    pre-normalization for the images. The  
    pre-normalization is done after the offset.  
    Default is [].  
    'PreNormArgs' - A cell array of additional  
    arguments to pass to the pre-normalization function.  
    Default is {}.  
    'UseWeights' - A logical indicating if to apply  
    weights. Default is true.  
    'Weights' - A vector of variances (one per image).  
    If empty, then will attempt to use the  
    VarImage image in the AstroImage.  
    Default is [].  
    'StackMethod' - - Stacking method. Options are:  
    'sum'  
    ['mean']  
    'median'  
    'var'  
    'rvar'  
    'min'  
    'max'  
    'range'  
    'quantile' - rquires a quqntile argument.  
    'wmean'  
    'sigmaclip' - for arguments see: imUtil.image.mean_sigclip  
    'wsigmaclip' - for arguments see: imUtil.image.wmean_sigclip  
    'bitor' - bit-wise or operation. Return only Stack.  
    'bitand' - bit-wise and operation. Return only Stack.  
    'bitnot' - bit-wise not operation. Return only Stack.  
    'StackArgs' - A cell array of arguments to pass to the  
    method function. Default is {}.  
    'MaskStackMethod' - Like 'StackMethod', but for the  
    coaddition of the Mask. Default is 'bitor'.  
    'MaskStackArgs' - A cell array of arguments to pass to the  
    mask method function. Default is {}.  
    'CombineBack' - A logical indicating if to  
    combine the background image (using the  
    StackMethod). Default is true.  
    'CombineMask' - A logical indicating if to  
    combine the mask image. Default is true.  
    'EmpiricalVarFun' - Default is @var.  
    'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.  
    'MedianVarCorrForEmpirical' - A logical indicating if to  
    correct the variance calculation by the ratio between  
    the variance of the median and variance of the mean.  
    Default is false.  
    'DivideEmpiricalByN' - A logical indicating if to divide  
    CoaddVarEmpirical by N. Default is false.  
    'PostNorm' - Like offset, but for the  
    post-normalization for the images (a scalar). The  
    post-normalization is done after the stacking.  
    Default is [].  
    'PostNormArgs' - A cell array of additional  
    arguments to pass to the post-normalization function.  
    Default is {}.  
    'HeaderCopy1' - A logical indicating if to copy  
    the header from the 1st coadd image.  
    Default is true.  
    'NewHeader' - An header to add to the coadd  
    image header. This can be a 3 column cell  
    array, an AstroHeader or AstroImage. If  
    empty do nothing. Default is [].  
    'UpdateTimes' - A logical indicatin if to add  
    keywords regarding the number of coadded  
    images and update the EXPTIME and MIDJD.  
    Default is true.  
    'SumExpTime' - A logical indicating if to sum  
    the EXPTIME in the new header, or to use  
    the mean (false). Default is true.  
    Output : - An AstroImage with the coadded image, includinf  
    the coadded background and mask. The VarData is always  
    including the empirical variance.  
    - A matrix in which each pixel give the number of  
    images on which the coaddition was based.  
    - The cube of images  
    Author : Eran Ofek (Apr 2021)  
    Example: AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});  
    [Result, CoaddN] = imProc.stack.coadd(AI);  
      
### imProc.stack.coaddProperCore

Proper coaddition of images in AstroImage object


    
    Proper coaddition of images in AstroImage object  
    Input  : -  
    Output : -  
    Author : Eran Ofek (Jun 2021)  
    Example: UNDER CONSTRUCTION  
      
### imProc.stack.divideFactor

Divide factor (constant) from AstroImage


    
    Divide factor (constant) from AstroImage  
    Input  : - An AstroImage object.  
    - An AstroImage object, or a cell array of matrices  
    (images) or scalars, or a vector of scalars, or a  
    function_handle.  
    If an array, then will be converted to a cell of  
    scalars using num2cell. Each cell element will be  
    divided from the AstroImage. Of one cell  
    element then will be subtracted from all images.  
    If function_handle (unary function) then it will  
    be used to calculate the factor using this  
    function.  
    * ...,key,val,...  
    'OpArgs' - A cell array of arguments to pass to  
    the Offset operator (if operator is provided).  
    'DataProp' - The data property in the AstroImage  
    (both input and offset) that contains the data.  
    Default is 'ImageData'.  
    'DataPropIn' - The data property in the  
    ImageComponent that contains the image  
    data. Default is 'Data'.  
    'CreateNewObj' - A logical indicating if the  
    output is a new object. If empty, then will  
    check the number of output argumnets. If  
    zero (e.g., C.subtractOffset(...)) then  
    will update the input image object. Else,  
    will create a new object.  
    Output : - An AstroImage object.  
    Author : Eran Ofek (Apr 2021)  
    Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});  
    R  = imProc.stack.divideFactor(AI,1);  
    R  = imProc.stack.divideFactor(AI,[1 2]);  
    R  = imProc.stack.divideFactor(AI,{1 2});  the same  
    R  = imProc.stack.divideFactor(AI,AI);  
    R  = imProc.stack.divideFactor(AI,@mean,'OpArgs',{'all'});  
      
### imProc.stack.funCube

Apply function/s on a single cube


    
    Apply function/s on a single cube  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'CCDSEC' - [Xmin Xmax Ymin Ymax] to stack.  
    If empty, use entire image. Default is  
    [].  
    'FunCube' - A function handle, or a cell array of  
    function handles to apply on cube.  
    Default is {@mean, @var}.  
    All the functions are applayed on the same  
    cube. Note that the number of functions  
    actually applied is min(nargout,numel(FunCube)).  
    Default is {@mean, @var}.  
    'FunArgs' - If FunCube is a function handle then  
    this is a cell array of additional  
    arguments to pass to the FunCube function.  
    If FunCube is a cell array of function  
    handles, then this is a cell array of cell  
    arrays of additional arguments for each  
    function.  
    Default is {{3,'omitnan'}, {[],3,'omitnan'}}.  
    'DataProp' - Data property in the AStroImage from  
    which to extract a cube.  
    Default is 'ImageData'.  
    'DataPropIn' - Data property in the ImageComponent  
    from which to extract the image data.  
    Default is 'Data'.  
    'SaveInProp' - A cell array of AstroImage  
    properties (one to one correspondence to  
    FunCube) in which to save the output  
    results.  
    If empty, then the output is matrices.  
    If provided, then the first output argument  
    is an AstroImage with the specific field  
    populated with the results of the  
    corresponding FunCube results.  
    Default is {'ImageData','VarData'}.  
    'DimIndex' - Dimension along which the cube will  
    be constructed. Default is 3.  
    Output : * Arbitrary number of arguments.  
    Each output contains a matrix of a coadd image that  
    corresponds to one 'FunCube' function.  
    Author : Eran Ofek (Apr 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10), rand(10,10)});  
    [Cube1, Cube2] = imProc.stack.funCube(AI);  
    [CAI] = imProc.stack.funCube(AI,'SaveInProp',{'ImageData','VarData'});  
      
### imProc.stack.functionalResponse

Fit the pixel response to light as a function of intensity in a cube of images Description: Given an AstroImage of flat field or dark images which have different mean intensity, fit some linear models to each model intensity vs. a user specified expected intensity or the mean value of the image.


    
    Fit the pixel response to light as a function of intensity in a cube of images  
    Description: Given an AstroImage of flat field or dark images which have different mean  
    intensity, fit some linear models to each model intensity  
    vs. a user specified expected intensity or the mean value of  
    the image.  
    This function can be used to fit the flat/dark image in each  
    pixel, to fit non-linaeity and to look for pixels with  
    anomalous response.  
    The function by default fit a null hypothesis model of the  
    form intensity=alpha*MeanIntensity (where alpha is a fitted  
    free parameter).  
    In addition the function fit additional user specified  
    models (e.g., offset + linear term, or quadratic model).  
    The Delta Chi^2 between these models and the null hypothesis  
    is calculated.  
    Input  : - An AStroImage object.  
    * Pairs of ...,key,val,... arguments. Options are:  
    'CCDSEC' - CCDSEC on which to operate:  
    [Xmin, Xmax, Ymin, Ymax].  
    Use [] for the entire image.  
    If not [], then DataPropIn/Out will be  
    modified to 'Image'.  
    'DataPropIn' - The data property that contains the  
    the data in the ImageComponent.  
    Default is 'Data'.  
    'DataProp' - Data propery to fit. Default is  
    'ImageData'.  
    'Gain' - Gain. The cube will be multiplied by these factor.  
    This is needed beacuse the function assume the images  
    noise is Poisson. Default is 1.  
    'ReadNoise' - Readnoise in electrons, used for the \chi^2  
    calculation. Default is 5.  
    'MeanFun' - If 'Intensity' is not porovided, this is a function  
    handle that will operate on each image in the cube in  
    order to calculate its mean value.  
    Default is @median.  
    'MeanFunPar' - A cella array of additional parameters to pass  
    to the 'MeanFun' function handle.  
    Default is {[1, 2]'omitnan'}.  
    'Intensity' - A vector if intensities for each image in the  
    cube. This can be the mean intensity of each image  
    (after gain correction), or exposure time (if flat is  
    based on constant illumination surface).  
    If empty, then will estimate the intensity using the  
    'MeanFun' option.  
    Default is empty.  
    'Model' - A cell array of additional models to fit.  
    Each element in the cell array is a string that  
    corresponds to one of the following models:  
    'c+x' : - bias + alpha*Intensity model  
    'c+x+X^2' - bias + alpha*I + beta.*I.^2 model  
    'x+x^2' - alpha*I + beta.*I.^2 model  
    Default is {'c+x','c+x+x^2','x+x^2'}.  
    Output : - A structure of the fit results. The following fields are  
    available:  
    .H0 (data for the null hypothesis fit.  
    A structure with the following fields:  
    .Model - Model name - i.e., 'x'  
    .Par.Par - A matrix of the fitted parameters (response  
    image  - i.e., flat field image).  
    .Chi2 - A matrix of \chi^2 per pixel.  
    .Npar - The number of free parameters.  
    .Ndof - The number of degrees of freedom (Nobs-Npar).  
    .ProbChi2 - 1 - The cumulative probability of the chi2,dof.  
    .H1 (data for the alternative hypothsis fits).  
    This is a structure array with element per alternative  
    model:  
    .Model - Model name - e.g., 'c+x'  
    .Par(i).Par - A matrix of the fitted parameters (response  
    image  - i.e., flat field image).  
    where i is the free parameter index.  
    For example, in the 'c+x' model i=1 is for the bias  
    level and i=2 is for the slope (response).  
    .Chi2 - A matrix of \chi^2 per pixel.  
    .Npar - The number of free parameters.  
    .Ndof - The number of degrees of freedom (Nobs-Npar).  
    .ProbDeltaChi2 - 1 - The cumulative probability of the  
    Delta \chi^2 between H1 and H0 where Npar-1 is the  
    number of degrees of freedom.  
    small or 0 where the model is prefered over H0.  
    Author : Eran Ofek (Apr 2021)  
    Example: AI = AstroImage({ones(3,3), 2.*ones(3,3), 10.*ones(3,3), 11.*ones(3,3), 13.*ones(3,3)});  
    C  = imProc.image.Stack;  
    Result = C.functionalResponse(AI);  
    Result = C.functionalResponse(AI, 'Intensity',[1 2 10 11 13])  
      
      
### imProc.stack.subtractOffset

Remove offset (constant) from AstroImage


    
    Remove offset (constant) from AstroImage  
    Input  : - An AstroImage object.  
    - An AstroImage object, or a cell array of matrices  
    (images) or scalars, or a vector of scalars, or a  
    function_handle.  
    If an array, then will be converted to a cell of  
    scalars using num2cell. Each cell element will be  
    subtracted from the AstroImage. Of one cell  
    element then will be subtracted from all images.  
    If function_handle (unary function) then it will  
    be used to calculate the offset using this  
    function.  
    * ...,key,val,...  
    'OpArgs' - A cell array of arguments to pass to  
    the Offset operator (if operator is provided).  
    'DataProp' - The data property in the AstroImage  
    (both input and offset) that contains the data.  
    Default is 'ImageData'.  
    'DataPropIn' - The data property in the  
    ImageComponent that contains the image  
    data. Default is 'Data'.  
    'CreateNewObj' - A logical indicating if the  
    output is a new object. If empty, then will  
    check the number of output argumnets. If  
    zero (e.g., C.subtractOffset(...)) then  
    will update the input image object. Else,  
    will create a new object.  
    Output : - An AstroImage object.  
    Author : Eran Ofek (Apr 2021)  
    Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});  
    R  = imProc.stack.subtractOffset(AI,1);  
    R  = imProc.stack.subtractOffset(AI,[1 2]);  
    R  = imProc.stack.subtractOffset(AI,{1 2});  the same  
    R  = imProc.stack.subtractOffset(AI,AI);  
    R  = imProc.stack.subtractOffset(AI,@mean,'OpArgs',{'all'});  
      
### imProc.stack.unitTest

unitTest for the Stack class Example: Result = imProc.stack.unitTest


    
    unitTest for the Stack class  
    Example: Result = imProc.stack.unitTest  
      
    applyUnaryFun  
