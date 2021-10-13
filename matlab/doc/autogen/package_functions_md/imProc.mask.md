# Package: imProc.mask


### imProc.mask.maskSaturated

set mask bits for saturated and non-linear pixels


    
    set mask bits for saturated and non-linear pixels  
    Input  : - An AstroImage object (multi elements supported).  
    * ...,key,val,...  
    'SatLevel' - A saturation level - A pixel above this level  
    will be set in the mask image. If empty, then will  
    attempt to read the level from the header using the  
    keyword in the 'SatKey' argument.  
    If also the level is not found in the header then  
    the mask will not be set.  
    Default is [].  
    'SatKey' - Saturation level header keyword name.  
    Default is 'SATURVAL'.  
    'BitName_Saturated' - Bit name of the saturated pixel.  
    Default is 'Saturated'.  
      
    'MultLevelByGain' - A logical indicating if the saturation and non-linear  
    levels are needed to be multiplied by the gain.  
    Default is false.  
    'Gain' - Either the gain value or an header keyword name from  
    which to read the gain.  
    If gain is not available in header then will be set  
    to 1. Default is 'GAIN'.  
    'SatPix2NaN' - A logical indicating if to replace  
    saturated pixels with NaN. Default is false.  
      
    'NonLinLevel' - Like 'SatLevel', but for non-linearity.  
    Default is [].  
    'NonLinKey' - Like 'SatKey' but for non-linearity.  
    Default is 'NONLIN'.  
    'BitName_NonLinear' - Like 'BitName_Saturation'.  
    Default is 'NonLinear'  
    'NonLinPix2NaN' - A logical indicating if to replace  
    non-linear pixels with NaN. Default is false.  
      
    'DefBitDict' - Default bit dictionary if  
    not exist. Default is  
    BitDictionary('BitMask.Image.Default').  
    'CreateNewObj' - Indicating if the output  
    is a new copy of the input (true), or an  
    handle of the input (false).  
    If empty (default), then this argument will  
    be set by the number of output args.  
    If 0, then false, otherwise true.  
    This means that IC.fun, will modify IC,  
    while IB=IC.fun will generate a new copy in  
    IB.  
    Some additional hidden arguments  
    Output : - An AstroImage object in which the MaskData is set with  
    saturated and non-linear pixels.  
    - A matrix of logicals (for the latest images) indicating  
    saturated pixels.  
    - A matrix of logicals (for the latest images) indicating  
    non-linear pixels.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10).*1000});  
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500)  
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500, 'NonLinLeve',100)  
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500, 'NonLinLeve',100,'MultLevelByGain',true,'Gain',5);  
      
### imProc.mask.replaceMaskedPixVal

Replace the values of image pixels which have specific bit mask


    
    Replace the values of image pixels which have specific bit mask  
    Input  : - An AstroImage object.  
    - A cell array of mask bit names. Pixels in which this bits  
    are on (any | or) will be set to ReplaceVal.  
    - Value to replace (ReplaceVal).  
    * ...,key,val,...  
    'Method' - Indicating if to look for pixels in  
    which all the requested bits are on  
    ('all'), or one or more of the requested  
    bits are on ('any').  
    Default is 'any'.  
    'DataProp' - Data property in AstroImage for which to  
    modify value. Default is 'Image'.  
    'CreateNewObj' - Logical indicating if to copy the input  
    object. Default is false.  
    Output : - An AstroImage object with the pixel values replaced.  
    Author : Eran Ofek (Aug 2021)  
    Example: imProc.mask.replaceMaskedPixVal(AI,  'Saturated', NaN);  
      
### imProc.mask.unitTest

unitTest for +imProc.mask Example: imProc.mask.unitTest


    
    unitTest for +imProc.mask  
    Example: imProc.mask.unitTest  
      
