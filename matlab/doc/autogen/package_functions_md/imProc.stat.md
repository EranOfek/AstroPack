# Package: imProc.stat


### imProc.stat.hist

Plot the histogram of a single property in a single AstroImage object image.


    
    Plot the histogram of a single property in a single AstroImage object image.  
    Input  : - A single element AstroImage or ImageComponent object.  
    - Property name. Default is 'Image'. If empty, use default.  
    * A cell array of arbitrary number of arguments to pass to  
    histogram.m. Default is {}.  
    If one argument, then do not need to put in cell.  
    Output : - histogram handle.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});  
    imProc.stat.hist(AI)  
    imProc.stat.hist(AI,[],100)  
    imProc.stat.hist(AI,[],{100,'Normalization','cdf'})  
      
### imProc.stat.histcounts

Return the histcounts of a single property in a single AstroImage object image.


    
    Return the histcounts of a single property in a single AstroImage object image.  
    Input  : - A single element AstroImage or ImageComponent object.  
    - Property name. Default is 'Image'. If empty, use default.  
    * A cell array of arbitrary number of arguments to pass to  
    histcounts.m. Default is {}.  
    Output : - N (see histcounts)  
    - Edges (see histcounts)  
    - Bin (see histcounts)  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});  
    [a,b,c] = imProc.stat.histcounts(AI)  
      
### imProc.stat.max

Return the max of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.


    
    Return the max of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted. A NaN value indicate the image is  
    empty.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    max value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.max(AI)  
    [a,b] = imProc.stat.max(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c,d] = imProc.stat.max(AI)  
      
### imProc.stat.mean

Return the mean of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.


    
    Return the mean of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    mean value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.mean(AI)  
    [a,b] = imProc.stat.mean(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.mean(AI)  
      
### imProc.stat.median

Return the median of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.


    
    Return the median of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    median value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.median(AI)  
    [a,b] = imProc.stat.median(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.median(AI)  
      
### imProc.stat.min

Return the min of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.


    
    Return the min of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted. A NaN value indicate the image is  
    empty.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    min value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.min(AI)  
    [a,b] = imProc.stat.min(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.min(AI)  
      
### imProc.stat.mode

Return the mode of AstroImage object images using imUtil.background.mode. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask,


    
    Return the mode of AstroImage object images using  
    imUtil.background.mode.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    mode value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.mode(AI)  
    [a,b] = imProc.stat.mode(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.mode(AI)  
      
### imProc.stat.moment

Return the moment of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.


    
    Return the moment of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted.  
    Input  : - An AstroImage object.  
    - Moment order.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    quantile value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.moment(AI, 3)  
    [a,b] = imProc.stat.moment(AI, 4)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.moment(AI, 5)  
      
### imProc.stat.quantile

Return the quantile of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.


    
    Return the quantile of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted.  
    Input  : - An AstroImage object.  
    - Quantile (between 0 and 1).  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    quantile value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.quantile(AI, 0.2)  
    [a,b] = imProc.stat.quantile(AI, 0.3)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.quantile(AI, 0.95)  
      
### imProc.stat.range

Return the range of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.


    
    Return the range of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted. A NaN value indicate the image is  
    empty.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    range value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.range(AI)  
    [a,b] = imProc.stat.range(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.range(AI)  
      
### imProc.stat.rstd

Return the rstd (robust std) of AstroImage object images using imUtil.background.rstd. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask,


    
    Return the rstd (robust std) of AstroImage object images using  
    imUtil.background.rstd.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.  
    By default NaNs are omitted.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    rstd value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.rstd(AI)  
    [a,b] = imProc.stat.rstd(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.rstd(AI)  
      
### imProc.stat.std

Return the std of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.c


    
    Return the std of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.c  
    By default NaNs are omitted.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    std value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.std(AI)  
    [a,b] = imProc.stat.std(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.std(AI)  
      
### imProc.stat.unitTest

unitTest for the +imProc.stat package Example: Result = imProc.stat.unitTest


    
    unitTest for the +imProc.stat package  
    Example: Result = imProc.stat.unitTest  
      
### imProc.stat.var

Return the var of AstroImage object images. The output is an array in which each element corresponds to an element in the AstroImage. The output arguments corresponds to the Image, Back, Var, Mask, respectively.c


    
    Return the var of AstroImage object images.  
    The output is an array in which each element corresponds to an  
    element in the AstroImage.  
    The output arguments corresponds to the Image, Back, Var, Mask,  
    respectively.c  
    By default NaNs are omitted.  
    Input  : - An AstroImage object.  
    * Arbitrary arguments to pass to AstroImage/funUnaryScalar.  
    Output : * By default will return up to 4 output arguments for the  
    var value of the Image, Back, Var and Mask.  
    Each argument is an array which size equal to the size of  
    the AstroImage, and each element corresponds to an  
    AstroImage element.  
    Author : Eran Ofek (May 2021)  
    Example: AI = AstroImage({rand(10,10), rand(10,10)});  
    imProc.stat.var(AI)  
    [a,b] = imProc.stat.var(AI)  
    AI = AstroImage({rand(10,10), rand(10,10)},'Back',{rand(10,10), rand(10,10)});  
    [a,b,c] = imProc.stat.var(AI)  
      
