# Package: imUtil.background


### imUtil.background.annulus_filter

Apply annului filters to an image Package: @imUtil.background


    
    Apply annului filters to an image  
    Package: @imUtil.background  
    Input  : - A 2D image.  
    - A two column matrix of [Inner, Outer] radii of annulus.  
    Default is [10 15; 20 23; 40 41.6].  
    Output : - A 3D matrix in which the 3rd dim is the image index (equal to  
    the number of filters). And the first 2D are the filtered  
    images.  
    - A vector of annuli area.  
    Author: Eran Ofek (Jun 2021)  
    Example: [F,A] = imUtil.background.annulus_filter(100+randn(1000,1000));  
      
### imUtil.background.background

Estimate the background and its variance for an astronomical image Package: @imUtil.background Description: A wrapper function for estimating the background and background variance of an imaage in a matrix form. The function partition the image into sub images


    
    Estimate the background and its variance for an astronomical image  
    Package: @imUtil.background  
    Description: A wrapper function for estimating the background and  
    background variance of an imaage in a matrix form.  
    The function partition the image into sub images  
    (using imUtil.image.partition_subimage) and  
    estimate the background and variance in each sub image.  
    Next, it collect all the sub images into a full image  
    (using: imUtil.image.subimages2image).  
    The background and variance are calculated in each sub image  
    by calling a user supplied functions.  
    Input  : - a 2D matrix.  
    * Pairs of ...,key,val,... The following keywords are available:  
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
    If 'full' or empty use full image.  
    Default is [128 128].  
    'Overlap' - The [X,Y] additional overlaping buffer between  
    sub images to add to each sub image.  
    Default is 16.  
    'DiluteStep' - Dilution for background calculation. This is  
    the step size in which the data in each sub image is selected.  
    Default is 1 (no dilution).  
    'ExtendFull' - A logical indicating if to extend the  
    background map into a full-size image. Default is true.  
    Not relevent anymore:  
    'StitchMethod' - Stitching method.  
    See imUtil.image.subimages2image for options.  
    Another option is 'scalar'. If there is only one sub  
    image, then the ouput will be a scalar.  
    Default is 'IgnoreOverlap'.  
    'SmoothSigma' - Smooth the background image using a gaussian  
    convolution kernel.  
    This is needed s the background calculation is done in  
    sub images, and each sub image has its own background  
    level. This is required in order to avoid sharp  
    background transitions.  
    If empty, then do not apply smoothing.  
    If NaN, then use default smoothing.  
    Otherwise should be [SigmaX, SigmaY], which are the  
    sigmas in X and Y directions of the Gaussian kernel.  
    Default is [SubSizeXY]./2.35.  
    Output : - Background image.  
    - Variance image.  
    By: Eran O. Ofek                       Apr 2020  
    Example: [Back,Var]=imUtil.background.background(rand(1024,1024));  
      
### imUtil.background.collapse_stat

Collapse an image on one dimension and calc line statistics Package: @imUtil.background Description: Collapse an image on one dimension and calculate the line statistics including smooth background level, and std estimate.


    
    Collapse an image on one dimension and calc line statistics  
    Package: @imUtil.background  
    Description: Collapse an image on one dimension and calculate the line  
    statistics including smooth background level, and std  
    estimate.  
    The matrix is collapsed and the output is save in the 'Line'  
    field of the output.  
    The 'Line' is filtered (smotthed) and the result is in  
    'FiltLine', while the std of the line is in 'StdLine'.  
    Input  : - An array.  
    * Pairs of ...,key,val,... arguments. Options are:  
    'Dim' - Dimension along to collapse the image.  
    Default is 1.  
    'CollapseFun' - Collapse function:  
    'median' | 'mean' | 'sum' | 'std' | 'var' (ignore  
    nans).  
    Default is 'median'.  
    'FilterCollapse' - Method by which to filter the collapsed  
    image:  
    'medfilt1' - use the medfilt1 function.  
    'sgolay' - use the sgolay function.  
    'hampel' - use the hampel function.  
    'movavg' - use the movavg function.  
    Default is 'medfilt1'.  
    'FilterCollapsePar' - A cell array of additional parameters to  
    pass to the FilterCollapse function.  
    Default is {10}.  
    'StdCollapse' - Method by which to estimate the std of the  
    line.  
    'std' - use the std function.  
    'rstd' - use the imUtil.background.rstd function.  
    Default is 'rstd'.  
    'StdCollapsePar' - A cell array of additional parameters to  
    pass to the StdCollapse function.  
    Default is {}.  
    Output : - A structure with fields:  
    .Line - The collapse image.  
    .FiltLine - The filtered line.  
    .StdLine - - The line std.  
    By: Eran O. Ofek                       May 2020  
    Example: Image = rand(100,150); Image(23,:) = Image(23,:).*3;  
    Res=imUtil.background.collapse_stat(Image,'Dim',2)  
    Res=imUtil.background.collapse_stat(Image,'Dim',2,'StdCollapse','rstd','StdCollapsePar',{})  
      
### imUtil.background.fill_sparse

given a list of sparse 2D positions and values, fill an image by interp Package: @imUtil.background


    
    given a list of sparse 2D positions and values, fill an image by interp  
    Package: @imUtil.background  
    Input  : - A three column matrix of [X,Y,Val],  
    where X and Y are pixel poistion and Val is the value at that  
    position.  
    - The [X, Y] size of the image to fill.  
    - Interpolation method:  
    'si' - use scatteredInterpolant with linear interpolation and  
    extrapolation.  
    'impaint' - use impaint (slow).  
    Default is 'si'.  
    Output : - The filled image.  
    By: Eran O. Ofek                       Apr 2020  
    Example: GridVal = [10 10 1; 10 20 1.1;20 10 1.2; 20 20 1.3];  
    imUtil.background.fill_sparse(GridVal,[30 30])  
      
### imUtil.background.flag_badcol

Flag a bad column/row in an image Package: @imUtil.background Description: Flag a column/row which background/noise level is high.


    
    Flag a bad column/row in an image  
    Package: @imUtil.background  
    Description: Flag a column/row which background/noise level is high.  
    Input  : - An array.  
    * Pairs of ...,key,val,... arguments. Options are:  
    'Dim' - Dimension along to collapse the image.  
    Default is 1.  
    'Threshold' - A threshold in units of sigma, for bad coumn/row  
    flagging.  
    Default is 10.  
    'CollapseFun' - Collapse function:  
    'median' | 'mean' | 'sum' | 'std' | 'var' (ignore  
    nans).  
    Default is 'median'.  
    'FilterCollapse' - Method by which to filter the collapsed  
    image:  
    'medfilt1' - use the medfilt1 function.  
    'sgolay' - use the sgolay function.  
    'hampel' - use the hampel function.  
    'movavg' - use the movavg function.  
    Default is 'medfilt1'.  
    'FilterCollapsePar' - A cell array of additional parameters to  
    pass to the FilterCollapse function.  
    Default is {10}.  
    'StdCollapse' - Method by which to estimate the std of the  
    line.  
    'std' - use the std function.  
    'rstd' - use the imUtil.background.rstd function.  
    Default is 'rstd'.  
    'StdCollapsePar' - A cell array of additional parameters to  
    pass to the StdCollapse function.  
    Default is {}.  
    Output : - A logical image indicating bad coumn/rows (true).  
    - The structure output of imUtil.background.collapse_stat  
    By: Eran O. Ofek                       May 2020  
    Example: Image = rand(100,150); Image(23,:) = Image(23,:).*5;  
    Flag=imUtil.background.flag_badcol(Image,'Dim',2)  
      
### imUtil.background.mode

Mode and variance of a distribution Package: @imUtil.background Description: Calculate the mode and robust variance of an array. The mode is calculated by making an histogram and choosing the bin with the highest number of counts. The robust


    
    Mode and variance of a distribution  
    Package: @imUtil.background  
    Description: Calculate the mode and robust variance of an array.  
    The mode is calculated by making an histogram and choosing  
    the bin with the highest number of counts. The robust  
    variance is calculated from the histogram (via interpolation).  
    Input  : - An array for which to calculate the global mode, and robust  
    variance.  
    - (Log) A logical indicating if to calculate the histogram of  
    the log of the values, this is recomended when the values  
    distribution has an higher tail (e.g., like in astronomical  
    images).  
    Default is true.  
    - Ignore NaNs. Default is true.  
    - (Accuracy). The (roughly) required accuracy. This parameter  
    controls the histogram bin size (requiring that on average  
    each bin contains (1/Accuracy)^2 points).  
    Default is 0.1.  
    - (MiN) Minimum number of points (not NaN) in the array.  
    If the number of points is smaller than this bound then the  
    function will return NaNs.  
    Default is 10.  
    - A logical indicating if to calculate the variance on the lower  
    quantile. Default is true.  
    Output : - The robust median calculated using the scaled iqr  
    By: Eran O. Ofek                       Apr 2020  
    Example: imUtil.background.mode(randn(1000,1000))  
      
### imUtil.background.rstd

robust std (default is for a matrix) calculated using scaled iqr Package: @imUtil.background Description: robust std of an array. Note that using the imUtil.background.mode function is much faster.


    
    robust std (default is for a matrix) calculated using scaled iqr  
    Package: @imUtil.background  
    Description: robust std of an array. Note that using the  
    imUtil.background.mode function is much faster.  
    Input  : - An array.  
    - Vector of dimensions over which to calculate the robust  
    std. Default is [1 2].  
    Output : - The robust median calculated using the scaled iqr  
    By: Eran O. Ofek                       Apr 2020  
    Example: imUtil.background.rstd(randn(1000,1000))  
      
      
### imUtil.background.rvar

robust variance (default is for a matrix) calculated using scaled iqr Package: @imUtil.background Description: robust variance of an array. Note that using the imUtil.background.mode function is much faster.


    
    robust variance (default is for a matrix) calculated using scaled iqr  
    Package: @imUtil.background  
    Description: robust variance of an array. Note that using the  
    imUtil.background.mode function is much faster.  
    Input  : - An array.  
    - Vector of dimensions over which to calculate the robust  
    variance. Default is [1 2].  
    Output : - The robust median calculated using the scaled iqr  
    By: Eran O. Ofek                       Apr 2020  
    Example: imUtil.background.rvar(randn(1000,1000))  
      
      
