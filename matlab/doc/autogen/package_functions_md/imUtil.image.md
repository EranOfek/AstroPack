# Package: imUtil.image


### imUtil.image.combine_proper

Proper coaddition of images in a cube Package: imUtil.image Description: Proper coaddition (Zackay & Ofek 2017) of images in a cube


    
    Proper coaddition of images in a cube  
    Package: imUtil.image  
    Description: Proper coaddition (Zackay & Ofek 2017) of images in a cube  
    Input  : - A cube of images, where the 3rd dimension is the image index.  
    - A cube of PSFs, where the 3rd dimension is the image index.  
    * Arbitrary number of ...,key,val,... arguments.  
    The following keywords are available:  
    'F' - A vector of weights (one weight per image).  
    Default is 1.  
    'Var' - A vector of variances (one variance per image).  
    Default is 1.  
    'PsfType' - A string indicating where is the center of the  
    PSF.  
    'center' - PSF is centered in stamp.  
    'corner' - PSF is in corner.  
    'Norm' - A logical flag indicating if to normalize the PSF to  
    unity prior to coaddition.  
    Default is true.  
    Output : - The proper coadded image.  
    - The proper PSF  
    - FFT of the proper coadded image.  
    - FFT of the proper PSF.  
    Reference: Ofek & Zackay 2017, ApJ 836, 188  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    May 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Psf = imUtil.kernel2.gauss([1 2 3 4 5]');  
    Data = Psf + randn(size(Psf)).*0.001;  
    [R,PR,R_f,PR_f]=imUtil.image.combine_proper(Data,Psf)  
    Reliable:  
      
      
### imUtil.image.cube_chi2

Calculate the sigma-clipped mean of a dataset Package: imUtil.image Description: Calculate the sigma-clipped mean of a dataset with iterations.


    
    Calculate the sigma-clipped mean of a dataset  
    Package: imUtil.image  
    Description: Calculate the sigma-clipped mean of a dataset with  
    iterations.  
    Input  : - The dataset of any dimensionality.  
    - Dimension over which to calculate the mean.  
    For example, if you have a cube of images in which the  
    3rd dimension is the image index, and you want to calculate  
    the mean image. Set Dim=3.  
    Deafult is ndims(Data).  
    * Arbitrary number of ...,key,val,... arguments.  
    The following keywords are available:  
    'MeanFun' - Funtion handle for calculating the mean.  
    Default is @nanamean.  
    'StdFun'  - A string indicating the method by which to  
    calculate the data StD.  
    Options are: 'std' - fo normal StD, or 'rstd' for  
    robust StD calculated using imUtil.background.rvar.  
    Default is 'rstd'.  
    'ThresholdSigma' - Will count the number of times that the  
    value is above this threshold (in units of sigma) for  
    each pixel. Default is 5.  
    'Abs' - Use abs value when counting the number of events above  
    sigma (i.e., true will count above and below the  
    threshold).  
    Default is true.  
    Output : - A matrix of chi^2 per pixel calculated over the index  
    dimension.  
    - A cube of the Z statistics (distance of each pixel from the  
    mean in units of the std).  
    - A matrix of the mean.  
    - A matrix of the Std.  
    - A matrix of the number of times the pixel is above/below the  
    threshold.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [Chi2,Z,Mean,Std,NaboveThreshold]=imUtil.image.cube_chi2(rand(10,10,5),3);  
    Reliable: 2  
      
      
### imUtil.image.find_within_radius_cell

Find points within a radius from a list of coordinates. Package: imUtil.image Description: Given a list of coordinates within an array, return for each coordinate a vector (within a cell array) of indices of the points in the image


    
    Find points within a radius from a list of coordinates.  
    Package: imUtil.image  
    Description: Given a list of coordinates within an array, return for each  
    coordinate a vector (within a cell array) of indices of  
    the points in the image  
    that are within a given radius from the coordinate.  
    Also return a corresponding vector of distances squared of  
    each point in the image from the coordinate.  
    See also find_within_radius_mat.m, but this function is  
    usually faster.  
    Input  : - An array size [Y, X] (e.g., size(Image)).  
    - A vector of X coordinates around to search for the points  
    within the radius.  
    - A vector of Y coordinates around to search for the points  
    within the radius.  
    - Radius [pix].  
    - If true, then will return only points within the radius.  
    If false, then will return all the points within a square of  
    size: radius*2+1.  
    Default is false (somewhat faster).  
    Output : - A cell array of vectors. Each vector contains the indices of  
    points that are found within the radius from the point.  
    Each cell element corresponds to a coordinate in the X,Y  
    vectors.  
    - A cell array of vectors. Each vector contains the distance  
    squares of the points in the image from the requested  
    coordinate.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jun 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [CI,CR2]=imUtil.image.find_within_radius_cell([1024 1024],X,Y,20);  
    Reliable: 2  
      
      
### imUtil.image.find_within_radius_mat

Construct a cube of stamps around specific locations in a 2D image. Package: imUtil.image Description: Given an image and a list of coordinates, construct a cybe of stamps around the coordinates. Region outside the image are padded with zeros.


    
    Construct a cube of stamps around specific locations in a 2D image.  
    Package: imUtil.image  
    Description: Given an image and a list of coordinates, construct a cybe  
    of stamps around the coordinates. Region outside the image  
    are padded with zeros.  
    coordinate a vector (within a cell array) of indices of  
    the points in the image  
    that are within a given radius from the coordinate.  
    Also return a corresponding vector of distances squared of  
    each point in the image from the coordinate.  
    See also find_within_radius_mat.m, but this function is  
    usually faster.  
    Input  : - An array size [Y, X] (e.g., size(Image)).  
    - A vector of X coordinates around to search for the points  
    within the radius.  
    - A vector of Y coordinates around to search for the points  
    within the radius.  
    - Radius [pix].  
    - If true, then will set all points outside the radius to NaN.  
    Default is false.  
    Output : - A cube of size 2.*Radius+1 by 2.*Radius+1 by number of  
    coordinates.  
    The cube contains the stamps around the requested rounded  
    coordinates. The third dimension is the coordinate index.  
    - Rounded X coordinates.  
    - Roundex Y coordinates.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jun 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Image=rand(1024,1024); X=rand(100,1).*1023+1; Y=rand(100,1).*1023+1;  
    [Cube,RoundX,RoundY]=imUtil.image.find_within_radius_mat(Image,X,Y,3);  
    Reliable: 2  
      
      
### imUtil.image.fun_binary_withVariance

Applay an operator on an array and its variance.


    
    Applay an operator on an array and its variance.  
    Input  : - Operator handle (e.g., @sin).  
    - First array to apply to the left of the operator.  
    - Second array to apply to the right of the operator.  
    - The first variance array. Default is [].  
    - The second variane array. Default is [].  
    If both variances are [], then the errors will not be  
    propagated.  
    - The covariance matrix between the two arrays.  
    Default is 0.  
    - A cell array of additional parameters to pass to the function  
    operator. Default is {}.  
    Output : - The result of applying the operator to the matrix.  
    - The result of applying the operator to the variance.  
    - A matrix of logicals indicating if a resulted matrix value is  
    NaN or Inf, or the resulted Variance is NaN.  
    Author : Eran Ofek (Mar 2021)  
    Example:  
    [Result,ResultVar,Flag]=imUtil.image.fun_binary_withVariance(@plus, randn(2,2), randn(2,2));  
    [Result,ResultVar,Flag]=imUtil.image.fun_binary_withVariance(@plus, randn(2,2), randn(2,2),[],[]);  
    [Result,ResultVar,Flag]=imUtil.image.fun_binary_withVariance(@times, randn(2,2), randn(2,2), 0.01, 0.01)  
    [Result,ResultVar,Flag]=imUtil.image.fun_binary_withVariance(@power, randn(2,2), randn(2,2), 0.01, 0.01)  
      
### imUtil.image.fun_cutouts

Apply a function thar returns a scalar to all cutouts in image around locations.


    
    Apply a function thar returns a scalar to all cutouts in image around locations.  
    Input  : - a 2D image (background subtracted), or a 3D cube of cutouts  
    (3rd dim is ciutout index).  
    - A vector of X coordinates around to calculate moments.  
    - A vector of Y coordinates around to calculate moments.  
    - Function handle to apply on all cutouts. The function gets a  
    matrix and returns a scalar.  
    * ...,key,val,...  
    'FunArgs' - A cell array of additional arguments to pass to  
    the function. Default is {}.  
    'FunWorksOnCube' - Function works on cube (where 3rd dim is  
    the cutout index). Default is false.  
    'FunWorksOnMatrix' - Function works on matrix (over 1st dim.).  
    Default is false.  
    'Radius' - Radius of cutouts (if input is a matrix).  
    'Circle' - A flag indicating if to extract the stamps with  
    circular shape. Default is false.  
    'mexCutout' - use imUtil.image.mexCutout.m (true) or  
    imUtil.image.find_within_radius_mat (false).  
    Default is true.  
    Output : * Arbitrary numbre of vectors. Each vector for each output of  
    Fun. each element in each vector corresponds to one cutout.  
    Author : Eran Ofek (Mar 2021)  
    Example: [M] = imUtil.image.fun_cutouts(rand(1000,1000), rand(100,1).*600+10, rand(100,1).*600+10, @median, 'FunWorksOnMatrix',true)  
    [M] = imUtil.image.fun_cutouts(rand(20,20,100), [], [], @median, 'FunWorksOnMatrix',true)  
      
### imUtil.image.fun_unary_withVariance

Applay an operator on an array and its variance.


    
    Applay an operator on an array and its variance.  
    Input  : - Operator handle (e.g., @sin).  
    - An array in which to apply the operator.  
    - A variance array or scalar of the array. Default is [].  
    * ...,key,val,...  
    'OpArgs' - A cell array of additional parameters to pass to the function  
    operator. Default is {}.  
    'CCDSEC' - A CCDSEC [Xmin, Xmax, Ymin, Ymax] section on which  
    to excute the operator. If empty, use entire image.  
    Default is [].  
    'OutOnlyCCDSEC' - INdicating if to return the full image or  
    only the CCDSEC region. If false, note that the variance  
    will be calculated on the entire image.  
    Default is true.  
    'PropagateErr' - A logical indicating if to propagate the  
    errors. Default is true.  
    'OperateOnVar' -  If PropagateErr=false, this is a logical  
    indicating if to operate the operator on the variance matrix.  
    Default is true.  
    Output : - The result of applying the operator to the matrix.  
    - The result of applying the operator to the variance.  
    - A matrix of logicals indicating if a resulted matrix value is  
    NaN or Inf, or the resulted Variance is NaN.  
    - The function handle for the derivative function (only if was  
    found symbolically).  
    Author : Eran Ofek (Mar 2021)  
    Example: [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@sin, randn(5,5), rand(5,5).*0.01)  
    [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@mean, randn(2,2), rand(2,2).*0.01)  
    [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@mean, randn(2,2), rand(2,2).*0.01,'OpArg',{'all'})  
    [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@tanh, randn(2,2), rand(2,2).*0.01);  
    [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@sin, randn(5,5), rand(5,5).*0.01,'CCDSEC',[1 2 1 2],'OutOnlyCCDSEC',true)  
    [Result,ResultVar,Flag,FunH]=imUtil.image.fun_unary_withVariance(@sin, randn(5,5), rand(5,5).*0.01,'CCDSEC',[1 2 1 2],'OutOnlyCCDSEC',false)  
      
### imUtil.image.images2cube

Store a set of images of the same size in a cube. Package: imUtil.image Description: Given a cell array of images or a structure array that contains a field with image, convert it to a cube of images. By default the image index is 3.


    
    Store a set of images of the same size in a cube.  
    Package: imUtil.image  
    Description: Given a cell array of images or a structure array that  
    contains a field with image, convert it to a cube of  
    images. By default the image index is 3.  
    Input  : -  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
    The following keywords are available:  
    'FieldName' - If the input is a structure array, then this is  
    the field name containing the image.  
    Default is 'Im.  
    'IndexDim'  - The dimension in which the image index is  
    stored in the output. Default is 3.  
    This can be either 1 or 3.  
    Output : - A cube of images.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Cube]=imUtil.image.images2cube({rand(5,4),ones(5,4)});  
    [Cube]=imUtil.image.images2cube({rand(5,4),ones(5,4)},'IndexDim',1);  
    Reliable: 2  
      
### imUtil.image.ind2sub_fast

ind2sub fast version for 2D matrices Description: A fast version of ind2sub for 2D arrays. If a non 2D array is provided then will use the built in ind2sub function.


    
    ind2sub fast version for 2D matrices  
    Description: A fast version of ind2sub for 2D arrays.  
    If a non 2D array is provided then will use the built in  
    ind2sub function.  
    Input  : - Array size [Y,X].  
    - Index  
    Output : - Y (i) positions (whole pixels)  
    - X (j) positions (whole pixels)  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [I,J]=imUtil.image.ind2sub_fast([3 3],2)  
    Reliable: 2  
      
      
### imUtil.image.local_maxima

Identify local maxima above a threshold in a 2D image Package: @imUtil.image Description: This function identify local maxima, above some threshold, in an image, or alternatively, in a max over cube of images. If the first input is an image and given a threshold image


    
    Identify local maxima above a threshold in a 2D image  
    Package: @imUtil.image  
    Description: This function identify local maxima, above some threshold,  
    in an image, or alternatively, in a max over cube of images.  
    If the first input is an image and given a threshold image  
    (e.g., background std image multiply by number of sigmas  
    for detection), identify local maxima above the threshold.  
    Return a matrix of logical indicating if a pixel is a local  
    maxima, and a list of [X,Y] coordinates of the local maxima  
    in the image.  
    Alternatively, if the first input is a cube of images, in  
    which the third dimension is the image index, then start by  
    dividing the cube by the variance image (or cube), and than  
    take the max over the 3rd dimension.  
    The thresholding and local-maxima identification is done on  
    this max image.  
    Input  : - Image, or a cube of image in which the third dimension is the  
    image index.  
    - Variance image (or cube).  
    For example, if the Imgae is a S/N image, then this parameter  
    should be 1.  
    Default is var(Image,0,[1 2]).  
    - Threshold in units of std (=sqrt(Variance)).  
    The function is searching only for local maxima above this  
    threshold.  
    Default is 5.  
    - The connectivity parameter used by imregionalmax for searching  
    local maxima.  
    Default is 8 (i.e., requires that all 8 surrounding pixels  
    will be below the local max).  
    Output : - A matrix of logical indicating if a pixel in the input image  
    is a local maximum above the threshold.  
    - A four column matrix of [X,Y,SN,index].  
    Each row corresponds to one local maxima identified in the S/N  
    image. X and Y are the position of the local maxima, SN is its  
    S/N value, and index is the image-index that contains the  
    maximum in the cube input (1 if input is a matrix).  
    - If the first input argument is a cube in which the 3rd  
    dimension is the image index, then this is a matrix  
    indicating in each pixel which image index have the maximum  
    value (the thresholding and local maxima is done on the max of  
    the cube, so this enables to identify which plane in the cube  
    contributed to the maximum).  
    If the first input is a 2D matrix, then this paarameter is  
    empty.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Image = imUtil.kernel2.gauss(1.5,[1024 1024]).*30 + randn(1024,1024);  
    Template = imUtil.kernel2.gauss(1.5);  
    [SN,Flux]=imUtil.filter.filter2_sn(Image,[],[],Template);  
    [BW,Pos]=imUtil.image.local_maxima(SN,1,5);  
    Example with a cube input (find the optimal focus)  
    Image = imUtil.kernel2.gauss(2.2,[1024 1024]).*300 + randn(1024,1024);  
    SN=imUtil.filter.filter2_snBank(Image,[],[],@imUtil.kernel2.gauss,(1:0.2:6).');  
    [BW,Pos,MaxIsn]=imUtil.image.local_maxima(SN,1,5);  
    Reliable: 2  
      
      
### imUtil.image.mean_sigclip

Calculate the sigma-clipped mean of a dataset Package: imUtil.image Description: Calculate the sigma-clipped mean of a dataset with iterations.


    
    Calculate the sigma-clipped mean of a dataset  
    Package: imUtil.image  
    Description: Calculate the sigma-clipped mean of a dataset with  
    iterations.  
    Input  : - The dataset of any dimensionality.  
    - Dimension over which to calculate the mean.  
    For example, if you have a cube of images in which the  
    3rd dimension is the image index, and you want to calculate  
    the mean image. Set Dim=3.  
    Deafult is ndims(Data).  
    * Arbitrary number of ...,key,val,... arguments.  
    The following keywords are available:  
    'MeanFun' - Funtion handle for calculating the mean.  
    Default is @nanamean.  
    'StdFun'  - A string indicating the method by which to  
    calculate the data StD.  
    Options are: 'std' - fo normal StD, or 'rstd' for  
    robust StD calculated using imUtil.background.rvar.  
    Default is 'rstd'.  
    'Nsigma' - [Lower, Upper] number of sigmas below/above to  
    sigma clip the data.  
    Default is [5 5].  
    'MaxIter' - Maximum number of iterations.  
    Use 0 in order to calculate the mean without sigma  
    clipping. Will stop before maximum number of iterations  
    reached if no new points were clipped.  
    Default is 3.  
    'EpsilonStd' - A small nuymber that will be added to the StD,  
    in order to avoid division by zero. Default is 1e-12.  
    Output : - Sigma clipped mean of data.  
    - Sigma clipped variance of the data.  
    - A logical array of the same size of the input Data which  
    indicate the good data points that were used for the mean  
    calculation.  
    - An image of number of images used in each pixel.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data = randn(10,10,100);  
    Data(4,4,18)=100; Data(1,1,1)=-14;   some outliers  
    [Mean,Var,FlagGood,NC]=imUtil.image.mean_sigclip(Data,3);  
    [i,j,k]=ind2sub(size(Data),find(~FlagGood))  
    Reliable: 2  
      
      
### imUtil.image.moment2

Calculate 1st, 2nd moments and (weighted) aperture photometry Package: @imUtil.image Description: Given a 2D image, or a 3D cube of image stamps, and X, Y coordinates of sources (or the center of the stamps), calculate for each stamp the 1st and 2nd moments, and


    
    Calculate 1st, 2nd moments and (weighted) aperture photometry  
    Package: @imUtil.image  
    Description: Given a 2D image, or a 3D cube of image stamps, and X, Y  
    coordinates of sources (or the center of the stamps),  
    calculate for each stamp the 1st and 2nd moments, and  
    aperture photometry.  
    By default, first moment is calculated iteratively around  
    the guess position. It is calculated using windowing (i.e.,  
    multiplying the stamp by a weight function). The user can  
    supply the weight function, but by default is a Gaussian  
    with a radius specified by the user. By default, the weight  
    function width is adapeted iteratively, where in the first  
    iteration a flat window is used, and then a Gaussian with  
    smaller and smaller sigma is used (see code for details).  
    The central second moment is calculated around the 1st  
    moment position with the weight function.  
    Aperture photometry is calculated in a list of apertures.  
    Also calculated is the total flux in the stamps, and the  
    weighted aperture photometry (weighted by the weight  
    function and properly normalized). This is roughly  
    equivalent to PSF photometry.  
    Note that the measurment of the second moment may be  
    biased and it can be usually used only as a relative  
    quantity.  
    Input  : - a 2D image (background subtracted), or a 3D cube of cutouts  
    (3rd dim is ciutout index).  
    - A vector of X coordinates around to calculate moments.  
    - A vector of Y coordinates around to calculate moments.  
    * Pairs of ...,key,val,... The following keywords are available:  
    'AperRadius' - Vector of aperture radii, in which to calculate  
    aperture photometry.  
    Default is [2 4 6].  
    'Annulus' - Vector of inner and outer radius of background  
    annulus. Default is [8 12].  
    'SubBack' - A logical indicating if to subtract background.  
    Default is true.  
    'BackFun' - Function handle to use for background estimation.  
    In order to meaningful this function must ignore  
    NaNs.  
    Default is @median.  
    'MomRadius' - Radius around position in which to calculate the  
    moments. Recomended ~1.7 FWHM. Default is 8.  
    'WeightFun' - The weight function to use for weighting the  
    moments and the weighted photometry.  
    This can be a scalar indicating the sigma of a  
    circularly symmetric Gaussian,  
    or a function handle that matrix of radii, and  
    return a matrix of weights (e.g., @(r)  
    exp(-r.^2./(2.*4))./(2.*pi.*4.^2); ).  
    Default is 2.  
    'Circle' - A flag indicating if to extract the stamps with  
    circular shape. Default is false.  
    'MaxIter' - Maximum number of 1st moment position iterations.  
    0 will perform aingle 1st moment calculation.  
    -1 will use the initial guess without estimating  
    the first moment (i.e., forced photometry).  
    Default is 10.  
    'NoWeightFirstIter' - A flag indicating if not to apply weight  
    on the first itearation. Default is true.  
    'PosConvergence' - Position convergence. Default is 1e-4.  
    'DynamicWindow' - Apply dynamic windowing. Default is true.  
    'WindowOnlyOnLastIter' -Default is false.  
    'FinalIterWithCorrectWin' - Apply an additional final  
    iteration with the correct window.  
    'mexCutout' - use imUtil.image.mexCutout.m (true) or  
    imUtil.image.find_within_radius_mat (false).  
    Default is true.  
    Output  : - First moment information.  
    A structure with the following fields.  
    .RoundX - Vector of roundex X position  
    .RoundY - Vector of roundex Y position  
    .DeltaLastX - Vector of the X shifts in the last position  
    iteration.  
    .DeltaLastY - Vector of the Y shifts in the last position  
    iteration.  
    .Iter - Number of position iterations.  
    .X    - 1st moment X position  
    .Y    - 1st moment Y position.  
    .Xstart - Starting X position,  
    .Ystart - Starting Y position.  
    - A second momement information.  
    A structure with the following fields.  
    .X2 - X^2 2nd moment.  
    .Y2 - Y.^2 2nd moment.  
    .XY - X*Y 2nd moment.  
    - Photometry information. A structure with the following fields.  
    .AperRadius - Vector of apertures radius.  
    .AperPhot - Matrix of aperture photometry. Column per  
    aperture.  
    .AperArea - Matrix of apertures area. Column per aperture.  
    .BoxPhot  - Vector of the full box photometry  
    .AnnulusBack - Annulus background.  
    .AnnulusStd - Annulus StD.  
    .WeightedAper - Weighted photometry. Weighted by the user  
    specified weight function.  
    By: Eran O. Ofek                       Apr 2020  
    Example: Image = rand(1024,1024); X=rand(1e4,1).*1023+1; Y=rand(1e4,1).*1023+1;  
    [M1,M2,Aper]=imUtil.image.moment2(Image,X,Y);  
    Matrix = imUtil.kernel2.gauss(2, [16 16]);  
    [M1,M2,Aper]=imUtil.image.moment2(Matrix,16,16)  
    [M1,M2,Aper]=imUtil.image.moment2(Matrix,16,16,'WeightFun',@(r) 1)  
    Cube = imUtil.kernel2.gauss([2;2.1;2.2], [31 31]);  
    [M1,M2,Aper]=imUtil.image.moment2(Cube,16,16)  
      
### imUtil.image.partition_subimage

Partition image into sub images Package: mUtil.image Description: Partition image into sub images defined either by a CCDSEC matrix, or by imUtil.image.subimage_grid function.


    
    Partition image into sub images  
    Package: mUtil.image  
    Description: Partition image into sub images defined either by a CCDSEC  
    matrix, or by imUtil.image.subimage_grid function.  
    Input  : - Image.  
    - A 4 column matrix of CCDSEC by which to partition the image.  
    Line per sub image. If empty, will use imUtil.image.subimage_grid  
    Default is empty.  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
    The following keywords are available:  
    'Output' - Output type {['cell'] | 'struct'}  
    'FieldName' - Field name in a struct output in which to store  
    the sub images. Default is 'Im'.  
    'SubSizeXY' - Sub image size [X,Y]. If empty, then full image.  
    Default is [128 128].  
    'Nxy' - Number of sub images along each dimension [Nx, Ny].  
    If empty then use SubSizeXY. Default is [].  
    'OverlapXY' - Overlapping extra [X, Y] to add to SubSizeXY  
    from each side. Default is [32 32].  
    Output : - A cell array or a structure array (depends on Output type), of  
    the sub images.  
    - A matrix of the CCDSEC for each sub image. A line per image.  
    [xmin, xmax, ymin, ymax].  
    - Centers of sub images [X,Y].  
    - CCDSEC without overlap. This will be rturned only of input CCDSEC  
    is empty. Otherwise, will return empty matrix.  
    - CCDSEC in the new sub image of the non-overlapping region  
    - Nxy [numbre of sub images in x-dir X number in y0dir].  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [SubImage,CCDSEC,Center,NoOverLap,NewNoOverlap]=imUtil.image.partition_subimage(rand(256,256),[],'SubSizeXY',[64 64],'OverlapXY',[50 50],'Output','struct');  
    [SubImage]=imUtil.image.partition_subimage(rand(256,258),CCDSEC)  
    cellfun(@(x) std(x(:)),SubImage,'UniformOutput',false)   calculate std for each sub image  
    [SubImage]=imUtil.image.partition_subimage(rand(256,258),CCDSEC,'Output','struct');  
    Reliable: 2  
      
      
### imUtil.image.sparse2full

Resize/extend/magnify an image sampled at some specific very sparse points into a full image.


    
    Resize/extend/magnify an image sampled at some specific very sparse points into a full image.  
    Input  : - A sparse image  
    - X points in which the sparse image is sampled from the  
    full image.  
    - Y points in which the sparse image is sampled from the  
    full image.  
    - [I,J] size of the full image.  
    * ...,key,val,...  
    'Method' - One of the following methods:  
    'interp' - use interp2  
    'ExtrapFill' - Extrpolate to image edges before  
    interpolation. Default is true.  
    'InterpMethod' - Interpolation method.  
    Default is 'linear'.  
    'Smooth' - Perform smoothing after filling.  
    Default is false.  
    'GaussSigma' - Sigma of gaussian smoothing kernel.  
    Default is 10.  
    'FiltSizeFactor' - Size of filter in units of GaussSigma.  
    Default is 6.  
    Output : - Filled image.  
    Author : Eran Ofek (Jun 2021)  
    Example: Result = imUtil.image.sparse2full(rand(2,4), [11 21 31 41], [11 21], [30 50])  
      
### imUtil.image.stackCube

Stack (coadd) a cube of images using various functions


    
    Stack (coadd) a cube of images using various functions  
    Input  : - A cube of images in which the image index in the 3rd dim.  
    - * ...,key,val,...  
    'StackMethod' - Stacking method. Options are:  
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
    'bitor' - bit-wise or operation. Return only Coadd.  
    'bitand' - bit-wise and operation. Return only Coadd.  
    'bitnot' - bit-wise not operation. Return only Coadd.  
    'StackArgs' - A cell array of arguments to pass to the  
    method function. Default is {}.  
    'EmpiricalVarFun' - Default is @var.  
    'EmpiricalVarFunArgs' - Default is {[],3,'omitnan'}.  
    'VarCube' - A cube of variances. Default is [].  
    'MedianVarCorrForEmpirical' - A logical indicating if to  
    correct the variance calculation by the ratio between  
    the variance of the median and variance of the mean.  
    Default is false.  
    'DivideEmpiricalByN' - A logical indicating if to divide  
    CoaddVarEmpirical by N. Default is false.  
    'DivideVarByN' - A logical indicating if to divide  
    CoaddVar by N. Default is false.  
    'CalcCoaddVarEmpirical' - Logical indicating if to calc the  
    CoaddVarEmpirical. Default is true.  
    'CalcCoaddVar' - Logical indicating if to calc the  
    CoaddVar. Default is true.  
    'CalcCoaddN' - Logical indicating if to calc the  
    CoaddN. Default is true.  
    Output : - Coadd image.  
    - CoaddVarEmpirical - This is the empirical variance of the  
    data. In case the variance of the mean is needed set DivideEmpiricalByN  
    to true.  
    - CoaddVar - This is the variance of the  
    data. In case the variance of the mean is needed set DivideByN  
    to true.  
    - CoaddN - This is the number of images used in the stacking of  
    each pixel.  
    Author : Eran Ofek (Apr 2021)  
    Example: Cube = 2.*randn(5,5,1000); Cube(1,1,1)=1000;  
    [Coadd] = imUtil.image.stackCube(Cube);  
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube)  
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4)  
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4,'StackMethod','sum')  
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4,'StackMethod','quantile','StackArgs',{0.1})  
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4,'StackMethod','sigmaclip');  
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = imUtil.image.stackCube(Cube,'VarCube',4,'StackMethod','wsigmaclip');  
    [Coadd] = imUtil.image.stackCube(uint32(randi(10,[5,5,10])),'StackMethod','bitand');  
    [Coadd] = imUtil.image.stackCube(uint32(randi(10,[5,5,3])),'StackMethod','bitor');  
      
      
### imUtil.image.sub2ind_fast

sub2ind fast version for 2D matrices Description: A fast version of sub2ind for 2D arrays.


    
    sub2ind fast version for 2D matrices  
    Description: A fast version of sub2ind for 2D arrays.  
    Input  : - Array size [Y,X].  
    - Y (i) positions (whole pixels)  
    - X (j) positions (whole pixels)  
    Output : - Linear index of position in array in uint32 format.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Ind=imUtil.image.sub2ind_fast([3 3],2,2)  
    Reliable: 2  
      
      
      
### imUtil.image.subimage_grid

Partition image size into a grid of sub images Package: mUtil.image Description: Given the size of a two dimensional array (e.g., image), and a sub image size or the number of partitions in each dimension, calculate the coordinates of the partitions


    
    Partition image size into a grid of sub images  
    Package: mUtil.image  
    Description: Given the size of a two dimensional array (e.g., image), and  
    a sub image size or the number of partitions in each  
    dimension, calculate the coordinates of the partitions  
    boundries.  
    Input  : - Image size [X, Y].  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
    The following keywords are available:  
    'SubSizeXY' - Sub image size [X,Y]. Default is [128 128].  
    'Nxy' - Number of sub images along each dimension [Nx, Ny].  
    If empty then use SubSizeXY. Default is [].  
    'OverlapXY' - Overlapping extra [X, Y] to add to SubSizeXY  
    from each side. Default is [32 32].  
    Output : - CCDSEC of the images with overlap [xmin, xmax, ymin, ymax].  
    A line per sub image.  
    - CCDSEC of the images without overlap.  
    - Two coloum vector of [X,Y] centers of the first output CCDSEC.  
    - Number of sub images in each dimension. [Nx, Ny].  
    - The CCDSEC of the non overlap region in the new image.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [CCDSEC,unCCDSEC,Center,Nxy]=imUtil.image.subimage_grid([256 258],'SubSizeXY',[64 64])  
    [CCDSEC,unCCDSEC,Center,Nxy]=imUtil.image.subimage_grid([256 258],'Nxy',[5 4])  
    Reliable: 2  
      
      
### imUtil.image.subimages2image

construct the full image from sub-images Package: mUtil.image


    
    construct the full image from sub-images  
    Package: mUtil.image  
    Input  : - A cell array of images, or a structure array of images (for  
    that purpose imCl and SIM are struct).  
    Each sub image can be an image which size is equivalent to the  
    size of the image in each CCDSEC line, or a scalar.  
    If this is a scalar, then it will be duplicated all over the  
    sub image.  
    - a matrix of CCDSEC [xmin xmax ymin ymax]. One line per  
    elelement in the sub images array.  
    The CCDSEC positions, refers to the final position of the sub  
    images in the reconstrcted full image.  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
    The following keywords are available:  
    'FieldName' - Field name in a struct output in which to store  
    the sub images. Default is 'Im'.  
    This is good also for a imCl or SIM input.  
    'StitchMethod' - Stitching method:  
    'IgnoreOverlap' - Ignore overlap between images.  
    This option is fast.  
    'MeanOverlap'   - Take the nan mean over overlap  
    regions.  
    This option is slow and requires a lot  
    of memory (size of full image X number  
    of sub images).  
    Default is 'IgnoreOverlap'.  
    Output : - Full reconstucted image.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [SubImage,CCDSEC,Center,NoCCDSEC]=imUtil.image.partition_subimage(Image,[],'SubSizeXY',[512 512],'OverlapXY',[16 16],'Output','struct');  
    [FullImage]=imUtil.image.subimages2image(SubImage,CCDSEC);  
    Reliable: 2  
      
      
### imUtil.image.trim

Trim an image or a cube using CCDSEC coordinates. Pacakge: imUtilimage Description: Trim an image or a cube using CCDSEC coordinates.


    
    Trim an image or a cube using CCDSEC coordinates.  
    Pacakge: imUtilimage  
    Description: Trim an image or a cube using CCDSEC coordinates.  
    Input  : - An image or a cube in which the 3rd dimension is the image  
    index. If empty, then will return only the CCDSEC vector.  
    - CCDSEC vector.  
    Either: [Xmin, Xmax, Ymin, Ymax] if type = 'ccdsec'  
    [Xcenter, Ycenter, Xhalfsize, Yhalfsize] if  
    type='center'  
    or [Xhalfsize, Yhalfsize] around central pixel if type =  
    'center'.  
    - Type. Either 'ccdsec', or 'center'.  
    Output : - A trimmed image or cube.  
    - A CCDSEC vector.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: D=rand(100,100);  
    imUtil.image.trim(D,[2 10 2 10])  
    Reliable: 2  
      
      
### imUtil.image.wmean_sigclip

Calculate the sigma-clipped weighted mean of a dataset Package: imUtil.image Description: Calculate the sigma-clipped weighted mean of a dataset with iterations. This is done by first selecting the datapoints to be used


    
    Calculate the sigma-clipped weighted mean of a dataset  
    Package: imUtil.image  
    Description: Calculate the sigma-clipped weighted mean of a dataset with  
    iterations.  
    This is done by first selecting the datapoints to be used  
    useing imUtil.image.mean_sigclip and applying the weighted  
    mean to the result.  
    If the provided variances are all zeros than will set it to  
    1.  
    Input  : - The dataset of any dimensionality.  
    - The variance of the dataset.  
    - Dimension over which to calculate the mean.  
    For example, if you have a cube of images in which the  
    3rd dimension is the image index, and you want to calculate  
    the mean image. Set Dim=3.  
    Deafult is ndims(Data).  
    * Arbitrary number of ...,key,val,... arguments to pass to  
    imUtil.image.mean_sigclip:  
    The following keywords are available:  
    'MeanFun' - Funtion handle for calculating the mean.  
    Default is @nanamean.  
    'StdFun'  - A string indicating the method by which to  
    calculate the data StD.  
    Options are: 'std' - fo normal StD, or 'rstd' for  
    robust StD calculated using imUtil.background.rvar.  
    Default is 'rstd'.  
    'Nsigma' - [Lower, Upper] number of sigmas below/above to  
    sigma clip the data.  
    Default is [5 5].  
    'MaxIter' - Maximum number of iterations.  
    Use 0 in order to calculate the mean without sigma  
    clipping. Will stop before maximum number of iterations  
    reached if no new points were clipped.  
    Default is 3.  
    Output : - Sigma clipped weighted mean of data.  
    - Sigma clipped weighted variance of adat.  
    - A logical array of the same size of the input Data which  
    indicate the good data points that were used for the mean  
    calculation.  
    - An image of number of images used in each pixel.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Sep 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data = randn(10,10,100);  
    Data(4,4,18)=100; Data(1,1,1)=-14;   some outliers  
    [Mean,VarFlagGood,NC]=imUtil.image.mean_sigclip(Data,3);  
    [i,j,k]=ind2sub(size(Data),find(~FlagGood))  
    Reliable: 2  
      
      
