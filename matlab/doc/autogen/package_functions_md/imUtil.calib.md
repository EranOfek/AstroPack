# Package: imUtil.calib


### imUtil.calib.fit_overscan

fit the overscan region in an image and subtract Package: +imUtil.calib Description: Given an image and an the coordinates of the over scan region in CCDSEC format (i.e., [Xmin Xmax Ymin Ymax]),


    
    fit the overscan region in an image and subtract  
    Package: +imUtil.calib  
    Description: Given an image and an the coordinates of the over scan  
    region in CCDSEC format (i.e., [Xmin Xmax Ymin Ymax]),  
    calculate the overscan 1-D function, and optionally subtract  
    it from the image.  
    Input  : - A cube of images in which the image index is in the 3rd  
    dimension.  
    If not provided then will run in simulation mode.  
    * Pairs of ...,key,val,... arguments. Options are:  
    'Dim' - Dimension over which to average the overscan.  
    If empty, then will attempt to guess this by taking the  
    dimension in 'OverSec' over which the number of pixels  
    is maximal.  
    Default is empty.  
    'OverSec' - A vector of [Xmin Xmax Ymin Ymax] of the overscan  
    region. This parameter must be provided.  
    Default is empty.  
    'MeanFun' - A function handle for collapsing the overscan  
    region over the 'Dim' dimension.  
    Default is @nanmedian.  
    'MeanFunPar' - A cell array of additional parameters to pass  
    to the 'MeanFun'. If the dimension is given, then it  
    must be 1. Default is {}.  
    'SmoothFun' - A function handle to use for smoothing the  
    collapsed overscan vector.  
    If empty, then do not smooth.  
    Default is empty.  
    'SmoothFunPar' - A cell array of additional parameters to pass  
    to the 'SmoothFun'. If the dimension is given, then it  
    must be 1. Default is {}.  
    'RemoveOS' - A logical indicating if to remove the overscan  
    region from the image.  
    Default is true.  
    'ReadNoise' - Detector readnoise in units in which the image  
    is provided. Default is 10.  
    'ThresholdDiffSigma' - In the diff of the un-smoothed overscan  
    rows in which the abs value of the diffrence is above a  
    threshold are flaged.  
    The threshold is calculated from ReadNoise/sqrt(NpixOS)  
    *ThresholdDiffSigma, whree NpixOS is the number of  
    pixels in the overscan.  
    Default is 8.  
    Output : - A structure with the following fields:  
    .OverScanSmooth - The smooth overscan.  
    .OverScanVec - The collapse overscan before smoothing.  
    .OverScanDiff - the diff of OverScanVec, where te first  
    element is NaN.  
    .Threshold - The threshold in units of the image.  
    .FlagLargeDiff - A vector of logical indicating if a  
    row/column in the overscan has a jump which is more than  
    ThresholdDiffSigma above the expected noise.  
    - Overscan subtrcated image.  
    - Vector of X indices in in which the non-overscan image reside.  
    - Vector of Y indices in in which the non-overscan image reside.  
    By: Eran O. Ofek                         Jun 2020  
    Example: imUtil.calib.fit_overscan(rand(10,10),'OverSec',[1 3 1 10])  
      
### imUtil.calib.pixel_flat_response

Fit the pixel response to light as a function of intensity in a cube of images Package: +imUtil.calib Description: Given a cube of flat field images which have different mean intensity, fit some linear models to each model intensity


    
    Fit the pixel response to light as a function of intensity in a cube of images  
    Package: +imUtil.calib  
    Description: Given a cube of flat field images which have different mean  
    intensity, fit some linear models to each model intensity  
    vs. a user specified expected intensity or the mean value of  
    the image.  
    This function can be used to fit the flat image in each  
    pixel, to fit non-linaeity and to look for pixels with  
    anomalous response.  
    The function by default fit a null hypothesis model of the  
    form intensity=alpha*MeanIntensity (where alpha is a fitted  
    free parameter).  
    In addition the function fit additional user specified  
    models (e.g., offset + linear term, or quadratic model).  
    The Delta Chi^2 between these models and the null hypothesis  
    is calculated.  
    Input  : - A cube of images in which the image index is in the 3rd  
    dimension.  
    If not provided then will run in simulation mode.  
    * Pairs of ...,key,val,... arguments. Options are:  
    'Gain' - Gain. The cube will be multiplied by these factor.  
    This is needed beacuse the function assume the images  
    noise is Poisson. Default is 1.  
    'ReadNoise' - Readnoise in electrons, used for the \chi^2  
    calculation. Default is 5.  
    'MeanFun' - If 'Intensity' is not porovided, this is a function  
    handle that will operate on each image in the cube in  
    order to calculate its mean value.  
    Default is @nanmedian.  
    'MeanFunPar' - A cell array of additional parameters to pass  
    to the 'MeanFun' function handle.  
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
    By: Eran O. Ofek                         Apr 2020  
    Example: A=[1 2;3 4;5 6]; Cube(:,:,1)=A; Cube(:,:,2)=A.*2; Cube(:,:,3)=A.*5;  
    Cube(:,:,4)=A.*7; Cube(:,:,5)=A.*10; Cube(:,:,6)=A.*30; Cube=Cube.*1000;  
    CubeP = poissrnd(Cube);  
    Res=imUtil.calib.pixel_flat_response(CubeP)  
      
### imUtil.calib.resid_vs_mag

Fit residuals vs. mag and flag good data (not outliers). Package: +imUtil.calib Description: Give vectors of residuals vs. magnitude, calculate the std or residuls vs. magnitude and flag good data (i.e., not


    
    Fit residuals vs. mag and flag good data (not outliers).  
    Package: +imUtil.calib  
    Description: Give vectors of residuals vs. magnitude, calculate the std  
    or residuls vs. magnitude and flag good data (i.e., not  
    outliers).  
    Done using either polynomial fit, to the residuals vs.  
    magnitude. In this case the std is global.  
    Or, binning the data and calculate the mean and std in each  
    bin. Outliers are defined to be ThresholdSigma times the std  
    above the mean value.  
    Input  : - A vector of magnitudes.  
    - A vector of residuals (one per magnitude).  
    * Pairs of ...,key,val,... arguments. Options are:  
    'MagRange' - Magnitude range in which to calculate the  
    residiuals. If [], then choose by min max.  
    Default is [].  
    'BinMethod' - Method to use:  
    'poly' - polynomial fit.  
    'bin' - binning the data.  
    Default is 'bin'  
    'PolyDeg' - Polynomial degree for the polynomial fit.  
    Default is 3.  
    'BinSize' - Bin size for binning. Default is 1 (mag).  
    'FunMean' - A function handle to use when calculating the mean  
    of the data in each bin.  
    Default is @nanmedian.  
    'FunStd' - A function handle to use when calculating the std  
    of the data in each bin, or when calculating the global  
    std after the polynomial fit.  
    Default is @imUttil.background.rstd.  
    'InterpMethod' - Interpolation method. Default is 'linear'.  
    'ThresholdSigma' - Threshold in sigmas (std) for flagging good  
    data. Default is 3.  
    'Plot' - A logical flag indicating if to plot the residuals  
    vs. mag. Default is false.  
    Output : - a vector of logical flags (one per mag/resid) indicating if  
    the source residual is smaller than the threshold (true; good  
    source) or above the threshold (false; outlier).  
    - A structure with the following fields:  
    .Mag - Vector of input Mag after removing out of range values.  
    .Resid - Vector of input Resid after removing out of range  
    values.  
    .InterpMeanResid - Vector of mean interpolated mean residual  
    at the source magnitude.  
    .InterpStdResid - Vector of interpolated or global std of  
    residuals at the source mag.  
    By: Eran O. Ofek                         Jun 2020  
    Example: Flag=imUtil.calib.resid_vs_mag(Mag,Resid);  
      
