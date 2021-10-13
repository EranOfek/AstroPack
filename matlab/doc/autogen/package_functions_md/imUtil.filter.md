# Package: imUtil.filter


### imUtil.filter.autocor

Autocorrelation of a 2D image Package: imUtil.filter Description: Autocorrelation of a 2D image, with optional background subtraction and StD normalization.


    
    Autocorrelation of a 2D image  
    Package: imUtil.filter  
    Description: Autocorrelation of a 2D image, with optional background  
    subtraction and StD normalization.  
    Input  : - A matrix.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'SubBack' - Subtract background and divide by std the image  
    prior to the autocorrelation.  
    Default is true.  
    'BackFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @nanmedian.  
    'BackFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {'all'}.  
    'VarFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @imUtil.backgroundau.rvar.  
    'VarFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {}.  
    'SubSizeXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [128 128].  
    'OverlapXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [16 16].  
    'MinVariance' - The minimum variance in in the 2D histogram,  
    That is used to calculate the S/N.  
    Default is 1.  
    Output : - The Aitocorrekation function  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [AC,Res]=imUtil.filter.autocor(imUtil.kernel2.gauss);  
    [AC,Res]=imUtil.filter.autocor(randn(30,31));  
    Reliable: 2  
      
      
      
### imUtil.filter.conv2_fast

convolve two 2-D matrices using either fft or conv, whichever faster. Package: imUtil.filter Description: Convolve two matrices in which the second matrix is smaller or equal in size to the first matrix. If the second matrix is smaller then it is post-padded with


    
    convolve two 2-D matrices using either fft or conv, whichever faster.  
    Package: imUtil.filter  
    Description: Convolve two matrices in which the second matrix is smaller  
    or equal in size to the first matrix.  
    If the second matrix is smaller then it is post-padded with  
    zeros. The convolution is done using either  
    imUtil.filter.fft or conv2, which ever is estimated to be  
    faster.  
    Input  : - A matrix.  
    - A matrix.  
    - UseFFT: Method to use. [] - auto, otherwise logical. Default is [].  
    - Padding before operation (padded region will be removed from  
    output).  
    '' - do nothing (Default).  
    'circular' - circular boundry conditions.  
    'replicate' - relpicate nearest edge value.  
    'symmetric' - mirror reflection boundry conditions.  
    Output : - Convolution between the two matrices.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: imUtil.filter.conv2_fast(rand(1000,1000),rand(1000,1000));  
      
    Reliable: 2  
      
      
### imUtil.filter.conv2_fft

convolve two 2-D matrices using fft. Package: imUtil.filter Description: Convolve two matrices in which the second matrix is smaller or equal in size to the first matrix. If the second matrix is smaller then it is post-padded with


    
    convolve two 2-D matrices using fft.  
    Package: imUtil.filter  
    Description: Convolve two matrices in which the second matrix is smaller  
    or equal in size to the first matrix.  
    If the second matrix is smaller then it is post-padded with  
    zeros. The convolution is done using fft.  
    Input  : - A matrix.  
    - A matrix.  
    Output : - Convolution between the two matrices (or filtered image).  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: imUtil.filter.conv2_fft(rand(1000,1000),rand(1000,1000));  
    Reliable: 2  
      
      
      
### imUtil.filter.crdetect_lacos

L.A. cosmic cosmic ray detection for astronomical images Package: imUtil.filter Description: Find and remove cosmic rays in an astronomical image using the L.A.cosmic algorithm (van Dokkum 2001).


    
    L.A. cosmic cosmic ray detection for astronomical images  
    Package: imUtil.filter  
    Description: Find and remove cosmic rays in an astronomical image  
    using the L.A.cosmic algorithm (van Dokkum 2001).  
    Input  : - Image matrix.  
    * Arbitrary number of pairs of input arguments ...,key,val,...  
    The following keywords are available:  
    'RepInf'- Replace infinities with a constant.  
    If empty, do nothing, otherwise this is the constant.  
    Default is 1e6.  
    'RN'   - CCD read noise [e-] for noise estimation.  
    Default is 10.  
    This can be an image of readnoise in e- per pixel.  
    'Nsigma' - CR detection threshold in sigma. Default is 10.  
    'Fth'    - Fine structure threshold. Default is 2.  
    If FWHM is provided than this parameter is  
    overrided.  
    'FWHM'   - PSF FWHM to estimate 'Fth' based on Figure 4  
    in van Dokkum (2001).  
    'BWmorph'- If not empty, then increase the CR effected area  
    by running a morphological filter using bwmorph.m.  
    Default is 'dilate'.  
    Other useful options are: 'majority' and 'bridge'.  
    'BWmorphN'-Number of times to run the morphological filter.  
    'MaskType' - Type of bit mask to create. Default is 'uint16'.  
    'IntMethod'- inpaint_nans.m interpolation method. Default is 2.  
    Output : - Bit mask of flag image.  
    If 'Bit_CR' is not provided than this is a flag image (logicals)  
    in which pixels affected by CR are true, all the rest are  
    false.  
    If 'Bit' is not empty, then this is a bit mask in which  
    of the CR affected pixels.  
    - Image interpolated over the CRs.  
    Reference: http://www.astro.yale.edu/dokkum/lacosmic/  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: A=randn(6,6)+10; A(3,3)=1000;  
    [ImageCR,CleanImage]=imUtil.filter.crdetect_lacos(A);  
    Reliable: 2  
      
      
### imUtil.filter.filter2_fast

Source/template detection in 2D images by filtering (cross-correlation) Package: imUtil.filter Description: 2D filtering (cross correkation) of an image (matrix) with a template (filter). The program chooeses which algorith to use, either filtering via convolution or fft, which ever


    
    Source/template detection in 2D images by filtering (cross-correlation)  
    Package: imUtil.filter  
    Description: 2D filtering (cross correkation) of an image (matrix) with a  
    template (filter). The program chooeses which algorith to  
    use, either filtering via convolution or fft, which ever  
    supposed to be faster given the filter size compared to the  
    matrix size.  
    Input  : - A matrix.  
    - A filter.  
    - UseFFT: Method to use. [] - auto, otherwise logical. Default is [].  
    - Padding before operation (padded region will be removed from  
    output).  
    '' - do nothing (Default).  
    'circular' - circular boundry conditions.  
    'replicate' - relpicate nearest edge value.  
    'symmetric' - mirror reflection boundry conditions.  
    Output : - Convolution between the two matrices.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: imUtil.filter.filter2_fast(rand(1000,1000),rand(1000,1000));  
    Reliable: 2  
      
      
      
### imUtil.filter.filter2_fft

cross-correlate (filter) two 2-D matrices using fft. Package: imUtil.filter Description: 2D Cross correlation (filtering) of two matrices. The second matrix is the "filter" and should be smaller or equal in size to the first matrix. Note that in filter2 the


    
    cross-correlate (filter) two 2-D matrices using fft.  
    Package: imUtil.filter  
    Description: 2D Cross correlation (filtering) of two matrices.  
    The second matrix is the "filter" and should be smaller or  
    equal in size to the first matrix. Note that in filter2 the  
    order of the inputs is reversed.  
    Unlike filter2, this function calculate the  
    cross-correlation using fft and hence is much faster in  
    cases in which the filter matrix is large.  
    Input  : - A matrix.  
    - A matrix (or filter).  
    - Method:  
    'rot' - rotate by 180 and convolve using fft (slowest).  
    'conj' - use conj, and make sure that result is not shifted.  
    For kernels that go to zeros at the edge, this is  
    equilavlent to rot, with the exceprtion of the image  
    boundries.  
    'conjpad' - like conj but the padding is done in the fft  
    stage (slower than 'conj').  
    'Conjsh' - Use conj, but don't shift by +1.  
    The output is shifted (cyclicaly) by +1 compared to the  
    output of 'rot' and 'conj'. (fastest).  
    Default is 'conj'.  
    Output : - Convolution between the two matrices (or filtered image).  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: imUtil.filter.filter2_fft(rand(1000,1000),rand(1000,1000));  
    Reliable: 2  
      
      
### imUtil.filter.filter2_fftfft

cross-correlate a 2-D matrix with bank using fft. The input maybe ffted Package: imUtil.filter Description: 2D Cross correlation (filtering) of two matrices, or a matrix with a template bank of matrices. The second matrix is the "filter" and should be smaller or


    
    cross-correlate a 2-D matrix with bank using fft. The input maybe ffted  
    Package: imUtil.filter  
    Description: 2D Cross correlation (filtering) of two matrices, or a  
    matrix with a template bank of matrices.  
    The second matrix is the "filter" and should be smaller or  
    equal in size to the first matrix. Note that in filter2 the  
    order of the inputs is reversed.  
    Unlike imUtil.filter2_fft in which both inputs are in the  
    space domain, here the input matrices may be either in the  
    space domain or frequency domaon.  
    Input  : - A matrix.  
    - A matrix (filter), or a cube in which the third dimension is  
    the template index.  
    - A logical flag indicating if the first input matrix is ffted.  
    Default is false.  
    - A logical flag indicating if the second input matrix is ffted.  
    Default is false.  
    If true, then the size of the second matrix must be equal to  
    the size of the first matrix.  
    Output : - Cross-correlation between the matrices.  
    If the second matrix is a cube, then will return a cube which  
    size is equal to the size of the second matrix. The third  
    dimension of the cube is the filter index.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: imUtil.filter.filter2_fftfft(rand(1000,1000),rand(1000,1000));  
    Reliable: 2  
      
      
### imUtil.filter.filter2_sn

Filter an image with a PSF and calculate the S/N and Flux estimators Package: imUtil.filter Description: Filter an image with a template/PSF and calculate the signal-to-noise ratio (S/N) for template-detection in each pixel, and the flux-estimator at


    
    Filter an image with a PSF and calculate the S/N and Flux estimators  
    Package: imUtil.filter  
    Description: Filter an image with a template/PSF and  
    calculate the signal-to-noise ratio (S/N) for  
    template-detection in each pixel, and the flux-estimator at  
    each pixel.  
    In order to find sources, you further need to identify  
    local maxima in the S/N  
    image (first output argument). The S/N of the source is the  
    S/N at the local maximum location, and the flux estimator of  
    the source is the Flux-image (second output argument) at  
    this local maximum.  
    See imUtil.background.<TAB> for background and variance  
    estimation functions.  
    See imUtil.image.local_maxima for source search options.  
    Input  : - An image.  
    - A background map, or scalar. If the input image is background  
    subtracted then give here 0.  
    If empty, or not provided then use imUtil.background.mode  
    to calculate the robust background.  
    The mode is calculated with the Log=false option in order to  
    avoid the log of negative numbers.  
    - A variance image (or scalar).  
    If empty, or not provided use imUtil.background.mode  
    to calculate the robust variance.  
    - Template (no assumption on normalization / will be normalized  
    to have unity sum).  
    Output : - S/N image (detection statistics). This is the S/N per pixel  
    for detecting the template.  
    In order to find sources, identify local maxima above your  
    threshold S/N.  
    The S/N image is give by FiltImage./sqrt(FiltImageVar)  
    - The flux estimator image, given by: FiltImage./FiltImageVar.  
    This is the estimator for a template flux at each pixel  
    position. This is roughlu equivalent to a PSF photometry (the  
    main difference is sub-pixel position).  
    - Filtered image (FiltImage).  
    - The variance image of the filtered image (FiltImageVar).  
    - Structure containing additional information.  
    The follwoing fields are available:  
    'Back' - Background image.  
    'Var' - Variance image.  
    'NormVar' -  The normalization factor you need to multiply  
    the original image variance in order to get the  
    variane of the filtred image.  
    'Template' - The unit-sum-normalized template used for the  
    detection.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: imUtil.filter.filter_snBank  
    Example: Image=randn(1024,1024); Variance=ones(1024,1024);  
    Template = imUtil.kernel2.gauss(1.5);  
    SN=imUtil.filter.filter2_sn(Image,0,Variance,Template);  
    Image = imUtil.kernel2.gauss(1.5,[1024 1024]).*300 + 10.*randn(1024,1024);  
    [SN,Flux]=imUtil.filter.filter2_sn(Image,0,Variance,Template);  
    SN(513,513) contains max S/N of expectency value of 5.64: (=30./sqrt(4.*pi.*1.5.^2.*1))  
    Flux(513,513) contains te flux estimator with expectency value of 30.  
    Reliable: 2  
      
      
      
### imUtil.filter.filter2_snBank

Filter an image with a bank of PSFs and calculate the S/N and Flux estimators Package: imUtil.filter Description: Filter an image with a template/PSF bank and calculate the signal-to-noise ratio (S/N) for template-detection in each pixel, and the flux-estimator at


    
    Filter an image with a bank of PSFs and calculate the S/N and Flux estimators  
    Package: imUtil.filter  
    Description: Filter an image with a template/PSF bank and  
    calculate the signal-to-noise ratio (S/N) for  
    template-detection in each pixel, and the flux-estimator at  
    each pixel.  
    In order to find sources, you further need to identify  
    local maxima in the S/N  
    image (first output argument). The S/N of the source is the  
    S/N at the local maximum location, and the flux estimator of  
    the source is the Flux-image (second output argument) at  
    this local maximum.  
    See imUtil.background.<TAB> for background and variance  
    estimation functions.  
    See imUtil.image.local_maxima for source search options.  
    Input  : - An image.  
    - A background map, or scalar. If the input image is background  
    subtracted then give here 0.  
    If empty, or not provided then use imUtil.background.mode  
    to calculate the robust background.  
    The mode is calculated with the Log=false option in order to  
    avoid the log of negative numbers.  
    - A variance image (or scalar).  
    If empty, or not provided use imUtil.background.mode  
    to calculate the robust variance.  
    - Template bank (no assumption on normalization / will be  
    normalized to have unity sum).  
    Templatemay be a single matrix, or a cube in which the third  
    dimension is the template index.  
    Alternativel, this can be a function handle that returns a  
    matrix or cube of templates.  
    Default is @imUtil.kernel2.gauss.  
    * Additional arguments to passt to the function handle that  
    generate the template bank.  
    Default is {}.  
    Output : - S/N image or cube (detection statistics). This is the S/N per pixel  
    for detecting the template.  
    In order to find sources, identify local maxima above your  
    threshold S/N. Template = imUtil.kernel2.gauss(1.5);  
    The S/N image is give by FiltImage./sqrt(FiltImageVar)  
    - The flux estimator image or cube, given by: FiltImage./FiltImageVar.  
    This is the estimator for a template flux at each pixel  
    position. This is roughlu equivalent to a PSF photometry (the  
    main difference is sub-pixel position).  
    - Filtered image or cube (FiltImage).  
    - The variance image of the filtered image (FiltImageVar).  
    - Structure containing additional information.  
    The follwoing fields are available:  
    'Back' - Background image.  
    'Var' - Variance image.  
    'NormVar' -  The normalization factor you need to multiply  
    the original image variance in order to get the  
    variane of the filtred image.  
    A vector, one element per template.  
    'Template' - The unit-sum-normalized template cube  used for  
    the  
    detection.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: imUtil.filter.filter_sn  
    Example: Image=randn(1024,1024); Variance=ones(1024,1024);  
    SN=imUtil.filter.filter2_snBank(Image);  
    SN=imUtil.filter.filter2_snBank(Image,[],[]);  
    An example for focusing a source  
    Image = imUtil.kernel2.gauss(2.4,[1024 1024]).*30 + randn(1024,1024);  
    Template = imUtil.kernel2.gauss((1:0.5:6).');  
    SN=imUtil.filter.filter2_snBank(Image,[],[],Template);  
    or equivalently:  
    SN=imUtil.filter.filter2_snBank(Image,[],[],@imUtil.kernel2.gauss,(1:0.5:6).');  
    plot((1:0.5:6).', squeeze(SN(513,513,:)))  
    Reliable: 2  
      
      
### imUtil.filter.filter2_var

Calculate the variance and mean filter of an image Package: imUtil.filter Description: Calculate the variance and mean filter of an image in circular apertures.


    
    Calculate the variance and mean filter of an image  
    Package: imUtil.filter  
    Description: Calculate the variance and mean filter of an image in  
    circular apertures.  
    Input  : - A matrix.  
    - Filter radius.  
    Output : - The variance filter.  
    - The mean filter.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mean,Var]=imUtil.filter.filter2_var(rand(100,100),3);  
    Reliable: 2  
      
      
      
### imUtil.filter.filter_sources

Generate a background image filtered from sources using sucessive filtering Description: This routine filter out sources from an image and generate an approximate background image. The following steps are implemented: 1. Filtering the image against template bank of sources.


    
    Generate a background image filtered from sources using sucessive filtering  
    Description: This routine filter out sources from an image and  
    generate an approximate background image.  
    The following steps are implemented:  
    1. Filtering the image against template bank of sources.  
    2. Set the value around sources found by step 1 to NaN.  
    3. Use inpaint_nans to interpolate over NaNs.  
    4. Smooth the image using another filter (default is annulus).  
    5. if Niter>1 go to step 1, and find sources this time using  
    the newly estimated background.  
    Input  : - A 2D image.  
    * ...,key,val,...  
    'Back' - Background matrix for first iteration.  
    If empty, estimate the background.  
    Default is [].  
    'Var' - Varince matrix for first iteration.  
    If empty, estimate the variance.  
    Default is [].  
    'Threshold' - Sources detection threshold (sigmas).  
    Default is 3.  
    'BackFun' - Function handle for background estimation.  
    Default is @median.  
    'BackFunArgs' - A cell array of additional arguments to  
    pass to the background estimation function.  
    Default is {[1 2],'omitnan'}.  
    'VarFun' - Function handle for variance estimation.  
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
    the SmoothFilter funcion.  
    If one then more filter is generated by this  
    function, then the image will be filtered with each  
    of these filters, and the mean will be used.  
    Default is {[16 24; 24 28;28 32]}.  
    'PadMethod' - Padding before convolution method.  
    Default is 'symmetric'.  
    'VarFromBack' - If true, for the next iteration use Var=Back.  
    Otherwise, use the origibal Var image.  
    'Niter' - Number of iterations. Default is 1.  
    Output : - The smooth background image.  
    - The background image prior to smoothing.  
    Author : Eran Ofek (Jul 2021)  
    Example: AI=AstroImage('PTF_Cropped.fits');  
    AI=cast(AI,'double'); AI.Image = AI.Image.*1.6;  
    [SmBackEst, BackEst] = imUtil.filter.filter_sources(AI.Image)  
      
### imUtil.filter.imlaplacian

Laplacian filter for a 2-D matrix Package: imUtil.filter Description: Calculate the laplacian of a 2-D image using a convolution kernel. This function is considerably faster than del2.m


    
    Laplacian filter for a 2-D matrix  
    Package: imUtil.filter  
    Description: Calculate the laplacian of a 2-D image using a convolution  
    kernel.  
    This function is considerably faster than del2.m  
    Input  : - 2-D matrix.  
    Output : - The laplacian of the image.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: R=randn(2048,2048)+100; R(900,700)=1000;  
    Lap=imUtil.filter.imlaplacian(R);  
    Reliable: 2  
      
      
### imUtil.filter.threshold_fluctuations

Estimate the number of filtered local maxima above/below some threshold. Package: imUtil.filter Description: Estimate the two-sided false alarm rate for source detection in astronomical images. The function generate random images, filter the images with a Gaussian with a specified FWHM,


    
    Estimate the number of filtered local maxima above/below some threshold.  
    Package: imUtil.filter  
    Description: Estimate the two-sided false alarm rate for source detection  
    in astronomical images. The function generate random images,  
    filter the images with a Gaussian with a specified FWHM,  
    and find connected local maxima above or below the user  
    specified threshold.  
    Input  : - Detection threshold. DEfault is 5.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'FWHM'    - Gaussian PSF FWHM [pix]. Default is 2.5.  
    'Nsim'    - Number of simulated images. Default is 1000.  
    'ImSize'  - Image Size [pix]. Default is 1024.  
    'PixScale'- Pixel scale [arcsec/pix].  
    Output : - Event rate above below threshold per deg^2.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [EventRate,Err]=imUtil.filter.threshold_fluctuations(5,'Nsim',100);  
    Reliable: 2  
      
      
### imUtil.filter.unpad_array

A simple unpadding of 2D matrices


    
    A simple unpadding of 2D matrices  
    Input  : - An image.  
    - PadSize [I, J], if scalar use the same pad length for both  
    axes.  
    Output : - An image with the trimmed padded area.  
    Author : Eran Ofek (Jul 2021)  
    Example: Array = ones(3,4);  
    Array = padarray(Array,[3 3],0,'both')  
    imUtil.filter.unpad_array(Array,[3 3])  
      
### imUtil.filter.xcorr2_fft

Cross-correlation of two matrices using fft, and search local maxima. Package: imUtil.filter Description: Cross correlate two 2D images. If a single image is provided then calculate the autocorrelation function. The cross-correlation is done using fft (unlike xcorr2.m).


    
    Cross-correlation of two matrices using fft, and search local maxima.  
    Package: imUtil.filter  
    Description: Cross correlate two 2D images. If a single image is provided  
    then calculate the autocorrelation function.  
    The cross-correlation is done using fft (unlike xcorr2.m).  
    Input  : - A 2D image.  
    - A 2D reference image.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'NormMethod' - Method by which to normalize the cross  
    correlation. Options are:  
    'StD' - by the std of the data (i.e., xcorr  
    definition).  
    'max' - By the maximum of the data.  
    This may be useful when the noise properties  
    are not well defined.  
    Default is 'StD'.  
    'MaxMethod' - The method by which the 2D histogram peaks will  
    be selected. The following options are available:  
    'thresh' - Select maxima larger than some threshold.  
    'max1' - Select the highest maxima.  
    'maxall' - Select all the maxima.  
    'max_fracmax' - Select all the maxima above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    'thresh_fracmax' - Select all the maxima above the  
    threshold and above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    Alternatively this can be a positive integer (N).  
    In this case will return the N highest maxima.  
    Default is 'thresh_fracmax'  
    'Threshold' - Peak selection threshold relative to the S/N.  
    Default is 5.  
    'FracOfMax' - The parameter that that used in '*_fracmax' and  
    for selecting peaks.  
    Only peaks that above the maximal peak multiplied by  
    this parameter will be returned.  
    'Conn' - local maxima finding connectivity parameter.  
    For details see imUtil.image.local_maxima.  
    Default is 8.  
    'SubBack' - Subtract background and divide by std the image  
    prior to the autocorrelation.  
    Default is true.  
    'BackFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @nanmedian.  
    'BackFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {'all'}.  
    'VarFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @imUtil.backgroundau.rvar.  
    'VarFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {}.  
    'SubSizeXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [128 128].  
    'OverlapXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [16 16].  
    'MinVariance' - The minimum variance in in the 2D histogram,  
    That is used to calculate the S/N.  
    Default is 0.  
    Output : - The cross correlation matrix, where zero shift is at the  
    center of the matrix.  
    - A structure containing information about the cross-corr  
    matrix. The following fields are available:  
    'VecX' - Vector of X shifts corresponding to the X-axis of the  
    output matrix.  
    'VecY' - Vector of Y shifts corresponding to the Y-axis of the  
    output matrix.  
    'X0'   - Index of the 0 shift X-axis position in the  
    cross-corr matrix.  
    'Y0'   - Index of the 0 shift Y-axis position in the  
    cross-corr matrix.  
    - A structure array containing information about the local  
    maxima of the cross-correlation matrix.  
    The following fields are available:  
    'StD' - StD of the cross-correlation matrix.  
    'MaxVal' - Value of peak  
    'MaxX' - X position of peak in cross-cor. matrix.  
    'MaxY' - Y position of peak in cross-cor. matrix.  
    'MaxSN' - S/N (relative to the StD) of the peak.  
    'ShiftX' - X-axis shift of first image relative to the  
    reference image.  
    'ShiftY' - Y-axis shift of first image relative to the  
    reference image.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [XC,Res]=imUtil.filter.xcorr2_fft(imUtil.kernel2.gauss,imUtil.kernel2.gauss);  
    [XC,Res,ResPeak]=imUtil.filter.xcorr2_fft(imUtil.kernel2.gauss(2,[41 30],[20 15])+randn(30,41).*0.001,imUtil.kernel2.gauss(2,[41 30],[22 10]),'NormMethod','max');  
    [XC,Res,ResPeak]=imUtil.filter.xcorr2_fft(randn(30,31));  
    Reliable: 2  
      
      
      
