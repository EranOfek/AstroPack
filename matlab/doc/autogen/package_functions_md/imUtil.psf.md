# Package: imUtil.psf


### imUtil.psf.curve_of_growth

Calculate the curve of growth of a PSF Package: imUtil.psf Description: Calculate the curve of growth of a PSF.


    
    Calculate the curve of growth of a PSF  
    Package: imUtil.psf  
    Description: Calculate the curve of growth of a PSF.  
    Input  : - 2-D matrix of a PSF.  
    - PSF center [x,y] coordinates. If [], or not provided, default  
    is half the PSF matrix size.  
    - Step size of curve of growth. Default is 1.  
    Output : - A structure with the radiao profile, including the following  
    fields:  
    .Radius - radius  
    .Sum    -sum  
    .Npix   - number of pixels in annulus  
    .Mean   - mean  
    .Med    - median  
    .CumSum - cumulative sum.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Curve=imUtil.psf.curve_of_growth(imUtil.kernel2.gauss)  
    Reliable: 2  
      
      
### imUtil.psf.fwhm_fromBank

Measure the FWHM of an image by cross corr. with a Gaussian template bank Package: +imUtil.psf Description: Measure the FWHM of an image by cross corr. with a Gaussian template bank and choose the best FWHM by the mode of most


    
    Measure the FWHM of an image by cross corr. with a Gaussian template bank  
    Package: +imUtil.psf  
    Description: Measure the FWHM of an image by cross corr. with a Gaussian  
    template bank and choose the best FWHM by the mode of most  
    detection above some S/N.  
    Input  : - An image in matrix format.  
    * list of ...,key,val,...  
    'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of region in which to  
    measure FWHM. If empty use entire image. Default is [].  
    'MinSN' - Minimum S/N to use. Default is 50.  
    'Background' - A background image. Default is [].  
    'Variance'   - A variance image. Default is [].  
    'SigmaVec'   - Vector of the Gaussian bank sigmas.  
    This should not include a sharp object (such a  
    sharp object is added by the code).  
    Default is logspace(0,2,5).  
    'MinStars'   - Minimum numbre of stars needed to estimate  
    FWHM. Default is 5.  
    'PixScale'   - Pixel scale ["/pix]. Default is 1.  
    'Method'     - Method:  
    'bisec' - Bi-sector search  
    'MaxNdet' - Choose the filter with the max  
    number of detections.  
    'MaxNdetInterp' - Same as 'MaxNdet', but with  
    interpolation over number of detections.  
    Default is 'bisec'.  
    'MaxIter'    - Numbre of iterations for the 'bisec' method.  
    Default is 6.  
    Output : - FWHM [arcsec].  
    - Number of stars used for estimating the FWHM.  
    Author : Eran Ofek (Mar 2021)  
    Example: FWHM=imUtil.psf.fwhm_fromBank(AI.Image);  
      
      
