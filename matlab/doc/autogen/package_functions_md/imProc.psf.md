# Package: imProc.psf


### imProc.psf.measureFWHM

Estimate image seeing or focus state


    
    Estimate image seeing or focus state  
    Input  : - AN image. Either a matrix, a cell of matrices, or an  
    AstroImage object.  
    * list of ...,key,val,...  
    'MinSN' - Minimum S/N to use. Default is 50.  
    'Background' - A background image. Default is [].  
    'Variance'   - A variance image. Default is [].  
    'SigmaVec'   - Vector of the Gaussian bank sigmas.  
    This should not include a sharp object (such a  
    sharp object is added by the code).  
    Default is logspace(0,1,25).'  
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
    Author : Eran Ofek (Aug 2021)  
    Example: [F,N]=imProc.psf.measureFWHM(AI)  
      
### imProc.psf.psf2cube

Construct a cube of PSF stamps from AstroImage


    
    Construct a cube of PSF stamps from AstroImage  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'StampSize' - PSF Stamp size. Empty for default. Default is empty.  
    'DataPSF' - see AstroPSF/getPSF for details.  
    'FunPSF' - see AstroPSF/getPSF for details.  
    'ArgVals' - see AstroPSF/getPSF for details.  
    'ArgNames' - see AstroPSF/getPSF for details.  
    Output : - A cube of PSFs, in which the PSF index is along the 3rd  
    dimension.  
    Author : Eran Ofek (Jun 2021)  
    Example: NOT TESTED  
      
