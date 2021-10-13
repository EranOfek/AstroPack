# Package: imProc.instCharc


### imProc.instCharc.gainFromFlat

Estimate the gain from flat image/s in native units. Estimated by dividing the mean by the variance.


    
    Estimate the gain from flat image/s in native units.  
    Estimated by dividing the mean by the variance.  
    Input  : - An AstroImage object with flat images.  
    * ...,key,val,...  
    'Method'   - One of the following estimation methods:  
    ['block'] - Estimate the gain based on a single image by  
    calculating the mean and var in each blck (sub image).  
    'pix' - Estimate the gain based on the mean and var of multiple  
    images in each pixel. The result is gain per pixel  
    image.  
    'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of the image  
    section on which to estimate the gain.  
    Output : - A structure array of results. The returned fields depands  
    on the method. For the 'block' method return an element  
    per image with the following fields:  
    'Gain' - Estimated gain in each single image.  
    'GainRMS' - Std of estimated gain per image.  
    'GainErr' - Error of estimated gain per image.  
    For the 'pix' method, return a single element with the  
    following fields:  
    'Gain' - An image of gain per pixel.  
    'GlobalGain' - median of image gain.  
    gain is returned in native units.  
    Author : Eran Ofek (Jun 2021)  
    Example: Image = poissrnd(ones(1000,1000).*1e4)./2.2;  
    AI = AstroImage({Image});  
    Result = imProc.instCharc.gainFromFlat(AI)  
    for I=1:1:10, Cell{I} = poissrnd(ones(1000,1000).*1e4)./2.2; end  
    AI = AstroImage(Cell);  
    Result = imProc.instCharc.gainFromFlat(AI, 'Method','pix')  
      
### imProc.instCharc.linearity

Estimate the non-linearity of a detector using the imUtil.calib.pixel_flat_response function


    
    Estimate the non-linearity of a detector  
    using the imUtil.calib.pixel_flat_response function  
    Input  : - An AstroImage object.  
    * ...,key,val,...  
    'Method' - Options are:  
    ['constFlux'] - Estimate the non-linearity of each  
    pixel by hypothesis testing of two models:  
    c+x and c+x+x^2.  
    This assumes that the source of light is  
    constant, so the intensity is linear with the  
    exposre time.  
    'KeyExpTime' - Header keyword name (or dictionary name)  
    containing the exp. time. Default is 'EXPTIME'.  
    'UseDict' - A logical indicating if to use dictionary for  
    the exp. time keyword. Default is true.  
    'CCDSEC' - CCDSEC on which to calculate the non-linearity.  
    If empty, use all image. Default is [].  
    'Gain' - Detector gain. Default is 1.  
    'ReadNoise' - Detector readnoise. Default is 5 e-.  
    'Model' - Models to fit. Default is {'c+x','c+x+x^2'}.  
    See imUtil.calib.pixel_flat_response for details.  
    Output : - A structure of the fit results.  
    See imUtil.calib.pixel_flat_response for details.  
    Author : Eran Ofek (Jun 2021)  
    Example: Im = ones(500,500);  
    AI = AstroImage({poissrnd(Im.*1e3), poissrnd(Im*3e3), poissrnd(Im.*1e4), poissrnd(Im.*1e4), poissrnd(Im.*2e4)});  
    AI(1).HeaderData.insertKey({'EXPTIME',1}); AI(2).HeaderData.insertKey({'EXPTIME',3}); AI(3).HeaderData.insertKey({'EXPTIME',10});  
    AI(4).HeaderData.insertKey({'EXPTIME',10}); AI(5).HeaderData.insertKey({'EXPTIME',20});  
    Result = imProc.instCharc.linearity(AI);  
      
### imProc.instCharc.readNoiseFromBias

Estimate the read noise from bias mage/s in native units. The read noise is estimated by calculating the rms in small blocks (sub images) of a bias image, or by calculating the rms per pixel (in multiple images).


    
    Estimate the read noise from bias mage/s in native units.  
    The read noise is estimated by calculating the rms in small blocks (sub  
    images) of a bias image, or by calculating the rms per pixel (in  
    multiple images).  
    Input  : - An AstroImage object with bias images.  
    * ...,key,val,...  
    'Method'   - One of the following estimation methods:  
    ['block'] - Estimate the RN based on a single image by  
    calculating the rms in each blck (sub image).  
    'pix' - Estimate the RN based on the rms of multiple  
    images in each pixel. The result is RN per pixel  
    image.  
    'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of the image  
    section on which to estimate the read noise.  
    Parameters for the 'block' method:  
    'BlockSize' - Block size in which to calc readnoise.  
    Default is [32 32].  
    'BlockStdFun' - Function handle for calculating the std in  
    the blck. Default is @imProc.stat.rstd.  
    'BlockMeanFun' - Function handle for calculating the global  
    readnoise from the blocks readnoise.  
    Default is @tools.math.stat.nanmedian.  
    'BlockMeanStdFun' - Function handle for calculating the rms  
    of the global readnoise.  
    Default is @imUtil.background.rstd.  
    Parameters for the 'pix method:  
    'PixStdFun' - Function handle for calculating the std per  
    pixel. Default is @imUtil.background.rstd.  
    'PixStdFunArgs' - A cell array of arguments to pass to the  
    PixStdFun function. Default is {3}.  
    Output : - A structure array of results. The returned fields depands  
    on the method. For the 'block' method return an element  
    per image with the following fields:  
    'ReadNoise' - Estimated read noise in each single image.  
    'ReadNoiseRMS' - Std of estimated read noise per image.  
    'ReadNoiseErr' - Error of estimated read noise per image.  
    For the 'pix' method, return a single element with the  
    following fields:  
    'ReadNoise' - An image of read noise per pixel.  
    Read noise is returned in native units.  
    Author : Eran Ofek (Jun 2021)  
    Example: AI = AstroImage({randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100});  
    Result = imProc.instCharc.readNoiseFromBias(AI)  
    Result = imProc.instCharc.readNoiseFromBias(AI, 'Method','pix')  
      
### imProc.instCharc.unitTest

unitTest for imProc.instCharc Example: Result = imProc.instCharc.unitTest


    
    unitTest for imProc.instCharc  
    Example: Result = imProc.instCharc.unitTest  
      
