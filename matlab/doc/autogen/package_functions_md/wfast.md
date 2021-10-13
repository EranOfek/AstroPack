# Package: wfast


### wfast.createMatchedSources

Example: Result = wfast.createMatchedSources('WFAST_Balor_20210529-011312-278_F505W_0_Image.h5z') [Result,JD] = wfast.createMatchedSources('WFAST_Balor_20210529-0025*','UseRegExp',false) [Result,JD] = wfast.createMatchedSources('WFAST*.h5z','UseRegExp',false)


    
      
    Example: Result = wfast.createMatchedSources('WFAST_Balor_20210529-011312-278_F505W_0_Image.h5z')  
    [Result,JD] = wfast.createMatchedSources('WFAST_Balor_20210529-0025*','UseRegExp',false)  
    [Result,JD] = wfast.createMatchedSources('WFAST*.h5z','UseRegExp',false)  
      
### wfast.cutouts_photometry

Examples: AI(1) = wfast.read2AstroImage('WFAST_Balor_20200801-020634-879_F505W_0_CutoutsStack.h5z','ReadType','cutouts','calibrate',false,'InterpOverNan',false); AI(2) = wfast.read2AstroImage('WFAST_Balor_20200801-020630-880_F505W_0_CutoutsStack.h5z','ReadType','cutouts','calibrate',false,'InterpOverNan',false);


    
      
    Examples: AI(1) = wfast.read2AstroImage('WFAST_Balor_20200801-020634-879_F505W_0_CutoutsStack.h5z','ReadType','cutouts','calibrate',false,'InterpOverNan',false);  
    AI(2) = wfast.read2AstroImage('WFAST_Balor_20200801-020630-880_F505W_0_CutoutsStack.h5z','ReadType','cutouts','calibrate',false,'InterpOverNan',false);  
      
    wfast.cutouts_photometry(AI)  
      
      
      
### wfast.read2AstroImage

Read W-FAST image from HDF5 file into an AstroImage and calibrate the image using the wfast calibration object (dark subtraction, flat correction). Populate the header, mask image, image, and interpolate over NaNs. REQUIREMENTS: addpath('/home/eran/matlab/GUYN/util')


    
    Read W-FAST image from HDF5 file into an AstroImage and calibrate the  
    image using the wfast calibration object (dark subtraction, flat  
    correction). Populate the header, mask image, image, and interpolate over  
    NaNs.  
    REQUIREMENTS: addpath('/home/eran/matlab/GUYN/util')  
    addpath('/home/eran/matlab/GUYN/WFAST/wfast')  
    Input  : - File name, or file name with regular expression.  
    * ...,key,val,...  
    'ReadType' - Which dataset to read:  
    'image' | 'stack' | 'cutouts'  
    Default is 'image'.  
    'UseRegExp' - A logical indicating if to use regular  
    expressions when interpreting file name.  
    Default is true.  
    'Calibrate' - Apply dark subtraction and flat correction.  
    Default is true.  
    'CalibDir' - W-FAST calibration directory.  
    'CalibFile' - W-FAST calibration file name.  
    'CalibObj' - Optional calibration object (from calibration  
    file). If empty, thenm will attempt to load from  
    calibration file. Default is [].  
    'InterpOverNan' - Interpolate over NaNs. Default is true.  
    Output : - An AstroImage object with loaded images, headers, and  
    masks.  
    - The calibration object.  
    Example: cd /data/euler/archive/WFAST/2021/2021-05-28/quadrature_run1  
    Result = wfast.read2AstroImage('WFAST_Balor_20210529-011410-415_F505W_0_Image.h5z');  
    AI =  
    wfast.read2AstroImage('WFAST_Balor_20200801-020630-880_F505W_0_CutoutsStack.h5z','ReadType','cutouts','calibrate',false,'InterpOverNan',false);  
      
      
