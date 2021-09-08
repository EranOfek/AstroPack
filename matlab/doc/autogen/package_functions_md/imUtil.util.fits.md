# Package: imUtil.util.fits


### imUtil.util.fits.fitswrite

Write a FITS file with a single image in HDU=1 Package: +imUtil.util.fits


    
    Write a FITS file with a single image in HDU=1  
    Package: +imUtil.util.fits  
    Input  : - Data  
    - File Name.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'Header' - A 3 column cell array of [key, val, comment].  
    Third column is optional.  
    Alternatively, and headCl or HEAD class.  
    Default is {}.  
    'Type' - Data class. Default is 'single'.  
    'HDU' - Default is 1 (FFU).  
    imUtil.util.fits.fitswrite(rand(10,10),'aaa.fits')  
      
      
      
