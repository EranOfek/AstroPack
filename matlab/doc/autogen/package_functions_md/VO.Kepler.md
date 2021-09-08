# Package: VO.Kepler


### VO.Kepler.read_lc_file

Read Kepler light curve FITS file. Package: VO.Kepler Description: Read kepler light curve fits file.


    
    Read Kepler light curve FITS file.  
    Package: VO.Kepler  
    Description: Read kepler light curve fits file.  
    Input  : - String containing file name.  
    Output : - AstCat object containing the LC.  
    - Head object containing the first extension header.  
    Tested : Matlab 7.8  
    By : Eran O. Ofek                    Jan 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [LC,Head]=VO.Kepler.read_lc_file('kplr000757076-2009131105131_llc.fits')  
    Reliable: 2  
      
      
