# Class: astro.spec.InterpLC



    
      
    InterpLC  class  
    Description: A static class for interpolating photometric light curves  
    Input  : null  
    Output : null  
    Tested : Matlab R2018a  
    By : Eran O. Ofek                    Mar 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### InterpLC

Interpolant from a time series of photometric observations in one band Package: astro.spec Description: Given a time series of observations take at a single band return an interpolant that allows to calculate the magnitude


    
    Interpolant from a time series of photometric observations in one band  
    Package: astro.spec  
    Description: Given a time series of observations take at a single band  
    return an interpolant that allows to calculate the magnitude  
    in each time within observations range.  
    Input  : - [Time, Mag, Err]  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'MaxTimeNoObs' - Maximum time without observations.  
    If gap larger than this exist in the data the  
    interpolant will not be valid in the gap.  
    Default is 5.  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Interp=astro.spec.InterpLC(Data)  
    Reliable:  
      
      
