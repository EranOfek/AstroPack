# Package: tools.find


### tools.find.bin_sear

Binary search for a value in a sorted vector. Package: Util.find Description: Binary search for a value in a sorted vector. If the value does not exist, return the closest index.


    
    Binary search for a value in a sorted vector.  
    Package: Util.find  
    Description: Binary search for a value in a sorted vector.  
    If the value does not exist, return the closest index.  
    Input  : - sorted vector (ascending).  
    - value to search.  
    Output : - Index of closest value.  
    See also: find_bin.c  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Sep 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: IndVal=bin_sear([1:1:12]',5.4);  
    Reliable: 1  
      
### tools.find.bin_sear2

bin_sear function                                                 FitFun Description: Binary search for a value in a sorted vector. If the value does not exist, return the closes index.


    
      
    bin_sear function                                                 FitFun  
    Description: Binary search for a value in a sorted vector.  
    If the value does not exist, return the closes index.  
    Input  : - sorted vector (ascending).  
    - value to search.  
    Output : - index of closest value.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Sep 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: IndVal=bin_sear([1:1:12]',5.4);  
    Reliable: 2  
      
### tools.find.bin_sear3

bin_sear function                                                 FitFun Description: Binary search for a value in a sorted vector. If the value does not exist, return the closes index.


    
      
    bin_sear function                                                 FitFun  
    Description: Binary search for a value in a sorted vector.  
    If the value does not exist, return the closes index.  
    Input  : - sorted vector (ascending).  
    - value to search.  
    Output : - index of closest value.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    September 1994  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: IndVal=bin_sear([1:1:12]',5.4);  
      
### tools.find.find_local_extremum

Use stirling interpolation to find local extremum in vector. Package: tools.find Description: Given table of equally spaced data, use Stirling interpolation formula to find the local extremums of


    
    Use stirling interpolation to find local extremum in vector.  
    Package: tools.find  
    Description: Given table of equally spaced data, use Stirling  
    interpolation formula to find the local extremums of  
    the tabulated data. The program find all the local  
    extremums between X(Deg/2) to X(end-Deg/2), where  
    Deg is the degree of interpolation.  
    Input  : - Equally spaced (and asendingly sorted) X.  
    - Y  
    - Order (currently supports only 4th degree), default is 4.  
    Output : - List of all local extremums:  
    values of [X, Y, 2nd derivative d^2Y/dX^2] in the points of  
    local extremums.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: interp_diff.m, find_local_zeros.m  
    Example: X=(1:1:100).'; Y=(X-10).^2; find_local_extremum(X,Y);  
    Reliable: 2  
    -  
### tools.find.find_local_zeros

- find_local_zeros function                                            FitFun Description: Given table of equally spaced data, use Stirling interpolation formula to find the local zeros of


    
    -  
    find_local_zeros function                                            FitFun  
    Description: Given table of equally spaced data, use Stirling  
    interpolation formula to find the local zeros of  
    the tabulated data. The program find all the local  
    zeros between X(Deg/2) to X(end-Deg/2), where  
    Deg is the degree of interpolation.  
    Input  : - Equally spaced (and asendingly sorted) X.  
    - Y  
    - Order (currently supports only 4th degree), default is 4.  
    Output : - List of all zeros:  
    values of [X, 1st derivative, 2nd derivative d^2Y/dX^2]  
    in the points of zeros.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    BUGS   : Fails to find some zeros  
    see also: interp_diff.m, find_local_extremum.m  
    Reliable: 2  
    -  
### tools.find.find_peak

find_peak function                                                   General Description: Given a tabulated function [X,Y], find the maximum near a given X0.


    
      
    find_peak function                                                   General  
    Description: Given a tabulated function [X,Y], find the maximum  
    near a given X0.  
    Input  : - X of tabulated function.  
    - Y of tabulated function.  
    - X0 around at which to search for the maximum.  
    - Method for maximum position estimation  
    {'max'|'wmean'|'stirling4'},  
    default is 'stirling4'.  
    The 'stirling4' calls find_local_extremum.m function.  
    - Semi width around X0 in which to search for the maximum,  
    default is 3.  
    - Optional selection method for 'stirling4' - if more than  
    one maximum is found in region, then select the  
    {'nearest'|'highest'}, default is 'heighest'.  
    - Optional half window size for find extremum using the  
    'stirling4' option, default is 10.  
    Output : - X position of of the maximum.  
    - Y value at maximum.  
    - Error in X position (only for 'wmean' option).  
    - Maximum type flag:  
    0  : Maximum is found inside search region  
    1  : Maximum is found on boundry of left side region,  
    however it is higher than the value Y to its left.  
    2  : Maximum is found on boundry of left side region,  
    however it is lower than the value Y to its left.  
    3: : Search region is out of vector (left) range.  
    -1  : Maximum is found on boundry of right side region,  
    however it is higher than the value Y to its right.  
    -2  : Maximum is found on boundry of right side region,  
    however it is lower than the value Y to its right.  
    -3: : Search region is out of vector (right) range.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                     Mar 2007  
    URL : http://wweizmann.ac.il/home/eofek/matlab/  
    Example: X=(1:1:10).'; Y=ones(size(X)); Y(4:6)=[2 3 2.1];  
    [MaxX,MaxY,ErrX,Flag]=find_peak(X,Y,4);  
    Reliable: 2  
      
### tools.find.find_peak_center

find_peak_center function                                        General Description:


    
      
    find_peak_center function                                        General  
    Description:  
    Input  : - Evenly spaced X position.  
    If empty then will use 1 to length of Y.  
    - Y values.  
    * Arbitrary number of ...,key,val,... input arguments.  
    The following keywords are available:  
    'Method' - One of the following fitting method:  
    'max'    - maximum  
    'wmean'  - Mean weighted by inverse of counts (Y).  
    'wmeanbs'- Mean weighted by inverse of counts (Y)  
    after median background subtraction.  
    'str'    - Striling interpolation.  
    Return all peaks.  
    'strh'   - Striling interpolation.  
    Return highest peak only.  
    'strn'   - Striling interpolation.  
    Return peak nearest to center (default).  
    'Ind' - Indices of X/Y vectors to use in search.  
    If empty use all. Default is empty.  
    Output : - Max X value. Return NaN if no peak found.  
    - Max Y value. Return NaN if no peak found.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=(1:1:10).'; Y=ones(size(X)); Y(4:6)=[2 3 2.1];  
    [MaxX,MaxY]=find_peak_center(X,Y,'Method','strh');  
    Reliable: 2  
      
      
      
      
### tools.find.fun_binsearch

fun_binsearch function                                            FitFun Description: Given a monotonic function, Y=Fun(X), and Y, search for X that satisfy Y=F(X). The search is done using a binary


    
      
    fun_binsearch function                                            FitFun  
    Description: Given a monotonic function, Y=Fun(X), and Y, search for  
    X that satisfy Y=F(X). The search is done using a binary  
    search between the values stated at X range.  
    Input  : - Function [i.e., Y=Fun(X)].  
    - Y value to search  
    - X range [min max] in which to search for a solution.  
    - Relative tolerance, default is 1e-3.  
    - Additional optional parameters of Fun  
    Output : - X value corresponds to the input Y value.  
    Tested : Matlab 6.5  
    By : Eran O. Ofek                    Feb 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Fun=@(x,a) a+x  
    Mid=fun_binsearch(Fun,6,[0 100],1e-6,1)  
    Notes: Previously called: binsear_f.m  
    Reliable: 1  
      
### tools.find.mfind_bin

Binary search on a vector running simolutnously on multiple values. Package: tools.find Description: Binary search on a vector running simolutnously on multiple values. A feature of this program is that it


    
    Binary search on a vector running simolutnously on multiple values.  
    Package: tools.find  
    Description: Binary search on a vector running simolutnously on  
    multiple values. A feature of this program is that it  
    you need to add 1 to the index in order to make sure  
    the found value is larger than the searched value.  
    Input  : - Sorted column vector.  
    - Row vector of values to search.  
    Output : - Indices of nearest values.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=sort(rand(1e6,1)); Vals=rand(1,1e5);  
    Im=tools.find.mfind_bin(X,Vals)  
    Reliable: 2  
      
      
