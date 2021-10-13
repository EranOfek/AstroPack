# Package: tools.interp


### tools.interp.bessel_icoef

Calculate the Bessel interpolation coefficiant. Package: Util.interp Description: Calculate the Bessel interpolation coefficiant.


    
    Calculate the Bessel interpolation coefficiant.  
    Package: Util.interp  
    Description: Calculate the Bessel interpolation coefficiant.  
    Input  : - Order.  
    - Interpolation factor (P=(X-X0)./H).  
    Output : - Bessel interpolation coeff.  
    Notes  : Used by interp_diff.m  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
    B{1} = inline('P - 0.5','P');  
      
    B{1} = @(P)P;  
    B{2} = @(P)0.25.*P.*(P - 1);  
    B{3} = @(P)(P - 0.5).*P.*(P - 1)./6;  
    B{4} = @(P)0.5.*(P + 1).*P.*(P - 1).*(P - 2)./24;  
    B{5} = @(P)(P - 0.5).*(P + 1).*P.*(P - 1).*(P - 2)./120;  
    B{6} = @(P)0.5.*(P + 2).*(P + 1).*P.*(P - 1).*(P - 2).*(P - 3)./720;  
      
      
### tools.interp.interp1_nan

Interpolate over NaNs in 1-D vector. Package: Util.interp Description: Interpolate over NaNs in 1-D vector.


    
    Interpolate over NaNs in 1-D vector.  
    Package: Util.interp  
    Description: Interpolate over NaNs in 1-D vector.  
    Input  : - X  
    - Y  
    * Additional parameters to pass to interp1.m.  
    E.g., 'cubic'.  
    Output : - Y values at all X values including those at which Y was NaN.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=(1:1:5); Y=[1 2 NaN 4 5]; YI=Util.interp.interp1_nan(X,Y);  
    Reliable: 2  
      
      
### tools.interp.interp1_sinc

1-D sinc interpolation Package: Util.interp Description: Interpolation of a 1-D array using the Whittaker–Shannon interpolation formula (i.e., sinc interpolation).


    
    1-D sinc interpolation  
    Package: Util.interp  
    Description: Interpolation of a 1-D array using the Whittaker–Shannon  
    interpolation formula (i.e., sinc interpolation).  
    Input  : - Coloumn vector of values sampled at equal intervals.  
    The index (i.e., position) of this values is assumed to be  
    1 to N, where N is the vector length.  
    - Vector of positions in which to perform the sinc  
    interpolation.  
    Output : - Interpolated values.  
    Reference: https://en.wikipedia.org/wiki/Weighted_median  
    License: GNU general public license version 3  
    Tested : Matlab R2015a  
    By : Eran O. Ofek                    Jun 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=(1:1:100)'; Y=sin(2.*pi.*X./10); Yxx=Util.interp.interp1_sinc(Y,[2.1 3.4])  
    XX = [1:0.1:100]; Yxx=Util.interp.interp1_sinc(Y,XX)  
    Reliable: 2  
      
      
      
### tools.interp.interp1evenlySpaced

A faster versio of interp1q for evenly spaced data (linear interpolation).


    
    A faster versio of interp1q for evenly spaced data (linear interpolation).  
    Input  : - X (equally spaced and sorted)  
    - Y  
    - Vector of new X values in which to interpolate.  
    Output : - Vector of interpolated Y values.  
    Author : Eran Ofek (Oct 2021)  
    Example: X = (1:2:100); Y = (1:2:100); NewX = [3.1, 4.1, 5.9];  
    tic, for I=1:1:10000, NewY = tools.interp.interp1evenlySpaced(X,Y,NewX); end, toc  
    tic; for I=1:1:10000, NewY=interp1(X,Y,NewX); end, toc  
    tic; for I=1:1:10000, NewY=interp1q(X,Y,NewX); end, toc  
      
### tools.interp.interp1lanczos

1-D Lanczos interpolation Package: Util.interp Description: 1D Lanczos interpolation.


    
    1-D Lanczos interpolation  
    Package: Util.interp  
    Description: 1D Lanczos interpolation.  
    Input  : - Equally spaced vector to interpolate.  
    - Positions to which to interpolate.  
    - Lanczos order (2|3). Default is 2.  
    Output : - Interpolated vector.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    May 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: YI=Util.interp.interp1lanczos([1;1;1.05;1.1;1.05;1;1],1.5)  
    Reliable:  
      
      
### tools.interp.interp2fast

Faster version of interp2 Package: Util.interp Description: A faster version of the interp2.m built in function. This function is faster than interp2.m when the XI and YI vectors span over a small range in the VecX and VecY


    
    Faster version of interp2  
    Package: Util.interp  
    Description: A faster version of the interp2.m built in function.  
    This function is faster than interp2.m when the XI and YI  
    vectors span over a small range in the VecX and VecY  
    space.  
    Input  : - Vector defining the X coordinates in the interpolated matrix.  
    - Vector defining the Y coordinates in the interpolated matrix.  
    - Matrix to interpolate.  
    - X values in which to interoplate the matrix.  
    - Y values in which to interpolate the matrix.  
    * Arbirtrary number of input arguments to pass to interp2.m  
    (e.g., interpolation method). Otherwise will use the interp2  
    defaults.  
    Output : - The interoplated vector.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Aug 2013  
    UR: : http://weizmann.ac.il/home/eofek/matlab/  
    Example: InterpTrace = Util.interp.interp2fast(VecX,VecY,ScienceImage,XI,YI,'linear');  
    Reliable: 2  
    -  
### tools.interp.interp3p

Stirling interpolation Package: Util.interp Description: Given three, equally spaced, data points (Y), and a vector of normalized positions in units in which the three data points are at [-1,0,1], return, based on the Stirling


    
    Stirling interpolation  
    Package: Util.interp  
    Description: Given three, equally spaced, data points (Y), and a vector  
    of normalized positions in units in which the three data  
    points are at [-1,0,1], return, based on the Stirling  
    interpolation formula, the values of the interpolation  
    function at the vector of positions, and information  
    regarding the extramum of the interpolation function.  
    Input  : - Vector of three values specified at positions [-1,0,1].  
    - Vector of positions at which to calculate the values  
    of the interpolation function.  
    Output : - Values of the interpolation function.  
    - Position of extramum.  
    - Value at extramum.  
    - 2nd derivative at extramum.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    May 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Fp,ExtramP,ExtramFp,Extram2D]=Util.interp.interp3p([10,1,8],[-1:0.01:1]');  
    Reliable:  
      
      
### tools.interp.interp_diff

Interpolation based on 4th order Stirling formula Package: Util.interp Description: Interpolation of equally spaced data using high-order differences.


    
    Interpolation based on 4th order Stirling formula  
    Package: Util.interp  
    Description: Interpolation of equally spaced data using  
    high-order differences.  
    Input  : - Equally spaced and (asendingly) sorted X.  
    - Y  
    - X values for which to interpolate.  
    - Degree of differences, default is 4.  
    - Check if X is equally spaced {true|false}, default is false.  
    Output : - Interpolated Y values.  
    Return NaN in case that extrapolation is needed.  
    - Stirling interpolation polynomial (only for 4th deg).  
    Each line contains a4, a3, a2, a1, a0 coeff.  
    then: f_p=(((a4*p+a3)*p+a2)*p+a1)*p+a0 ,  
    where p is the interpolation factor: p=(X-X0)./H  
    The polynomial is suitable for use in the range -0.5<p<0.5,  
    and it may be adequate in the range -2<p<2.  
    - The tabulation interval H.  
    - X0 of the interpolation for each point/polynomial.  
    See also: find_local_extremum.m, find_local_zeros.m, interp_diff_ang.m  
    Reference: Seidelmann 1992, Explanatory Supp. to the Astron. Almanac  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=[-5:1:5].';  Y=(X-0.2).^2;  
    [Yi,SPoly,H,X0]=Util.interp.interp_diff(X,Y,0.2);  
    Reliable: 1  
      
### tools.interp.interp_diff_ang

Stirling 4th order interpolation for angular values Package: tools.interp Description: Given a vector of time and a vector of coordinate on a sphere interpolate the spherical coordinate in a list of times. This function is talking into account


    
    Stirling 4th order interpolation for angular values  
    Package: tools.interp  
    Description: Given a vector of time and a vector of coordinate on a sphere  
    interpolate the spherical coordinate in a list of times.  
    This function is talking into account  
    [0..2pi] angels discontinuity.  
    Input  : - Equally spaced and (asendingly) sorted X.  
    - Y [angle in radians in range 0..2pi].  
    - X values for which to interpolate.  
    - degree of differences, default is 4.  
    - Check if X is equally spaced {'y' | 'n'}, default is 'n'.  
    Output : - Interpolated Y values.  
    Return NaN in case that extrapolation is needed.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    May 2006  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference: Seidelmann 1992, Explanatory Supp. to the Astron. Almanac  
    See also: find_local_extremum.m; interp_diff.m  
    Reliable: 2  
    -  
### tools.interp.interp_diff_longlat

Bessel interpolation of equally space time series of lon/lat coordinates Package: Util Description: Interpolate equally space time series of lon/lat coordinates using 4th order Bessel differences interpolation.


    
    Bessel interpolation of equally space time series of lon/lat coordinates  
    Package: Util  
    Description: Interpolate equally space time series of lon/lat coordinates  
    using 4th order Bessel differences interpolation.  
    Input  : -  
    - degree of differences, default is 4.  
    - Check if X is equally spaced {'y' | 'n'}, default is 'n'.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Long,Lat]=Util.interp.interp_diff_longlat(  
    Reliable:  
      
      
      
