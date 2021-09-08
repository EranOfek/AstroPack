# Package: imUtil.util.fit


### imUtil.util.fit.circ_fit

Fit a circle to a set of 2D coordinates on a plane or the sphere Package: +imUtil.util.fit Description: Fit points, on a plane or a sphere, to a circle. Calculate the best fit radius and the center of the circle.


    
    Fit a circle to a set of 2D coordinates on a plane or the sphere  
    Package: +imUtil.util.fit  
    Description: Fit points, on a plane or a sphere, to a circle.  
    Calculate the best fit radius and the center of the circle.  
    Input  : - Matrix of data [RA, Dec, Property].  
    Where RA can be a column vector [radians],  
    or matrix of [H M S].  
    Dec can be a column vector [radians],  
    or matrix of [Sign D M S].  
    An optional column of a property of each point.  
    - Geometry:  
    'sphere' - celestial sphere.  
    'plane'  - plane geometry.  
    - Minimizing method:  
    'rms'  - by radii rms  
    'diff' - by max(R)-min(R)  
    'rd'   - by (max(R)-min(R))/R  
    - Number of scanning steps, default is 200.  
    Output : - Best center [X,Y] or [RA,Dec] in radians.  
    - Best radius in radian.  
    - Best RMS  
    - Best max(R)-min(R)  
    - Best (max(R)-min(R))/R  
    - Information on each point, one line per point, sorted by PA:  
    [Distance from best center [radians],  
    PA relative to best center [radians],  
    Property of point (if not given then = NaN),  
    Clockwise angular distance from next point on the circle [rad]  
    Index of respective line in original data matrix]  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                   January 2004  
    Reliable: 1  
      
      
### imUtil.util.fit.linfit_sc

Linear fitting with weights and sigma clipping Package: +imUtil.util.fit Description:


    
    Linear fitting with weights and sigma clipping  
    Package: +imUtil.util.fit  
    Description:  
    Input  : - Vector of X.  
    - Vector of Y.  
    - Sclar or vector of errors in Y. If scalr, assume all Ys have  
    the same errors. Default is 1.  
    * Pairs of ...,key,val,... The following keywords are available:  
    'Funs' - A cell array of functions that construct the linear  
    transformation of the form: Par1*Fun{1} +  
    Par2*Fun{2}+...  
    Default is {@(x) ones(size(x)), @(x) x}  
    I.e., Y=a+b*x  
    'MaxIter' - Maximum number of outlier rejection iterations.  
    Default is 2.  
    'ClipMethod' - Method to clip outliers.  
    Default is 'sigclip'  
    'StdFun' - A function handle by which to calculate the  
    residuals std.  
    Default is @imUtil.background.rstd  
    'MeanFun' -  A function handle by which to calculate the  
    residuals mean.  
    Default is @median  
    'Nsigma' - [low high] number of sigma below, above the mean to  
    reject outliers. Default is 3.  
    Output : - Best fit parameters. Given in order of Funs.  
    - Best fit errors in parameters. Given in order of Funs.  
    - Structure with the following information:  
    .Resid - Vector of residuals from best fit  
    .Std - std of residuals  
    .Chi2 - \chi^2  
    .Npar - number of parameters  
    .Ngood - number of good points  
    .Dof   - degrees of freedom  
    .Pchi2 - Cumulative probability of chi2 given dof.  
    .FlagGood - A vector of logical indicating the points used in  
    the last iteartion.  
    Example: X=(1:1:100)'; Y=X+randn(100,1).*0.5; Y(6)=100; Y(80)=70;  
    [Par,ParErr,Stat]=imUtil.util.fit.linfit_sc(X,Y,0.5)  
      
      
