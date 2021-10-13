# Package: tools.math.stat


### tools.math.stat.bc_a

bc_a function                                                      AstroStat Description: Bias correction and acceleration for bootstrap and Jackknife estimetors of the confidence interval. The bc_a bias coorection and acceleration can be used


    
      
    bc_a function                                                      AstroStat  
    Description: Bias correction and acceleration for bootstrap and  
    Jackknife estimetors of the confidence interval.  
    The bc_a bias coorection and acceleration can be used  
    to estimate the bias corrected and accelerated confiedence  
    interval (CI).  
    Input  : - bc_a bias correction (Z0).  
    - bc_a acceleration (Ac).  
    Output : - The new lower (A1) percentile to plug in err_cl.m in order  
    to get the bias corrected and accelerated CI.  
    - (A2) Like A1, but for the upper percentile.  
    Reference : Efron B. & Tibshirani, R.J., 1993,  
    in: An Introduction to the Bootstrap  
    see also: bootstrap_std.m, bc_a.m, err_cl.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                   October 2002  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example:  Looking for the CI for the 0.9545 CL:  
    [A1,A2]=bc_a(Z0,Ac,0.9545);   estimate "corrected" percentile,  
    CI=err_cl(Theta,0.9545);  
    Reliable: 1  
      
### tools.math.stat.bin2dFun

2-D binning and apply functions to bins


    
    2-D binning and apply functions to bins  
    Input  : - Vector of X coordinates.  
    - Vector of Y coordinates.  
    - Vector of values corresponding to X/Y positions.  
    * ...,key,val,...  
    'Fun' - A cell array of functions, to applay to the values  
    found in each bin. Each function is corresponding to an  
    output argument.  
    Default is {@numel, @mean, @median, @std};  
    'FunArgs' - A cell array of cell arrays of additional arguments to  
    pass to each function.  
    Default is {{}, {'all','omitnan'}, {'all','omitnan'}, {[],'all','omitnan'}}.  
    'RangeX' - [min, max] of X coordinates in which to  
    calculate the bins. If empty, then use the min and  
    max of the X coordinates. Default is [].  
    'RangeY' - [min, max] of Y coordinates in which to  
    calculate the bins. If empty, then use the min and  
    max of the Y coordinates. Default is [].  
    'StepX' - Bin size in X axis. Default is 1.  
    'StepY' - Bin siez in Y axis. Default is 1.  
    'Step' - Bin size in X and Y axes. If not empty, then  
    overriding both StepX and StepY. Default is [].  
    'Nbin' - Number of bins in each axis [X Y], or [X]. If given, overide  
    all step options. Default is [].  
    Output : * One argument per Function. Each argument is a 2D matrix,  
    corresponding to applying the function on the values in the bins.  
    Author : Eran Ofek (Jul 2021)  
    Example:  
    M=tools.math.stat.bin2dFun(rand(1000,1),rand(1000,1),rand(1000,1),'StepX',0.1,'StepY',0.1);  
    [M,~,S]=tools.math.stat.bin2dFun(rand(1000,1),rand(1000,1),rand(1000,1),'Step',0.1);  
      
### tools.math.stat.bootstrap_std

bootstrap_std function                                         AstroStat Description: Given an estimator (given by a function) calculate the Bootstrap StD for this estimator.


    
      
    bootstrap_std function                                         AstroStat  
    Description: Given an estimator (given by a function) calculate the  
    Bootstrap StD for this estimator.  
    Input  : - Matrix of data set, row per measurment.  
    - Estimator function name,  
    Fun(Data,Additional_Parameters)  
    - Number of Bootstrap simulations.  
    - Additional parameters of 'Fun'.  
    Output : - Bootstrap StD.  
    - Vector of bootstrap estimator.  
    That could be used for plotting the estimator distribution,  
    or to find percentile confidence interval using err_cl.m  
    - Bootstrap estimate of the bias.  
    Subtract the bias from the estimator  
    to get the corrected estimator.  
    - bc_a bias correction (Z0).  
    - bc_a acceleration (Ac).  
    The bc_a bias coorection and acceleration can be used  
    to estimate the bias corrected and accelerated  
    confiedence interval.  
    For example: if you want the CI for the 0.9545 CL.  
    use the function [A1,A2]=bc_a(Z0,Ac,0.9545),  
    to estimate "corrected" percentile,  
    and then CI=err_cl(Theta,0.9545);  
    Reference : Efron B. & Tibshirani, R.J., 1993,  
    in: An Introduction to the Bootstrap  
    Example: [StD,Theta]=bootstrap_std(Data,'mean',1000,1)  
    then UnBiased_Theta = Theta - Bias  
    see also: jackknife.m, bc_a.m, err_cl.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2002  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [StD,Theta,Bias,Z0,Ac]=bootstrap_std(randn(100,1),@mean,1000);  
    Reliable: 1  
      
      
      
### tools.math.stat.cel_coo_rnd

cel_coo_rnd function                                               AstroStat Description: Generate random coordinates on the celestial sphere. The program can applay matrix rotation, reject coordinates, and generate non uniform coordinates.


    
      
    cel_coo_rnd function                                               AstroStat  
    Description: Generate random coordinates on the celestial sphere.  
    The program can applay matrix rotation, reject  
    coordinates, and generate non uniform coordinates.  
    [preliminary version].  
    Input  : - If scalar then: Number of random element to generate,  
    Else: a list of coordinates [Long, Lat, Err, ErrType] (in radians)  
    for which to add random error to each coordinates,  
    using the error column.  
    ErrType is: 0 - for Gaussian errors  
    1 - for Fisher errors ('Gaussian on a sphere').  
    * Aribtrary number of pairs of arguments:  
    ...,keyword,value,...  
    - Note the program execute the commands by their order  
    from first to last -  
    Possible keywords are:  
    'Rotate'       - Applay a rotation matrix to coordinates.  
    'RejLatRange'  - Reject Galactic latitude range [MinLat, MaxLat],  
    default is [] (don't rejuct).  
    'RejLongRange' - Reject Galactic longitute range [MinLat, MaxLat],  
    default is [] (don't rejuct).  
    default is [] (don't rejuct).  
    'RejPoly'      - Cell array of polygons to rejuct.  
    default is [] (don't rejuct).  
    'ProbLong'     - Galactic longitude probability [Long, Prob],  
    default is, [], flat probability (i.e., Prob=1).  
    'ProbLat'      - Galactic latitude probability [Lat, Prob],  
    default is, [], flat probability (i.e., Prob=1).  
    Output : - Random coordinates [Long, Lat] in radians.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                  November 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: RandCoo=cel_coo_rnd(1000,'RejLatRange',[-20 20; 85 86]./RAD);  
    RandCoo=cel_coo_rnd(1000,'RejLatRange',[-20 20; 85 86]./RAD,...  
    'Rotate',RotMat,'ProbLat',ProbLat);  
    (1) reject in Galactic  
    (2) rotate to equatorial  
    (3) applay probability map  
    RandCoo=cel_coo_rnd(1000,'RejLatRange',[-20 20; 85 86]./RAD,..  
    'Rotate',rotm_coo('G'),...  
    'ProbLat',[-pi./2 1; +pi./2 0.5]);  
    Reliable: 2  
      
      
### tools.math.stat.cell_stat

cell_stat function                                               AstroStat Description: Given a list of x,y coordinates (with optional property columns), count the number of points in each cell in the x-y plane and calculate the statistics (e.g., mean, medain


    
      
    cell_stat function                                               AstroStat  
    Description: Given a list of x,y coordinates (with optional property  
    columns), count the number of points in each cell in the  
    x-y plane and calculate the statistics (e.g., mean, medain  
    [ignore NaNs]) of the optional property in each cell.  
    Input  : - matrix of events in 2-d space [X, Y, Value, Err]  
    Value and Err are optional.  
    If Err is not given it is taken to be 1.  
    - Cell size [StepX, StepY].  
    - Space boundry [Xmin, Xmax, Ymin, Ymax].  
    - Minimum number of points for which to calculate statistics,  
    default is 0.  
    Output : - Vector of cells X-coordinate position.  
    - Vector of cells Y-coordinate position.  
    - Matrix of number of all points in each cell.  
    If value is not given in input then return NaN.  
    - Matrix of number of non-NaNs points in each cell.  
    If value is not given in input then return NaN.  
    - Matrix of mean value of point in each cell.  
    If value is not given in input then return NaN.  
    - Matrix of median value of point in each cell.  
    If value is not given in input then return NaN.  
    - Matrix of values weighted mean in each cell.  
    If value is not given in input then return NaN.  
    - Matrix of values weighted error in each cell.  
    If value is not given in input then return NaN.  
    - Matrix of Values StD in each cell.  
    If value is not given in input then return NaN.  
    - Matrix of Values information entropy  
    [-sum(V*ln(V))] in each cell.  
    If value is not given in input then return NaN.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek         July 2003  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### tools.math.stat.centermass2d

centermass2d function                                                 FitFun Description: Calculate the center of mass and second moments of 2-dimensional matrix.


    
      
    centermass2d function                                                 FitFun  
    Description: Calculate the center of mass and second moments of  
    2-dimensional matrix.  
    Input  : - Matrix or vector containing X coordinates of data.  
    - Matrix or vector containing Y coordinates of data.  
    - Matrix or vector containing data.  
    Output : - Structure containing the moments of data:  
    .X     - first moment (barycenter) in X  
    .Y     - first moment (barycenter) in Y  
    .X2    - second moment in X  
    .Y2    - second moment in Y  
    .XY    - second moment in XY  
    .ErrX  - Error in first momet in X.  
    .ErrY  - Error in first momet in Y.  
    .Theta - ellipsoid rotation  
    .R     - radius  
    .A     - major axis  
    .B     - minor axis  
    .Rho   - correlation  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                     April 2007  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: [MatX,MatY]=meshgrid([1:1:10],[1:1:10]);  
    G=bivar_gauss(MatX,MatY,[4.834 5.12 3 3 0]);  
    N = 1000; B=100;  
    [M]=center2d_cm(MatX,MatY,sqrt(G.*N+B));  
    Notes: previously called: enter2d_cm.m  
    Reliable: 2  
      
      
### tools.math.stat.confint_probdist

confint_probdist function                                          AstroStat Description: Calculate two-sided confidence interval of a given numerical probability distribution function.


    
      
    confint_probdist function                                          AstroStat  
    Description: Calculate two-sided confidence interval of a given numerical  
    probability distribution function.  
    Input  : - Two column matrix of the probability distribution function [X,dP]  
    - List of cumulative probabilities in which to report the  
    position X such that P=\int_{-\infty}^{X}{dP(X)dx}.  
    Default is [0.0013, 0.0227, 0.1587, 0.5, 0.8413, 0.9773, 0.9987],  
    which correspinds to -3,-2,-1,0,1,2,3 sigma.  
    - Interpolation method (see interp1.m for options).  
    Default is 'linear'.  
    - Epsilon value (for non monotonic series) default is 1e-12.  
    Output : - The X position such that P=\int_{-\infty}^{X}{dP(X)dx}.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: x=[-100:0.1:100]'; p=exp(-x.^2./2);  
    PX=confint_probdist([x,p]);  
    Reliable: 2  
      
      
### tools.math.stat.corrsim

Correlation between two vectors and confidence region using bootstrap Package: Util.stat Description: Calculate the correlation between two vectors and use the bootstrap method to estimate the probability to get a correlation larger than the observed correlation.


    
    Correlation between two vectors and confidence region using bootstrap  
    Package: Util.stat  
    Description: Calculate the correlation between two vectors and use the  
    bootstrap method to estimate the probability to get  
    a correlation larger than the observed correlation.  
    The function ignores NaN values.  
    Input  : - X vector.  
    - Y vector.  
    - Number of bootstrap simulations. Default is 1000.  
    - Which vector to permute in bootstrap test {'x'|'y'}.  
    Default is 'y'.  
    * Arbitrary number of input argument to pass to the  
    built in function corr.m  
    'type' - 'Pearson' (default) to compute Pearson's linear  
    correlation coefficient.  
    'Kendall' to compute Kendall's tau.  
    'Spearman' to compute Spearman's rho.  
    Output : - The Correlation.  
    - Probability to get larger correlation.  
    - Vector of simulated correlations.  
    - Number of entries used to calculate correlation.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Mar 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Corr,Prob,SimCorr,N]=corrsim(rand(100,1),rand(100,1));  
    Reliable: 1  
      
      
      
### tools.math.stat.corrsim_cov

Correlation matrix between N columns and bootstrap estimation of errors. Package: Util.stat Description: Given a matrix with N columns, calculate the correlation between each pair of columns and use the bootstrap method to estimate the probability to get


    
    Correlation matrix between N columns and bootstrap estimation of errors.  
    Package: Util.stat  
    Description: Given a matrix with N columns, calculate the correlation  
    between each pair of columns and use the  
    bootstrap method to estimate the probability to get  
    a correlation larger than the observed correlation.  
    The function ignores NaN values.  
    Input  : - Matrix with N columns.  
    - Number of bootstrap simulations. Default is 1000.  
    - Which vector to permute in bootstrap test {'x'|'y'}.  
    Default is 'y'.  
    * Arbitrary number of input argument to pass to the  
    built in function corr.m  
    'type' - 'Pearson' (the default) to compute Pearson's linear  
    correlation coefficient.  
    'Kendall' to compute Kendall's tau.  
    'Spearman' to compute Spearman's rho.  
    Output : - Matrix of all correlations.  
    - Matrix of all probabilities to get larger correlation.  
    - Matrix of number of meauseremnets used in each correlation.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Mar 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Corr,Prob]=corrsim_cov(rand(100,4));  
    Reliable: 1  
      
      
### tools.math.stat.err_cl

Numerical estimate of percentiles. Package: Util.stat Description: Given a vector of data points, calculate the lower and upper bounds of an interval that contains a given precentage (P) of the data. (1-P)/2 of the data points are


    
    Numerical estimate of percentiles.  
    Package: Util.stat  
    Description: Given a vector of data points, calculate the lower and  
    upper bounds of an interval that contains a given  
    precentage (P) of the data. (1-P)/2 of the data points are  
    below and above the lower and upper bounds, respectively.  
    Remove NaNs prior to calculation.  
    Input  : - Data vector.  
    - Lower and upper bounds of an interval that contains a given  
    precentage (P) of the data.  
    default is [0.6827; 0.9545; 0.9973]  
    error is calculate to each one of the levels.  
    Output : - Matrix of confidence interval (CI), the first column for  
    left CI and the second column to right CI.  
    Each line for each confidence level.  
    - Estimate the standard errors in the confidence intervals.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Nov 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Err,ErrErr]=Util.stat.err_cl(randn(10000,1));  
    Reliable: 1  
      
### tools.math.stat.error2ensemble

error2ensemble function                                        AstroStat Description: Generate a realization of data points given the data probability distribution function.


    
      
    error2ensemble function                                        AstroStat  
    Description: Generate a realization of data points given the data  
    probability distribution function.  
    Input  : - Column vector of the expectency value for each data point.  
    - One or two column matrix containing the lower (first column)  
    and upper (second column) error for each data point  
    realization. If single column is given then the lower and  
    upper errors are equal.  
    If empty matrix (i.e., []), then the distribution function  
    is given numerically.  
    - Function to use in order to generate the realizations.  
    This can be a mumerical function [X, P], where X=0  
    corresponds to the expectency value of the distribution.  
    Alternatively this can be a string containing a pre-defined  
    function: {'normal'|'lognormal'|'poisson'|'flat'},  
    default is 'normal'.  
    'normal'  use the lower and upper errors as the lower and  
    upper 1\sigma errors.  
    'flat'    use the errors as the bounderies of the flat  
    distribution.  
    'poisson' use the first column in errors as lambda  
    (the second column is ignored).  
    - Two columns matrix with the [minimum, maximum] bounds  
    of the minumum and maximum allowed in each realization.  
    Default is [-Inf Inf].  
    If a data point with value below or above these bounds  
    is found, then it is being replaced by the bound value.  
    Output : - Column vector of data realizations.  
    - Number of boundary replacments.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    May 1998  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [NewData,Nbounds]=error2ensemble(ones(1000,1),[1 5]);  
    Reliable: 2  
      
### tools.math.stat.fab_counts

- fab_counts function                                           AstroStat Description: Calculate the False Alarm Probability (FAP) that a source is real rather than a background fluctutation, given the source counts and the background counts.


    
    -  
    fab_counts function                                           AstroStat  
    Description: Calculate the False Alarm Probability (FAP) that a source  
    is real rather than a background fluctutation,  
    given the source counts and the background counts.  
    Input  : - Source counts.  
    - Expectency of background counts within the source  
    extraction aperture.  
    Alternatively, if a third argument is given this is the  
    total measured background counts within a given area.  
    In this case the third parameter give the ratio between  
    the background area and the source extraction area.  
    - The ratio between the background area and the source  
    extraction area.  
    Tested : Matlab 7.13  
    By : Eran O. Ofek                    Oct 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    -  
      
      
### tools.math.stat.hist2d

calculate the 2-D histogram of 2-D data set. Package: Util.stat Description: calculate the 2-D histogram of 2-D data set.


    
    calculate the 2-D histogram of 2-D data set.  
    Package: Util.stat  
    Description: calculate the 2-D histogram of 2-D data set.  
    Input  : - Vector of X coordinates.  
    - Vector of Y coordinates.  
    - Range of X histogram [min max].  
    - Step size of X histogram.  
    - Range of Y histogram [min max].  
    - Step size of Y histogram.  
    Output : - 2-D histogram  
    - Vector of X coordinate of center of X bins.  
    - Vector of Y coordinate of center of Y bins.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Feb 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Xv=rand(100000,1).*2; Yv=rand(100000,1).*3+100; Mat=Util.stat.hist2d(Xv,Yv,[0 2],0.1,[100 103],0.1);  
    Reliable: 2  
      
      
    reject points out of range  
    I    = find(Xv>=RangeX(1) & Xv<=RangeX(2) & Yv>=RangeY(1) & Yv<=RangeY(2));  
    Xv   = Xv(I);  
    Yv   = Yv(I);  
      
    NXv  = (Xv - RangeX(1))./StepX;  
    NYv  = (Yv - RangeY(1))./StepY;  
    VecX = (RangeX(1):StepX:(RangeX(2)-StepX)).' + StepX.*0.5;  
    VecY = (RangeY(1):StepY:(RangeY(2)-StepY)).' + StepY.*0.5;  
      
    XY   = NYv + floor(NXv).*numel(VecY);  
    N    = histc(XY,(0:1:numel(VecX).*numel(VecY)).');  
    N    = N(1:end-1);  
      
    Mat  = reshape(N,length(VecY),length(VecX));  
      
    using histcounts2 spped it up  
      
### tools.math.stat.iqrFast

A fast iqr (inter quantile range) function (without interpolation)


    
    A fast iqr (inter quantile range) function (without interpolation)  
    Input  : - An array.  
    - Fraction, or a vector of fractions.  
    - Dimension along to calculate the quantile (1 | 2 | [1 2]).  
    Default is 1.  
    Output : - IQR.  
    Author : Eran Ofek (Jul 2021)  
    Spped  : ~3 times faster than iqr.  
    Example: R=rand(10000,1);  
    Quant = tools.math.stat.iqrFast(R)  
    tic; for I=1:1:1e3, Quant = tools.math.stat.iqrFast(R); end, toc  
    tic; for I=1:1:1e3, Quant = iqr(R); end, toc  
      
### tools.math.stat.jackknife

Given an estimator, calculate the Jacknife StD. Package: Util.stat Description: Given an estimator (given by a function), calculate the Jackknife StD and the first order Quenouille-Tukey Jacknife bias for this estimator.


    
    Given an estimator, calculate the Jacknife StD.  
    Package: Util.stat  
    Description: Given an estimator (given by a function), calculate the  
    Jackknife StD and the first order Quenouille-Tukey  
    Jacknife bias for this estimator.  
    Notes: - the estimator should be continues.  
    - if Bias/StD~<0.25 then the bias is probably not  
    an issue.  
    - The bias estimate is not reliable if the estimator  
    is an unsmooth statistic (e.g., median).  
    Input  : - vector of data set.  
    - Estimator function name,  
    Fun(Data,Additional_Parameters)  
    - Additional parameters of 'Fun'.  
    Output : - Jackknife StD.  
    - Vector of jackknife estimator.  
    That could be used for plotting the estimator distribution,  
    or to find percentile confidence interval using err_cl.m  
    - Jackknife Bias.  
    - bc_a bias correction (Z0).  
    - bc_a acceleration (Ac).  
    The bc_a bias coorection and acceleration can be used  
    to estimate the bias corrected and accelerated  
    confiedence interval.  
    For example: if you want the CI for the 0.9545 CL.  
    use the function [A1,A2]=bc_a(Z0,Ac,0.9545),  
    to estimate "corrected" percentile,  
    and then CI=err_cl(Theta,0.9545);  
    Reference : Efron B., 1982, in: The Jackknife, the bootstrap and other  
    resampling plans.  
    Efron B. & Tibshirani, R.J., 1993,  
    in: An Introduction to the Bootstrap  
    see also: bootstrap_std.m, bc_a.m, err_cl.m  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Sep 2002  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [StD,Bias]=Util.stat.jackknife(Data,'mean',1);  
    then UnBiased_Theta = Theta - Bias  
    Reliable: 1  
      
      
      
### tools.math.stat.lognlogs

- lognlogs function                                               AstroStat Description: Calculate Log N - Log S plot for set of flux measurments.


    
    -  
    lognlogs function                                               AstroStat  
    Description: Calculate Log N - Log S plot for set of flux measurments.  
    Input  : - List of flux measurments.  
    - List of Edges in which to calculate N(>S).  
    Output : - Cumulative number of fluexs above S [e.g., N(>S)].  
    - Vector of fluxes.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                   January 2008  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: D=rand(100000,3); D=sqrt(sum(D.^2,2)); I=find(D<1);  
    D=D(I); L=D.^-2;  
    [N,S] = lognlogs(L,logspace(1,4,30)');  
    loglog(S,N);  
    Par=fitpow(S,N,sqrt(N));  
    -  
      
### tools.math.stat.max_likelihood

Likelihood from observations and numerical probability distribution. Package: Util.stat Description: Given a numerical probability distribution and list of 'events', calculate the 'likelihood' for the events given the probability distribution. In addition, calculate the


    
    Likelihood from observations and numerical probability distribution.  
    Package: Util.stat  
    Description: Given a numerical probability distribution and list of  
    'events', calculate the 'likelihood' for the events given  
    the probability distribution. In addition, calculate the  
    ML ratio test and preform Monte-Carlo simulations by  
    generating realizations of events given the probability  
    distribution and calculate the likelihood-probability  
    distribution.  
    Input  : - Two column matrix of parent numerical distribution.  
    [X, P], where P is the probabiliy density.  
    The program will normalize P if necessary.  
    If one column matrix is given, then the numerical probability  
    distribution is calculated by an histogram.  
    - List of event [X].  
    - Number of Degree's of Freedom. Default is 1  
    (Free parameters of the model).  
    - Number of Monte-Carlo simulations. Default is 1000.  
    - Column vector of Probabilities for the ML ratio-test.  
    Return the MLRT for each probability.  
    Default is [0.6827; 0.9545; 0.9973].  
    Usefull when ML is calculated as function of a free parameters.  
    - Optional value: If (ParNumDist) is single-column, then  
    generate ParNumDist so that each bin will have (Nfactor) points.  
    Default is 100.  
    - Optional offset to add to the objects and to the  
    Monte-Carlo objects.  
    In each MC simulation, add an offset to the simulated events  
    before calculating the ML.  
    Default is 0 - no offset.  
    - Interpolation method in calculating the probability from  
    the numerical function. Default is 'linear'.  
    Output : - log Likelihood.  
    - Chi2 per DoF.  
    - ML Ratio-test.  
    - Vector of simulated MLs.  
    - Given the ML and Simulated ML, return the probability to  
    get the Events ML from the Parent Simulated ML distribution:  
    P(SimML>ML).  
    Tested : Matlab 5.3  
    By : Eran O. Ofek          September 2001  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example:  calculate likelihood for events from a gaussian distribution  
    x=[-10:0.01:10]';  
    y=exp(-x.^2./(2))./sqrt(2.*pi);  
    [ML,Chi2,MLRT,SimML,P_SimML]=max_likelihood([x,y],randn(100,1));  
    Reliable: 2  
      
### tools.math.stat.maxnd

maxnd function                                                     AstroStat Description: Return the global maximum of a N-D matrix and its indices. This is equivalent to max(Data(:)), but it also returns the indices of the global maximum.


    
      
    maxnd function                                                     AstroStat  
    Description: Return the global maximum of a N-D matrix and its indices.  
    This is equivalent to max(Data(:)), but it also returns the  
    indices of the global maximum.  
    Input  : - N-D matrix  
    Output : - Value of global Maximum  
    - Vector that contains the indices of the global maximum  
    in the N-D matrix.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                      July 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See also: meannd.m, minnd.m, maxnd.m, mediannd.m  
    Example: [M,I]=maxnd(rand(4,4,4,4));  
    Reliable: 1  
      
      
### tools.math.stat.mean_error

Calculate the error on the mean using std/sqrt(N). Package: Util.stat Description: Calculate the error on the mean using std/sqrt(N).


    
    Calculate the error on the mean using std/sqrt(N).  
    Package: Util.stat  
    Description: Calculate the error on the mean using std/sqrt(N).  
    Input  : - An array.  
    - Diemsnion along to calculate the error on the mean.  
    Default is 1.  
    Output : - Error on the mean.  
    - StD.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Jan 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [ErrorMean,StD]=mean_error(rand(5,3,3),1)  
    Reliable: 2  
      
### tools.math.stat.meannd

meannd function                                                    AstroStat Description: Return the global mean of a N-D matrix.


    
      
    meannd function                                                    AstroStat  
    Description: Return the global mean of a N-D matrix.  
    Input  : - N-D matrix  
    Output : - Value of global Mean.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                      July 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See also: meannd.m, minnd.m, maxnd.m, mediannd.m  
    Example: M=meannd(rand(10,10,10));  
    Reliable: 1  
      
      
    SizeData = size(Data);  
    Vec = reshape(Data, [prod(SizeData), 1]);  
    [Mean] = mean(Vec);  
      
### tools.math.stat.mediannd

mediannd function                                                  AstroStat Description: Return the global median of a N-D matrix.


    
      
    mediannd function                                                  AstroStat  
    Description: Return the global median of a N-D matrix.  
    Input  : - N-D matrix  
    Output : - Value of global Median.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                      July 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See also: meannd.m, minnd.m, maxnd.m, mediannd.m  
    Example: M=mediannd(rand(10,10,10));  
    Reliable: 1  
      
      
    SizeData = size(Data);  
    Vec = reshape(Data, [prod(SizeData), 1]);  
    [Median] = median(Vec);  
      
### tools.math.stat.minnd

minnd function                                                     AstroStat Description: Return the global minimum of a N-D matrix and its indices. This is equivalent to min(Data(:)), but it also returns the indices of the global minimum.


    
      
    minnd function                                                     AstroStat  
    Description: Return the global minimum of a N-D matrix and its indices.  
    This is equivalent to min(Data(:)), but it also returns the  
    indices of the global minimum.  
    Input  : - N-D matrix  
    Output : - Value of global Minimum  
    - Vector that contains the indices of the global minimum  
    in the N-D matrix.  
    Tested : Matlab 6.5  
    By : Eran O. Ofek                       Feb 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: [M,I]=minnd(rand(4,4,4,4))  
    Reliable: 1  
      
      
### tools.math.stat.mode_bin

mode_bin function                                              AstroStat Description:


    
      
    mode_bin function                                              AstroStat  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    - Additional parameters  
    Any additional key,val, that are recognized by one of the  
    following programs:  
    Output : -  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jun 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### tools.math.stat.mode_density

Calculate the mode by estimating the density of points Package: Util.stat Description: Estimate the mode by constructing equal number of points histogram and choosing the bin with smallest range.


    
    Calculate the mode by estimating the density of points  
    Package: Util.stat  
    Description: Estimate the mode by constructing equal number of points  
    histogram and choosing the bin with smallest range.  
    Input  : - Array.  
    - Dimesnion over which to calculate the mode.  
    1,2,'all'.  
    Default is 'all'.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'NinBin' - Number of points in bin. Default is 30.  
    'JoinOut' - Join output parameters into one variable.  
    Default is false.  
    Output : - Mode estimator.  
    - Estimator for the Std of a Gaussian peak.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mode]=Util.stat.mode_density(rand(10000,1),1)  
    Reliable:  
      
      
      
### tools.math.stat.mode_fit

Estimate the mode of an array by fitting a Gaussian to its histogram. Package: +tools.math.stat Description: Estimate the mode of an array by fitting a Gaussian to the histogram of the array around its median. Return also the Sigma of the Gaussian fit.


    
    Estimate the mode of an array by fitting a Gaussian to its histogram.  
    Package: +tools.math.stat  
    Description: Estimate the mode of an array by fitting a Gaussian to  
    the histogram of the array around its median.  
    Return also the Sigma of the Gaussian fit.  
    If best fit is out of range then set the mode to the maximum  
    value of the histogram and std to sqrt of the max value.  
    Input  : - Array for which to calculate the mode and StD.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'ElementPerBin' - Typical number of points per histogram bin.  
    'TrimEdge2D' - If the input is a 2D array, trim the edges  
    by this number of pixels. Default is 5.  
    'Percent'    - The percentile of the data for which to fit  
    the mode [lower upper]. This means that  
    only values between the lower and upper  
    percentiles will be used.  
    Default is [0.025 0.9].  
    'Nbad'       - If histogram difference contains jumps  
    which are larger than this value then these  
    jumps will be removed from the fit.  
    INACTIVE.  
    'BinSize'    - Bin size. If empty then will attempt to  
    calculate bin size. Default is empty.  
    'JoinOut'    - Joining the Mode and Std into the first output  
    argument. Default is false.  
    Output : - Mode.  
    - Fitted sigma.  
    License: GNU general public license version 3  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Apr 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mode,StD]=tools.math.stat.mode_fit(randn(1000,1000))  
    Reliable: 2  
      
### tools.math.stat.mode_vec

mode_vec function                                              AstroStat Description: Calculate the mode of a vector using histogram. The histogram step size chosen to contain some mean number of points per bin.


    
      
    mode_vec function                                              AstroStat  
    Description: Calculate the mode of a vector using histogram.  
    The histogram step size chosen to contain some mean number  
    of points per bin.  
    The function uses only data within the 25 and 75  
    percentile.  
    Input  : - Vector, or an array that will be treated as a vector.  
    - Optional mean number of points per bin. Default is 200.  
    Output : - Mode.  
    - The weighted mode. This is calculated by weighting the  
    histogram within the 25 and 75 percentile by the number of  
    counts in each bin.  
    - Step size of the histogram. This can be regarded as an  
    approximate resolutin of the mode.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jul 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Mode,WMode]=mode_vec(randn(100000,1))  
    Reliable: 2  
      
      
### tools.math.stat.moment_2d

moment_2d function                                                 AstroStat Description: Calculate first and second moments of a 2D matrix.


    
      
    moment_2d function                                                 AstroStat  
    Description: Calculate first and second moments of a 2D matrix.  
    Input  : - Matrix  
    - X coordinate of matrix j-axis. Default is [1:1:size(Mat,2)].'  
    - Y coordinate of matrix i-axis. Default is [1:1:size(Mat,1)].'  
    Output : - First moment [E(x),E(y)].  
    - Second Moment [E(x^2) E(x*y); E(y*x) E(y^2)].  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jun 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [M1,M2]=moment_2d(rand(10,10));  
    Reliable: 2  
      
      
### tools.math.stat.mutual_info

Calculate the mutual information of two vectors (degree of independency) Package: Util.stat Description: Calculate the mutual information of two vectors.


    
    Calculate the mutual information of two vectors (degree of independency)  
    Package: Util.stat  
    Description: Calculate the mutual information of two vectors.  
    Input  : - A vector (X).  
    - A vector (Y).  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'XLim' -  
    'YLim' -  
    'NbinX'-  
    'NbinY'-  
    Output : - Mutual information  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: MI=Util.stat.mutual_info(randn(1000,1),rand(1000,1))  
    Reliable:  
      
      
      
      
### tools.math.stat.nanmedian

faster version of nanmedian using the 'omitnan' option. see median for options: Example: tools.math.stat.nanmedian(rand(100,100),2)


    
    faster version of nanmedian using the 'omitnan' option.  
    see median for options:  
    Example: tools.math.stat.nanmedian(rand(100,100),2)  
      
### tools.math.stat.nanrstd

Robust nanstd. Package: Util.stat Description: Robust nanstd. Estimating the std (like nanstd.m), based on the 50-percentile of the distribution.


    
    Robust nanstd.  
    Package: Util.stat  
    Description: Robust nanstd. Estimating the std (like nanstd.m), based  
    on the 50-percentile of the distribution.  
    Input  : - Data.  
    - If 0, normalize by N-1. If 1, normalize by N. Default is 0.  
    - Dimension along to calculate StD. Default is 1.  
    Outout : - Robust Std (output and behavior is like nanstd.m).  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Mar 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: StD=nanrstd(randn(5,5))  
    Reliable: 2  
      
      
### tools.math.stat.nanstd

faster version of nanstd using the 'omitnan' option. see median for options: Example: tools.math.stat.nanstd(rand(100,100),[],2)


    
    faster version of nanstd using the 'omitnan' option.  
    see median for options:  
    Example: tools.math.stat.nanstd(rand(100,100),[],2)  
      
### tools.math.stat.noiser

noiser function                                                AstroStat Description: Add multiple noise components to a vector.


    
      
    noiser function                                                AstroStat  
    Description: Add multiple noise components to a vector.  
    Input  : - Vector of values to which to add noise.  
    * Arbitrary number of ...,key,val, input arguments.  
    The following keywords are available:  
    'op'   - Add or multiply the noise {'+'|'*'}. Default is '+'.  
    This can be either a char or a cell array. If cell  
    array then each element refer to a specific noise  
    element.  
    'noise'- Type of random numbers noise: {'norm','poiss'}.  
    Default is 'norm'.  
    This can be either a char or a cell array. If cell  
    array then each element refer to a specific noise  
    element.  
    'sigma'- Vector or scalar of sigma of normal distribution.  
    Default is 1.  
    'sigmaL'- If this parameter is specified then use different  
    noise for the upper and lower sigma of the normal  
    distribution. While 'sigma' corresponds to the upper  
    sigma, 'sigmaL' corresponds to the lower sigma.  
    Default is empty.  
    'lambda'-Scalar or vectot of Lambda (expectency) of the  
    Poisson distribution. Default is to use the first  
    input argument (i.e., Y).  
      
    Output : - Noisy vector  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Jan 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Y=noiser(rand(1000,1).*100,'op',{'+','+'},'noise',{'poiss','norm'},'sigma',6);  
    Reliable: 2  
      
      
### tools.math.stat.poissconf

Upper/lower confidence intervals on N events assuming Poisson statistics Package: Util.stat Description: Given the number of observed events, calculates the two sided upper and lower confidence intervals, assuming Poisson statistics. Below N=140 use the Gehrels (1986)


    
    Upper/lower confidence intervals on N events assuming Poisson statistics  
    Package: Util.stat  
    Description: Given the number of observed events, calculates the two  
    sided upper and lower confidence intervals, assuming  
    Poisson statistics. Below N=140 use the Gehrels (1986)  
    algorithm. Above this, use sqrt(N) approximation.  
    Input  : - Coloumn vector of numbers.  
    - Two sided confidence limit, default is the 1\sigma 0.6827 CL.  
    Can get also '1' - for 1\sigma 0.6827 CL.  
    '2' - for 2\sigma 0.9545 CL.  
    '3' - for 3\sigma 0.9973 CL.  
    Output : - Two column matrix of lower and upper confidence intervals.  
    Left column for lower CI, right column for right CI.  
    - Tow column matrix of the lower and upper errors.  
    Reference: Gehrels, N. 1986, ApJ 303, 336  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Dec 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [CI,CE]=poissconf([0;1;2;3;4]);  
    Reliable: 2  
      
      
      
      
      
### tools.math.stat.prob2find_inr

prob2find_inr function                                             AstroStat Description: Given a density (number per unit area), and a distance from a point, calculate the the probability to find an object within radius R from the point (assuming Poisson


    
      
    prob2find_inr function                                             AstroStat  
    Description: Given a density (number per unit area), and a distance  
    from a point, calculate the the probability to find an  
    object within radius R from the point (assuming Poisson  
    statistics).  
    Input  : - Object density [Nuber/Area]  
    - Radius [Dist]  
    Output : - Probability to find an object within radius R.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Feb 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
### tools.math.stat.psigma

psigma function                                                AstroStat Description: Return the two sided or one sided probability for a given sigma level.


    
      
    psigma function                                                AstroStat  
    Description: Return the two sided or one sided probability for a given  
    sigma level.  
    Input  : - Sigma level.  
    - Number of sides {1|2}, default is 2.  
    For example, 1 sigma for 2 sides is 0.6827  
    and for 1 side is 0.8413.  
    Output : - Probability.  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Mar 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: P=psigma(1,2);  
    Reliable: 1  
      
### tools.math.stat.quantileFast

A fast quantile function (without interpolation)


    
    A fast quantile function (without interpolation)  
    Input  : - An array.  
    - Fraction, or a vector of fractions.  
    - Dimension along to calculate the quantile (1 | 2 | [1 2]).  
    Default is 1.  
    Output : - Quantile.  
    Author : Eran Ofek (Jul 2021)  
    Spped  : ~3 times faster than quantile.  
    Example: R=rand(10000,1);  
    Quant = tools.math.stat.quantileFast(R, 0.1)  
    Quant = tools.math.stat.quantileFast(R, [0.1,0.2])  
    tic; for I=1:1:1e3, Quant = tools.math.stat.quantileFast(R, 0.1); end, toc  
    tic; for I=1:1:1e3, Quant = quantile(R, 0.1); end, toc  
      
### tools.math.stat.rand_circle

rand_circle function                                               AstroStat Description: Generate random number equally distributed inside a unit circle.


    
      
    rand_circle function                                               AstroStat  
    Description: Generate random number equally distributed inside a unit  
    circle.  
    Input  : * Number of random numbers to generate:  
    e.g. (5,1)  
    Output : - X.  
    - Y.  
    - R (radius).  
    Tested : MATLAB 5.1  
    By : Eran O. Ofek                    Feb 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [X,Y,R]=rand_circle(10000,1);  
    plot(X,Y,'.');  
    Reliable: 2  
      
      
### tools.math.stat.rand_ps

Generate a random time series with a given power spectrum. Package: Util.stat Description: Generate a random realization of a time series with a given power spectrum (e.g., power-law) and optional gaussian measuments errors.


    
    Generate a random time series with a given power spectrum.  
    Package: Util.stat  
    Description: Generate a random realization of a time series with a given  
    power spectrum (e.g., power-law) and optional gaussian  
    measuments errors.  
    Input  : - Column vector of times in which to generate the time series.  
    - Power spectra.  
    * If two element vector is given, then the first is taken as  
    the power-law index of a power-law spectra (i.e., P=(1/f)^-Beta),  
    and the second is the power-spectrum normalization (amplitude^2)  
    at the minimum frequency (total time-span).  
    If the power-spectrum normalization is negative then  
    the standard deviation of the generated LC is taken  
    to be the absolute value of this normalization.  
    * In case that two column matrix is given, then the first  
    column is the frequency and the second column is the power  
    (i.e., amplitude^2), (e.g., [Freq, Power]).  
    Note that the power is taken as zero, outside the  
    frequency range given in this matrix.  
    - Optional column vector of errors (normal) to apply  
    to the time series, defualt is zeros.  
    Output : - Time series, [Time, Value, Err].  
    Note that the error is optional.  
    Reference: Timmer, J., Konig, M., 1995, A&A 300, 707.  
    Tested : Matlab 5.3  
    By : Eran Ofek                      Nov 2002  
    URL : http://weizmann.ac.il/home/eran/matlab/  
    Example: TimeSeries=Util.stat.rand_ps((0:1:100)',[2 1],randn(101,1).*0.1);  
    Reliable: 2  
    -  
### tools.math.stat.rand_range

rand_range function                                            AstroStat Description: Generate uniformly random number in a given range. The numbers can be uniform either in linear space or log10 space.


    
      
    rand_range function                                            AstroStat  
    Description: Generate uniformly random number in a given range.  
    The numbers can be uniform either in linear space or  
    log10 space.  
    Input  : - Size of random number matrix [M by N].  
    - range of random values [Min Max].  
    - Uniform numbers in {'lin' | 'log'} space, default is 'lin'.  
    Output : - Matrix of M by N random numbers.  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Oct 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Rand=rand_range([100000 1],[1 5]);  
    Rand=rand_range([100000 1],[10 1000],'log');  
    Reliable: 2  
      
      
### tools.math.stat.randgen

Random numbers generator for arbitrary  distribution. Package: Util.stat Description: Random numbers generator for arbitrary function given by a numerical distribution.


    
    Random numbers generator for arbitrary  distribution.  
    Package: Util.stat  
    Description: Random numbers generator for arbitrary function given by a  
    numerical distribution.  
    Input  : - The distribution. This is a two column vector in which the  
    first column is the independent variable and the second column  
    is the probability density function (PDF) or CDF of the  
    distribution.  
    - Number of elements in the random number vector.  
    - Probability type (of the first input argument):  
    'd' : differential PDF (default).  
    'c' : cumulative CDF.  
    - Interpolation type in the distribution matrix:  
    'linear'  - linear interpolation (default).  
    'nearest' - nearest neighbor interpolation.  
    'spline'  - cubic spline interpolation.  
    'cubic'   - cubic interpolation.  
    - NewSeed parameter: {'y' | 'n'}  
    'y' : take new seed each in each session.  
    'n' : do not take new seed (default).  
    Output : - Vector of random numbers.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jun 2000  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  using randgen instead of randn (and comparison):  
    x=[-10:0.1:10]';  
    y=exp(-x.^2./(2))./sqrt(2.*pi);  
    R=randgen([x,y],100000);  
    [x,n]=realhist(randn(100000,1),[-10 10 2000]);  
    [x1,n1]=realhist(R,[-10 10 2000]);  
    plot(x,n);hold on; plot(x1,n1,'r')  
    Comments: When matlab initialize it start with the same seed number.  
    Therefore, it will generate the same random numbers in each  
    session.  
    One way to avoid this problem is to add the following command  
    to your startup.m file:  
    rand('state',sum(100*clock));  
    Reliable: 1  
      
### tools.math.stat.randinpolygon

Generate random positions inside a polygon Package: Util.stat Description: Generate random positions inside a polygon, defined on a plane or a sphere.


    
    Generate random positions inside a polygon  
    Package: Util.stat  
    Description: Generate random positions inside a polygon, defined on a  
    plane or a sphere.  
    Input  : - Polygon boundries [X, Y] (in case of spherical geometry  
    [long, lat] in radians.  
    Note you must connect also the first and last point!  
    - Number of random points to generate.  
    - Geometry:  
    'plane'    - polygon on a plane (default).  
    'sphere'   - polygin on a sphere.  
    In that case the [X,Y] coordinates  
    must be in radians.  
    Output : - Coordinates [X,Y] of random points inside polygon.  
    Tested : Matlab 6.5  
    By : Eran O. Ofek                    Aug 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### tools.math.stat.rangend

rangend function                                                   AstroStat Description: Return the global Range of a N-D matrix.


    
      
    rangend function                                                   AstroStat  
    Description: Return the global Range of a N-D matrix.  
    Input  : - N-D matrix  
    Output : - Value of global Range.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                      July 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See also: meannd.m, minnd.m, maxnd.m, mediannd.m  
    Reliable: 1  
      
      
### tools.math.stat.realhist

Calculate histogram for a dataset in a given range. Package: Util.stat Description: Calculate histogram for a dataset in a given range.


    
    Calculate histogram for a dataset in a given range.  
    Package: Util.stat  
    Description: Calculate histogram for a dataset in a given range.  
    Input  : - Column vector for which to calculate the histogram.  
    - Histogram range, few optional formats:  
    [from, to, NumberOfBins] : for the histogram.  
    (default is [min, max, 10]).  
    [X1;X2;...;Xn] : hist edges (see: histc_debug).  
    - Options :  
    'n'  : non cumulative (normal).  
    'c+' : cumulative histogram (in increasing direction).  
    'c-' : cumulative histogram (in decreasing direction).  
    - normalization -  
    'a' : as is (default).  
    'p' : probability normailzation. (normaliz sum to 1).  
    's' : spheric area normalization (range should be in radians).  
    (the normalization is | sin(lat1) - sin(lat2) |).  
    and the total sum is normalized to 1.  
    Output : - Vector of histogram bin centeres.  
    - Vector of number of data points in each bin.  
    - Edges vector, [X1;X2;...;Xn].  
    - One sigma Poisson's confidence interval per bin.  
    This is two column vector, for right and  
    left boundry confidence interval.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Sep 1995  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: R=rand(1000,1);  
    [x,n]=realhist(R,[0 1 50]);  
    bar(x,n);  
    Reliable: 2  
      
### tools.math.stat.rmean

Calculate the rubust mean over one of the dimensions. Package: Util.stat Description: Calculate the rubust mean over one of the dimensions.


    
    Calculate the rubust mean over one of the dimensions.  
    Package: Util.stat  
    Description: Calculate the rubust mean over one of the dimensions.  
    Input  : - A matrix.  
    - Dimension along to calculate the ribust mean.  
    If empty calc over all dimensions.  
    Deafult is 'all'.  
    - [Low, High] fraction of the values to remove prior to the  
    mean calculation. Default is [0.05 0.05].  
    I.e., remove the loewer and upper 5 percentile and  
    calculate the mean.  
    Output : - The mean.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: A=rand(10000,4); A(1,:)=1e7; rmaen(A)  
    Reliable: 2  
      
      
### tools.math.stat.rstd

Robust std calculated from the 50 inner percentile of the data. Package: Util.stat Description: Robust std calculated from the 50 inner percentile of the data.


    
    Robust std calculated from the 50 inner percentile of the data.  
    Package: Util.stat  
    Description: Robust std calculated from the 50 inner percentile  
    of the data.  
    Input  : - Matrix.  
    - Dimension along to calculate the std. Default is 1.  
    Output : - Robust std.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Mar 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Rstd=rstd(randn(1000,3))  
    Reliable: 2  
      
      
### tools.math.stat.sphere_conv

sphere_conv function                                           AstroStat Description: Convolve a 2D matrix which point coordinates are on the celestial sphere, with a kernel.


    
      
    sphere_conv function                                           AstroStat  
    Description: Convolve a 2D matrix which point coordinates are on the  
    celestial sphere, with a kernel.  
    Input  : - Vector of longitudes [rad].  
    - Vector of latitudes [rad].  
    - Matrix [Long,Lat].  
    - Matrix of weights (or area per cell).  
    - Kernel [radius(rad), amplitude].  
    Output : - Convolved matrix.  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Nov 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: ConvMat=sphere_conv(Long,Lat,Mat,Weight,Kernel)  
    Reliable: bug (not normalized by area)  
      
### tools.math.stat.stat_in_htm

SHORT DESCRIPTION HERE Package: Util Description:


    
    SHORT DESCRIPTION HERE  
    Package: Util  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Util.stat.stat_in_htm(Table(:,[1 2 3]));  
    Reliable:  
      
      
### tools.math.stat.std_w

std_w function                                                 AstroStat Description: Weighted standard deviation.


    
      
    std_w function                                                 AstroStat  
    Description: Weighted standard deviation.  
    Input  : - Matrix of values for which to calculate the weighted std.  
    - Matrix of sigmas (Weight= 1./Sig^2).  
    - Dimension along to calculate the weighted std. Default is 1.  
    Output : - Weighted std.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Jan 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
### tools.math.stat.stdnd

Return the global StD of a N-D matrix. Package: Util.stat Description: Return the global StD of a N-D matrix.


    
    Return the global StD of a N-D matrix.  
    Package: Util.stat  
    Description: Return the global StD of a N-D matrix.  
    Input  : - N-D matrix.  
    - Flag: 0 to normalize by N-1; 1 to normalized by N,  
    default is 0.  
    Output : - Value of global StD.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                      July 2005  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See also: meannd.m, minnd.m, maxnd.m, mediannd.m  
    Example: S=stdnd(rand(10,10,10),1)  
    Reliable: 1  
      
### tools.math.stat.sumnd

sumnd function                                                     AstroStat Description: Return the global sum of a N-D matrix.


    
      
    sumnd function                                                     AstroStat  
    Description: Return the global sum of a N-D matrix.  
    Input  : - N-D matrix  
    Output : - Value of global sum.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                     March 2007  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    See also: meannd.m, minnd.m, maxnd.m, mediannd.m  
    Example: Sum=sumnd(rand(4,4,4,4));  
    Reliable: 1  
      
      
### tools.math.stat.wmean

wmean function                                                 AstroStat Description: Calculated the weighted mean of a sample.


    
      
    wmean function                                                 AstroStat  
    Description: Calculated the weighted mean of a sample.  
    Input  : - Either a two column matrix [Val, Err] or a matrix of values,  
    while the errors are in the second argument.  
    - Optional mtarix of errors. If given, then the first input  
    argument is treated as values.  
    - If the first two input arguments are provided than this is the  
    dimension along to calculate the weighted mean.  
    Default is 1.  
    - Ignore nans. Default is true.  
    Output : - Weighted mean.  
    - Weighted error on weighted mean.  
    - Weighted standard deviation.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Jun 1998  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [M,E]=Util.stat.wmean([1;2;3;4],[1;1;1;50]);  
    Reliable: 2  
      
### tools.math.stat.wmedian

Weighted median for a vector. Package: Util.stat Description: Weighted median for a vector. Calculates the weighted median of a vector given the error on each value in the vector.


    
    Weighted median for a vector.  
    Package: Util.stat  
    Description: Weighted median for a vector.  
    Calculates the weighted median of a vector  
    given the error on each value in the vector.  
    Input  : * Either two arguments Data,Error or a two column matrix  
    of [Data,Error].  
    Output : - Weighted median  
    Reference: https://en.wikipedia.org/wiki/Weighted_median  
    License: GNU general public license version 3  
    Tested : Matlab R2015a  
    By : Eran O. Ofek                    Jun 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: D=wmedian(V,E);  
    Reliable: 2  
      
      
### tools.math.stat.wmedian_im

wmedian_im function                                              General Description: Weighted median on a set of images in a cube, where the image index is in the 1st dimension.


    
      
    wmedian_im function                                              General  
    Description: Weighted median on a set of images in a cube, where the  
    image index is in the 1st dimension.  
    Input  : - Cube of images in which the image index is the 1st dimension.  
    - Vector of error, per image.  
    - Percentile. Defaiult is 0.5 (i.e., median).  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    - Additional parameters  
    Any additional key,val, that are recognized by one of the  
    following programs:  
    Output : - Weighted median image. Note that sometimes the weighted  
    median is undefined.  
    License: GNU general public license version 3  
    Tested : Matlab R2015a  
    By : Eran O. Ofek                    Aug 2015  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [D]=wmedian_im(rand(5,10,10),rand(5,1))  
    Reliable:  
      
      
