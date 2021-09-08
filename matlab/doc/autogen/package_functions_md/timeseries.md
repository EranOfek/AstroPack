# Package: timeseries


### timeseries.arp

Fit autoregression model for evently spaced time series Package: timeseries Description: Calculate the autoregessive process of order p. Moddeling an evenly spaced time series, z(t), with:


    
    Fit autoregression model for evently spaced time series  
    Package: timeseries  
    Description: Calculate the autoregessive process of order p.  
    Moddeling an evenly spaced time series, z(t), with:  
    z(t) = sum_i[a(i)*z(t-i)] + err(t)  
    Input  : - Matrix in which the first column is index (time)  
    and the second column is the observation value.  
    - The autoregressive order (lag to run on). default is N/4.  
    - The number of points beyond the last point of the series  
    to extraplate the series. Default is zero.  
    Output : - AR(p) parameters.  
    - AR(p) error in the parameters.  
    - Series extrapolated into the future.  
    The first column is index and the second is for the  
    predicted value of the series.  
    Reference : Koen, C. & Lombard, F. 1993 MNRAS 263, 287-308  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Sep 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [A,Cer]=timeseries.arp([(1:1:100)',rand(100,1)],10);  
    Reliable: 2  
      
      
### timeseries.bin_by_eye

Bin data by define bins interactively Package: timeseries Description: Bin light curve by eye. The function plots the light curve and let the user to define bins using the mouse.


    
    Bin data by define bins interactively  
    Package: timeseries  
    Description: Bin light curve by eye. The function plots the light  
    curve and let the user to define bins using the mouse.  
    The user mark (with the mouse) the beginning and end  
    points of each bin. The left and right limits of each  
    user defined bin are marked on the plot using cyan and  
    red dashed lines, respectively.  
    Input  : - Data matrix, [Time, Value, Error].  
    Output : - Matrix with the following columns:  
    [Bin_Center,  
    <Y>,  
    StD(Y)/sqrt(N),  
    <X>,  
    Weighted-<Y>,  
    Formal-Error<Y>,  
    N]  
    In case that no errors are given, columns 5 and 6  
    will be zeros.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2003  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: binning.m, runmean.m  
    Reliable: 1  
      
### timeseries.binning

Binning function. Calculate various functions in data bins. Package: timeseries Description: Binning a timeseries. In each bin, calculate various functions (e.g., mean) or


    
    Binning function. Calculate various functions in data bins.  
    Package: timeseries  
    Description: Binning a timeseries.  
    In each bin, calculate various functions (e.g., mean) or  
    any user supplied functions.  
    Note that this function was changed on June 2016, and it  
    is not backward compatible. For the old binning.m function  
    use binning_old.m.  
    Note that NaN values are removed prior to binning.  
    Input  : - Matrix, in which the first column is the depandent variable  
    by which to bin (e.g., time) and the second column is for the  
    observations. An optional third column may contains the errors  
    in the observations (default is equal weight errors of 1).  
    - Bin size, in units of the "time" column.  
    Alternaively this can be a vector of edges (override the  
    StartEnd input argument).  
    Default is to divide the "time" range into 10 bins.  
    - A two element vector of [Start End] "time".  
    A NaN value for either Start or End will be replaced with the  
    min(time) and max(time), respectively.  
    Default is [NaN NaN].  
    - Cell array of column names to calculate. Possible column  
    names are:  
    'MidBin'  
    'StartBin'  
    'EndBin'  
    'MeanBin'  
    'MedianBin'  
    'StdBin'  
    Or any function that returns a scalar to apply to the  
    observations (e.g., @numel, @mean, @median).  
    There are several special functions that will be applied also  
    for the errors column. These are @wmean, @wmedian.  
    Default is {'MidBin', @numel, @mean, @median, @std}.  
    - Output type: 'mat'|'astcat'. Default is 'mat'.  
    Output : - Matrix or an AstCat object with with the requested columns.  
    - Output column names.  
    See also: bin_by_eye.m, runmean.m, bin_en.m  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Feb 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: B=timeseries.binning(rand(1000,2),0.1,[0 1]);  
    B=timeseries.binning(rand(1000,2),0.1,[0 1],{'MeanBin',@rstd,@numel});  
    Reliable: 2  
      
      
### timeseries.binning_adaptive

Construct list of edges for binning, given some bin size criteria Package: timeseries Description:


    
    Construct list of edges for binning, given some bin size criteria  
    Package: timeseries  
    Description:  
    Input  : - List of times for which to construct edges for binning.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: AllEdges=timeseries.binning_adaptive  
    Reliable:  
      
      
      
    T=[1 1.1   2 2.2 2.3 2.6 2.8 3 3.4 3.8 4 4.4  6 6.1]  
      
      
### timeseries.binning_npt

Binning a time series by equal number of sucssesive points Package: timeseries Description: Binning a time series by equal number of sucssesive points


    
    Binning a time series by equal number of sucssesive points  
    Package: timeseries  
    Description: Binning a time series by equal number of sucssesive points  
    Input  : - A two column [Time property] series.  
    - Number of points in each bin. Default is 10.  
    - Cell array of columns to calculate. Options:  
    'meanbin'   - mean time of bin.  
    'medianbin' - median time of bin.  
    'stdbin'    - std time of bin.  
    'rangebin'  - range time of bin.  
    'minbin'    - min time of bin.  
    'maxbin'    - max time of bin.  
    and any function handle that works on the first dimension.  
    Default is: {'meanbin',@nanmean,@nanstd}.  
    Output : - A matrix with the binned data.  
    - Cell array of the requested columns.  
    By : - Eran O. Ofek                  Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [B,C]=timeseries.binning_npt(rand(102,2))  
    [B,C]=timeseries.binning_npt(rand(102,2),30,{'medianbin',@range,@max})  
    Reliable : 2  
      
### timeseries.binning_old

The old version of the binning function. Use binning instead. Package: timeseries Description: Binning a timeseries using equally spaced bins. In each bin, calculate the mean, median, std,


    
    The old version of the binning function. Use binning instead.  
    Package: timeseries  
    Description: Binning a timeseries using equally spaced bins.  
    In each bin, calculate the mean, median, std,  
    skewness of the data.  
    OBSOLETE: Use binning.m instead.  
    Input  : - Matrix, in which the first column is the  
    time, the second column is the observation, and  
    optional third column is the value error.  
    - bin size, in units of the "time" column.  
    - First bin start Time (default is start point).  
    - Last bin end Time (default is end point).  
    Output : - Matrix with the following columns:  
    [Bin_Center,  
    <Y>,  
    StD(Y)/sqrt(N),  
    <X>,  
    Weighted-<Y>,  
    Formal-Error<Y>,  
    N,  
    median(Y),  
    StD(Y),  
    range(X)/2]  
    In case that no errors are given, columns 5 and 6  
    will be zeros.  
    - The matrix as above, but the NaNs are eliminated.  
    See also: bin_by_eye.m, runmean.m, bin_en.m  
    Tested : Matlab 5.0  
    By : Eran O. Ofek                    Feb 1999  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
### timeseries.calib_flux_lsq

Best fit zeropoints and mean magnitudes to a matrix of magnitudes. Package: timeseries Description: Calculate the best fit photometric zero point (ZP) per image and mean magnitude per source to a set of instrumental


    
    Best fit zeropoints and mean magnitudes to a matrix of magnitudes.  
    Package: timeseries  
    Description: Calculate the best fit photometric zero point (ZP) per  
    image and mean magnitude per source to a set of instrumental  
    magnitudes.  
    The function receive a a matrix of the instrumental  
    magnitudes of the sources in a set of images. The matrix  
    size is Nsrc X Nepoch (i.e., row per source, column per  
    image). Using linear least squares, it solves for the best  
    fit ZP per image and mean magnitude per star.  
    I.e., it solves the equation M_ij = ZP_j + <M>_i, where M_ij  
    is the matrix of instrumental magnitudes, ZP_j is the ZP of  
    the j-th image, and <M>_i is the mean magnitude of the i-th  
    source. Given the calibrated magnitudes of some of the  
    sources the function  can calibrate the solution.  
    Furthermore, de-trending using additional parameters is  
    possible.  
    Input  : - Matrix of instrumental magnitudes [Src,Epoch] to calibrate.  
    If no arguments are provided than will run in simulation mode.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=timeseries.calib_flux_lsq;  
    Reliable:  
      
      
### timeseries.calib_flux_sum

Normalize (calibrate) a matrix of fluxes using sum of flux in each epoch. Package: timeseries Description: Normalize (calibrate) a matrix of fluxes (Stars, Epochs) using sum of flux in each epoch.


    
    Normalize (calibrate) a matrix of fluxes using sum of flux in each epoch.  
    Package: timeseries  
    Description: Normalize (calibrate) a matrix of fluxes (Stars, Epochs)  
    using sum of flux in each epoch.  
    Select stars that appear in all epochs.  
    Calculate the sum of flux of these stars per epoch.  
    Normalize by the mean sum of fluxes.  
    Divide the stars fluxes by the normaliztion.  
    Remove stars based on quantile and max rms and iterate.  
    Input  : - Matrix of flux measurments (Flux_ij), for star i at epoch j.  
    THe values should be in units of electrons (see 'Gain').  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'Gain' - Gain by which to multiply to convert to electrons.  
    Default is 1.  
    'MeanNormFun' - Function to use for the sum flux normaliztion.  
    Default is @mean.  
    'MaxRMS' - Maximum relative rms of calibration stars.  
    Default is 0.1.  
    'QuantileRMS' - Fraction of best rms stars to use in  
    calibration. Default is 0.5.  
    'Niter' - Number of iterations. Default is 3.  
    Output : - Matrix of calibrated flux.  
    A structure containing the following fields:  
    'MeanFlux' - Vector of mean flux per star.  
    'StarsRelRMS' - Vector of stars relative rms after  
    calibration.  
    'FlagCS' - Flag indicating stars used for calibration.  
    'rmsZP'  - rms of ZP (normalization) at each epoch, as  
    100measured from the mean-normalized calibrated stars.  
    'rmsflux_selectPar' - Cell array of key,val parameters to pass  
    to timeseries.rmsflux_select.m.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: MeanFlux = rand(100,1).*1e5;  
    Norm     = 1+0.05.*randn(1,200);  
    Flux     = poissrnd(MeanFlux.*Norm);  
    [CalFlux,Res] = timeseries.calib_flux_sum(Flux);  
    loglog(Res.MeanFlux,Res.StarsRelRMS,'.')  
    Reliable: 2  
      
      
      
      
### timeseries.ccf

ccf function                                                      timeseries Description: Discrete cross-correlation function for two sets of unequaly spaced stationary time series.


    
      
    ccf function                                                      timeseries  
    Description: Discrete cross-correlation function for two sets of  
    unequaly spaced stationary time series.  
    ccf.m is obsolte - use dcf.m instead.  
    Input  : - first series matrix:  
    [Time, Mag, Err], the third column (Err) is optional,  
    if not given, assumes Err=0.  
    - second series matrix:  
    [Time, Mag, Err], the third column (Err) is optional,  
    if not given, assumes Err=0.  
    - BinSize is the delta lag on which the CCF is calculated.  
    - Type of correlation:  
    'normal' : normal correlation, error given by:  
    (1-rho^2)/sqrt(N-1), (default).  
    'z'      : z-transform, rho is transformed to z  
    (using z-transform), the errors are given  
    by 1/sqrt(N-3) has more Gaussian shape.  
    (Barlow 1989, p. 80)  
    - Correct structure function to measurments errors  
    {'y' | 'n'}, default is 'y'.  
    - Maximum lag to check, default is no limit (i.e., NaN).  
    Output : - CCF matrix:  
    [Mid. Lag, CCF, Err, point in bin-lag, Mean Lag, Struct_fun, err_struct_fun].  
    Note that: The structure function has units of amplitude^2.  
    The structure function is corrected for the  
    measurments errors, so it has zero amplitude  
    at zero lag.  
    Reference : Edelson, R.A. & Krolik, J.H. 1988 MNRAS 333, 646-659.  
    Koen, C. & Lombard, F. 1993 MNRAS 263, 287-308.  
    See Also  : ccf_o.m (old version - for equally spaced...)  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                      June 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### timeseries.ccf1

ccf1 function                                                 timeseries Description:


    
      
    ccf1 function                                                 timeseries  
    Description:  
    Input  : -  
    * Arbitrary number of pairs or arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    - Additional parameters  
    Any additional key,val, that are recognized by one of the  
    following programs:  
    Output : -  
    Tested : Matlab R2014a  
    By : Eran O. Ofek                    Sep 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X1=(1:0.1:100).'; Y1=sin(X1.*2.*pi./50)+randn(size(X1)).*0.01;  
    X2=(1:0.1:100).'; Y2=sin((X2+10).*2.*pi./50)+randn(size(X2)).*0.01;  
    [XC,Res]=ccf1([X1,Y1,0.01.*ones(size(X1))],[X2,Y2,0.01.*ones(size(X2))],1);  
    Reliable:  
      
      
### timeseries.ccf_fft

Cross correlation function of evenly spaced data using fft Package: timeseries Description: Calculate the normalized cross correlation function of two evenly spaced timeseries using fft.


    
    Cross correlation function of evenly spaced data using fft  
    Package: timeseries  
    Description: Calculate the normalized cross correlation function of  
    two evenly spaced timeseries using fft.  
    Input  : - First series. Either Y or [X,Y].  
    - Second series. Either Y or [X,Y].  
    Output : - Structure containing the following fields:  
    .X - The lag  
    .C - The normalized cross correlation.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Dec 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: R=randn(100,1); CCF=timeseries.ccf_fft(R,R); plot(CCF.X,CCF.C)  
    An example for a matched filter use:  
    Std = 0.5;  
    R=randn(100000,1).*Std;  generate a random equally spaced time series.  
    T=[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1]';  define a filter  
    R(30001:30015)=R(30001:30015) + T;  insert the template to the data  
    tic;CCF=timeseries.ccf_fft(R,T);toc  
    plot(CCF.X,CCF.C);  you can clearly see the peak at ~30,000  
    Reliable: 2  
      
### timeseries.ccf_old

Cross coorelation between two equally spaced series Package: timeseries Description: Cross correlation function for two sets of equally spaced time series.


    
    Cross coorelation between two equally spaced series  
    Package: timeseries  
    Description: Cross correlation function for two sets of equally  
    spaced time series.  
    Input  : - two column matrix in which the first column, is  
    an arbitrary index, and the second column is  
    the variable to correlate.  
    - second matrix of observations.  
    - lag to run on (default is N/4).  
    Output : - Two column matrix, in which the first column contains  
    the lag and the second the correlation.  
    - Pormanteau statistics.  
    - The standard deviation of the ccf (for very large N).  
    Reference : Koen, C. & Lombard, F. 1993 MNRAS 263, 287-308  
    See Also  : acf.m, ccf.m  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Nov 1995  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: timeseries.ccf_old([(1:1:100)',rand(100,1)],[(1:1:100)',rand(100,1)],20)  
    Reliable: 2  
      
### timeseries.cosbell

Cosine bell function (Obsolete: use timeseries.taper instead) Package: timeseries Description: cosine bell function Generating cosine bell function in the range


    
    Cosine bell function (Obsolete: use timeseries.taper instead)  
    Package: timeseries  
    Description: cosine bell function  
    Generating cosine bell function in the range  
    [Start:End] with its inner PercentFlat part  
    as flat function.  
    Input  : - The precentage of flat part of the cosine bell.  
    (in the range 0..1).  
    - Column vector of independent variable,  
    default is [0:0.01:1]'.  
    Output : - Column vector of cosine bell value.  
    - The Range (independent variable) column vector.  
    Tested : Matlab 5.0  
    By : Eran O. Ofek                    Dec 1997  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable : 2  
      
      
### timeseries.dcf

ccf function                                                  timeseries Description: Discrete cross-correlation function and structure function for two sets of unequaly spaced stationary


    
      
    ccf function                                                  timeseries  
    Description: Discrete cross-correlation function and structure  
    function for two sets of unequaly spaced stationary  
    time series.  
    Input  : - first series matrix:  
    [Time, Mag, Err], the third column (Err) is optional,  
    if not given, assumes Err=0.  
    - second series matrix:  
    [Time, Mag, Err], the third column (Err) is optional,  
    if not given, assumes Err=0.  
    - BinSize is the delta lag on which the CCF is calculated.  
    Alternatively this can be a colmn vector of lag edges.  
    - Use absolute value of time lag {'y' | 'n'}, default is 'y'.  
    In that case CCF will be symmetrical around zero lag.  
    Output : - Structure containing the discrete correlation function  
    with the following fields:  
    .Lag      - Mean lag.  
    .LagEdge  - Lower and upper edges of Lag.  
    .N        - Number of points in each lag.  
    .DCF      - The discrete correlation function.  
    .medDCF   - The median discrete correlation function.  
    .StD      - StD of discrete correlation function.  
    To estimate error divide .StD by .N  
    .BS_Err   - Bootstrap errors in discrete correlation function.  
    - Structure function structure with the following fields:  
    .Lag      - Mean Lag.  
    .SF       - Structure function [units if input Mag/flux]  
    .Err      - Error in structure function.  
    Reference : Edelson & Krolik, 1988, MNRAS 333, 646-659.  
    Koen, C. & Lombard, F. 1993 MNRAS 263, 287-308.  
    See Also  : ccf.m, ccf_o.m (old version - for equally spaced data)  
    Tested : Matlab 7.10  
    By : Eran O. Ofek                    Sep 2010  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### timeseries.dcf1

dcf1 function                                                 timeseries Description:


    
      
    dcf1 function                                                 timeseries  
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
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: T1=sort(rand(1000,1).*1000); T2=sort(rand(1000,1).*1000);  
    X1=sin(2.*pi.*T1./100); X2=sin(2.*pi.*T2./100+0.5);  
    XC = dcf1([T1,X1],[T2,X2],2);  
    Reliable:  
      
      
### timeseries.extract_phase

Extract observations take at some phase range Package: timeseries Description: Given a time series, extract observation made in a given phases range. The phase and epoch is defined by the user.


    
    Extract observations take at some phase range  
    Package: timeseries  
    Description: Given a time series, extract observation made in a given  
    phases range. The phase and epoch is defined by the user.  
    Input  : - observations matrix in which first column is time and  
    second column is observed value.  
    - ephemeris start time, T0.  
    - ephemeris period.  
    - Vector of phase range: [From To].  
    Output : - Observations matrix, containing only the observations  
    for which the phase restriction are fulfilled.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Oct 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: OutMat=timeseries.extract_phase(rand(100,2),0,0.1,[0.1 0.2]);  
    Reliable: 2  
      
      
### timeseries.fitPolyHyp

Hypothesis testing between fitting polynomials of various degrees to a matrix of light curves (with unknown errors).


    
    Hypothesis testing between fitting polynomials of various degrees to  
    a matrix of light curves (with unknown errors).  
    Input  : - A vector of times.  
    - A matrix of light curves [Epochs X Sources]  
    * ...,key,vals,...  
    'PolyDeg' - A cell array in wich each element contains all  
    the degrees of the polynomial to fit.  
    E.g., [0:1:2], is a full 2nd deg polynomial.  
    The first cell corresponds to the null hypothesis.  
    The Delta\chi2^2 is calculated relative to the null  
    hypothesis. In addition, the error normalization is  
    calculated such that the chi^2/dof of the null  
    hypothesis will be 1 (with uniform errors).  
    Default is {[0], [0:1:1], [0:1:2], [0:1:3], [0:1:4], [0:1:5]}.  
    'SubtractMeanT' - A logical indicating if to subtract the  
    mean of the time vectors from all the times.  
    Default is true.  
    'NormT' - A logical indicating if to normalize the times  
    to unity (i.e., max of abs of times will be 1.  
    Default is true.  
    'CalcProb' - Add a field to the output structure with the  
    probability to reject the null hypothesis given the  
    \Delta\chi^2. This may double the run time.  
    Default is false.  
    Output : - A structure array with parameters of the fit for each  
    tested polynomial (number of elements is like the number  
    of elements in PolyDeg).  
    .PolyDeg - Polynomial degrees in the fit.  
    .Npar - Number of free parameters in the fit.  
    .Par - The best fitted parameter for each LC. [Npar X Nsrc]  
    .Chi2 - chi^2 per source.  
    .Ndof - Number of degrees of freedom.  
    .ResidStd - Vector of std of residuals for each source.  
    .DeltaChi2 - A vector of \Delta\chi^2 per source.  
    .DeltaNdof - The difference in degrees of freedom between  
    this hypotesis and the null hypothesis.  
    .ProbChi2 - (return only if ProbChi2=true). - The  
    probability to reject the null hypothesis.  
    Author : Eran Ofek (Sep 2021)  
    Example: T   = (1:1:20)./1440;  
    Mag = randn(20,500);  
    Res = timeseries.fitPolyHyp(T, Mag);  
      
### timeseries.fit_polys_deltachi2

Fit polynomials of various orders to a time series and calculate chi^2 Package: timeseries Description: Fit polynomials of various orders to a time series and calculate chi^2 and rms for each polynomial fit.


    
    Fit polynomials of various orders to a time series and calculate chi^2  
    Package: timeseries  
    Description: Fit polynomials of various orders to a time series and  
    calculate chi^2 and rms for each polynomial fit.  
    Input  : - Data [time, magnitude, error]  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'MaxOrder'   - Maximum polynomial order. Default is 10.  
    'RejectProb' - Rejection probability for \Delta\chi^2  
    Default is 0.95.  
    'ColT'       - Time colum index. Default is 1.  
    'ColM'       - magnitude colum index. Default is 2.  
    'ColE'       - Error colum index. Default is 3.  
    Output : - Structure with fit information:  
    '  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Data=timeseries.fit_polys_deltachi2(Cat)  
    Reliable:  
      
      
### timeseries.fmaxs

fmaxs function                                                    timeseries Description: Given a matrix, find local maxima (in one of the columns) and return the maxima position and height.


    
      
    fmaxs function                                                    timeseries  
    Description: Given a matrix, find local maxima (in one of the columns)  
    and return the maxima position and height.  
    Input  : - Matrix of at least two columns.  
    - The column index of the independent variable,  
    default is 1.  
    - The column index of the dependent variable,  
    default is 2.  
    - Half window size for calculating the peak position  
    using wmean, default is 3.  
    Output : - Two column matrix of all local maxima.  
    The first column is the independent variable  
    (maxima position) while the second column is the  
    dependent variable (maxima height).  
    - Vector of indices of the local maxima in the original  
    matrix.  
    - Weighted mean position, error and maximum of fitted parabola  
    for each peak.  
    The weights are taken as 1/sqrt(Height).  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                  December 1993  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
      
### timeseries.folding

Folding a timeseries by some period Package: timeseries Description: Folding a time series into a period, and calculate the phase for each data point in the time series.


    
    Folding a timeseries by some period  
    Package: timeseries  
    Description: Folding a time series into a period, and calculate the phase  
    for each data point in the time series.  
    Input  : - Matrix in which one of the columns is the time.  
    - period to fold into.  
    - Column number to fold by (time column). Defualt is 1.  
    Output : - Matrix similar to the input matrix, but in which  
    the time column is replaced by the phase., and  
    the time column is concatenate in the last column.  
    The output matrix is sorted by phase.  
    Tested : Matlab 3.5  
    By : Eran O. Ofek                    Nov 1993  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: timeseries.folding(rand(100,2).*100,17);  
    Reliable: 1  
      
### timeseries.folding_solarsys

Folding a timeseries for a solar system object Package: timeseries Description: Folding a time series of a solar system object into a period, and calculate the phase for each data point in


    
    Folding a timeseries for a solar system object  
    Package: timeseries  
    Description: Folding a time series of a solar system object into a  
    period, and calculate the phase for each data point in  
    the time series.  Taking  
    into account ligh travel effect, phase correction due to  
    phase angle, and phase angle changes in brightness.  
    Input  : - Matrix of [Time, Mag, Err], where time is measured in days.  
    - Period to fold into [day].  
    - Observer-Target distance [AU].  
    - Sun-Target distance [AU].  
    - Sun-Target-Observer angle (phase angle) [deg].  
    This should be negative if the target is west of opposition.  
    - Slope parameter in the HG system. Default is 0.15.  
    - Retrograde (-1) or prograde (1) rotation. Default is 1.  
    Output : - Vector of phases (0 to 1) corresponding to each line in the  
    first input argument.  
    - Vector of minor planet corrected HG magnitudes.  
    Tested : Matlab 3.5  
    By : Eran O. Ofek                    Jul 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### timeseries.glsper_4github

Calculates The generalized Lomb-Scargle periodogram (Zechmeister 2009). Package: +timeseries Description: Calculates The generalized Lomb-Scargle periodogram (Zechmeister 2009).


    
    Calculates The generalized Lomb-Scargle periodogram (Zechmeister 2009).  
    Package: +timeseries  
    Description: Calculates The generalized Lomb-Scargle periodogram  
    (Zechmeister 2009).  
    If errors are not given, then unweighted means are used.  
    Input: f - frequencies vector to calculate GLS  
    t - time vecotor of the data  
    y - the data  
    e - errors of the data (optional)  
    fap_vals - GLS power values to calculate the fap (default = [0.1 0.01 0.001])  
    Output: gls - normalized GLS power for each f.  
    fap - selected false-alarm probability GLS values.  
    Created: 2015, Shay Zucker  
    Modified: 2018, Lev Tal-Or  
      
      
      
    Allocate output array.  
      
### timeseries.hjd

hjd function                                                  timeseries Description: Convert Julian Day (UTC) to Helicentric/Barycentric Julian Day for geocentric observer.


    
      
    hjd function                                                  timeseries  
    Description: Convert Julian Day (UTC) to Helicentric/Barycentric Julian  
    Day for geocentric observer.  
    Input  : - Column vector of julian days (UTC time system).  
    - J2000.0 object coordinates, [RA, Dec], in radians.  
    - Observer Geodetic position,  
    [East_Long (rad), North_Lat (rad), Geodetic height (meters)].  
    If geodetic position is not given (or empty matrix),  
    then assume geocentric observer.  
    - Output type:  
    'lh' - low accuracy heliocentric JD.  
    'hh' - high accuracy (full VSOP87) heliocentric JD.  
    'hb' - high accuracy (full VSOP87) barycentric JD, default.  
    Output : - Heliocentric/Barycentric julian day (for geocentric observer).  
    - Heliocentric/Barycentric velocity comonent in object  
    direction [km/sec] (only for 'hh' | 'hb' options).  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Nov 1993  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example : [OutJD,ObjVel]=hjd(JD,[RA Dec],'hb');  
    Reliable: 2  
      
      
### timeseries.matched_filter

Matched filter for 1-D time series Package: timeseries Description: Run a matched filter (North filter) for a 1-D series and return the possible peaks (signal) above a given threshold.


    
    Matched filter for 1-D time series  
    Package: timeseries  
    Description: Run a matched filter (North filter) for a 1-D series and  
    return the possible peaks (signal) above a given threshold.  
    Input  : - 1-D evenly spaced series.  
    - Filter. This is either a vector of filter, or a cell  
    array in which the first element is a function hendle  
    to generate the filter, and the other elements are input  
    arguments to pass to the filter function  
    (e.g., {'fun_gauss',[1 5 2],(1:1:10)',0}).  
    - Subtract a background prior to filtering.  
    This is a cell array of extra parameters to pass to  
    the subtract_back1d.m program (e.g., {'mean'}).  
    Default is {'none'}. If empty, then use default.  
    - Threshold above to select peaks. Default is 3.  
    If negative number is provided (e.g., -5), then will  
    estimate the sigma using the 68th percentile, and use  
    the absolute value of the threshold as the number of sigmas.  
    (e.g., -5 is 5 sigma).  
    Output : - Matrix of values above threshold:  
    [X index, Y at index, X at index, X after background removal].  
    - The matched filter series (Y).  
    - Actual threshold used.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Feb 2014  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=ones(10000,1); X(5001:5005)=[1.5 2 2.2 2 1.5]';  
    X=X+randn(10000,1).*0.1; F=[1.5 2 2.2 2 1.5]';  
    [Peaks,Y]=timeseries.matched_filter(X,F);  
    [Peaks,Y,Thresh]=timeseries.matched_filter(X,F,{'median'},-5);  
    Reliable: 2  
      
      
### timeseries.mean_plweighted

Power-law weighted mean of a time series. Package: timeseries Description:


    
    Power-law weighted mean of a time series.  
    Package: timeseries  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: timeseries.mean_plweighted(MatF)  
    Reliable:  
      
      
      
    DefV. =  
    InPar = InArg.populate_keyval(DefV,varargin,mfilename);  
      
### timeseries.minclp

minclp function                                                   timeseries Description: Search for periodicity in a time series, using the minimum-curve-length method. The program calculates the


    
      
    minclp function                                                   timeseries  
    Description: Search for periodicity in a time series, using the  
    minimum-curve-length method. The program calculates the  
    curve length for each trail frequency, and return the curve  
    length as function of frequency.  
    Input  : - Data matrix, sorted by time.  
    - Minimum frequency to search.  
    - Maximum frequency to search.  
    - Frequency interval to search. defualt is 0.2/Time_Span.  
    - Optional binning. If 0, then don't use binning, if ~=0  
    then use bins of size 'bin'. Default is 0.  
    - The time column, defualt is 1.  
    - The dependent variable column, defualt is 2.  
    Output : - Curve length as function of frequency,  
    [Frequency, Lengh].  
    - Frequency for which minimum length was obtained.  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                  November 1993  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
      
### timeseries.pdm

Periodicity search using period dispersion minimization Package: timeseries Description: Periodicity search using phase dispersion minimization. The time series is folded into trail frequencies, and in


    
    Periodicity search using period dispersion minimization  
    Package: timeseries  
    Description: Periodicity search using phase dispersion minimization.  
    The time series is folded into trail frequencies, and in  
    each trial frequency, the dispersion in bins is calculated.  
    Input  : - Data matrix, first column for time and second for magnitude.  
    - Frequency vector, [minimum_freq, maximum_freq, delta_freq]  
    - Number of bins in the folded period.  
    Output : - Matrix in which the first column is frequency,  
    the second is the dispersion indicator normalized  
    by the variance of the data (for a true period =1).  
    Reference: Stellingwerf, R.F. ApJ 224, 953-960 (1978).  
    Schwarzenberg-Czerny, A. ApJL, 489, 941-945 (1997)  
    see also : minds function.  
    Tested : Matlab 5.0  
    By : Eran O. Ofek                    Jun 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: t=(1:1:1000)'; M=sin(2.*pi.*t./27.1)+randn(1000,1).*0.1; E=0.1.*ones(1000,1); P=timeseries.pdm([t, M E],(0:0.0005:1)',10);  
    Reliable:  
      
      
### timeseries.pdm_phot

pdm_phot function    Phase Dispersion Minimization to Photon arrival time data.


    
      
    pdm_phot function    Phase Dispersion Minimization to Photon  
    arrival time data.  
    Input  : - Sorted vector of arrival times.  
    - Number of bins.  
    - Vector of frequencies to search,  
    [Low Frequency, High Frequency, Frequency Interval]  
    default is to choose the Low Freq. as 1/(Time Span)  
    the High Freq. as the mean(diff(photon arrival time))  
    Freq. Interval as 1/(4 X Time Span)  
    Output : - Vector of frequencies  
    - Vector of variances coresponding to each trial frequency.  
    - Vector of standard deviations of means coresponding  
    to each trial frequency.  
    Tested : Matlab 5.1  
    By : Eran O. Ofek           November 1996  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
### timeseries.period

Periodicity search in a non-equally spaced time series Package: timeseries Description: Periodicy search in a time series. This function can be used to run different types of periodicity searches and


    
    Periodicity search in a non-equally spaced time series  
    Package: timeseries  
    Description: Periodicy search in a time series. This function can be  
    used to run different types of periodicity searches and  
    window functions, and automatically select the range  
    of frequencies to search.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - Frequency range and interval in which to calculate the  
    power spectrum.  
    This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    Alternatively, this is a row vector of the following form  
    [MinFreq, MaxFreq, FreqInterval].  
    In this case MinFreq is the minimum frequency,  
    MaxFreq is the maximum frequency and FreqInterval  
    is the frequency interval. All given in units of 1/time.  
    Or [MaxFreq, FreqInterval]. In this case MinFreq=0.  
    Or [OverSampling] where oversampling is a scalar indicating  
    the over sampling to use for the FreqInterval. In this case  
    FreqInterval = MinFreq/OverSampling.  
    MinFreq      = 1/range(Time).  
    MaxFreq      = 1/min(diff(Time)).  
    If this parameter is an empty matrix (i.e., []) or  
    not specified, then the program use OverSampling=4  
    and calculate the rest of the parameters automatically.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    Where the possible keywords are available:  
    'Type'   - Type of power spectrum and algorithm:  
    'Norm'     - Normal (Lomb) periodogram.  
    'Norm2'    - Normal (Lomb) periodogram of the  
    second order (two equally weighted  
    harmonies).  
    'complex'  - The Fourier transform of an unevenly  
    spaced data.  
    'Scargle'  - Scargle periodogram (Default).  
    'NormNL'   - Normal periodogram with  
    no loops. The no-loops version is  
    faster but requires a lot of memory.  
    'ScargleNL'- Scargle periodogram with no loops.  
    'fft'      - FFT for equaly spaced time series.  
    In this case the Freq vector is  
    overrid by the program.  
    'Win'      - Calculate the window function.  
    Always normalized by amplitude.  
    'WinNL'    - Calculate the window function using  
    no loops.  
    Always normalized by amplitude.  
    'Norm'    - Method of normalization:  
    'var'      - Normalized by the variance (Default).  
    'amp'      - Amplitude normalization.  
    'RmNaN'   - Remove NaNs from time series beofre analysis  
    {'y'|'n'}, default is 'y'.  
    'Taper'   - Taper function name {'none'|'cosbell'|'trapz'}.  
    Default is 'none'.  
    'TaperPar'- Taper parameters - see taper.m for details.  
    Output : - Power spectrum [Frequency, Power].  
    - Structure containing information about the peaks in the power  
    spectrum. The following fields are available:  
    .FAB   - 1 - False alarma probability  
    .Freq  - Frequency  
    .Per   - Period (1/Frequency)  
    .PS    - Power.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=sort(rand(100,1).*100); Y=sin(X.*2.*pi.*0.9)+randn(100,1);  
    [PS,Peaks]=timeseries.period([X,Y]);  
    [PS,Peaks]=timeseries.period([X,Y],[0 2 0.001]);  
    [PS,Peaks]=timeseries.period([X,Y],[0 2 0.001],'Type','Scargle');  
    [PS,Peaks]=timeseries.period([X,Y],[],'Taper','cosbell','TaperPar',0.5);  
    Reliable: 2  
      
### timeseries.period_complex

Fourier transform of non equally spaced time series Package: timeseries Description: Calculate the complex fourier transform of an unevenly spaced time series.


    
    Fourier transform of non equally spaced time series  
    Package: timeseries  
    Description: Calculate the complex fourier transform of an unevenly  
    spaced time series.  
    See period.m for a more flexiable function.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - Frequency range and interval in which to calculate the  
    power spectrum.  
    This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    'Amp' - Normalize by amplitude.  
    - Subtract mean {true|false}. Default is true.  
    Output : - Two columns matrix of the un-normalized power spectrum  
    [frequency, power_spec].  
    See also: period.m  
    Tested : Matlab 2015b  
    By : Eran O. Ofek                    Dec 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### timeseries.period_consistent

SHORT DESCRIPTION HERE Package: timeseries Description:


    
    SHORT DESCRIPTION HERE  
    Package: timeseries  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Mar 2018  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: N=1000; T = sort(rand(N,1).*1000); Per=23.2; M=sin(2.*pi.*T./Per)+randn(N,1).*0.1;  
    p = T./30 - floor(T./30); F=p<0.3; T=T(F); M=M(F);  
    Freq = (0:0.0005:0.1)';  
    PSn=timeseries.period_norm([T,M],Freq);  
    Reliable:  
      
      
### timeseries.period_events

- period_events function                                           timeseries Description: Search for periodicity in a list of "time tagged" events. The search is done by folding the events


    
    -  
    period_events function                                           timeseries  
    Description: Search for periodicity in a list of "time tagged"  
    events. The search is done by folding the events  
    and background event to each trial period. In each  
    trial period the data is binned and the \chi^2  
    for a constant rate is calculated.  
    Input  : - Vector of events [time tags].  
    - Vector of background events [time tags].  
    - Ratio between the area of aperture from which the background  
    events were extracted and the area of the aperture from which  
    the events were extracted (=BackgroundArea/SourceArea).  
    - Number of bins in each trial period.  
    - Vector of trial periods to test.  
    - Method in which to calculate mean count rate:  
    {'wmean'|'mean'|'median}, default is 'median'.  
    - Number of sigmas from mean which will be counted  
    (see ouput: Sigma counter), default is 2.  
    Output : - Chi^2 after subtracting the actual background rate in each bin.  
    - Number of degrees of freedom.  
    - Sigma Counter: Number of bins which falls [below, above] Nsigma  
    from mean.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                       Feb 2007  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    -  
### timeseries.period_fft

Power spectrum of evenly spaced time series using fft Package: timeseries Description: Calculate power spectrum for evenly spaced time series using fast Fourier transform.


    
    Power spectrum of evenly spaced time series using fft  
    Package: timeseries  
    Description: Calculate power spectrum for evenly spaced time series  
    using fast Fourier transform.  
    See also period.m  
    Input  : - Two columns matrix (e.g., [time, x]).  
    - Power spectrum normalization:  
    'no'    - no normalization.  
    'amp'   - amplitude normalization (e.g., sqrt of power).  
    'var'   - Normalize by variance, default.  
    Output : - Power spectrum [Frequency, power].  
    - Structure containing information about the peaks in the power  
    spectrum. The following fields are available:  
    .FAB   - 1 - False alarma probability  
    .Freq  - Frequency  
    .Per   - Period (1/Frequency)  
    .PS    - Power.  
    See also: period.m  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Jul 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### timeseries.period_fitfourier

Fit a Fourier series to a time series Package: timeseries Description: Fit a polynomial and fourier series as a function of frequency to a time series.


    
    Fit a Fourier series to a time series  
    Package: timeseries  
    Description: Fit a polynomial and fourier series as a function of  
    frequency to a time series.  
    The fitted series is:  
    Y = A_ + A_*T + A_*T^3,... A_*sin(2*pi*f*T*H(1)) +  
    A_*sin(2*pi*f*T*H(2)) + ...  
    A_*cos(2*pi*f*T*H(1)) +  
    A_*cos(2*pi*f*T*H(2)) + ...  
    See period.m for a more flexiable function.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - Frequency range and interval in which to calculate the  
    power spectrum.  
    This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Row vector of Harmonies to fit, e.g. [1 2 3].  
    Default is [1 2].  
    - Degree of polynomials to fit. Default is 1.  
    Output : - Two columns matrix of the power spectrum equivalent  
    [frequency, amplitude], where amplitude is the total amplitude  
    of all the harmonies at a given frequency.  
    - Structure containing the best fit parameters and errors  
    for each frequency.  
    [1 T T^2 T^3,... sin(2*pi*f*H*T), ..., cos(2*pi*f*H*T),...]  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [PS,Par,Stat]=timeseries.period_fitfourier(Data,FreqVec,Harmon,PolyN)  
      
      
### timeseries.period_mi

SHORT DESCRIPTION HERE Package: timeseries Description:


    
    SHORT DESCRIPTION HERE  
    Package: timeseries  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: P=timeseries.period_mi([x,y],Freq);  
    Reliable:  
      
      
      
### timeseries.period_min_curve_length

Periodicity search using minimum curve length Package: timeseries Description: Search for periodicity in a time series, using the minimum-curve-length method. The program calculates the


    
    Periodicity search using minimum curve length  
    Package: timeseries  
    Description: Search for periodicity in a time series, using the  
    minimum-curve-length method. The program calculates the  
    curve length for each trail frequency, and return the curve  
    length as function of frequency.  
    Input  : - Data matrix, sorted by time.  
    - Minimum frequency to search.  
    - Maximum frequency to search.  
    - Frequency interval to search. defualt is 0.2/Time_Span.  
    - Optional binning. If 0, then don't use binning, if ~=0  
    then use bins of size 'bin'. Default is 0.  
    - The time column, defualt is 1.  
    - The dependent variable column, defualt is 2.  
    Output : - Curve length as function of frequency,  
    [Frequency, Lengh].  
    - Frequency for which minimum length was obtained.  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Nov 1993  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### timeseries.period_norm

Normalized power spectrum of non equally spaced time series Package: timeseries Description: Calculate the normalized normal power spectrum of a times series. See period.m for a more flexiable function.


    
    Normalized power spectrum of non equally spaced time series  
    Package: timeseries  
    Description: Calculate the normalized normal power spectrum of a times  
    series. See period.m for a more flexiable function.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    'Amp' - Normalize by amplitude.  
    - Subtract mean {true|false}. Default is true.  
    Output : - Two columns matrix of the un-normalized power spectrum  
    [frequency, power_spec].  
    See also: period.m  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### timeseries.period_norm_bootstrap

Run bootstrap normal pertiodogram on a light curve. Package: timeseries Description:


    
    Run bootstrap normal pertiodogram on a light curve.  
    Package: timeseries  
    Description:  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    'Amp' - Normalize by amplitude.  
    - Subtract mean {true|false}. Default is true.  
    - Number of bootstrap simulations. Default is 1000.  
    Output : - Matrix in wgich each column is the power spectrum for a  
    specific bootstrap simulation. Number of columns is the number  
    of simulations.  
    - Vector of corresponding frequencies.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: t=timeseries.random_time_sequence;  
    x=0.2.*sin(2.*pi.*t./11) + randn(numel(t),1);  
    F = (0:0.001:1)';  
    P=timeseries.period([t,x],F);  
    [PS,F]=timeseries.period_norm_bootstrap([t,x],F);  
    plot(F,P(:,2)./std(PS,[],2))  
    Reliable: 2  
      
      
      
      
    DefV. =  
    InPar = InArg.populate_keyval(DefV,varargin,mfilename);  
      
      
### timeseries.period_norm_order2

2nd order harmonic power spectrum of non evenly spaced time series Package: timeseries Description: Calculate the normalized power spectrum of the second order of a times series (i.e., assuming that the first and second


    
    2nd order harmonic power spectrum of non evenly spaced time series  
    Package: timeseries  
    Description: Calculate the normalized power spectrum of the second order  
    of a times series (i.e., assuming that the first and second  
    harmonies have the same amplitude).  
    See period.m for a more flexiable function.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - Frequency range and interval in which to calculate the  
    power spectrum.  
    This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    'Amp' - Normalize by amplitude.  
    - Subtract mean {true|false}. Default is true.  
    Output : - Two columns matrix of the un-normalized power spectrum  
    [frequency, power_spec].  
    See also: period.m  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X = (0:1:100).'; Y = sin(2.*pi.*X./11) + sin(4.*pi.*X./11 + 1) +randn(size(X)).*0.01;  
    PS=timeseries.period_norm_order2([X,Y],(0:0.001:1).');  
    Reliable: 2  
      
### timeseries.period_norm_solarsys

Normalized power spectrum for a Solar System object Package: timeseries Description: Calculate the normalized normal power spectrum of a times series for a solar system object orbiting the Sun. Taking


    
    Normalized power spectrum for a Solar System object  
    Package: timeseries  
    Description: Calculate the normalized normal power spectrum of a times  
    series for a solar system object orbiting the Sun. Taking  
    into account ligh travel effect, phase correction due to  
    phase angle, and phase angle changes in brightness.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    Where time is measured in days.  
    - Frequency range and interval in which to calculate the  
    power spectrum.  
    This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    If empty use default.  
    'Amp' - Normalize by amplitude.  
    - Observer-Target distance [AU].  
    - Sun-Target distance [AU].  
    - Sun-Target-Observer angle (phase angle) [deg].  
    This should be negative if the target is west of opposition.  
    - Slope parameter in the HG system. Default is 0.15.  
    - Retrograde (-1) or prograde (1) rotation. Default is 1.  
    Output : - Two columns matrix of the un-normalized power spectrum  
    [frequency, power_spec].  
    See also: period.m  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    Jul 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
### timeseries.period_normnl

Normzlied power spectrum using no loops (may be faster in some cases) Package: timeseries Description: Calculate the classical (Lomb) power spectrum of a time series using no loops.


    
    Normzlied power spectrum using no loops (may be faster in some cases)  
    Package: timeseries  
    Description: Calculate the classical (Lomb) power spectrum of a time  
    series using no loops.  
    See period.m for a more flexiable function.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - Frequency range and interval in which to calculate the  
    power spectrum.  
    This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    'Amp' - Normalize by amplitude.  
    Output : - Two columns matrix of the un-normalized power spectrum  
    [frequency, power_spec].  
    See also: period.m  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: FreqVec=(0:0.001:1).'; T=(1:1:500).'; Data = [T,sin(2.*pi.*0.1.*T)];  
    PS = timeseries.period_normnl(Data,FreqVec);  
    Reliable: 2  
      
      
### timeseries.period_np

SHORT DESCRIPTION HERE Package: timeseries Description:


    
    SHORT DESCRIPTION HERE  
    Package: timeseries  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
### timeseries.period_pdm

SHORT DESCRIPTION HERE Package: timeseries Description:


    
    SHORT DESCRIPTION HERE  
    Package: timeseries  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: PS=timeseries.period_pdm([T,M,E],FreqVec);  
    Reliable:  
      
      
      
### timeseries.period_plot

SHORT DESCRIPTION HERE Package: timeseries Description:


    
    SHORT DESCRIPTION HERE  
    Package: timeseries  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
### timeseries.period_proper

SHORT DESCRIPTION HERE Package: timeseries Description:


    
    SHORT DESCRIPTION HERE  
    Package: timeseries  
    Description:  
    Input  : -  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jan 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    Reliable:  
      
      
      
      
### timeseries.period_scargle

Scargle power spectrum of non equally spaced time series Package: timeseries Description: Calculate the un-normalized Scargle power spectrum of a times series. See period.m for a more flexiable function.


    
    Scargle power spectrum of non equally spaced time series  
    Package: timeseries  
    Description: Calculate the un-normalized Scargle power spectrum of a times  
    series. See period.m for a more flexiable function.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - Frequency range and interval in which to calculate the  
    power spectrum.  
    This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    'Amp' - Normalize by amplitude.  
    Output : - Two columns matrix of the un-normalized power spectrum  
    [frequency, power_spec].  
    Reference: Scargle, J.D. ApJ 263, 835-853 (1982).  
    Horne, J.H. & Baliunas, S.L. ApJ 302, 757-763 (1986).  
    See also: period.m  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### timeseries.period_scarglenl

Scargle power spectrum of non equally spaced time series / no loops Package: timeseries Description: Calculate the un-normalized Scargle power spectrum of a times series using no loops.


    
    Scargle power spectrum of non equally spaced time series / no loops  
    Package: timeseries  
    Description: Calculate the un-normalized Scargle power spectrum of a times  
    series using no loops.  
    See period.m for a more flexiable function.  
    Input  : - Two column matrix containing the time series  
    [Time, measurment] or [Time, measurment, error].  
    - Frequency range and interval in which to calculate the  
    power spectrum.  
    This is a column vector of frequencies at which to  
    calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    'Amp' - Normalize by amplitude.  
    Output : - Two columns matrix of the un-normalized power spectrum  
    [frequency, power_spec].  
    Reference: Scargle, J.D. ApJ 263, 835-853 (1982).  
    Horne, J.H. & Baliunas, S.L. ApJ 302, 757-763 (1986).  
    See also: period.m  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
      
### timeseries.periodia

Classical power spectrum of non-evenly space time series (OBSOLETE) Package: timeseries Description: Classical power-spectrum of a non-evenly spaced time series. The power spectrum is normalized by the variance of the data.


    
    Classical power spectrum of non-evenly space time series (OBSOLETE)  
    Package: timeseries  
    Description: Classical power-spectrum of a non-evenly spaced time series.  
    The power spectrum is normalized by the variance of the data.  
    OBSOLETE: Use timeseries.period instead.  
    Input  : - Time series matrix, [Time, Mag], in which the first column  
    is the time and the second column is the magnitude.  
    - Lowest frequency (h_l).  
    - Highest frequency (h_f).  
    - Frequency interval (df).  
    - The column of time (c_x), default is 1.  
    - The column of magnitudes (c_y), default is 2.  
    Output : - Periodigram matrix. normalized with the variance  
    of the observations (Horne & Baliunas 1986).  
    The first column is the frequency and  
    the second column is the power.  
    - The peaks of the periodogram sorted by the probability.  
    [Frequency, Power, Period, (1-False alarm probability)].  
    The probability is good only for evenly spaced data  
    See Also : periodis; periodit; pdm; fitharmo; period  
    Reference: Koen, C. 1990, ApJ 348, 700-702.  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Dec 1993  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
      
### timeseries.periodis

Scargle periodogram. OBSOLETE: Use period.m instead. Package: timeseries Description: Lomb-Scargle periodigram. Calculate the Lomb-Scargle power spectrum for a time series.


    
    Scargle periodogram. OBSOLETE: Use period.m instead.  
    Package: timeseries  
    Description: Lomb-Scargle periodigram. Calculate the Lomb-Scargle power  
    spectrum for a time series.  
    OBSOLETE: Use timeseries.period instead.  
    Input  : - Timeseries matrix, [Time, Mag].  
    - Lowest frequency (h_l).  
    - Highest frequency (h_f).  
    - Frequency interval (df).  
    - The probability cutoff (pr), default no cutoff.  
    Power spectra peaks with probability smaller than this  
    cutoff are not listed.  
    Output : - Periodigram matrix [Frequency, Power] normalized by the  
    variance of the data points (Horne & Baliunas 1986).  
    - The peaks of the periodogram sorted by the spectral power.  
    [Frequency, Power, Period, (1-False alarm probability)].  
    See Also : periodia; periodit; pdm; fitharmo; period  
    Reference: Scargle, J.D. ApJ 263, 835-853 (1982).  
    Horne, J.H. & Baliunas, S.L. ApJ 302, 757-763 (1986).  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Mar 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 1  
      
      
### timeseries.periodit

Calculate the periodogram as a function of time (will be removed) Package: timeseries Description: Calculate periodogram as function of time. This script can be used for searchnig for long term varibility in


    
    Calculate the periodogram as a function of time (will be removed)  
    Package: timeseries  
    Description: Calculate periodogram as function of time. This script can  
    be used for searchnig for long term varibility in  
    periodicities. The time series ins divided into blocks  
    with or without overlays. Each block can be tapered, and a  
    periodogram (Clasical or Scargle) is calculated. The results  
    are displaied as a mesh of frequncy vs. time vs. power.  
    Input  : - Matrix of observations. Should contain at least two  
    column. First column for time and second column for  
    "magnitude". Each row for each observation.  
    - vector of parameters defined the epochs of sub  
    periodograms. First element should contain the number  
    of epochs in the time span. and the second element  
    should contain the size of each sub epoch in units of  
    the total time span. default is [10,0.2].  
    - Tapering function parameters. Each sub epoch could be  
    weighted by a tapering function.  
    Avaliable tapers:  
    ['C',p] : cosine bell taper while "p" is the precentage  
    of flat (uniform) part of the cosine bell in  
    the range 0-1.  
    ['F']   : Flat (uniform) taper. No tapering.  
    default is ['C',0.6].  
    - frequency vector of the type:  
    [Low_Freqency, High_Frequency, Frequency_Interval].  
    default is [0,1./(2min(Interval)),1./(2(Time_Span)].  
    - Periodogram type:  
    'c' : classical periodogram.  
    's' : Scargle periodogram.  
    defualt is 'c' (classical).  
    - Display mesh at end of calculation.  
    'y' or 'n'. default is 'y'.  
    Output : - Matrix of powers each column contain power for each  
    sub epoch.  
    - Matrix of leading frequency. Not Yet Avaliable.  
    - vector of sub epochs mid time.  
    - vector of frequencies searched.  
    Remarks: Not all the option were checked. Please report  
    any bug/anomalies.  
    Example: [P,F,T,Fr]=timeseries.periodit([(1:1:1000)',rand(1000,1)],[30,0.1],[],[],'s');  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                    Dec 1997  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### timeseries.periodmulti_norm

Simultanous power spectrum of time series with common times Package: timeseries periodmulti_norm function                                     timeseries Description: Calculate the normal power spectrum of a set of multiple


    
    Simultanous power spectrum of time series with common times  
    Package: timeseries  
    periodmulti_norm function                                     timeseries  
    Description: Calculate the normal power spectrum of a set of multiple  
    times series which have common times.  
    This program run the power spectrum simultaneously for all  
    time serieses and its is faster than running the power  
    spectrum on a single time series at a time.  
    Input  : - Vector of common times for all time serieses.  
    - Matrix of measurements in which each column refer to one  
    time series and each row refer to one time.  
    - Vector of frequencies in which to calculate the power spectrum.  
    - Normalization method:  
    'Var' - Normalize by variance (Default).  
    'Amp' - Normalize by amplitude.  
    Output : - Vector of frequencies.  
    - Matrix of power spectrum. Each column represent the power  
    spectrum for each time series.  
    See also: period.m, period_norm.m  
    Tested : Matlab 7.16  
    By : Eran O. Ofek                    Sep 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### timeseries.perioent

Periodogram using information entropy Package: timeseries Description: Periodogram using information entropy. For each trail period, the phase-magnitude space is divided into m by m


    
    Periodogram using information entropy  
    Package: timeseries  
    Description: Periodogram using information entropy. For each trail  
    period, the phase-magnitude space is divided into m by m  
    cells, and the information entropy is calculated.  
    Then the probability to find observations in each square is  
    MU(i) and the Entropy is (-Sigma{MU*ln(MU)}). The output  
    Entropy is normalized by ln(m*m).  
    Input  : - Data matrix, sorted by time.  
    - Frequency Vector, [Lower Freq., Higher Freq., Freq. Interval]  
    - Square root of number of elements in the unit square.  
    Output : - Two column matrix, [Frequency, Entropy].  
    reference: Cincotta, Mendez & Nunez 1995, ApJ 449, 231-235.  
    Tested : Matlab 5.1  
    By : Eran O. Ofek                    Nov 1996  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=(1:1:100).'; Y=sin(2.*pi.*X./5)+randn(size(X));  
    S=timeseries.period_entropy([X,Y],[0 1 0.0025],10);  
    Reliable: 2  
      
      
### timeseries.phot_event_me

phot_event_me function     Searching periodicity in time-tagged events using information entropy. For each trail period, the phase-magnitude space


    
      
    phot_event_me function     Searching periodicity in time-tagged  
    events using information entropy.  
    For each trail period, the phase-magnitude space  
    is divided into m by m cells, and the information  
    entropy is calculated.  
    Then the probability to find observations  
    in each square is MU(i) and the Entropy is  
    (-Sigma{MU*ln(MU)}).  
    The output Entropy is normalized by ln(m*m).  
    input  : - Sorted vector of events.  
    - Frequency Vector, [Lower Freq., Higher Freq., Freq. Interval]  
    - number of elements in phase (number of bins).  
    output : - Entropy vs. Frequency.  
    This is a two column matrix. The first column contains  
    the frequency and the second list the entropy.  
    - One sigma probability.  
    Reference : Cincotta et al. 1999, MNRAS 302,582.  
    Tested : Matlab 5.2  
    By  Eran O. Ofek           April 1999  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
      
      
      
### timeseries.plot_inspect_lc

plot and inspect light curve and power spectrum Package: timeseries Description:


    
    plot and inspect light curve and power spectrum  
    Package: timeseries  
    Description:  
    Input  : - Light curve [time, mag, error]  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : - Structure of light curve properties.  
    - Power spectrum.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Sep 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Prop,PS]=plot_inspect_lc(LC)  
    Reliable:  
      
      
      
### timeseries.plot_period_folder

plot_period_folder function                                       timeseries Description: Given the power spectrum and light curve, let the user to interactively select peaks in the power spectrum, and


    
      
    plot_period_folder function                                       timeseries  
    Description: Given the power spectrum and light curve, let the user to  
    interactively select peaks in the power spectrum, and  
    present the folded (and binned) light curve for the  
    selected periods.  
    Input  : - Power spectrum [frequency, power].  
    - Light curve [Time, Magnitude, Error], where Error is  
    optional.  
    If only one column is given [Time], then assume  
    the light curve is photon time tags, and present  
    an histogram of counts as a function o phase.  
    - Bin size for folding [phase units], default is 0.2.  
    Output : - Vector of all selected periods.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                       Feb 2007  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
      
### timeseries.polysubs

Fit and subtract polynomials from a timeseries [T,Y]. Package: timeseris Description: Subtract polynomial from a data set (no errors).


    
    Fit and subtract polynomials from a timeseries [T,Y].  
    Package: timeseris  
    Description: Subtract polynomial from a data set (no errors).  
    Input  : - Data matrix.  
    - Degree of polynom to subtract.  
    - c_x, column of dependent variable, default is 1.  
    - c_y, column of independent variable, default is 2.  
    Output : - Data matrix after the polynomial subtraction.  
    The other columns of original matrix are copied to y  
    only if c_x=1 and c_y=2.  
    - Polynomial coefecient of fit.  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                       May 1994  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Reliable: 2  
      
      
### timeseries.ps_whitening

ps_whitening function                                         timeseries Description: Whiten a time series using its power spectrum. I.e., ifft(fft(x)./abs(fft(x)))/


    
      
    ps_whitening function                                         timeseries  
    Description: Whiten a time series using its power spectrum.  
    I.e., ifft(fft(x)./abs(fft(x)))/  
    Input  : - Time series [T, X], or [X] if the time series is equally  
    spaced.  
    Output : - Whitned time series.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Feb 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [WTS]=ps_whitening(TS  
    Reliable:  
      
      
      
      
      
### timeseries.random_time_sequence

Generate random times for an astronomical time series. Package: timeseries Description: Generate random times for an astronomical time series, including daily and annual gaps.


    
    Generate random times for an astronomical time series.  
    Package: timeseries  
    Description: Generate random times for an astronomical time series,  
    including daily and annual gaps.  
    Input  : - Total time span [days]. Default is 365.*3.  
    - Mean cadence [days]. Default is 2.  
    - Length of annual obsrving period [days]. Default is 240.  
    - Random std for observing times [days]. Default is 0.05.  
    - Montly observing fraction. Default is 0.8.  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: T=timeseries.random_time_sequence  
    Reliable:  
      
### timeseries.resample_uniform

Uniform resampling of a non-evenly spaced time series. Package: timeseries Description: Uniform resampling of a non-evenly spaced time series.


    
    Uniform resampling of a non-evenly spaced time series.  
    Package: timeseries  
    Description: Uniform resampling of a non-evenly spaced time series.  
    Input  : - A column vector of times.  
    - A matrix of values to resample, one column per property.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'DeltaT' - If a scalar than this is the time step (in the tim  
    units) of the equally spaced resampled time series.  
    If a vector than this is the vector of time at  
    which to resample the light curve (not necesserly  
    evenly spaced). Default is 1.  
    'InterpMethod' - A cell array of interpolation methods with  
    which to resample the light curve. A method per  
    column in the second input argument. If a single  
    method is provided than it will be applied to all  
    columns.  
    Default is 'linear'.  
    Output : - A matrix of resampled light curves (one per column).  
    - A vector of the resampled times.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Aug 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [IntVal,NewT]=timeseries.resample_uniform(rand(100,1).*100,rand(100,1))  
    Reliable: 2  
      
      
      
### timeseries.rmsflux_select

Select points in an flux/mag vs. rms like diagram Package: timeseries Description: Select points in an rms vs. flux like diagram. Bin the flux/mag vs. rms diagram by flux bins or by constant


    
    Select points in an flux/mag vs. rms like diagram  
    Package: timeseries  
    Description: Select points in an rms vs. flux like diagram.  
    Bin the flux/mag vs. rms diagram by flux bins or by constant  
    number of points per bin. Calculate the median and rstd in  
    each bin and flag sources which are below the median +  
    N*rstd, where N is the 'SigClip' parameter.  
    I.e., good sources are flaged.  
    Input  : - A vector of flux/mag -like parameter.  
    - A vector of rms-like parameter.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'BinMethod' - 'Fit' : Fit a polynomial to the flux vs. rms.  
    This is more reliable when only small  
    number of sources are available.  
    'Flux': bin by constant flux bins.  
    'Number' (default) - by constant number of  
    sources in bin.  
    'PolyOrder' - Order of polynomial fitted when 'BinMethod' is  
    'Fit'.  
    'PolyFluxLog' - {true|false} When using the 'Fit' BinMethod,  
    apply log10 to flux before fit. Default is  
    true.  
    'NinBin' - Number in bin (default is 30) for the 'Number'  
    method.  
    'FluxBinSize' - Bin size for the 'Flux' method. Default is 0.5  
    (good for magnitude units).  
    'MinInBin' - Minimum niber of sources in bin. Default is 3.  
    'MinGoodBins' - Required number of good bins (that contains  
    more than 'MinInBin' stars. Default is 2.  
    If not will end with error.  
    'SigClip' - Sigma clipping parameter. Default is 3.  
    'StdFun'  - Default is @Util.stat.rstd  
    'MeanFun' - Default is @nanmedian  
    'InterpMethod' - Default is 'linear'  
    'Plot'    - Plot rms vs. flux with selected and unselected  
    sources.  
    Output : - Flag of sources which RMS is below the rms vs. flux envelope.  
    - Structure containing additional data:  
    'Bin' - Matrix of binned rms vs, flux.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Flag,Res]=timeseries.rmsflux_select(Flux,RMS);  
    A full example:  
    MeanFlux = rand(100,1).*1e5;  
    Norm     = 1+0.05.*randn(1,200);  
    Flux     = poissrnd(MeanFlux.*Norm);  
    [CalFlux,Res] = timeseries.calib_flux_sum(Flux);  
    loglog(Res.MeanFlux,Res.StarsRelRMS,'.')  
    [Flag,ResS]=timeseries.rmsflux_select(Res.MeanFlux,Res.StarsRelRMS,'Plot',true);  
    set(gca,'XS','log','YS','log')  
    Reliable: 2  
      
      
### timeseries.runmean

Running mean of un-evenly spaced time series Package: timeseries Description: Calculate the runing mean of an unevenly spaced time series with different weight functions and weight schemes.


    
    Running mean of un-evenly spaced time series  
    Package: timeseries  
    Description: Calculate the runing mean of an unevenly spaced time  
    series with different weight functions and weight schemes.  
    Input  : - Data matrix, [Time, Value, Error].  
    Error is optional, if not given then assumes  
    equal weights.  
    - Weight function type:  
    'f' : flat function:  
    parameter is : i) total width  
    'g' : gaussian function (truncated at 3\sigma):  
    parameter is : i) HWHM width  
    'c' : cosine-bell function:  
    parameters are: i) total width  
    ii) fraction of flat part  
    - Weight function vector of parameters.  
    - Weighting scheme:  
    'f2' - use weight function squared only, default.  
    'f'  - use weight function only.  
    'wm' - use points-errors (weighted mean) only.  
    'wf' - use sum of squares of weight function and  
    normalize points-errors so their mean is RelWeight.  
    - Time vector for which to calculate the running mean,  
    default is Data(:,1).  
    Output : - Runing mean matrix [Time, Value].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Jan 2002  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: timeseries.runmean(rand(100,3),'c',[0.1 0.05],'wf');  
    Reliable: 2  
      
      
### timeseries.runmean1

Running mean on equally spaced 1-D vector Package: timeseries Description: running mean on a 1-D vector.


    
    Running mean on equally spaced 1-D vector  
    Package: timeseries  
    Description: running mean on a 1-D vector.  
    Input  : - Vector.  
    - Half size of running mean filter. Filter size will be  
    2*HalfSize+1. Default is 1.  
    Output : - Output vector after running mean. The output vector will  
    have the same size as the input vector.  
    Tested : Matlab R2011b  
    By : Eran O. Ofek                    Sep 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: R=zeros(10,1); R(5)=1; RM=timeseries.runmean1(R,2);  
    Reliable: 1  
      
      
      
### timeseries.sf_interp

sf_interp function                                            timeseries Description: Interpolate a time series, and estimate the errors due to the interpolation using the structure function


    
      
    sf_interp function                                            timeseries  
    Description: Interpolate a time series, and estimate the errors due  
    to the interpolation using the structure function  
    of the time series. The error in each interpolated  
    point is calculated by adding in quadrature the error  
    of the neastest point with the amplitude of the  
    stracture function at the the lag equal to the  
    difference between the interpolated point and the  
    nearest point.  
    Input  : - Observations [Time, Mag, Err].  
    - Structure function binning interval.  
    - Vector of times for which to interpolate the LC.  
    - Interpolation method, default is 'linear'.  
    - Optional CCF matrix (see ccf.m) with the  
    structure function in columns [1 6 7].  
    If not given then calculate the structure function.  
    Output : - Interpolated LC, [Time, Mag, Err].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Dec 2002  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reference : Ofek & Maoz 2003 ApJ 594, 101  
    Reliable: 2  
      
      
### timeseries.simulated_elc

Simulated photons light curve Package: timeseries Description: Given a model light curve, generate a list of time-tagged events that their rate follows the model light curve.


    
    Simulated photons light curve  
    Package: timeseries  
    Description: Given a model light curve, generate a list of time-tagged  
    events that their rate follows the model light curve.  
    The events are generated between the first and last time  
    in the model light curve.  
    Note that the first event is always at the first time  
    in the model light curve.  
    The rate at each time is calculated using a second  
    order approximation: Tau0*(1 + dTau/dt + [dTau/dt]^2).  
    Input  : - Light curve model [time, Lambda], where Lambda is the mean  
    rate (>0) at each time (e.g., events per unit time).  
    - Matrix, containing in each line the start and end times of  
    active windows. Events which thier time tags is outside  
    these windows are deleted.  
    If empty matrix (e.g., []) then, all times are used.  
    Default is [].  
    - Rate (per unit time) of optional background.  
    If empty matrix (e.g., []), then no background. Default is [].  
    Output : - Sorted time tagged events.  
    - Number of backround events.  
    Tested : Matlab 7.3  
    By : Eran O. Ofek                    Jan 2008  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Events,Nbe]=timeseries.simulated_elc([1 10; 2 10; 3 500; 4 10; 5 10],[1 5],5);  
    Reliable: 2  
      
      
### timeseries.specwin

Power spectrum window (use timeseries.period instead) Package: timeseries Description: Calculate the spectral window of a time series. OBSOLOETE: Use timeseries.period instead


    
    Power spectrum window (use timeseries.period instead)  
    Package: timeseries  
    Description: Calculate the spectral window of a time series.  
    OBSOLOETE: Use timeseries.period instead  
    Input  : - Time series matrix.  
    - h_l, is the low frequency.  
    - h_f, is the high frequency.  
    - df, is the frequency interval.  
    - Time column, default is 1.  
    Output : - Spectral window, [Freq, Power].  
    Tested : Matlab 4.2  
    By : Eran O. Ofek                    Oct 1994  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### timeseries.stdfilt1

Standart deviation filter Package: timeseries Description: One dimensional StD filter.


    
    Standart deviation filter  
    Package: timeseries  
    Description: One dimensional StD filter.  
    Input  : - Vector on which to run the filter.  
    - Filter order, default is 3.  
    If order is odd then Y(k) is the std  
    of X( k-(N-1)/2 : k+(N-1)/2 ), else  
    it is the StD of X( k-N/2 : k+N/2-1 )  
    - BlockSize, by default its length of the input vector.  
    Use in case not enough computer memory.  
    If empty matrix then use default.  
    - If this parameter is provided then use quantile instaed  
    of std, where the parameter specify the fraction of data  
    to be in the returned range. For example 0.6834 is analog  
    to one sigma.  
    If two elements vector is given than these are the lower  
    and upper quantile range.  
    Default is empty matrix (i.e., use std).  
    Output : - StD vector of the same length of the input vector.  
    Assuming the edges are equal to zero.  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Jun 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Y=timeseries.stdfilt1(rand(100,1),10,[],0.68);  
    Reliable: 2  
      
      
### timeseries.subtract_back1d

subtract_back1d function                                             General Description: Subtract background level from a 1-D vector.


    
      
    subtract_back1d function                                             General  
    Description: Subtract background level from a 1-D vector.  
    Input  : - Vector from which to subtract background.  
    If two columns matrix then the first column indicate position,  
    while the second column is for the value from which to subtract  
    the background.  
    - Algorithm:  
    'none'     - do not subtract background.  
    'mean'     - subtract the mean.  
    'median'   - subtract the median.  
    'medfilt'  - subtract a median filter, where the block size  
    is provided by the next input argument.  
    * Additional parameters to pass to the subtraction algorithm.  
    Output : - A background subtracted vector.  
    If the input is a two column matrix than the output will  
    be a two column matrix.  
    Tested : Matlab R2012A  
    By : Eran O. Ofek                    Oct 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: VecSub=subtract_back1d(rand(100,1),'median');  
    Reliable: 2  
      
      
### timeseries.sysrem

Apply the Tamuz et al. sysrem decomposition to a matrix of residuals Package: timeseries Description: Given a matrix of residuals (R_ij), of star i in image j, iteratively decompose the matrix by minimizing


    
    Apply the Tamuz et al. sysrem decomposition to a matrix of residuals  
    Package: timeseries  
    Description: Given a matrix of residuals (R_ij), of star i in image j,  
    iteratively decompose the matrix by minimizing  
    R'_ij = R_ij - C_i*A_j. The C and A vectors are free  
    parameters that are found by iterations.  
    Optionally, this can be applied iterativel several times -  
    e.g., R''_ij = R'_ij - C'_i * A'_j.  
    Input  : - Matrix of residuals (R_ij), of star i in image j.  
    - Matrix of errors in residuals (Sigma_ij), of star i in image  
    j. If scalar then will be applied to all observations.  
    Default is 1.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'A' - Guess vector of A to be used in the first iteration.  
    If scalar then will be duplicated into a vector.  
    Default is 1.  
    For Iter>1, 1 is used.  
    'C' - Guess vector of A to be used in the first iteration.  
    If scalar then will be duplicated into a vector.  
    Default is 0.  
    For Iter>1, 0 is used.  
    'ReNormSigma' - Renormalize Sigma matrix (before first  
    iterauion), such that the Chi^2 per dof will be 1.  
    {true | false}. Default is true.  
    'ThreshDeltaS2' - Convergence threshold (in units of chi^2).  
    Default is 1.  
    'Niter' - Number of iteration to apply sysrem. Default is 1.  
    Output : - The resulted S2 value (\chi^2).  
    - A structure array of length (Niter+1), that contains the  
    results from each iteration, The last element is for the final  
    iteration.  
    Available fields:  
    'Resid' - Matrix of new residuals.  
    'A'     - Vector A  
    'B'     - Vector B  
    'S2'    - \chi^2  
    'rms'   - std of Resid matrix.  
    'Ndof   - Number of degrees of freedom.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jul 2019  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [S2,Res]=timeseries.sysrem(R,1)  
    Reliable: 2  
      
      
### timeseries.taper

Generate a taper function Package: timeseries Description: Generate a taper function for a timeseries. Taper function is a weight/window function in the time domain.


    
    Generate a taper function  
    Package: timeseries  
    Description: Generate a taper function for a timeseries. Taper function is  
    a weight/window function in the time domain.  
    This can be used in order to give reduce weight to data  
    found near the edges of a timeseries in order to reduce  
    aliasing with the series length.  
    Input  : - A column vector of X (e.g., time). If multiple columns are  
    given, then the program will use the first column only.  
    * Arbitrary number of pairs of ...,key,val,...  
    The following keywords are available:  
    'TaperFun' - Name of taper function  
    'cosbell' - cosine bell (default).  
    'trapz'   - a trapzoid.  
    Alternatively, this can be a nuerical function  
    [X,Y] norzmlized that such X is in the range 0 to 1.  
    Alternatively, this can be a function handle.  
    Y=@fun(X), where X is in the rane 0 to 1.  
    'TaperPar' - Parameters of the taper function.  
    For 'cosbell' and 'trapz' this is the precentage  
    of flat part of the cosine bell (in the range 0..1).  
    Where the total range of the taper function is  
    the total range of X.  
    Default is 0.9.  
    If TaperFun is a function handle, this can be  
    a cell array of additional parameters that will be  
    passed to the function.  
    'Norm'     - {'y'|'n'}. Normalize the taper function such that  
    the maximum height of the taper is 1.  
    This is useful if TaperFun is an unnormalized  
    function. Default is 'n'.  
    'Y'        - Y value of the time series. If this parameter is  
    provided, then the output argument is the taper  
    function multiplied by the Y value.  
    Default is Y=ones(size(X));  
    'InterpMethod' - Interpolation method to be used if the taper  
    function is numeric. See interp1.m for options.  
    Default is 'linear'.  
    Output - The value of the taper function for each X value in the input  
    vector. If The keyword 'Y' is provided then this will be the  
    taper function multiply by the Y value.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Jan 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X = (100:1:200)';  
    TaperVal=timeseries.taper(X);  
    TaperVal=timeseries.taper(X,'TaperPar',0.0);  
    TaperVal=timeseries.taper(X,'TaperPar',0.5,'TaperFun','trapz','Y',ones(size(X)).*2);  
    TaperVal=timeseries.taper(X,'TaperFun',@sin);  
    Reliable: 2  
      
      
### timeseries.xcorr

Calculate the \chi2 and cross correlation between two time series. Package: timeseries Description: Calculate the \chi2 and cross correlation between two time series.


    
    Calculate the \chi2 and cross correlation between two time series.  
    Package: timeseries  
    Description: Calculate the \chi2 and cross correlation between two time  
    series.  
    Input  : - First time series. Matrix of [Time, Mag, Error], where Error  
    is optional. If Error column is not provided then will be set  
    to 1. Note that Error is used only in the \chi2^2 calculation  
    and not for the cross-correlation.  
    - Second time series.  
    - Vector of lags to check.  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    'SubMeanChi2'- For each lag, subtract mean value of  
    difference between the two series.  
    Default is true.  
    'SubMeanCorr'- For each lag, subtract the mean of each series  
    before cross-correlation.  
    Default is true.  
    'ErrConf'    - Percentiles for condidence intervals.  
    Default is 1 - 2.*normcdf([1;2;3],0,1,'upper')  
    'ColT'       - Time column in input time series. Default is 1.  
    'ColM'       - Mag column in input time series. Default is 2.  
    'ColE'       - Err column in input time series. Default is 3.  
    'InterpMethod'- Series interpolation method.  
    Default is 'linear'.  
    'InterpMethodErr'- Error interpolation method.  
    Default is 'nearest'.  
    Output : - A matrix with [Lag, Chi2, Dof, cross-correlation].  
    - A structure with the following information:  
    'Npar'      - Number of free parameters (2).  
    'MeanDiffM' - Vector of the Mean difference between the two  
    series for each lag.  
    'MinChi2'   - Minimum \chi^2 value.  
    'MinChi2Ind'- Lag index of min \chi^2.  
    'MinChi2Lag'- Lag of min \chi^2.  
    'MinDof'    - Dof at min \chi^2.  
    'MaxCorr'   - Maximum cross-correlation.  
    'MaxCorrInd'- Lag index of max cross-correlation.  
    'MaxCorrLag'- Lag of max cross-correlation.  
    'DeltaChi2'  
    'AdjustedChi2'  
    'CI'  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Jun 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [T,F,TotalF,X,Y]=AstroUtil.lensing.generate_timedelay_lc;  
    TS1=[T,F(:,1)];  
    TS2=[T,F(:,2)];  
    [XC,R]=timeseries.xcorr(TS1,TS2,(-100:1:100)');  
    Reliable: 2  
      
      
