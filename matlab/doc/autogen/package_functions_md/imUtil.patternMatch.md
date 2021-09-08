# Package: imUtil.patternMatch


### imUtil.patternMatch.find_registration_trans

Example: imUtil.patternMatch.find_registration_trans


    
      
    Example: imUtil.patternMatch.find_registration_trans  
      
      
### imUtil.patternMatch.find_rot_pairs

Find the best rotation required to match two catalogs using dist-rot hist Package: imUtil.patternMatch Description: Find the best rotation required to match two catalogs using The Kaiser method. The histogram of the distance and


    
    Find the best rotation required to match two catalogs using dist-rot hist  
    Package: imUtil.patternMatch  
    Description: Find the best rotation required to match two catalogs using  
    The Kaiser method. The histogram of the distance and  
    angle between stars in the reference is cross correlated  
    with the same histogram for the reference.  
    This function will return at least two solutions, where one  
    of the solutions is wrong.  
    Input  : - A catalog.  
    If no arguments are provided, then run in simulation mode.  
    - A reference catalog.  
    * Pairs of ...,key,val,... The following keywords are avaialble:  
    'Flip' - A two column matrix of all possible flips to test.  
    Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].  
    Default is [1 1; 1 -1].  
    This default covers all options.  
    'HistDistEdges' - Edges for distance axis of the histogram.  
    Default is (12:3:300).'.  
    'HistRotEdges' - Edges for angle axis of the histogram.  
    Default is (-90:0.2:90).'.  
    'MaxMethod' - The method by which the 2D histogram peaks will  
    be selected. The following options are available:  
    'thresh' - Select maxima larger than some threshold.  
    'max1' - Select the highest maxima.  
    'maxall' - Select all the maxima.  
    'max_fracmax' - Select all the maxima above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    'thresh_fracmax' - Select all the maxima above the  
    threshold and above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    Alternatively this can be a positive integer (N).  
    In this case will return the N highest maxima.  
    Default is 'thresh_fracmax'  
    'Threshold' - Peak selection threshold  
    Default is 5.  
    'FracOfMax' - The parameter that that used in '*_fracmax' and  
    for selecting peaks.  
    Only peaks that above the maximal peak multiplied by  
    this parameter will be returned.  
    'CatColX' - Catalog column that contains the X axis. Default is 1.  
    'CatColY' - Catalog column that contains the Y axis. Default is 2.  
    'RefColX' - Reference column that contains the X axis. Default is 1.  
    'RefColY' - Reference column that contains the Y axis. Default is 2.  
    Output : - A structure containing the following fields:  
    'SN' - A vector of the S/N of all selected  
    cross-correkation peaks.  
    'Rot' - A vector of all rotation angles of all selected  
    cross-correlation peaks [deg].  
    This is is the angle by which one should rotate Ref, in  
    order to get Cat.  
    Apply this transformation using  
    imUtil.cat.affine2d_transformation(Ref,Tr,'+');  
    'Flip' - A two column matrix [FlipX, FlipY] corresponding to  
    the flips of all selected peaks. This is is the flip  
    needed to be applied to Ref, in  
    order to get Cat.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=imUtil.patternMatch.find_rot_pairs;  
    Reliable: 2  
      
      
### imUtil.patternMatch.find_scalerot_pairs

Find rotation and scale required to match two catalogs using logdist-rot hist Package: imUtil.patternMatch Description: Find the best rotation and scale and flip (by that order) needed to be applied to a acatalog in order to match it


    
    Find rotation and scale required to match two catalogs using logdist-rot hist  
    Package: imUtil.patternMatch  
    Description: Find the best rotation and scale and flip (by that order)  
    needed to be applied to a acatalog in order to match it  
    to a reference catalog.  
    The algorithm is using the Kaiser method. The histogram of  
    the log-distance and angle between stars in the reference is  
    cross correlated with the same histogram for the reference.  
    Input  : - A catalog.  
    If no arguments are provided, then run in simulation mode.  
    - A reference catalog.  
    * Pairs of ...,key,val,... The following keywords are avaialble:  
    'Flip' - A two column matrix of all possible flips to test.  
    Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].  
    Default is [1 1; 1 -1].  
    This default covers all options.  
    'Scale' - Range of allowed scale. Default is [0 Inf].  
    'HistDistEdges' - [MinDist, MaxDist, NumberOf points] for  
    distance histogram.  
    Default is [10 600 300].  
    'HistRotEdges' - Edges for angle axis of the histogram.  
    Default is (-90:0.2:90).'.  
    'MaxMethod' - The method by which the 2D histogram peaks will  
    be selected. The following options are available:  
    'thresh' - Select maxima larger than some threshold.  
    'max1' - Select the highest maxima.  
    'maxall' - Select all the maxima.  
    'max_fracmax' - Select all the maxima above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    'thresh_fracmax' - Select all the maxima above the  
    threshold and above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    Alternatively this can be a positive integer (N).  
    In this case will return the N highest maxima.  
    Default is 'thresh_fracmax'  
    'Threshold' - Peak selection threshold  
    Default is 5.  
    'FracOfMax' - The parameter that that used in '*_fracmax' and  
    for selecting peaks.  
    Only peaks that above the maximal peak multiplied by  
    this parameter will be returned.  
    'CatColX' - Catalog column that contains the X axis. Default is 1.  
    'CatColY' - Catalog column that contains the Y axis. Default is 2.  
    'RefColX' - Reference column that contains the X axis. Default is 1.  
    'RefColY' - Reference column that contains the Y axis. Default is 2.  
    Output : - A structure containing the following fields:  
    'SN'  - A vector of the S/N of all selected  
    cross-correkation peaks.  
    'Rot' - A vector of all rotation angles of all selected  
    cross-correkation peaks.  
    The rotation angle is in the range [0 180] deg.  
    The full range is achived by a combination of Flip and Rot.  
    'Scale' - A vector of scales of all selected  
    cross-correkation peaks.  
    'Flip' - A two column matrix [FlipX, FlipY] corresponding to  
    the flips of all selected peaks.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=imUtil.patternMatch.find_scalerot_pairs;  
    Reliable: 2  
      
      
### imUtil.patternMatch.find_shift_pairs

find best X/Y shift between two catalogs from all pairs differences Package: imUtil.patternMatch Description: Find best X/Y shift between two catalogs that contains [X,Y] positions. The shifts are identified by calculating the


    
    find best X/Y shift between two catalogs from all pairs differences  
    Package: imUtil.patternMatch  
    Description: Find best X/Y shift between two catalogs that contains [X,Y]  
    positions. The shifts are identified by calculating the  
    histogram of all the X differences between the two catalogs  
    and the Y differences between the two catalogs.  
    Next, search for peaks in the 2D histogram using various  
    methods, and return all the peaks that satisfy some  
    selection criteria.  
    The returned shifts are the offset one need to add to the  
    Catalog in order to get the Referece.  
    If flip is applied, then the flip need to be applied on the  
    catalog before the shift is applied.  
    Input  : - A catalog. A matrix in which two columns (default is 1 and 2)  
    contains the X and Y position of the sources.  
    If no parameters are provided, then will run in  
    simulation/test mode.  
    - A reference catalog. A matrix in which two columns  
    (default is 1 and 2)  
    contains the X and Y position of the sources.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'Flip' - A two column matrix of all possible flips to test.  
    Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].  
    Default is [1 1].  
    'RangeX' - [Min Max] range of X shifts to test.  
    If empty, then will select automatically the maximal  
    possible range.  
    Alternatively, this can be a vector of X-axis  
    histogram edges.  
    Default is [-1000 1000].  
    'RangeY' - [Min Max] range of Y shifts to test.  
    If empty, then will select automatically the maximal  
    possible range.  
    Alternatively, this can be a vector of Y-axis  
    histogram edges.  
    Default is [-1000 1000].  
    'StepX' - X-axis step size for the histogram calculation.  
    If empty, then will use RangeX as a vector of edges.  
    Default is 3.  
    'StepY' - Y-axis step size for the histogram calculation.  
    If empty, then will use RangeY as a vector of edges.  
    Default is 3.  
    'MaxMethod' - he method by which the 2D histogram peaks will  
    be selected. The following options are available:  
    'thresh' - Select maxima larger than some threshold.  
    'max1' - Select the highest maxima.  
    'maxall' - Select all the maxima.  
    'max_fracmax' - Select all the maxima above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    'thresh_fracmax' - Select all the maxima above the  
    threshold and above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    Alternatively this can be a positive integer (N).  
    In this case will return the N highest maxima.  
    Default is 'thresh_fracmax'  
    'Threshold' - Detection threshold.  
    If PeakMethod is 'sn' then this has units of S/N.  
    Otherwise, this is the number of matches.  
    Default is 8.  
    'Conn' - local maxima finding connectivity parameter.  
    For details see imUtil.image.local_maxima.  
    Default is 8.  
    'FracOfMax' - The parameter that that used in 'max1frac' and  
    'sn' PeakMethod, for selecting peaks.  
    Only peaks that above the maximal peak multiplied by  
    this parameter will be returned.  
    'BackFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @median.  
    'BackFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {'all','omitnan'}.  
    'VarFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @imUtil.background.rvar.  
    'VarFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {}.  
    'SubSizeXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [128 128].  
    'Overlap' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [16 16].  
    'MinVariance' - The minimum variance in in the 2D histogram,  
    That is used to calculate the S/N.  
    Default is 1.  
    'FilterSigma' - Width [sigma units] of Gaussian filter with  
    which to cross-correlate the H2 (hits) matrix.  
    If empty, no filtering is applied. Default is 3.  
    'CatColX' - Catalog column that contains the X axis. Default is 1.  
    'CatColY' - Catalog column that contains the Y axis. Default is 2.  
    'RefColX' - Reference column that contains the X axis. Default is 1.  
    'RefColY' - Reference column that contains the Y axis. Default is 2.  
    Output : - A structure that contains a list of all possible shifts that  
    can explain the data. The following fields are available:  
    'ShiftX' - A vector of X shifts one need to add to the  
    reference  
    in order to get the catalog (after the flip is  
    applied to the reference).  
    'ShiftY' - A vector of Y shifts one need to add to the  
    reference  
    in order to get the catalog (after the flip is  
    applied to the reference).  
    'MaxVal' - Number of matched sources found in the histogram  
    peak. One entry per source.  
    'SN'    - S/N for each peak.  
    'Flip'  - A two column matrix of flip [X,Y] corresponding to  
    each peak. This flip needed to be applied to Ref in  
    order to get Cat.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=imUtil.patternMatch.find_shift_pairs;  
    Reliable: 2  
      
      
      
### imUtil.patternMatch.find_shift_xc_bincat

find best X/Y shift between two catalogs from xcorr binned catalogs Package: imUtil.patternMatch Description: Find best X/Y shift between two catalogs that contains [X,Y] positions. The shifts are identified by calculating the


    
    find best X/Y shift between two catalogs from xcorr binned catalogs  
    Package: imUtil.patternMatch  
    Description: Find best X/Y shift between two catalogs that contains [X,Y]  
    positions. The shifts are identified by calculating the  
    histogram of each catalog (into an effective image), and  
    cross correlating the two histograms.  
    Next, search for peaks in the 2D histogram using various  
    methods, and return all the peaks that satisfy some  
    selection criteria.  
    The returned shifts are the offset one need to add to the  
    Catalog in order to get the Referece.  
    If flip is applied, then the flip need to be applied on the  
    catalog before the shift is applied.  
    Input  : - A catalog. A matrix in which two columns (default is 1 and 2)  
    contains the X and Y position of the sources.  
    If no parameters are provided, then will run in  
    simulation/test mode.  
    - A reference catalog. A matrix in which two columns  
    (default is 1 and 2)  
    contains the X and Y position of the sources.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'Flip' - A two column matrix of all possible flips to test.  
    Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].  
    Default is [1 1].  
    'RangeX' - [Min Max] range of X shifts to test.  
    If empty, then will select automaticall the maximal  
    possible range.  
    Alternatively, this can be a vector of X-axis  
    histogram edges.  
    Default is [-1000 1000].  
    'RangeY' - [Min Max] range of Y shifts to test.  
    If empty, then will select automaticall the maximal  
    possible range.  
    Alternatively, this can be a vector of Y-axis  
    histogram edges.  
    Default is [-1000 1000].  
    'StepX' - X-axis step size for the histogram calculation.  
    If empty, then will use RangeX as a vector of edges.  
    Default is 3.  
    'StepY' - Y-axis step size for the histogram calculation.  
    If empty, then will use RangeY as a vector of edges.  
    Default is 3.  
    'MaxMethod' - he method by which the 2D histogram peaks will  
    be selected. The following options are available:  
    'thresh' - Select maxima larger than some threshold.  
    'max1' - Select the highest maxima.  
    'maxall' - Select all the maxima.  
    'max_fracmax' - Select all the maxima above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    'thresh_fracmax' - Select all the maxima above the  
    threshold and above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    Alternatively this can be a positive integer (N).  
    In this case will return the N highest maxima.  
    Default is 'thresh_fracmax'  
    'Threshold' - Detection threshold.  
    If PeakMethod is 'sn' then this has units of S/N.  
    Otherwise, this is the number of matches.  
    Default is 8.  
    'Conn' - local maxima finding connectivity parameter.  
    For details see imUtil.image.local_maxima.  
    Default is 8.  
    'FracOfMax' - The parameter that that used in 'max1frac' and  
    'sn' PeakMethod, for selecting peaks.  
    Only peaks that above the maximal peak multiplied by  
    this parameter will be returned.  
    'BackFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @median.  
    'BackFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {'all','omitnan'}.  
    'VarFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @imUtil.background.rvar.  
    'VarFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {}.  
    'SubSizeXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [128 128].  
    'OverlapXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [16 16].  
    'MinVariance' - The minimum variance in in the 2D histogram,  
    That is used to calculate the S/N.  
    Default is 1.  
    'CatColX' - Catalog column that contains the X axis. Default is 1.  
    'CatColY' - Catalog column that contains the Y axis. Default is 2.  
    'RefColX' - Reference column that contains the X axis. Default is 1.  
    'RefColY' - Reference column that contains the Y axis. Default is 2.  
    Output : - A structure that contains a list of all possible shifts that  
    can explain the data. The following fields are available:  
    'MaxX' - A vector of X shifts one need to add to the catalog  
    in order to get the reference (after the flip is  
    applied to the catalog).  
    'MaxY' - A vector of Y shifts one need to add to the catalog  
    in order to get the reference (after the flip is  
    applied to the catalog).  
    'MaxVal' - Number of matched sources found in the histogram  
    peak. One entry per source.  
    'MaxSN'   - S/N for each peak.  
    'Flip'  - A two column matrix of flip [X,Y] corresponding to  
    each peak.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=imUtil.patternMatch.find_shift_pairs;  
    Reliable: 2  
      
      
      
### imUtil.patternMatch.fit_astrometric_tran

Fit astrometric transformation Package: +imUtil.patternMatch Description: Fit astrometruc transformation to two matched catalogs. Xref = a_0 + a_1*Xcat + a_2*Ycat + ...


    
    Fit astrometric transformation  
    Package: +imUtil.patternMatch  
    Description: Fit astrometruc transformation to two matched catalogs.  
    Xref = a_0 + a_1*Xcat + a_2*Ycat + ...  
    + b_0.*Ccat + b_1*Xcat*Ccat + b_2*Ycat*Ccat + ...  
    Input  : - Matrix containing catalog with [X,Y,[Mag]].  
    - Reference matrix catalog with at least 2 columns.  
    Mandatory columns are [X,Y] and optional columns are  
    [Mag, Color, AirMass, ParallacticAngle, ModulusXpix,  
    ModulusYpix].  
    * Pairs of ...,key,val,... arguments.  
    The possible keywords are:  
    'FunX'  
    'FunY'  
    'FunNX'  
    'NunNY'  
    'Tran'  
    'MaxIter'  
    'ErrPos' -  
    'Norm'  
    'FitMethod'  
    'Algo'  
    'ColCatX'  
    'ColCatY'  
    'ColCatM'  
    'ColRefX'  
    'ColRefY'  
    'ColRefM'  
    'ColRefC'  
    'ColRefAM'  
    'ColRefPA'  
    'ColRefModX'  
    'ColRefModY'  
    Parameters of resid_vs_mag  
    'BinMethod' - Method to use:  
    'poly' - polynomial fit.  
    'bin' - binning the data.  
    Default is 'bin'  
    'PolyDeg' - Polynomial degree for the polynomial fit.  
    Default is 3.  
    'BinSize' - Bin size for binning. Default is 1 (mag).  
    'FunMean' - A function handle to use when calculating the mean  
    of the data in each bin.  
    Default is @nanmedian.  
    'FunStd' - A function handle to use when calculating the std  
    of the data in each bin, or when calculating the global  
    std after the polynomial fit.  
    Default is @imUttil.background.rstd.  
    'InterpMethod' - Interpolation method. Default is 'linear'.  
    'ThresholdSigma' - Threshold in sigmas (std) for flagging good  
    data. Default is 3.  
    Output : -  
    Example:  
    [Param,Res,ResLoop]=imUtil.patternMatch.fit_astrometric_tran;  
      
### imUtil.patternMatch.fit_astrometry

Package: imUtil.patternMatch Description:


    
      
    Package: imUtil.patternMatch  
    Description:  
    Input  : -  
    * Pairs of ...,key,val,... Possible keywords include:  
      
    Output : - A vector of logicals indicating the selected peaks.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=imUtil.patternMatch.fit_astrometry;  
    Reliable: 2  
      
### imUtil.patternMatch.hist2d

calculate the 2-D histogram of 2-D data set. Package: imUtil.patternMatch Description: calculate the 2-D histogram of 2-D data set.


    
    calculate the 2-D histogram of 2-D data set.  
    Package: imUtil.patternMatch  
    Description: calculate the 2-D histogram of 2-D data set.  
    Input  : - Vector of X coordinates.  
    - Vector of Y coordinates.  
    - If 4 input arguments are provided, then this is the vector of  
    X edges in which the histogram will be calculated with.  
    If 6 input arguments are provided, then this is the X-axis  
    range of the histogram [min max].  
    - If 4 input arguments are provided, then this is the vector of  
    Y edges in which the histogram will be calculated with.  
    If 6 input arguments are provided, then this is the Y-axis  
    range of the histogram [min max].  
    - Step size of X histogram.  
    - Step size of Y histogram.  
    Output : - 2-D histogram  
    - Vector of X coordinate of center of X bins.  
    - Vector of Y coordinate of center of Y bins.  
    - The index array BinX (see histcounts2).  
    - The index array BinY (see histcounts2).  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Feb 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Xv=rand(100000,1).*2; Yv=rand(100000,1).*3+100; Mat=imUtil.patternMatch.hist2d(Xv,Yv,[0 2],[100 103],0.1,0.1);  
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
      
      
### imUtil.patternMatch.match_scale_rot_shift

Affine transformation matching of the coordinate systems of two catalogs Package: imUtil.patternMatch Description: Given two catalogs that coordinadte systems are related by flip, scale, rotation and shift, search the the approximate


    
    Affine transformation matching of the coordinate systems of two catalogs  
    Package: imUtil.patternMatch  
    Description: Given two catalogs that coordinadte systems are related  
    by flip, scale, rotation and shift, search the the approximate  
    affine transformation  
    that is required in order to align the coordinate systems of  
    the two catalogs. The search is done by matching patterns in  
    one catalog to the other.  
    Input  : - A catalog that contains Xcat,Ycat coordinates.  
    Alternatively, if only one argument is sprovided then will run  
    in simulation mode.  
    - A referece catalog that contains Xref,Yref coordinates.  
    Xcat/Ycat and Xref/Yref are related by flip, scaling, rotation  
    and shift.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'Scale' - The scale to apply to the reference catalog in order  
    to convert it to the input catalog. If this is a  
    scalar, then will not attempt to search for the best  
    scaleing. If a two element vector, then these are the  
    [min, max] scale range to search.  
    'HistDistEdgesRotScale' - [MinDist, MaxDist, NumberOf points] for  
    distance histogram.  
    Default is [10 600 300].  
    'HistRotEdges' - Edges for angle axis of the histogram.  
    If a scalar is provided then this rotation will be  
    assumed.  
    Default is (-90:0.2:90).'.  
    'RangeX' - [Min Max] range of X shifts to test.  
    If empty, then will select automaticall the maximal  
    possible range.  
    Alternatively, this can be a vector of X-axis  
    histogram edges.  
    Default is [-1000 1000].  
    'RangeY' - [Min Max] range of Y shifts to test.  
    If empty, then will select automaticall the maximal  
    possible range.  
    Alternatively, this can be a vector of Y-axis  
    histogram edges.  
    Default is [-1000 1000].  
    'StepX' - X-axis step size for the histogram calculation.  
    If empty, then will use RangeX as a vector of edges.  
    Default is 3.  
    'StepY' - Y-axis step size for the histogram calculation.  
    If empty, then will use RangeY as a vector of edges.  
    Default is 3.  
    'Flip' - A two column matrix of all possible flips to test.  
    Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].  
    Default is [1 1].  
    'SearchRadius' - Searchj radius for final source matching  
    [pix]. Default is 4.  
    'MaxMethod' - he method by which the 2D histogram peaks will  
    be selected. The following options are available:  
    'thresh' - Select maxima larger than some threshold.  
    'max1' - Select the highest maxima.  
    'maxall' - Select all the maxima.  
    'max_fracmax' - Select all the maxima above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    'thresh_fracmax' - Select all the maxima above the  
    threshold and above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    Alternatively this can be a positive integer (N).  
    In this case will return the N highest maxima.  
    Default is 'thresh_fracmax'  
    'Threshold' - Detection threshold.  
    If PeakMethod is 'sn' then this has units of S/N.  
    Otherwise, this is the number of matches.  
    Default is 8.  
    'Conn' - local maxima finding connectivity parameter.  
    For details see imUtil.image.local_maxima.  
    Default is 8.  
    'FracOfMax' - The parameter that that used in 'max1frac' and  
    'sn' PeakMethod, for selecting peaks.  
    Only peaks that above the maximal peak multiplied by  
    this parameter will be returned.  
    'BackFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @median.  
    'BackFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {'all','omitnan'}.  
    'VarFun' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is @imUtil.background.rvar.  
    'VarFunPar' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is {}.  
    'SubSizeXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [128 128].  
    'OverlapXY' - Used for 'sn' PeakMethod.  
    For details see imUtil.background.background.  
    Default is [16 16].  
    'MinVariance' - The minimum variance in in the 2D histogram,  
    That is used to calculate the S/N.  
    Default is 1.  
    'FilterSigma' - Width [sigma units] of Gaussian filter with  
    which to cross-correlate the H2 (hits) matrix.  
    If empty, no filtering is applied. Default is 3.  
    'CatColX' - Catalog column that contains the X axis. Default is 1.  
    'CatColY' - Catalog column that contains the Y axis. Default is 2.  
    'RefColX' - Reference column that contains the X axis. Default is 1.  
    'RefColY' - Reference column that contains the Y axis. Default is 2.  
    Output : - A structure of possible solutions for matching between the two  
    catalogs. Follwoing fields are available:  
    .SN  
    .MaxVal  
    .Flip  
    .Rot  
    .Scale  
    .ShiftX - The shift in X one need to add to Ref in order to  
    get Cat.  
    .ShiftY - The shift in Y one need to add to Ref in order to  
    get Cat.  
    .AffineTran - A cell array of affine matrix transformations  
    Apply this transformation to Ref using imUtil.cat.affine2d_transformation  
    In order to get Cat.  
    This is the rotation transformation for the reference frame  
    and not the reference coordinates.  
    - A structure of matching steps, including ResShift and ResRot.  
    ResShift is documented in  
    imUtil.patternMatch.find_shift_pairs, while ResRot in imUtil.patternMatch.find_rot_pairs  
    or imUtil.patternMatch.find_scalerot_pairs.  
    - A structure containing the matched sources for each solution.  
    The following fields are available:  
    'MatchedCat - [X,Y] of the sources in Cat matched to  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Sol,PrevStep,Matched]=imUtil.patternMatch.match_scale_rot_shift  
    Reliable:  
      
      
### imUtil.patternMatch.matched_cat_residuals

Calculate the astrometric residuals and errors between two matched catalogs Package: imUtil.patternMatch Description: Given a catalog (Cat) and a reference catalog (Ref), both containing astrometric X/Y measurments of matched sources


    
    Calculate the astrometric residuals and errors between two matched catalogs  
    Package: imUtil.patternMatch  
    Description: Given a catalog (Cat) and a reference catalog (Ref), both  
    containing astrometric X/Y measurments of matched sources  
    (i.e., each line in Cat corresponds to the same line in  
    Ref), calculate the astrometric residuals.  
    If a mgnitude vector (per source) is given, then calculate  
    an histogram (or fit a polynomial) of the residuals as a  
    function of magnitude. The median residal at each magnitude  
    is returned as the assumed astrometric error for each  
    source.  
    Input  : - A catalog (at least two column matrix).  
    - A reference catalog (at least two column matrix).  
    * Pairs of ...,key,val,... Possible keywords include:  
    'Mag' - An optional column containing the magnitude for each  
    source.  
    If empty, then calculate only the std of the residauls.  
    'MagMethod' - Method by which to fit the residuals.  
    Options are:  
    'poly' - Fit a polynomial to residuals vs. mag in two  
    iteartions.  
    'hist' - calculate an histogram.  
    Default is 'hist'.  
    'HistBin' - Bin size for hsitogram. Default is 0.5 mag.  
    'HistRange' - Gistogram range [low high]. If [NaN NaN] use  
    min, max of magnitudes. Default is [NaN NaN].  
    'PolyOrder - Polynomial order to fit. Default is 3.  
    'Nsigma'   - Number of sigma for sigma clipping in polynomial  
    fitting. Default is 3.  
    'InterpMethod' - Interpolation method when calcualting the  
    error as a function of mag.  
    Default is 'linear'.  
    'CatColX' - Catalog column that contains the X axis. Default is 1.  
    'CatColY' - Catalog column that contains the Y axis. Default is 2.  
    'RefColX' - Reference column that contains the X axis. Default is 1.  
    'RefColY' - Reference column that contains the Y axis. Default is 2.  
    Output : - A structure containing the following fields:  
    'Err' - Vector of errors per source, as interpolated from the  
    residuals vs. magnitude.  
    'MinErr' - Min of Err.  
    'ResidStD' - StD of Resid vector.  
    'Resid' - Vector of residuals per source.  
    'DX'    - Residuals in X-axis per source.  
    'DY'    - Residuals in Y-axis per source.  
    'Chi2'  - \chi^2  
    'Nobs'  - Number of observations.  
    'Number of degrees of freedom.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=imUtil.patternMatch.matched_cat_residuals(rand(100,2),rand(100,2),'Mag',rand(100,1).*10)  
    Reliable: 2  
      
      
### imUtil.patternMatch.prep_cat_for_astrometry

Clean two catalogs and equalize their surface density Package: imUtil.patternMatch Description: Given two catalogs (e.g., Cat and Ref), clean the catalogs by removing NaN coordinates,


    
    Clean two catalogs and equalize their surface density  
    Package: imUtil.patternMatch  
    Description: Given two catalogs (e.g., Cat and Ref), clean the catalogs  
    by removing NaN coordinates,  
    imUtil.cat.flag_overdense_colrow, imUtil.cat.flag_overdense,  
    estimate their density using imUtil.cat.surface_density, and  
    equalize their surface density by removing the faintest  
    sources in one of the catalogs using  
    imUtil.cat.dilute_cat_by_mag  
    Input  : - A catalog with [X,Y,Mag] columns.  
    - A Ref catalog wiyh [X,Y,Mag] columns.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'CatRemoveNaN' - A logical indicating if to remove NaN  
    coordinates from the Cat. Default is true.  
    'CatRemoveBadColRow' - A logical indicating if to remove  
    source in overdense columns/rows  
    from the Cat. Default is true.  
    'CatRemoveOverDense' - A logical indicating if to remove  
    source in overdense regions  
    from the Cat. Default is true.  
    'RefRemoveNaN'  - A logical indicating if to remove NaN  
    coordinates from the Ref. Default is false.  
    'RefRemoveBadColRow' - A logical indicating if to remove  
    source in overdense columns/rows  
    from the Ref. Default is true.  
    'RefRemoveOverDense' - A logical indicating if to remove  
    source in overdense regions  
    from the Ref. Default is false.  
    'EqualizeDensity' - A logical indicating if to equalize the  
    surface density of the two catalogs.  
    'DiluteThreshold' - If the surface density of the Ref minus  
    Cat divided by Cat (abs value) is larger than this  
    factor then applay source diluation.  
    Default is 0.5.  
    'ColRowPar' - A cell array of addotional parameters to pass to  
    imUtil.cat.flag_overdense_colrow.  
    Default is {}.  
    'OverdensePar' - A cell array of addotional parameters to pass to  
    imUtil.cat.flag_overdense  
    Default is {}.  
    'CatHalfSize' - Either radius, or [half width, half height] of  
    the Cat catalog. If empty, then estimate area using  
    convex hull. Default is empty.  
    'RefHalfSize' - Either radius, or [half width, half height] of  
    the Ref catalog. If empty, then estimate area using  
    convex hull. Default is empty.  
    'ColCatX' - X coordinates column index in the Cat.  
    Default is 1.  
    'ColCatY' - Y coordinates column index in the Cat.  
    Default is 2.  
    'ColCatMag' - Mag column index in the Cat.  
    Default is 3.  
    'ColRefX' - X coordinates column index in the Ref.  
    Default is 1.  
    'ColRefY' - Y coordinates column index in the Ref.  
    Default is 2.  
    'ColRefMag' - Mag column index in the Ref.  
    Default is 3.  
    Output : - A new clean/diluted catalog.  
    - A new clean/diluted Ref catalog.  
    - A vector of logicals indicating the selected sources in Cat.  
    - A vector of logicals indicating the selected sources in Ref.  
    - Summary of number of sources survived after each step.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example:  
    [Cat,Ref]=imUtil.patternMatch.prep_cat_for_astrometry(rand(100,3).*1000,rand(200,3).*1000);  
    Reliable: 2  
      
      
### imUtil.patternMatch.refine_fit

Match sources in two catalogs and fit a positional transformation Package: imUtil.patternMatch Description: Given two catalogs of sources that have rougghly the same coordinate system (e.g., images are aligned to a few pixels


    
    Match sources in two catalogs and fit a positional transformation  
    Package: imUtil.patternMatch  
    Description: Given two catalogs of sources that have rougghly the same  
    coordinate system (e.g., images are aligned to a few pixels  
    lebvel), match the sources, and then fit a transformation  
    between the Reference coordinate system to the Catalog  
    system.  
    This function calls: imUtil.cat.match_sources and  
    imUtil.cat.fit_astrometric_tran  
    Input  : - A catalog.  
    - A reference catalog.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'SearchRadius' - Search radius for matching.  
    Default is 3.  
    'IsSpherical' - Indicating if catalogs are in sphereical  
    coordinates [Long,Lat] (true) or planner (false).  
    Default is false.  
    'ErrPos' - A vector of positional errors in each axis for  
    the sources. Default is 1e-5.  
    'MatchPar' - A cell array of additional parameters to pass to  
    imUtil.cat.match_sources.  
    Default is {}.  
    'FitPar' - A cell array of additional parameters to pass to  
    imUtil.cat.fit_astrometric_tran  
    Default is {}.  
    'CatColX' - Column for X/Long coordinates in Cat.  
    Default is 1.  
    'CatColY' - Column for Y/Lat coordinates in Cat.  
    Default is 2.  
    'RefColX' - Column for X/Long coordinates in Ref.  
    Default is 1.  
    'RefColY' - Column for Y/Lat coordinates in Ref.  
    Default is 2.  
    Output : - A structure with the transformation parameters  
    See imUtil.patternMatch.fit_astrometric_tran for details.  
    - A structure with the best fit residuals information.  
    See imUtil.patternMatch.fit_astrometric_tran for details.  
    - The matched catalog.  
    - The matched Ref.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Param,Res,MatchedCat,MatchedRef]=imUtil.patternMatch.refine_fit(Cat,Ref)  
    Reliable: 2  
      
      
### imUtil.patternMatch.select_maxima

Select some maxima out of list by some criteria Package: imUtil.patternMatch Description: Given a list of maxima, select a sub list by some criteria. For example, can be used to select N highest peaks, or all


    
    Select some maxima out of list by some criteria  
    Package: imUtil.patternMatch  
    Description: Given a list of maxima, select a sub list by some criteria.  
    For example, can be used to select N highest peaks, or all  
    peaks above some threshold, and larger than some fraction of  
    the highest peak.  
    Input  : - A vector of maxima values.  
    * Pairs of ...,key,val,... Possible keywords include:  
    'MaxMethod' - The method by which the 2D histogram peaks will  
    be selected. The following options are available:  
    'thresh' - Select maxima larger than some threshold.  
    'max1' - Select the highest maxima.  
    'maxall' - Select all the maxima.  
    'max_fracmax' - Select all the maxima above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    'thresh_fracmax' - Select all the maxima above the  
    threshold and above a  
    fraction (given by FracOfMax) of the  
    highest maximum.  
    Alternatively this can be a positive integer (N).  
    In this case will return the N highest maxima.  
    Default is 'thresh'  
    'Threshold' - Peak selection threshold  
    Default is 5.  
    'FracOfMax' - The parameter that that used in '*_fracmax' and  
    for selecting peaks.  
    Only peaks that above the maximal peak multiplied by  
    this parameter will be returned.  
    Output : - A vector of logicals indicating the selected peaks.  
    License: GNU general public license version 3  
    Tested : Matlab R2015b  
    By : Eran O. Ofek                    Apr 2016  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Flag=imUtil.patternMatch.select_maxima(randn(100,1).*2);  
    Reliable: 2  
      
      
