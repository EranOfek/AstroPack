# Package: lcUtil


### lcUtil.fitMotion

fit Proper motion in RA/Dec to coordinates in MatchedSources object. Given a MatchedSources object with RA/Dec fields, fit proper motions using the celestial.pm.fitMultiProperMotion function.


    
    fit Proper motion in RA/Dec to coordinates in MatchedSources object.  
    Given a MatchedSources object with RA/Dec fields, fit proper  
    motions using the celestial.pm.fitMultiProperMotion function.  
    Input  : - A MatchedSources Object.  
    * ...,key,val,...  
    'MinNobs' - minimum number of data points required for fit  
    (used only when the number of observations is not  
    the same for all sources).  
    Default is 5.  
    'Prob' - Vector of probabilities for which to calculate  
    the probably difference between H1 and H0.  
    Default is [1e-3 1e-5].  
    'Units' - Units of RA/Dec. This is used only for the the  
    checking that the RA data is not crossing zero.  
    Default is 'deg'.  
    'RenormErr' - A logical flag inducating if to normalize  
    the chi2 for the H1 hypothesis to 1.  
    Default is true.  
    Output : - A structure with the following fields:  
    .MeanT - Mean epoch relative to which the fit is done.  
    .RA.ParH1 - Parameters for H1 [pos; vel] for each source.  
    .RA.ParH0 - Parameters for H0 [pos] for each source.  
    .RA.Chi2_H1 - \chi^2 for H1  
    .RA.Chi2_H0 - \chi^2 for H0  
    .RA.Nobs - Number of observations for each source.  
    .RA.DeltaChi2 - \Delta\chi^2 between H0 and H1.  
    .RA.FlagH1 - A logical indicating if the source has a  
    prefreed H1 model over H0. each line, for each  
    requested proabability.  
    .RA.StdResid_H1 - std of H1 residuals.  
    .RA.StdResid_H0 - std of H0 residuals.  
    .Dec. - the same as .RA, but for the Dec axis.  
    Author : Eran Ofek (Sep 2021)  
    Example: Time=(1:1:20)'; RA = randn(20,1e5)./(3600.*100); Dec=randn(20,1e5)./(3600.*200);  
    RA(:,1) = (0.1:0.1:2)'./(3600);  
    Obj = MatchedSources;  
    Obj.addMatrix(RA,'RA');  
    Obj.addMatrix(Dec,'DEC');  
    Result = lcUtil.fitMotion(Obj);  
      
### lcUtil.fitPolyHyp

Hypothesis testing between fitting polynomials of various degrees to a matrix of light curves in a MatchedSources object (with unknown errors). Like timeseries.fitPolyHyp, but for a MatchedSources class.


    
    Hypothesis testing between fitting polynomials of various degrees to  
    a matrix of light curves in a MatchedSources object (with unknown errors).  
    Like timeseries.fitPolyHyp, but for a MatchedSources class.  
    Input  : - A MatchedSources object.  
    * ...,key,vals,...  
    'MagFieldNames' - A cell array of dictionary field names  
    for the Magnitude matrix in the MatchedSources  
    object.  
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
    Example: MS = MatchedSources;  
    MS.addMatrix(rand(100,200),'FLUX')  
    MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})  
    Result = lcUtil.fitPolyHyp(MS);  
      
### lcUtil.zp_lsq

Apply a relative photometry least square ZP solution to a light curve in MatchedSources object.


    
    Apply a relative photometry least square ZP solution to a light curve  
    in MatchedSources object.  
    Input  : - A MatchedSources object.  
    * ...,key,val,...  
    'MagField' - Mag field in the MatchedSources object data.  
    Default is 'MAG'.  
    'MagErrField' - Mag error field in the MatchedSources object data.  
    Default is 'MAGERR'.  
    'UseSparse' - A logical indicating if to generate  
    a sparse design matrix. Default is false.  
    'SrcProp' - A cell array of additional properties  
    to add to the design matrix.  
    Each cell element can be a row vector  
    (property per source; e.g., color), a  
    column vector (property per epoch; e.g.,  
    air mass), or a matrix of size Nep X Nsrc.  
    These properties will be added to the  
    design matrix according to the scheme  
    dictated by 'SrcPropCoefType'.  
    Default is {}.  
    'SrcPropCoefType' - A vector of numbers, each  
    element corresponds to a cell element in  
    'SrcProp'. The numbres may be one of the  
    following:  
    1 - will add a single column to the design  
    matrix (i.e., a single coef.).  
    2 - will add a column per epoch.  
    'MinNepoch' - Use sources which the min. number of epochs  
    they appear (not NaN) is larger than this number.  
    Default is 10.  
    'Niter' - Number of fitting iterations.  
    In the second iteration lscov will be used with the  
    errors estimated from the mag vs. std plot.  
    'UseBL' - A logical indicating if to use the "\" operator  
    in the first iteration. Default is true.  
    'CalibMag' - A vector of calibrated (external) magnitude  
    per source. The resulted magnitude will calibrated  
    according to this magnitudes.  
    'Plot' - A logical indicating if to plot a mag. vs. std.  
    plot for each iteration. Default is false.  
    Output : - A structure containing the best fit information:  
    .FlagMin - A vector of flags (one per source) indicating  
    if the source was used. I.e., number of epochs  
    larger than MinNepoch.  
    .FlagSrc - A vector of flags (Nsrc X Nep) indicating if  
    the source was used in the final iteration  
    calibration.  
    .FitZP - A vector of fitted ZP [mag].  
    .FitMeanMag - A vector of fitted Mean Mag [mag].  
    .FitExtra - A vector of extra fitted parameters.  
    .MeanMag - A vector of mean mags.  
    .AllResid - An array of all residuals from best fit.  
    .StdSrc - A vector of Std of residuals per source.  
    .StdEpoch - A vector of std of residuals per epoch.  
    Author : Eran Ofek (Sep 2021)  
    Example: Fzp   = 1 + rand(100,1);  
    Fstar = rand(1,200).*3900 + 100;  
    Flux = Fzp.*Fstar;  
    Flux = poissrnd(Flux);  
    FluxErr = sqrt(Flux);  
    Mag     = 22-2.5.*log10(Flux);  
    MeanMag = mean(Mag);  
    MagErr  = 1.086.*FluxErr./Flux;  
    MS = MatchedSources;  
    MS.addMatrix({Mag, MagErr},{'MAG','MAGERR'});  
      
    [R, CorrMS] = lcUtil.zp_lsq(MS);  
    [R, CorrMS] = lcUtil.zp_lsq(MS, 'CalibMag',MeanMag);  
    std(R.FitZP +2.5.*log10(Fzp))   verify that std is small  
      
### lcUtil.zp_sumFlux

Example: Fzp = rand(100,1).*1000; Flux = randn(100,200) + Fzp; Result = lcUtil.zp_sumFlux(Flux, 'MaxRelErr',[]);


    
      
    Example: Fzp = rand(100,1).*1000;  
    Flux = randn(100,200) + Fzp;  
    Result = lcUtil.zp_sumFlux(Flux, 'MaxRelErr',[]);  
      
    Fzp   = 1 + rand(100,1);  
    Fstar = rand(1,200).*3900 + 100;  
    Flux = Fzp.*Fstar;  
    Flux = poissrnd(Flux);  
    FluxErr = sqrt(Flux);  
    MS = MatchedSources;  
    MS.addMatrix({Flux, FluxErr},{'FLUX','FLUXERR'});  
    [Result, Info] = lcUtil.zp_sumFlux(MS);  
    if  
    any(abs(Info.FluxZP./median(Info.FluxZP)./(Fzp./median(Fzp))-1)>0.01),  
    error  
      
      
