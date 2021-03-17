function [Flag,Res]=resid_vs_mag(Mag,Resid,varargin)
% Fit residuals vs. mag and flag good data (not outliers).
% Package: +imUtil.calib
% Description: Give vectors of residuals vs. magnitude, calculate the std
%              or residuls vs. magnitude and flag good data (i.e., not
%              outliers).
%              Done using either polynomial fit, to the residuals vs.
%              magnitude. In this case the std is global.
%              Or, binning the data and calculate the mean and std in each
%              bin. Outliers are defined to be ThresholdSigma times the std
%              above the mean value.
% Input  : - A vector of magnitudes.
%          - A vector of residuals (one per magnitude).
%          * Pairs of ...,key,val,... arguments. Options are:
%            'MagRange' - Magnitude range in which to calculate the
%                   residiuals. If [], then choose by min max.
%                   Default is [].
%            'BinMethod' - Method to use:
%                   'poly' - polynomial fit.
%                   'bin' - binning the data.
%                   Default is 'bin'
%            'PolyDeg' - Polynomial degree for the polynomial fit.
%                   Default is 3.
%            'BinSize' - Bin size for binning. Default is 1 (mag).
%            'FunMean' - A function handle to use when calculating the mean
%                   of the data in each bin.
%                   Default is @nanmedian.
%            'FunStd' - A function handle to use when calculating the std
%                   of the data in each bin, or when calculating the global
%                   std after the polynomial fit.
%                   Default is @imUttil.background.rstd.
%            'InterpMethod' - Interpolation method. Default is 'linear'.
%            'ThresholdSigma' - Threshold in sigmas (std) for flagging good
%                   data. Default is 3.
%            'Plot' - A logical flag indicating if to plot the residuals
%                   vs. mag. Default is false.
% Output : - a vector of logical flags (one per mag/resid) indicating if
%            the source residual is smaller than the threshold (true; good
%            source) or above the threshold (false; outlier).
%          - A structure with the following fields:
%            .Mag - Vector of input Mag after removing out of range values.
%            .Resid - Vector of input Resid after removing out of range
%                   values.
%            .InterpMeanResid - Vector of mean interpolated mean residual
%                   at the source magnitude.
%            .InterpStdResid - Vector of interpolated or global std of
%                   residuals at the source mag.
%      By: Eran O. Ofek                         Jun 2020
% Example: Flag=imUtil.calib.resid_vs_mag(Mag,Resid);

InPar = inputParser;
addOptional(InPar,'MagRange',[]); 
addOptional(InPar,'BinMethod','bin'); % 'bin' | 'poly'
addOptional(InPar,'PolyDeg',3);
addOptional(InPar,'BinSize',1);
addOptional(InPar,'FunMean',@nanmedian);
addOptional(InPar,'FunStd',@imUtil.background.rstd);
addOptional(InPar,'InterpMethod','linear');
addOptional(InPar,'ThresholdSigma',3);
addOptional(InPar,'Plot',false);

parse(InPar,varargin{:});
InPar = InPar.Results;

if isempty(InPar.MagRange)
    InPar.MagRange = [min(Mag), max(Mag)];
end

FlagMag = Mag>=InPar.MagRange(1) & Mag<=InPar.MagRange(2);
Mag     = Mag(FlagMag);
Resid   = Resid(FlagMag);

Res.Mag              = Mag;
Res.Resid            = Resid;

switch lower(InPar.BinMethod)
    case 'fit'
        % fit polynomial to resid vs. mag
        
        Par = polyfit(Mag,Resid,InPar.PolyDeg);
        
        Res.InterpMeanResid = polyval(Par,Mag);
        Res.InterpStdResid = InPar.FunStd(DeltaResid);
        Flag = (Resid - Res.InterpMeanResid)./Res.InterpStdResid < InPar.ThresholdSigma;

    case 'bin'
        % binning of resid vs. mag

        B = timeseries.binning([Mag, Resid],InPar.BinSize,InPar.MagRange,{'MidBin',InPar.FunMean,InPar.FunStd,@numel});
        % interpolate B over missing points
        Res.InterpMeanResid = interp1(B(:,1),B(:,2),Mag,InPar.InterpMethod);
        Res.InterpStdResid  = interp1(B(:,1),B(:,3),Mag,InPar.InterpMethod);
        
        Flag = (Resid - Res.InterpMeanResid)./Res.InterpStdResid < InPar.ThresholdSigma;

    otherwise
        error('Unknown Method option');
end



if InPar.Plot
    % plot
   
    semilogy(Mag,Resid,'.','MarkerSize',1);
    hold on;
    semilogy(Mag(Flag),Resid(Flag),'o');
    
    
end