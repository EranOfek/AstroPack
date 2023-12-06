function [Flag,Res]=resid_vs_mag(Mag, Resid, Args)
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
%                   Default is @tools.math.stat.nanmedian.
%            'FunStd' - A function handle to use when calculating the std
%                   of the data in each bin, or when calculating the global
%                   std after the polynomial fit.
%                   Default is @imUttil.background.rstd.
%            'InterpMethod' - Interpolation method. Default is 'linear'.
%            'ThresholdSigma' - Threshold in sigmas (std) for flagging good
%                   data. Default is 3.
%            'Plot' - A logical flag indicating if to plot the residuals
%                   vs. mag. Default is false.
% Output : - A vector of logical flags (one per mag/resid) indicating if
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

arguments
    Mag
    Resid
    Args.MagRange          = [];
    Args.BinMethod         = 'bin';    % 'bin' | 'poly'
    Args.PolyDeg           = 3;
    Args.BinSize           = 1;
    Args.FunMean           = @tools.math.stat.nanmedian;
    Args.FunStd            = @imUtil.background.rstd;
    Args.InterpMethod      = 'linear';
    Args.ThresholdSigma    = 3;
    Args.Plot(1,1) logical = false;
end


if isempty(Args.MagRange)
    Args.MagRange = [min(Mag), max(Mag)];
end

FlagMag = Mag>=Args.MagRange(1) & Mag<=Args.MagRange(2);
%Mag     = Mag(FlagMag);
%Resid   = Resid(FlagMag);

Res.Mag              = Mag;
Res.Resid            = Resid;

switch lower(Args.BinMethod)
    case 'fit'
        % fit polynomial to resid vs. mag
        
        Par = polyfit(Mag, Resid, Args.PolyDeg);
        
        Res.InterpMeanResid = polyval(Par, Mag);
        Res.InterpStdResid = Args.FunStd(DeltaResid);
        Flag = (Resid - Res.InterpMeanResid)./Res.InterpStdResid < Args.ThresholdSigma;

    case 'bin'
        % binning of resid vs. mag

        B = timeSeries.bin.binningFast([Mag, Resid], Args.BinSize, Args.MagRange, {'MidBin',Args.FunMean,Args.FunStd,@numel});
        % interpolate B over missing points
        Res.InterpMeanResid = interp1(B(:,1), B(:,2), Mag, Args.InterpMethod,'extrap');
        Res.InterpStdResid  = interp1(B(:,1), B(:,3), Mag, Args.InterpMethod,'extrap');
        
        Flag = abs(Resid - Res.InterpMeanResid)./Res.InterpStdResid < Args.ThresholdSigma & FlagMag;

    otherwise
        error('Unknown Method option');
end



if Args.Plot
    % plot
   
    semilogy(Mag,Resid,'.','MarkerSize',1);
    hold on;
    semilogy(Mag(Flag),Resid(Flag),'o');
    
    
end