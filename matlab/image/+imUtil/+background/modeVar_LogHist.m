function [Mode, Var] = modeVar_LogHist(Array, Args)
    % Estimate the mode and variance of an array (over all dims).
    %   Obsolete: use instead:  imUtil.background.modeVar_SampleHist
    %   This function is designed for astronomical images in units of
    %   electrons (i.e., their Poisson properties are still in place) and
    %   that do not have negative values.
    %   The mode is estimated by two iteration histograms.
    %   In the first iteration an approximate mode (of the log array) is
    %   estimated, while in the second iteartion its value is refined.
    %   The log of the data histogram is then fitted with a parabola.
    %   The maximum of the parabola is the mode, while the points it which
    %   it intersects the maximum-0.5 defines the std and the variance.
    %       See also: imUtil.background.modeVar_LogHist
    %       
    % Input  : - An array for which to calculate the mode (over all dims).
    %          * ...,key,val,... 
    %            'DiluteFactor' - (positive integer) If >1 then will dilute the array by this
    %                   factor (for speed up). Default is 1.
    %            'DiluteFactor1' - Additional diluation factor (over
    %                   DiluteFactor) that will be applied only in the
    %                   first iteration. Default is 10.
    %            'Convert2single' - Convert input array to single.
    %                   Default is false.
    %            'MinVal' - Remove values from the array which are below
    %                   this number. If empty, then do not remove.
    %                   Default is [].
    %            'MaxVal' - Remove values from the array which are above
    %                   this number. If empty, then do not remove.
    %                   Default is [].
    %            'RemoveLowerQuantile' - Remove lower quantile from data.
    %                   Default is 0.
    %            'RemoveUpperQuantile' - Remove upper quantile from data.
    %                   Default is 0.
    %            'UseSlash' - A logical indicating if to use the \ operator
    %                   to fit the 2nd order polynomial. If false, then use
    %                   polyfit.
    %                   Default is true.
    %            'RN2' - ReadNoise^2 of image. Will be used only if
    %                   variance estimation failed.
    %                   Default is 12.
    %            'CalclPoissVar' - A logical indicating if to calculate the
    %                   variance assuming pure Poisson noise + ReadNoise^2.
    %                   Default is false.
    %            'VarSqrtFactor' - If variance is estimated assuming
    %                   Poisson noise + Readnoise^2, then this factor multiply the
    %                   final variance. Default is 1.05;
    %
    %            See code for additional arguments (use with care).
    %
    % Output : - Estimated mode.
    %          - Estimated variance.
    % Notes  : - Array diluation is done in a weird way and should be
    %            revisted.
    % Author : Eran Ofek (2023 Dec) 
    % Example: [M,V]=imUtil.background.modeVar_LogHist(R);
    %          for I=1:1:numel(AI); [M(I),V(I)]=imUtil.background.modeVar_LogHist(AI(1).Image(:)); [M1(I),V1(I)]=imUtil.background.mode(AI(1).Image(:)); end

    arguments
        Array
        Args.DiluteFactor              = 2;
        Args.DiluteFactor1             = 10;
        
        Args.Convert2single            = false;
        Args.MinVal                    = [];
        Args.MaxVal                    = [];
        Args.RemoveLowerQuantile       = 0; %0.01; %0.01;
        Args.RemoveUpperQuantile       = 0; %0.1; %0.1;

        Args.UseSlash                  = true;

        Args.EdgesFactor               = 0.5;
        Args.OverSampling              = 0.1;
        Args.LogNhistFromPeak          = 1.8; %2;  % assymetric [left, right]

        Args.MinNbin1                  = 7;   % minimum number of bins in the 1st log iteration
        
        Args.VarSqrtFactor             = 1.05;   % if Var/Mode is failed - estimate Var from Mode0*VarSqrtFactor
        Args.RN2                       = 12;     % RN variance (RN^2)
        Args.CalcPoissVar              = false;
        Args.UseMex                    = true;
    end
    
    %OrigArray = Array;
    % convert to vector
    %Array = Array(:);
 
    if Args.DiluteFactor>1
        Array = Array(1:Args.DiluteFactor:end, 1:Args.DiluteFactor:end);
    end
    
    % convert to vector
    Array = Array(:);
    
    if Args.Convert2single
        Array = single(Array);
    end
    if ~isempty(Args.MinVal) || ~isempty(Args.MaxVal)
        Array = Array(Array>Args.MinVal & Array<Args.MaxVal);
    end

    % Remove NaNs
    Array = Array(~isnan(Array));
    
    
    % remove lower/upper quantile
    if Args.RemoveLowerQuantile>0 || Args.RemoveUpperQuantile>0
        QLevel = quantile(Array, [Args.RemoveLowerQuantile, 1-Args.RemoveUpperQuantile]);
        Array  = Array(Array>QLevel(1) & Array<QLevel(2));
    end
    
    Na = numel(Array);
    if Args.UseMex
        LogArray = tools.math.fun.logApprox(Array); % approximation to better than 10^-5
    else
        LogArray = log(Array); %(1:Args.DiluteFactor1:end));
    end
    if Args.UseMex
        [Min,Max]= tools.math.stat.mex.minmaxGlobal_mex(LogArray);
    else
        Min      = min(LogArray);
        Max      = max(LogArray);
    end
    Range    = Max - Min;

    Nbin     = round(max(Args.MinNbin1, 2.*ceil(Range)));
    BinSize   = Range./Nbin;

    if Args.UseMex
        Nbin = round((Range + 2.*BinSize)./BinSize);    
        [Nhist,Edges,BinCenter] = tools.hist.mex.hist1reg_mex(LogArray, double([Min, Max+2.*BinSize]), Nbin, single(1), false);
       
    else
        % Why Max+2.*BinSize: sometimes due to pixels with low counts, the
        % number of bins (Args.MinNbin1) is not sufficient 
        Edges     = (Min:BinSize:(Max+2.*BinSize)).';
    
        BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
        if any(isnan(Edges))
            error('Edges is NaN use MinVal, MaxVal inputs');
        end
        Nhist = matlab.internal.math.histcounts(LogArray, Edges);    
    end

    % why is this needed?
    Nhist = Nhist(1:end-1);
    BinCenter = BinCenter(1:end-1);
    % Debug: bar(BinCenter, Nhist)

    [~,Imax] = max(Nhist);
    Mode1 = BinCenter(Imax);

%     Edges = (Mode1-1:0.01:Mode1+1).';
%     Edges     = exp(Edges);
%     BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
    
    Mode1 = exp(Mode1);
    
    
    if Args.UseMex
        Edges = (Mode1.*Args.EdgesFactor: sqrt(Mode1).*Args.OverSampling:Mode1./Args.EdgesFactor).';
        HistStart = Edges(1);
        HistEnd   = Edges(end);
        Nbin      = numel(Edges) - 1;
        %HistStart = Mode1.*Args.EdgesFactor;
        %HistBin   = sqrt(Mode1).*Args.OverSampling;
        %HistEnd = Mode1./Args.EdgesFactor;
        %Nbin = ceil((HistEnd - HistStart)./HistBin);
        [Nhist,Edges,BinCenter] = tools.hist.mex.hist1reg_mex(Array, [HistStart HistEnd], Nbin, single(1), false);
    else
        Edges = (Mode1.*Args.EdgesFactor: sqrt(Mode1).*Args.OverSampling:Mode1./Args.EdgesFactor).';
        BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
        Nhist = matlab.internal.math.histcounts(Array, Edges);
    end
    % why needed?
    Nhist = Nhist(1:end-1);
    BinCenter = BinCenter(1:end-1);


    Nhist = log(Nhist);
    [MaxNhist,Imax] = max(Nhist);
    Mode0 = BinCenter(Imax);
    % Debug: bar(BinCenter, Nhist)
    

    %Ind   = Nhist(:)>1 & BinCenter(:)<Mode0.*1.3;
    
    Ind       = Nhist>(MaxNhist - Args.LogNhistFromPeak); % & BinCenter.'<(Mode0.*1.3);
    Nhist = Nhist(Ind);
    BinCenter = BinCenter(Ind);
    
    Nbin  = numel(Nhist);
    
    
    if Args.UseSlash
        % H = [BinCenter.^2, ones(Nbin,1)];
        H = [(BinCenter-Mode0).^2, (BinCenter-Mode0), ones(Nbin,1)];
        Par = (H\Nhist(:)).';

        % X=[100:1:250]';  H = [(X-Mode0).^2, (X-Mode0), ones(numel(X),1)];
        % plot(BinCenter, Nhist,'o'); hold on; plot(X,H*Par(:) )
        

        % Equivalent to:
        % x = BinCenter - Mode0;
        % y = Nhist(:);
        % 
        % S0 = numel(x);
        % S1 = sum(x);
        % S2 = sum(x.^2);
        % S3 = sum(x.^3);
        % S4 = sum(x.^4);
        % 
        % T0 = sum(y);
        % T1 = sum(y.*x);
        % T2 = sum(y.*x.^2);
        % 
        % A = [S4, S3, S2; ...
        %      S3, S2, S1; ...
        %      S2, S1, S0];
        % 
        % b = [T2; T1; T0];
        % 
        % Par = (A\b).';



    else
        % slower
        Par   = polyfit(BinCenter-Mode0, Nhist,2);
        % X=[100:1:350]';  H = [(X-Mode0).^2, (X-Mode0), ones(numel(X),1)];
        % plot(BinCenter, Nhist,'o'); hold on; plot(X,polyval(Par,X) )
        
    end
    Mode  = Mode0 - Par(2)./(2.*Par(1));
    Var = -0.5./Par(1);
    if Var<0 || Args.CalcPoissVar
        % use Mode from hsitogram
        Mode = Mode0;
        % For variance, assume Poisson noise
        Var  = (Mode + Args.RN2).*Args.VarSqrtFactor;

        %error('Unable to find Var (Var is negative) - need to debug: Mode=%f   Var=%f',Mode,Var);
    end
    

    if isinf(Mode) || isnan(Mode)
        error('Unable to find Mode - need to debug: Mode=%f   Var=%f',Mode,Var);
    end


    % if Par(1)<0
    %     % minimum found
    %     Mode  = -Par(2)./(2.*Par(1));
    %     Npeak = polyval(Par, Mode);
    %     Par1  = Par;
    %     Par1(3) = Par1(3) - (Npeak - 0.5);  %.*0.60653;    % normpdf([1],0,1)./normpdf(0,0,1)
    %     Roots = roots(Par1);
    %     Var   = (0.5.*(Roots(2) - Roots(1))).^2;
    % else
    %     Mode = NaN;
    %     Var  = NaN;
    % end

end
