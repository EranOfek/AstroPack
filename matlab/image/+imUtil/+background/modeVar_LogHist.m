function [Mode, Var] = modeVar_LogHist(Array, Args)
    % Estimate the mode and variance of an array (over all dims).
    %   The mode is estimated by two iteration histograms.
    %   In the first iteration an approximate mode (of the log array) is
    %   estimated, while in the second iteartion its value is refined.
    %   The log of the data histogram is then fitted with a parabola.
    %   The maximum of the parabola is the mode, while the points it which
    %   it intersects the maximum-0.5 defines the std and the variance.
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
    %            'RemoveLowerQuantile' - Remove lower quantile from data.
    %                   Default is 0.
    %            'RemoveUpperQuantile' - Remove upper quantile from data.
    %                   Default is 0.
    %            'UseSlash' - A logical indicating if to use the \ operator
    %                   to fit the 2nd order polynomial. If false, then use
    %                   polyfit.
    %                   Default is true.
    %            See code for additional arguments (use with care).
    % Output : - Estimated mode.
    %          - Estimated variance.
    % Author : Eran Ofek (2023 Dec) 
    % Example: [M,V]=imUtil.background.modeVar_LogHist(R);
    %          for I=1:1:numel(AI); [M(I),V(I)]=imUtil.background.modeVar_LogHist(AI(1).Image(:)); [M1(I),V1(I)]=imUtil.background.mode(AI(1).Image(:)); end

    arguments
        Array
        Args.DiluteFactor              = 1;
        Args.DiluteFactor1             = 10;
        
        Args.Convert2single logical    = false;
        Args.MinVal                    = [];
        Args.RemoveLowerQuantile       = 0; %0.01; %0.01;
        Args.RemoveUpperQuantile       = 0; %0.1; %0.1;

        Args.UseSlash logical          = true;

        Args.EdgesFactor               = 0.5;
        Args.OverSampling              = 0.3;
        Args.LogNhistFromPeak          = 2;
        
    end
    
    % convert to vector
    Array = Array(:);
 
    if Args.DiluteFactor>1
        Array = Array(1:Args.DiluteFactor:end, 1:Args.DiluteFactor:end);
    end
    
    % convert to vector
    Array = Array(:);
    
    if Args.Convert2single
        Array = single(Array);
    end
    if ~isempty(Args.MinVal)
        Array = Array(Array>Args.MinVal);
    end
    
    
    % remove lower/upper quantile
    if Args.RemoveLowerQuantile>0 || Args.RemoveUpperQuantile>0
        QLevel = quantile(Array, [Args.RemoveLowerQuantile, 1-Args.RemoveUpperQuantile]);
        Array  = Array(Array>QLevel(1) & Array<QLevel(2));
    end
    
    Na = numel(Array);
    
    LogArray = log(Array(1:Args.DiluteFactor1:end));
    Min      = min(LogArray);
    Max      = max(LogArray);
    Range    = Max - Min;
    Nbin     = max(5, 2.*ceil(Range));
    BinSize   = Range./Nbin;
    Edges     = (Min:BinSize:Max).';
    BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
    Nhist = matlab.internal.math.histcounts(LogArray, Edges);
    Nhist = Nhist(1:end-1);
    BinCenter = BinCenter(1:end-1);

    [~,Imax] = max(Nhist);
    Mode1 = BinCenter(Imax);

%     Edges = (Mode1-1:0.01:Mode1+1).';
%     Edges     = exp(Edges);
%     BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
    
    Mode1 = exp(Mode1);
    Edges = (Mode1.*Args.EdgesFactor: sqrt(Mode1).*Args.OverSampling:Mode1./Args.EdgesFactor).';
    BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
    
    Nhist = matlab.internal.math.histcounts(Array, Edges);
    Nhist = Nhist(1:end-1);
    BinCenter = BinCenter(1:end-1);

    Nhist = log(Nhist);
    [MaxNhist,Imax] = max(Nhist);
    Mode0 = BinCenter(Imax);

    

    %Ind   = Nhist(:)>1 & BinCenter(:)<Mode0.*1.3;
    Ind       = Nhist>(MaxNhist - Args.LogNhistFromPeak);
    Nhist = Nhist(Ind);
    BinCenter = BinCenter(Ind);
    
    Nbin  = numel(Nhist);
    
    
    if Args.UseSlash
        % H = [BinCenter.^2, ones(Nbin,1)];
        H = [(BinCenter-Mode0).^2, (BinCenter-Mode0), ones(Nbin,1)];
        Par = (H\Nhist(:)).';

        % X=[100:1:250]';  H = [(X-Mode0).^2, (X-Mode0), ones(numel(X),1)];
        % plot(BinCenter, Nhist,'o'); hold on; plot(X,H*Par(:) )
        
    else
        % slower
        Par   = polyfit(BinCenter-Mode0, Nhist,2);
        % X=[100:1:250]';  H = [(X-Mode0).^2, (X-Mode0), ones(numel(X),1)];
        % plot(BinCenter, Nhist,'o'); hold on; plot(X,polyval(Par,X) )
        
    end
    Mode  = Mode0 - Par(2)./(2.*Par(1));
    Var = -0.5./Par(1);
    

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
