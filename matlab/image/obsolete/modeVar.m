function [Mode, Var] = modeVar(Array, Args)
    % Calculate the mode and variance of an array (over all dims)
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2023 Dec) 
    % Example: imUtil.background.modeVar(poissrnd(180,500,500));

    arguments
        Array
        Args.DiluteFactor           = 1;  % 2...
        Args.Convert2single logical = false;
        Args.MinVal                 = 0;   % if empty - ignore
        Args.OmitNaN logical        = false;
                
        Args.RemoveLowerQuantile    = 0; %0.01; %0.01;
        Args.RemoveUpperQuantile    = 0; %0.1; %0.1;
        
        Args.Method                 = 'histiter'; %'hist';  %'sort';
        Args.NbinParabola           = 10;
        Args.Log logical            = true;
        Args.OnlyLower logical      = true;
        Args.BinSize                = [];
        Args.NbinPerDec             = 10;
        Args.MinNbin                = 20;
        Args.MinBinSize             = 1;
        
        Args.SortOrder              = 100;
        Args.HistRangeMethod        = 'range';  % 'range'|'median'|'sort
        
    end
    
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
    if Args.OmitNaN
        Array = Array(~isnan(Array));
    end
    
    % remove lower/upper quantile
    if Args.RemoveLowerQuantile>0 || Args.RemoveUpperQuantile>0
        QLevel = quantile(Array, [Args.RemoveLowerQuantile, 1-Args.RemoveUpperQuantile]);
        Array  = Array(Array>QLevel(1) & Array<QLevel(2));
    end
    
    Na = numel(Array);

    
    switch lower(Args.Method)
        case 'histiter'
            % iterative histograms
            
            LogArray = log(Array(1:10:end));
            Min      = min(LogArray);
            Max      = max(LogArray);
            Range    = Max - Min;
            Nbin     = max(5, ceil(Range));
            BinSize   = Range./Nbin;
            Edges     = (Min:BinSize:Max).';
            BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
            Nhist = matlab.internal.math.histcounts(LogArray, Edges);
            Nhist = Nhist(1:end-1);
            BinCenter = BinCenter(1:end-1);
            
            [~,Imax] = max(Nhist);
            Mode1 = BinCenter(Imax);
            
            Edges = (Mode1-1:0.1:Mode1+1).';
            Edges     = exp(Edges);
            BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
            Nhist = matlab.internal.math.histcounts(Array, Edges);
            Nhist = Nhist(1:end-1);
            BinCenter = BinCenter(1:end-1);
            
            Ind   = Nhist>1;
            Nhist = Nhist(Ind);
            BinCenter = BinCenter(Ind);
            Nhist = log(Nhist);
            
            Par   = polyfit(BinCenter, Nhist,2);
            
            % plot(BinCenter, Nhist,'o'); hold on; X=[100:1:250]'; plot(X, polyval(Par,X))
            
            if Par(1)<0
                % minimum found
                Mode  = -Par(2)./(2.*Par(1));
                Npeak = polyval(Par, Mode);
                Par1  = Par;
                Par1(3) = Par1(3) - (Npeak - 0.5);  %.*0.60653;    % normpdf([1],0,1)./normpdf(0,0,1)
                Roots = roots(Par1);
                Var   = (0.5.*(Roots(2) - Roots(1))).^2;
            else
                Mode = NaN;
                Var  = NaN;
            end
                
            
            
            
        case 'hist'
            if Args.Log
                LogArray = log(Array);
            else
                LogArray = Array;
            end
            Min   = min(LogArray);
            Max   = max(LogArray);
            if isempty(Args.BinSize)
                % estimate BinSize
                if Args.Log
                    Args.Nbin = (Max-Min).*Args.NbinPerDec;
                else
                    Args.Nbin    = sqrt(Na);
                end
                Args.Nbin = max(Args.Nbin, Args.MinNbin);
                Args.BinSize = (Max - Min)./Args.Nbin;
                if ~Args.Log
                    Args.BinSize = max(Args.BinSize, Args.MinBinSize);
                end
            end
            
            Edges = (Min:Args.BinSize:Max);
            if Args.Log
                Edges = exp(Edges);
            end
            
            BinCenter = (Edges(1:end-1) + Edges(2:end)).*0.5;
            Nhist = matlab.internal.math.histcounts(Array,Edges);
            Nhist = Nhist(1:end-1);
            BinCenter = BinCenter(1:end-1);
            Nbin      = numel(BinCenter);
            %Nhist = Nhist(1:end-1);
            [~, Imax] = max(Nhist);
            Nhist     = log(Nhist);
            Mode  = BinCenter(Imax);   %0.5.*(Edges(Imax) + Edges(Imax+1));
            Ind   = (Imax-Args.NbinParabola:Imax+Args.NbinParabola).';
            Ind   = Ind(find(Ind>0 & Ind<=Nbin));
            Par   = polyfit(BinCenter(Ind), Nhist(Ind),2);
            
            % plot(BinCenter, Nhist,'o'); hold on; X=[150:1:200]'; plot(X, polyval(Par,X))
            
            if Par(1)<0
                % minimum found
                Mode  = -Par(2)./(2.*Par(1));
                Npeak = polyval(Par, Mode);
                Par1  = Par;
                Par1(3) = Par1(3) - (Npeak - 0.5);  %.*0.60653;    % normpdf([1],0,1)./normpdf(0,0,1)
                Roots = roots(Par1);
                Var   = (0.5.*(Roots(2) - Roots(1))).^2;
            else
                % no minima
                % Mode is based on regular minimum
            end
            
            
            
%             if nargout>1
%                 CumN = cumsum(Nhist(:));
%                 CumN = CumN + (1:1:numel(CumN)).'.*10000.*eps;
%                 
%                 % interp1q is faster, but doesnt check validity
%                 IqrVal = interp1q(CumN(:), BinCenter(:), [0.25 0.75]'.*CumN(end));
%               
%                 Factor = 1.482602; % 1./(norminv(0.75,0,1)-norminv(0.5,0,1))
% 
%                 
%                 if Args.Log
%                     % convert back to numbers
%                     IqrVal = exp(IqrVal);
%                 end
% 
%                 if Args.OnlyLower
%                     IqrHalfRange = Mode - IqrVal(1);
%                 else
%                     IqrHalfRange = 0.5.*(IqrVal(2)-IqrVal(1));
%                 end
%                 
%                 Var = (IqrHalfRange.*Factor).^2;                
%             
%             end            
            
        case 'sort'
            % slow - not good for integer numbers
            Array  = sort(Array);
            VecInd = (1:Args.SortOrder:(Na-Args.SortOrder));
            Diff  = 0;
            for I=1:1:Args.SortOrder
                Diff = Diff + diff(Array(VecInd+I-1));
            end
                
            %[~,Imin] = min(diff(Array(1:Args.SortOrder:end-1)));
            [~,Imin] = min(Diff);
            Mode = Array(Imin.*Args.SortOrder);
            
            Var  = NaN;
        otherwise
            error('Unknown Method option');
    end
    
            
        
end
