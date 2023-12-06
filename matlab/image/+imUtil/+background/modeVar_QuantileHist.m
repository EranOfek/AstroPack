function [Mode,Variance]=modeVar_QuantileHist(Array,Log,IgnoreNaN,Accuracy,MinN,OnlyLower) 
    % Mode and variance of a distribution
    %   In most cases you may want to use: use imUtil.background.modeVar_LogHist instead.
    % Description: Calculate the mode and robust variance of an array.
    %              The mode is calculated by making an histogram and choosing
    %              the bin with the highest number of counts. The robust 
    %              variance is calculated from the histogram (via interpolation).
    % Input  : - An array for which to calculate the global mode, and robust
    %            variance.
    %          - (Log) A logical indicating if to calculate the histogram of
    %            the log of the values, this is recomended when the values
    %            distribution has an higher tail (e.g., like in astronomical
    %            images).
    %            Default is true.
    %          - Ignore NaNs. Default is true.
    %          - (Accuracy). The (roughly) required accuracy. This parameter
    %            controls the histogram bin size (requiring that on average
    %            each bin contains (1/Accuracy)^2 points).
    %            Default is 0.1.
    %          - (MiN) Minimum number of points (not NaN) in the array.
    %            If the number of points is smaller than this bound then the
    %            function will return NaNs.
    %            Default is 10.
    %          - A logical indicating if to calculate the variance on the lower
    %            quantile. Default is true.
    % Output : - The robust median calculated using the scaled iqr
    % Author : Eran Ofek (Apr 2020)
    % Example: imUtil.background.mode(randn(1000,1000))
    
    arguments
        Array
        Log             = false;
        IgnoreNaN       = false;
        Accuracy        = 0.1;
        MinN            = 10;
        OnlyLower       = true;
    end

    %[Mode, Variance] = tools.math.stat.mode_vec(Array,Log,IgnoreNaN,Accuracy,MinN,OnlyLower);
    

    if IgnoreNaN
        Array = Array(~isnan(Array) & ~isinf(Array));
    end


    N = numel(Array);
    if N<MinN
        Mode      = NaN;
        Variance  = NaN;
    else
        if Log
            % remove negative numbers
            % Fnneg = Array>0;
            % Array = log10(Array(Array>0));
            % faster to use log
            %Array = single(Array);
            
            Array = log(Array(Array>0));
            %Array = Array(:);
        else
            Array = Array(:);
        end

        % remove infinities
        %Array = Array(~isinf(Array));

        Min = min(Array);
%         if Log
%             Min = max(1, Min);  % ignore negative values
%         end
        Max = max(Array);

        Nbin = (1./Accuracy).^2;
        
%         if Log
%             Edges = logspace(log10(Min), log10(Max), Nbin);
%         else
            BinSize = (Max - Min)./Nbin;
            Edges = (Min-BinSize:BinSize:Max+BinSize).';
%         end
        if isempty(Edges)
            % this happens when there is only a single value
            if Max==Min
                if Log
                    %Mode = Max; %exp(Max);
                    Mode = exp(Max);
                    
                else
                    Mode = Max;
                end
                Variance = 0;
            else
                error('Can not calculate mode - likely because all pixels have identical value');
            end
        else


            % accumarray is slower
    %         Ind = ceil((Array - Edges(1))./BinSize);
    %         %Ind  = (1:1:numel(Edges)).';
    %         Data  = ones(size(Ind));
    %         NNN=accumarray(Ind, Data,[numel(Edges), 1]);
    %         [~,MaxI] = max(NNN);

            %Nhist = histcounts(Array,Edges);
            Nhist = matlab.internal.math.histcounts(Array,Edges);
            %Nhist = cast(Nhist, 'like',Array);  % not helping
            
            [~,MaxI]  = max(Nhist);

            Mode = Edges(MaxI) + 0.5.*BinSize;
            %Mode = (Edges(MaxI) + Edges(MaxI+1)).*0.5;

            if Log
                %Mode = Mode; %exp(Mode);
                Mode = exp(Mode);
            end

            if nargout>1
                CumN = cumsum(Nhist(:));
                CumN = CumN + (1:1:numel(CumN)).'.*10000.*eps;
                
                %IqrVal = interp1(CumN, Edges(1:end-1)+0.5.*BinSize, [0.25 0.75].*CumN(end), 'linear').';
                %IqrVal = interp1(CumN,Edges(1:end-1)+0.5.*BinSize,[0.25 0.75]'.*CumN(end),'linear');
                %IqrVal1 = interp1q(CumN,10.^Edges(1:end-1)+0.5.*BinSize,[0.25 0.75]'.*CumN(end));
                
                % interp1q is faster, but doesnt check validity
                IqrVal = interp1q(CumN,Edges(1:end-1)+0.5.*BinSize,[0.25 0.75]'.*CumN(end));
                
%                 %Ntot = CumN(end);
%                 I25=tools.find.mfind_bin(CumN, [0.25 0.75].*CumN(end));
%                 %I75=tools.find.bin_sear3(CumN, 0.75.*CumN(end));                
%                 %I25 = find(CumN>(0.25.*Ntot), 1);
%                 %I75 = find(CumN>(0.75.*Ntot), 1);
%                 IqrVal = [Edges(I25(1)); Edges(I25(2))];% + 0.5.*BinSize;
                
                %Factor = 0.7413;  %  = 1./(norminv(0.75,0,1)-norminv(0.25,0,1))
                Factor = 1.482602; % 1./(norminv(0.75,0,1)-norminv(0.5,0,1))

                if Log
                    % convert back to numbers
                    IqrVal = exp(IqrVal);
                end

                if OnlyLower
                    IqrHalfRange = Mode - IqrVal(1);
                else
                    IqrHalfRange = 0.5.*(IqrVal(2)-IqrVal(1));
                end
                
                Variance = (IqrHalfRange.*Factor).^2;



                % if OnlyLower
                %     IqrVal(2) = log(Mode);
                %     Factor    = Factor.*2;
                % 
                % end
                % 
                % if Log
                %     %Variance = (range(IqrVal).*Factor).^2;  %(range(exp(IqrVal)).*Factor).^2;
                %     Variance = (range(exp(IqrVal)).*Factor).^2;
                % else
                %     Variance = (range(IqrVal).*Factor).^2;
                % end
                % range((IqrVal)).*Factor
    %             Variance
    %             'a'
            end
        end
    end
end



