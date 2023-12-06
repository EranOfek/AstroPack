function WMed = wmedian(Val, Err, Dim)
    % Weighted median for a vector.
    % Description: Weighted median for a vector.
    %              Calculates the weighted median of a vector
    %              given the error on each value in the vector.
    % Input  : - Data (vector or matrix only)
    %          - Error for each data point in Data.
    %            The weights are given by 1./Error.^2
    % Output : - Weighted median. If the inputs are matrices, than this is
    %            a vector of weighted median (one per column if Dim=1) or one per row
    %            (for Dim=2).
    % Reference: https://en.wikipedia.org/wiki/Weighted_median
    % License: GNU general public license version 3
    % Tested : Matlab R2015a
    %     By : Eran O. Ofek                    Jun 2015
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: D=tools.math.stat.wmedian(V,E);
    % Reliable: 2
    %--------------------------------------------------------------------------

    arguments
        Val
        Err
        Dim   = 1;  % 1, 2 or [1 2]/'all'
    end
    
    switch Dim
        case 1
            % do nothing
        case 2
            Val = Val.';
            Err = Err.';
        otherwise
            % Dim is [1 2] or 'all'
            Val = Val(:);
            Err = Err(:);
    end
    
    if size(Val,1)==0
        WMed = nan(1, size(Val,2));
    else
        [SV,SI] = sort(Val, 1);
        Weights = 1./Err(SI).^2;
        CSW     = cumsum(Weights, 1, 'omitnan');  % cumsum of the weights
        if any(isinf(CSW),'all')
            WMed = NaN;
        else
            NVar    = size(Val,2);

            Norm    = sum(Weights, 1, 'omitnan');
            %CSWnorm = CSW./CSW(end);
            CSWnorm = CSW./Norm;

            WMed = nan(1,NVar);
            for Iv=1:1:NVar
                CSWnorm(1,Iv) = min(CSWnorm(1,Iv), 0.5);
                if isnan(SV(end,Iv))
                    CSWnorm(end,Iv) = 1;
                    SV(end,Iv)      = max(SV(:,end));
                end
                WMed(Iv) = interp1(CSWnorm(:,Iv),SV(:,Iv),0.5,'linear');
            end
        end
    end

end
