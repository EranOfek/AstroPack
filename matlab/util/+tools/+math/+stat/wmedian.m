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
    
    arguments
        Val
        Err
        Dim   = 1;  % 1, 2 or [1 2]/'all'
    end

    % Normalize Dim and orient so we operate down columns
    switch Dim
        case 1
            % as is
            doTranspose = false;
        case 2
            Val = Val.';
            Err = Err.';
            doTranspose = true;
        otherwise
            % Dim is [1 2] or 'all'
            Val = Val(:);
            Err = Err(:);
            doTranspose = false;
    end

    [Nrow, Ncol] = size(Val);
    if Nrow==0 || Ncol==0
        WMed = nan(1, max(1,Ncol));
        if doTranspose
            WMed = WMed.'; % one per row in original orientation
        end
        return
    end

    WMed = nan(1, Ncol);

    for Iv = 1:Ncol
        v = Val(:,Iv);
        e = Err(:,Iv);

        % 1) Clean rows where either is NaN
        mask = ~isnan(v) & ~isnan(e);
        v = v(mask);
        e = e(mask);

        if isempty(v)
            WMed(Iv) = NaN;
            continue
        end

        % 2) Handle zero-error points (infinite weight) FIRST
        iz = (e==0);
        if any(iz)
            % The weighted median collapses to the (unweighted) median of v(iz)
            vz = v(iz);
            WMed(Iv) = median(vz);   % standard median, handles even count
            continue
        end

        % 3) Compute weights; guard pathologies
        w = 1./(e.^2);
        w(~isfinite(w) | w<0) = 0;   % negative/inf -> 0
        sw = sum(w);
        if sw<=0
            WMed(Iv) = NaN;
            continue
        end
        w = w / sw;

        % 4) Sort by value, accumulate CDF
        [vs, I] = sort(v, 'ascend');
        ws = w(I);
        C  = cumsum(ws);

        % 5) Discrete L1 median = smallest vs where C >= 0.5
        k = find(C >= 0.5, 1, 'first');
        if isempty(k)
            k = numel(C);  % numerical safety
        end
        WMed(Iv) = vs(k);
    end

    % Return shape consistent with docstring
    if doTranspose
        WMed = WMed.';  % one per original row
    end
end
