function [MidLagVector, SF, SFerr, Npair, Result] = structFun(Data, Args)
    % Structure function of an irregularly sampled time series.
    % Package: timeSeries
    % Description: Calculate the structure function of an irregularly sampled
    %              time series by averaging squared differences of all pairs
    %              whose time differences fall into each lag bin. Optional
    %              measurement errors may be subtracted in order to estimate
    %              the intrinsic structure function. Several error estimators
    %              are supported, including jackknife over the original data
    %              points.
    % Input  : - Time series matrix. Either:
    %             [Time, Val]
    %             [Time, Val, ValErr]
    %            Time units must be consistent with Args.LagEdges.
    %          * ...,key,val,...
    %            'LagEdges' - Vector of lag-bin edges.
    %                            Default is logspace(-2,1,20).
    %            'SubtractNoise' - Logical. If true and ValErr exists,
    %                            subtract the expected measurement-noise term
    %                            E(i)^2 + E(j)^2 from each pair contribution.
    %                            Default is false.
    %            'ReturnRMS' - Logical. If false, return the second-order
    %                            structure function:
    %                               SF(tau) = <(x(t+tau)-x(t))^2>
    %                            If true, return the RMS structure function:
    %                               SF(tau) = sqrt(<(x(t+tau)-x(t))^2>)
    %                            Default is false.
    %            'MinPairs'  - Minimum number of pairs required in a bin in
    %                            order to report SF/SFerr. Default is 1.
    %            'NonNegative' - Logical. If true, clip negative SF values
    %                            to zero after noise subtraction. Default false.
    %            'UseMedian' - Logical. If true, use the median instead of
    %                            the mean for the bin statistic. Default false.
    %            'ErrorMethod' - Error-bar method:
    %                            'jackknife' : delete-1-point jackknife over
    %                                          the original epochs (default).
    %                            'neff'      : use effective number of pairs
    %                                          based on point reuse.
    %                            'naive'     : naive pair-scatter / sqrt(Npair).
    %            'JackknifeMinPairs' - Minimum number of surviving pairs in a
    %                            jackknife realization for that realization to
    %                            contribute to the jackknife variance.
    %                            Default is 1.
    % Output : - MidLagVector : Lag-bin centers.
    %          - SF           : Structure function estimate in each lag bin.
    %          - SFerr        : Estimated uncertainty per lag bin.
    %          - Npair        : Number of contributing pairs per lag bin.
    %          - Result       : Additional diagnostics structure with fields:
    %                           .SFobs        - observed SF before noise subtraction
    %                           .SFnoise      - mean subtracted noise term
    %                           .SFintrinsic  - intrinsic SF before RMS transform
    %                           .Neff         - effective number of pairs for
    %                                           Args.ErrorMethod='neff'
    %                           .SFjk         - jackknife realizations
    % Brief description:
    %   For all pairs (i,j) with j>i define
    %
    %       DeltaT_ij = Time(j) - Time(i)
    %
    %   and assign the pair to lag bin k if
    %
    %       LagEdges(k) <= DeltaT_ij < LagEdges(k+1)
    %
    %   The second-order pair contribution is
    %
    %       Dobs_ij = (Val(j) - Val(i))^2
    %
    %   If Args.SubtractNoise=true and errors exist, subtract
    %
    %       N_ij = Err(i)^2 + Err(j)^2
    %
    %   so the intrinsic pair contribution is
    %
    %       Dint_ij = Dobs_ij - N_ij
    %
    %   Then the second-order structure function in bin k is estimated by
    %
    %       SF(k) = mean(Dint_ij)
    %
    %   over all pairs in the bin, or by the median if Args.UseMedian=true.
    %
    %   If Args.ReturnRMS=true, then return
    %
    %       SF(k) = sqrt( mean(Dint_ij) )
    %
    %   after clipping to non-negative values if requested.
    %
    %   Error bars:
    %     'naive' uses the within-bin scatter divided by sqrt(Npair).
    %
    %     'neff' replaces Npair by an effective number of pairs
    %
    %       Neff = Npair^2 / sum_r m_r^2
    %
    %     where m_r is the number of pairs in the bin using epoch r.
    %
    %     'jackknife' deletes one original epoch at a time, recomputes the bin
    %     statistic, and uses
    %
    %       Var_jack = (N-1)/N * sum_r (SF^(-r) - <SF^(-)>)^2
    %
    % Notes:
    %   - If Args.SubtractNoise=false, the returned SF includes measurement
    %     noise.
    %   - If no error column exists, noise subtraction is skipped.
    %   - Jackknife is usually more realistic than naive pair-based errors,
    %     because the pairs are not independent.
    %
    % Author : ChatGPT + Eran Ofek (Mar 2026)
    %
    % Latex help:
    % \textbf{structure\_function\_ts} calculates the structure function of an
    % irregularly sampled time series by averaging squared differences in lag
    % bins.
    %
    % For each pair $(i,j)$ with $j>i$:
    % \[
    % \Delta t_{ij} = t_j - t_i
    % \]
    %
    % The pair belongs to bin $k$ if
    % \[
    % \mathrm{LagEdges}_k \le \Delta t_{ij} < \mathrm{LagEdges}_{k+1}
    % \]
    %
    % The lag-bin center is
    % \[
    % \tau_k = \frac{\mathrm{LagEdges}_k + \mathrm{LagEdges}_{k+1}}{2}
    % \]
    %
    % The observed pair contribution is
    % \[
    % D^{\mathrm{obs}}_{ij} = (x_j - x_i)^2
    % \]
    %
    % If measurement errors are available and noise subtraction is enabled,
    % subtract the expected noise term
    % \[
    % N_{ij} = e_i^2 + e_j^2
    % \]
    %
    % so that the intrinsic pair contribution becomes
    % \[
    % D^{\mathrm{int}}_{ij} = D^{\mathrm{obs}}_{ij} - N_{ij}
    % \]
    %
    % The second-order structure function in bin $k$ is estimated by
    % \[
    % SF_k = \left\langle D^{\mathrm{int}}_{ij} \right\rangle
    % \]
    %
    % If RMS output is requested, return instead
    % \[
    % SF_k^{\mathrm{RMS}} = \sqrt{\left\langle D^{\mathrm{int}}_{ij}\right\rangle}
    % \]
    %
    % For the effective-pairs error model:
    % \[
    % N_{\mathrm{eff},k} = \frac{N_{\mathrm{pair},k}^{2}}{\sum_r m_{r,k}^{2}}
    % \]
    %
    % For the delete-1-point jackknife:
    % \[
    % \mathrm{Var}_{\mathrm{jack}}(SF_k)
    % = \frac{N-1}{N}\sum_{r=1}^{N}
    % \left(SF_k^{(-r)}-\overline{SF_k^{(-)}}\right)^2
    % \]
    %
    % Example:
    %   Args.LagEdges      = logspace(-2,2,30);
    %   Args.SubtractNoise = true;
    %   Args.ErrorMethod   = 'jackknife';
    %   [MidLagVector, SF, SFerr, Npair, Result] = timeSeries.xcorr.structFun(Data, Args);
    %
    
    arguments
        Data (:,:) double
        Args.LagEdges (1,:) double = logspace(-2,1,20)
        Args.SubtractNoise (1,1) logical = true
        Args.ReturnRMS (1,1) logical = false
        Args.MinPairs (1,1) double {mustBeInteger,mustBePositive} = 1
        Args.NonNegative (1,1) logical = false
        Args.UseMedian (1,1) logical = false
        Args.ErrorMethod (1,1) string {mustBeMember(Args.ErrorMethod,["jackknife","neff","naive"])} = "jackknife"
        Args.JackknifeMinPairs (1,1) double {mustBeInteger,mustBePositive} = 1
    end
    
    Time = Data(:,1);
    Val  = Data(:,2);
    if size(Data,2) >= 3
        Err = Data(:,3);
    else
        Err = zeros(size(Val));
    end
    
    [Time, SI] = sort(Time);
    Val = Val(SI);
    Err = Err(SI);
    
    LagEdges = Args.LagEdges(:).';
    if numel(LagEdges) < 2
        error('Args.LagEdges must contain at least two elements.');
    end
    if any(diff(LagEdges) <= 0)
        error('Args.LagEdges must be strictly increasing.');
    end
    
    MidLagVector = 0.5 .* (LagEdges(1:end-1) + LagEdges(2:end));
    Nbin         = numel(MidLagVector);
    N            = numel(Time);
    
    Npair   = zeros(Nbin,1);
    SF      = nan(Nbin,1);
    SFerr   = nan(Nbin,1);
    
    SumObs   = zeros(Nbin,1);
    SumObs2  = zeros(Nbin,1);
    SumNoise = zeros(Nbin,1);
    SumInt   = zeros(Nbin,1);
    SumInt2  = zeros(Nbin,1);
    
    if Args.UseMedian
        BinObsValues   = cell(Nbin,1);
        BinNoiseValues = cell(Nbin,1);
        BinIntValues   = cell(Nbin,1);
    else
        BinObsValues   = [];
        BinNoiseValues = [];
        BinIntValues   = [];
    end
    
    NeedNeff = Args.ErrorMethod == "neff";
    if NeedNeff
        CountPerPoint = zeros(Nbin, N);
    else
        CountPerPoint = [];
    end
    
    DoJackknife = Args.ErrorMethod == "jackknife";
    if DoJackknife
        % Jackknife accumulators over delete-1 realizations
        JkSum   = zeros(Nbin, N);
        JkCount = zeros(Nbin, N);
    else
        JkSum   = [];
        JkCount = [];
    end
    
    UseNoiseSub = Args.SubtractNoise && any(Err ~= 0);
    
    for I1 = 1:(N-1)
        Dt = Time(I1+1:end) - Time(I1);
        Bin = discretize(Dt, LagEdges);
    
        Good = ~isnan(Bin) & isfinite(Val(I1)) & isfinite(Val(I1+1:end));
        if ~any(Good)
            continue;
        end
    
        Jrel = find(Good);
        Jg   = I1 + Jrel;
        Bg   = Bin(Good);
    
        Dobs = (Val(Jg) - Val(I1)).^2;
    
        if UseNoiseSub
            Dnoise = Err(I1).^2 + Err(Jg).^2;
        else
            Dnoise = zeros(size(Dobs));
        end
    
        Dint = Dobs - Dnoise;
    
        for Ipair = 1:numel(Bg)
            Ib = Bg(Ipair);
    
            Npair(Ib)   = Npair(Ib)   + 1;
            SumObs(Ib)  = SumObs(Ib)  + Dobs(Ipair);
            SumObs2(Ib) = SumObs2(Ib) + Dobs(Ipair).^2;
            SumNoise(Ib)= SumNoise(Ib)+ Dnoise(Ipair);
            SumInt(Ib)  = SumInt(Ib)  + Dint(Ipair);
            SumInt2(Ib) = SumInt2(Ib) + Dint(Ipair).^2;
    
            if Args.UseMedian
                BinObsValues{Ib}(end+1,1)   = Dobs(Ipair);   %#ok<AGROW>
                BinNoiseValues{Ib}(end+1,1) = Dnoise(Ipair); %#ok<AGROW>
                BinIntValues{Ib}(end+1,1)   = Dint(Ipair);   %#ok<AGROW>
            end
    
            if NeedNeff
                CountPerPoint(Ib, I1)      = CountPerPoint(Ib, I1)      + 1;
                CountPerPoint(Ib, Jg(Ipair)) = CountPerPoint(Ib, Jg(Ipair)) + 1;
            end
    
            if DoJackknife
                % This pair survives in all delete-1 realizations except when
                % removing endpoint I1 or Jg(Ipair).
                ValidR = true(1,N);
                ValidR(I1) = false;
                ValidR(Jg(Ipair)) = false;
    
                JkSum(Ib,ValidR)   = JkSum(Ib,ValidR)   + Dint(Ipair);
                JkCount(Ib,ValidR) = JkCount(Ib,ValidR) + 1;
            end
        end
    end
    
    SFobs       = nan(Nbin,1);
    SFnoise     = nan(Nbin,1);
    SFintrinsic = nan(Nbin,1);
    Neff        = nan(Nbin,1);
    
    for Ib = 1:Nbin
        if Npair(Ib) < Args.MinPairs
            continue;
        end
    
        if Args.UseMedian
            DobsVec   = BinObsValues{Ib};
            DnoiseVec = BinNoiseValues{Ib};
            DintVec   = BinIntValues{Ib};
    
            if isempty(DintVec)
                continue;
            end
    
            SFobs(Ib)       = median(DobsVec,   'omitnan');
            SFnoise(Ib)     = median(DnoiseVec, 'omitnan');
            SFintrinsic(Ib) = median(DintVec,   'omitnan');
        else
            SFobs(Ib)       = SumObs(Ib)   ./ Npair(Ib);
            SFnoise(Ib)     = SumNoise(Ib) ./ Npair(Ib);
            SFintrinsic(Ib) = SumInt(Ib)   ./ Npair(Ib);
        end
    
        if Args.NonNegative
            SFintrinsic(Ib) = max(SFintrinsic(Ib), 0);
        end
    
        if Args.ReturnRMS
            if isfinite(SFintrinsic(Ib)) && SFintrinsic(Ib) >= 0
                SF(Ib) = sqrt(SFintrinsic(Ib));
            else
                SF(Ib) = NaN;
            end
        else
            SF(Ib) = SFintrinsic(Ib);
        end
    
        if NeedNeff
            Den = sum(CountPerPoint(Ib,:).^2);
            if Den > 0
                Neff(Ib) = Npair(Ib).^2 ./ Den;
            end
        end
    end
    
    switch Args.ErrorMethod
        case "naive"
            for Ib = 1:Nbin
                if Npair(Ib) < max(Args.MinPairs,2)
                    continue;
                end
    
                if Args.UseMedian
                    Dvec = BinIntValues{Ib};
                    if isempty(Dvec)
                        continue;
                    end
                    VarD = var(Dvec, 'omitnan');
                else
                    VarD = SumInt2(Ib)./Npair(Ib) - (SumInt(Ib)./Npair(Ib)).^2;
                    VarD = max(VarD, 0);
                    VarD = VarD .* Npair(Ib) ./ max(Npair(Ib)-1,1);  % sample variance
                end
    
                ErrMean = sqrt(VarD ./ Npair(Ib));
                SFerr(Ib) = localTransformErr(SFintrinsic(Ib), ErrMean, Args.ReturnRMS, Args.NonNegative);
            end
    
        case "neff"
            for Ib = 1:Nbin
                if Npair(Ib) < max(Args.MinPairs,2) || ~(isfinite(Neff(Ib)) && Neff(Ib) > 0)
                    continue;
                end
    
                if Args.UseMedian
                    Dvec = BinIntValues{Ib};
                    if isempty(Dvec)
                        continue;
                    end
                    VarD = var(Dvec, 'omitnan');
                else
                    VarD = SumInt2(Ib)./Npair(Ib) - (SumInt(Ib)./Npair(Ib)).^2;
                    VarD = max(VarD, 0);
                    VarD = VarD .* Npair(Ib) ./ max(Npair(Ib)-1,1);  % sample variance
                end
    
                ErrMean = sqrt(VarD ./ Neff(Ib));
                SFerr(Ib) = localTransformErr(SFintrinsic(Ib), ErrMean, Args.ReturnRMS, Args.NonNegative);
            end
    
        case "jackknife"
            SFjk = nan(Nbin, N);
    
            for Irm = 1:N
                ValidBins = JkCount(:,Irm) >= max(Args.MinPairs, Args.JackknifeMinPairs);
    
                if Args.UseMedian
                    % Exact median jackknife would require storing all delete-1
                    % realizations explicitly. For UseMedian=true, fall back to
                    % delete-1 mean jackknife on Dint pairs.
                    warning('For Args.UseMedian=true, jackknife is calculated from delete-1 means, not medians.');
                end
    
                JkVal = nan(Nbin,1);
                JkVal(ValidBins) = JkSum(ValidBins,Irm) ./ JkCount(ValidBins,Irm);
    
                if Args.NonNegative
                    JkVal = max(JkVal, 0);
                end
    
                if Args.ReturnRMS
                    Good = isfinite(JkVal) & JkVal >= 0;
                    Tmp = nan(size(JkVal));
                    Tmp(Good) = sqrt(JkVal(Good));
                    JkVal = Tmp;
                end
    
                SFjk(:,Irm) = JkVal;
            end
    
            for Ib = 1:Nbin
                Good = isfinite(SFjk(Ib,:));
                Ngood = sum(Good);
    
                if Ngood >= 2 && Npair(Ib) >= Args.MinPairs
                    MeanJk = mean(SFjk(Ib,Good));
                    SFerr(Ib) = sqrt((Ngood - 1) ./ Ngood .* sum((SFjk(Ib,Good) - MeanJk).^2));
                end
            end
    
        otherwise
            error('Unknown Args.ErrorMethod option.');
    end
    
    Result = struct;
    Result.SFobs       = SFobs;
    Result.SFnoise     = SFnoise;
    Result.SFintrinsic = SFintrinsic;
    Result.Neff        = Neff;
    
    if DoJackknife
        Result.SFjk = SFjk;
    else
        Result.SFjk = [];
    end
    
    end
    
    
    function ErrOut = localTransformErr(BaseValue, ErrIn, ReturnRMS, NonNegative)
    % Convert error on second-order SF to error on returned quantity.
    
    if ~isfinite(ErrIn)
        ErrOut = NaN;
        return;
    end
    
    if ~ReturnRMS
        ErrOut = ErrIn;
        return;
    end
    
    if NonNegative
        BaseValue = max(BaseValue, 0);
    end
    
    if isfinite(BaseValue) && BaseValue > 0
        ErrOut = 0.5 .* ErrIn ./ sqrt(BaseValue);
    else
        ErrOut = NaN;
    end

end