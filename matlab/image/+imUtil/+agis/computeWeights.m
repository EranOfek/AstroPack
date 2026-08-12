function W = computeWeights(Data, Rx, Ry, Args)
    % Compute per-observation weights from empirical, magnitude-binned
    % astrometric scatter, with outlier down-weighting (Segev et al. 2025, S6.1).
    % Input  : - Data : struct from imUtil.agis.buildFitData (needs .Mag, .JD).
    %          - Rx, Ry : [Nepoch x Nsrc] current residuals.
    %          * ...,key,val,...
    %            'MinUncertainty'            - floor on sigma [pix or mas]. Default 2/400.
    %            'MagBinWidth'                - magnitude bin width. Default 1.
    %            'PosOutlierThresholdFactor'  - isoutlier threshold on Rx/Ry. Default 3.
    %            'MagOutlierThresholdFactor'  - isoutlier threshold on Mag. Default 1.5.
    %            'MovMedianWindow'            - moving-median window (# epochs). Default 50.
    %            'RmsOutlierMadFactor'        - source flagged if RMS > median+factor*MAD. Default 3.
    %            'RmsOutlierDownweight'       - weight divisor for flagged sources. Default 10.
    %            'Normalize'                  - normalize sum(W(:))==1. Default true.
    %            'UseWeights'                 - if false, return uniform weights. Default true.
    % Output : - W : [Nepoch x Nsrc] weight matrix (0 for excluded observations).
    % Author : N. Segev / imUtil.agis rewrite
    % Example: W = imUtil.agis.computeWeights(Data,Rx,Ry);

    arguments
        Data (1,1) struct
        Rx (:,:) double
        Ry (:,:) double
        Args.MinUncertainty (1,1) double = 2/400
        Args.MagBinWidth (1,1) double = 1
        Args.PosOutlierThresholdFactor (1,1) double = 3
        Args.MagOutlierThresholdFactor (1,1) double = 1.5
        Args.MovMedianWindow (1,1) double = 50
        Args.RmsOutlierMadFactor (1,1) double = 3
        Args.RmsOutlierDownweight (1,1) double = 10
        Args.Normalize (1,1) logical = true
        Args.UseWeights (1,1) logical = true
    end

    if ~Args.UseWeights
        W = ones(size(Rx));
        return
    end

    R2D = sqrt(Rx.^2 + Ry.^2);

    try
        OutlierPos = isoutlier(Rx, 'movmedian', Args.MovMedianWindow, ...
                        'ThresholdFactor', Args.PosOutlierThresholdFactor, 'SamplePoints', Data.JD) | ...
                     isoutlier(Ry, 'movmedian', Args.MovMedianWindow, ...
                        'ThresholdFactor', Args.PosOutlierThresholdFactor, 'SamplePoints', Data.JD);

        OutlierMag = isoutlier(Data.Mag, 'movmedian', Args.MovMedianWindow, ...
                        'ThresholdFactor', Args.MagOutlierThresholdFactor, 'SamplePoints', Data.JD);

        Good = ~isnan(R2D) & ~OutlierPos & ~OutlierMag;

        % --- magnitude-binned, per-epoch scatter (Eq. 14)
        MedMag = median(Data.Mag, 1, 'omitnan');                 % [1 x Nsrc]
        Edges  = floor(min(MedMag)) : Args.MagBinWidth : (ceil(max(MedMag)) + Args.MagBinWidth);
        [~, ~, BinId] = histcounts(MedMag, Edges);
        BinId(BinId == 0) = 1;
        NBins = max(BinId);

        Sigma = nan(Data.Nepoch, Data.Nsrc);
        for Ib = 1:NBins
            Cols = (BinId == Ib);
            if ~any(Cols)
                continue
            end
            SubR = R2D(:, Cols);
            SubR(~Good(:, Cols)) = NaN;
            Sigma(:, Cols) = repmat(median(SubR, 2, 'omitnan'), 1, sum(Cols));
        end

        % --- per-source RMS outlier down-weighting
        RmsSrc = sqrt(median(Rx.^2, 1, 'omitnan') + median(Ry.^2, 1, 'omitnan'));
        MedRms = median(RmsSrc, 'omitnan');
        MadRms = mad(RmsSrc(~isnan(RmsSrc)), 1);
        SrcOutlier = RmsSrc > (MedRms + Args.RmsOutlierMadFactor * MadRms);

        Sigma(Sigma < Args.MinUncertainty) = Args.MinUncertainty;

        W = 1 ./ Sigma.^2;
        W(:, SrcOutlier) = W(:, SrcOutlier) / Args.RmsOutlierDownweight;
        W(~Good) = 0;
        W(isnan(W)) = 0;

        if Args.Normalize
            SumW = sum(W(:));
            if SumW > 0
                W = W ./ SumW;
            end
        end
    catch ME
        warning('imUtil:agis:computeWeights:failed', ...
            'Weight calculation failed (%s); using uniform weights.', ME.message);
        W = ones(size(Rx));
    end
end
