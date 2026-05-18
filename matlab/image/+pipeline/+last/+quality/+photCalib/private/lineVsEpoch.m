function H = lineVsEpoch(Ax, EpochVec, Mat, Args)
    % Draw per-crop quantity-vs-epoch lines with a median overlay.
    % Description: Plots one line per crop column of Mat against EpochVec —
    %              central crops as bold solid lines, peripheral crops as thin
    %              dashed lines — and overlays the cross-crop median as a
    %              thick black line. Shared time-series primitive for the
    %              vs-epoch plotters.
    % Input  : - Ax       - target axes handle.
    %          - EpochVec - epoch axis values (length Nepoch).
    %          - Mat      - [Nepoch x Ncrop] matrix of the plotted quantity.
    %          * ...,key,val,...
    %            'CropsToUse'   - Crop columns to draw. Default [] (all).
    %            'CentralCrops' - Crop indices drawn bold/solid. Default [].
    %            'ShowMedian'   - Overlay the cross-crop median. Default true.
    %            'Color'        - [] (default) draws per-crop lines from the
    %                             lines() colormap with a black median; an
    %                             RGB row draws every line in that single
    %                             colour (for a grey background overlay).
    %            'MedianWidth'  - Median line width. Default 3.5.
    % Output : - H - column of line handles (per-crop lines, then the median
    %            line last when drawn).
    % Author : photCalib package refactor (2026-05)
    % Example: H = lineVsEpoch(gca, 1:Nv, ParMat, 'CentralCrops', C);

    arguments
        Ax
        EpochVec
        Mat
        Args.CropsToUse   double  = []
        Args.CentralCrops double  = []
        Args.ShowMedian   logical = true
        Args.Color                = []
        Args.MedianWidth  (1,1) double = 3.5
    end

    EpochVec = EpochVec(:);
    Ncrop    = size(Mat, 2);

    Crops = Args.CropsToUse;
    if isempty(Crops)
        Crops = 1:Ncrop;
    end
    Crops = Crops(Crops >= 1 & Crops <= Ncrop);

    hold(Ax, 'on');
    if isempty(Args.Color)
        Cmap   = lines(max(numel(Crops), 1));
        MedCol = [0 0 0];
    else
        Cmap   = repmat(Args.Color(:).', max(numel(Crops), 1), 1);
        MedCol = Args.Color * 0.7;
    end
    H = gobjects(numel(Crops) + double(Args.ShowMedian), 1);

    for Ii = 1:numel(Crops)
        Ic = Crops(Ii);
        if ismember(Ic, Args.CentralCrops)
            H(Ii) = plot(Ax, EpochVec, Mat(:,Ic), '-', 'LineWidth', 1.5, ...
                'Color', [Cmap(Ii,:) 0.7]);
        else
            H(Ii) = plot(Ax, EpochVec, Mat(:,Ic), '--', 'LineWidth', 0.5, ...
                'Color', [Cmap(Ii,:) 0.3]);
        end
    end

    if Args.ShowMedian
        Med = median(Mat(:,Crops), 2, 'omitnan');
        H(end) = plot(Ax, EpochVec, Med, '-', 'Color', MedCol, ...
            'LineWidth', Args.MedianWidth);
    end

    box(Ax, 'on');
    grid(Ax, 'on');
end
