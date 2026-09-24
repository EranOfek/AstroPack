function Result = compareTran2D(PC_A, PC_B, Args)
    % Compare position-dependent transmission surfaces of two PC arrays
    % Description: For each crop in two paired PhotCalibTrans arrays
    %              (one per crop, paired index-by-index), evaluates the
    %              transmission at a fixed reference wavelength on a
    %              GridSize x GridSize (X,Y) grid covering the crop, and
    %              reports per-crop scalar metrics on the pointwise
    %              difference D = T_A - T_B between the two surfaces.
    %              Comparing surface VALUES (not raw polynomial
    %              coefficients) is the meaningful test, since the same
    %              surface can be expressed by very different coefficient
    %              vectors in different bases.
    %
    %              Metrics computed per crop (default subset
    %              {'RMS','Max','Med'}; the other three are still
    %              available via 'Metrics'):
    %                'RMS'    sqrt(mean(D.^2))
    %                'Max'    max(abs(D))
    %                'Med'    median(abs(D))
    %                'P84'    prctile(abs(D), 84)
    %                'L2_rel' RMS / sqrt(mean(T_A.^2))     (unitless)
    %                'R2'     1 - var(D)/var(T_A)         (1 = identical)
    %
    %              Units: T comes from
    %              CompositeFun.evaluateWithPosition, i.e. dimensionless
    %              transmission (~0..1). It is NOT in magnitudes. To
    %              convert a small transmission difference to mag,
    %              Delta_mag ~= 1.086 * (T_A - T_B) / T_A.
    %
    %              Crops where either PC has no Tran2DObj or evaluation
    %              fails get NaN metrics.
    %
    % Input  : - PC_A  - 1xNcrop PhotCalibTrans array (set A).
    %          - PC_B  - 1xNcrop PhotCalibTrans array (set B), aligned
    %                    crop-for-crop with PC_A.
    %                    * ...,key,val,...
    %                      'GridSize'   - Grid resolution per axis.
    %                                     Default 50.
    %                      'RefWvl'     - Reference wavelength in Angstrom
    %                                     for transmission evaluation.
    %                                     Default 6000.
    %                      'XRange'     - [xmin xmax] in pixels. Default
    %                                     [] (uses ImageSize).
    %                      'YRange'     - [ymin ymax] in pixels. Default
    %                                     [] (uses ImageSize).
    %                      'ImageSize'  - [Y X] image size in pixels used
    %                                     to build the grid when XRange/
    %                                     YRange are empty. Default
    %                                     [1726 1726] (LAST crop).
    %                      'Metrics'    - Cell of metric names to compute.
    %                                     Subset of {'RMS','Max','Med',
    %                                     'P84','L2_rel','R2'}.
    %                                     Default: all.
    %                      'OutDir'     - Directory for figure / .mat.
    %                                     Default '~/results_calib_audit'.
    %                      'Plot'       - Default true.
    %                      'SaveFig'    - Default true.
    %                      'Verbose'    - Default true.
    % Output : - Result struct:
    %            .Metrics  - table with columns {'Crop', requested
    %                        metrics}, one row per crop.
    %            .Args     - echo of arguments.
    % Author : D. Kovaleva (May 2026)
    % Example: % Compare two calibration runs on the same visit
    %          % (default 50x50 grid, 6000 A pivot, all six metrics):
    %          R = pipeline.last.quality.photCalib.compareTran2D(PC_A, PC_B);
    %          disp(R.Metrics);                       % 24-row table
    %
    %          % Finer grid and a redder pivot wavelength:
    %          R = pipeline.last.quality.photCalib.compareTran2D(PC_A, PC_B, ...
    %                  'GridSize', 100, 'RefWvl', 7000);
    %
    %          % Only the two metrics I care about:
    %          R = pipeline.last.quality.photCalib.compareTran2D(PC_A, PC_B, ...
    %                  'Metrics', {'RMS','L2_rel'});
    %
    %          % Single visit-level summary number from the per-crop table:
    %          fprintf('Median RMS across crops = %.4g\n', ...
    %                  median(R.Metrics.RMS, 'omitnan'));
    %          fprintf('Median fractional diff  = %.3g %%\n', ...
    %                  100 * median(R.Metrics.L2_rel, 'omitnan'));
    %
    %          % Restrict the grid to the central science region (drop a
    %          % 50-pixel edge margin known to be polynomial-extrapolated):
    %          R = pipeline.last.quality.photCalib.compareTran2D(PC_A, PC_B, ...
    %                  'XRange', [50 1676], 'YRange', [50 1676]);
    %
    %          % Headless run (no figure, no .mat save) for a batch script:
    %          R = pipeline.last.quality.photCalib.compareTran2D(PC_A, PC_B, ...
    %                  'Plot', false, 'SaveFig', false, 'Verbose', false);

    arguments
        PC_A (1,:)
        PC_B (1,:)
        Args.GridSize    (1,1) double {mustBePositive, mustBeInteger} = 50
        Args.RefWvl      (1,1) double {mustBePositive}                = 6000
        Args.XRange                                                   = []
        Args.YRange                                                   = []
        Args.ImageSize   (1,2) double                                 = [1726 1726]
        Args.Metrics     cell                                         = ...
            {'RMS','Max','Med'}
        Args.OutDir      {mustBeText}                                 = '~/results_calib_audit'
        Args.Plot        logical                                      = true
        Args.SaveFig     logical                                      = true
        Args.Verbose     logical                                      = true
    end

    Ncrop = numel(PC_A);
    if numel(PC_B) ~= Ncrop
        error('compareTran2D:Mismatch', ...
            'PC_A (%d) and PC_B (%d) must have the same length.', ...
            Ncrop, numel(PC_B));
    end

    AllowedMetrics = {'RMS','Max','Med','P84','L2_rel','R2'};
    Bad = setdiff(Args.Metrics, AllowedMetrics);
    if ~isempty(Bad)
        error('compareTran2D:UnknownMetric', ...
            'Unknown metric(s): %s. Allowed: %s', ...
            strjoin(Bad, ', '), strjoin(AllowedMetrics, ', '));
    end
    Names = Args.Metrics(:).';

    % Build the (X,Y) grid (reused across crops).
    XR = Args.XRange;  if isempty(XR); XR = [1, Args.ImageSize(2)]; end
    YR = Args.YRange;  if isempty(YR); YR = [1, Args.ImageSize(1)]; end
    Xv = linspace(XR(1), XR(2), Args.GridSize);
    Yv = linspace(YR(1), YR(2), Args.GridSize);
    [Xg, Yg] = meshgrid(Xv, Yv);
    Xc = Xg(:);  Yc = Yg(:);

    M = nan(Ncrop, numel(Names));

    for I = 1:Ncrop
        if isempty(PC_A(I).TransModel) || isempty(PC_B(I).TransModel); continue; end
        try
            TA = PC_A(I).TransModel.evaluateWithPosition(Args.RefWvl, Xc, Yc);
            TB = PC_B(I).TransModel.evaluateWithPosition(Args.RefWvl, Xc, Yc);
        catch ME
            if Args.Verbose
                fprintf('  crop %d: evaluateWithPosition failed (%s)\n', ...
                    I, ME.message);
            end
            continue;
        end
        TA = TA(:);  TB = TB(:);
        D  = TA - TB;
        Da = abs(D);

        for J = 1:numel(Names)
            switch Names{J}
                case 'RMS', M(I, J) = sqrt(mean(D.^2));
                case 'Max', M(I, J) = max(Da);
                case 'Med', M(I, J) = median(Da);
                case 'P84', M(I, J) = prctile(Da, 84);
                case 'L2_rel'
                    Denom = sqrt(mean(TA.^2));
                    if Denom > 0
                        M(I, J) = sqrt(mean(D.^2)) / Denom;
                    end
                case 'R2'
                    Vt = var(TA);
                    if Vt > 0
                        M(I, J) = 1 - var(D) / Vt;
                    end
            end
        end
    end

    Tbl = array2table(M, 'VariableNames', Names);
    Tbl.Crop = (1:Ncrop).';
    Tbl = movevars(Tbl, 'Crop', 'Before', 1);

    Result.Metrics = Tbl;
    Result.Args    = Args;

    if Args.Verbose
        fprintf('compareTran2D: Ncrop=%d, GridSize=%dx%d, RefWvl=%.0f A\n', ...
            Ncrop, Args.GridSize, Args.GridSize, Args.RefWvl);
        disp(Tbl);
    end

    OutDir = char(Args.OutDir);
    if Args.SaveFig
        if ~exist(OutDir, 'dir'); mkdir(OutDir); end
        try
            save(fullfile(OutDir, 'compareTran2D.mat'), 'Result', '-v7.3');
        catch ME
            warning('compareTran2D:SaveFailed', 'Save failed: %s', ME.message);
        end
    end

    if Args.Plot
        plotMetrics(Result, OutDir);
    end
end

% =========================================================================
function plotMetrics(R, OutDir)
    % Per-metric histogram across crops (one panel per metric).
    Names = R.Metrics.Properties.VariableNames;
    Names = Names(~strcmp(Names, 'Crop'));
    Nm = numel(Names);
    if Nm == 0; return; end
    Ncols = ceil(sqrt(Nm));
    Nrows = ceil(Nm / Ncols);
    F = figure('Name', 'compareTran2D', 'Position', [60 60 1400 800]);
    for I = 1:Nm
        subplot(Nrows, Ncols, I);
        V = R.Metrics.(Names{I});
        V = V(isfinite(V));
        if isempty(V)
            title(sprintf('%s (no data)', Names{I}), 'Interpreter', 'none');
            continue;
        end
        histogram(V, 'FaceColor', [0.40 0.40 0.40], ...
            'EdgeColor', [0.20 0.20 0.20]);
        xlabel(Names{I}, 'Interpreter', 'none');
        ylabel('Number of crops');
        title(sprintf('%s   med=%.4g  std=%.3g', ...
            Names{I}, median(V), std(V)), 'Interpreter', 'none');
        grid on; box on;
    end
    sgtitle(sprintf(['Tran2D pointwise difference at %.0f A ', ...
        '(%dx%d grid)'], R.Args.RefWvl, R.Args.GridSize, R.Args.GridSize), ...
        'Interpreter', 'none');
    if R.Args.SaveFig
        if ~exist(OutDir, 'dir'); mkdir(OutDir); end
        savefig(F, fullfile(OutDir, 'compareTran2D.fig'));
        saveas(F,  fullfile(OutDir, 'compareTran2D.png'));
    end
end
