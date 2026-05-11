function Result = compareCalibSpectra(PC, Args)
    % Compare calibrator spectra for Used vs NonUsed sub-populations
    % Description: Pools resampled calibrator spectra
    %              (PC(k).SpecData.SpecFluxMatrix) across all crops of
    %              one visit, normalises each spectrum at a pivot
    %              wavelength to isolate spectral SHAPE, splits by
    %              SourceData.Table.Used, and produces:
    %                (1) median + 16/84 percentile envelopes for U and NU
    %                    on the common wavelength grid (PC.TransWvl),
    %                (2) the residual (median_NU - median_U) / median_U,
    %                (3) a two-band colour proxy (Blue/Red) histogram for
    %                    U vs NU, with KS / MW / Cliffs delta stats.
    %
    %              All spectra are aligned column-by-column with the
    %              corresponding SourceData rows, so Used flags transfer
    %              directly. Spectra whose pivot value is non-positive or
    %              non-finite are dropped.
    %
    % Input  : - PC 1xNcrop PhotCalibTrans array, post-calibrate
    %            (SpecData.SpecFluxMatrix and SourceData.Used populated).
    %          * ...,key,val,...
    %            'Pivot'    - pivot wavelength [A]. Default 6000.
    %            'BlueBand' - [lo hi] in A. Default [4000 5500].
    %            'RedBand'  - [lo hi] in A. Default [5500 7000].
    %            'OutDir'   - directory for figures + Result.mat.
    %                         Default '~/results_calib_audit'.
    %            'Plot','SaveFig','Verbose' - logical defaults true.
    % Output : - Result struct:
    %            .Wvl                            common wavelength grid [A]
    %            .MedU, .P16U, .P84U             U  ensemble per-wvl
    %            .MedNU, .P16NU, .P84NU          NU ensemble per-wvl
    %            .Residual                       (Med_NU - Med_U)/Med_U
    %            .NU, .NNU                       sample sizes (after drops)
    %            .ColorRatio                     per-spectrum BlueFlux/RedFlux
    %            .UsedMask                       per-spectrum Used flag
    %            .ColorStats                     struct with KS_p, MW_p,
    %                                            CliffsDelta on ColorRatio
    %            .Args                           echo
    % Author : D. Kovaleva (May 2026)
    % Example: R = pipeline.last.quality.photCalib.compareCalibSpectra(PC);

    arguments
        PC (1,:)
        Args.Pivot    (1,1) double {mustBePositive} = 6000
        Args.BlueBand (1,2) double                  = [4000 5500]
        Args.RedBand  (1,2) double                  = [5500 7000]
        Args.OutDir   {mustBeText}                  = '~/results_calib_audit'
        Args.Plot     logical                       = true
        Args.SaveFig  logical                       = true
        Args.Verbose  logical                       = true
    end

    Ncrop = numel(PC);
    OutDir = char(Args.OutDir);
    if (Args.Plot || Args.SaveFig) && ~exist(OutDir, 'dir')
        mkdir(OutDir);
    end

    % -------- Phase 1: pool spectra ---------------------------------
    SCells = cell(1, Ncrop);
    UCells = cell(1, Ncrop);
    Wvl    = [];
    for K = 1:Ncrop
        if isempty(PC(K).SourceData) || ~PC(K).Success; continue; end
        if isempty(PC(K).SpecData)  || ~isfield(PC(K).SpecData, 'SpecFluxMatrix')
            continue;
        end
        S = PC(K).SpecData.SpecFluxMatrix;
        T = PC(K).SourceData.Table;
        if isempty(S) || ~ismember('Used', T.Properties.VariableNames); continue; end
        if size(S, 2) ~= height(T); continue; end
        SCells{K} = S;
        UCells{K} = logical(T.Used);
        if isempty(Wvl); Wvl = PC(K).TransWvl(:); end
    end
    Keep = ~cellfun(@isempty, SCells);
    if ~any(Keep) || isempty(Wvl)
        error('compareCalibSpectra:NoSpectra', ...
            'No PCs with populated SpecData.SpecFluxMatrix and Used.');
    end
    Spec = horzcat(SCells{Keep});
    Used = vertcat(UCells{Keep});
    if size(Spec, 2) ~= numel(Used)
        error('compareCalibSpectra:Shape', 'Spec/Used length mismatch.');
    end

    % -------- Phase 2: pivot normalisation --------------------------
    [~, IPivot] = min(abs(Wvl - Args.Pivot));
    PivotVals = Spec(IPivot, :);
    GoodCol = isfinite(PivotVals) & PivotVals > 0;
    Spec  = Spec(:, GoodCol);
    Used  = Used(GoodCol);
    Spec  = Spec ./ PivotVals(GoodCol);   % shape-only spectra

    if Args.Verbose
        fprintf('compareCalibSpectra: %d spectra (%d U, %d NU) on %d wvl bins\n', ...
            size(Spec, 2), sum(Used), sum(~Used), numel(Wvl));
    end

    % -------- Phase 3: per-wvl U / NU aggregates --------------------
    SU  = Spec(:,  Used);
    SNU = Spec(:, ~Used);
    [MedU,  P16U,  P84U]  = bandStats(SU);
    [MedNU, P16NU, P84NU] = bandStats(SNU);
    Residual = (MedNU - MedU) ./ MedU;

    % -------- Phase 4: colour ratio + stats -------------------------
    BlueFlux = bandFlux(Wvl, Spec, Args.BlueBand);
    RedFlux  = bandFlux(Wvl, Spec, Args.RedBand);
    ColorRatio = (BlueFlux ./ RedFlux).';   % column vector, aligned with Used

    Cu = ColorRatio( Used); Cu = Cu(isfinite(Cu));
    Cn = ColorRatio(~Used); Cn = Cn(isfinite(Cn));
    Stats = struct('N_U', numel(Cu), 'N_NU', numel(Cn), ...
        'MedU', median(Cu), 'MedNU', median(Cn), ...
        'KS_p', NaN, 'MW_p', NaN, 'CliffsDelta', NaN);
    if numel(Cu) >= 5 && numel(Cn) >= 5
        try [~, Stats.KS_p] = kstest2(Cu, Cn); catch; end
        try Stats.MW_p = ranksum(Cu, Cn);     catch; end
        Stats.CliffsDelta = cliffsDelta(Cu, Cn);
    end

    % -------- Result ------------------------------------------------
    Result.Wvl       = Wvl;
    Result.MedU      = MedU;   Result.P16U  = P16U;  Result.P84U  = P84U;
    Result.MedNU     = MedNU;  Result.P16NU = P16NU; Result.P84NU = P84NU;
    Result.Residual  = Residual;
    Result.NU        = sum(Used);
    Result.NNU       = sum(~Used);
    Result.ColorRatio = ColorRatio;
    Result.UsedMask   = Used;
    Result.ColorStats = Stats;
    Result.Args       = Args;

    if Args.SaveFig
        try
            save(fullfile(OutDir, 'compareCalibSpectra.mat'), 'Result', '-v7.3');
        catch ME
            warning('compareCalibSpectra:SaveFailed', ...
                'Save failed: %s', ME.message);
        end
    end

    if Args.Verbose
        fprintf('  Color ratio (Blue %g-%g / Red %g-%g):\n', ...
            Args.BlueBand, Args.RedBand);
        fprintf('    U  : N=%d  med=%.4g\n', Stats.N_U,  Stats.MedU);
        fprintf('    NU : N=%d  med=%.4g\n', Stats.N_NU, Stats.MedNU);
        fprintf('    KS_p=%.3g  MW_p=%.3g  Cliffs=%+.3f\n', ...
            Stats.KS_p, Stats.MW_p, Stats.CliffsDelta);
    end

    % -------- Phase 5: plots ----------------------------------------
    if Args.Plot
        plotMedianSpectra(Result, Args, OutDir);
        plotResidual(Result, Args, OutDir);
        plotColorHist(Result, Args, OutDir);
    end
end

% =========================================================================
function [Med, P16, P84] = bandStats(M)
    if isempty(M)
        Med = nan(0, 1); P16 = Med; P84 = Med; return;
    end
    Med = median(M, 2, 'omitnan');
    P16 = prctile(M, 16, 2);
    P84 = prctile(M, 84, 2);
end

% =========================================================================
function F = bandFlux(Wvl, Spec, Band)
    InBand = Wvl >= Band(1) & Wvl <= Band(2);
    if nnz(InBand) < 2
        F = nan(1, size(Spec, 2)); return;
    end
    F = trapz(Wvl(InBand), Spec(InBand, :), 1);
end

% =========================================================================
function d = cliffsDelta(a, b)
    a = a(:); b = b(:);
    if isempty(a) || isempty(b); d = NaN; return; end
    R = tiedrank([a; b]);
    Ra = sum(R(1:numel(a)));
    n_a = numel(a); n_b = numel(b);
    U = Ra - n_a * (n_a + 1) / 2;
    d = 2 * U / (n_a * n_b) - 1;
end

% =========================================================================
function plotMedianSpectra(R, Args, OutDir)
    F = figure('Name', 'compareCalibSpectra: median spectra', ...
        'Position', [80 80 950 600]);
    hold on;
    % NU: keep light-grey filled 16-84 band.
    bandShade(R.Wvl, R.P16NU, R.P84NU, [0.85 0.85 0.85]);
    % U: outline-only (thin lines for P16 and P84) so the two envelopes
    % don't visually compete.
    bandOutline(R.Wvl, R.P16U, R.P84U, [0.30 0.30 0.30]);
    plot(R.Wvl, R.MedNU, '-', 'Color', [0.85 0.20 0.20], 'LineWidth', 2.0, ...
        'DisplayName', sprintf('NU median (N=%d)', R.NNU));
    plot(R.Wvl, R.MedU,  '-', 'Color', [0.10 0.10 0.10], 'LineWidth', 1.5, ...
        'DisplayName', sprintf('U median (N=%d)',  R.NU));
    xlabel('Wavelength [A]', 'Interpreter', 'none');
    ylabel(sprintf('Normalised flux (pivot %g A)', Args.Pivot), ...
        'Interpreter', 'none');
    title('Calibrator spectra: U vs NU (median + 16-84% band)');
    legend('Location', 'best');
    grid on; box on;
    if Args.SaveFig
        savefig(F, fullfile(OutDir, 'spec_median.fig'));
        saveas(F,  fullfile(OutDir, 'spec_median.png'));
    end
end

% =========================================================================
function plotResidual(R, Args, OutDir)
    F = figure('Name', 'compareCalibSpectra: residual', ...
        'Position', [80 80 950 380]);
    hold on;
    plot(R.Wvl, R.Residual, '-', 'Color', [0.85 0.20 0.20], 'LineWidth', 1.5, ...
        'DisplayName', '(NU - U) / U');
    yline(0, '-', 'Color', [0.5 0.5 0.5], 'HandleVisibility', 'off');
    xlabel('Wavelength [A]', 'Interpreter', 'none');
    ylabel('Fractional residual', 'Interpreter', 'none');
    title('Median spectral residual: NU vs U (pivot-normalised shape)');
    legend('Location', 'best');
    grid on; box on;
    if Args.SaveFig
        savefig(F, fullfile(OutDir, 'spec_residual.fig'));
        saveas(F,  fullfile(OutDir, 'spec_residual.png'));
    end
end

% =========================================================================
function plotColorHist(R, Args, OutDir)
    F = figure('Name', 'compareCalibSpectra: colour histogram', ...
        'Position', [80 80 760 520]);
    hold on;
    Cu = R.ColorRatio( R.UsedMask); Cu = Cu(isfinite(Cu));
    Cn = R.ColorRatio(~R.UsedMask); Cn = Cn(isfinite(Cn));
    if isempty(Cu) && isempty(Cn); return; end
    Combined = [Cu; Cn];
    Edges = linspace(min(Combined), max(Combined), 40);
    histogram(Cn, Edges, 'FaceColor', [0.85 0.85 0.85], ...
        'EdgeColor', 'k', 'LineWidth', 2.0, ...
        'Normalization', 'probability', ...
        'DisplayName', sprintf('NU (N=%d)', numel(Cn)));
    histogram(Cu, Edges, 'FaceColor', [0.40 0.40 0.40], ...
        'FaceAlpha', 0.85, 'EdgeColor', [0.25 0.25 0.25], ...
        'LineWidth', 0.5, ...
        'Normalization', 'probability', ...
        'DisplayName', sprintf('U (N=%d)', numel(Cu)));
    xlabel(sprintf('Blue [%g-%g A] / Red [%g-%g A]', ...
        Args.BlueBand, Args.RedBand), 'Interpreter', 'none');
    ylabel('Fraction', 'Interpreter', 'none');
    title(sprintf('Colour ratio (KS p=%.2g, Cliffs=%+.3f)', ...
        R.ColorStats.KS_p, R.ColorStats.CliffsDelta));
    legend('Location', 'best');
    grid on; box on;
    if Args.SaveFig
        savefig(F, fullfile(OutDir, 'spec_colour_hist.fig'));
        saveas(F,  fullfile(OutDir, 'spec_colour_hist.png'));
    end
end

% =========================================================================
function bandShade(X, Lo, Hi, Color)
    Fin = isfinite(Lo) & isfinite(Hi);
    if ~any(Fin); return; end
    Xs = X(Fin); Ls = Lo(Fin); Hs = Hi(Fin);
    fill([Xs; flipud(Xs)], [Ls; flipud(Hs)], Color, ...
        'EdgeColor', 'none', 'FaceAlpha', 0.6, ...
        'HandleVisibility', 'off');
end

% =========================================================================
function bandOutline(X, Lo, Hi, Color)
    % Draw only the P16 and P84 curves as thin lines (no fill).
    Fin = isfinite(Lo) & isfinite(Hi);
    if ~any(Fin); return; end
    plot(X(Fin), Lo(Fin), '-', 'Color', Color, 'LineWidth', 0.7, ...
        'HandleVisibility', 'off');
    plot(X(Fin), Hi(Fin), '-', 'Color', Color, 'LineWidth', 0.7, ...
        'HandleVisibility', 'off');
end
