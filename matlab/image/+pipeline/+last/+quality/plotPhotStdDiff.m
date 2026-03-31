function plotPhotStdDiff(MS, Args)
    % Plot epoch-to-epoch std difference: percrop vs other modes
    % Description: For each magnitude field, plots Std(percrop)-Std(other)
    %              vs magnitude. Points > 0 mean the non-percrop mode is better.
    %              Uses timeSeries.bin.binningFast for trend lines.
    %
    % Input  : - MS struct with MS.(mode){crop} = MatchedSources
    %            (from matchPhotEpochs).
    %          * ...,key,val,...
    %            'Modes'       - Cell array of modes (must include 'percrop').
    %            'MagFields'   - AB magnitude columns. Default is {'MAG_AB_PSF','MAG_AB_APER_3'}.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all available).
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'TrendBinWidth'- Bin width [mag]. Default is 0.5.
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotStdDiff(MS, 'Modes', {'percrop','perimage'});

    arguments
        MS struct
        Args.Modes cell
        Args.MagFields      = {'MAG_AB_PSF', 'MAG_AB_APER_3'}
        Args.CompareFields cell = {}  % e.g., {'MAG_AB_PSF','MAG_CB_PSF'} — compare fields within same mode
        Args.CropsToAnalyze = []
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.5
        Args.MinEpochs      = 0    % Min non-NaN epochs per source; 0 = no filter
    end

    % Two modes: compare modes (original) or compare fields within a mode
    if ~isempty(Args.CompareFields) && numel(Args.CompareFields) == 2
        plotFieldDiff(MS, Args);
        return;
    end

    if ~ismember('percrop', Args.Modes) || ~isfield(MS, 'percrop') || numel(Args.Modes) < 2
        return;
    end

    OtherModes = setdiff(Args.Modes, {'percrop'}, 'stable');
    Nother = numel(OtherModes);

    for Imf = 1:numel(Args.MagFields)
        MagField = Args.MagFields{Imf};

        figure('Name', sprintf('Std difference — %s', MagField), ...
               'Position', [50, 50, 400*Nother, 500]);

        for Io = 1:Nother
            Mode = OtherModes{Io};
            if ~isfield(MS, Mode); continue; end
            AllMedMag = [];
            AllDeltaStd = [];

            CropsToUse = Args.CropsToAnalyze;
            if isempty(CropsToUse)
                CropsToUse = 1:max(numel(MS.percrop), numel(MS.(Mode)));
            end

            for Ic = CropsToUse
                if Ic > numel(MS.percrop) || isempty(MS.percrop{Ic})
                    continue;
                end
                if Ic > numel(MS.(Mode)) || isempty(MS.(Mode){Ic})
                    continue;
                end

                MS_pc = MS.percrop{Ic};
                MS_other = MS.(Mode){Ic};

                if ~isfield(MS_pc.Data, MagField) || ~isfield(MS_other.Data, MagField)
                    continue;
                end

                Nsrc = min(MS_pc.Nsrc, MS_other.Nsrc);
                Mag_pc    = MS_pc.Data.(MagField)(:, 1:Nsrc);
                Mag_other = MS_other.Data.(MagField)(:, 1:Nsrc);

                % Filter sources with too few valid epochs
                if Args.MinEpochs > 0
                    Good = sum(~isnan(Mag_pc), 1) >= Args.MinEpochs & ...
                           sum(~isnan(Mag_other), 1) >= Args.MinEpochs;
                    Mag_pc    = Mag_pc(:, Good);
                    Mag_other = Mag_other(:, Good);
                end

                Std_pc    = nanstd(Mag_pc, 0, 1);
                Std_other = nanstd(Mag_other, 0, 1);
                MedMag    = nanmedian(Mag_pc, 1);

                AllMedMag = [AllMedMag, MedMag];
                AllDeltaStd = [AllDeltaStd, Std_pc - Std_other];
            end

            subplot(1, Nother, Io);
            if ~isempty(AllMedMag)
                plot(AllMedMag, AllDeltaStd, '.', 'MarkerSize', 4);
                hold on;
                plot(xlim, [0 0], 'k--');
                if ~strcmp(Args.OverlayTrend, 'none')
                    TrendFun = str2func(['nan' Args.OverlayTrend]);
                    R = timeSeries.bin.binningFast([AllMedMag(:), AllDeltaStd(:)], ...
                        Args.TrendBinWidth, [9 22], {'MidBin', @numel, TrendFun});
                    ValidBins = R(:,2) >= 5;
                    plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);

                    % Annotate min/max trend values with reference bin
                    TrendVals = R(ValidBins, 3);
                    TrendMags = R(ValidBins, 1);
                    TrendCounts = R(ValidBins, 2);
                    if ~isempty(TrendVals)
                        [MaxVal, MaxIdx] = max(TrendVals);
                        [MinVal, MinIdx] = min(TrendVals);
                        text(0.02, 0.97, sprintf('max: %.4f @ mag %.1f (N=%d)\nmin: %.4f @ mag %.1f (N=%d)', ...
                            MaxVal, TrendMags(MaxIdx), TrendCounts(MaxIdx), ...
                            MinVal, TrendMags(MinIdx), TrendCounts(MinIdx)), ...
                            'Units', 'normalized', 'VerticalAlignment', 'top', ...
                            'FontSize', 8, 'BackgroundColor', 'w');
                    end

                    % Print bin counts to console
                    fprintf('  %s vs percrop — %s bins:\n', Mode, MagField);
                    for Ib = 1:size(R, 1)
                        if R(Ib, 2) > 0
                            fprintf('    mag %.1f: N=%d, trend=%.5f\n', R(Ib,1), R(Ib,2), R(Ib,3));
                        end
                    end
                end
            end
            box on; grid on;
            xlabel('Median Magnitude');
            ylabel(sprintf('Std(percrop) - Std(%s) [mag]', Mode));
            xlim([9 22]);
            title(sprintf('percrop - %s  (%d sources)', Mode, numel(AllDeltaStd)));
        end
        sgtitle(sprintf('Std difference (>0 = non-percrop better): %s', ...
            strrep(MagField, '_', '\_')), 'FontSize', 11);
        % Shrink subplots to make room for sgtitle above Y exponent
        for Isub = 1:Nother
            ax = subplot(1, Nother, Isub);
            ax.Position(4) = ax.Position(4) * 0.92;
        end
    end
end

% =========================================================================
function plotFieldDiff(MS, Args)
    % Compare Std of two magnitude fields within the same mode
    % e.g., Std(MAG_AB_PSF) - Std(MAG_CB_PSF)
    FieldA = Args.CompareFields{1};
    FieldB = Args.CompareFields{2};
    Mode = Args.Modes{1};

    if ~isfield(MS, Mode); return; end

    CropsToUse = Args.CropsToAnalyze;
    if isempty(CropsToUse)
        CropsToUse = 1:numel(MS.(Mode));
    end

    AllMedMag = [];
    AllDeltaStd = [];

    for Ic = CropsToUse
        if Ic > numel(MS.(Mode)) || isempty(MS.(Mode){Ic}); continue; end
        MSobj = MS.(Mode){Ic};
        if ~isfield(MSobj.Data, FieldA) || ~isfield(MSobj.Data, FieldB); continue; end

        MagA = MSobj.Data.(FieldA);
        MagB = MSobj.Data.(FieldB);

        if Args.MinEpochs > 0
            Good = sum(~isnan(MagA), 1) >= Args.MinEpochs & ...
                   sum(~isnan(MagB), 1) >= Args.MinEpochs;
            MagA = MagA(:, Good);
            MagB = MagB(:, Good);
        end

        StdA = nanstd(MagA, 0, 1);
        StdB = nanstd(MagB, 0, 1);
        MedMag = nanmedian(MagA, 1);

        AllMedMag = [AllMedMag, MedMag];
        AllDeltaStd = [AllDeltaStd, StdA - StdB];
    end

    % Extract short names for labels (e.g., 'AB' and 'CB')
    TokA = strsplit(FieldA, '_'); LabelA = TokA{2};
    TokB = strsplit(FieldB, '_'); LabelB = TokB{2};
    AperType = strjoin(TokA(3:end), '\_');

    figure('Name', sprintf('Std difference — %s vs %s (%s)', LabelA, LabelB, AperType), ...
           'Position', [50, 50, 600, 500]);

    if ~isempty(AllMedMag)
        plot(AllMedMag, AllDeltaStd, '.', 'MarkerSize', 4);
        hold on;
        plot(xlim, [0 0], 'k--');
        if ~strcmp(Args.OverlayTrend, 'none')
            TrendFun = str2func(['nan' Args.OverlayTrend]);
            R = timeSeries.bin.binningFast([AllMedMag(:), AllDeltaStd(:)], ...
                Args.TrendBinWidth, [9 22], {'MidBin', @numel, TrendFun});
            ValidBins = R(:,2) >= 5;
            plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);

            TrendVals = R(ValidBins, 3);
            TrendMags = R(ValidBins, 1);
            TrendCounts = R(ValidBins, 2);
            if ~isempty(TrendVals)
                [MaxVal, MaxIdx] = max(TrendVals);
                [MinVal, MinIdx] = min(TrendVals);
                text(0.02, 0.97, sprintf('max: %.4f @ mag %.1f (N=%d)\nmin: %.4f @ mag %.1f (N=%d)', ...
                    MaxVal, TrendMags(MaxIdx), TrendCounts(MaxIdx), ...
                    MinVal, TrendMags(MinIdx), TrendCounts(MinIdx)), ...
                    'Units', 'normalized', 'VerticalAlignment', 'top', ...
                    'FontSize', 8, 'BackgroundColor', 'w');
            end
        end
    end
    box on; grid on;
    xlabel('Median Magnitude');
    ylabel(sprintf('Std(%s) - Std(%s) [mag]', LabelA, LabelB));
    xlim([9 22]);
    title(sprintf('%s - %s (%s, %d sources)', LabelA, LabelB, AperType, numel(AllDeltaStd)));
end
