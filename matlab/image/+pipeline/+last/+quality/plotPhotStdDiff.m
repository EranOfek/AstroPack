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
    % Example: pipeline.last.quality.plotPhotStdDiff(MS, 'Modes', {'percrop','refzp'});

    arguments
        MS struct
        Args.Modes cell
        Args.MagFields      = {'MAG_AB_PSF', 'MAG_AB_APER_3'}
        Args.CropsToAnalyze = []
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.5
        Args.MinEpochs      = 0    % Min non-NaN epochs per source; 0 = no filter
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
            allMedMag = [];
            allDeltaStd = [];

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

                allMedMag = [allMedMag, MedMag];
                allDeltaStd = [allDeltaStd, Std_pc - Std_other];
            end

            subplot(1, Nother, Io);
            if ~isempty(allMedMag)
                plot(allMedMag, allDeltaStd, '.', 'MarkerSize', 4);
                hold on;
                plot(xlim, [0 0], 'k--');
                if ~strcmp(Args.OverlayTrend, 'none')
                    TrendFun = str2func(['nan' Args.OverlayTrend]);
                    R = timeSeries.bin.binningFast([allMedMag(:), allDeltaStd(:)], ...
                        Args.TrendBinWidth, [9 22], {'MidBin', @numel, TrendFun});
                    ValidBins = R(:,2) >= 5;
                    plot(R(ValidBins,1), R(ValidBins,3), '-r', 'LineWidth', 2);
                end
            end
            box on; grid on;
            xlabel('Median Magnitude');
            ylabel(sprintf('Std(percrop) - Std(%s) [mag]', Mode));
            xlim([9 22]);
            title(sprintf('percrop - %s', Mode));
        end
        sgtitle(sprintf('Std difference (>0 = non-percrop better): %s', ...
            strrep(MagField, '_', '\_')));
    end
end
