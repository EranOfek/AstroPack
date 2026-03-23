function plotPhotScatter(MS, Args)
    % Plot epoch-to-epoch magnitude scatter (Mag vs Std) per mode
    % Description: For each magnitude field, creates a figure with one panel
    %              per mode showing median magnitude vs epoch-to-epoch std.
    %              Optionally overlays original (instrumental) mag scatter in
    %              gray and binned trend lines via timeSeries.bin.binningFast.
    %
    % Input  : - MS struct with MS.(mode){crop} = MatchedSources
    %            (from matchPhotEpochs).
    %          * ...,key,val,...
    %            'Modes'       - Cell array of modes. Required.
    %            'MagFields'   - AB magnitude columns. Default is {'MAG_AB_PSF','MAG_AB_APER_3'}.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all available).
    %            'ShowOrigMag' - Overlay instrumental mag scatter. Default is true.
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'TrendBinWidth'- Bin width [mag]. Default is 0.5.
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotScatter(MS, 'Modes', {'percrop','refzp'});

    arguments
        MS struct
        Args.Modes cell
        Args.MagFields      = {'MAG_AB_PSF', 'MAG_AB_APER_3'}
        Args.CropsToAnalyze = []
        Args.ShowOrigMag logical = true
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.5
    end

    Nmodes = numel(Args.Modes);
    Colors = lines(Nmodes);
    TrendBinEdges = 9:Args.TrendBinWidth:22;

    for Imf = 1:numel(Args.MagFields)
        MagField = Args.MagFields{Imf};

        figure('Name', sprintf('Mag vs Std — %s', MagField), ...
               'Position', [50, 50, 400*Nmodes, 500]);

        % Derive original (non-AB) mag field name
        OrigMagField = '';
        if Args.ShowOrigMag && contains(MagField, '_AB_')
            OrigMagField = strrep(MagField, '_AB_', '_');
        end

        for Im = 1:Nmodes
            Mode = Args.Modes{Im};
            if ~isfield(MS, Mode); continue; end
            subplot(1, Nmodes, Im);
            hold on;

            CropsToUse = Args.CropsToAnalyze;
            if isempty(CropsToUse)
                CropsToUse = 1:numel(MS.(Mode));
            end

            % --- Original mag scatter (background, gray) ---
            if ~isempty(OrigMagField)
                allMedOrig = [];
                allStdOrig = [];
                for Ic = CropsToUse
                    if Ic > numel(MS.(Mode)) || isempty(MS.(Mode){Ic})
                        continue;
                    end
                    MSobj = MS.(Mode){Ic};
                    if ~isfield(MSobj.Data, OrigMagField); continue; end
                    OrigData = MSobj.Data.(OrigMagField);
                    allMedOrig = [allMedOrig, nanmedian(OrigData, 1)];
                    allStdOrig = [allStdOrig, nanstd(OrigData, 0, 1)];
                end
                if ~isempty(allMedOrig)
                    plot(allMedOrig, allStdOrig, '.', 'Color', [0.75 0.75 0.75], 'MarkerSize', 3);
                    if ~strcmp(Args.OverlayTrend, 'none')
                        TrendFun = str2func(['nan' Args.OverlayTrend]);
                        R = timeSeries.bin.binningFast([allMedOrig(:), allStdOrig(:)], ...
                            Args.TrendBinWidth, [9 22], {'MidBin', @numel, TrendFun});
                        ValidBins = R(:,2) >= 5;
                        plot(R(ValidBins,1), R(ValidBins,3), '--', ...
                            'Color', [0.5 0.5 0.5], 'LineWidth', 2.5);
                    end
                end
            end

            % --- AB mag scatter (foreground, color) ---
            allMedMag = [];
            allStdMag = [];

            for Ic = CropsToUse
                if Ic > numel(MS.(Mode)) || isempty(MS.(Mode){Ic})
                    continue;
                end
                MSobj = MS.(Mode){Ic};
                if ~isfield(MSobj.Data, MagField); continue; end

                MagData = MSobj.Data.(MagField);
                allMedMag = [allMedMag, nanmedian(MagData, 1)];
                allStdMag = [allStdMag, nanstd(MagData, 0, 1)];
            end

            if ~isempty(allMedMag)
                plot(allMedMag, allStdMag, '.', 'Color', Colors(Im,:), 'MarkerSize', 4);
                if ~strcmp(Args.OverlayTrend, 'none')
                    TrendFun = str2func(['nan' Args.OverlayTrend]);
                    R = timeSeries.bin.binningFast([allMedMag(:), allStdMag(:)], ...
                        Args.TrendBinWidth, [9 22], {'MidBin', @numel, TrendFun});
                    ValidBins = R(:,2) >= 5;
                    plot(R(ValidBins,1), R(ValidBins,3), '-k', 'LineWidth', 2);
                end
            end
            set(gca, 'YScale', 'log');
            box on; grid on;
            xlabel('Median Magnitude');
            ylabel('Std [mag]');
            xlim([9 22]);
            ylim([1e-3 10]);
            title(sprintf('%s', Mode));
        end
        sgtitle(sprintf('Epoch-to-epoch scatter: %s', strrep(MagField, '_', '\_')));
    end
end
