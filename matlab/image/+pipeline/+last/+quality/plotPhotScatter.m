function plotPhotScatter(MS, Args)
    % Plot epoch-to-epoch magnitude scatter (Mag vs Std)
    % Description: For each magnitude field, plots median magnitude vs
    %              epoch-to-epoch std. Shows both relative and calibrated
    %              photometry scatter with binned trend lines.
    %              Two layout modes:
    %                TwoPanels=true:  separate panels for relative (left) and
    %                                 calibrated (right) photometry.
    %                TwoPanels=false: single panel with relative (grey, behind)
    %                                 and calibrated (color, on top) overlaid.
    %              Optionally color-codes sources by crop ID.
    %
    % Input  : - MS struct with MS.(mode){crop} = MatchedSources
    %            (from matchPhotEpochs or loadMergedMat).
    %          * ...,key,val,...
    %            'Modes'       - Cell array of modes. Required.
    %            'MagFields'   - Magnitude columns. Default is {'MAG_AB_PSF','MAG_AB_APER_3'}.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'TwoPanels'   - true: two panels (relative + calibrated).
    %                            false: one panel, both overlaid. Default is true.
    %            'OverlayTrend'- 'median'|'mean'|'none'. Default is 'median'.
    %            'TrendBinWidth'- Bin width [mag]. Default is 0.75.
    %            'MinEpochs'   - Min non-NaN epochs per source. Default is 0.
    %            'ColorByCrop' - Color sources by crop ID. Default is false.
    % Author : D. Kovaleva (Mar 2026)
    % Example: % Two panels (default):
    %          pipeline.last.quality.plotPhotScatter(R.MS, 'Modes', {'percrop'});
    %
    %          % Overlaid on one panel:
    %          pipeline.last.quality.plotPhotScatter(R.MS, 'Modes', {'percrop'}, ...
    %              'TwoPanels', false);
    %
    %          % Color by crop:
    %          pipeline.last.quality.plotPhotScatter(R.MS, 'Modes', {'percrop'}, ...
    %              'ColorByCrop', true);
    %
    %          % From MergedMat (relative photometry only, no AB):
    %          pipeline.last.quality.plotPhotScatter(MS, 'Modes', {'percrop'}, ...
    %              'MagFields', {'MAG_APER_3'}, 'TwoPanels', false);

    arguments
        MS struct
        Args.Modes cell
        Args.MagFields      = {'MAG_AB_PSF', 'MAG_AB_APER_3'}
        Args.CropsToAnalyze = []
        Args.TwoPanels logical = true
        Args.OverlayTrend   = 'median'
        Args.TrendBinWidth  = 0.75
        Args.MinEpochs      = 0   % Min non-NaN epochs per source; 0 = no filter
        Args.ColorByCrop logical = false  % Color sources by crop ID (multicolor)
        Args.CentralEdge logical = false  % Distinguish central vs edge crops by shade
        Args.TileOrder      = 'rowmajor'  % For CentralEdge: 'colmajor'|'rowmajor'
    end

    Nmodes = numel(Args.Modes);
    Colors = lines(Nmodes);
    TrendBinEdges = 9:Args.TrendBinWidth:22;

    % Central crop IDs for CentralEdge mode
    switch lower(Args.TileOrder)
        case 'colmajor'
            CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor'
            CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise
            CentralCrops = [];
    end

    for Imf = 1:numel(Args.MagFields)
        MagField = Args.MagFields{Imf};

        % Derive original (instrumental) mag field name
        OrigMagField = '';
        if Args.TwoPanels
            if contains(MagField, '_AB_')
                OrigMagField = strrep(MagField, '_AB_', '_');
            elseif contains(MagField, '_CB_')
                OrigMagField = strrep(MagField, '_CB_', '_');
            end
        end

        % Number of panels: 2 per mode if TwoPanels, else 1 per mode (overlaid)
        if ~isempty(OrigMagField) && Args.TwoPanels
            Npanels = 2 * Nmodes;
        else
            Npanels = Nmodes;
        end

        figure('Name', sprintf('Mag vs Std — %s', MagField), ...
               'Position', [50, 50, 400*Npanels, 500]);

        for Im = 1:Nmodes
            Mode = Args.Modes{Im};
            if ~isfield(MS, Mode); continue; end

            CropsToUse = Args.CropsToAnalyze;
            if isempty(CropsToUse)
                CropsToUse = 1:numel(MS.(Mode));
            end

            % Collect data per crop (for ColorByCrop) and aggregated (for trend)
            CropCmap = lines(numel(CropsToUse));
            AllMedOrig = [];  AllStdOrig = [];
            AllMedMag  = [];  AllStdMag  = [];
            PerCropOrig = cell(numel(CropsToUse), 1);  % {Iic}.Med, .Std
            PerCropMag  = cell(numel(CropsToUse), 1);

            for Iic = 1:numel(CropsToUse)
                Ic = CropsToUse(Iic);
                if Ic > numel(MS.(Mode)) || isempty(MS.(Mode){Ic}); continue; end
                MSobj = MS.(Mode){Ic};

                % Relative photometry
                if ~isempty(OrigMagField) && isfield(MSobj.Data, OrigMagField)
                    OrigData = MSobj.Data.(OrigMagField);
                    if Args.MinEpochs > 0
                        Good = sum(~isnan(OrigData), 1) >= Args.MinEpochs;
                        OrigData = OrigData(:, Good);
                    end
                    Med = nanmedian(OrigData, 1);
                    Std = nanstd(OrigData, 0, 1);
                    PerCropOrig{Iic} = struct('Med', Med, 'Std', Std);
                    AllMedOrig = [AllMedOrig, Med];
                    AllStdOrig = [AllStdOrig, Std];
                end

                % Calibrated
                if isfield(MSobj.Data, MagField)
                    MagData = MSobj.Data.(MagField);
                    if Args.MinEpochs > 0
                        Good = sum(~isnan(MagData), 1) >= Args.MinEpochs;
                        MagData = MagData(:, Good);
                    end
                    Med = nanmedian(MagData, 1);
                    Std = nanstd(MagData, 0, 1);
                    PerCropMag{Iic} = struct('Med', Med, 'Std', Std);
                    AllMedMag = [AllMedMag, Med];
                    AllStdMag = [AllStdMag, Std];
                end
            end

            % --- Panel 1: Relative photometry ---
            if ~isempty(OrigMagField) && Args.TwoPanels
                % Separate panel for relative photometry
                PanelIdx = (Im - 1) * 2 + 1;
                subplot(1, Npanels, PanelIdx);
                hold on;
                plotCropDots(CropsToUse, PerCropOrig, Args, CropCmap, ...
                    CentralCrops, [0.6 0.6 0.6], [0.85 0.85 0.85]);
                if Args.ColorByCrop
                    addCropLegend(CropsToUse, PerCropOrig, CropCmap);
                end
                if ~isempty(AllMedOrig) && ~strcmp(Args.OverlayTrend, 'none')
                    TrendFun = str2func(['nan' Args.OverlayTrend]);
                    Rorig = timeSeries.bin.binningFast([AllMedOrig(:), AllStdOrig(:)], ...
                        Args.TrendBinWidth, [9 22], {'MidBin', @numel, TrendFun});
                    ValidBinsOrig = Rorig(:,2) >= 5;
                    plot(Rorig(ValidBinsOrig,1), Rorig(ValidBinsOrig,3), '-', ...
                        'Color', [0.3 0.3 0.3], 'LineWidth', 2.5);
                end
                set(gca, 'YScale', 'log');
                box on; grid on;
                xlabel('Median Magnitude');
                ylabel('Std [mag]');
                xlim([9 22]); ylim([1e-3 10]);
                title(strrep(OrigMagField, '_', '\_'));

                PanelIdx = (Im - 1) * 2 + 2;
            else
                PanelIdx = Im;
            end

            % --- Panel 2 (or single panel): Calibrated ---
            subplot(1, Npanels, PanelIdx);
            hold on;

            % Overlay relative photometry on same panel when TwoPanels=false
            if ~isempty(OrigMagField) && ~Args.TwoPanels
                plotCropDots(CropsToUse, PerCropOrig, Args, CropCmap, ...
                    CentralCrops, [0.75 0.75 0.75], [0.88 0.88 0.88]);
            end
            % Calibrated dots
            plotCropDots(CropsToUse, PerCropMag, Args, CropCmap, ...
                CentralCrops, [0.2 0.4 0.8], [0.6 0.75 0.95]);
            if Args.ColorByCrop
                addCropLegend(CropsToUse, PerCropMag, CropCmap);
            end
            % Trend lines
            if ~strcmp(Args.OverlayTrend, 'none')
                TrendFun = str2func(['nan' Args.OverlayTrend]);
                % Relative trend (grey, dashed) for single-panel overlay
                if ~isempty(OrigMagField) && ~Args.TwoPanels && ~isempty(AllMedOrig)
                    Rorig = timeSeries.bin.binningFast([AllMedOrig(:), AllStdOrig(:)], ...
                        Args.TrendBinWidth, [9 22], {'MidBin', @numel, TrendFun});
                    ValidBinsOrig = Rorig(:,2) >= 5;
                    plot(Rorig(ValidBinsOrig,1), Rorig(ValidBinsOrig,3), '--', ...
                        'Color', [0.5 0.5 0.5], 'LineWidth', 2.5);
                end
                % Calibrated trend (black, solid)
                if ~isempty(AllMedMag)
                    R = timeSeries.bin.binningFast([AllMedMag(:), AllStdMag(:)], ...
                        Args.TrendBinWidth, [9 22], {'MidBin', @numel, TrendFun});
                    ValidBins = R(:,2) >= 5;
                    plot(R(ValidBins,1), R(ValidBins,3), '-k', 'LineWidth', 2);
                end
            end
            set(gca, 'YScale', 'log');
            box on; grid on;
            xlabel('Median Magnitude');
            ylabel('Std [mag]');
            xlim([9 22]); ylim([1e-3 10]);
            title(strrep(MagField, '_', '\_'));
        end
        sgtitle(sprintf('Epoch-to-epoch scatter: %s', strrep(MagField, '_', '\_')), ...
            'FontSize', 11);
        for Isub = 1:Npanels
            Ax = subplot(1, Npanels, Isub);
            Ax.Position(4) = Ax.Position(4) * 0.88;
        end
    end
end

% =========================================================================
function plotCropDots(CropsToUse, PerCropData, Args, CropCmap, CentralCrops, CentralColor, EdgeColor)
    % Plot dots for each crop with appropriate coloring mode
    for Iic = 1:numel(CropsToUse)
        if isempty(PerCropData{Iic}); continue; end
        Ic = CropsToUse(Iic);
        Med = PerCropData{Iic}.Med;
        Std = PerCropData{Iic}.Std;

        if Args.ColorByCrop
            % Each crop gets its own color from colormap
            plot(Med, Std, '.', 'Color', CropCmap(Iic,:), 'MarkerSize', 3);
        elseif Args.CentralEdge
            % Central crops: darker, Edge crops: lighter
            if ismember(Ic, CentralCrops)
                plot(Med, Std, '.', 'Color', CentralColor, 'MarkerSize', 4);
            else
                plot(Med, Std, '.', 'Color', EdgeColor, 'MarkerSize', 2);
            end
        else
            % Default: single color
            plot(Med, Std, '.', 'Color', CentralColor, 'MarkerSize', 3);
        end
    end
end

% =========================================================================
function addCropLegend(CropsToUse, PerCropData, CropCmap)
    % Add legend with crop numbers for ColorByCrop mode
    LegHandles = [];
    LegLabels  = {};
    for Iic = 1:numel(CropsToUse)
        if isempty(PerCropData{Iic}); continue; end
        Ic = CropsToUse(Iic);
        H = plot(NaN, NaN, '.', 'Color', CropCmap(Iic,:), 'MarkerSize', 10);
        LegHandles = [LegHandles, H];
        LegLabels{end+1} = sprintf('%d', Ic);
    end
    if ~isempty(LegHandles)
        legend(LegHandles, LegLabels, 'Location', 'southwest', 'FontSize', 6, 'NumColumns', 4);
    end
end
