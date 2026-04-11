function Figs = plotCatalogFlagStatistics(Report, Args)
    % Plot spatial distribution of flagged sources from catalogFlagStatistics
    % Description: For each bit (or a user-selected subset), renders the X/Y
    %   positions of all flagged sources accumulated across catalog files.
    %   Three display modes:
    %     'scatter'  - one marker per flagged source.
    %     'density'  - 2D histogram of flagged source counts per bin.
    %     'fraction' - 2D map of flagged-source fraction per bin
    %                  (count(flagged) / count(total)). Bins with too few
    %                  total sources are masked to suppress noise.
    %   'scatter' and 'density' require KeepFlaggedSourcesXY=true in the
    %   report; 'fraction' additionally requires KeepAllSourcesXY=true.
    %
    % Input  : - Report struct returned by
    %            pipeline.last.quality.catalogFlagStatistics with
    %            KeepFlaggedSourcesXY=true.
    %          * ...,key,val,...
    %            'BitNames'   - Cell array of bit names to plot. If empty,
    %                           plots every bit found in Report.FlaggedSources.
    %                           Default is {}.
    %            'Mode'       - 'scatter' | 'density' | 'fraction'.
    %                           Default is 'density'.
    %            'BinSize'    - Bin size in pixels for density/fraction modes.
    %                           Default is 20.
    %            'MarkerSize' - Marker size for scatter mode. Default is 4.
    %            'LogScale'   - Log color scale (density/fraction modes).
    %                           Default false.
    %            'MinSourcesPerBin' - For fraction mode: bins with fewer
    %                           total sources than this are masked (NaN) to
    %                           suppress small-N noise. Default is 5.
    %            'Colormap'   - Colormap name. Default 'parula'.
    %            'Title'      - Optional title prefix. Default ''.
    %            'SaveDir'    - If non-empty, save figures as PNG.
    %                           Default ''.
    %            'Visible'    - 'on'|'off'. Default 'on'.
    % Output : - Figs: array of figure handles created.
    %
    % Author : Dana Kovaleva (Apr 2026)
    % Example: R = pipeline.last.quality.catalogFlagStatistics(DataPath, ...
    %                  'BitNames', {'CoaddLessImages'});
    %          % Density of flagged sources (counts per bin):
    %          pipeline.last.quality.plotCatalogFlagStatistics(R);
    %          % Scatter of flagged sources:
    %          pipeline.last.quality.plotCatalogFlagStatistics(R, 'Mode','scatter');
    %          % Fraction of sources that are flagged, per bin:
    %          pipeline.last.quality.plotCatalogFlagStatistics(R, 'Mode','fraction');
    %          % Larger bins, stricter noise rejection, log-scale color:
    %          pipeline.last.quality.plotCatalogFlagStatistics(R, ...
    %                  'Mode','fraction', 'BinSize', 40, ...
    %                  'MinSourcesPerBin', 10, 'LogScale', true);

    arguments
        Report                       struct
        Args.BitNames                cell        = {}
        Args.Mode                    (1,:) char  {mustBeMember(Args.Mode, {'scatter','density','fraction'})} = 'density'
        Args.BinSize                 (1,1) double = 20
        Args.MarkerSize              (1,1) double = 4
        Args.LogScale                (1,1) logical = false
        Args.MinSourcesPerBin        (1,1) double = 5
        Args.Colormap                (1,:) char  = 'parula'
        Args.Title                   (1,:) char  = ''
        Args.SaveDir                 (1,:) char  = ''
        Args.Visible                 (1,:) char  = 'on'
    end

    if ~isfield(Report, 'FlaggedSources') || height(Report.FlaggedSources) == 0
        error(['Report has no FlaggedSources data. Re-run catalogFlagStatistics ' ...
               'with ''KeepFlaggedSourcesXY'', true on data that contains ' ...
               'flagged sources.']);
    end
    if strcmp(Args.Mode, 'fraction')
        if ~isfield(Report, 'AllSources') || height(Report.AllSources) == 0
            error(['Mode ''fraction'' requires Report.AllSources. Re-run ' ...
                   'catalogFlagStatistics with ''KeepAllSourcesXY'', true.']);
        end
    end

    FS = Report.FlaggedSources;

    % Resolve which bits to plot
    AllBits = unique(FS.BitName, 'stable');
    if isempty(Args.BitNames)
        ReqBits = AllBits;
    else
        ReqBits = string(Args.BitNames(:));
        Missing = setdiff(ReqBits, AllBits);
        if ~isempty(Missing)
            warning('Bit name(s) not in FlaggedSources: %s', ...
                    strjoin(cellstr(Missing), ', '));
            ReqBits = intersect(ReqBits, AllBits, 'stable');
        end
    end

    if ~isempty(Args.SaveDir) && ~isfolder(Args.SaveDir)
        mkdir(Args.SaveDir);
    end

    % Determine plotting bounds from PerFile size info if available, else from data
    if isfield(Report, 'PerFile') && all(ismember({'SizeX','SizeY'}, ...
                                                 Report.PerFile.Properties.VariableNames))
        XMax = max(Report.PerFile.SizeX);
        YMax = max(Report.PerFile.SizeY);
    else
        XMax = max(FS.X);
        YMax = max(FS.Y);
    end

    % Pre-compute total-source 2D histogram once (used by 'fraction' mode)
    XEdgesGlobal = 0:Args.BinSize:XMax;
    YEdgesGlobal = 0:Args.BinSize:YMax;
    if strcmp(Args.Mode, 'fraction')
        TotalCounts = histcounts2(Report.AllSources.X, Report.AllSources.Y, ...
                                  XEdgesGlobal, YEdgesGlobal);
    end

    Figs = gobjects(0);

    for Ib = 1:numel(ReqBits)
        BitName = char(ReqBits(Ib));
        Sel = FS.BitName == ReqBits(Ib);
        Xs  = FS.X(Sel);
        Ys  = FS.Y(Sel);
        Nsrc = numel(Xs);

        if Nsrc == 0
            continue;
        end

        Fig = figure('Visible', Args.Visible);

        switch Args.Mode
            case 'scatter'
                scatter(Xs, Ys, Args.MarkerSize, 'filled');
                axis equal;
                xlim([0 XMax]);
                ylim([0 YMax]);
                xlabel('X [pix]');
                ylabel('Y [pix]');

            case 'density'
                Counts = histcounts2(Xs, Ys, XEdgesGlobal, YEdgesGlobal);
                if Args.LogScale
                    DispMap = log10(Counts' + 1);
                    CbarLabel = 'log_{10}(NumSources + 1)';
                else
                    DispMap = Counts';
                    CbarLabel = 'NumSources';
                end
                imagesc(XEdgesGlobal, YEdgesGlobal, DispMap);
                axis image;
                set(gca, 'YDir', 'normal');
                colormap(Args.Colormap);
                cb = colorbar;
                cb.Label.String = CbarLabel;
                xlabel('X [pix]');
                ylabel('Y [pix]');

            case 'fraction'
                FlaggedCounts = histcounts2(Xs, Ys, XEdgesGlobal, YEdgesGlobal);
                FracMap = FlaggedCounts ./ TotalCounts;
                % Mask noisy bins (too few total sources) and empty bins
                BadBins = (TotalCounts < Args.MinSourcesPerBin);
                FracMap(BadBins) = NaN;

                if Args.LogScale
                    DispMap = log10(FracMap');
                    CbarLabel = 'log_{10}(NumFlagged / NumTotal)';
                else
                    DispMap = FracMap';
                    CbarLabel = 'NumFlagged / NumTotal';
                end
                h = imagesc(XEdgesGlobal, YEdgesGlobal, DispMap);
                set(h, 'AlphaData', ~isnan(DispMap));    % NaN bins -> transparent
                axis image;
                set(gca, 'YDir', 'normal', 'Color', [0.85 0.85 0.85]);
                colormap(Args.Colormap);
                cb = colorbar;
                cb.Label.String = CbarLabel;
                xlabel('X [pix]');
                ylabel('Y [pix]');
        end

        % Look up the per-bit fraction from the report's Aggregate table.
        % Falls back to "n/a" if the Aggregate row for this bit is missing.
        OverallFrac  = NaN;
        TotalSources = NaN;
        if isfield(Report, 'Aggregate') && ~isempty(Report.Aggregate)
            AggSel = Report.Aggregate.BitName == ReqBits(Ib);
            if any(AggSel)
                OverallFrac  = Report.Aggregate.OverallFraction(find(AggSel, 1));
                TotalSources = Report.Aggregate.TotalSources(find(AggSel, 1));
            end
        end

        if ~isnan(OverallFrac) && ~isnan(TotalSources)
            TitleStr = sprintf(['Bit "%s" — %d / %d flagged sources ' ...
                                '(%.3f%%) across %d files'], ...
                               BitName, Nsrc, TotalSources, ...
                               100*OverallFrac, Report.NumFiles);
        else
            TitleStr = sprintf('Bit "%s" — %d flagged sources across %d files', ...
                               BitName, Nsrc, Report.NumFiles);
        end
        if ~isempty(Args.Title)
            TitleStr = sprintf('%s | %s', Args.Title, TitleStr);
        end
        title(TitleStr, 'Interpreter', 'none');

        Figs(end+1) = Fig; %#ok<AGROW>

        if ~isempty(Args.SaveDir)
            SanBit = matlab.lang.makeValidName(BitName);
            FName = sprintf('catFlag_%s_%s.png', SanBit, Args.Mode);
            exportgraphics(Fig, fullfile(Args.SaveDir, FName), 'Resolution', 150);
        end
    end
end
