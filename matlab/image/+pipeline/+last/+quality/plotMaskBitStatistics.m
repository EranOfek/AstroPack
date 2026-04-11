function Figs = plotMaskBitStatistics(Report, Args)
    % Plot accumulated 2D mask-bit maps from a maskBitStatistics report
    % Description: For each bit (or a user-selected subset), renders the
    %   per-pixel count of files in which that bit is set as a 2D heatmap
    %   (imagesc + colorbar). One figure per bit per size group.
    %   Requires the report to have been generated with
    %   maskBitStatistics(..., 'AccumulateMaps', true).
    %
    % Input  : - Report struct returned by
    %            pipeline.last.quality.maskBitStatistics with AccumulateMaps=true.
    %          * ...,key,val,...
    %            'BitNames'   - Cell array of bit names to plot. If empty,
    %                           plots every bit found in Report.AccumMaps.
    %                           Default is {}.
    %            'LogScale'   - If true, use log10 of counts (with offset 1).
    %                           Useful when most pixels have 0 counts and a
    %                           few have many. Default is false.
    %            'Colormap'   - Colormap name. Default is 'parula'.
    %            'Title'      - Optional title prefix. Default is ''.
    %            'SaveDir'    - If non-empty, save each figure as PNG to this
    %                           directory using a name derived from the bit
    %                           and size key. Default is ''.
    %            'Visible'    - 'on'|'off'. Useful for batch saving without
    %                           opening windows. Default is 'on'.
    % Output : - Figs: array of figure handles created.
    %
    % Author : Dana Kovaleva (Apr 2026)
    % Example: % Build report with map accumulation enabled:
    %          R = pipeline.last.quality.maskBitStatistics(DataPath, ...
    %                  'BitNames', {'CoaddLessImages'}, 'AccumulateMaps', true);
    %          % Default heatmap (counts of files per pixel):
    %          pipeline.last.quality.plotMaskBitStatistics(R);
    %          % Log scale + save PNGs to disk without opening windows:
    %          pipeline.last.quality.plotMaskBitStatistics(R, ...
    %                  'LogScale', true, 'SaveDir', '/tmp/maps', ...
    %                  'Visible', 'off');

    arguments
        Report                       struct
        Args.BitNames                cell        = {}
        Args.LogScale                (1,1) logical = false
        Args.Colormap                (1,:) char  = 'parula'
        Args.Title                   (1,:) char  = ''
        Args.SaveDir                 (1,:) char  = ''
        Args.Visible                 (1,:) char  = 'on'
    end

    if ~isfield(Report, 'AccumMaps') || isempty(fieldnames(Report.AccumMaps))
        error(['Report has no AccumMaps. Re-run maskBitStatistics with ' ...
               '''AccumulateMaps'', true.']);
    end

    AccumMaps = Report.AccumMaps;
    BitNameMap = Report.AccumMapsBitNames;   % sanitized -> original

    % Resolve which bits to plot (use sanitized keys internally)
    AllSanBits = keys(BitNameMap);
    AllOrigBits = values(BitNameMap);
    if isempty(Args.BitNames)
        ReqSanBits = AllSanBits;
    else
        ReqSanBits = matlab.lang.makeValidName(cellstr(Args.BitNames));
        Missing = setdiff(ReqSanBits, AllSanBits);
        if ~isempty(Missing)
            warning('Bit name(s) not in AccumMaps: %s', strjoin(Missing, ', '));
            ReqSanBits = intersect(ReqSanBits, AllSanBits, 'stable');
        end
    end

    if ~isempty(Args.SaveDir) && ~isfolder(Args.SaveDir)
        mkdir(Args.SaveDir);
    end

    SizeKeys = fieldnames(AccumMaps);
    Figs     = gobjects(0);

    for Isz = 1:numel(SizeKeys)
        SizeKey  = SizeKeys{Isz};
        BitGroup = AccumMaps.(SizeKey);

        for Ib = 1:numel(ReqSanBits)
            SanBit = ReqSanBits{Ib};
            if ~isfield(BitGroup, SanBit)
                continue;
            end

            CountMap = double(BitGroup.(SanBit));
            if Args.LogScale
                DispMap = log10(CountMap + 1);
                CbarLabel = 'log_{10}(NumFiles + 1)';
            else
                DispMap = CountMap;
                CbarLabel = 'NumFiles with bit set';
            end

            % Find original bit name from the sanitized key
            BitName = BitNameMap(SanBit);
            if iscell(BitName); BitName = BitName{1}; end

            Fig = figure('Visible', Args.Visible);
            imagesc(DispMap);
            axis image;
            set(gca, 'YDir', 'normal');           % matches FITS convention
            colormap(Args.Colormap);
            cb = colorbar;
            cb.Label.String = CbarLabel;
            xlabel('X [pix]');
            ylabel('Y [pix]');

            TitleStr = sprintf('Bit "%s" — %s — %d files total', ...
                               BitName, SizeKey, Report.NumFiles);
            if ~isempty(Args.Title)
                TitleStr = sprintf('%s | %s', Args.Title, TitleStr);
            end
            title(TitleStr, 'Interpreter', 'none');

            Figs(end+1) = Fig; %#ok<AGROW>

            if ~isempty(Args.SaveDir)
                FName = sprintf('maskBit_%s_%s.png', SanBit, SizeKey);
                exportgraphics(Fig, fullfile(Args.SaveDir, FName), 'Resolution', 150);
            end
        end
    end
end
