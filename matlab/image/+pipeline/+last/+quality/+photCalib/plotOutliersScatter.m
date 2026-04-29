function Cat = plotOutliersScatter(AI, MS, Args)
    % Find scatter outliers (bright + high-std sources) for one crop and
    %   either plot their lightcurves or overlay them on the image in DS9.
    %
    % Input  : - AI: AstroImage (scalar coadd or per-epoch vector). Used
    %            only when PlotMode='ds9'; AI(1).Image is displayed.
    %          - MS: a single MatchedSources object for the same crop.
    %          * ...,key,val,...
    %            'MagField' - Magnitude field in MS.Data. Default 'MAG_APER_3'.
    %            'MaxMag'   - Maximum median magnitude. Default 15.
    %            'MinStd'   - Minimum std [mag]. Default 0.05.
    %            'FilterFlags' - Cell of FLAGS names to NaN out per epoch
    %                        before computing median/std. Default
    %                        {'Saturated','NearEdge','NaN'}. {} disables.
    %            'PlotMode' - 'ds9' (default) | 'lc' | 'none'.
    %            'MarkerColor' - DS9 marker color. Default 'red'.
    %            'MarkerSize'  - DS9 marker size.  Default 20.
    % Output : - AstroCatalog of outliers with columns
    %            {SrcIdx, X, Y, RA, Dec, MedMag, StdMag}.
    % Author : D. Kovaleva (Apr 2026)
    % Example: [AllSI, Coadd, MS] = pipeline.last.load.loadVisit(VisitDir);
    %          Ic = 8;
    %          Cat = pipeline.last.quality.photCalib.plotOutliersScatter( ...
    %              Coadd(Ic), MS(Ic));

    arguments
        AI
        MS  MatchedSources
        Args.MagField     = 'MAG_APER_3'
        Args.MaxMag       = 15
        Args.MinStd       = 0.05
        Args.FilterFlags cell = {'Saturated', 'NearEdge', 'NaN'}
        Args.PlotMode     = 'ds9'
        Args.MarkerColor  = 'green'
        Args.MarkerSize   = 20
    end

    Mag = MS.Data.(Args.MagField);

    % NaN out epochs whose FLAGS match any FilterFlags bit
    if ~isempty(Args.FilterFlags) && isfield(MS.Data, 'FLAGS')
        FM = MS.Data.FLAGS;
        FM(isnan(FM)) = 0;
        try
            BD = BitDictionary;
            Bad = false(size(FM));
            for Ifl = 1:numel(Args.FilterFlags)
                [~, ~, BitDec] = BD.name2bit(Args.FilterFlags{Ifl});
                Bad = Bad | (bitand(uint32(FM), uint32(BitDec)) > 0);
            end
            Mag(Bad) = NaN;
        catch
        end
    end

    MedMag = median(Mag, 1, 'omitnan');
    StdMag = std(Mag, 0, 1, 'omitnan');
    Sel    = MedMag < Args.MaxMag & StdMag > Args.MinStd & isfinite(MedMag);
    Idx    = find(Sel);

    fprintf('%d outliers (MedMag<%.2f, Std>%.3f)\n', numel(Idx), Args.MaxMag, Args.MinStd);

    if isfield(MS.Data, 'X1'); XF = 'X1'; YF = 'Y1';
    else;                       XF = 'X';  YF = 'Y';
    end
    X  = median(MS.Data.(XF), 1, 'omitnan');
    Y  = median(MS.Data.(YF), 1, 'omitnan');
    if isfield(MS.Data, 'RA');  RA  = median(MS.Data.RA,  1, 'omitnan');
    else;                        RA  = nan(size(X));
    end
    if isfield(MS.Data, 'Dec'); Dec = median(MS.Data.Dec, 1, 'omitnan');
    else;                        Dec = nan(size(X));
    end

    Cat = AstroCatalog;
    if ~isempty(Idx)
        Cat.Catalog = [Idx(:), X(Idx).', Y(Idx).', RA(Idx).', Dec(Idx).', ...
                       MedMag(Idx).', StdMag(Idx).'];
    else
        Cat.Catalog = zeros(0, 7);
    end
    Cat.ColNames = {'SrcIdx', 'X', 'Y', 'RA', 'Dec', 'MedMag', 'StdMag'};
    Cat.ColUnits = {'',       'pix','pix','deg','deg','mag',    'mag'};

    switch lower(Args.PlotMode)
        case 'ds9'
            ensureDS9Ready();
            ds9.disp(AI(1).Image, 1);
            if ~isempty(Idx)
                ds9.plotXY([X(Idx).', Y(Idx).'], [], ...
                    'Marker', 'o', 'Color', Args.MarkerColor, ...
                    'MarkerSize', Args.MarkerSize, 'CooType', 'image');
            end

        case 'lc'
            if isempty(Idx); return; end
            [~, Ord] = sort(StdMag(Idx), 'descend');
            Idx = Idx(Ord);
            N = numel(Idx);
            Nc = ceil(sqrt(N));
            Nr = ceil(N / Nc);
            figure('Name', 'Outlier lightcurves', ...
                'Position', [50, 50, 250*Nc, 200*Nr]);
            for Is = 1:N
                subplot(Nr, Nc, Is);
                LC = Mag(:, Idx(Is));
                Ep = (1:numel(LC))';
                V  = isfinite(LC);
                hold on;
                if sum(V) >= 2
                    plot(Ep(V), LC(V), '-', 'Color', [0.4 0.6 1]);
                end
                plot(Ep(V), LC(V), '.', 'MarkerSize', 12, 'Color', [0 0.3 0.8]);
                box on; grid on;
                title(sprintf('src%d M=%.2f S=%.3f', ...
                    Idx(Is), MedMag(Idx(Is)), StdMag(Idx(Is))), 'FontSize', 8);
            end
            sgtitle(sprintf('Outliers: MedMag<%.2f, Std>%.3f  (%s)', ...
                Args.MaxMag, Args.MinStd, strrep(Args.MagField, '_', '\_')));

        case 'none'
            % no plotting

        otherwise
            warning('plotOutliersScatter:BadPlotMode', ...
                'Unknown PlotMode ''%s''.', Args.PlotMode);
    end
end


function ensureDS9Ready()
    % This workstation's xpa install does not auto-spawn xpans, so XPA
    % calls fail until we launch it explicitly. Spawn xpans, ds9, and
    % poll until ds9 answers XPA commands.
    [~, A] = system('xpaaccess -n xpans 2>/dev/null');
    if str2double(strtrim(A)) < 1
        system('xpans & disown');
        for K = 1:8
            pause(0.25);
            [~, A] = system('xpaaccess -n xpans 2>/dev/null');
            if str2double(strtrim(A)) >= 1; break; end
        end
    end
    if ~ds9.isopen; ds9.open; end
    for K = 1:20
        try
            ds9.system('xpaget ds9 frame frameno');
            return;
        catch
            pause(0.5);
        end
    end
    warning('plotOutliersScatter:DS9Unreachable', 'ds9 not responding to XPA.');
end
