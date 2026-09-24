function H = plotOverlapFlagHist(Src, Args)
    % Histograms of a coordinate/quantity for one- vs two-Overlap-flag sources.
    % Description: Takes the per-pair Sources produced by
    %   pipeline.last.quality.masks.overlapFlagTally (KeepSources / AllColumns)
    %   and, for each requested Field (default XFULL and YFULL), overlays 1D
    %   histograms of that field split by Overlap-flag category. By default
    %   the split is ONE (flagged in exactly one crop, C1 or C2) vs BOTH
    %   (flagged in both) - i.e. "one flag" vs "two flags". One subplot per
    %   Field, on shared bin edges so the groups are directly comparable.
    %
    %   The field value is taken from member 1 (Args.Member); for full-frame
    %   / sky coordinates the two members coincide, so member 1 is the natural
    %   choice. Field is a base name resolved to the curated column ('XFULL1')
    %   or the AllColumns column ('XFULL_1') automatically, so any per-source
    %   quantity works (e.g. 'MAG_PSF', 'SN', 'FWHM').
    %
    % Input  : - Src: overlapFlagTally Report struct (its .Sources is used) or
    %            the Sources table directly.
    %          * ...,key,val,...
    %            'Fields'      - Cellstr of columns to histogram, one subplot
    %                            each. Default {'XFULL','YFULL'}. Each entry
    %                            may be EITHER an exact column of R.Sources
    %                            (e.g. 'MAG_PSF_1', 'SN_2', 'Interface') -
    %                            used as-is - OR a base name (e.g. 'XFULL',
    %                            'MAG_PSF') that is resolved to that member's
    %                            column via the Member suffix ('XFULL1' /
    %                            'XFULL_1'). Exact match wins.
    %            'Group'       - 'oneVsBoth' (default) overlays ONE vs BOTH;
    %                            'category' overlays C1 / C2 / BOTH / NONE
    %                            separately.
    %            'Show'        - For Group='category', subset of
    %                            {'BOTH','C1','C2','NONE'}. Default
    %                            {'BOTH','C1','C2'}. Ignored for oneVsBoth.
    %            'Member'      - Which member's value to histogram (1 or 2).
    %                            Default 1.
    %            'NBins'       - Number of bins (shared per subplot). Default 50.
    %            'Normalization' - histogram Normalization: 'count'
    %                            (default) | 'probability' | 'pdf' | 'cdf' |
    %                            'countdensity'.
    %            'Style'       - 'stairs' (default, clean overlay) | 'bar'.
    %            'ColorBoth'   - RGB. Default [0.85 0.10 0.10] (red).
    %            'ColorOne'    - RGB for the ONE group. Default [0.10 0.35 0.85].
    %            'ColorC1'     - RGB. Default [0.10 0.35 0.85] (blue).
    %            'ColorC2'     - RGB. Default [0.15 0.60 0.20] (green).
    %            'ColorNone'   - RGB. Default [0.70 0.70 0.70] (gray).
    %            'LineWidth'   - Stairs line width. Default 1.5.
    %            'Visible'     - 'on' | 'off'. Default 'on'.
    %            'Title'       - Overall title. Default '' (auto).
    % Output : - H struct: .Fig, .Axes (1xNField), .Hist (struct per field of
    %            histogram handles), .Counts (.BOTH/.C1/.C2/.NONE/.ONE).
    % Author : D. Kovaleva (Jul 2026)
    % See also: pipeline.last.quality.masks.overlapFlagTally,
    %           pipeline.last.quality.masks.plotOverlapFlagSources.
    % Example:
    %   R = pipeline.last.quality.masks.overlapFlagTally(AI, 'KeepSources',true);
    %   pipeline.last.quality.masks.plotOverlapFlagHist(R);                    % XFULL & YFULL, ONE vs BOTH
    %   pipeline.last.quality.masks.plotOverlapFlagHist(R, 'Group','category');
    %   pipeline.last.quality.masks.plotOverlapFlagHist(R, 'Fields',{'MAG_PSF'});
    %   % Any exact column of R.Sources (e.g. from AllColumns) works too:
    %   pipeline.last.quality.masks.plotOverlapFlagHist(R, ...
    %           'Fields',{'MAG_PSF_1','SN_1','FLUX_APER_3_2'});
    arguments
        Src
        Args.Fields           cell = {'XFULL','YFULL'}
        Args.Group      (1,:) char {mustBeMember(Args.Group,{'oneVsBoth','category'})} = 'oneVsBoth'
        Args.Show             cell = {'BOTH','C1','C2'}
        Args.Member     (1,1) double {mustBeMember(Args.Member,[1 2])} = 1
        Args.NBins      (1,1) double = 50
        Args.Normalization (1,:) char = 'count'
        Args.Style      (1,:) char {mustBeMember(Args.Style,{'stairs','bar'})} = 'stairs'
        Args.ColorBoth  (1,3) double = [0.85 0.10 0.10]
        Args.ColorOne   (1,3) double = [0.10 0.35 0.85]
        Args.ColorC1    (1,3) double = [0.10 0.35 0.85]
        Args.ColorC2    (1,3) double = [0.15 0.60 0.20]
        Args.ColorNone  (1,3) double = [0.70 0.70 0.70]
        Args.LineWidth  (1,1) double = 1.5
        Args.Visible    (1,:) char {mustBeMember(Args.Visible,{'on','off'})} = 'on'
        Args.Title      (1,:) char = ''
    end

    % --- Resolve the Sources table ---------------------------------------
    if istable(Src)
        T = Src;
    elseif isstruct(Src) && isfield(Src, 'Sources') && istable(Src.Sources)
        T = Src.Sources;
    else
        error('pipeline:last:quality:masks:plotOverlapFlagHist:BadInput', ...
              'Src must be a Sources table or an overlapFlagTally Report struct.');
    end
    if height(T) == 0
        error('pipeline:last:quality:masks:plotOverlapFlagHist:Empty', ...
              'Sources table is empty - run overlapFlagTally with ''KeepSources'',true.');
    end

    % --- Categories -------------------------------------------------------
    O1 = T.Ovlp1 == 1;
    O2 = T.Ovlp2 == 1;
    Cat.BOTH = O1 & O2;
    Cat.C1   = O1 & ~O2;
    Cat.C2   = ~O1 & O2;
    Cat.NONE = ~O1 & ~O2;
    Cat.ONE  = Cat.C1 | Cat.C2;
    H.Counts = struct('BOTH',nnz(Cat.BOTH),'C1',nnz(Cat.C1),'C2',nnz(Cat.C2), ...
                      'NONE',nnz(Cat.NONE),'ONE',nnz(Cat.ONE));

    % --- Groups to overlay ------------------------------------------------
    if strcmp(Args.Group, 'oneVsBoth')
        Keys   = {'ONE','BOTH'};
        Colors = {Args.ColorOne, Args.ColorBoth};
        Labels = {'one flag', 'two flags'};
    else
        AllK   = {'BOTH','C1','C2','NONE'};
        AllCol = {Args.ColorBoth, Args.ColorC1, Args.ColorC2, Args.ColorNone};
        AllLab = {'both crops','only crop 1','only crop 2','neither'};
        sel    = ismember(AllK, Args.Show);
        Keys   = AllK(sel);  Colors = AllCol(sel);  Labels = AllLab(sel);
    end

    % --- Figure / subplots ------------------------------------------------
    NF = numel(Args.Fields);
    H.Fig  = figure('Visible', Args.Visible);
    H.Axes = gobjects(1, NF);
    H.Hist = struct();
    for IF = 1:NF
        Base = Args.Fields{IF};
        % Exact column wins; otherwise resolve Base with the member suffix
        % (curated 'Base1' or AllColumns 'Base_1').
        [Val, ColName] = i_pick(T, {Base, sprintf('%s%d', Base, Args.Member), ...
                                          sprintf('%s_%d', Base, Args.Member)});
        ax = subplot(1, NF, IF, 'Parent', H.Fig);
        hold(ax, 'on');

        % Shared bin edges from the pooled finite values of the shown groups.
        Pool = [];
        for G = 1:numel(Keys)
            Pool = [Pool; Val(Cat.(Keys{G}))]; %#ok<AGROW>
        end
        Pool = Pool(isfinite(Pool));
        if isempty(Pool)
            warning('pipeline:last:quality:masks:plotOverlapFlagHist:NoData', ...
                    'Field %s has no finite values for the shown groups.', Base);
            Edges = [0 1];
        elseif range(Pool) == 0
            Edges = [Pool(1)-0.5, Pool(1)+0.5];
        else
            Edges = linspace(min(Pool), max(Pool), Args.NBins + 1);
        end

        Hh = struct();
        for G = 1:numel(Keys)
            V = Val(Cat.(Keys{G}));
            V = V(isfinite(V));
            hh = histogram(ax, V, Edges, 'Normalization', Args.Normalization, ...
                'DisplayStyle', Args.Style, 'EdgeColor', Colors{G}, ...
                'LineWidth', Args.LineWidth, ...
                'DisplayName', sprintf('%s (n=%d)', Labels{G}, numel(V)));
            if strcmp(Args.Style, 'bar')
                hh.FaceColor = Colors{G};
                hh.FaceAlpha = 0.4;
                hh.EdgeColor = 'none';
            end
            Hh.(Keys{G}) = hh;
        end
        H.Hist.(matlab.lang.makeValidName(ColName)) = Hh;

        xlabel(ax, ColName, 'Interpreter','none');
        ylabel(ax, Args.Normalization);
        grid(ax, 'on');  box(ax, 'on');
        legend(ax, 'Location', 'best');
        title(ax, ColName, 'Interpreter', 'none');
        hold(ax, 'off');
        H.Axes(IF) = ax;
    end

    if isempty(Args.Title)
        C = H.Counts;
        if strcmp(Args.Group, 'oneVsBoth')
            Sg = sprintf('one=%d  two=%d', C.ONE, C.BOTH);
        else
            Sg = sprintf('BOTH=%d C1=%d C2=%d NONE=%d', C.BOTH, C.C1, C.C2, C.NONE);
        end
        sgtitle(H.Fig, sprintf('Overlap-flag source histograms  (%s)', Sg), 'Interpreter','none');
    else
        sgtitle(H.Fig, Args.Title, 'Interpreter', 'none');
    end
end


function [v, name] = i_pick(T, Names)
    % First present column among Names; returns its values and resolved name.
    for I = 1:numel(Names)
        if ismember(Names{I}, T.Properties.VariableNames)
            v = double(T.(Names{I}));
            name = Names{I};
            return;
        end
    end
    error('pipeline:last:quality:masks:plotOverlapFlagHist:NoCol', ...
          'None of the columns {%s} found in the Sources table.', strjoin(Names, ', '));
end
