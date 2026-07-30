function H = plotOverlapFlagSources(Src, Args)
    % Scatter overlap-matched sources, colour-coded by Overlap-flag category.
    % Description: Plots the per-pair source rows produced by
    %   pipeline.last.quality.masks.overlapFlagTally (with 'KeepSources',true)
    %   over a chosen coordinate frame - sky (RA/Dec), native crop pixels
    %   (X/Y), or full-frame pixels (XFULL/YFULL) - colouring each matched
    %   pair by which crops carry the 'Overlap' bit. The four categories are
    %   mutually exclusive and partition every matched pair:
    %     'BOTH' - flagged in BOTH crops
    %     'C1'   - flagged ONLY in crop 1 (the first crop of the pair, Ind(:,1))
    %     'C2'   - flagged ONLY in crop 2 (the second crop, Ind(:,2))
    %     'NONE' - flagged in NEITHER
    %   In the 'XY' (native) frame the two crop copies of a pair sit at
    %   different pixel positions, so both members are plotted by default
    %   (PlotBothMembersXY); in 'RADec'/'XYFULL' the two copies coincide, so
    %   one point per pair is drawn (member 1). XFULL/YFULL come from
    %   imProc.cat.addXYfull upstream (NaN where a crop lacked ORIGSEC).
    %
    % Input  : - Src: either the Report struct returned by overlapFlagTally
    %            (its .Sources table is used) or the Sources table directly.
    %          * ...,key,val,...
    %            'Coord'      - 'RADec' | 'XY' | 'XYFULL'. Default 'RADec'.
    %            'Show'       - Cellstr subset of {'BOTH','C1','C2','NONE'} to
    %                           draw. Default {'BOTH','C1','C2'}.
    %            'ColorBoth'  - RGB for 'BOTH'. Default [0.85 0.10 0.10] (red).
    %            'ColorC1'    - RGB for 'C1'.   Default [0.10 0.35 0.85] (blue).
    %            'ColorC2'    - RGB for 'C2'.   Default [0.15 0.60 0.20] (green).
    %            'ColorNone'  - RGB for 'NONE'. Default [0.70 0.70 0.70] (gray).
    %            'MarkerSize' - Scatter marker area. Default 16.
    %            'PlotBothMembersXY' - In 'XY', plot both crop members per
    %                           pair. Default true. Ignored for RADec/XYFULL.
    %            'InvertRA'   - Reverse the RA axis (astro convention) in
    %                           'RADec'. Default true.
    %            'Axes'       - Target axes handle. Default [] (new figure).
    %            'Legend'     - Draw a legend with per-category pair counts.
    %                           Default true.
    %            'Title'      - Title string. Default '' (auto).
    %            'Visible'    - Figure visibility 'on'|'off'. Default 'on'.
    % Output : - H struct with fields:
    %            .Fig, .Axes - graphics handles.
    %            .Scatter    - struct of scatter handles by drawn category.
    %            .Counts     - struct .BOTH/.C1/.C2/.NONE pair counts (whole
    %                          table, independent of Show).
    % Author : D. Kovaleva (Jul 2026)
    % See also: pipeline.last.quality.masks.overlapFlagTally,
    %           imProc.cat.addXYfull.
    % Example:
    %   R = pipeline.last.quality.masks.overlapFlagTally(AI, 'KeepSources',true);
    %   pipeline.last.quality.masks.plotOverlapFlagSources(R);                   % RA/Dec
    %   pipeline.last.quality.masks.plotOverlapFlagSources(R, 'Coord','XYFULL');
    %   pipeline.last.quality.masks.plotOverlapFlagSources(R.Sources, ...
    %           'Coord','XY', 'Show',{'BOTH','C1','C2','NONE'});
    arguments
        Src
        Args.Coord      (1,:) char {mustBeMember(Args.Coord,{'RADec','XY','XYFULL'})} = 'RADec'
        Args.Show             cell = {'BOTH','C1','C2'}
        Args.ColorBoth  (1,3) double = [0.85 0.10 0.10]
        Args.ColorC1    (1,3) double = [0.10 0.35 0.85]
        Args.ColorC2    (1,3) double = [0.15 0.60 0.20]
        Args.ColorNone  (1,3) double = [0.70 0.70 0.70]
        Args.MarkerSize (1,1) double = 16
        Args.PlotBothMembersXY logical = true
        Args.InvertRA         logical = true
        Args.Axes             = []
        Args.Legend           logical = true
        Args.Title      (1,:) char = ''
        Args.Visible    (1,:) char {mustBeMember(Args.Visible,{'on','off'})} = 'on'
    end

    % --- Resolve the Sources table ---------------------------------------
    if istable(Src)
        T = Src;
    elseif isstruct(Src) && isfield(Src, 'Sources') && istable(Src.Sources)
        T = Src.Sources;
    else
        error('pipeline:last:quality:masks:plotOverlapFlagSources:BadInput', ...
              'Src must be a Sources table or an overlapFlagTally Report struct.');
    end
    if height(T) == 0
        error('pipeline:last:quality:masks:plotOverlapFlagSources:Empty', ...
              'Sources table is empty - run overlapFlagTally with ''KeepSources'',true.');
    end

    % --- Categories (per pair) -------------------------------------------
    O1 = T.Ovlp1 == 1;
    O2 = T.Ovlp2 == 1;
    Cat.BOTH = O1 & O2;
    Cat.C1   = O1 & ~O2;    % flagged ONLY in crop 1
    Cat.C2   = ~O1 & O2;    % flagged ONLY in crop 2
    Cat.NONE = ~O1 & ~O2;
    H.Counts = struct('BOTH', nnz(Cat.BOTH), 'C1', nnz(Cat.C1), ...
                      'C2', nnz(Cat.C2), 'NONE', nnz(Cat.NONE));

    % --- Coordinate columns ----------------------------------------------
    % Resolve coordinate columns, accepting both the curated names ('RA1')
    % and the AllColumns names ('RA_1').
    switch Args.Coord
        case 'RADec'
            XA = i_pick(T,{'RA1','RA_1'});     YA = i_pick(T,{'Dec1','Dec_1'});
            XB = i_pick(T,{'RA2','RA_2'});     YB = i_pick(T,{'Dec2','Dec_2'});
            XLab = 'RA [deg]';  YLab = 'Dec [deg]';  TwoMembers = false;
        case 'XYFULL'
            XA = i_pick(T,{'XFULL1','XFULL_1'}); YA = i_pick(T,{'YFULL1','YFULL_1'});
            XB = i_pick(T,{'XFULL2','XFULL_2'}); YB = i_pick(T,{'YFULL2','YFULL_2'});
            XLab = 'XFULL [pix]'; YLab = 'YFULL [pix]'; TwoMembers = false;
        otherwise  % 'XY'
            XA = i_pick(T,{'X1','X_1'});       YA = i_pick(T,{'Y1','Y_1'});
            XB = i_pick(T,{'X2','X_2'});       YB = i_pick(T,{'Y2','Y_2'});
            XLab = 'X [pix]';   YLab = 'Y [pix]';   TwoMembers = Args.PlotBothMembersXY;
    end
    if all(isnan(XA)) && all(isnan(YA))
        warning('pipeline:last:quality:masks:plotOverlapFlagSources:AllNaN', ...
                'All %s coordinates are NaN (missing ORIGSEC?) - nothing to plot.', Args.Coord);
    end

    % --- Axes ------------------------------------------------------------
    if isempty(Args.Axes)
        H.Fig  = figure('Visible', Args.Visible);
        H.Axes = axes(H.Fig);
    else
        H.Axes = Args.Axes;
        H.Fig  = ancestor(H.Axes, 'figure');
    end
    hold(H.Axes, 'on');

    % --- Draw (none behind, both in front) -------------------------------
    Order  = {'NONE','C1','C2','BOTH'};   % background -> foreground
    Colors = struct('NONE', Args.ColorNone, 'C1', Args.ColorC1, ...
                    'C2', Args.ColorC2, 'BOTH', Args.ColorBoth);
    Labels = struct('NONE', 'neither', 'C1', 'only crop 1', ...
                    'C2', 'only crop 2', 'BOTH', 'both crops');
    H.Scatter = struct();
    for I = 1:numel(Order)
        Key = Order{I};
        Draw = any(strcmp(Args.Show, Key));
        if Draw
            Mask = Cat.(Key);
            if TwoMembers
                X = [XA(Mask); XB(Mask)];
                Y = [YA(Mask); YB(Mask)];
            else
                X = XA(Mask);
                Y = YA(Mask);
            end
            Hs = scatter(H.Axes, X, Y, Args.MarkerSize, Colors.(Key), 'filled', ...
                'MarkerFaceAlpha', 0.7, 'MarkerEdgeColor', 'none', ...
                'DisplayName', sprintf('%s (n=%d)', Labels.(Key), nnz(Mask)));
            H.Scatter.(Key) = Hs;
        end
    end

    % --- Cosmetics -------------------------------------------------------
    xlabel(H.Axes, XLab);
    ylabel(H.Axes, YLab);
    grid(H.Axes, 'on');
    box(H.Axes, 'on');
    if strcmp(Args.Coord, 'RADec')
        if Args.InvertRA
            set(H.Axes, 'XDir', 'reverse');
        end
    else
        daspect(H.Axes, [1 1 1]);   % pixels: square aspect
    end
    if isempty(Args.Title)
        title(H.Axes, sprintf('Overlap-flag sources (%s):  BOTH=%d  C1=%d  C2=%d  NONE=%d', ...
            Args.Coord, H.Counts.BOTH, H.Counts.C1, H.Counts.C2, H.Counts.NONE), 'Interpreter','none');
    else
        title(H.Axes, Args.Title, 'Interpreter', 'none');
    end
    if Args.Legend
        legend(H.Axes, 'Location', 'best');
    end
    hold(H.Axes, 'off');
end


function v = i_pick(T, Names)
    % Return the first present column among Names (curated or _1/_2 form).
    for I = 1:numel(Names)
        if ismember(Names{I}, T.Properties.VariableNames)
            v = double(T.(Names{I}));
            return;
        end
    end
    error('pipeline:last:quality:masks:plotOverlapFlagSources:NoCol', ...
          'None of the columns {%s} found in the Sources table.', strjoin(Names, ', '));
end
