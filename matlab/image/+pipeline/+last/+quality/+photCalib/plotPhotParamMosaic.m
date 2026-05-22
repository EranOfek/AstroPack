function Result = plotPhotParamMosaic(PC, Args)
    % Per-crop mosaic map of one or more fitted PhotCalibTrans parameters
    % Description: For a single epoch (one 1xNcrop PhotCalibTrans array),
    %              extracts per-crop scalar values of the requested
    %              quantities via resolvePCParam and lays them out as a
    %              6x4 mosaic (one panel per quantity) using the same
    %              tile geometry as obsolete/plotPhotMosaic. Tile color
    %              encodes the value; tile text shows the crop index and
    %              the value. Light-to-dark gray colormap by default
    %              (larger values = darker).
    %
    %              Companion of plotPhotFittedParams (which plots per-crop
    %              evolution vs epoch as line plots): use plotPhotFittedParams
    %              for many epochs over time, plotPhotParamMosaic for the
    %              spatial layout in one epoch.
    %
    % Input  : - PC: PhotCalibTrans array (1xNcrop), OR any source
    %            accepted by resolveInput (private helper)
    %            (Result struct, mode-keyed PC struct, etc.). Multi-visit
    %            sources collapse to the first non-empty visit.
    %          * ...,key,val,...
    %            'ParamNames'   - Cell of names to plot. Each is resolved
    %                             via resolvePCParam, so any of the usual
    %                             names works: fitted transmission params
    %                             ('TauAod500','PWV_cm','Center_Ang','Norm'),
    %                             fit-quality scalars ('RMS','Chi2','DOF',
    %                             'Chi2_DOF'), PhotCalibTrans properties
    %                             ('AirMass','Temp','ARMS', ...), or
    %                             SourceData summaries ('NCalib','MedResidual', ...).
    %                             Default {'TauAod500','PWV_cm','Center_Ang','Norm'}.
    %            'CropsToAnalyze' - Crop indices to include (others left
    %                             blank in the mosaic). Default [] (all).
    %            'Ncrop'        - Number of crops. Default 24.
    %            'Nrows','Ncols' - Mosaic grid. Default 6 x 4.
    %            'TileOrder'    - 'rowmajor' (default) or 'colmajor'.
    %            'Colormap'     - colormap to apply (matrix or builtin name).
    %                             Default = light-to-dark gray.
    %            'ShowValues'   - Annotate each tile with crop ID + value.
    %                             Default true.
    %            'ValueFormat'  - sprintf format for the per-tile value.
    %                             Default '%.3g'.
    %            'CLim'         - 1xK cell of [lo hi] color limits per
    %                             quantity. Empty entries / cell = auto.
    %                             Default {}.
    %            'FigPosition'  - [x y w h] for the figure. Default scales
    %                             with number of params and the optional
    %                             histogram row: [100 100 380*K 480*Nrows].
    %            'AddHistograms' - When true, add a second row of panels
    %                             below the mosaics: per-parameter
    %                             histograms with central and peripheral
    %                             crops drawn as overlaid translucent bars
    %                             on the same bin grid (medians marked
    %                             with vertical lines). Central crop set
    %                             is determined from TileOrder. Default false.
    %            'HistBins'     - Number of bins (or vector of edges) for
    %                             the histogram row. Default 10.
    %            'HistColors'   - 2x3 matrix [central; peripheral] face
    %                             colors. Default greyscale —
    %                             [0.40 0.40 0.40; 0.85 0.85 0.85] (dark
    %                             grey central, light grey peripheral).
    %                             Peripheral bars get a bold black edge
    %                             so the two groups stay distinguishable.
    %            'HistEdgeWidth' - Line width on the peripheral bar edges.
    %                             Default 1.4.
    %            'RefValues'    - Struct of reference values keyed by
    %                             parameter name. When a parameter has a
    %                             reference, the histogram panel for that
    %                             parameter gets a secondary (top) X axis
    %                             whose ticks show (Ref - X), so the
    %                             reader can read off the offset directly.
    %                             Default struct('Center_Ang', 5709.73).
    %                             Pass struct() to disable.
    %            'Verbose'      - Print summary. Default false.
    % Output : - Result struct with fields:
    %            .ParamNames    - 1xK cell of names plotted.
    %            .Values        - struct, .(SanitizedName) = 1xNcrop vector.
    %            .CropIDs       - 1xNcrop vector of crop indices.
    %            .IsCentralCrop - 1xNcrop logical (true for central crops).
    %            .HistStats     - (only when AddHistograms=true) struct
    %                             keyed by sanitized param name, each:
    %                             .NCentral, .NPeripheral,
    %                             .MedCentral, .StdCentral,
    %                             .MedPeripheral, .StdPeripheral.
    % Author : D. Kovaleva (May 2026)
    % Example: % After fitting one coadd:
    %          [~, PC] = imProc.calib.fitPhotCalibTrans(Coadd);
    %          R = pipeline.last.quality.photCalib.plotPhotParamMosaic(PC);
    %
    %          % Map only one parameter, with a per-tile value format:
    %          R = pipeline.last.quality.photCalib.plotPhotParamMosaic(PC, ...
    %                  'ParamNames', {'TauAod500'}, 'ValueFormat', '%.4f');
    %
    %          % Mix fitted params with fit-quality scalars:
    %          R = pipeline.last.quality.photCalib.plotPhotParamMosaic(PC, ...
    %                  'ParamNames', {'TauAod500','PWV_cm','Chi2_DOF','ARMS'});
    %
    %          % Add a row of central/peripheral histograms below the mosaics:
    %          R = pipeline.last.quality.photCalib.plotPhotParamMosaic(PC, ...
    %                  'AddHistograms', true);
    %          R.HistStats.TauAod500.MedCentral
    %          R.HistStats.TauAod500.MedPeripheral

    arguments
        PC
        Args.ParamNames     cell    = {'TauAod500','PWV_cm','Center_Ang','Norm'}
        Args.CropsToAnalyze         = []
        Args.Ncrop                  = 24
        Args.Nrows                  = 6
        Args.Ncols                  = 4
        Args.TileOrder      char    = 'rowmajor'
        Args.Colormap               = []
        Args.ShowValues     logical = true
        Args.ValueFormat    char    = '%.3g'
        Args.CLim           cell    = {}
        Args.FigPosition            = []
        Args.AddHistograms  logical = false
        Args.HistBins               = 10
        Args.HistColors             = [0.40 0.40 0.40; 0.85 0.85 0.85]   % central (dark grey), peripheral (light grey)
        Args.HistEdgeWidth          = 2.4    % line width on peripheral bar edges
        Args.RefValues      struct  = struct('Center_Ang', 5709.73)
        Args.Verbose        logical = false
    end

    % --- Resolve PC to a single 1xNcrop PhotCalibTrans array ------------
    if isa(PC, 'PhotCalibTrans')
        PCarray = PC(:).';
    else
        PCcell = resolveInput(PC);
        Idx = find(~cellfun(@isempty, PCcell), 1);
        if isempty(Idx)
            error('plotPhotParamMosaic:NoPC', ...
                'No non-empty PhotCalibTrans array found in input.');
        end
        if numel(PCcell) > 1 && Args.Verbose
            fprintf(['plotPhotParamMosaic: multi-visit input, using ', ...
                     'visit %d (first non-empty).\n'], Idx);
        end
        PCarray = PCcell{Idx}(:).';
    end

    Args.Ncrop = numel(PCarray);
    if isempty(Args.CropsToAnalyze)
        CropIdx = 1:Args.Ncrop;
    else
        CropIdx = Args.CropsToAnalyze(:).';
        CropIdx = CropIdx(CropIdx >= 1 & CropIdx <= Args.Ncrop);
    end

    % --- Extract per-crop values for each quantity ----------------------
    Names  = Args.ParamNames(:).';
    Nq     = numel(Names);
    Fields = cellfun(@matlab.lang.makeValidName, Names, 'UniformOutput', false);

    Values = struct();
    for Iq = 1:Nq
        V = nan(1, Args.Ncrop);
        for K = 1:numel(CropIdx)
            Ic = CropIdx(K);
            V(Ic) = resolvePCParam( ...
                PCarray(Ic), Names{Iq});
        end
        Values.(Fields{Iq}) = V;
        if Args.Verbose
            fprintf('  %-14s : N_finite = %d/%d  med=%.4g\n', ...
                Names{Iq}, nnz(isfinite(V)), Args.Ncrop, ...
                median(V, 'omitnan'));
        end
    end

    % --- Colormap -------------------------------------------------------
    if isempty(Args.Colormap)
        Cmap = flipud(gray(256));
        Cmap = Cmap(26:230, :);
    elseif ischar(Args.Colormap) || isstring(Args.Colormap)
        Cmap = colormap(char(Args.Colormap));
    else
        Cmap = Args.Colormap;
    end

    % --- Central / peripheral classification ----------------------------
    CentralCrops = centralCrops(Args.TileOrder);
    IsCentralCrop = false(1, Args.Ncrop);
    IsCentralCrop(CentralCrops(CentralCrops <= Args.Ncrop)) = true;

    % --- Render ---------------------------------------------------------
    NumRows = 1 + double(Args.AddHistograms);
    if isempty(Args.FigPosition)
        FigPos = [100, 100, 380*Nq, 480 * NumRows];
    else
        FigPos = Args.FigPosition;
    end
    figure('Name', sprintf('PhotCalib param mosaic (%s)', strjoin(Names, ', ')), ...
           'Position', FigPos);

    HistStats = struct();
    for Iq = 1:Nq
        subplot(NumRows, Nq, Iq);
        PlotVals = Values.(Fields{Iq});

        MosaicImg = nan(Args.Nrows, Args.Ncols);
        for Ic = 1:Args.Ncrop
            [Row, Col] = PhotCalibTrans.cropID2RowCol( ...
                Ic, Args.Nrows, Args.Ncols, Args.TileOrder);
            if Ic <= numel(PlotVals)
                MosaicImg(Row, Col) = PlotVals(Ic);
            end
        end

        imagesc(MosaicImg);
        axis xy equal tight;
        colormap(gca, Cmap);
        cb = colorbar;
        ylabel(cb, Names{Iq}, 'Interpreter', 'none');

        % Optional CLim per quantity
        if numel(Args.CLim) >= Iq && ~isempty(Args.CLim{Iq})
            caxis(Args.CLim{Iq});
        end

        % Tile labels
        if Args.ShowValues
            hold on;
            for Ic = 1:Args.Ncrop
                [Row, Col] = PhotCalibTrans.cropID2RowCol( ...
                    Ic, Args.Nrows, Args.Ncols, Args.TileOrder);
                Val = PlotVals(Ic);
                if isfinite(Val)
                    text(Col, Row, ...
                        sprintf(['%d\n', Args.ValueFormat], Ic, Val), ...
                        'HorizontalAlignment', 'center', ...
                        'Color', 'w', 'FontSize', 8, 'FontWeight', 'bold', ...
                        'Interpreter', 'none');
                else
                    text(Col, Row, sprintf('%d', Ic), ...
                        'HorizontalAlignment', 'center', ...
                        'Color', 'w', 'FontSize', 8, ...
                        'Interpreter', 'none');
                end
            end
            hold off;
        end

        title(Names{Iq}, 'Interpreter', 'none');
        xlabel(''); ylabel('');
        set(gca, 'XTick', [], 'YTick', []);

        % --- Histogram row (central vs peripheral) ----------------------
        if Args.AddHistograms
            VC = PlotVals(IsCentralCrop);
            VP = PlotVals(~IsCentralCrop);
            VC = VC(isfinite(VC));
            VP = VP(isfinite(VP));

            S.NCentral    = numel(VC);
            S.NPeripheral = numel(VP);
            if S.NCentral > 0
                S.MedCentral = median(VC);
                S.StdCentral = std(VC);
            else
                S.MedCentral = NaN; S.StdCentral = NaN;
            end
            if S.NPeripheral > 0
                S.MedPeripheral = median(VP);
                S.StdPeripheral = std(VP);
            else
                S.MedPeripheral = NaN; S.StdPeripheral = NaN;
            end
            HistStats.(Fields{Iq}) = S;

            subplot(NumRows, Nq, Nq + Iq); hold on;
            % Common edges so the two histograms align bin-for-bin
            AllVals = [VC(:); VP(:)];
            if isscalar(Args.HistBins)
                if isempty(AllVals)
                    Edges = linspace(0, 1, 2);
                else
                    Edges = linspace(min(AllVals), max(AllVals), ...
                        Args.HistBins + 1);
                end
            else
                Edges = Args.HistBins;
            end
            ColCen = Args.HistColors(1, :);
            ColPer = Args.HistColors(2, :);
            % Peripheral first (so central draws on top); peripheral has
            % a bold black edge to stand apart from the dark central fill.
            HP = histogram(VP, Edges, 'FaceColor', ColPer, ...
                'FaceAlpha', 1.0, 'EdgeColor', 'k', ...
                'LineWidth', Args.HistEdgeWidth, ...
                'DisplayName', sprintf('peripheral (N=%d)', S.NPeripheral));
            HC = histogram(VC, Edges, 'FaceColor', ColCen, ...
                'FaceAlpha', 0.85, 'EdgeColor', ColCen * 0.6, ...
                'LineWidth', 0.5, ...
                'DisplayName', sprintf('central (N=%d)', S.NCentral));
            grid on; box on;
            xlabel(Names{Iq}, 'Interpreter', 'none');
            ylabel('Count');
            legend([HC, HP], 'Location', 'best', 'Interpreter', 'none', ...
                'FontSize', 8);
            title(sprintf('%s   med_c=%.4g  med_p=%.4g', Names{Iq}, ...
                S.MedCentral, S.MedPeripheral), 'Interpreter', 'none');
            hold off;

            % --- Secondary X axis: distance from reference value -------
            % If the current parameter has a reference value in RefValues,
            % overlay a top X axis whose tick labels show (Ref - X) so
            % readers can read off the offset directly.
            FName = Fields{Iq};
            if isfield(Args.RefValues, FName)
                Ref     = Args.RefValues.(FName);
                AxBot   = gca;
                XL      = xlim(AxBot);
                NumTick = 5;
                XTickB  = linspace(XL(1), XL(2), NumTick);
                XTickT  = Ref - XTickB;   % transformed values shown on top
                AxTop   = axes('Position',       AxBot.Position, ...
                               'XAxisLocation',  'top', ...
                               'YAxisLocation',  'right', ...
                               'Color',          'none', ...
                               'YColor',         'none', ...
                               'XLim',           XL, ...
                               'XTick',          XTickB, ...
                               'XTickLabel',     arrayfun( ...
                                   @(v) sprintf('%g', v), XTickT, ...
                                   'UniformOutput', false));
                xlabel(AxTop, sprintf('%s_0 - %s   (ref %s_0 = %g)', ...
                    Names{Iq}, Names{Iq}, Names{Iq}, Ref), ...
                    'Interpreter', 'none');
                % Hand control back to AxBot so the next subplot call
                % advances to the next panel as expected.
                axes(AxBot);
            end
        end
    end

    % --- Output ---------------------------------------------------------
    Result.ParamNames   = Names;
    Result.Values       = Values;
    Result.CropIDs      = 1:Args.Ncrop;
    Result.IsCentralCrop = IsCentralCrop;
    if Args.AddHistograms
        Result.HistStats = HistStats;
    end
end
