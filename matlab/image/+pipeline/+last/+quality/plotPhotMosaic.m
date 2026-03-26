function plotPhotMosaic(CalibResult, Args)
    % Plot RMS/ZP mosaic and ZP map comparison across modes
    % Description: Two figures:
    %   1. Two-panel mosaic: median fit RMS and center ZP std per crop
    %      (light-to-dark gray colormap).
    %   2. Side-by-side ZP maps for the first visit, one panel per mode.
    %
    % Input  : - CalibResult struct (from calibratePhotModes) with fields:
    %            .PC, .FitRMS, .ZPcenter.
    %          * ...,key,val,...
    %            'Modes'     - Cell array of modes. Required.
    %            'Visits'    - Visit index vector. Default is 1:20.
    %            'Ncrop'     - Number of crops. Default is 24.
    %            'RefCrop'   - Reference crop for ZP maps. Default is 10.
    %            'TileOrder' - 'colmajor'|'rowmajor'. Default is 'rowmajor'.
    % Output : - CalibResult.CLim is NOT modified (caller can read it from
    %            the figure if needed).
    % Author : D. Kovaleva (Mar 2026)
    % Example: pipeline.last.quality.plotPhotMosaic(Calib, 'Modes', {'percrop','perimage'});

    arguments
        CalibResult struct
        Args.Modes cell
        Args.Visits  = 1:20
        Args.Ncrop   = 24
        Args.RefCrop = 10
        Args.TileOrder = 'rowmajor'
    end

    Nmodes  = numel(Args.Modes);
    Nvisits = numel(Args.Visits);
    Nrows = 6;
    Ncols = 4;

    % --- RMS & ZP-RMS Mosaics (single figure, two panels) ---
    % Light-to-dark gray colormap (larger values = darker)
    GrayMap = flipud(gray(256));
    GrayMap = GrayMap(26:230, :);

    RMSmat = CalibResult.FitRMS;
    MedRMS = nanmedian(RMSmat, 1);
    ZPstd  = nanstd(CalibResult.ZPcenter.percrop, 0, 1);

    figure('Name', 'Fit RMS & ZP RMS Mosaic', 'Position', [100, 100, 1000, 600]);

    for Ipanel = 1:2
        subplot(1, 2, Ipanel);

        if Ipanel == 1
            PlotVals = MedRMS;
            CbLabel  = 'Median fit RMS [mag]';
            PanelTitle = sprintf('Median fit RMS over %d epochs', Nvisits);
        else
            PlotVals = ZPstd;
            CbLabel  = 'ZP std [mag]';
            PanelTitle = sprintf('Center ZP std over %d epochs', Nvisits);
        end

        MosaicImg = nan(Nrows, Ncols);
        for Ic = 1:Args.Ncrop
            [Row, Col] = PhotCalibTrans.cropID2RowCol(Ic, Nrows, Ncols, Args.TileOrder);
            if Ic <= numel(PlotVals)
                MosaicImg(Row, Col) = PlotVals(Ic);
            end
        end

        imagesc(MosaicImg);
        axis xy equal tight;
        colormap(gca, GrayMap);
        cb = colorbar;
        ylabel(cb, CbLabel);

        hold on;
        for Ic = 1:Args.Ncrop
            [Row, Col] = PhotCalibTrans.cropID2RowCol(Ic, Nrows, Ncols, Args.TileOrder);
            Val = PlotVals(Ic);
            if isfinite(Val)
                text(Col, Row, sprintf('%d\n%.4f', Ic, Val), ...
                    'HorizontalAlignment', 'center', 'Color', 'w', ...
                    'FontSize', 8, 'FontWeight', 'bold');
            else
                text(Col, Row, sprintf('%d', Ic), ...
                    'HorizontalAlignment', 'center', 'Color', 'w', ...
                    'FontSize', 8);
            end
        end
        hold off;
        title(PanelTitle);
    end

    % --- ZP Mosaic comparison for selected visit ---
    VisitIdx = find(Args.Visits == min(Args.Visits), 1);
    VisitModes = {'perset', 'perset_raw', 'shapeset'};

    % Determine CLim from percrop ZPs
    if isfield(CalibResult.PC, 'percrop') && ~isempty(CalibResult.PC.percrop{VisitIdx})
        PCref = CalibResult.PC.percrop{VisitIdx};
        ZPvals = nan(Args.Ncrop, 1);
        for Ic = 1:numel(PCref)
            if PCref(Ic).Success
                ZPvals(Ic) = PCref(Ic).evaluateZP('X', 863, 'Y', 863);
            end
        end
        CLim = [nanmin(ZPvals) - 0.05, nanmax(ZPvals) + 0.05];

        figure('Position', [50, 50, 500*Nmodes, 500], ...
               'Name', sprintf('ZP Mosaic — Visit %d', Args.Visits(VisitIdx)));
        for Im = 1:Nmodes
            Mode = Args.Modes{Im};
            if ~isfield(CalibResult.PC, Mode); continue; end
            if isempty(CalibResult.PC.(Mode){VisitIdx}); continue; end
            subplot(1, Nmodes, Im);

            if ismember(Mode, VisitModes) && isfield(CalibResult, 'PersetInfo') && ...
               isfield(CalibResult.PersetInfo, Mode)
                % Visit-level modes: evaluate ZP per crop with visit-averaged
                % shape and per-crop Tran2D
                Info = CalibResult.PersetInfo.(Mode);
                PCvis = CalibResult.PC.(Mode){VisitIdx};
                SubImgSize = 1550;
                GridN = 50;

                MosaicFull = nan(Nrows * GridN, Ncols * GridN);
                for Ic = 1:min(Args.Ncrop, numel(PCvis))
                    if ~PCvis(Ic).Success; continue; end

                    CropParams = Info.VisitRefParams;
                    if Info.IsShapeset
                        % shapeset: per-crop Norm, no Tran2D normalization
                        UseRefNorm = false;
                    else
                        % perset/perset_raw: adjusted Norm to target ZP
                        % Compute baseline ZP from visit-averaged shape (not percrop)
                        UseRefNorm = true;
                        ZPvisitBase = PCvis(Ic).evaluateZP( ...
                            'RefTransParams', Info.VisitRefParams, ...
                            'UseRefNorm', true, ...
                            'NormTran2D', Info.DoNormTran2D);
                        if isfinite(ZPvisitBase) && Ic <= numel(Info.TargetZP) && ...
                           isfinite(Info.TargetZP(Ic))
                            DeltaZP = Info.TargetZP(Ic) - ZPvisitBase;
                            CropParams(Info.NormIdx) = ...
                                Info.VisitRefParams(Info.NormIdx) * 10^(DeltaZP / 2.5);
                        end
                    end

                    Xvec = linspace(1, SubImgSize, GridN);
                    Yvec = linspace(1, SubImgSize, GridN);
                    [Xgrid, Ygrid] = meshgrid(Xvec, Yvec);

                    ZP = PCvis(Ic).evaluateZP('X', Xgrid(:), 'Y', Ygrid(:), ...
                        'RefTransParams', CropParams, ...
                        'UseRefNorm', UseRefNorm, ...
                        'NormTran2D', Info.DoNormTran2D);
                    ZPgrid = reshape(ZP, GridN, GridN);

                    [Row, Col] = PhotCalibTrans.cropID2RowCol(Ic, Nrows, Ncols, Args.TileOrder);
                    RowRange = (Row-1)*GridN + (1:GridN);
                    ColRange = (Col-1)*GridN + (1:GridN);
                    MosaicFull(RowRange, ColRange) = ZPgrid;
                end

                imagesc(MosaicFull);
                axis xy equal tight;
                colormap(gca, jet);
                caxis(CLim);
                colorbar;
                hold on;
                for Icol = 1:Ncols-1
                    Xline = Icol * GridN + 0.5;
                    plot([Xline, Xline], [0.5, Nrows*GridN+0.5], 'w-', 'LineWidth', 0.5);
                end
                for Irow = 1:Nrows-1
                    Yline = Irow * GridN + 0.5;
                    plot([0.5, Ncols*GridN+0.5], [Yline, Yline], 'w-', 'LineWidth', 0.5);
                end
                for Ilbl = 1:Args.Ncrop
                    [RowL, ColL] = PhotCalibTrans.cropID2RowCol(Ilbl, Nrows, Ncols, Args.TileOrder);
                    text((ColL - 0.5) * GridN, (RowL - 0.5) * GridN, sprintf('%d', Ilbl), ...
                        'HorizontalAlignment', 'center', 'Color', 'w', ...
                        'FontSize', 8, 'FontWeight', 'bold');
                end
                hold off;
            else
                % Per-epoch modes: use plotZPMap with the actual PC objects
                % Map quality mode names to plotZPMap PhotSys names
                PhotSysMap = containers.Map( ...
                    {'percrop','shapeimage','perimage','perimage_raw'}, ...
                    {'percrop','refshape',  'refzp',   'refzp_raw'});
                if PhotSysMap.isKey(Mode)
                    PhotSysName = PhotSysMap(Mode);
                else
                    PhotSysName = 'percrop';
                end
                CalibResult.PC.(Mode){VisitIdx}.plotZPMap('NewFigure', false, ...
                    'CLim', CLim, 'SmoothSigma', 0, ...
                    'PhotSys', PhotSysName, 'RefCrop', Args.RefCrop, ...
                    'TileOrder', Args.TileOrder);
            end
            title(Mode);
        end
    end
end
