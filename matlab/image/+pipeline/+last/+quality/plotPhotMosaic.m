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
    % Example: pipeline.last.quality.plotPhotMosaic(Calib, 'Modes', {'percrop','refzp'});

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
    ZPstd  = nanstd(CalibResult.ZPcenter, 0, 1);

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
    % Find first mode that has PC data for this visit
    FirstMode = '';
    for Itmp = 1:Nmodes
        M = Args.Modes{Itmp};
        if isfield(CalibResult.PC, M) && ~isempty(CalibResult.PC.(M){VisitIdx})
            FirstMode = M;
            break;
        end
    end
    if ~isempty(FirstMode)
        PCref = CalibResult.PC.(FirstMode){VisitIdx};
        ZPvals = nan(Args.Ncrop, 1);
        for Ic = 1:numel(PCref)
            if PCref(Ic).Success
                ZPvals(Ic) = PCref(Ic).evaluateZP('X', 863, 'Y', 863);
            end
        end
        CLim = [min(ZPvals) - 0.05, max(ZPvals) + 0.05];

        figure('Position', [50, 50, 500*Nmodes, 500], ...
               'Name', sprintf('ZP Mosaic — Visit %d', Args.Visits(VisitIdx)));
        for Im = 1:Nmodes
            Mode = Args.Modes{Im};
            if ~isfield(CalibResult.PC, Mode); continue; end
            if isempty(CalibResult.PC.(Mode){VisitIdx}); continue; end
            subplot(1, Nmodes, Im);
            CalibResult.PC.(Mode){VisitIdx}.plotZPMap('NewFigure', false, ...
                'CLim', CLim, 'SmoothSigma', 0, ...
                'PhotSys', Mode, 'RefCrop', Args.RefCrop, ...
                'TileOrder', Args.TileOrder);
            title(Mode);
        end
    end
end
