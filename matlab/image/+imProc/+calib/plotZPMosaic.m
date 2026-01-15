function Fig = plotZPMosaic(AI, Args)
    % Plot mosaic of zero point maps from array of AstroImages
    % Input  : - AI - Array of AstroImage objects with ZP column in catalog
    %          * ...,key,val,...
    %            'ZPColName' - ZP column name in catalog. Default is 'ZP'.
    %            'RAKey' - Header key for RA center. Default is 'CRVAL1'.
    %            'DecKey' - Header key for Dec center. Default is 'CRVAL2'.
    %            'GridSize' - Grid resolution for each subimage [Nx, Ny]. Default is [30, 30].
    %            'CLim' - Color limits [min max]. Default is [] (auto).
    % Output : - Fig - Figure handle
    % Author : D. Kovaleva (Jan 2026)
    % Example: Fig = imProc.calib.plotZPMosaic(AI);

    arguments
        AI
        Args.ZPColName = 'ZP'
        Args.RAKey = 'CRVAL1'
        Args.DecKey = 'CRVAL2'
        Args.GridSize = [30, 30]
        Args.CLim = []
    end

    Nobj = numel(AI);

    % Extract RA, Dec centers from headers
    RA = zeros(Nobj, 1);
    Dec = zeros(Nobj, 1);
    for i = 1:Nobj
        RA(i) = AI(i).HeaderData.getVal(Args.RAKey);
        Dec(i) = AI(i).HeaderData.getVal(Args.DecKey);
    end

    % Convert to relative positions (arcmin from center)
    RAcenter = mean(RA);
    DecCenter = mean(Dec);
    dRA = (RA - RAcenter) * cosd(DecCenter) * 60;   % arcmin
    dDec = (Dec - DecCenter) * 60;                   % arcmin

    % Determine grid layout from positions
    % Find unique rows and columns based on clustering
    [~, ~, colIdx] = unique(round(dRA * 10) / 10);
    [~, ~, rowIdx] = unique(round(dDec * 10) / 10);
    Ncols = max(colIdx);
    Nrows = max(rowIdx);

    % Create figure
    Fig = figure('Position', [100, 100, 200*Ncols, 200*Nrows]);

    % Collect all ZP values for consistent color scale
    allZP = [];
    for i = 1:Nobj
        if AI(i).CatData.isColumn(Args.ZPColName)
            ZPvals = AI(i).CatData.getCol(Args.ZPColName);
            allZP = [allZP; ZPvals(isfinite(ZPvals))];
        end
    end

    if isempty(Args.CLim) && ~isempty(allZP)
        Args.CLim = [prctile(allZP, 1), prctile(allZP, 99)];
    end

    % Plot each subimage
    for i = 1:Nobj
        % Get subplot position (row 1 = top)
        subplotIdx = (Nrows - rowIdx(i)) * Ncols + colIdx(i);
        subplot(Nrows, Ncols, subplotIdx);

        % Get catalog data
        Cat = AI(i).CatData;
        if ~Cat.isColumn(Args.ZPColName)
            title(sprintf('#%d: No ZP', i));
            continue;
        end

        X = Cat.getCol('X');
        Y = Cat.getCol('Y');
        ZP = Cat.getCol(Args.ZPColName);

        % Create interpolated grid
        ValidIdx = isfinite(ZP) & isfinite(X) & isfinite(Y);
        if sum(ValidIdx) < 10
            title(sprintf('#%d: Too few points', i));
            continue;
        end

        Xv = X(ValidIdx);
        Yv = Y(ValidIdx);
        ZPv = ZP(ValidIdx);

        % Create grid
        Xvec = linspace(min(Xv), max(Xv), Args.GridSize(1));
        Yvec = linspace(min(Yv), max(Yv), Args.GridSize(2));
        [Xgrid, Ygrid] = meshgrid(Xvec, Yvec);

        % Interpolate ZP onto grid
        F = scatteredInterpolant(Xv, Yv, ZPv, 'natural', 'none');
        ZPgrid = F(Xgrid, Ygrid);

        % Plot
        imagesc(Xvec, Yvec, ZPgrid);
        if ~isempty(Args.CLim)
            caxis(Args.CLim);
        end
        axis xy equal tight;
        set(gca, 'XTick', [], 'YTick', []);
    end

    % Add colorbar
    colormap(jet);
    cb = colorbar('Position', [0.92, 0.1, 0.02, 0.8]);
    ylabel(cb, 'ZP [mag]');

    % Add title
    sgtitle('Zero Point Mosaic');
end
