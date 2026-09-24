function [Report, Fig] = mapAperCorrGradients(AI, Args)
    % Full-frame map of the position-dependent aperture correction per crop.
    % Description: For every crop of a visit, fits the position-dependent
    %   aperture correction MagDiff(X,Y) (bilinear, via
    %   PhotCalibTrans.calcAperCorr) for one aperture, then assembles the fitted
    %   surfaces over their full-frame footprints into a single mosaic image
    %   (like PhotCalibTrans.plotZPMap). This shows directly whether the
    %   correction is the genuine field-dependent PSF effect - a smooth RADIAL
    %   BOWL centred near the frame centre (least-negative at centre, most
    %   negative at the corners) - or a crop-local artifact - the SAME tile
    %   repeated in every crop.
    %
    %   A single number, Report.Radiality, quantifies it: the mean cos-angle
    %   between each crop's correction gradient (Cx,Cy) and the inward
    %   direction (frame centre - crop centre). ~+1 => arrows converge inward
    %   (physical radial); ~0 => arrows all parallel (crop-local artifact);
    %   ~-1 => radial outward. Set 'PlotStyle','quiver' (or 'both') to see the
    %   per-crop gradient arrows instead of / on top of the mosaic.
    %
    % Input  : - AI: AstroImage array of one visit's crops, element i = crop i,
    %            each with CatData populated. Crop footprint in the full frame
    %            comes from the ORIGSEC header keyword; if absent, a regular
    %            grid is built from the crop index via cropID2RowCol.
    %          * ...,key,val,...
    %            'ApColName'   - Aperture flux column to map. Default
    %                            'FLUX_APER_1' (smallest -> strongest signal).
    %            'CalcCorrType'- 'flux' (default) | 'mag' for the fit.
    %            'RefFluxCol'  - Reference flux column. Default 'FLUX_APER_3'.
    %            'SNColName'/'MinSN' - S/N selection. Default 'SN' / 30.
    %            'ColX'/'ColY' - Position columns. Default 'X'/'Y'.
    %            'CCDSEC'      - Per-crop section for the [-1,1] normalization.
    %                            Default [1 1726 1 1726] (LAST per-crop).
    %            'KeyOrigSec'  - Full-frame section keyword. Default 'ORIGSEC'.
    %            'Nrows'/'Ncols'/'TileOrder' - grid used ONLY when ORIGSEC is
    %                            absent (cropID2RowCol). TileOrder 'rowmajor'
    %                            (new pipeline, IDs fill left->right then
    %                            bottom->top) | 'colmajor' (old pipeline, IDs
    %                            fill bottom->top then left->right). Default
    %                            6 / 4 / 'rowmajor'.
    %            'MagColPrefix'- Mag prefix for 'mag' mode. Default 'MAG_AB_'.
    %            'PlotStyle'   - 'map' (default, mosaic) | 'quiver' (gradient
    %                            arrows) | 'both'.
    %            'GridPerCrop' - surface samples per axis per crop. Default 15.
    %            'GridFull'    - mosaic grid points per axis. Default 250.
    %            'ArrowScale'  - quiver scale factor. Default 1.
    %            'Plot'/'Visible' - Default true / 'on'.
    % Output : - Report struct: .Table (CropId, Xc, Yc, C0, Cx, Cy, Cxy,
    %            Nstars, InwardCos), .Radiality, .FrameCenter, .ApColName,
    %            .MapXY {gx,gy}, .Map (assembled mosaic image; NaN outside).
    %            .Calib - table of the aperture-correction calibrators used
    %                     (the sources passing the S/N + bad-FLAGS selection,
    %                     NOT the Gaia photometric calibrators), across all
    %                     crops: CropId, XFULL, YFULL, X, Y, MagDiff.
    %          - Fig: figure handle ([] if Plot=false).
    % Author : D. Kovaleva (Aug 2026)
    % See also: PhotCalibTrans.plotZPMap,
    %           pipeline.last.quality.photCalib.plotAperCorrPositional,
    %           PhotCalibTrans.calcAperCorr, PhotCalibTrans.cropID2RowCol.
    % Example:
    %   [Rep,~] = pipeline.last.quality.photCalib.mapAperCorrGradients(Coadd0);
    %   fprintf('radiality = %+.2f\n', Rep.Radiality);
    arguments
        AI
        Args.ApColName    (1,:) char = 'FLUX_APER_1'
        Args.CalcCorrType (1,:) char {mustBeMember(Args.CalcCorrType,{'flux','mag'})} = 'flux'
        Args.RefFluxCol   (1,:) char = 'FLUX_APER_3'
        Args.SNColName    (1,:) char = 'SN'
        Args.MinSN        (1,1) double = 30
        Args.ColX         (1,:) char = 'X'
        Args.ColY         (1,:) char = 'Y'
        Args.CCDSEC       double = [1 1726 1 1726]
        Args.KeyOrigSec   (1,:) char = 'ORIGSEC'
        Args.Nrows        (1,1) double = 6
        Args.Ncols        (1,1) double = 4
        Args.TileOrder    (1,:) char {mustBeMember(Args.TileOrder,{'rowmajor','colmajor'})} = 'rowmajor'
        Args.MagColPrefix (1,:) char = 'MAG_AB_'
        Args.PlotStyle    (1,:) char {mustBeMember(Args.PlotStyle,{'map','quiver','both'})} = 'map'
        Args.GridPerCrop  (1,1) double {mustBePositive,mustBeInteger} = 15
        Args.GridFull     (1,1) double {mustBePositive,mustBeInteger} = 250
        Args.ArrowScale   (1,1) double = 1
        Args.Plot         logical = true
        Args.Visible      (1,:) char {mustBeMember(Args.Visible,{'on','off'})} = 'on'
    end

    Ncrop = numel(AI);
    Wc = Args.CCDSEC(2) - Args.CCDSEC(1) + 1;   % crop size
    Hc = Args.CCDSEC(4) - Args.CCDSEC(3) + 1;

    CropId=[]; Xc=[]; Yc=[]; C0=[]; Cx=[]; Cy=[]; Cxy=[]; Nstars=[];
    Fp = {}; Pars = {};
    calId=[]; calX=[]; calY=[]; calXF=[]; calYF=[]; calMD=[];   % aper-corr calibrators
    for I = 1:Ncrop
        Cat = AI(I).CatData;
        if isempty(Cat) || isempty(Cat.Catalog); continue; end
        PC = PhotCalibTrans; PC.CCDSEC = Args.CCDSEC; PC.MagColPrefix = Args.MagColPrefix;
        try
            PC = PC.calcAperCorr(Cat, 'Positional', true, ...
                'CalcCorrType', Args.CalcCorrType, 'RefFluxCol', Args.RefFluxCol, ...
                'SNColName', Args.SNColName, 'MinSN', Args.MinSN, ...
                'PosColNameX', Args.ColX, 'PosColNameY', Args.ColY);
        catch
            continue;
        end
        Idx = find(strcmp(PC.AperCorrColNames, Args.ApColName), 1);
        if isempty(Idx) && strcmp(Args.CalcCorrType,'mag')
            Idx = find(strcmp(PC.AperCorrColNames, strrep(Args.ApColName,'FLUX_',Args.MagColPrefix)), 1);
        end
        if isempty(Idx); continue; end
        PF = PC.AperCorrPositional{Idx};
        if ~(isstruct(PF) && isfield(PF,'Par') && numel(PF.Par) >= 4); continue; end
        Par = PF.Par(:).';

        fp = i_cropFootprint(AI(I).HeaderData, Args.KeyOrigSec, I, ...
                             Args.Nrows, Args.Ncols, Args.TileOrder, Wc, Hc);

        CropId(end+1)=I; %#ok<AGROW>
        Xc(end+1)=(fp(1)+fp(2))/2; Yc(end+1)=(fp(3)+fp(4))/2; %#ok<AGROW>
        C0(end+1)=Par(1); Cx(end+1)=Par(2); Cy(end+1)=Par(3); Cxy(end+1)=Par(4); %#ok<AGROW>
        Nstars(end+1)=PC.AperCorrNStars; %#ok<AGROW>
        Fp{end+1}=fp; Pars{end+1}=Par; %#ok<AGROW>

        % The aperture-correction calibrators this crop's fit was built on,
        % mapped to full-frame coordinates via the crop footprint.
        if isfield(PF,'Xfit') && isfield(PF,'Yfit') && isfield(PF,'MagDiff') && ~isempty(PF.Xfit)
            xl=PF.Xfit(:); yl=PF.Yfit(:); md=PF.MagDiff(:);
            calId=[calId; repmat(I,numel(xl),1)];       %#ok<AGROW>
            calX =[calX; xl];  calY =[calY; yl];        %#ok<AGROW>
            calXF=[calXF; fp(1)+(xl-1)]; calYF=[calYF; fp(3)+(yl-1)]; %#ok<AGROW>
            calMD=[calMD; md];                          %#ok<AGROW>
        end
    end
    if isempty(CropId)
        error('pipeline:last:quality:photCalib:mapAperCorrGradients:NoFits', ...
              'No crop produced a position-dependent fit for %s.', Args.ApColName);
    end

    % Radiality metric.
    FrameCenter = [ (min(Xc)+max(Xc))/2, (min(Yc)+max(Yc))/2 ];
    InX = FrameCenter(1)-Xc(:); InY = FrameCenter(2)-Yc(:);
    Gn = hypot(Cx(:),Cy(:)); Rn = hypot(InX,InY);
    InwardCos = (Cx(:).*InX + Cy(:).*InY) ./ max(Gn.*Rn, eps);
    Valid = Gn>0 & Rn>0;
    Radiality = mean(InwardCos(Valid));

    % Assemble the full-frame mosaic (each crop's surface over its footprint).
    aX=[]; aY=[]; aV=[];
    lx = linspace(1, Wc, Args.GridPerCrop); ly = linspace(1, Hc, Args.GridPerCrop);
    [LX,LY] = meshgrid(lx, ly);
    for K = 1:numel(Pars)
        V  = PhotCalibTrans.evalAperPos(Pars{K}, LX(:), LY(:), Args.CCDSEC);
        XF = Fp{K}(1) + (LX(:)-1);   YF = Fp{K}(3) + (LY(:)-1);
        aX=[aX;XF]; aY=[aY;YF]; aV=[aV;V]; %#ok<AGROW>
    end
    gx = linspace(min(aX), max(aX), Args.GridFull);
    gy = linspace(min(aY), max(aY), Args.GridFull);
    [GX,GY] = meshgrid(gx, gy);
    Fi = scatteredInterpolant(aX, aY, aV, 'natural', 'none');
    Map = Fi(GX, GY);

    Report.Table = table(CropId(:), Xc(:), Yc(:), C0(:), Cx(:), Cy(:), Cxy(:), ...
        Nstars(:), InwardCos(:), 'VariableNames', ...
        {'CropId','Xc','Yc','C0','Cx','Cy','Cxy','Nstars','InwardCos'});
    Report.Radiality   = Radiality;
    Report.FrameCenter = FrameCenter;
    Report.ApColName   = Args.ApColName;
    Report.MapXY       = {gx, gy};
    Report.Map         = Map;
    % The aperture-correction calibrators used, across all crops (one row each):
    % CropId, full-frame XFULL/YFULL, native X/Y, and MagDiff for ApColName.
    Report.Calib = table(calId, calXF, calYF, calX, calY, calMD, ...
        'VariableNames', {'CropId','XFULL','YFULL','X','Y','MagDiff'});

    % --- Plot -----------------------------------------------------------
    Fig = [];
    if Args.Plot
        Fig = figure('Visible', Args.Visible); ax = axes(Fig); hold(ax,'on');
        if any(strcmp(Args.PlotStyle, {'map','both'}))
            imagesc(ax, gx, gy, Map, 'AlphaData', ~isnan(Map));
            colorbar(ax);
        end
        if any(strcmp(Args.PlotStyle, {'quiver','both'}))
            S = 0.4*Wc/max(Gn(Valid)) * Args.ArrowScale;
            quiver(ax, Xc(:), Yc(:), Cx(:)*S, Cy(:)*S, 0, 'k', 'LineWidth',1.2, 'MaxHeadSize',0.4);
            text(Xc(:)+0.02*Wc, Yc(:), compose('%d',CropId(:)), 'Parent',ax, 'FontSize',8);
        end
        plot(ax, FrameCenter(1), FrameCenter(2), 'p', 'MarkerSize',16, ...
             'MarkerFaceColor',[1 1 0], 'MarkerEdgeColor','k');
        hold(ax,'off'); axis(ax,'xy'); axis(ax,'image'); box(ax,'on');
        xlabel(ax,'XFULL [pix]'); ylabel(ax,'YFULL [pix]');
        title(ax, sprintf(['%s position-dependent aperture correction [mag], full frame ' ...
            '(radiality = %+.2f)\nradial bowl = physical PSF ;  tiled repeat = crop-local artifact'], ...
            Args.ApColName, Radiality), 'Interpreter','none');
    end
end


% ==== helpers ===========================================================

function fp = i_cropFootprint(Hdr, KeyOrigSec, CropId, Nrows, Ncols, TileOrder, Wc, Hc)
    % Full-frame crop footprint [x1 x2 y1 y2] from ORIGSEC, else a regular grid
    % via cropID2RowCol (row 1 at top -> flipped to y-up).
    fp = [];
    if ~isempty(Hdr) && isa(Hdr,'AstroHeader') && Hdr.isKeyExist(KeyOrigSec)
        v = Hdr.getVal(KeyOrigSec,'ReadCCDSEC',true);
        if numel(v)>=4 && all(isfinite(v(1:4))); fp = v(1:4); end
    end
    if isempty(fp)
        [Row, Col] = PhotCalibTrans.cropID2RowCol(CropId, Nrows, Ncols, TileOrder);
        x1 = (Col-1)*Wc + 1;             y1 = (Nrows-Row)*Hc + 1;
        fp = [x1, x1+Wc-1, y1, y1+Hc-1];
    end
end
