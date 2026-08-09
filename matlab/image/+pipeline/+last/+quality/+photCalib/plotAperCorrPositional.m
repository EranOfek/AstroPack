function [Report, Fig] = plotAperCorrPositional(PC, Args)
    % Compare position-dependent aperture corrections to the scalar median.
    % Description: For each aperture, evaluates the fitted position-dependent
    %   correction surface AperCorr_i(X,Y) over the image CCDSEC and compares
    %   it to the legacy scalar (median) AperCorr_i. It answers two things:
    %     (1) how the positional values correspond to the old median - the
    %         field-average and centre value should sit close to the scalar,
    %         and the offset PosMean-Scalar quantifies any bias; and
    %     (2) how large / what shape the position dependence is - the surface
    %         map and the amplitude (max-min over the field) show it directly.
    %
    %   The positional (and scalar) corrections can come from EITHER a
    %   pre-fitted PhotCalibTrans / header (APC0_* keywords), OR be fit on the
    %   fly from a supplied catalog via PhotCalibTrans.calcAperCorr - so you can
    %   point it at an ordinary calibrated image+catalog that carries only the
    %   scalar APCOR_* and still see the position dependence. By default the
    %   on-the-fly fit uses FLUX_* columns ('flux' mode), which are pristine
    %   (never aperture-corrected), so it recovers the true correction even on
    %   an image whose magnitudes were already aperture-corrected.
    %
    % Input  : - PC, one of:
    %            * a PhotCalibTrans object (AperCorr[+Positional]+CCDSEC), or
    %            * an AstroHeader / AstroImage / AstroCatalog carrying the
    %              APCOR_* (and optionally APC0_*) keywords + CCDSEC.
    %          * ...,key,val,...
    %            'Cat'        - AstroCatalog to fit from and/or overlay
    %                           (needs FLUX_*/MAG_* + ColX/ColY + SN). When set,
    %                           the correction is (re)fit from it per 'Refit'.
    %                           Default [] .
    %            'Refit'      - 'auto' (default): fit from Cat only if the PC has
    %                           no positional fit; 'always': always fit from Cat;
    %                           'never': never fit (use the PC's stored fit).
    %            'CalcCorrType'-'flux' (default) or 'mag' for the on-the-fly fit.
    %            'MagColPrefix'-Mag prefix for 'mag' mode. Default '' -> the PC's.
    %            'CCDSEC'     - [Xmin Xmax Ymin Ymax] to normalize/evaluate over.
    %                           Default [] resolves in order: PC.CCDSEC -> header
    %                           CCDSEC keyword -> [1 NAXIS1 1 NAXIS2] from an
    %                           image header -> image size -> [1 1726 1 1726]
    %                           (LAST per-crop, with a warning). Pass explicitly
    %                           to override.
    %            'GridN'      - Surface grid resolution per axis. Default 60.
    %            'ColX'/'ColY'- Position columns. Default 'X'/'Y'.
    %            'RefFluxCol' - Reference flux column. Default 'FLUX_APER_3'.
    %            'SNColName'  - S/N column. Default 'SN'.
    %            'MinSN'      - Min S/N. Default 30.
    %            'Apertures'  - Cell of aperture column names to include.
    %                           Default {} (all with a positional fit).
    %            'WriteHeader'- Write the (fitted) aperture-correction keywords
    %                           back to the FIRST input's header when it is an
    %                           AstroImage / AstroHeader, via
    %                           PhotCalibTrans.aperCorrToHeader (only APCOR_*/
    %                           APC0_* keys are touched; the header is a handle,
    %                           so the input is updated in place). Default false.
    %            'Overlay'    - Scatter the calibrator points on each surface.
    %                           Default true; set false to show the fitted
    %                           surface only (the catalog is still used for the
    %                           fit).
    %            'OverlayColorBy' - 'diff' (default) colours each calibrator by
    %                           its MagDiff (same scale as the surface, so it
    %                           blends in where the fit is good); 'flat' draws
    %                           the calibrators as plain position markers in a
    %                           single colour (OverlayColor), so you see WHERE
    %                           they are regardless of fit quality.
    %            'OverlayColor' - RGB for 'flat' markers. Default [0 0 0].
    %            'OverlayMarkerSize' - Marker area. Default 12.
    %            'Plot'       - Draw the figure. Default true.
    %            'Visible'    - 'on'|'off'. Default 'on'.
    % Output : - Report struct with:
    %            .Table  - one row per aperture: ColName, Scalar, C0, Cx, Cy,
    %                      Cxy, PosMean, PosMin, PosMax, Amplitude,
    %                      OffsetFromScalar (=PosMean-Scalar), Nstars [mag].
    %            .CCDSEC, .Fitted (true if fit on the fly from Cat),
    %            .WroteHeader (true if keywords were written to the header).
    %          - Fig    - figure handle ([] when Plot=false).
    % Author : D. Kovaleva (Aug 2026)
    % See also: PhotCalibTrans.calcAperCorr, PhotCalibTrans.applyAperCorr,
    %           PhotCalibTrans.evalAperPos, imProc.calib.fitPhotCalibTrans.
    % Example:
    %   % Fit from an ordinary calibrated image + its catalog, then compare:
    %   pipeline.last.quality.photCalib.plotAperCorrPositional(AItest, 'Cat', AItest.CatData);
    %   % Or show a PC that already carries the positional fit:
    %   [Rep,~] = pipeline.last.quality.photCalib.plotAperCorrPositional(PC);
    arguments
        PC
        Args.Cat          = []
        Args.Refit        (1,:) char {mustBeMember(Args.Refit,{'auto','always','never'})} = 'auto'
        Args.CalcCorrType (1,:) char {mustBeMember(Args.CalcCorrType,{'flux','mag'})} = 'flux'
        Args.MagColPrefix (1,:) char = ''
        Args.CCDSEC       double = []
        Args.GridN        (1,1) double {mustBePositive, mustBeInteger} = 60
        Args.ColX         (1,:) char = 'X'
        Args.ColY         (1,:) char = 'Y'
        Args.RefFluxCol   (1,:) char = 'FLUX_APER_3'
        Args.SNColName    (1,:) char = 'SN'
        Args.MinSN        (1,1) double = 30
        Args.Apertures    cell = {}
        Args.WriteHeader  logical = false
        Args.Overlay      logical = true
        Args.OverlayColorBy (1,:) char {mustBeMember(Args.OverlayColorBy,{'diff','flat'})} = 'diff'
        Args.OverlayColor   (1,3) double = [0 0 0]
        Args.OverlayMarkerSize (1,1) double = 12
        Args.Plot         logical = true
        Args.Visible      (1,:) char {mustBeMember(Args.Visible,{'on','off'})} = 'on'
    end

    % --- Resolve a base PhotCalibTrans + header -------------------------
    PCobj = []; Hdr = [];
    if isa(PC, 'PhotCalibTrans')
        PCobj = PC;
    else
        Hdr = i_getHeader(PC);
        if ~isempty(Hdr)
            PCobj = PhotCalibTrans().photCalibTransFromHeader(Hdr);
        end
    end

    % --- CCDSEC ---------------------------------------------------------
    % Explicit arg -> PC.CCDSEC -> header CCDSEC keyword -> derived from the
    % crop size (a per-crop CCDSEC is [1 NAXIS1 1 NAXIS2]), since a catalog
    % header (e.g. from loadVisitCatHdr) may not carry the CCDSEC keyword.
    CCDSEC = Args.CCDSEC;
    if isempty(CCDSEC) && ~isempty(PCobj); CCDSEC = PCobj.CCDSEC; end
    if isempty(CCDSEC) && ~isempty(Hdr) && Hdr.isKeyExist('CCDSEC')
        v = Hdr.getVal('CCDSEC','ReadCCDSEC',true);
        if numel(v) >= 4 && all(isfinite(v(1:4))); CCDSEC = v(1:4); end
    end
    if isempty(CCDSEC) && ~isempty(Hdr) && ~Hdr.isKeyExist('TFIELDS')
        % NAXIS1/NAXIS2 are the image dimensions ONLY on an image header;
        % on a binary-table (catalog) header they are the table byte-width /
        % row count - TFIELDS marks that case, so skip it there.
        nx = Hdr.getVal('NAXIS1'); ny = Hdr.getVal('NAXIS2');
        if isnumeric(nx) && isnumeric(ny) && isscalar(nx) && isscalar(ny) && ...
                isfinite(nx) && isfinite(ny) && nx > 0 && ny > 0
            CCDSEC = [1 nx 1 ny];
        end
    end
    if isempty(CCDSEC) && isa(PC,'AstroImage') && ~isempty(PC.Image)
        sz = size(PC.Image); CCDSEC = [1 sz(2) 1 sz(1)];
    end
    if isempty(CCDSEC) || numel(CCDSEC) < 4
        % Last resort: assume the LAST per-crop size. A ~10-pixel error vs the
        % true size is negligible for the [-1,1] normalization; pass 'CCDSEC'
        % (or provide a header with CCDSEC/NAXIS) for an exact section.
        CCDSEC = [1 1726 1 1726];
        warning('pipeline:last:quality:photCalib:plotAperCorrPositional:AssumedCCDSEC', ...
                ['CCDSEC/NAXIS not found in the header; assuming [1 1726 1 1726] ' ...
                 '(LAST per-crop). Pass ''CCDSEC'',[Xmin Xmax Ymin Ymax] to override.']);
    end

    % --- Fit on the fly from the catalog, if asked / needed -------------
    HaveCat = ~isempty(Args.Cat) && isa(Args.Cat, 'AstroCatalog');
    HasPos  = ~isempty(PCobj) && i_hasPositional(PCobj);
    DoFit   = HaveCat && (strcmp(Args.Refit,'always') || ...
                          (strcmp(Args.Refit,'auto') && ~HasPos));
    Fitted = false;
    if DoFit
        Prefix = Args.MagColPrefix;
        if isempty(Prefix)
            if ~isempty(PCobj); Prefix = PCobj.MagColPrefix; else; Prefix = 'MAG_AB_'; end
        end
        PCfit = PhotCalibTrans();
        PCfit.MagColPrefix = Prefix;
        PCfit.CCDSEC = CCDSEC;
        PCfit = PCfit.calcAperCorr(Args.Cat, 'Positional', true, ...
            'CalcCorrType', Args.CalcCorrType, 'RefFluxCol', Args.RefFluxCol, ...
            'SNColName', Args.SNColName, 'MinSN', Args.MinSN, ...
            'PosColNameX', Args.ColX, 'PosColNameY', Args.ColY);
        PCobj = PCfit;
        Fitted = true;
    end

    if isempty(PCobj) || isempty(PCobj.AperCorr) || isempty(PCobj.AperCorrColNames)
        error('pipeline:last:quality:photCalib:plotAperCorrPositional:NoAperCorr', ...
              'No aperture corrections available. Pass a calibrated ''Cat'' to fit them.');
    end

    % --- Select apertures with a (bilinear) positional fit --------------
    Sel = [];
    for I = 1:numel(PCobj.AperCorrColNames)
        if ~isempty(Args.Apertures) && ~ismember(PCobj.AperCorrColNames{I}, Args.Apertures)
            continue;
        end
        PF = PCobj.AperCorrPositional{I};
        if isstruct(PF) && isfield(PF,'Par') && numel(PF.Par) >= 4
            Sel(end+1) = I; %#ok<AGROW>
        end
    end
    if isempty(Sel)
        error('pipeline:last:quality:photCalib:plotAperCorrPositional:NoPositional', ...
              ['No aperture has a position-dependent fit. Pass a calibrated ''Cat'' ' ...
               'to fit it on the fly (the header of %s carries only the scalar APCOR_*).'], class(PC));
    end

    % --- Grid + optional overlay data -----------------------------------
    xv = linspace(CCDSEC(1), CCDSEC(2), Args.GridN);
    yv = linspace(CCDSEC(3), CCDSEC(4), Args.GridN);
    [XG, YG] = meshgrid(xv, yv);
    if HaveCat
        Vn = Args.Cat.Table.Properties.VariableNames;
        MaskCat = true(size(Args.Cat.Catalog,1),1);
        if ismember(Args.SNColName, Vn); MaskCat = Args.Cat.getCol(Args.SNColName) > Args.MinSN; end
        Xcat = []; Ycat = [];
        if ismember(Args.ColX, Vn); Xcat = Args.Cat.getCol(Args.ColX); end
        if ismember(Args.ColY, Vn); Ycat = Args.Cat.getCol(Args.ColY); end
    end

    % --- Per-aperture report + surfaces ---------------------------------
    N = numel(Sel);
    ColName=cell(N,1); Scalar=nan(N,1); C0=nan(N,1); Cx=nan(N,1); Cy=nan(N,1);
    Cxy=nan(N,1); PosMean=nan(N,1); PosMin=nan(N,1); PosMax=nan(N,1);
    Amplitude=nan(N,1); OffsetFromScalar=nan(N,1); Nstars=nan(N,1);
    Surf=cell(N,1);
    for K = 1:N
        I = Sel(K);
        Par = PCobj.AperCorrPositional{I}.Par(:).';
        S = reshape(PhotCalibTrans.evalAperPos(Par, XG(:), YG(:), CCDSEC), size(XG));
        Surf{K} = S;
        ColName{K}=PCobj.AperCorrColNames{I};
        Scalar(K)=PCobj.AperCorr(I);
        C0(K)=Par(1); Cx(K)=Par(2); Cy(K)=Par(3); Cxy(K)=Par(4);
        PosMean(K)=mean(S(:)); PosMin(K)=min(S(:)); PosMax(K)=max(S(:));
        Amplitude(K)=PosMax(K)-PosMin(K); OffsetFromScalar(K)=PosMean(K)-Scalar(K);
        Nstars(K)=PCobj.AperCorrNStars;
    end
    Report.Table = table(ColName,Scalar,C0,Cx,Cy,Cxy,PosMean,PosMin,PosMax, ...
                         Amplitude,OffsetFromScalar,Nstars);
    Report.CCDSEC = CCDSEC;
    Report.Fitted = Fitted;

    % --- Optionally persist the (re)fit to the input image header -------
    % Writes ONLY the aperture-correction keywords (aperCorrToHeader), so an
    % otherwise-calibrated header is left intact. The header is a handle, so
    % this mutates the AstroImage/AstroHeader passed as the first argument.
    Report.WroteHeader = false;
    if Args.WriteHeader
        if ~isempty(Hdr) && isa(Hdr, 'AstroHeader')
            % Ensure CCDSEC is present so the coefs can be normalized on
            % read-back (photCalibTransFromHeader reads CCDSEC). Harmless if
            % already there; a per-crop section is [1 NAXIS1 1 NAXIS2].
            if ~Hdr.isKeyExist('CCDSEC')
                Hdr.replaceVal('CCDSEC', imUtil.ccdsec.ccdsec2str(CCDSEC));
            end
            PCobj.aperCorrToHeader(Hdr);   % AstroHeader is a handle: mutates in place
            Report.WroteHeader = true;
        else
            warning('pipeline:last:quality:photCalib:plotAperCorrPositional:NoHeaderToWrite', ...
                ['WriteHeader requested but the first input carries no writable header ' ...
                 '- pass an AstroImage or AstroHeader (not a bare catalog / PhotCalibTrans).']);
        end
    end

    % --- Plot -----------------------------------------------------------
    Fig = [];
    if Args.Plot
        Fig = figure('Visible', Args.Visible);
        Ncol = ceil(sqrt(N)); Nrow = ceil(N/Ncol);
        for K = 1:N
            ax = subplot(Nrow, Ncol, K, 'Parent', Fig);
            imagesc(ax, xv, yv, Surf{K}); axis(ax,'xy'); axis(ax,'image');
            colorbar(ax); hold(ax,'on');
            if Args.Overlay && HaveCat && ~isempty(Xcat) && ~isempty(Ycat)
                good = MaskCat & isfinite(Xcat) & isfinite(Ycat);
                if strcmp(Args.OverlayColorBy, 'diff')
                    % Colour each calibrator by its MagDiff (matches the surface
                    % scale; blends in where the fit is good).
                    dS = i_srcDiff(Args.Cat, ColName{K}, Args.RefFluxCol, PCobj.MagColPrefix);
                    if ~isempty(dS)
                        good = good & isfinite(dS);
                        if any(good)
                            scatter(ax, Xcat(good), Ycat(good), Args.OverlayMarkerSize, dS(good), ...
                                'filled', 'MarkerEdgeColor',[0 0 0], 'LineWidth',0.25);
                        end
                    end
                elseif any(good)
                    % 'flat' - positions only, single colour (white halo so the
                    % markers stay visible on top of the coloured surface).
                    scatter(ax, Xcat(good), Ycat(good), Args.OverlayMarkerSize, Args.OverlayColor, ...
                        'filled', 'MarkerEdgeColor',[1 1 1], 'LineWidth',0.25);
                end
            end
            hold(ax,'off'); xlabel(ax,'X [pix]'); ylabel(ax,'Y [pix]');
            title(ax, sprintf('%s\nmedian=%+.4f  amp=%.4f  off=%+.4f', ...
                ColName{K}, Scalar(K), Amplitude(K), OffsetFromScalar(K)), 'Interpreter','none');
        end
        Tag = 'from header/PC'; if Fitted; Tag = sprintf('fit on the fly (%s)', Args.CalcCorrType); end
        sgtitle(Fig, sprintf('Position-dependent aperture correction [mag] vs scalar median  (%s)', Tag), ...
                'Interpreter','none');
    end
end


% ==== helpers ===========================================================

function tf = i_hasPositional(PCobj)
    tf = false;
    if ~iscell(PCobj.AperCorrPositional) || isempty(PCobj.AperCorr); return; end
    if numel(PCobj.AperCorrPositional) ~= numel(PCobj.AperCorr); return; end
    for I = 1:numel(PCobj.AperCorrPositional)
        PF = PCobj.AperCorrPositional{I};
        if isstruct(PF) && isfield(PF,'Par') && numel(PF.Par) >= 4; tf = true; return; end
    end
end

function Hdr = i_getHeader(In)
    Hdr = [];
    if isa(In, 'AstroHeader')
        Hdr = In;
    elseif isa(In, 'AstroImage')
        Hdr = In.HeaderData;
    elseif isa(In, 'AstroCatalog')
        try
            Hdr = In.HeaderData;
        catch
            Hdr = [];
        end
        if isempty(Hdr) || ~isa(Hdr, 'AstroHeader'); Hdr = []; end
    end
end

function d = i_srcDiff(Cat, AperCol, RefFluxCol, MagPrefix)
    % Per-source correction diff, matching the aperture column's mode:
    %   FLUX_* column -> 2.5*log10(FluxAper/FluxRef); MAG_* column -> MagRef-MagAper.
    d = []; Vn = Cat.Table.Properties.VariableNames;
    if startsWith(AperCol, 'FLUX_')
        if ~ismember(AperCol, Vn) || ~ismember(RefFluxCol, Vn); return; end
        R = Cat.getCol(AperCol) ./ Cat.getCol(RefFluxCol);
        R(R <= 0 | ~isfinite(R)) = NaN;
        d = 2.5 * log10(R);
    else
        RefMagCol = strrep(RefFluxCol, 'FLUX_', MagPrefix);
        if ~ismember(RefMagCol, Vn) || ~ismember(AperCol, Vn); return; end
        d = Cat.getCol(RefMagCol) - Cat.getCol(AperCol);
    end
end
