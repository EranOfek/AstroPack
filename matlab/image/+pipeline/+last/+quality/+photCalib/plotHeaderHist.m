function Result = plotHeaderHist(Data, Args)
    % Histogram of one or more FITS header keywords pooled over many visit dirs.
    % Description: Pulls the requested header keyword(s) via
    %              extractHeaderData, pools the (visit x crop) values into
    %              one (or several overlaid) distribution(s), and plots a
    %              histogram with optional summary statistics.
    %
    %              Three input shapes are accepted:
    %              (1) char/string DataPath: discovers visit subdirectories
    %                  matching VisitPattern under DataPath (recursively by
    %                  default), then loads each via loadVisitCatHdr.
    %              (2) AstroImage array (e.g. one coadd's 1xNcrop AI):
    %                  treated as a single visit, no disk walk.
    %              (3) Cell array of AstroImage arrays: treated as one
    %                  visit per cell element (multi-visit, in-memory).
    %
    %              Use this for any header-only quantity (no calibration
    %              required), e.g. AST_ARMS (asymptotic astrometry RMS),
    %              PH_RMS (relative photometry RMS), PT_ARMS / PT_RMS
    %              (absolute photometry asymptotic / mean RMS), FWHM,
    %              LIMMAG, BACKMAG, MED_X2, APCOR_PS, AST_NSRC, etc.
    %
    %              Counterpart of plotPhotHistogram (which reads from
    %              PhotCalibTrans) and plotPhotCatHist (which reads from
    %              CatData rows). Faster than plotPhotHistogram because no
    %              per-visit calibration step is needed.
    %
    % Input  : - Data, one of:
    %              char/string DataPath (parent dir; visit dirs discovered),
    %              AstroImage array (single visit, in memory),
    %              or cell of AstroImage arrays (multi-visit, in memory).
    %            VisitPattern / Recursive / FileType are silently irrelevant
    %            for in-memory inputs.
    %          * ...,key,val,...
    %            'HeaderKeys'   - Header keyword (char) or cell of names to
    %                             overlay (e.g. {'PT_RMS','PT_ARMS','AST_ARMS','PH_RMS'}).
    %                             Required.
    %            'VisitPattern' - Glob for visit subdirectory names.
    %                             Default '*v[0-9]*'.
    %            'Recursive'    - If true, search for visit dirs at any depth
    %                             under DataPath; otherwise only direct
    %                             children. Default true.
    %            'FileType'     - 'coadd' or 'proc'. Forwarded to
    %                             loadVisitCatHdr. Default 'coadd'.
    %            'Ncrop'        - Number of crops per visit. Default 24.
    %            'CropsToAnalyze' - Crop indices (1..Ncrop) to include from
    %                             each visit. Default [] (all).
    %            'Bins'         - Number of bins or vector of edges. Default 30.
    %            'XLim'         - x-limits [min max]. Default [] (auto).
    %            'LogX'         - Log-transformed x axis. Default false.
    %            'Stats'        - Overlay median + 16/84 percentiles + N as
    %                             text (single-key mode only). Default true.
    %            'Color'        - Bar color for single-key mode. Default
    %                             [0.30 0.55 0.90].
    %            'Verbose'      - Print progress / per-key N. Default false.
    % Output : - Result struct with fields:
    %            .HeaderKeys - 1xK cell of keyword names actually plotted.
    %            .Values     - 1xK cell, each a column vector of pooled values
    %                          (NaNs and (LogX=true) non-positives dropped).
    %            .Stats      - 1xK struct array: .N .Median .Mean .Std .P16
    %                          .P84 .Min .Max  per key.
    %            .HeaderData - struct returned by extractHeaderData (per-key
    %                          [Nvisits x Ncrop] matrices), useful for
    %                          downstream per-crop or per-visit analyses.
    %            .DataPath, .VisitPattern, .Recursive, .FileType - echoed
    %                          (DataPath = '<in-memory>' for AI inputs).
    %            .NVisits    - number of visits processed.
    % Author : D. Kovaleva (Apr 2026; AI input May 2026)
    % Example: % All four RMS-style header keys overlaid (disk walk):
    %          DP = '/archimedes/LASTunitTest/2025/04/26';
    %          R  = pipeline.last.quality.photCalib.plotHeaderHist(DP, ...
    %                  'HeaderKeys', {'PT_RMS','PT_ARMS','AST_ARMS','PH_RMS'});
    %
    %          % In-memory AI from fitPhotCalibTrans / loadVisit (no reload):
    %          [AI, ~] = imProc.calib.fitPhotCalibTrans(Coadd);
    %          R  = pipeline.last.quality.photCalib.plotHeaderHist(AI, ...
    %                  'HeaderKeys', {'PT_DOF','PT_CHI2','AST_ARMS','PH_RMS'});
    %
    %          % Multi-visit in memory (cell of AI arrays):
    %          R  = pipeline.last.quality.photCalib.plotHeaderHist({AI1, AI2}, ...
    %                  'HeaderKeys', 'PH_RMS');
    %
    %          % Whole year, only v0 coadds, log-x FWHM:
    %          R  = pipeline.last.quality.photCalib.plotHeaderHist( ...
    %                  '/archimedes/LASTunitTest/2025', ...
    %                  'VisitPattern', '*v0', 'HeaderKeys', 'FWHM');

    arguments
        Data                                                     % char/string DataPath, AstroImage array, or cell of AI arrays
        Args.HeaderKeys                                          % char or cell
        Args.VisitPattern    {mustBeText}              = '*v[0-9]*'
        Args.Recursive       logical                    = true
        Args.FileType        {mustBeMember(Args.FileType, {'coadd','proc'})} = 'coadd'
        Args.Ncrop                                       = 24
        Args.CropsToAnalyze                              = []
        Args.Bins                                        = 30
        Args.XLim                                        = []
        Args.LogX            logical                    = false
        Args.Stats           logical                    = true
        Args.Color                                       = [0.30 0.55 0.90]
        Args.Verbose         logical                    = false
    end

    % --- Normalize header-key list --------------------------------------
    if ischar(Args.HeaderKeys) || (isstring(Args.HeaderKeys) && isscalar(Args.HeaderKeys))
        Keys = {char(Args.HeaderKeys)};
    elseif iscell(Args.HeaderKeys) || isstring(Args.HeaderKeys)
        Keys = cellfun(@char, cellstr(Args.HeaderKeys), 'UniformOutput', false);
    else
        error('plotHeaderHist:BadHeaderKeys', ...
            'HeaderKeys must be a char, string, or cell of names.');
    end
    Nkeys = numel(Keys);
    FieldNames = cellfun(@matlab.lang.makeValidName, Keys, 'UniformOutput', false);

    % --- Detect input shape and obtain AIc cell -------------------------
    %   AIc is a cell of AstroImage arrays, one per visit (the contract
    %   that extractHeaderData expects).
    IsTextInput = ischar(Data) || (isstring(Data) && isscalar(Data));
    IsAIInput   = isa(Data, 'AstroImage');
    IsCellInput = iscell(Data) && ~isempty(Data) && ...
                  all(cellfun(@(x) isa(x, 'AstroImage'), Data));

    if IsAIInput
        AIc = {Data(:).'};
        Nvisits = 1;
        DataPathEcho = '<in-memory AstroImage>';
    elseif IsCellInput
        AIc = Data(:).';
        Nvisits = numel(AIc);
        DataPathEcho = '<in-memory cell>';
    elseif IsTextInput
        DataPath = char(Data);
        DataPathEcho = DataPath;
        if Args.Recursive
            Listing = dir(fullfile(DataPath, '**', Args.VisitPattern));
        else
            Listing = dir(fullfile(DataPath, Args.VisitPattern));
        end
        Listing = Listing([Listing.isdir] & ~startsWith({Listing.name}, '.'));
        if isempty(Listing)
            ScopeStr = DataPath;
            if Args.Recursive; ScopeStr = [ScopeStr ' (recursive)']; end
            error('plotHeaderHist:NoVisits', ...
                'No subdirectories matching "%s" found under %s', ...
                Args.VisitPattern, ScopeStr);
        end
        Nvisits = numel(Listing);
        VisitDirs = strings(1, Nvisits);
        for I = 1:Nvisits
            VisitDirs(I) = string(fullfile(Listing(I).folder, Listing(I).name));
        end
        AIc = pipeline.last.load.loadVisitCatHdr( ...
            'VisitDirs', VisitDirs, ...
            'FileType',  Args.FileType, ...
            'Verbose',   false);
    else
        error('plotHeaderHist:BadInput', ...
            ['First argument must be a DataPath (char/string), an ', ...
             'AstroImage array, or a cell of AstroImage arrays.']);
    end

    if Args.Verbose
        fprintf('plotHeaderHist: %d visit(s), Keys={%s}\n', ...
            Nvisits, strjoin(Keys, ','));
    end

    HD = pipeline.last.load.extractHeaderData(AIc, ...
        'HeaderKeys', Keys, ...
        'Ncrop',      Args.Ncrop, ...
        'Verbose',    false);

    % --- Pool per-key values across (visit, crop) -----------------------
    if isempty(Args.CropsToAnalyze)
        CropIdx = 1:Args.Ncrop;
    else
        CropIdx = Args.CropsToAnalyze;
        CropIdx = CropIdx(CropIdx >= 1 & CropIdx <= Args.Ncrop);
    end

    Vals = cell(1, Nkeys);
    for Ik = 1:Nkeys
        M = HD.(FieldNames{Ik});                  % [Nvisits x Ncrop]
        Sub = M(:, CropIdx);
        V = Sub(:);
        Keep = isfinite(V);
        if Args.LogX; Keep = Keep & V > 0; end
        Vals{Ik} = V(Keep);
    end

    Empty = cellfun(@isempty, Vals);
    if all(Empty)
        error('plotHeaderHist:Empty', ...
            'No finite values collected for any of: %s', strjoin(Keys, ', '));
    end
    if any(Empty)
        warning('plotHeaderHist:EmptyKeys', ...
            'No finite values for: %s', strjoin(Keys(Empty), ', '));
    end

    % --- Per-key stats --------------------------------------------------
    Stats = repmat(struct('N',0,'Median',NaN,'Mean',NaN,'Std',NaN, ...
                          'P16',NaN,'P84',NaN,'Min',NaN,'Max',NaN), 1, Nkeys);
    for Ik = 1:Nkeys
        V = Vals{Ik};
        Stats(Ik).N = numel(V);
        if ~isempty(V)
            Stats(Ik).Median = median(V);
            Stats(Ik).Mean   = mean(V);
            Stats(Ik).Std    = std(V);
            Stats(Ik).P16    = prctile(V, 16);
            Stats(Ik).P84    = prctile(V, 84);
            Stats(Ik).Min    = min(V);
            Stats(Ik).Max    = max(V);
        end
    end

    if Args.Verbose
        for Ik = 1:Nkeys
            fprintf('  %-12s : N = %d  med = %.4g  [P16 P84] = [%.4g %.4g]\n', ...
                Keys{Ik}, Stats(Ik).N, ...
                Stats(Ik).Median, Stats(Ik).P16, Stats(Ik).P84);
        end
    end

    % --- Plot -----------------------------------------------------------
    figure; hold on;
    Cmap = lines(max(Nkeys, 1));

    AllVals = vertcat(Vals{:});
    if Args.LogX; AllPlot = log10(AllVals); else; AllPlot = AllVals; end
    if isscalar(Args.Bins)
        if isempty(Args.XLim)
            EdgeLo = min(AllPlot); EdgeHi = max(AllPlot);
        else
            EL = Args.XLim;
            if Args.LogX; EL = log10(EL); end
            EdgeLo = EL(1); EdgeHi = EL(2);
        end
        Edges = linspace(EdgeLo, EdgeHi, Args.Bins + 1);
    else
        Edges = Args.Bins;
        if Args.LogX; Edges = log10(Edges); end
    end

    Hbar = gobjects(Nkeys, 1);
    for Ik = 1:Nkeys
        V = Vals{Ik};
        if isempty(V); continue; end
        if Args.LogX; Vplot = log10(V); else; Vplot = V; end
        if Nkeys == 1
            FaceCol = Args.Color; FaceAlpha = 1.0; EdgeCol = 'k';
        else
            FaceCol = Cmap(Ik,:); FaceAlpha = 0.45; EdgeCol = Cmap(Ik,:) * 0.6;
        end
        Hbar(Ik) = histogram(Vplot, Edges, ...
            'FaceColor', FaceCol, 'FaceAlpha', FaceAlpha, ...
            'EdgeColor', EdgeCol, ...
            'DisplayName', sprintf('%s  (N=%d, med=%.4g)', ...
                Keys{Ik}, Stats(Ik).N, Stats(Ik).Median));
    end

    if Nkeys == 1
        XLab = Keys{1};
        if Args.LogX; XLab = sprintf('log10(%s)', XLab); end
    else
        XLab = 'value';
        if Args.LogX; XLab = 'log10(value)'; end
    end
    xlabel(XLab, 'Interpreter', 'none');
    ylabel('Count'); grid on; box on;
    if ~isempty(Args.XLim); xlim(Args.XLim); end

    if Nkeys == 1
        title(sprintf('%s  (N = %d, %d visits)', ...
            Keys{1}, Stats(1).N, Nvisits), 'Interpreter', 'none');
    else
        title(sprintf('Header keys over %d visits', Nvisits), ...
            'Interpreter', 'none');
        legend(Hbar(arrayfun(@(h) isgraphics(h), Hbar)), ...
            'Location', 'best', 'Interpreter', 'none');
    end

    % --- Stats overlay (single-key only) --------------------------------
    if Args.Stats && Nkeys == 1 && Stats(1).N > 0
        S = Stats(1);
        YLim = ylim;
        if Args.LogX
            MedX = log10(S.Median); P16X = log10(S.P16); P84X = log10(S.P84);
        else
            MedX = S.Median;        P16X = S.P16;        P84X = S.P84;
        end
        plot([MedX MedX], YLim, 'k-',  'LineWidth', 1.4);
        plot([P16X P16X], YLim, 'k--', 'LineWidth', 0.8);
        plot([P84X P84X], YLim, 'k--', 'LineWidth', 0.8);
        Txt = sprintf( ...
            'N = %d\nMed = %.4g\n[P16, P84] = [%.4g, %.4g]\nMean = %.4g\nStd = %.4g', ...
            S.N, S.Median, S.P16, S.P84, S.Mean, S.Std);
        XL = xlim;
        text(XL(1) + 0.65*(XL(2)-XL(1)), YLim(1) + 0.92*(YLim(2)-YLim(1)), ...
            Txt, 'VerticalAlignment','top', 'HorizontalAlignment','left', ...
            'BackgroundColor', [1 1 1 0.85], 'EdgeColor','k', 'FontSize', 9, ...
            'Interpreter', 'none');
    end
    hold off;

    % --- Assemble output ------------------------------------------------
    Result.HeaderKeys   = Keys;
    Result.Values       = Vals;
    Result.Stats        = Stats;
    Result.HeaderData   = HD;
    Result.DataPath     = DataPathEcho;
    Result.VisitPattern = Args.VisitPattern;
    Result.Recursive    = Args.Recursive;
    Result.FileType     = Args.FileType;
    Result.NVisits      = Nvisits;
end
