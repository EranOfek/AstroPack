function [Result, Fig] = plotPhotForcedMatch(MS, Args)
    % Paired RMS comparison of FORCED=1 vs FORCED=0 sources via internal
    % MatchedSources cross-match.
    % Description: For each crop, splits sources by the per-source FORCED
    %              flag, applies an SN cut on the FORCED=1 seed side,
    %              cross-matches the two populations by sky position within
    %              MatchRadius, then computes std over epochs for the matched
    %              FORCED=1 / FORCED=0 partners. Plots rms_FORCED=1 vs
    %              rms_FORCED=0 as one panel per Quantity (log-log, y=x
    %              diagonal overlay).
    %
    % Input  : - MS - a MatchedSources array (one element per crop, e.g.
    %            from matchEpochs) or a cell of MatchedSources. Each element
    %            must carry per-source Data.(ForcedField), Data.(RAField),
    %            Data.(DecField) and per-(epoch,source) Data.(Quantities{i}).
    %          * ...,key,val,...
    %            'Quantities'   - Cell of MS.Data field names whose std is
    %                             computed and plotted (one panel each).
    %                             Default {'MAG_APER_3','MAG_PSF'}.
    %            'MinSN'        - Median-SN threshold applied per Args.SNSide.
    %                             0 disables. Silently skipped if MS lacks
    %                             an SN field. Default 5.
    %            'SNSide'       - Where to apply the SN cut: 'forced' (seed
    %                             side only), 'both', or 'none'.
    %                             Default 'forced'.
    %            'MinEpochs'    - Minimum number of finite epochs per source
    %                             to compute its std; sources below this get
    %                             NaN and drop out. Default 5.
    %            'MatchRadius'  - Sky cross-match radius [arcsec]. Default 2.
    %            'UniqueNormal' - If true, drop any FORCED=1 source whose
    %                             nearest FORCED=0 partner is also the
    %                             nearest match for another FORCED=1 source
    %                             (ambiguous many-to-one). Default true.
    %            'CropsToAnalyze' - Crop indices to include. Default [] (all).
    %            'RAField'      - MS.Data field for RA [deg]. Default 'RA'.
    %            'DecField'     - MS.Data field for Dec [deg]. Default 'Dec'.
    %            'ForcedField'  - MS.Data field holding the FORCED flag.
    %                             Default 'FORCED'.
    %            'LogScale'     - Log-log axes. Default true.
    %            'MarkerSize'   - Scatter marker size. Default 4.
    %            'ColorByNepochs' - Shade each dot in a blue gradient
    %                             (light = few epochs, dark = many)
    %                             using the number of finite epochs the
    %                             FORCED=0 partner has in that Quantity.
    %                             Switches the panel from a plain scatter
    %                             to a scatter+colorbar rendering.
    %                             Default true. Mutually exclusive with
    %                             HighlightFlag; HighlightFlag wins when
    %                             both are requested.
    %            'HighlightFlag'- Char name of a FLAGS bit (e.g. 'Overlap',
    %                             'NearEdge'). When non-empty, sources
    %                             whose FORCED=0 partner carries that bit
    %                             in any epoch are drawn in a separate
    %                             colour (red) on top of the unflagged
    %                             population (blue). Silently no-op if
    %                             the MS lacks a FLAGS field or the bit
    %                             name isn't in its BitDictionary.
    %                             Default '' (disabled).
    %            'Verbose'      - Print per-crop pair counts. Default false.
    %
    % Output : - Result - struct with fields:
    %                       .Quantities  cell, copied from input
    %                       .RmsForced   {1,Nq} pooled row vectors over all crops
    %                       .RmsNormal   {1,Nq} pooled row vectors over all crops
    %                       .NepNormal   {1,Nq} pooled # finite epochs on
    %                                    the FORCED=0 side (same length as
    %                                    RmsNormal{Iq})
    %                       .FlagNormal  {1,Nq} pooled logical row vectors:
    %                                    true where the FORCED=0 partner
    %                                    carries the HighlightFlag bit in
    %                                    any epoch (empty when
    %                                    HighlightFlag is '')
    %                       .Npairs      [1,Ncrop] paired sources per crop
    %                       .Args        the args struct actually used
    %          - Fig    - figure handle ([] when there is no data).
    % Author : D. Kovaleva (May 2026)
    % Example:
    %   R = pipeline.last.quality.photCalib.plotPhotForcedMatch(MST);
    %   R = pipeline.last.quality.photCalib.plotPhotForcedMatch(MST, ...
    %           'Quantities', {'MAG_APER_3','MAG_PSF'}, ...
    %           'MinSN', 10, 'MinEpochs', 10, 'MatchRadius', 1);

    arguments
        MS
        Args.Quantities    cell                  = {'MAG_APER_3','MAG_PSF'}
        Args.MinSN         (1,1) double          = 5
        Args.SNSide        {mustBeMember(Args.SNSide,{'forced','both','none'})} = 'forced'
        Args.MinEpochs     (1,1) double          = 20
        Args.MatchRadius   (1,1) double          = 2
        Args.UniqueNormal  logical               = true
        Args.CropsToAnalyze double               = []
        Args.RAField       {mustBeTextScalar}    = 'RA'
        Args.DecField      {mustBeTextScalar}    = 'Dec'
        Args.ForcedField   {mustBeTextScalar}    = 'FORCED'
        Args.LogScale      logical               = true
        Args.MarkerSize    (1,1) double          = 4
        Args.ColorByNepochs logical              = false
        Args.HighlightFlag {mustBeTextScalar}    = ''
        Args.Verbose       logical               = false
    end

    if ~isempty(Args.HighlightFlag) && Args.ColorByNepochs
        % HighlightFlag takes precedence; downgrade ColorByNepochs silently
        % so the two colourings don't fight on the same panel.
        Args.ColorByNepochs = false;
    end

    Result = struct();
    Fig    = [];

    % --- Normalise MS to a cell of MatchedSources (one per crop) ------
    if isa(MS, 'MatchedSources')
        MSc = num2cell(MS(:).');
    elseif iscell(MS)
        MSc = MS(:).';
    else
        error('plotPhotForcedMatch:BadInput', ...
            'MS must be a MatchedSources array or a cell of MatchedSources.');
    end

    Crops = Args.CropsToAnalyze;
    if isempty(Crops); Crops = 1:numel(MSc); end

    Nq    = numel(Args.Quantities);
    RAd   = pi/180;
    Tol   = (Args.MatchRadius/3600) * RAd;

    RmsF   = repmat({zeros(1,0)}, 1, Nq);
    RmsN   = repmat({zeros(1,0)}, 1, Nq);
    NepN   = repmat({zeros(1,0)}, 1, Nq);
    OvlN   = repmat({false(1,0)}, 1, Nq);
    Npairs = zeros(1, numel(MSc));

    for Ic = Crops
        if Ic < 1 || Ic > numel(MSc); continue; end
        MSk = MSc{Ic};
        if isempty(MSk) || ~isfield(MSk.Data, Args.ForcedField) || ...
           ~isfield(MSk.Data, Args.RAField) || ~isfield(MSk.Data, Args.DecField)
            continue
        end

        F      = MSk.Data.(Args.ForcedField);
        RAsrc  = median(MSk.Data.(Args.RAField),  1, 'omitnan');
        Decsrc = median(MSk.Data.(Args.DecField), 1, 'omitnan');

        % Per-source FORCED — first finite value down each column
        Fsrc = nan(1, MSk.Nsrc);
        for Is = 1:MSk.Nsrc
            v = F(:,Is);  v = v(~isnan(v));
            if ~isempty(v);  Fsrc(Is) = v(1);  end
        end

        IsForced = (Fsrc == 1);
        IsNormal = (Fsrc == 0);

        % SN cut (median over epochs)
        if ~strcmpi(Args.SNSide, 'none') && Args.MinSN > 0 && isfield(MSk.Data, 'SN')
            MedSN = median(MSk.Data.SN, 1, 'omitnan');
            switch lower(Args.SNSide)
                case 'forced'
                    IsForced = IsForced & (MedSN > Args.MinSN);
                case 'both'
                    IsForced = IsForced & (MedSN > Args.MinSN);
                    IsNormal = IsNormal & (MedSN > Args.MinSN);
            end
        end

        IdxF = find(IsForced);
        IdxN = find(IsNormal);
        if isempty(IdxF) || isempty(IdxN); continue; end

        % Pairwise sky distances [nF x nN], radians
        DM = celestial.coo.sphere_dist_fast( ...
                (RAsrc(IdxF)*RAd).', (Decsrc(IdxF)*RAd).', ...
                 RAsrc(IdxN)*RAd,     Decsrc(IdxN)*RAd );
        [Dmin, IdxBest] = min(DM, [], 2);
        IsMatch = Dmin(:) <= Tol;

        % Drop ambiguous many-to-one matches (two forced -> same normal)
        if Args.UniqueNormal && any(IsMatch)
            MatchedRows = find(IsMatch);
            [~,~,Grp]   = unique(IdxBest(MatchedRows));
            Counts      = accumarray(Grp, 1);
            BadRows     = MatchedRows(ismember(Grp, find(Counts > 1)));
            IsMatch(BadRows) = false;
        end

        PairF = IdxF(IsMatch);
        PairN = IdxN(IdxBest(IsMatch));
        Npairs(Ic) = numel(PairF);

        if Args.Verbose
            fprintf('  Crop %02d: nF=%d, nN=%d, matched=%d\n', ...
                    Ic, numel(IdxF), numel(IdxN), Npairs(Ic));
        end

        % Per-source flag status (any epoch): used only when HighlightFlag
        % is non-empty. Computed once per crop (it doesn't depend on Q).
        HasFlag = false(1, MSk.Nsrc);
        if ~isempty(Args.HighlightFlag) && isfield(MSk.Data, 'FLAGS')
            try
                BadFlag = MSk.searchFlags('FlagsList', {Args.HighlightFlag});
                HasFlag = any(BadFlag, 1);
            catch
                % Leave HasFlag = false if the bit name is unknown
            end
        end

        for Iq = 1:Nq
            Q = Args.Quantities{Iq};
            if ~isfield(MSk.Data, Q); continue; end
            Y = MSk.Data.(Q);
            sF = i_stdMinEpochs(Y(:,PairF), Args.MinEpochs);
            sN = i_stdMinEpochs(Y(:,PairN), Args.MinEpochs);
            nN = sum(isfinite(Y(:,PairN)), 1);   % # finite epochs, FORCED=0 side
            flN = logical(HasFlag(PairN));       % 1 x numel(PairN)
            RmsF{Iq} = [RmsF{Iq}, sF];
            RmsN{Iq} = [RmsN{Iq}, sN];
            NepN{Iq} = [NepN{Iq}, nN];
            OvlN{Iq} = [OvlN{Iq}, flN];
        end
    end

    % --- Plot ---------------------------------------------------------
    HasData = any(cellfun(@(c) any(isfinite(c)), RmsF)) && ...
              any(cellfun(@(c) any(isfinite(c)), RmsN));
    if HasData
        Fig = figure;
        tiledlayout(1, Nq);
        for Iq = 1:Nq
            Ax = nexttile;
            x = RmsN{Iq};  y = RmsF{Iq};
            if Args.LogScale
                g = isfinite(x) & isfinite(y) & x > 0 & y > 0;
            else
                g = isfinite(x) & isfinite(y);
            end
            if ~isempty(Args.HighlightFlag) && Iq <= numel(OvlN) && ~isempty(OvlN{Iq})
                Flagged = OvlN{Iq} & g;
                Plain   = ~OvlN{Iq} & g;
                Cblue   = [0.20 0.40 0.80];
                Cred    = [0.85 0.20 0.10];
                plot(Ax, x(Plain), y(Plain), '.', 'MarkerSize', Args.MarkerSize, ...
                    'Color', Cblue, 'DisplayName', sprintf('no %s', Args.HighlightFlag));
                hold(Ax, 'on');
                plot(Ax, x(Flagged), y(Flagged), '.', 'MarkerSize', Args.MarkerSize, ...
                    'Color', Cred,  'DisplayName', Args.HighlightFlag);
                legend(Ax, 'Location', 'best', 'Interpreter', 'none');
            elseif Args.ColorByNepochs && Iq <= numel(NepN) && ~isempty(NepN{Iq})
                ScSize = max(20, Args.MarkerSize.^2 * 4);
                c      = NepN{Iq};
                colormap(Ax, i_blueRamp(256));
                scatter(Ax, x(g), y(g), ScSize, c(g), 'filled');
                cb = colorbar(Ax);
                cb.Label.String = '# epochs (FORCED=0)';
            else
                plot(Ax, x(g), y(g), '.', 'MarkerSize', Args.MarkerSize);
            end
            hold(Ax, 'on');
            if any(g)
                lims = [min([x(g), y(g)]), max([x(g), y(g)])];
                plot(Ax, lims, lims, 'k--', 'HandleVisibility', 'off');
            end
            if Args.LogScale
                set(Ax, 'XScale', 'log', 'YScale', 'log');
            end
            grid(Ax, 'on');  axis(Ax, 'square');
            xlabel(Ax, sprintf('rms(%s) — FORCED=0', Args.Quantities{Iq}), 'Interpreter', 'none');
            ylabel(Ax, sprintf('rms(%s) — FORCED=1', Args.Quantities{Iq}), 'Interpreter', 'none');
            if ~isempty(Args.HighlightFlag) && Iq <= numel(OvlN) && ~isempty(OvlN{Iq})
                title(Ax, sprintf('N=%d paired (%d %s flagged)', ...
                    nnz(g), nnz(OvlN{Iq} & g), Args.HighlightFlag), ...
                    'Interpreter', 'none');
            else
                title(Ax, sprintf('N=%d paired sources', nnz(g)));
            end
        end
        sgtitle(sprintf('Forced vs normal RMS  (MatchRadius=%g", MinSN=%g, SNSide=%s, MinEpochs=%d)', ...
                        Args.MatchRadius, Args.MinSN, Args.SNSide, Args.MinEpochs));
    end

    Result.Quantities = Args.Quantities;
    Result.RmsForced  = RmsF;
    Result.RmsNormal  = RmsN;
    Result.NepNormal  = NepN;
    Result.FlagNormal = OvlN;
    Result.Npairs     = Npairs;
    Result.Args       = Args;
end

% -------------------------------------------------------------------------
function s = i_stdMinEpochs(Y, MinEp)
    % std along dim 1 with NaN-omission; NaN where finite count < MinEp.
    if isempty(Y)
        s = zeros(1, 0);
        return;
    end
    N = sum(isfinite(Y), 1);
    s = std(Y, 0, 1, 'omitnan');
    s(N < MinEp) = NaN;
end

% -------------------------------------------------------------------------
function C = i_blueRamp(N)
    % N-step colormap from pale blue (low) to deep navy (high).
    if nargin < 1 || isempty(N); N = 256; end
    C = [linspace(0.85, 0.00, N).', ...   % R
         linspace(0.92, 0.10, N).', ...   % G
         linspace(1.00, 0.45, N).'];      % B
end
