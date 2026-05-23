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
    %            'Verbose'      - Print per-crop pair counts. Default false.
    %
    % Output : - Result - struct with fields:
    %                       .Quantities  cell, copied from input
    %                       .RmsForced   {1,Nq} pooled row vectors over all crops
    %                       .RmsNormal   {1,Nq} pooled row vectors over all crops
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
        Args.MinEpochs     (1,1) double          = 5
        Args.MatchRadius   (1,1) double          = 2
        Args.UniqueNormal  logical               = true
        Args.CropsToAnalyze double               = []
        Args.RAField       {mustBeTextScalar}    = 'RA'
        Args.DecField      {mustBeTextScalar}    = 'Dec'
        Args.ForcedField   {mustBeTextScalar}    = 'FORCED'
        Args.LogScale      logical               = true
        Args.MarkerSize    (1,1) double          = 4
        Args.Verbose       logical               = false
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

        for Iq = 1:Nq
            Q = Args.Quantities{Iq};
            if ~isfield(MSk.Data, Q); continue; end
            Y = MSk.Data.(Q);
            sF = i_stdMinEpochs(Y(:,PairF), Args.MinEpochs);
            sN = i_stdMinEpochs(Y(:,PairN), Args.MinEpochs);
            RmsF{Iq} = [RmsF{Iq}, sF];
            RmsN{Iq} = [RmsN{Iq}, sN];
        end
    end

    % --- Plot ---------------------------------------------------------
    HasData = any(cellfun(@(c) any(isfinite(c)), RmsF)) && ...
              any(cellfun(@(c) any(isfinite(c)), RmsN));
    if HasData
        Fig = figure;
        tiledlayout(1, Nq);
        for Iq = 1:Nq
            nexttile;
            x = RmsN{Iq};  y = RmsF{Iq};
            if Args.LogScale
                g = isfinite(x) & isfinite(y) & x > 0 & y > 0;
                loglog(x(g), y(g), '.', 'MarkerSize', Args.MarkerSize);
            else
                g = isfinite(x) & isfinite(y);
                plot(x(g), y(g), '.', 'MarkerSize', Args.MarkerSize);
            end
            hold on;
            if any(g)
                lims = [min([x(g), y(g)]), max([x(g), y(g)])];
                plot(lims, lims, 'k--');
            end
            grid on;  axis square;
            xlabel(sprintf('rms(%s) — FORCED=0', Args.Quantities{Iq}), 'Interpreter', 'none');
            ylabel(sprintf('rms(%s) — FORCED=1', Args.Quantities{Iq}), 'Interpreter', 'none');
            title(sprintf('N=%d paired sources', nnz(g)));
        end
        sgtitle(sprintf('Forced vs normal RMS  (MatchRadius=%g", MinSN=%g, SNSide=%s, MinEpochs=%d)', ...
                        Args.MatchRadius, Args.MinSN, Args.SNSide, Args.MinEpochs));
    end

    Result.Quantities = Args.Quantities;
    Result.RmsForced  = RmsF;
    Result.RmsNormal  = RmsN;
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
