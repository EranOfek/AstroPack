function Result = overlapPairStability(MS, Args)
    % Per-source stability across epochs for overlap sources shared by two
    % crops, in three scenarios: crop-A only, crop-B only, and random per-
    % epoch pick from {A, B}. Compare distributions and per-mag trends to
    % detect inter-crop calibration drift.
    % Description: For each sky-matched source pair in the overlap zone
    %              between two per-crop MatchedSources (from a per-crop
    %              stabilityN3 loop), builds three [Nepoch x Npairs] mag
    %              matrices:
    %                MA   = MSA.Data.(MagCol)(:, iA)
    %                MB   = MSB.Data.(MagCol)(:, iB)
    %                MMIX = Bernoulli per-(epoch, pair) pick from {MA, MB}
    %              and reduces each to per-pair robust or plain STD using
    %              the shared MinEpochs floor. The Bernoulli mixing is
    %              bootstrapped (NRandomTrials realizations) so RMS_mix is
    %              reported as mean +- std across trials.
    %              A well-calibrated pair should give RMS_A ~ RMS_B ~
    %              RMS_mix ~ sqrt((RMS_A^2 + RMS_B^2)/2). Excess of RMS_mix
    %              over that theoretical baseline is differential ZP drift
    %              between the two crops across epochs.
    % Input  : - MS - MatchedSources array (or cell) with at least CropA
    %                 and CropB indices populated. Each element must carry
    %                 Data.MAG (or Args.MagCol), Data.RA, Data.Dec, and
    %                 optionally Data.FLAGS / Data.SN for the reject mask.
    %                 Same input shape as plotPhotStabilityMap /
    %                 plotPhotStabilityXY.
    %          * ...,key,val,...
    %            'CropA'        - Index into the MS array/cell for the first
    %                             crop. Required.
    %            'CropB'        - Second crop index. Required.
    %            'MagCol'       - Magnitude column. Default 'MAG_APER_3'.
    %            'MatchRadius'  - Cross-crop sky match radius [arcsec].
    %                             Default 1.
    %            'MagRange'     - [MagMin MagMax] filter on the per-pair
    %                             median mag (across all epochs, taken on
    %                             MA; MB is expected to be within 1 sigma
    %                             of MA). Empty = no filter. Default [].
    %            'StdMethod'    - 'robust' (1.4826 * MAD) | 'plain' (sample
    %                             std). Default 'robust'.
    %            'MinEpochs'    - Drop pairs with <= this many finite mag
    %                             epochs in EITHER A or B. Default 10.
    %            'BadFlags'     - FLAGS bit names to mask per (epoch, source)
    %                             before per-source std. Default
    %                             {'Saturated','NearEdge'}. {} to disable.
    %            'SNmin'        - Mask (epoch, source) with SN < SNmin.
    %                             0 disables. Default 10.
    %            'NRandomTrials'- Bootstrap trials for the RMS_mix estimate.
    %                             Default 20.
    %            'RandomSeed'   - RNG seed for reproducibility. Default 0.
    %                             [] leaves RNG state untouched.
    %            'Plot'         - Draw the 3-panel figure. Default true.
    %            'OutFile'      - Save Result to this .mat path. Default ''.
    %            'Verbose'      - Print pair count and pool stats. Default true.
    % Output : - Result struct with:
    %            .CropA, .CropB       - crop indices used
    %            .NPairs              - number of matched pairs surviving
    %                                   MatchRadius + MinEpochs
    %            .PairsRA, .PairsDec  - per-pair median (RA, Dec) [deg]
    %            .MedMag              - per-pair median mag (from A)
    %            .RMS_A               - per-pair STD across epochs, A only
    %            .RMS_B               - per-pair STD across epochs, B only
    %            .RMS_mix             - per-pair mean-over-trials mixed STD
    %            .RMS_mix_sigma       - per-pair STD across trials (bootstrap uncertainty)
    %            .RMS_base_theory     - sqrt((RMS_A^2 + RMS_B^2)/2)
    %            .Excess              - RMS_mix - RMS_base_theory
    %            .Args                - resolved Args (for reproducibility)
    % Author : D. Kovaleva (Aug 2026)
    % See also: stabilityN3, plotPhotStabilityMap, plotPhotStabilityXY,
    %           pipeline.last.quality.overlapSources (single-epoch pair
    %           diff analysis).
    % Example:
    %   % Build per-crop MS via stabilityN3 loop (existing workflow).
    %   MSarr = cell(1, 24);
    %   for k = 1:24
    %       Pat = sprintf('LAST*_1679.c_*_%03d_sci_coadd_Cat_1.fits', k);
    %       MSarr{k} = pipeline.last.quality.photCalib.stabilityN3( ...
    %                    'DataPath', BaseDir, 'Pattern', Pat);
    %   end
    %
    %   % Stability comparison for the interface between crops 10 and 11.
    %   R = pipeline.last.quality.photCalib.overlapPairStability(MSarr, ...
    %           'CropA', 10, 'CropB', 11);
    %
    %   % Sweep every LAST interface with a caller loop; keep the excess:
    %   Pairs = pipeline.last.quality.LASToverlaps('CroppingScheme', 'old');
    %   Excess = cell(1, size(Pairs, 1));
    %   for k = 1:size(Pairs, 1)
    %       R = pipeline.last.quality.photCalib.overlapPairStability(MSarr, ...
    %               'CropA', Pairs(k,1), 'CropB', Pairs(k,2), 'Plot', false);
    %       Excess{k} = R.Excess;
    %   end

    arguments
        MS
        Args.CropA         (1,1) double
        Args.CropB         (1,1) double
        Args.MagCol        (1,:) char   = 'MAG_APER_3'
        Args.MatchRadius   (1,1) double = 1
        Args.MagRange                    = []
        Args.StdMethod     (1,:) char {mustBeMember(Args.StdMethod,{'robust','plain'})} = 'robust'
        Args.MinEpochs     (1,1) double = 10
        Args.BadFlags           cell    = {'Saturated','NearEdge'}
        Args.SNmin         (1,1) double = 10
        Args.NRandomTrials (1,1) double = 20
        Args.RandomSeed                 = 0
        Args.Plot          (1,1) logical = true
        Args.OutFile       (1,:) char   = ''
        Args.Verbose       (1,1) logical = true
    end

    % --- Resolve the two MS elements from any input shape ----------------
    MScell = i_flattenMSToCell(MS);
    if Args.CropA < 1 || Args.CropA > numel(MScell) || ...
       Args.CropB < 1 || Args.CropB > numel(MScell)
        error('pipeline:last:quality:photCalib:overlapPairStability:BadCrop', ...
            'CropA=%d, CropB=%d must lie in 1..%d (numel(MS))', ...
            Args.CropA, Args.CropB, numel(MScell));
    end
    if Args.CropA == Args.CropB
        error('pipeline:last:quality:photCalib:overlapPairStability:SameCrop', ...
            'CropA and CropB must differ (got %d twice)', Args.CropA);
    end
    MSA = MScell{Args.CropA};
    MSB = MScell{Args.CropB};
    Need = {Args.MagCol, 'RA', 'Dec'};
    i_requireFields(MSA, Need, Args.CropA);
    i_requireFields(MSB, Need, Args.CropB);

    % Epoch alignment: the "random pick per epoch" scenario is only
    % meaningful when A and B share the same visit set. LAST per-crop
    % stabilityN3 loops with the same DataPath / Pattern produce matched
    % JD vectors, so this normally holds - guard with a clear error.
    if size(MSA.Data.(Args.MagCol), 1) ~= size(MSB.Data.(Args.MagCol), 1)
        error('pipeline:last:quality:photCalib:overlapPairStability:NepochMismatch', ...
            'MS(%d) has %d epochs, MS(%d) has %d - the mixing scenario needs a shared epoch axis.', ...
            Args.CropA, size(MSA.Data.(Args.MagCol), 1), ...
            Args.CropB, size(MSB.Data.(Args.MagCol), 1));
    end
    if ~isempty(MSA.JD) && ~isempty(MSB.JD) && ...
            max(abs(MSA.JD(:) - MSB.JD(:))) > 1/86400   % 1 second slack
        warning('pipeline:last:quality:photCalib:overlapPairStability:JDMismatch', ...
            'MS(%d).JD and MS(%d).JD differ by > 1s in some epochs.', Args.CropA, Args.CropB);
    end

    % --- Apply the same reject mask stabilityN3 / plotters use ----------
    MA = double(MSA.Data.(Args.MagCol));
    MB = double(MSB.Data.(Args.MagCol));
    MA(i_rejectMask(MSA, Args.BadFlags, Args.SNmin, Args.MagCol)) = NaN;
    MB(i_rejectMask(MSB, Args.BadFlags, Args.SNmin, Args.MagCol)) = NaN;

    % --- Sky-match A -> B by median (RA, Dec) ---------------------------
    RAmedA  = median(MSA.Data.RA,  1, 'omitnan');
    DecmedA = median(MSA.Data.Dec, 1, 'omitnan');
    RAmedB  = median(MSB.Data.RA,  1, 'omitnan');
    DecmedB = median(MSB.Data.Dec, 1, 'omitnan');

    Kdt = celestial.KDTreeCoo;
    Kdt = Kdt.populate(RAmedB(:), DecmedB(:), 'InUnits', 'deg');
    NnCell = Kdt.coneSearch(RAmedA(:), DecmedA(:), Args.MatchRadius, ...
                'RadiusUnits', 'arcsec', 'InUnits', 'deg');

    % Keep A's sources that have at least one B neighbour; pick the first
    % (which is the nearest for a small radius). Multi-nearest merging is
    % avoided intentionally - degenerate cases will show up as excess.
    HasMatch = ~cellfun(@isempty, NnCell);
    iA = find(HasMatch);
    iB = cellfun(@(v) v(1), NnCell(HasMatch));

    if isempty(iA)
        Result = i_makeEmptyResult(Args);
        if Args.Verbose
            fprintf('overlapPairStability: crops %d-%d: no sky matches within %.2f"\n', ...
                Args.CropA, Args.CropB, Args.MatchRadius);
        end
        return;
    end

    % --- Pull matched columns from each MS and NaN-align MinEpochs ------
    MA_p = MA(:, iA);
    MB_p = MB(:, iB);
    NokA = sum(~isnan(MA_p), 1);
    NokB = sum(~isnan(MB_p), 1);
    Keep = (NokA > Args.MinEpochs) & (NokB > Args.MinEpochs);

    % Optional mag-window cut on the per-pair median A mag.
    MedMagAll = median(MA_p, 1, 'omitnan');
    if ~isempty(Args.MagRange)
        Keep = Keep & (MedMagAll >= Args.MagRange(1) & MedMagAll <= Args.MagRange(2));
    end

    if ~any(Keep)
        Result = i_makeEmptyResult(Args);
        if Args.Verbose
            fprintf('overlapPairStability: crops %d-%d: %d sky matches but 0 survived MinEpochs / MagRange cut\n', ...
                Args.CropA, Args.CropB, numel(iA));
        end
        return;
    end

    % Cache the per-pair mag BEFORE rescoping iA (which then indexes into
    % A's original source list, not the matched-pair vector).
    MedMag = MedMagAll(Keep);
    iA = iA(Keep);  iB = iB(Keep);
    MA_p = MA_p(:, Keep);
    MB_p = MB_p(:, Keep);
    Npairs = numel(iA);
    Nep    = size(MA_p, 1);

    % --- Per-source scatter for A only and B only -----------------------
    RMS_A = i_perSourceStd(MA_p, Args.MinEpochs, Args.StdMethod);
    RMS_B = i_perSourceStd(MB_p, Args.MinEpochs, Args.StdMethod);

    % --- Random per-epoch mixing bootstrap ------------------------------
    if ~isempty(Args.RandomSeed)
        RngPrev = rng;
        rng(Args.RandomSeed, 'twister');
        CleanupObj = onCleanup(@() rng(RngPrev));   %#ok<NASGU>
    end
    RMS_mix_trials = nan(Args.NRandomTrials, Npairs);
    for T = 1:Args.NRandomTrials
        Pick  = rand(Nep, Npairs) < 0.5;
        MMIX  = MA_p;
        MMIX(~Pick) = MB_p(~Pick);
        % NaN-align: where the selected source is NaN, fall back to the
        % other; only if both are NaN does the mix stay NaN. This keeps
        % the epoch count roughly balanced with RMS_A and RMS_B.
        Nan1 = isnan(MMIX);
        MMIX(Nan1 &  Pick) = MB_p(Nan1 &  Pick);
        MMIX(Nan1 & ~Pick) = MA_p(Nan1 & ~Pick);
        RMS_mix_trials(T, :) = i_perSourceStd(MMIX, Args.MinEpochs, Args.StdMethod);
    end
    RMS_mix       = mean(RMS_mix_trials, 1, 'omitnan');
    RMS_mix_sigma = std (RMS_mix_trials, 0, 1, 'omitnan');

    % --- Theoretical baseline + excess -----------------------------------
    RMS_base_theory = sqrt((RMS_A.^2 + RMS_B.^2) / 2);
    Excess          = RMS_mix - RMS_base_theory;

    Result = struct( ...
        'CropA',           Args.CropA, ...
        'CropB',           Args.CropB, ...
        'NPairs',          Npairs, ...
        'PairsRA',         RAmedA(iA),  'PairsDec', DecmedA(iA), ...
        'MedMag',          MedMag, ...
        'RMS_A',           RMS_A, ...
        'RMS_B',           RMS_B, ...
        'RMS_mix',         RMS_mix, ...
        'RMS_mix_sigma',   RMS_mix_sigma, ...
        'RMS_base_theory', RMS_base_theory, ...
        'Excess',          Excess, ...
        'Args',            Args);

    if Args.Verbose
        fprintf(['overlapPairStability: crops %d-%d\n' ...
                 '  NPairs=%d  median RMS_A=%.4f  RMS_B=%.4f  RMS_mix=%.4f  RMS_base=%.4f  excess=%+.4f\n'], ...
            Args.CropA, Args.CropB, Npairs, ...
            median(RMS_A, 'omitnan'), median(RMS_B, 'omitnan'), ...
            median(RMS_mix, 'omitnan'), median(RMS_base_theory, 'omitnan'), ...
            median(Excess, 'omitnan'));
    end

    if ~isempty(Args.OutFile)
        [D, ~, ~] = fileparts(Args.OutFile);
        if ~isempty(D) && ~exist(D, 'dir'); mkdir(D); end
        save(Args.OutFile, 'Result', '-v7.3');
        if Args.Verbose
            fprintf('overlapPairStability: saved to %s\n', Args.OutFile);
        end
    end

    if Args.Plot
        TitleTag = sprintf('crops %d-%d, N=%d', Result.CropA, Result.CropB, Result.NPairs);
        plotOverlapStability(Result, Args.MagCol, Args.StdMethod, TitleTag);
    end
end


% =========================================================================
function R = i_makeEmptyResult(Args)
    R = struct('CropA', Args.CropA, 'CropB', Args.CropB, 'NPairs', 0, ...
        'PairsRA', [], 'PairsDec', [], 'MedMag', [], ...
        'RMS_A', [], 'RMS_B', [], 'RMS_mix', [], 'RMS_mix_sigma', [], ...
        'RMS_base_theory', [], 'Excess', [], 'Args', Args);
end


% =========================================================================
function C = i_flattenMSToCell(MS)
    if iscell(MS)
        C = {};
        for I = 1:numel(MS)
            Inner = MS{I};
            for J = 1:numel(Inner)
                C{end+1} = Inner(J); %#ok<AGROW>
            end
        end
    else
        C = cell(1, numel(MS));
        for I = 1:numel(MS)
            C{I} = MS(I);
        end
    end
end


% =========================================================================
function i_requireFields(MSk, Fields, K)
    for I = 1:numel(Fields)
        if ~isfield(MSk.Data, Fields{I})
            error('pipeline:last:quality:photCalib:overlapPairStability:MissingField', ...
                'MS(%d) has no Data.%s', K, Fields{I});
        end
    end
end


% =========================================================================
function Rej = i_rejectMask(MSk, BadFlags, SNmin, RefField)
    % Same reject mask as stabilityN3 / plotPhotStabilityMap - bad FLAGS
    % bits OR SN below SNmin.
    Rej = flagBadEpochs(MSk, BadFlags, 'SizeRefField', RefField);
    if SNmin > 0 && isfield(MSk.Data, 'SN')
        Rej = Rej | ~(MSk.Data.SN >= SNmin);
    end
end


% =========================================================================
function S = i_perSourceStd(M, MinEpochs, Method)
    % Per-column epoch scatter (MAD or std) along dim 1 of M. Same idiom
    % used by stabilityN3 / plotPhotStabilityMap so all three tools stay
    % numerically consistent.
    Med = median(M, 1, 'omitnan');
    switch lower(Method)
        case 'robust'
            S = 1.4826 * median(abs(M - Med), 1, 'omitnan');
        case 'plain'
            S = std(M, 0, 1, 'omitnan');
    end
    S(sum(~isnan(M), 1) <= MinEpochs) = NaN;
end
