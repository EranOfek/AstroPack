function Result = overlapPoolStability(MS, Args)
    % Pool overlap-source stability across every crop interface of a
    % LAST-tiled focal plane, then plot the three-scenario STD-vs-mag
    % comparison (crop A only, crop B only, random per-epoch mix).
    % Description: Wraps overlapPairStability in a loop over every crop-
    %              pair interface returned by LASToverlaps (18 or 38 pairs
    %              depending on CroppingScheme). Each pair contributes its
    %              matched-pair per-source arrays (RMS_A, RMS_B, RMS_mix,
    %              MedMag, Excess) to a single pooled Result and pooled
    %              figure. Use to check whether inter-crop calibration
    %              drift is a systemic problem across the focal plane
    %              rather than a quirk of one specific pair.
    % Input  : - MS - MatchedSources array (or cell), one element per crop.
    %                 Same input contract as overlapPairStability /
    %                 plotPhotStabilityMap.
    %          * ...,key,val,...
    %            'CroppingScheme' - 'new' (default) | 'old'. Selects the
    %                               LAST interface table (18 or 38 pairs).
    %            'PairsOverride'  - Optional Nx2 override for the interface
    %                               table (use to restrict to a specific
    %                               subset, e.g. rows-only or columns-only
    %                               interfaces). Default [] (use table
    %                               from LASToverlaps + CroppingScheme).
    %            'MagCol'         - Default 'MAG_APER_3'.
    %            'MatchRadius'    - Default 1 arcsec.
    %            'MagRange'       - Default [].
    %            'StdMethod'      - 'robust' (default) | 'plain'.
    %            'MinEpochs'      - Default 10.
    %            'BadFlags'       - Default {'Saturated','NearEdge'}.
    %            'SNmin'          - Default 10.
    %            'NRandomTrials'  - Default 20.
    %            'RandomSeed'     - Default 0.
    %            'Plot'           - Draw the pooled figure. Default true.
    %            'OutFile'        - Save Result to this .mat. Default ''.
    %            'Verbose'        - Per-interface progress + pooled summary.
    %                               Default true.
    % Output : - Result struct with:
    %            .Pairs           - the interface table used [Ninterfaces x 2]
    %            .PerInterface    - 1xNinterfaces struct array; each is the
    %                               overlapPairStability Result (empty entries
    %                               for interfaces where no pairs survived).
    %            .PoolAll.*       - pooled per-source arrays over every
    %                               interface: RMS_A, RMS_B, RMS_mix,
    %                               RMS_mix_sigma, RMS_base_theory, Excess,
    %                               MedMag, PairsRA, PairsDec, InterfaceIdx
    %                               (1..Ninterfaces, records where each pair
    %                               came from).
    %            .NInterfaces, .NPairs, .Args
    % Author : D. Kovaleva (Aug 2026)
    % See also: overlapPairStability, LASToverlaps,
    %           plotPhotStability (single-set STD-vs-mag plumbing).
    % Example:
    %   % Per-crop stabilityN3 loop first:
    %   MSarr = cell(1, 24);
    %   for k = 1:24
    %       Pat = sprintf('LAST*_1679.c_*_%03d_sci_coadd_Cat_1.fits', k);
    %       MSarr{k} = pipeline.last.quality.photCalib.stabilityN3( ...
    %                    'DataPath', BaseDir, 'Pattern', Pat);
    %   end
    %
    %   % Pool every interface and plot pooled STD-vs-mag / mix-vs-base /
    %   % excess-vs-mag panels:
    %   Rpool = pipeline.last.quality.photCalib.overlapPoolStability(MSarr, ...
    %               'CroppingScheme', 'old', 'MagRange', [12 17]);
    %
    %   % Access the per-interface breakdown afterwards:
    %   [Rpool.PerInterface.NPairs]
    %   Rpool.PoolAll.RMS_mix

    arguments
        MS
        Args.CroppingScheme (1,:) char {mustBeMember(Args.CroppingScheme,{'new','old'})} = 'new'
        Args.PairsOverride                    = []
        Args.MagCol         (1,:) char        = 'MAG_APER_3'
        Args.MatchRadius    (1,1) double      = 1
        Args.MagRange                          = []
        Args.StdMethod      (1,:) char {mustBeMember(Args.StdMethod,{'robust','plain'})} = 'robust'
        Args.MinEpochs      (1,1) double      = 10
        Args.BadFlags             cell        = {'Saturated','NearEdge'}
        Args.SNmin          (1,1) double      = 10
        Args.NRandomTrials  (1,1) double      = 20
        Args.RandomSeed                        = 0
        Args.Plot           (1,1) logical     = true
        Args.OutFile        (1,:) char        = ''
        Args.Verbose        (1,1) logical     = true
    end

    % --- Resolve interface table -----------------------------------------
    if ~isempty(Args.PairsOverride)
        Pairs = Args.PairsOverride;
        if size(Pairs, 2) ~= 2
            error('pipeline:last:quality:photCalib:overlapPoolStability:BadPairs', ...
                'PairsOverride must be Nx2 (crop indices).');
        end
    else
        Pairs = pipeline.last.quality.LASToverlaps('CroppingScheme', Args.CroppingScheme);
    end
    NIfc = size(Pairs, 1);

    if Args.Verbose
        fprintf('overlapPoolStability: %d interfaces to process (%s)\n', ...
            NIfc, Args.CroppingScheme);
    end

    % Pass-through Args for each per-pair call. Explicitly force Plot=false
    % / Verbose=false / no OutFile - only the pooled figure/save happens.
    PairArgs = { ...
        'MagCol',        Args.MagCol, ...
        'MatchRadius',   Args.MatchRadius, ...
        'MagRange',      Args.MagRange, ...
        'StdMethod',     Args.StdMethod, ...
        'MinEpochs',     Args.MinEpochs, ...
        'BadFlags',      Args.BadFlags, ...
        'SNmin',         Args.SNmin, ...
        'NRandomTrials', Args.NRandomTrials, ...
        'RandomSeed',    Args.RandomSeed, ...
        'Plot',          false, ...
        'Verbose',       false};

    PerInterface = repmat(i_emptyPairResult(), 1, NIfc);
    for K = 1:NIfc
        try
            R = pipeline.last.quality.photCalib.overlapPairStability(MS, ...
                    PairArgs{:}, 'CropA', Pairs(K, 1), 'CropB', Pairs(K, 2));
            PerInterface(K) = R;
            if Args.Verbose
                fprintf('  ifc %2d/%d: crops %2d-%-2d  N=%4d\n', ...
                    K, NIfc, Pairs(K, 1), Pairs(K, 2), R.NPairs);
            end
        catch ME
            if Args.Verbose
                fprintf('  ifc %2d/%d: crops %2d-%-2d  FAILED: %s\n', ...
                    K, NIfc, Pairs(K, 1), Pairs(K, 2), ME.message);
            end
        end
    end

    % --- Pool across interfaces ------------------------------------------
    Pool = i_poolPerInterface(PerInterface);
    NPairs = numel(Pool.RMS_A);

    Result = struct( ...
        'Pairs',        Pairs, ...
        'PerInterface', PerInterface, ...
        'PoolAll',      Pool, ...
        'NInterfaces',  NIfc, ...
        'NPairs',       NPairs, ...
        'Args',         Args);

    if Args.Verbose
        fprintf(['overlapPoolStability: pooled  N=%d  ' ...
                 'median RMS_A=%.4f  RMS_B=%.4f  RMS_mix=%.4f  RMS_base=%.4f  excess=%+.4f\n'], ...
            NPairs, ...
            median(Pool.RMS_A,           'omitnan'), ...
            median(Pool.RMS_B,           'omitnan'), ...
            median(Pool.RMS_mix,         'omitnan'), ...
            median(Pool.RMS_base_theory, 'omitnan'), ...
            median(Pool.Excess,          'omitnan'));
    end

    if ~isempty(Args.OutFile)
        [D, ~, ~] = fileparts(Args.OutFile);
        if ~isempty(D) && ~exist(D, 'dir'); mkdir(D); end
        save(Args.OutFile, 'Result', '-v7.3');
        if Args.Verbose
            fprintf('overlapPoolStability: saved to %s\n', Args.OutFile);
        end
    end

    if Args.Plot && NPairs > 0
        TitleTag = sprintf('pooled %d interfaces, N=%d', NIfc, NPairs);
        % plotOverlapStability expects a struct with the flat per-source
        % fields — pass the Pool sub-struct directly (it carries them).
        plotOverlapStability(Pool, Args.MagCol, Args.StdMethod, TitleTag);
    end
end


% =========================================================================
function R = i_emptyPairResult()
    % Placeholder for interfaces that produced nothing usable; matches the
    % overlapPairStability empty-result schema (via i_makeEmptyResult).
    R = struct('CropA', NaN, 'CropB', NaN, 'NPairs', 0, ...
        'PairsRA', [], 'PairsDec', [], 'MedMag', [], ...
        'RMS_A', [], 'RMS_B', [], 'RMS_mix', [], 'RMS_mix_sigma', [], ...
        'RMS_base_theory', [], 'Excess', [], 'Args', struct());
end


% =========================================================================
function Pool = i_poolPerInterface(PerInterface)
    % Concatenate per-source arrays across every non-empty PerInterface
    % entry into one pooled struct. Adds InterfaceIdx so a pooled point
    % can be traced back to its source interface.
    RMS_A = []; RMS_B = []; RMS_mix = []; RMS_mix_sigma = [];
    RMS_base_theory = []; Excess = []; MedMag = [];
    PairsRA = []; PairsDec = []; InterfaceIdx = [];

    for K = 1:numel(PerInterface)
        R = PerInterface(K);
        if isempty(R.RMS_A); continue; end
        N = numel(R.RMS_A);
        RMS_A           = [RMS_A,           R.RMS_A(:).'];           %#ok<AGROW>
        RMS_B           = [RMS_B,           R.RMS_B(:).'];           %#ok<AGROW>
        RMS_mix         = [RMS_mix,         R.RMS_mix(:).'];         %#ok<AGROW>
        RMS_mix_sigma   = [RMS_mix_sigma,   R.RMS_mix_sigma(:).'];   %#ok<AGROW>
        RMS_base_theory = [RMS_base_theory, R.RMS_base_theory(:).']; %#ok<AGROW>
        Excess          = [Excess,          R.Excess(:).'];          %#ok<AGROW>
        MedMag          = [MedMag,          R.MedMag(:).'];          %#ok<AGROW>
        PairsRA         = [PairsRA,         R.PairsRA(:).'];         %#ok<AGROW>
        PairsDec        = [PairsDec,        R.PairsDec(:).'];        %#ok<AGROW>
        InterfaceIdx    = [InterfaceIdx,    repmat(K, 1, N)];        %#ok<AGROW>
    end

    Pool = struct( ...
        'NPairs',          numel(RMS_A), ...
        'RMS_A',           RMS_A, ...
        'RMS_B',           RMS_B, ...
        'RMS_mix',         RMS_mix, ...
        'RMS_mix_sigma',   RMS_mix_sigma, ...
        'RMS_base_theory', RMS_base_theory, ...
        'Excess',          Excess, ...
        'MedMag',          MedMag, ...
        'PairsRA',         PairsRA, ...
        'PairsDec',        PairsDec, ...
        'InterfaceIdx',    InterfaceIdx);
end
