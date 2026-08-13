function MS = stabilityN3(Args)
    % Photometric stability across the calibrated catalogs in /home/dana/tmp/N3.
    %
    % Pipeline:
    %   1. Glob calibrated *_Cat_1.fits from Args.DataPath.
    %   2. Header pre-pass: drop visits with FWHM > FWHMMax or AIRMASS > MaxAirmass.
    %   3. Load surviving catalogs into an AstroCatalog array and run
    %      imProc.match.mergeCatalogs into a MatchedSources object MS.
    %   4. Per-(epoch, source) reject: BadFlags via searchFlags OR SN < SNmin.
    %   5. Optional per-mag relative-ZP correction (lcUtil.zp_meddiff + applyZP)
    %      gated by ApplyMedZP / MedZPMags.
    %   6. Per-source (median, scatter) reduction (StdMethod = 'robust' | 'plain').
    %   7. Scatter plot + binned trend line per requested mag.
    %
    % Returns:
    %   MS - the MatchedSources object (so it can be reused via 'MS', MS).1
    %
    % Examples:
    %   % --- Defaults: load /home/dana/tmp/N3/16062026, plot MAG_APER_3 and
    %   %     MAGAB__APER_3 with robust scatter and edge-binned trend.
    %   MS = stabilityN3();
    %
    %   % --- Same MS, cheap re-plot (skip the load/merge phase) with different
    %   %     scatter estimator and binning strategy.
    %   stabilityN3('MS', MS, 'StdMethod','plain', ...
    %               'BinMode','equalCount', 'MinPerBin', 80);
    %
    %   % --- Both std curves vs a single brightness ruler (median MAGAB__APER_3):
    %   stabilityN3('MS', MS, ...
    %       'Mags',     {'MAGAB__APER_3','MAG_APER_3'}, ...
    %       'RefMagX',  'MAGAB__APER_3');
    %
    %   % --- Custom legend / axis / title text + custom colours, big dots
    %   %     and thick trend lines for a presentation-friendly figure:
    %   stabilityN3('MS', MS, ...
    %       'Mags',       {'MAGAB__APER_3','MAG_APER_3'}, ...
    %       'RefMagX',    'MAGAB__APER_3', ...
    %       'Colors',     struct('MAGAB__APER_3',[0.85 0.10 0.10], ...
    %                            'MAG_APER_3',   [0.00 0.00 0.00]), ...
    %       'Labels',     {'AB calibrated', 'Instrumental'}, ...
    %       'XLabel',     'AB magnitude (median per source)', ...
    %       'YLabel',     'Robust scatter [mag]', ...
    %       'Title',      'Field 1716.c crop 10 — 600 visits, RefSpec=0', ...
    %       'MarkerSize', 12, ...
    %       'LineWidth',  3);
    %
    %   % --- Tiny dots, thin trend (good for dense scatter plots):
    %   stabilityN3('MS', MS, 'MarkerSize', 3, 'LineWidth', 1);
    %
    %   % --- Merge sparse mag bins so each trend point reflects >=200 sources:
    %   stabilityN3('MS', MS, 'BinMode','edges', 'MinPerBin', 200);
    %
    %   % --- Legacy per-edge binning (NaN gaps for empty bins, no merging):
    %   stabilityN3('MS', MS, 'MinPerBin', 0);
    %
    %   % --- Apply relative-ZP correction to only the instrumental column:
    %   stabilityN3('MS', MS, 'ApplyMedZP', true, ...
    %               'MedZPMags', {'MAG_APER_3'}, 'MedZPMaxErr', 0.005);
    %
    %   % --- Convert a Simone flat table to MS, then run stabilityN3 on it:
    %   T  = loadSimoneTable('/home/dana/Downloads/MatchedSources_1716c.csv');
    %   MSs = simoneTableToMS(T);
    %   stabilityN3('MS', MSs, 'Mags', {'MAGAB__APER_3','MAG_APER_3'}, ...
    %               'RefMagX','MAGAB__APER_3', 'StdMethod','robust');

    arguments
        Args.MinEpochs  (1,1) double = 10    % Drop sources cross-matched in <= MinEpochs epochs
        Args.NEpochsCap (1,1) double = 200   % Cap on number of catalogs loaded
        Args.Radius     (1,1) double = 1     % Cross-match radius [arcsec]
        Args.AddXYfull  logical      = true % Stamp MS.Data.XFULL/YFULL after merge
        % ORIGSEC is fixed per crop (same across every visit of a given
        % Pattern), so we read it ONCE from the first surviving catalog and
        % broadcast XFULL = X + (ORIGSEC(1)-1), YFULL = Y + (ORIGSEC(3)-1)
        % into MS.Data as full [Nepoch x Nsrc] matrices - matches the shape
        % convention every other Data field uses. In Join mode (below)
        % XFULL/YFULL come from joinCropsToCatalog directly, so this stamp
        % becomes a no-op.
        Args.JoinedMagLimit (1,1) double = Inf % Per-visit mag cut in Join mode
        % When finite AND Args.Join=true, drop rows from every joined
        % catalog whose Args.MagColForCut column exceeds this limit BEFORE
        % mergeCatalogs. Motivation: the final MS.Data matrices are
        % [Nvisit x Nsrc_pooled] per column, and Nsrc_pooled is dominated
        % by the faint tail of sources detected in only a handful of
        % visits (not usable for per-source scatter anyway). A limit of
        % ~18 mag typically shrinks Nsrc by 10-50x, taking the MS memory
        % peak from tens of GB to <1 GB on 200-visit Join runs. Bright
        % sources (which is what stability characterises) survive.
        Args.JoinedSNmin    (1,1) double = 0   % Per-visit SN cut in Join mode
        % Same idea, gated on SN instead of mag. Applied AFTER MagLimit
        % if both are set; either or both may be used.
        Args.MagColForCut   (1,:) char   = 'MAG_APER_3'
        % Column name used by JoinedMagLimit for the per-visit cut.
        Args.CropOrigsecFromFile logical = false % Override HeaderData.ORIGSEC
        % When true in Join mode, parse each file's crop index from its
        % filename (LAST convention '_<crop>_sci_coadd_(Image|Cat)_<N>.fits')
        % and stamp the correct full-frame ORIGSEC on AI(IC).HeaderData
        % BEFORE joinCropsToCatalog runs. Use when the per-crop Cat headers
        % carry a bogus ORIGSEC (typical after a joint-mode per-crop write
        % that dropped the source coadd's ORIGSEC and left [1 Nx 1 Ny] in
        % every crop). Requires FullFrameSize + CropSize + TileOrder.
        Args.FullFrameSize (1,2) double  = [6388 9576]
        % [Nx Ny] of the LAST focal-plane frame used to compute per-crop
        % ORIGSEC via imUtil.cut.gridSubImage. Default matches the "new"
        % pipeline (6388 x 9576). Override for other frame sizes.
        Args.CropSize      (1,2) double  = [1726 1726]
        % [Sx Sy] of each crop. Default matches the "old" LAST pipeline
        % (1726 x 1726); pass [1716 1716] for the new pipeline.
        Args.TileOrder     (1,:) char {mustBeMember(Args.TileOrder,{'rowmajor','colmajor'})} = 'colmajor'
        % Order in which crop IDs fill the tile grid. 'colmajor' = old
        % pipeline (fills bottom-to-top, then left-to-right). 'rowmajor'
        % = new pipeline (fills left-to-right, then bottom-to-top). Ties
        % into PhotCalibTrans.cropID2RowCol.
        Args.Join       logical      = false % Per-visit join loader (deduped full-frame MS)
        % When true, the loader groups the globbed files by visit stem
        % (parsed via the LAST filename convention
        % '<stem>_<crop>_sci_coadd_(Image|Cat)_<N>.fits'), and for each
        % visit loads ALL crops as an AstroImage array (Image + sibling
        % Cat) and runs imProc.cat.joinCropsToCatalog once, producing a
        % single full-frame AstroCatalog per visit already stamped with
        % XFULL/YFULL/CropID/AIRMASS and deduplicated at overlap zones
        % (highest-SN wins per cluster). The AC array of these per-visit
        % joins then merges into a SINGLE MS in the full-frame frame, so
        % overlap sources are counted exactly once. Use for aggregated
        % stability across all crops of a field without the double-
        % counting artefact you get from a per-crop stabilityN3 loop.
        % Pattern must match every crop of every visit (drop the crop-
        % index token from a per-crop pattern), e.g.
        %   'LAST*_1679.c_*_sci_coadd_Cat_1.fits'.
        % Requires the sibling *_Image_1.fits file next to every Cat.
        Args.Mags       cell         = {'MAG_APER_3', 'MAGAB__APER_3'} %, 'MAG_PSF', 'MAGAB__PSF'}
        Args.FWHMKey    (1,:) char   = 'FWHM'    % Header key to read seeing/FWHM from
        Args.FWHMMax    (1,1) double = 10.0      % Drop catalogs whose FWHM exceeds this
        Args.MaxAirmass (1,1) double = 2.4       % Drop catalogs whose AIRMASS exceeds this. Set Inf to disable.
        Args.BadFlags   cell         = {'Saturated','NearEdge'}
        % Bit names from BitMask.Image.Default. Per-(epoch,source) entries
        % with any of these bits set are masked to NaN before per-source
        % stats. Set to {} to disable.
        Args.SNmin      (1,1) double = 10       % Minimum SN per (epoch,source); 0 disables
        Args.ApplyMedZP logical      = false    % Master switch for medzp
        Args.MedZPMags  cell         = {}       % Subset of Args.Mags to medzp.
        % {} -> apply to all entries in Args.Mags. Non-empty -> apply only
        % to listed mags (must be a subset of Args.Mags). Useful when one
        % calibration left no per-epoch wobble and medzp just injects noise.
        Args.MedZPMaxErr (1,1) double = 0.01    % MaxMagErr passed to zp_meddiff
        Args.BinEdges   (1,:) double = 9:0.5:22 % Mag bin edges for the binned-median trend lines
        Args.DataPath   (1,:) char   = '/home/dana/tmp/N3/16062026'
        % Directory holding the calibrated _Cat_1.fits coadds.
        Args.Pattern    (1,:) char   = 'LAST.01.05.03_*_clear_1716.c_*_001_010_sci_coadd_Cat_1.fits'
        % dir() glob (relative to DataPath) selecting the calibrated coadd
        % catalogs to load. Override to run on a different field / crop /
        % mount. Ignored when a pre-built MS is supplied via 'MS'.
        Args.StdMethod  (1,:) char   = 'robust'  % 'robust' | 'plain'
        % Per-source scatter estimator in perSourceStats:
        %   'robust' -> 1.4826 * median(|x - median|, 'omitnan')  (MAD)
        %   'plain'  -> std(x, 0, 'omitnan')                       (sample std)
        Args.MS                      = []        % Pre-built MatchedSources object
        % When non-empty, skips DataPath glob + header pre-pass + AC load +
        % mergeCatalogs and operates directly on the supplied MS for the
        % masking / stats / plotting phase. Use when only the stats/plot
        % step needs re-running (cheap iteration on StdMethod, BadFlags,
        % BinEdges, SNmin, ApplyMedZP, MedZPMaxErr, MinEpochs).
        Args.RefMagX    (1,:) char   = ''        % Per-source X-axis ref mag
        % If non-empty, the X for every plotted curve is the per-source
        % median of this MS.Data field (e.g. 'MAGAB__APER_3' so std of both
        % calibrated and instrumental mags share one brightness ruler).
        % Default '' keeps old behaviour (X = per-source median of same mag).
        Args.Colors     struct       = struct()  % Per-field RGB overrides
        % Struct whose field names match entries in Args.Mags, values are
        % 1x3 RGB triplets in [0,1]. Anything not in the struct falls back
        % to the built-in colorFor() palette, then lines(N).
        % Example:
        %   stabilityN3('Colors', struct( ...
        %       'MAG_APER_3',    [0.00 0.60 0.00], ...
        %       'MAGAB__APER_3', [0.85 0.10 0.10]))
        Args.BinMode    (1,:) char   = 'edges'   % 'edges' | 'equalCount'
        % Trend-line bin strategy:
        %   'edges'      -> fixed mag bins from Args.BinEdges. Bins with
        %                   fewer than Args.MinPerBin sources are merged
        %                   into their right-hand neighbour (and the tail
        %                   merges into the previous emitted bin) so no
        %                   noisy single-star points are drawn. Set
        %                   MinPerBin=0 to emit every non-empty bin.
        %   'equalCount' -> equal-population bins, Args.MinPerBin sources
        %                   per bin, X plotted at the chunk's median X.
        Args.MinPerBin  (1,1) double = 50        % Min sources/bin (both modes)
        Args.Labels     cell         = {}        % Legend labels, one per Args.Mags
        % Display names for the legend, in the same order as Args.Mags. The
        % dot-series uses Labels{K} verbatim; the median trend uses
        % [Labels{K} ' median']. Empty {} -> auto-derive via labelFor()
        % (MAGAB__ -> MAG_AB_, then TeX-escape underscores).
        Args.XLabel     (1,:) char   = ''        % X-axis label override
        Args.YLabel     (1,:) char   = ''        % Y-axis label override
        Args.Title      (1,:) char   = ''        % Title override
        % User-supplied labels render with 'Interpreter','none' so '_' is
        % literal; auto labels keep the latex interpreter for consistency
        % with the default 'MAG'/'STD' rendering.
        Args.MarkerSize (1,1) double = 6         % Dot size for scatter
        Args.LineWidth  (1,1) double = 2         % Width of binned trend lines
        Args.Plot       (1,1) logical = false     % Draw the std-vs-mag figure.
        % Set false to only build/return MS without a figure - useful in loops
        % (e.g. one MS per crop across all 24 crops) where 24 figures are not
        % wanted, or for headless reuse of the MS by plotPhotStabilityMap.
    end

    DataPath = Args.DataPath;
    Pattern  = Args.Pattern;
    Radius   = Args.Radius;
    NEpochsCap = Args.NEpochsCap;

    % ErrFields is also needed in the medzp block downstream, so compute it
    % unconditionally regardless of whether we build MS or reuse it.
    ErrFields = cellfun(@errFieldFor, Args.Mags, 'UniformOutput', false);

    if isempty(Args.MS)
        % --- build phase: glob, header pre-pass, AC load, mergeCatalogs ---
        FileList = dir(fullfile(DataPath, Pattern));
        % Fail clearly on an empty match: the default Pattern is field-specific
        % (1716.c crop 10), so a different field/crop silently matches nothing
        % and would otherwise crash deep in mergeCatalogs on an empty array.
        if isempty(FileList)
            error('pipeline:last:quality:photCalib:stabilityN3:NoCatalogs', ...
                ['No catalogs matched Pattern "%s" under %s.\n', ...
                 'The default Pattern targets field 1716.c crop 10 - pass a ', ...
                 '''Pattern'' for your field/crop, e.g.\n', ...
                 '  ''Pattern'', ''LAST*_1679*_*_010_sci_coadd_Cat_1.fits'''], ...
                Pattern, DataPath);
        end
        [~, ord] = sort({FileList.name});
        FileList = FileList(ord);
        % NEpochsCap unit differs by mode:
        %   * per-crop (Join=false): cap is FILES (one file = one epoch).
        %   * per-visit (Join=true): cap is VISITS. We can't apply it here
        %     because file->visit grouping happens after the header peek;
        %     defer to i_loadJoinedPerVisit (which caps whole visits so we
        %     never load a partial 24-crop set).
        if Args.Join
            NEpochs = numel(FileList);   % peek every file's header
        else
            NEpochs  = min(NEpochsCap, numel(FileList));
            FileList = FileList(1:NEpochs);
        end
        fprintf('Loading %d catalogs from %s\n', NEpochs, DataPath);

        % Pass 1: header-only metadata read + quality cuts. Avoids loading
        % any AstroCatalog data for visits that fail FWHM/AIRMASS thresholds.
        JD   = nan(NEpochs, 1);
        FWHM = nan(NEpochs, 1);
        AM   = nan(NEpochs, 1);
        for I = 1:NEpochs
            FullPath = fullfile(DataPath, FileList(I).name);
            JD(I)   = readHdrAny(FullPath, {'JD', 'MIDJD'});
            FWHM(I) = readHdrAny(FullPath, {Args.FWHMKey});
            AM(I)   = readHdrAny(FullPath, {'AIRMASS'});
        end

        KeepFWHM = ~isfinite(Args.FWHMMax)   | (isfinite(FWHM) & FWHM <= Args.FWHMMax);
        KeepAM   = ~isfinite(Args.MaxAirmass) | (isfinite(AM)   & AM   <= Args.MaxAirmass);
        KeepCat  = KeepFWHM & KeepAM;
        if any(~KeepFWHM)
            fprintf('Dropping %d/%d catalogs with %s > %.2f\n', ...
                sum(~KeepFWHM), NEpochs, Args.FWHMKey, Args.FWHMMax);
        end
        if any(~KeepAM)
            fprintf('Dropping %d/%d catalogs with AIRMASS > %.2f\n', ...
                sum(~KeepAM), NEpochs, Args.MaxAirmass);
        end
        FileList = FileList(KeepCat);
        JD       = JD(KeepCat);
        FWHM     = FWHM(KeepCat);
        AM       = AM(KeepCat);
        NEpochs  = numel(FileList);

        if Args.Join
            % Per-visit loader: group surviving files by visit stem, then
            % for each visit load the 24-crop AstroImage array and run
            % joinCropsToCatalog. AC ends up length = number of visits
            % (not files), and every AC(v) is a full-frame catalog with
            % overlap sources already deduplicated. NEpochsCap is applied
            % here at visit granularity so partial visits are never loaded.
            [AC, VisitStems, JD, AM] = i_loadJoinedPerVisit( ...
                DataPath, FileList, JD, AM, Args.FWHMKey, FWHM, NEpochsCap, ...
                Args.JoinedMagLimit, Args.JoinedSNmin, Args.MagColForCut, ...
                Args.CropOrigsecFromFile, Args.FullFrameSize, Args.CropSize, Args.TileOrder);
            NEpochs = numel(AC);
        else
            % Pass 2: load only the survivors (per-file / per-crop).
            fprintf('Loading %d surviving catalogs\n', NEpochs);
            AC = AstroCatalog.empty(0, NEpochs);
            for I = 1:NEpochs
                FullPath = fullfile(DataPath, FileList(I).name);
                AC(I)    = AstroCatalog(FullPath);
                fprintf('  %3d: %s  JD=%.5f  %s=%.3f  AIRMASS=%.3f\n', ...
                    I, FileList(I).name, JD(I), Args.FWHMKey, FWHM(I), AM(I));
            end
        end

        BaseMatch  = {'RA','Dec','X','Y','SN','FLAGS', ...
                      'BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS', ...
                      'FLUX_APER_3', 'MAG_PSF', 'MAGAB__PSF'};
        MatchedColums = unique([BaseMatch, Args.Mags], 'stable');
        StatCols   = unique([{'RA','Dec','X','Y','SN'}, Args.Mags], 'stable');
        StatFunInd = repmat({[1 3]}, 1, numel(StatCols));
        % AllCols: full [Nepoch x Nsrc] matrices in MS.Data. Include FLAGS
        % so searchFlags has data to consume per-epoch-per-source, SN so
        % the SNmin filter can act per (epoch,source), and X/Y so a downstream
        % positional analysis (plotPhotStabilityMap) can map per-source scatter
        % vs detector position without a rebuild. MAGERR_<suffix> is needed by
        % zp_meddiff if ApplyMedZP=true — gated to avoid bloating the merge.
        AllCols   = unique([Args.Mags, {'FLAGS','SN','X','Y'}], 'stable');
        % In Join mode, promote XFULL/YFULL/CropID (produced by
        % joinCropsToCatalog) so plotPhotStabilityMap/XY can address the
        % full-frame frame directly. AddXYfull's post-hoc stamp becomes a
        % no-op downstream because MS.Data.XFULL/YFULL already exist.
        if Args.Join
            JoinCols       = {'XFULL','YFULL','CropID'};
            MatchedColums  = unique([MatchedColums, JoinCols], 'stable');
            AllCols        = unique([AllCols,       JoinCols], 'stable');
        end
        if Args.ApplyMedZP
            AllCols       = unique([AllCols,       ErrFields], 'stable');
            MatchedColums = unique([MatchedColums, ErrFields], 'stable');
        end

        [~, MS] = imProc.match.mergeCatalogs(AC(:), ...
            'Radius',        Radius, ...
            'MatchedColums', MatchedColums, ...
            'JD',            JD, ...
            'ColNamesStat',  StatCols, ...
            'FunIndStat',    StatFunInd, ...
            'ColNamesAll',   AllCols,...
            'RelPhot', false);

        % Catalog array is no longer needed once MS is built; free it
        % before the stats/plot phase to avoid carrying ~0.5-1 MB per epoch.
        clear AC

        % Stash per-epoch AIRMASS on the MS as broadcast [Nepoch x Nsrc] so
        % downstream tools (plotMagCurves, ad-hoc analyses) can read it
        % without re-peeking headers or requiring a CSV. Broadcast keeps the
        % [Nep x Nsrc] shape convention of every other MS.Data field. Cost
        % ~ Nep*Nsrc*8 bytes (~4 MB for 100 epochs x 5000 sources).
        NsrcFinal = size(MS.Data.(Args.Mags{1}), 2);
        MS.Data.AIRMASS = repmat(AM(:), 1, NsrcFinal);

        % Optional XFULL/YFULL stamp. ORIGSEC is fixed per crop (same
        % across every visit of a given Pattern), so we read it ONCE from
        % the first surviving catalog and broadcast the offsets across
        % every epoch. Adds MS.Data.XFULL/YFULL as full [Nepoch x Nsrc]
        % matrices so downstream plotPhotStabilityMap('XField','XFULL',...)
        % works without a per-caller rebuild.
        % Skip the post-hoc stamp when XFULL/YFULL are already on MS.Data —
        % Join mode fills them via joinCropsToCatalog + the (correct)
        % in-memory ORIGSEC override, and the on-disk file's ORIGSEC is
        % often stale ([1 Nx 1 Ny] from a broken joint-write step). If we
        % re-read ORIGSEC from the file here we would clobber the correct
        % XFULL/YFULL with X + 0, Y + 0.
        if Args.AddXYfull && isfield(MS.Data, 'X') && isfield(MS.Data, 'Y') ...
                && ~isfield(MS.Data, 'XFULL')
            SamplePath = fullfile(DataPath, FileList(1).name);
            OS = [];
            for HDU = [2, 3, 1]   % same HDU search order as readHdrAny
                try
                    AH  = AstroHeader(SamplePath, HDU);
                    Raw = AH.getVal('ORIGSEC');
                    if ischar(Raw) || isstring(Raw)
                        OS = imUtil.ccdsec.ccdsecStr2num(char(Raw));
                        if numel(OS) >= 4 && all(isfinite(OS)); break; end
                        OS = [];
                    end
                catch
                    % HDU absent or unreadable - try next
                end
            end
            if isempty(OS)
                warning('stabilityN3:AddXYfull', ...
                    'Could not read ORIGSEC from %s; skipping XFULL/YFULL', ...
                    SamplePath);
            else
                MS.Data.XFULL = MS.Data.X + (OS(1) - 1);
                MS.Data.YFULL = MS.Data.Y + (OS(3) - 1);
                fprintf('Stamped XFULL/YFULL using ORIGSEC = [%g %g %g %g]\n', OS);
            end
        end
    else
        % --- reuse phase: skip straight to masking + stats + plotting ---
        MS = Args.MS;
        NEpochs = size(MS.Data.(Args.Mags{1}), 1);
        fprintf('Reusing supplied MS: %d epochs x %d sources\n', ...
                NEpochs, size(MS.Data.(Args.Mags{1}), 2));
    end

    % Per-(epoch,source) reject mask (Nepoch x Nsrc): bad bits OR low S/N.
    % NaN'd into each mag matrix below before the per-source reduction.
    MatSize = size(MS.Data.(Args.Mags{1}));
    RejectMask = false(MatSize);
    if ~isempty(Args.BadFlags) && isfield(MS.Data, 'FLAGS')
        try
            RejectMask = RejectMask | MS.searchFlags('FlagsList', Args.BadFlags);
        catch ME
            warning('stabilityN3:Flags', ...
                'searchFlags failed (%s) — disabling flag filter', ME.message);
        end
    end
    if Args.SNmin > 0 && isfield(MS.Data, 'SN')
        RejectMask = RejectMask | ~(MS.Data.SN >= Args.SNmin);   % also catches NaN SN
    end
    fprintf('Rejected %d/%d (epoch,source) cells (flags + SN<%.2f)\n', ...
        nnz(RejectMask), numel(RejectMask), Args.SNmin);

    % Optional per-mag relative-ZP correction (lcUtil.zp_meddiff +
    % MatchedSources.applyZP). Acts on each requested mag independently:
    % zp_meddiff computes a per-epoch FitZP from bright sources (median of
    % differences), applyZP then subtracts FitZP from that mag matrix
    % in-place. Same pattern as the canonical user recipe.
    if Args.ApplyMedZP
        if isempty(Args.MedZPMags)
            DoMedZP = true(1, numel(Args.Mags));
        else
            DoMedZP = ismember(Args.Mags, Args.MedZPMags);
        end
        fprintf('--- medzp pass (%d/%d mags) ---\n', sum(DoMedZP), numel(Args.Mags));
        for K = 1:numel(Args.Mags)
          if DoMedZP(K)
            F  = Args.Mags{K};
            EF = ErrFields{K};
            if ~isfield(MS.Data, F) || ~isfield(MS.Data, EF)
                fprintf('  [%d/%d] %s SKIP: missing %s in MS.Data\n', ...
                    K, numel(Args.Mags), F, ...
                    onlyMissing({F, EF}, MS.Data));
            else
                try
                    R = lcUtil.zp_meddiff(MS, ...
                        'MagField',    {F}, ...
                        'MagErrField', {EF}, ...
                        'MaxMagErr',   Args.MedZPMaxErr);
                    FitZP = R.FitZP(:);
                    if all(isnan(FitZP))
                        warning('stabilityN3:MedZP', ...
                            '  [%d/%d] %s : zp_meddiff returned all-NaN FitZP (likely no source met MaxMagErr=%.3f). Skipping applyZP.', ...
                            K, numel(Args.Mags), F, Args.MedZPMaxErr);
                    else
                        [MS, Applied] = applyZP(MS, FitZP, ...
                            'ApplyToMagField', F);
                        fprintf(['  [%d/%d] %-20s applied to {%s}: ', ...
                                 'Nsrc=%d  FitZP range=[%+.4f, %+.4f]  std=%.4f mag\n'], ...
                            K, numel(Args.Mags), F, strjoin(Applied, ','), ...
                            R.Nsrc, ...
                            min(FitZP), max(FitZP), ...
                            1.4826 * median(abs(FitZP - median(FitZP, 'omitnan')), 'omitnan'));
                    end
                catch ME
                    warning('stabilityN3:MedZP', ...
                        '  [%d/%d] %s FAILED (%s) — leaving mags unchanged', ...
                        K, numel(Args.Mags), F, ME.message);
                end
            end
          end   % closes if DoMedZP(K)
        end     % closes for K
    end         % closes if Args.ApplyMedZP

    % Per-source (median, std) for each requested mag, restricted to sources
    % with more than Args.MinEpochs finite cross-IDs in that mag (per-quantity
    % so a source missing in one column doesn't taint another).
    Nm     = numel(Args.Mags);
    Colors = zeros(Nm, 3);
    Fallback = lines(Nm);
    for K = 1:Nm
        F = Args.Mags{K};
        if isfield(Args.Colors, F)
            Colors(K, :) = Args.Colors.(F);
        else
            Colors(K, :) = colorFor(F, Fallback(K, :));
        end
    end
    PerMag(Nm) = struct('Med', [], 'Std', []);
    for K = 1:Nm
        F = Args.Mags{K};
        if isfield(MS.Data, F)
            M = MS.Data.(F);
            M(RejectMask) = NaN;   % drop (epoch,source) cells: bad bits OR low SN
            [PerMag(K).Med, PerMag(K).Std] = perSourceStats(M, Args.MinEpochs, Args.StdMethod);
            fprintf('Kept %d/%d (%s) sources with N_epochs > %d\n', ...
                sum(~isnan(PerMag(K).Med)), numel(PerMag(K).Med), F, Args.MinEpochs);
        else
            warning('stabilityN3:NoField', ...
                'MS.Data has no field %s — skipping', F);
        end
    end

    % Optional shared X-axis: per-source median of a single reference mag,
    % NaN-masked to match the same RejectMask + MinEpochs filter PerMag uses.
    UseRefX = ~isempty(Args.RefMagX);
    if UseRefX
        if ~isfield(MS.Data, Args.RefMagX)
            error('stabilityN3:RefMagX', ...
                'RefMagX="%s" is not a field of MS.Data', Args.RefMagX);
        end
        Mref = MS.Data.(Args.RefMagX);
        Mref(RejectMask) = NaN;
        Xref = median(Mref, 1, 'omitnan');
        Xref(sum(~isnan(Mref), 1) <= Args.MinEpochs) = NaN;
        fprintf('Shared X-axis = median(%s) [%d sources finite]\n', ...
            Args.RefMagX, sum(isfinite(Xref)));
    end

    if ~isempty(Args.Labels) && numel(Args.Labels) ~= Nm
        error('stabilityN3:Labels', ...
            'numel(Labels)=%d must equal numel(Mags)=%d.', ...
            numel(Args.Labels), Nm);
    end
    UseAutoLabels = isempty(Args.Labels);

    if ~Args.Plot
        return;
    end

    figure('WindowStyle','docked','Color',[1 1 1]); box on; hold on; grid on;
    set(gca, 'YScale', 'log');
    LegEntries = {};
    for K = 1:Nm
        if ~isempty(PerMag(K).Med)
            if UseRefX; Xk = Xref; else; Xk = PerMag(K).Med; end
            plot(Xk, PerMag(K).Std, '.', ...
                'MarkerSize', Args.MarkerSize, 'Color', Colors(K,:));
            if UseAutoLabels
                LegEntries{end+1} = labelFor(Args.Mags{K}); %#ok<AGROW>
            else
                LegEntries{end+1} = Args.Labels{K}; %#ok<AGROW>
            end
        end
    end
    for K = 1:Nm
        if ~isempty(PerMag(K).Med)
            if UseRefX; Xk = Xref; else; Xk = PerMag(K).Med; end
            [Bx, By] = binTrend(Xk, PerMag(K).Std, Args);
            plot(Bx, By, '-', 'LineWidth', Args.LineWidth, 'Color', Colors(K,:));
            if UseAutoLabels
                Lab = labelFor(Args.Mags{K});
            else
                Lab = Args.Labels{K};
            end
            LegEntries{end+1} = [Lab, ' median']; %#ok<AGROW>
        end
    end

    xlim([9, 22]); ylim([1e-3, 10]);
    if ~isempty(Args.XLabel)
        xlabel(Args.XLabel, 'Interpreter','none');
    elseif UseRefX
        xlabel(sprintf('Median %s', Args.RefMagX), 'Interpreter','none');
    else
        xlabel('MAG', 'Interpreter','latex');
    end
    if ~isempty(Args.YLabel)
        ylabel(Args.YLabel, 'Interpreter','none');
    else
        ylabel('STD', 'Interpreter','latex');
    end
    if UseAutoLabels
        legend(LegEntries, 'Location','best');
    else
        legend(LegEntries, 'Location','best', 'Interpreter','none');
    end
    if ~isempty(Args.Title)
        title(Args.Title, 'Interpreter','none');
    else
        title(sprintf('Stability across %d epochs', NEpochs));
    end
end

% =========================================================================
function [AC, VisitStems, JDvis, AMvis] = i_loadJoinedPerVisit(DataPath, FileList, JDfile, AMfile, FWHMKey, FWHMfile, NEpochsCap, MagLimit, SNmin, MagColForCut, CropOrigsecFromFile, FullFrameSize, CropSize, TileOrder)
    % Per-visit loader for stabilityN3's Join mode.
    % Groups the surviving files by visit stem (parsed from the LAST
    % filename convention '<stem>_<crop>_sci_coadd_(Image|Cat)_<N>.fits'),
    % loads every crop of each visit as an AstroImage array via
    % AstroImage.readProducts (Image + sibling Cat), and runs
    % imProc.cat.joinCropsToCatalog to build ONE full-frame AstroCatalog
    % per visit (already deduplicated at overlap zones by
    % joinCropsToCatalog's own highest-SN cluster winner).
    % NEpochsCap gates the number of VISITS loaded (not files), so
    % partial 24-crop visits are never produced by the cap.
    % Returns:
    %   AC          - 1xNvisits AstroCatalog array (empty entries dropped)
    %   VisitStems  - 1xNvisits cellstr of visit stems (for diagnostics)
    %   JDvis, AMvis - per-visit JD and AIRMASS (copied from the visit's
    %                  first surviving file - all crops of one visit share
    %                  these header values).
    Nfile = numel(FileList);
    Stems = cell(1, Nfile);
    CropIdx = nan(1, Nfile);
    for I = 1:Nfile
        [~, FName, ~] = fileparts(FileList(I).name);
        Tok = regexp(FName, '^(.+?)_(\d+)_sci_coadd_(?:Image|Cat)_\d+$', 'tokens', 'once');
        if ~isempty(Tok)
            Stems{I} = Tok{1};
            CropIdx(I) = str2double(Tok{2});
        else
            Stems{I} = FName;   % ungrouped filenames become singleton visits
        end
    end
    [Us, ~, IUs] = unique(Stems);
    Nvis = numel(Us);
    % Sort visits by first-file JD so the cap keeps a chronological prefix
    % (matches the file-mode cap which keeps the first N sorted files).
    FirstJD = nan(1, Nvis);
    for V = 1:Nvis
        M = find(IUs == V, 1);
        FirstJD(V) = JDfile(M);
    end
    [~, VOrd] = sort(FirstJD);
    if isfinite(NEpochsCap) && NEpochsCap > 0 && NEpochsCap < Nvis
        fprintf('NEpochsCap=%d: keeping the earliest %d of %d visits\n', ...
            NEpochsCap, NEpochsCap, Nvis);
        VOrd = VOrd(1:NEpochsCap);
    end
    % Rebuild the ordering so the outer loop indexes visits in JD order.
    Us  = Us(VOrd);
    Rev = zeros(1, Nvis);
    Rev(VOrd) = 1:numel(VOrd);
    IUs = Rev(IUs);
    KeepFile = IUs > 0;                    % files whose visit survived
    IUs = IUs(KeepFile);
    FileList = FileList(KeepFile);
    JDfile   = JDfile(KeepFile);
    AMfile   = AMfile(KeepFile);
    FWHMfile = FWHMfile(KeepFile);
    CropIdx  = CropIdx(KeepFile);
    Nvis = numel(Us);
    fprintf('Joining %d visits from %d file(s)\n', Nvis, numel(FileList));

    % Precompute per-crop ORIGSEC table if override requested.
    % gridSubImage returns rows in 'row-major, X changes fastest' order,
    % which matches PhotCalibTrans.cropID2RowCol convention 'rowmajor':
    % index i = (Row-1)*Ncols + Col. For 'colmajor' we translate CropID
    % -> (Row, Col) -> rowmajor index -> row in the CCDSEC table.
    OrigsecTable = [];
    if CropOrigsecFromFile
        RowMajor = imUtil.cut.gridSubImage(FullFrameSize, CropSize);
        Ncols = ceil(FullFrameSize(1) / CropSize(1));
        Nrows = ceil(FullFrameSize(2) / CropSize(2));
        Ntiles = size(RowMajor, 1);
        OrigsecTable = zeros(Ntiles, 4);
        for K = 1:Ntiles
            [R, C] = PhotCalibTrans.cropID2RowCol(K, Nrows, Ncols, TileOrder);
            OrigsecTable(K, :) = double(RowMajor((R - 1) * Ncols + C, :));
        end
        fprintf('CropOrigsecFromFile: precomputed ORIGSEC for %d crops (%s, FullFrame=[%d %d], CropSize=[%d %d])\n', ...
            Ntiles, TileOrder, FullFrameSize(1), FullFrameSize(2), CropSize(1), CropSize(2));
    end

    AC          = AstroCatalog.empty(0, Nvis);
    VisitStems  = cell(1, Nvis);
    JDvis       = nan(1, Nvis);
    AMvis       = nan(1, Nvis);
    Kept        = false(1, Nvis);
    for V = 1:Nvis
        M = find(IUs == V);
        [~, Ord] = sort(CropIdx(M));
        M = M(Ord);
        CatPaths = arrayfun(@(m) fullfile(DataPath, FileList(m).name), M(:), 'UniformOutput', false);
        % Sibling Image_1.fits paths — AstroImage.readProducts wants the
        % Image side; the AstroFileName parser auto-attaches the Cat sibling.
        ImgPaths = cellfun(@(p) strrep(p, '_Cat_1.fits', '_Image_1.fits'), CatPaths, 'UniformOutput', false);
        try
            AI_v = AstroImage.readProducts(ImgPaths, 'ExtraOutProduct', "Cat");
            % Optional: override each crop's HeaderData.ORIGSEC using the
            % crop index parsed from the filename. Fixes the "every crop's
            % Cat header carries [1 Nx 1 Ny]" bug from joint-mode per-crop
            % writeback, without which XFULL/YFULL collapse to X/Y.
            if ~isempty(OrigsecTable)
                for II = 1:numel(AI_v)
                    Cid = CropIdx(M(II));
                    if isfinite(Cid) && Cid >= 1 && Cid <= size(OrigsecTable, 1)
                        AI_v(II).HeaderData.replaceVal({'ORIGSEC'}, ...
                            {sprintf('[%d %d %d %d]', OrigsecTable(Cid, :))});
                    end
                end
            end
            [J, ~] = imProc.cat.joinCropsToCatalog(AI_v, 'Verbose', false);
            if isempty(J) || isempty(J.Catalog)
                fprintf('  %3d: %s  join yielded empty catalog, skipping\n', V, Us{V});
                continue;
            end
            % Optional per-visit pre-merge cuts. Purpose is purely memory
            % pressure: MS.Data ends up [Nvisit x Nsrc_pooled] per column,
            % so dropping the faint tail here shrinks the peak by ~1-2
            % orders of magnitude on typical LAST fields. Bright sources
            % (which is what stability analysis characterises) survive.
            NBefore = height(J.Table);
            if isfinite(MagLimit)
                Vn = J.Table.Properties.VariableNames;
                if any(strcmp(Vn, MagColForCut))
                    Keep = J.Table.(MagColForCut) < MagLimit;
                    J.Catalog = J.Catalog(Keep, :);
                end
            end
            if SNmin > 0
                Vn = J.Table.Properties.VariableNames;
                if any(strcmp(Vn, 'SN'))
                    Keep = J.Table.SN >= SNmin;
                    J.Catalog = J.Catalog(Keep, :);
                end
            end
            NAfter = height(J.Table);
            AC(V) = J;
            VisitStems{V} = Us{V};
            JDvis(V) = JDfile(M(1));
            AMvis(V) = AMfile(M(1));
            Kept(V)  = true;
            if NAfter < NBefore
                fprintf('  %3d: %s  %d crops joined  N=%d -> %d after cuts  JD=%.5f  %s=%.3f  AIRMASS=%.3f\n', ...
                    V, Us{V}, numel(M), NBefore, NAfter, JDvis(V), FWHMKey, FWHMfile(M(1)), AMvis(V));
            else
                fprintf('  %3d: %s  %d crops joined  N=%d  JD=%.5f  %s=%.3f  AIRMASS=%.3f\n', ...
                    V, Us{V}, numel(M), NAfter, JDvis(V), FWHMKey, FWHMfile(M(1)), AMvis(V));
            end
        catch ME
            fprintf('  %3d: %s  load/join failed (%s); skipping\n', V, Us{V}, ME.message);
        end
    end
    if ~any(Kept)
        error('pipeline:last:quality:photCalib:stabilityN3:JoinFailed', ...
              'No visits survived the load/join step in Join mode.');
    end
    AC         = AC(Kept);
    VisitStems = VisitStems(Kept);
    JDvis      = JDvis(Kept);
    AMvis      = AMvis(Kept);
end


% =========================================================================
function V = readHdrAny(FullPath, Keys)
    % Look up the first matching key across HDUs 2,3,1 (our calibrated FITS
    % writes the user header on HDU 2; original LAST coadds carry it on
    % HDU 3; HDU 1 as last resort). Returns NaN if no key/HDU combination
    % yields a finite value.
    V = NaN;
    Found = false;
    for HDU = [2, 3, 1]
        if ~Found
            try
                AH = AstroHeader(FullPath, HDU);
                for K = 1:numel(Keys)
                    if ~Found
                        Val = AH.getVal(Keys{K});
                        if isnumeric(Val) && ~isnan(Val)
                            V = Val;
                            Found = true;
                        end
                    end
                end
            catch
                % HDU absent or unreadable — try next
            end
        end
    end
end

% =========================================================================
function L = labelFor(Field)
    % Display label: MAGAB__ -> MAG_AB_, then TeX-escape underscores.
    L = strrep(Field, 'MAGAB__', 'MAG_AB_');
    L = strrep(L, '_', '\_');
end

% =========================================================================
function S = onlyMissing(Names, DataStruct)
    % Comma-joined list of names that are NOT fields of DataStruct.
    Miss = ~cellfun(@(n) isfield(DataStruct, n), Names);
    S = strjoin(Names(Miss), ', ');
end

% =========================================================================
function EF = errFieldFor(F)
    % Strip the MAG / MAGAB__ prefix and prepend MAGERR_ so that the
    % calibrated columns (MAGAB__APER_3) and the instrumental columns
    % (MAG_APER_3) both map to the same instrumental MAGERR_<suffix>.
    % Calibration is additive, so the same MagErr applies to both.
    if startsWith(F, 'MAGAB__')
        EF = ['MAGERR_', F(8:end)];   % MAGAB__APER_3 -> MAGERR_APER_3
    elseif startsWith(F, 'MAG_')
        EF = ['MAGERR_', F(5:end)];   % MAG_APER_3 -> MAGERR_APER_3
    else
        EF = '';
    end
end

% =========================================================================
function C = colorFor(Field, Fallback)
    % Per-column color overrides; falls back to the supplied color otherwise.
    switch Field
        case 'MAG_PSF',        C = [0.85, 0.10, 0.10];   % red
        case 'MAG_APER_3',     C = [0.00, 0.00, 0.00];   % black
        case 'MAGAB__APER_3',  C = [0.30, 0.75, 0.93];   % light blue
        otherwise
            C = Fallback;
    end
end

% =========================================================================
function [Med, Std] = perSourceStats(M, MinEpochs, StdMethod)
    % Median and per-source scatter along epochs for each source column of M
    % ([Nepoch x Nsrc]), with NaN for sources that have <= MinEpochs finite
    % epochs. StdMethod selects the estimator:
    %   'robust' -> 1.4826 * median(|x - median|, 'omitnan')  (MAD-based)
    %   'plain'  -> std(x, 0, 'omitnan')                       (sample std)
    if nargin < 3 || isempty(StdMethod); StdMethod = 'robust'; end
    Med = median(M, 1, 'omitnan');
    switch lower(StdMethod)
        case 'robust'
            Std = 1.4826 * median(abs(M - Med), 1, 'omitnan');
        case 'plain'
            Std = std(M, 0, 1, 'omitnan');
        otherwise
            error('stabilityN3:perSourceStats:BadStdMethod', ...
                  'StdMethod must be ''robust'' or ''plain'' (got ''%s'')', StdMethod);
    end
    Ncross = sum(~isnan(M), 1);
    Drop   = Ncross <= MinEpochs;
    Med(Drop) = NaN;
    Std(Drop) = NaN;
end

% =========================================================================
function [Bx, By] = binTrend(X, Y, Args)
    % Dispatcher: fixed-edge bins vs equal-population bins.
    switch lower(Args.BinMode)
        case 'edges'
            [Bx, By] = binMed(X, Y, Args.BinEdges, Args.MinPerBin);
        case 'equalcount'
            [Bx, By] = binEqualCount(X, Y, Args.MinPerBin);
        otherwise
            error('stabilityN3:binTrend:BadMode', ...
                'BinMode must be ''edges'' or ''equalCount'' (got ''%s'').', ...
                Args.BinMode);
    end
end

% =========================================================================
function [Bx, By] = binMed(X, Y, Edges, MinPerBin)
    % Median of Y in each X-bin. With MinPerBin > 0, bins with fewer than
    % MinPerBin sources are merged with the next bin to the right; if the
    % trailing bin is still short it folds back into the previous emitted
    % group, so every drawn point reflects >= MinPerBin sources (except
    % possibly the rightmost when the total is itself smaller).
    % MinPerBin = 0 reproduces the legacy behaviour: one output per edge
    % bin at the bin centre, NaN for empty bins.
    if nargin < 4 || isempty(MinPerBin); MinPerBin = 0; end
    Good = isfinite(X) & isfinite(Y);
    X = X(Good); Y = Y(Good);
    Nb = numel(Edges) - 1;
    if MinPerBin <= 0
        % Legacy path: one output per bin, NaN for empty.
        Bx = 0.5 * (Edges(1:end-1) + Edges(2:end));
        By = nan(1, Nb);
        for B = 1:Nb
            In = X >= Edges(B) & X < Edges(B+1);
            if B == Nb; In = In | X == Edges(B+1); end
            if any(In); By(B) = median(Y(In)); end
        end
        return
    end
    % Merge path: accumulate sources from left to right; emit a group when
    % its count crosses MinPerBin. Tail merges into the previous group.
    % Force column orientation so vertcat works whether X/Y came in as a
    % row or column vector.
    X = X(:); Y = Y(:);
    BinIdx = discretize(X, Edges);
    OK = ~isnan(BinIdx);
    X = X(OK); Y = Y(OK); BinIdx = BinIdx(OK);
    Groups = {};
    AccX = zeros(0,1); AccY = zeros(0,1);
    for B = 1:Nb
        Sel  = BinIdx == B;
        AccX = [AccX; X(Sel)];     %#ok<AGROW>
        AccY = [AccY; Y(Sel)];     %#ok<AGROW>
        if numel(AccY) >= MinPerBin
            Groups{end+1} = {AccX, AccY}; %#ok<AGROW>
            AccX = zeros(0,1); AccY = zeros(0,1);
        end
    end
    if ~isempty(AccY)
        if ~isempty(Groups)
            Groups{end}{1} = [Groups{end}{1}; AccX];
            Groups{end}{2} = [Groups{end}{2}; AccY];
        else
            Groups{end+1} = {AccX, AccY};
        end
    end
    NG = numel(Groups);
    Bx = nan(1, NG); By = nan(1, NG);
    for G = 1:NG
        Bx(G) = median(Groups{G}{1});
        By(G) = median(Groups{G}{2});
    end
end

% =========================================================================
function [Bx, By] = binEqualCount(X, Y, N)
    % Equal-population bins: sort by X, chunk into Nb bins of >= N sources
    % each (the last bin absorbs the remainder). X plotted at chunk median.
    Good = isfinite(X) & isfinite(Y);
    X = X(Good); Y = Y(Good);
    Ntot = numel(X);
    if Ntot == 0; Bx = []; By = []; return; end
    [Xs, Ord] = sort(X);
    Ys = Y(Ord);
    Nb = max(1, floor(Ntot / N));
    Bx = nan(1, Nb); By = nan(1, Nb);
    for B = 1:Nb
        I1 = (B-1)*N + 1;
        if B == Nb; I2 = Ntot; else; I2 = B*N; end
        Bx(B) = median(Xs(I1:I2));
        By(B) = median(Ys(I1:I2));
    end
end
