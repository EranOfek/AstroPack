function compareStabilityMS(MSList, Labels, Args)
    % Overlay binned Std-vs-Median trend lines from two or more MS sets
    % Description: Lightweight comparator that takes a cell of MS structs
    %              (e.g. R0.MS, R2.MS from two batchPhotStability runs),
    %              computes the per-source (median, std) for each named
    %              quantity, bins by median, and overlays the binned-
    %              median trend line of std-vs-median for each set on
    %              the same axes — one subplot per quantity, one line
    %              per MS. Optional shaded +-std band per set; no per-
    %              source dots (those clutter overlays). For angular
    %              quantities (RA / Dec) std is rescaled to arcsec and
    %              for RA additionally multiplied by cos(median Dec).
    %
    % Input  : - MSList cell. Each entry may be any of:
    %              (a) a 1xNcrop MatchedSources array (direct loadVisit
    %                  output)
    %              (b) a cell of MatchedSources (one per crop)
    %              (c) a legacy mode-keyed struct MS.(Mode){crop} = MS
    %                  (e.g. returned by matchPhotEpochs /
    %                  batchPhotStability).
    %            All entries are normalised internally to shape (c).
    %          - Labels cell of legend labels, one per MSList entry.
    %                    * ...,key,val,...
    %                      'Quantities' - Cell of column names to plot.
    %                                     Default {'RA','Dec','MAG_PSF',
    %                                     'MAG_APER_3','MAG_AB_PSF',
    %                                     'MAG_AB_APER_3'}. Applied to
    %                                     every MS when QuantitiesPerSet
    %                                     is empty.
    %                      'QuantitiesPerSet' - cell of cells (one inner
    %                                     cell per MS in MSList), each
    %                                     the same length. Entries at
    %                                     the same panel index are
    %                                     overlaid on the same axes, so
    %                                     v0 can be drawn with
    %                                     'MAG_PSF' while v1/v2 are
    %                                     drawn with 'MAG_AB_PSF' on
    %                                     the same panel. Default {}
    %                                     (use Args.Quantities for all).
    %                      'AngularQuantities' - Names treated as angular
    %                                     (std rescaled to arcsec).
    %                                     Default {'RA','Dec'}.
    %                      'Mode'       - Mode field in each MS to use.
    %                                     Default 'percrop'.
    %                      'RefMag'     - X-axis ref mag for angular
    %                                     quantities. Default
    %                                     'MAG_APER_3'.
    %                      'BinWidth'   - Bin width [mag]. Default 0.5.
    %                      'MinEpochs'  - Min non-NaN epochs per source.
    %                                     Default 2. Sources with fewer
    %                                     finite epochs in the named
    %                                     quantity get dropped before
    %                                     binning.
    %                      'ShowStdBand'- Shade +-std around each trend
    %                                     (within-bin scatter of per-
    %                                     source Std values, including
    %                                     outliers). Default false.
    %                      'ShowMedianErrorBand' - Shade the standard
    %                                     error of the binned median
    %                                     itself, computed as
    %                                     1.2533 * std(Y) / sqrt(N) per
    %                                     bin (normal-approx SE of the
    %                                     median). Narrower than the
    %                                     ShowStdBand envelope; tells
    %                                     you how well the trend LINE is
    %                                     determined, not how wide the
    %                                     data scatter is. Default false.
    %                      'ShowLegend' - Draw the per-panel legend with
    %                                     one entry per overlaid MS set.
    %                                     Default true; pass false to
    %                                     suppress the legend (useful
    %                                     when the legend box obscures
    %                                     the trend lines).
    %                      'BackgroundMag' - Faint cut on RefMag for
    %                                     angular quantities. Default
    %                                     22.
    %                      'LinkAxes'   - 'xy'|'x'|'y'|'off'. Default
    %                                     'y'. Linking is applied
    %                                     PER ROW only — panels in the
    %                                     same subplot row share a
    %                                     scale, but different rows
    %                                     remain independent (mags,
    %                                     fluxes and arcsec coords
    %                                     have very different ranges).
    %                      'Nrows','Ncols' - explicit subplot grid. If
    %                                     both empty (default) the
    %                                     layout is ceil(sqrt(Nq))
    %                                     columns by ceil(Nq/Ncols)
    %                                     rows. Set one (or both) to
    %                                     pin the grid; the other is
    %                                     derived. Errors when
    %                                     Nrows*Ncols < numel(QPS{1}).
    %                      'LogXQuantities' - Cell of column names
    %                                     whose panel uses log-scale X
    %                                     and log-spaced bins (so flux
    %                                     ranges spanning many decades
    %                                     bin uniformly). Default
    %                                     {'FLUX_APER_3','FLUX_PSF'}.
    %                                     Set to {} to keep all panels
    %                                     linear. Note: columns also
    %                                     listed in FluxAsMag override
    %                                     this (they become mag panels).
    %                      'FluxAsMag'  - Cell of column names whose
    %                                     values are converted to
    %                                     instrumental magnitudes
    %                                     (Y -> -2.5*log10(Y) per
    %                                     epoch) before reduction. The
    %                                     resulting panel is rendered
    %                                     in mag units: linear X (with
    %                                     MaxMedMag cap), log Std on Y,
    %                                     so flux stability is
    %                                     directly comparable with the
    %                                     MAG panels. Default
    %                                     {'FLUX_APER_3','FLUX_PSF'}.
    %                                     Set to {} to keep flux in
    %                                     raw flux units (log axes).
    %                      'AIcoadd'    - Optional cell of AstroImage
    %                                     arrays, one per MS in MSList,
    %                                     each a 1xNcrop array of the
    %                                     coadds whose headers carry
    %                                     the photometric ZP. When
    %                                     non-empty and Q is in
    %                                     FluxAsMag, the X axis becomes
    %                                     -2.5*log10(median Flux) + ZP
    %                                     (ZP from the per-crop coadd
    %                                     header keyword ZPKey). Std
    %                                     on Y is unaffected (ZP is a
    %                                     constant offset per crop).
    %                                     Default {} (no ZP shift).
    %                      'ZPKey'      - Header keyword for the ZP
    %                                     value. Default 'PH_ZP'.
    %                      'LogYQuantities' - Cell of column names
    %                                     whose panel uses log-scale Y
    %                                     (Std axis). Default includes
    %                                     all common stability columns
    %                                     ('FLUX_APER_3','FLUX_PSF',
    %                                     'MAG_PSF','MAG_APER_3',
    %                                     'MAG_AB_PSF','MAG_AB_APER_3',
    %                                     'RA','Dec') since per-source
    %                                     Std spans a wide range for
    %                                     each — so the default
    %                                     preserves the previous global
    %                                     log-Y behaviour. Pass a
    %                                     narrower list to restrict
    %                                     log-Y to only those panels.
    %                      'SplitByRow' - When true, each row of the
    %                                     Nrows x Ncols grid is rendered
    %                                     as its own separate figure
    %                                     (so axis-linking is naturally
    %                                     scoped to one row at a time
    %                                     and rows that don't share
    %                                     units never compete for the
    %                                     same Y scale). Default false
    %                                     (single figure with subplots).
    %                      'MaxMedMag'  - Hard upper limit on the
    %                                     X axis for MAG-units panels
    %                                     (mag quantities and the
    %                                     angular RefMag axis). Default
    %                                     20. Set to Inf to disable.
    %                                     Does not apply to LogX panels
    %                                     (FLUX-style).
    %                      'MinTicks'   - Minimum number of numbered
    %                                     ticks on each axis. After
    %                                     linkaxes, any axis with fewer
    %                                     auto-picked ticks gets back-
    %                                     filled with MinTicks evenly
    %                                     spaced positions (log-spaced
    %                                     on log axes). Default 2.
    %                      'ARMS_N'     - Number of lowest per-source
    %                                     Std values used for the
    %                                     "asymptotic RMS" estimate.
    %                                     ARMS = median of the N lowest
    %                                     STRICTLY-POSITIVE Std values
    %                                     pooled across all crops of
    %                                     the MS (exact zeros excluded
    %                                     as float32 quantization
    %                                     artifacts), per cohort per
    %                                     panel quantity, shown in the
    %                                     legend. Default 20.
    %                      'BadFlags'   - cell of FLAGS bit names; any
    %                                     (epoch,source) entry carrying
    %                                     one of these bits is set to
    %                                     NaN before the Std reduction
    %                                     (via MatchedSources.searchFlags).
    %                                     Default {'Saturated',
    %                                     'NearEdge','Overlap'}; {}
    %                                     disables FLAGS filtering.
    % Output : - none (creates one figure).
    % Author : D. Kovaleva (May 2026)
    % Example: % Compare v0 vs v2 stability for the same date:
    %          DataPath = '/archimedes/iss165/LAST.01.03.01/2025/06/01/proc';
    %          R0 = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DataPath, 'VisitGlob','*v0', 'Recursive',true, ...
    %                  'FileType','coadd', 'Plot',false);
    %          R2 = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DataPath, 'VisitGlob','*v2', 'Recursive',true, ...
    %                  'FileType','coadd', 'Plot',false);
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'});
    %
    %          % Add flux columns; include shaded +-std bands:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'Quantities', {'FLUX_APER_3','FLUX_PSF', ...
    %                                 'MAG_PSF','MAG_APER_3'}, ...
    %                  'ShowStdBand', true);
    %
    %          % Compare three v-groups in one figure, per-bin median lines:
    %          R1 = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DataPath, 'VisitGlob','*v1', 'Recursive',true, ...
    %                  'FileType','coadd', 'Plot',false);
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R1.MS, R2.MS}, {'v0','v1','v2'});
    %
    %          % Coadd vs proc within the same v-group:
    %          R0c = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DataPath, 'VisitGlob','*v0', 'Recursive',true, ...
    %                  'FileType','coadd', 'Plot',false);
    %          R0p = pipeline.last.quality.photCalib.batchPhotStability( ...
    %                  DataPath, 'VisitGlob','*v0', 'Recursive',true, ...
    %                  'FileType','proc',  'Plot',false);
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0c.MS, R0p.MS}, {'v0 coadd','v0 proc'}, ...
    %                  'Quantities', {'MAG_PSF','MAG_APER_3'});
    %
    %          % Re-plot from a saved batchPhotStability .mat:
    %          S0 = load('~/stab_v0_coadd.mat');
    %          S2 = load('~/stab_v2_coadd.mat');
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {S0.Result.MS, S2.Result.MS}, {'v0','v2'});
    %
    %          % Loosen MinEpochs (more sources kept) but turn off
    %          % central/peripheral linking and use a smaller bin:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'MinEpochs', 2, 'BinWidth', 0.25, ...
    %                  'LinkAxes', 'off');
    %
    %          % v0 has relative MAGs only; v1 and v2 are calibrated, so
    %          % compare v0's MAG_PSF/MAG_APER_3 against v1/v2's
    %          % MAG_AB_PSF/MAG_AB_APER_3 on the same panels:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R1.MS, R2.MS}, {'v0','v1','v2'}, ...
    %                  'QuantitiesPerSet', { ...
    %                      {'MAG_PSF',    'MAG_APER_3'   }, ...   % v0
    %                      {'MAG_AB_PSF', 'MAG_AB_APER_3'}, ...   % v1
    %                      {'MAG_AB_PSF', 'MAG_AB_APER_3'}});      % v2
    %
    %          % 3x2 grid with per-row axis linking. Row 1 (RA/Dec)
    %          % shares an arcsec Std scale, row 2 (fluxes) shares a
    %          % flux Std scale, row 3 (mags) shares a mag Std scale;
    %          % rows do not affect each other.
    %          %   row 1: RA           Dec
    %          %   row 2: FLUX_PSF     FLUX_APER_3
    %          %   row 3: MAG_PSF (v0) MAG_APER_3 (v0)
    %          %          vs          vs
    %          %          MAG_AB_PSF  MAG_AB_APER_3 (v2)
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'QuantitiesPerSet', { ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_PSF',   'MAG_APER_3'   }, ...   % v0
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_AB_PSF','MAG_AB_APER_3'}}, ...  % v2
    %                  'Nrows', 3, 'Ncols', 2);
    %
    %          % Pin layout to a single column (one panel per row, each
    %          % row independent):
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'Quantities', {'MAG_PSF','MAG_APER_3','RA','Dec'}, ...
    %                  'Ncols', 1);
    %
    %          % Same 3x2 grid but with each row also sharing the X axis
    %          % within the row (useful when comparing FLUX_PSF vs
    %          % FLUX_APER_3 which span similar but not identical ranges):
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'QuantitiesPerSet', { ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_PSF',   'MAG_APER_3'   }, ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_AB_PSF','MAG_AB_APER_3'}}, ...
    %                  'Nrows', 3, 'Ncols', 2, 'LinkAxes', 'xy');
    %
    %          % Same grid but each panel auto-scaled (no row linking):
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'QuantitiesPerSet', { ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_PSF',   'MAG_APER_3'   }, ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_AB_PSF','MAG_AB_APER_3'}}, ...
    %                  'Nrows', 3, 'Ncols', 2, 'LinkAxes', 'off');
    %
    %          % Same 3x2 grid; flux panels use log-X automatically via
    %          % the default LogXQuantities. Override or extend the list
    %          % per call, e.g. to add a custom flux column or to keep
    %          % all panels linear:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'QuantitiesPerSet', { ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_PSF',   'MAG_APER_3'   }, ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_AB_PSF','MAG_AB_APER_3'}}, ...
    %                  'Nrows', 3, 'Ncols', 2, ...
    %                  'LogXQuantities', {'FLUX_PSF','FLUX_APER_3','SN'});
    %
    %          % Force every panel linear-X (disable the flux log-X
    %          % default):
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'Quantities', {'FLUX_PSF','FLUX_APER_3', ...
    %                                 'MAG_PSF','MAG_APER_3'}, ...
    %                  'LogXQuantities', {});
    %
    %          % Three separate figures (one per row), each with two
    %          % panels sharing both X and Y axes within the figure:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'QuantitiesPerSet', { ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_PSF',   'MAG_APER_3'   }, ...
    %                      {'RA','Dec','FLUX_PSF','FLUX_APER_3', ...
    %                       'MAG_AB_PSF','MAG_AB_APER_3'}}, ...
    %                  'Nrows', 3, 'Ncols', 2, ...
    %                  'SplitByRow', true, 'LinkAxes', 'xy');
    %
    %          % Trend with the standard error of the binned median
    %          % shaded (narrow, dark band — represents the uncertainty
    %          % of the trend line itself):
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'ShowMedianErrorBand', true);
    %
    %          % Both bands: fat translucent std-of-data + narrow darker
    %          % standard-error-of-median ribbon on top of the trend:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'ShowStdBand', true, 'ShowMedianErrorBand', true);
    %
    %          % Force at least 4 numbered ticks on every axis (useful
    %          % when linkaxes collapsed the visible range and only one
    %          % decade label survived on the log Std axis):
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, 'MinTicks', 4);
    %
    %          % ARMS shown per cohort in the legend; tune the depth
    %          % and the FLAGS rejection list:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'ARMS_N', 30, ...
    %                  'BadFlags', {'Saturated','NearEdge','Overlap'});
    %
    %          % Disable FLAGS filtering (Std on raw photometry):
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, 'BadFlags', {});
    %
    %          % Flux panels as instrumental magnitudes (default):
    %          % both axes are mag, so the flux stability lines align
    %          % directly with the MAG_PSF / MAG_APER_3 panels for
    %          % apples-to-apples comparison.
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'Quantities', {'MAG_PSF','MAG_APER_3', ...
    %                                 'FLUX_PSF','FLUX_APER_3'});
    %
    %          % Keep fluxes in raw flux units (log X / log Y), MAG
    %          % panels unchanged:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'Quantities', {'MAG_PSF','MAG_APER_3', ...
    %                                 'FLUX_PSF','FLUX_APER_3'}, ...
    %                  'FluxAsMag', {});
    %
    %          % Convert only one of the two flux columns to mag:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {R0.MS, R2.MS}, {'v0','v2'}, ...
    %                  'Quantities', {'FLUX_PSF','FLUX_APER_3'}, ...
    %                  'FluxAsMag', {'FLUX_PSF'});
    %
    %          % Flux-as-mag X axis with photometric ZP added from each
    %          % coadd's FITS header (PH_ZP). Load the coadds once per
    %          % visit and hand them in alongside MST0/MST2:
    %          AIcA = pipeline.last.load.loadVisitCatHdr( ...
    %                  'VisitDirs', "/path/to/v0_visit", 'FileType','coadd');
    %          AIcB = pipeline.last.load.loadVisitCatHdr( ...
    %                  'VisitDirs', "/path/to/v2_visit", 'FileType','coadd');
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {MST0, MST2}, {'v0','v2'}, ...
    %                  'AIcoadd', {AIcA{1}, AIcB{1}}, ...
    %                  'ZPKey',   'PH_ZP');           % default; can omit
    %
    %          % Same idea but a non-default header key:
    %          pipeline.last.quality.photCalib.compareStabilityMS( ...
    %                  {MST0, MST2}, {'v0','v2'}, ...
    %                  'AIcoadd', {AIcA{1}, AIcB{1}}, ...
    %                  'ZPKey',   'MAGZP');

    arguments
        MSList                cell
        Labels                cell
        Args.Quantities       cell = {'RA','Dec','MAG_PSF','MAG_APER_3', ...
                                      'MAG_AB_PSF','MAG_AB_APER_3'}
        Args.QuantitiesPerSet cell = {}
        Args.AngularQuantities cell = {'RA','Dec'}
        Args.Mode             (1,:) char = 'percrop'
        Args.RefMag           (1,:) char = 'MAG_APER_3'
        Args.BinWidth         (1,1) double = 0.5
        Args.MinEpochs        (1,1) double = 2
        Args.ShowStdBand      logical = false
        Args.ShowMedianErrorBand logical = false
        Args.ShowLegend       logical = true
        Args.BackgroundMag    (1,1) double = 22
        Args.LinkAxes         (1,:) char ...
            {mustBeMember(Args.LinkAxes, {'xy','x','y','off'})} = 'y'
        Args.Nrows            double = []
        Args.Ncols            double = []
        Args.LogXQuantities   cell   = {'FLUX_APER_3','FLUX_PSF'}
        % Flux columns to convert to instrumental magnitude
        % (Y -> -2.5*log10(Y) per epoch) before reduction. The panels
        % then plot in mag units on both axes (linear-X, log-Y same as
        % MAG panels). Conversion drops F <= 0 to NaN.
        Args.FluxAsMag        cell   = {'FLUX_APER_3','FLUX_PSF'}
        % Optional: per-set list of coadd AstroImages from which to read
        % the ZP for the FluxAsMag conversion. AIcoadd{k} is a 1xNcrop
        % AstroImage array aligned with the crops of MSList{k}. When
        % empty (default) no ZP is added — the X axis is the raw
        % -2.5*log10(median Flux). When provided, the X axis becomes
        % -2.5*log10(median Flux) + ZP, where ZP comes from
        % AIcoadd{k}(crop).HeaderData.getVal(ZPKey).
        Args.AIcoadd          cell   = {}
        Args.ZPKey            (1,:) char = 'PH_ZP'
        % Y-axis: Std spans a wide range for *every* stability quantity
        % (mag, arcsec, flux), so the default keeps every common panel
        % on log Y. Pass {'FLUX_APER_3','FLUX_PSF'} (or any subset) to
        % restrict log Y to only those panels.
        Args.LogYQuantities   cell   = {'FLUX_APER_3','FLUX_PSF', ...
                                        'MAG_PSF','MAG_APER_3', ...
                                        'MAG_AB_PSF','MAG_AB_APER_3', ...
                                        'RA','Dec'}
        Args.SplitByRow       logical = false
        Args.MaxMedMag        (1,1) double = 20
        Args.MinTicks         (1,1) double {mustBePositive, mustBeInteger} = 1
        Args.ARMS_N           (1,1) double {mustBePositive, mustBeInteger} = 20
        Args.BadFlags         cell = {'Saturated','NearEdge','Overlap'}
    end

    if numel(Labels) < numel(MSList)
        for K = numel(Labels)+1:numel(MSList)
            Labels{K} = sprintf('set %d', K);
        end
    end

    % Normalise each entry in MSList. Accepted shapes per element:
    %   (1) MatchedSources array     (e.g. direct from loadVisit; 1xNcrop)
    %   (2) cell of MatchedSources   (e.g. {Ms_crop1, Ms_crop2, ...})
    %   (3) mode-keyed struct        (legacy: MS.(Mode){crop} = MS)
    % After this block every entry is shape (3) keyed by Args.Mode.
    for K = 1:numel(MSList)
        M = MSList{K};
        if isa(M, 'MatchedSources')
            MSList{K} = struct(Args.Mode, {num2cell(M(:).')});
        elseif iscell(M)
            MSList{K} = struct(Args.Mode, {M(:).'});
        elseif ~isstruct(M)
            error('compareStabilityMS:BadInput', ...
                ['MSList{%d} must be a MatchedSources array, a cell of ', ...
                 'MatchedSources, or a mode-keyed struct.'], K);
        end
    end

    % Per-set quantity lists. If user did not supply QuantitiesPerSet,
    % every MS uses the same Args.Quantities list. Each entry must have
    % the same length; entries with identical positions get drawn on the
    % same panel (so set 1 can be MAG_PSF and set 2 MAG_AB_PSF, etc.).
    if isempty(Args.QuantitiesPerSet)
        QPS = repmat({Args.Quantities}, 1, numel(MSList));
    else
        if numel(Args.QuantitiesPerSet) ~= numel(MSList)
            error('compareStabilityMS:BadQPS', ...
                'QuantitiesPerSet must have one cell entry per MS in MSList.');
        end
        QPS = Args.QuantitiesPerSet;
        Lens = cellfun(@numel, QPS);
        if any(Lens ~= Lens(1))
            error('compareStabilityMS:UnevenQPS', ...
                'All QuantitiesPerSet entries must have the same length.');
        end
    end

    Nq = numel(QPS{1});
    if isempty(Args.Nrows) && isempty(Args.Ncols)
        Ncols = ceil(sqrt(Nq));
        Nrows = ceil(Nq / Ncols);
    elseif isempty(Args.Nrows)
        Ncols = Args.Ncols;
        Nrows = ceil(Nq / Ncols);
    elseif isempty(Args.Ncols)
        Nrows = Args.Nrows;
        Ncols = ceil(Nq / Nrows);
    else
        Nrows = Args.Nrows;
        Ncols = Args.Ncols;
        if Nrows * Ncols < Nq
            error('compareStabilityMS:LayoutTooSmall', ...
                'Nrows*Ncols=%d < Nq=%d', Nrows*Ncols, Nq);
        end
    end
    Cmap  = lines(numel(MSList));

    if ~Args.SplitByRow
        figure('Name', sprintf('Stability comparison: %s', strjoin(Labels, ' vs ')), ...
               'Position', [50 50 380*Ncols, 350*Nrows]);
    end

    AxArr = gobjects(1, Nq);
    for Iq = 1:Nq
        if Args.SplitByRow
            Rr = ceil(Iq / Ncols);
            Cc = mod(Iq - 1, Ncols) + 1;
            if Cc == 1
                figure('Name', sprintf('Stability comparison row %d: %s', ...
                    Rr, strjoin(Labels, ' vs ')), ...
                    'Position', [50 50 380*Ncols, 350]);
            end
            Ax = subplot(1, Ncols, Cc); hold(Ax, 'on');
        else
            Ax = subplot(Nrows, Ncols, Iq); hold(Ax, 'on');
        end
        AxArr(Iq) = Ax;
        % Use the first set's name for the panel title / axis labels;
        % each set still plots its own column (possibly different).
        Q0 = QPS{1}{Iq};
        AllNames = cellfun(@(C) C{Iq}, QPS, 'Uni', 0);
        IsAng = ismember(Q0, Args.AngularQuantities);
        % LogX panel if any set's column for this row is a configured
        % LogXQuantity (flux columns by default).
        % Quantities converted to magnitudes lose their log-X / log-Y
        % flux treatment — they behave like ordinary MAG panels
        % (linear X with MaxMedMag cap, log Y for the Std axis).
        IsFluxAsMag = any(ismember(AllNames, Args.FluxAsMag));
        IsLogX = any(ismember(AllNames, Args.LogXQuantities)) && ~IsFluxAsMag;
        IsLogY = any(ismember(AllNames, Args.LogYQuantities));
        for K = 1:numel(MSList)
            Qk    = QPS{K}{Iq};
            IsAngK = ismember(Qk, Args.AngularQuantities);
            AIcrops = [];
            if ~isempty(Args.AIcoadd) && K <= numel(Args.AIcoadd)
                AIcrops = Args.AIcoadd{K};
            end
            [Med, Std] = collectMedStd(MSList{K}, Qk, IsAngK, Args, AIcrops);
            if isempty(Med); continue; end
            [Bx, By, Bs, Bn] = binStats(Med, Std, Args.BinWidth, IsLogX);
            DName = Labels{K};
            if ~strcmp(Qk, Q0)
                DName = sprintf('%s [%s]', Labels{K}, Qk);   % name differs from panel default
            end
            % N reflects sources actually rendered: only those whose
            % median fell in a bin that survived binStats' N>=5 cut.
            % ARMS = median of the N lowest per-source Std values (the
            % "asymptotic" / irreducible-floor estimate). Exact zeros
            % are excluded — std==0 is a float32 quantization artifact
            % (large RA values stored as single), not a real floor.
            Sfin = Std(isfinite(Std) & Std > 0);
            if numel(Sfin) >= Args.ARMS_N
                Ssort = sort(Sfin);
                ARMS  = median(Ssort(1:Args.ARMS_N));
            else
                ARMS  = NaN;
            end
            DName = sprintf('%s (N=%d, ARMS=%.3g)', DName, sum(Bn), ARMS);
            if Args.ShowStdBand && ~isempty(Bx)
                Lo = max(By - Bs, eps);  Hi = By + Bs;
                fill([Bx; flipud(Bx)], [Lo; flipud(Hi)], Cmap(K,:), ...
                    'EdgeColor', 'none', 'FaceAlpha', 0.18, ...
                    'HandleVisibility', 'off');
            end
            if Args.ShowMedianErrorBand && ~isempty(Bx)
                % Standard error of the median per bin, normal-approx:
                % SE_med ~= 1.2533 * std(Y) / sqrt(N).
                SEM = 1.2533 .* Bs ./ sqrt(max(Bn, 1));
                Lo  = max(By - SEM, eps);  Hi = By + SEM;
                fill([Bx; flipud(Bx)], [Lo; flipud(Hi)], Cmap(K,:), ...
                    'EdgeColor', 'none', 'FaceAlpha', 0.35, ...
                    'HandleVisibility', 'off');
            end
            plot(Ax, Bx, By, '-', 'Color', Cmap(K,:), 'LineWidth', 2, ...
                'DisplayName', DName);
        end
        if IsLogY; set(Ax, 'YScale', 'log'); end
        grid(Ax, 'on'); box(Ax, 'on');
        if IsLogX
            set(Ax, 'XScale', 'log');
        elseif isfinite(Args.MaxMedMag)
            % Cap median-MAG axis at the user-set upper limit (default 22
            % mag); preserve whatever lower bound MATLAB auto-picked.
            xl = xlim(Ax);
            xlim(Ax, [xl(1), min(xl(2), Args.MaxMedMag)]);
        end
        if IsAng
            xlabel(Ax, sprintf('Median %s', Args.RefMag), 'Interpreter', 'none');
            ylabel(Ax, 'Std [arcsec]');
        elseif IsFluxAsMag
            % Escape '_' so TeX renders 'FLUX_PSF' literally (not as a
            % subscript) while still interpreting the log_{10} sub.
            Qe = strrep(Q0, '_', '\_');
            ZpSuffix = '';
            if ~isempty(Args.AIcoadd)
                ZpSuffix = sprintf(' + ZP (%s)', strrep(Args.ZPKey, '_', '\_'));
            end
            xlabel(Ax, sprintf('-2.5 log_{10}(median %s)%s [mag]', Qe, ZpSuffix), ...
                'Interpreter', 'tex');
            ylabel(Ax, sprintf('Std(-2.5 log_{10}(%s)) [mag]', Qe), ...
                'Interpreter', 'tex');
        else
            xlabel(Ax, sprintf('Median %s', Q0), 'Interpreter', 'none');
            ylabel(Ax, sprintf('Std %s', Q0), 'Interpreter', 'none');
        end
        if all(strcmp(AllNames, Q0))
            title(Ax, Q0, 'Interpreter', 'none');
        else
            title(Ax, strjoin(AllNames, ' / '), 'Interpreter', 'none');
        end
        if Args.ShowLegend
            legend(Ax, 'Location', 'best', 'Interpreter', 'none');
        end
    end

    % Axis linking is scoped per ROW: only panels in the same row share
    % a scale (RA/Dec, fluxes, mags have very different Y ranges and
    % linking everything would compress the small panels to invisibility).
    if ~strcmpi(Args.LinkAxes, 'off')
        for Rr = 1:Nrows
            From = (Rr - 1) * Ncols + 1;
            To   = min(Rr * Ncols, Nq);
            if To > From
                linkaxes(AxArr(From:To), Args.LinkAxes);
            end
        end
    end

    % Ensure every axis ends up with at least Args.MinTicks numbered
    % ticks (auto-ticks can collapse to 0 or 1 when the linked range is
    % a sub-decade on log axes).
    for I = 1:numel(AxArr)
        if isgraphics(AxArr(I))
            ensureMinTicks(AxArr(I), Args.MinTicks);
        end
    end
end

% =========================================================================
function ensureMinTicks(Ax, N)
    XL = xlim(Ax);
    YL = ylim(Ax);
    if numel(get(Ax, 'XTick')) < N && XL(2) > XL(1)
        if strcmp(get(Ax, 'XScale'), 'log') && XL(1) > 0
            set(Ax, 'XTick', logspace(log10(XL(1)), log10(XL(2)), N));
        else
            set(Ax, 'XTick', linspace(XL(1), XL(2), N));
        end
    end
    if numel(get(Ax, 'YTick')) < N && YL(2) > YL(1)
        if strcmp(get(Ax, 'YScale'), 'log') && YL(1) > 0
            set(Ax, 'YTick', logspace(log10(YL(1)), log10(YL(2)), N));
        else
            set(Ax, 'YTick', linspace(YL(1), YL(2), N));
        end
    end
end

% =========================================================================
function [Med, Std] = collectMedStd(MS, Q, IsAng, Args, AIcrops)
    % Pool per-source (median, std) of column Q across all crops of MS.(Mode).
    if nargin < 5; AIcrops = []; end
    Med = []; Std = [];
    if ~isstruct(MS) || ~isfield(MS, Args.Mode); return; end
    CCell = MS.(Args.Mode);
    for Ic = 1:numel(CCell)
        Msi = CCell{Ic};
        if isempty(Msi) || ~isfield(Msi.Data, Q); continue; end
        Y = Msi.Data.(Q);
        if isempty(Y); continue; end
        % Per-epoch FLAGS rejection: NaN out (epoch,source) entries
        % carrying any of the BadFlags bits before the Std reduction.
        if ~isempty(Args.BadFlags) && isfield(Msi.Data, 'FLAGS')
            try
                Bad = Msi.searchFlags('FlagsList', Args.BadFlags);
                Y(Bad) = NaN;
            catch
            end
        end
        % Flux-as-mag conversion (per epoch, per source). Non-positive
        % flux values become NaN so median/std see clean magnitudes.
        % Optional additive ZP from the coadd header (Args.ZPKey,
        % default 'PH_ZP') shifts X to calibrated magnitudes; doesn't
        % affect the Std on Y (ZP is constant per crop).
        if ismember(Q, Args.FluxAsMag)
            Y(Y <= 0) = NaN;
            Y = -2.5 * log10(Y);
            if ~isempty(AIcrops) && Ic <= numel(AIcrops)
                try
                    ZP = AIcrops(Ic).HeaderData.getVal(Args.ZPKey);
                    if isnumeric(ZP) && isscalar(ZP) && isfinite(ZP)
                        Y = Y + ZP;
                    end
                catch
                end
            end
        end
        M = median(Y, 1, 'omitnan').';     % column Nsrc x 1
        S = std(Y, 0, 1, 'omitnan').';     % column Nsrc x 1
        NValid = sum(~isnan(Y), 1).';
        Keep = NValid >= Args.MinEpochs;
        if IsAng
            % Faint cut on RefMag for angular quantities, std -> arcsec.
            if isfield(Msi.Data, Args.RefMag)
                Rm = median(Msi.Data.(Args.RefMag), 1, 'omitnan').';
                Keep = Keep & (Rm <= Args.BackgroundMag);
            else
                Rm = M;        % fall back so X is well-defined
            end
            S = S * 3600;
            if strcmpi(Q, 'RA') && isfield(Msi.Data, 'Dec')
                Dm = median(Msi.Data.Dec, 1, 'omitnan').';
                S  = S .* cosd(Dm);
            end
            X = Rm;
        else
            X = M;
        end
        Med = [Med; X(Keep)];     %#ok<AGROW>
        Std = [Std; S(Keep)];     %#ok<AGROW>
    end
end

% =========================================================================
function [Bx, By, Bs, Bn] = binStats(X, Y, BinWidth, IsLog)
    if nargin < 4; IsLog = false; end
    Bx = []; By = []; Bs = []; Bn = [];
    if isempty(X); return; end
    X = X(:); Y = Y(:);
    Fin = isfinite(X) & isfinite(Y);
    if IsLog; Fin = Fin & X > 0; end
    X = X(Fin); Y = Y(Fin);
    if isempty(X); return; end
    if IsLog
        % ~25 log-spaced bins spanning the data range.
        Edges = logspace(log10(min(X)), log10(max(X)), 25);
        Edges = unique(Edges);
    else
        Lo = floor(min(X) / BinWidth) * BinWidth;
        Hi = ceil(max(X)  / BinWidth) * BinWidth;
        Edges = Lo:BinWidth:Hi;
    end
    if numel(Edges) < 2; return; end
    [N, ~, BinIdx] = histcounts(X, Edges);
    Nb = numel(Edges) - 1;
    Bx = nan(Nb, 1); By = nan(Nb, 1); Bs = nan(Nb, 1); Bn = zeros(Nb, 1);
    for I = 1:Nb
        if N(I) < 5; continue; end
        Sel = (BinIdx == I);
        if IsLog
            Bx(I) = sqrt(Edges(I) * Edges(I+1));      % geometric mean
        else
            Bx(I) = 0.5 * (Edges(I) + Edges(I+1));    % arithmetic mean
        end
        By(I) = median(Y(Sel));
        Bs(I) = std(Y(Sel));
        Bn(I) = N(I);
    end
    Keep = ~isnan(By);
    Bx = Bx(Keep); By = By(Keep); Bs = Bs(Keep); Bn = Bn(Keep);
end
