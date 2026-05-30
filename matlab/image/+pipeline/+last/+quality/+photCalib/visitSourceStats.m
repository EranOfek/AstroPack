function [Result, Fig] = visitSourceStats(MS, Args)
    % Per-visit source-count and epoch-distribution statistics.
    % Description: Inventory of a single visit's sources: how many coadd
    %              detections, how many regular vs forced rows in the
    %              MatchedSources, and (for regular sources only by
    %              default; forced included as a secondary column) the
    %              distribution of detection counts across epochs. All
    %              quantities are reported BOTH summed over all crops and
    %              broken down per crop.
    %
    % Input  : - MS - 1xNcrop MatchedSources array, or a cell of
    %            MatchedSources (one element per crop). Each element must
    %            carry Data.(ForcedField) and Data.(NepochField).
    %          * ...,key,val,...
    %            'Coadd'        - Optional 1xNcrop AstroImage (or
    %                             AstroCatalog) array carrying the visit's
    %                             coadd source catalog. When present, the
    %                             Ncoadd per-crop counts are filled in
    %                             from size(.CatData.Catalog, 1) (or
    %                             size(.Catalog, 1) for AstroCatalog).
    %                             Default [] (Ncoadd remains 0).
    %            'ForcedField'  - Per-source field holding the forced/
    %                             regular flag. Default 'FORCED'.
    %            'NepochField'  - Per-(epoch,source) field whose finite
    %                             values are counted to determine "epochs
    %                             this source was detected in". Default
    %                             'MAG_PSF'. Use a column that's NaN
    %                             when the source is not detected in
    %                             that epoch (MAG_PSF / RA / FLUX_PSF
    %                             all work).
    %            'EpochEdges'   - Histogram edges for the epoch-count
    %                             distribution. Default
    %                             0.5:1:(max(20,Nepoch)+0.5), so bin
    %                             centres land on 1,2,...
    %            'Plot'         - Render a 2-panel summary figure
    %                             (per-crop bars + epoch histogram).
    %                             Default false.
    %            'Verbose'      - Print a tabular summary. Default false.
    %
    % Output : - Result - struct with the following fields. "Total" values
    %            are scalars (summed across crops); per-crop arrays are
    %            length Ncrop.
    %              .Ncrop          number of crops
    %              .Ncoadd         [1,Ncrop] sources in coadd catalog
    %              .NcoaddTotal    sum of Ncoadd
    %              .Nforced        [1,Ncrop] forced-phot rows in MS
    %              .NforcedTotal   sum of Nforced
    %              .Nregular       [1,Ncrop] regular rows in MS
    %              .NregularTotal  sum of Nregular
    %              .EpochEdges     histogram edges actually used
    %              .EpochCenters   bin centers (length Nbin)
    %              .EpochDistRegular [Nbin,Ncrop] # regular sources per
    %                             epoch-count bin per crop
    %              .EpochDistRegularTotal [Nbin,1] summed across crops
    %              .EpochDistForced [Nbin,Ncrop] same for forced
    %              .EpochDistForcedTotal  [Nbin,1] summed
    %              .Summary       MATLAB table: Crop, Ncoadd, Nforced,
    %                             Nregular per row, one row per crop
    %              .EpochTable    MATLAB table: EpochBin, TotalRegular,
    %                             TotalForced, Crop01...CropNN columns of
    %                             the regular distribution
    %              .Args          the args struct actually used
    %          - Fig    - figure handle when Plot=true ([] otherwise).
    % Author : D. Kovaleva (May 2026)
    % Example:
    %   % loadVisit already returns the merged MS -- no need to rebuild it
    %   % via matchEpochs (which re-runs the cross-match from scratch).
    %   [~, Coadd, MS] = pipeline.last.load.loadVisit(VisitPath, ...
    %                       'TempName_IndivIm', []);
    %   R = pipeline.last.quality.photCalib.visitSourceStats(MS, ...
    %             'Coadd', Coadd, 'Verbose', true, 'Plot', true);
    %   disp(R.Summary);
    %   disp(R.EpochTable);

    arguments
        MS
        Args.Coadd                                = []
        Args.ForcedField {mustBeTextScalar}       = 'FORCED'
        Args.NepochField {mustBeTextScalar}       = 'MAG_PSF'
        Args.EpochEdges  (1,:) double             = []
        Args.Plot        logical                  = false
        Args.Verbose     logical                  = false
    end

    Fig = [];

    % --- Normalise MS to a cell of crops ----------------------------------
    if isa(MS, 'MatchedSources')
        MSc = num2cell(MS(:).');
    elseif iscell(MS)
        MSc = MS(:).';
    else
        error('visitSourceStats:BadInput', ...
            'MS must be a MatchedSources array or a cell of MatchedSources.');
    end
    Ncrop = numel(MSc);

    Ncoadd       = zeros(1, Ncrop);
    Nforced      = zeros(1, Ncrop);
    Nregular     = zeros(1, Ncrop);
    EpochsReg    = cell(1, Ncrop);   % epoch counts per regular source
    EpochsFor    = cell(1, Ncrop);   % epoch counts per forced source
    NepochMax    = 0;

    for Ic = 1:Ncrop
        MSk = MSc{Ic};

        % Coadd source count for this crop
        if ~isempty(Args.Coadd) && Ic <= numel(Args.Coadd)
            AI = Args.Coadd(Ic);
            if isa(AI, 'AstroImage') && ~isempty(AI.CatData) && ...
               ~isempty(AI.CatData.Catalog)
                Ncoadd(Ic) = size(AI.CatData.Catalog, 1);
            elseif isa(AI, 'AstroCatalog') && ~isempty(AI.Catalog)
                Ncoadd(Ic) = size(AI.Catalog, 1);
            end
        end

        if isempty(MSk) || ~isfield(MSk.Data, Args.ForcedField); continue; end
        NepochMax = max(NepochMax, MSk.Nepoch);

        % Per-source FORCED flag — first finite value down each column
        F = MSk.Data.(Args.ForcedField);
        Fsrc = nan(1, MSk.Nsrc);
        for Is = 1:MSk.Nsrc
            v = F(:,Is);  v = v(~isnan(v));
            if ~isempty(v);  Fsrc(Is) = v(1);  end
        end
        IsForced  = (Fsrc == 1);
        IsRegular = (Fsrc == 0);
        Nforced(Ic)  = nnz(IsForced);
        Nregular(Ic) = nnz(IsRegular);

        % Per-source epoch counts (finite entries in NepochField)
        if isfield(MSk.Data, Args.NepochField)
            Y  = MSk.Data.(Args.NepochField);
            ep = sum(~isnan(Y), 1);   % 1 x Nsrc
            EpochsReg{Ic} = ep(IsRegular);
            EpochsFor{Ic} = ep(IsForced);
        end
    end

    % --- Epoch-distribution histogram -------------------------------------
    if isempty(Args.EpochEdges)
        Edges = 0.5 : 1 : (max(20, NepochMax) + 0.5);
    else
        Edges = Args.EpochEdges(:).';
    end
    Nbin    = numel(Edges) - 1;
    Centers = 0.5 * (Edges(1:end-1) + Edges(2:end));

    EpDistReg = zeros(Nbin, Ncrop);
    EpDistFor = zeros(Nbin, Ncrop);
    for Ic = 1:Ncrop
        if ~isempty(EpochsReg{Ic})
            EpDistReg(:, Ic) = histcounts(EpochsReg{Ic}, Edges).';
        end
        if ~isempty(EpochsFor{Ic})
            EpDistFor(:, Ic) = histcounts(EpochsFor{Ic}, Edges).';
        end
    end

    % --- Assemble Result --------------------------------------------------
    Result = struct();
    Result.Ncrop           = Ncrop;
    Result.Ncoadd          = Ncoadd;
    Result.NcoaddTotal     = sum(Ncoadd);
    Result.Nforced         = Nforced;
    Result.NforcedTotal    = sum(Nforced);
    Result.Nregular        = Nregular;
    Result.NregularTotal   = sum(Nregular);
    Result.EpochEdges      = Edges;
    Result.EpochCenters    = Centers;
    Result.EpochDistRegular      = EpDistReg;
    Result.EpochDistRegularTotal = sum(EpDistReg, 2);
    Result.EpochDistForced       = EpDistFor;
    Result.EpochDistForcedTotal  = sum(EpDistFor, 2);

    Result.Summary = table((1:Ncrop).', Ncoadd.', Nforced.', Nregular.', ...
        'VariableNames', {'Crop','Ncoadd','Nforced','Nregular'});

    CropCols = arrayfun(@(c) sprintf('Crop%02d', c), 1:Ncrop, 'Uni', 0);
    Result.EpochTable = array2table( ...
        [Centers(:), sum(EpDistReg,2), sum(EpDistFor,2), EpDistReg], ...
        'VariableNames', [{'EpochBin','TotalRegular','TotalForced'}, CropCols]);

    Result.Args = Args;

    % --- Verbose summary --------------------------------------------------
    if Args.Verbose
        fprintf('visitSourceStats: %d crops\n', Ncrop);
        fprintf('  Ncoadd:   total=%d (mean/crop=%.1f, max=%d)\n', ...
            sum(Ncoadd), mean(Ncoadd), max(Ncoadd));
        fprintf('  Nforced:  total=%d (mean/crop=%.1f, max=%d)\n', ...
            sum(Nforced), mean(Nforced), max(Nforced));
        fprintf('  Nregular: total=%d (mean/crop=%.1f, max=%d)\n', ...
            sum(Nregular), mean(Nregular), max(Nregular));
        fprintf('  Regular-source epoch distribution (all crops pooled):\n');
        for I = 1:Nbin
            n = Result.EpochDistRegularTotal(I);
            if n > 0
                fprintf('    %4.1f epoch(s): %6d sources\n', Centers(I), n);
            end
        end
    end

    % --- Optional plot ----------------------------------------------------
    if Args.Plot
        Fig = figure('Position', [80 80 1100 380]);
        tiledlayout(1, 2);

        % Panel 1: per-crop bars
        nexttile;
        Stk = [Ncoadd(:), Nforced(:), Nregular(:)];
        bar(1:Ncrop, Stk, 'grouped');
        xlabel('Crop'); ylabel('# sources');
        legend({'Coadd','Forced','Regular'}, 'Location', 'best');
        grid on;
        title(sprintf('Per-crop counts (totals: coadd=%d, forced=%d, reg=%d)', ...
            sum(Ncoadd), sum(Nforced), sum(Nregular)));

        % Panel 2: epoch-count histogram (regular vs forced)
        nexttile;
        bar(Centers, [Result.EpochDistRegularTotal, Result.EpochDistForcedTotal], ...
            'grouped');
        xlabel('# epochs in which source was detected');
        ylabel('# sources');
        legend({'Regular','Forced'}, 'Location', 'best');
        grid on;
        title(sprintf('Epoch distribution (NepochField=%s)', Args.NepochField), ...
            'Interpreter', 'none');
    end
end
