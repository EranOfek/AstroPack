function [JointCat, JointHeader, AI] = joinCropsToCatalog(AI, Args)
    % Build a single full-frame AstroCatalog from an N-crop AstroImage array.
    % Runs 5 steps: (1) add XFULL/YFULL via imProc.cat.addXYfull, (2) per-crop
    % metadata stamping (CropID, AIRMASS), (3) vertcat of per-crop tables
    % into one field-wide table with NaN-fill for any missing columns,
    % (4) overlap-dedup by RA/Dec (keep highest SN), (5) wrap into a scalar
    % AstroCatalog. The (Row, Col) grid position is not stamped — it's
    % derivable from CropID + TileOrder + Nrows/Ncols via
    % PhotCalibTrans.cropID2RowCol at any time, so storing it would be
    % redundant.
    %
    % Crops with empty CatData or missing ORIGSEC are silently skipped so the
    % function keeps working when a single sub-image failed upstream.
    % Input  : - AI - 1xN AstroImage array (the crops of one exposure).
    %                 CropID stamped on each row = the input-array index I,
    %                 so the caller's array ordering is what the CropID
    %                 column reflects. If a caller needs the (Row, Col)
    %                 grid position of a crop, call
    %                 PhotCalibTrans.cropID2RowCol(CropID, Nrows, Ncols,
    %                 TileOrder) at read time — no stamping here.
    %          * ...,key,val,...
    %            'Dedup'        - logical. Default true. When false, skip the
    %                             overlap-dedup step entirely: every crop-local
    %                             row survives, even the ones that appear in
    %                             more than one crop. Needed by workflows that
    %                             later propagate the joint calibration result
    %                             back to per-crop AI(i).CatData rows.
    %            'DedupRadius'  - overlap-dedup radius [arcsec]. Default 1.0.
    %            'SNColName'    - column used to rank duplicates within a
    %                             cluster. Default 'SN'. Falls back to
    %                             first-encountered if the column is absent
    %                             or all-NaN.
    %            'KeyCCDSEC'    - HeaderData key for CCDSEC. Default 'ORIGSEC'.
    %            'KeyAirmass'   - HeaderData key for AIRMASS. Default 'AIRMASS'.
    %            'CreateNewObj' - logical, forwarded to addXYfull. Default
    %                             true so the caller's array is not mutated.
    %            'Verbose'      - logical. Default false.
    % Output : - JointCat    - scalar AstroCatalog. Columns = union of every
    %                          per-crop CatData column, plus XFULL, YFULL,
    %                          CropID, AIRMASS. JointCat.JD is populated
    %                          from the first non-empty crop's header.
    %          - JointHeader - AstroHeader (a copy of AI(FirstNonEmpty).
    %                          HeaderData). Fits the calibrate('Metadata')
    %                          slot so downstream calibration can read
    %                          ExpTime, Pressure, Temp, ObsTime etc.
    %                          Empty if no crop had a header. Caller passes
    %                          this to fitPhotCalibTrans via 'Metadata' in
    %                          the CalibArgs cell.
    %          - AI          - the AstroImage array as returned by addXYfull.
    %                          CreateNewObj=true (default) -> a fresh copy;
    %                          the caller's input remains untouched.
    % Author : D. Kovaleva (July 2026).
    % Example: JCat = imProc.cat.joinCropsToCatalog(AI);
    %          [JCat, ~] = imProc.cat.joinCropsToCatalog(AI, ...
    %              'DedupRadius', 1.5, 'Verbose', true);

    arguments
        AI
        Args.Dedup          logical = true
        Args.DedupRadius    (1,1) double = 1.0
        Args.SNColName      char    = 'SN'
        Args.KeyCCDSEC      char    = 'ORIGSEC'
        Args.KeyAirmass     char    = 'AIRMASS'
        Args.CreateNewObj   logical = true
        Args.Verbose        logical = false
    end

    Ncrops = numel(AI);
    JointHeader = AstroHeader.empty;
    if Ncrops == 0
        JointCat = AstroCatalog;
        return;
    end

    % --- Step 1: add XFULL/YFULL to every crop's CatData -------------------
    [~, AI] = imProc.cat.addXYfull(AI, ...
        'CreateNewObj', Args.CreateNewObj, ...
        'KeyCCDSEC',    Args.KeyCCDSEC);

    % --- Step 2: per-crop metadata stamping --------------------------------
    TabCell = cell(Ncrops, 1);
    for I = 1:Ncrops
        CatData = AI(I).CatData;
        Tab = CatData.Table;
        if isempty(Tab) || height(Tab) == 0
            if Args.Verbose
                fprintf('  Crop %d: empty CatData, skipping\n', I);
            end
            continue;
        end

        % Airmass from header (NaN-fallback if the key is missing)
        try
            AirmassVal = double(AI(I).HeaderData.getVal(Args.KeyAirmass));
        catch
            AirmassVal = NaN;
            if Args.Verbose
                fprintf('  Crop %d: missing %s in header, filling AIRMASS with NaN\n', ...
                    I, Args.KeyAirmass);
            end
        end
        if ~isscalar(AirmassVal) || ~isfinite(AirmassVal)
            AirmassVal = NaN;
        end

        Nrow = height(Tab);
        Tab.CropID  = repmat(double(I),          Nrow, 1);
        Tab.AIRMASS = repmat(double(AirmassVal), Nrow, 1);
        TabCell{I} = Tab;
    end

    NonEmpty = ~cellfun(@isempty, TabCell);
    if ~any(NonEmpty)
        if Args.Verbose
            fprintf('  All crops empty -> returning empty AstroCatalog\n');
        end
        JointCat = AstroCatalog;
        return;
    end

    % --- Step 3: vertcat with NaN-fill for schema mismatches ---------------
    Cells = TabCell(NonEmpty);
    AllCols = Cells{1}.Properties.VariableNames;
    for K = 2:numel(Cells)
        AllCols = union(AllCols, Cells{K}.Properties.VariableNames, 'stable');
    end
    MismatchReport = {};
    for K = 1:numel(Cells)
        VN = Cells{K}.Properties.VariableNames;
        Missing = setdiff(AllCols, VN, 'stable');
        if ~isempty(Missing)
            MismatchReport{end+1, 1} = sprintf( ...
                'crop-cell %d missing: %s', K, strjoin(Missing, ', ')); %#ok<AGROW>
            for MI = 1:numel(Missing)
                Cells{K}.(Missing{MI}) = nan(height(Cells{K}), 1);
            end
        end
        % Reorder to canonical order so vertcat aligns cleanly
        Cells{K} = Cells{K}(:, AllCols);
    end
    if ~isempty(MismatchReport) && Args.Verbose
        fprintf('  Schema mismatch across crops - NaN-filled:\n');
        for MI = 1:numel(MismatchReport)
            fprintf('    %s\n', MismatchReport{MI});
        end
    end
    TabAll = vertcat(Cells{:});
    N = height(TabAll);

    % --- Step 4: dedup overlap sources (highest SN wins per cluster) -------
    VNAll = TabAll.Properties.VariableNames;
    if Args.Dedup && N > 1 && ismember('RA', VNAll) && ismember('Dec', VNAll)
        RA_deg  = double(TabAll.RA);
        Dec_deg = double(TabAll.Dec);
        HasSky  = isfinite(RA_deg) & isfinite(Dec_deg);
        Nsky    = sum(HasSky);

        if Nsky > 1
            SkyIdx  = find(HasSky);
            RA_sky  = RA_deg(SkyIdx);
            Dec_sky = Dec_deg(SkyIdx);

            % Rank order within the sky-valid subset (highest SN first).
            % SN column missing / all-NaN -> fall back to input order.
            if ismember(Args.SNColName, VNAll)
                SNvec = double(TabAll.(Args.SNColName)(SkyIdx));
                SNvec(~isfinite(SNvec)) = -inf;
                [~, Ord] = sort(SNvec, 'descend');
            else
                Ord = (1:Nsky).';
            end

            % Build the KDTree over SN-sorted positions so that cone-search
            % index space matches the greedy-NMS walk order below.
            % celestial.KDTreeCoo handles the unit-sphere Cartesian
            % projection + range-search internally (chord vs great-circle
            % is negligible at arcsec-scale radii — the two agree to below
            % double-precision noise for radii <~ a few arcmin).
            KT = celestial.KDTreeCoo;
            KT = KT.populate(RA_sky(Ord), Dec_sky(Ord), 'InUnits', 'deg');
            NbrIdxCell = KT.coneSearch(RA_sky(Ord), Dec_sky(Ord), ...
                Args.DedupRadius, 'RadiusUnits', 'arcsec', 'InUnits', 'deg');

            % Greedy non-maximum suppression: walk sorted list from
            % highest-SN; each surviving source kills all direct
            % neighbours within DedupRadius. Highest-SN wins per cluster.
            KeepInSort = true(Nsky, 1);
            for J = 1:Nsky
                if ~KeepInSort(J); continue; end
                Dups = NbrIdxCell{J};
                Dups(Dups == J) = [];
                KeepInSort(Dups) = false;
            end

            % Map back to full-table indices; sources with NaN RA/Dec pass
            % through untouched (no matching possible).
            SurviveSky = SkyIdx(Ord(KeepInSort));
            KeepMask = false(N, 1);
            KeepMask(SurviveSky) = true;
            KeepMask(~HasSky)    = true;

            NRemoved = N - sum(KeepMask);
            if Args.Verbose
                fprintf('  Dedup: %d overlap duplicates removed (radius=%.2f arcsec, SN column=%s)\n', ...
                    NRemoved, Args.DedupRadius, Args.SNColName);
            end
            TabAll = TabAll(KeepMask, :);
        end
    elseif Args.Verbose
        if ~Args.Dedup
            fprintf('  Dedup disabled (Dedup=false) -> all %d rows kept\n', N);
        elseif ~ismember('RA', VNAll) || ~ismember('Dec', VNAll)
            fprintf('  No RA/Dec on pooled table -> dedup skipped\n');
        end
    end

    % --- Step 5: wrap into AstroCatalog ------------------------------------
    % Store Catalog as MATRIX + separate ColNames (matches per-crop LAST
    % CatData convention). Using a table here would trip a latent bug in
    % AstroTable.insertColumn -> table hconcat that mislabels inserted
    % columns as 'NewData' and collides on the second insert.
    JointCat          = AstroCatalog;
    JointCat.Catalog  = table2array(TabAll);
    JointCat.ColNames = TabAll.Properties.VariableNames;

    % --- Step 6: image-level metadata --------------------------------------
    % (a) JD onto the catalog: one value for the whole visit, identical
    %     across all crops. Read from the first non-empty crop's header.
    % (b) JointHeader = a copy of that same crop's HeaderData, so the caller
    %     can hand it to fitPhotCalibTrans / PhotCalibTrans.calibrate via
    %     the 'Metadata' arg (that path reads ExpTime, NCoadd, AirMass,
    %     Pressure, Temp, etc. off an AstroHeader). All 24 crops share the
    %     same header values within one visit, so the first suffices.
    FirstNonEmpty = find(NonEmpty, 1, 'first');
    if ~isempty(FirstNonEmpty)
        Hdr = AI(FirstNonEmpty).HeaderData;
        try
            JD_val = Hdr.getVal('JD');
            if isnumeric(JD_val) && isscalar(JD_val) && isfinite(JD_val)
                JointCat.JD = JD_val;
            end
        catch
            % JD missing / unreadable — leave JointCat.JD at default.
        end
        try
            JointHeader = Hdr.copy();
        catch
            % Header uncopyable (unlikely) — leave JointHeader empty.
        end
    end

    if Args.Verbose
        fprintf('  Joint catalog: %d sources across %d crops (post-dedup), JD=%.5f\n', ...
            height(TabAll), sum(NonEmpty), JointCat.JD);
    end
end
