function Report = overlapFlagTally(Input, Args)
    % Tally the 'Overlap' mask bit over cross-matched crop-pair sources.
    % Description: For each neighbour interface of LAST's 24-crop mosaic,
    %   cross-matches the two crops' catalogs by RA/Dec (exactly the pairing
    %   pipeline.last.quality.overlapSources uses), then decodes the
    %   'Overlap' mask bit in BOTH matched copies and counts how many pairs
    %   carry it in BOTH crops / exactly ONE crop / NEITHER. This answers
    %   "do the two copies of a seam-crossing star both carry the Overlap
    %   flag?" without touching pixel positions or the flag itself during
    %   matching.
    %
    %   With 'KeepSources' (default false) it additionally records one row
    %   per matched pair into Report.Sources: for BOTH members it stores the
    %   crop number, native X/Y, full-frame XFULL/YFULL (computed via the
    %   canonical imProc.cat.addXYfull from each crop's ORIGSEC; NaN when the
    %   header lacks it), RA/Dec, and the Overlap bit as 0/1. Use
    %   'KeepOnlyFlagged' to keep only pairs flagged in at least one crop.
    %
    %   Interpretation. Under the current cropping partition each crop marks
    %   'Overlap' on everything OUTSIDE its own unique (no-overlap) section,
    %   i.e. it flags every source it does not own. Those sections tile the
    %   full frame with no gaps (for the LAST grid they are exactly the
    %   Voronoi cells of the crop centres), so every source is owned by
    %   exactly one crop and flagged by all the others.
    %
    %   NBoth is therefore NOT expected to vanish: the per-pair tally has a
    %   floor set by the geometry, from two contributions -
    %     - 4-crop corners: a source owned by a THIRD crop is correctly
    %       flagged in both members of the pair. For the standard frame
    %       ([6388 9600] cropped into 4x6 subimages of 1716^2) this is 6.9%
    %       of the matched pairs.
    %     - the FLAGS cutout: catalog FLAGS are ORed over a
    %       (2*FlagHalfSize+1)^2 box (imProc.sources.findMeasureSources,
    %       FlagHalfSize=3 by default), which dilates the flagged region
    %       across the partition line and adds a further ~4%.
    %   So for the standard LAST geometry the floor is FracBoth ~ 0.11, and a
    %   measured value near it indicates a CORRECT flag. A materially LARGER
    %   fraction (0.15-0.20, seen before issue #1183 was fixed) is the
    %   signature of the coadd inheriting the bit from the dithered
    %   single-epoch masks, or of an older scheme that set 'Overlap'
    %   symmetrically in both crops.
    %
    %   NNeither, in contrast, IS expected to be exactly 0: the unique
    %   sections tile the frame, so at least one member of every matched pair
    %   is a non-owner, and the FLAGS cutout only ever adds flags.
    %
    %   Because of the floor the per-pair tally has no achievable perfect
    %   score. The metric that does is per SOURCE rather than per pair:
    %   exactly one crop leaves each source clean, i.e. NFlagged == NCopies-1
    %   over all the crops covering it.
    %
    %   The Overlap bit is NEVER used to pair sources or to filter them -
    %   matching is purely positional (RA/Dec), so the tally is an unbiased
    %   census of the flag state on the matched pairs.
    %
    % Input  : - Input, one of:
    %            * An AstroImage array of ONE visit's crops, element i = crop
    %              i (1..NCrops), each with CatData populated (RA, Dec, FLAGS,
    %              the mag column, and X/Y for KeepSources). HeaderData with
    %              ORIGSEC is used for XFULL/YFULL when present.
    %            * A char/string directory path. Discovers coadd products
    %              (FilePattern, recursive by default), groups them into
    %              visits by the LAST filename stem, loads each visit's crops
    %              (via AstroImage.readProducts ExtraOutProduct "Cat"), tallies
    %              per visit and aggregates across visits.
    %          * ...,key,val,...
    %            'CroppingScheme' - 'new' (default) | 'old'. Passed to
    %                               pipeline.last.quality.LASToverlaps to get
    %                               the interface table.
    %            'MatchRadius'    - Cross-match radius [arcsec]. Default 1.
    %            'MagRange'       - [lo hi] window on the mag column used to
    %                               select matched pairs. Default [-Inf Inf]
    %                               (ALL matched pairs - the flag question is
    %                               geometric, not photometric). Narrow it to
    %                               restrict the census to a mag range.
    %            'OverlapBitName' - Mask bit name to tally. Default 'Overlap'.
    %            'ColFlags'       - FLAGS column name. Default 'FLAGS'.
    %            'ColMag'         - Mag column (match/pair gate). Default
    %                               'MAG_APER_3'.
    %            'ColX'           - Native X column (for KeepSources).
    %                               Default 'X'.
    %            'ColY'           - Native Y column (for KeepSources).
    %                               Default 'Y'.
    %            'KeyCCDSEC'      - Header key giving the crop's full-frame
    %                               section, used for XFULL/YFULL. Default
    %                               'ORIGSEC'.
    %            'KeepSources'    - Record per-pair source rows into
    %                               Report.Sources. Default false (memory:
    %                               one row per matched pair per visit).
    %            'KeepOnlyFlagged'- When KeepSources, keep only pairs flagged
    %                               in at least one crop. Default false.
    %            'AllColumns'     - Store EVERY catalog column of both members
    %                               in Report.Sources instead of the curated
    %                               set. Each crop-1 column is suffixed '_1',
    %                               each crop-2 column '_2', preceded by the
    %                               metadata columns Visit, Interface, Crop1,
    %                               Crop2, Ovlp1, Ovlp2. Lets you do stats on
    %                               any quantity (MAG/FLUX/SN/...) for the
    %                               both/one/none sets. Implies KeepSources.
    %                               Columns absent in a crop are NaN-filled.
    %                               Default false.
    %            'BitDictName'    - BitDictionary name. Default
    %                               'BitMask.Image.Default'.
    %            'NCrops'         - Crop count per visit. Default 24.
    %            'FilePattern'    - (path mode) dir() glob for the discovery
    %                               product. Default
    %                               'LAST*_sci_coadd_Image_1.fits'.
    %            'Recursive'      - (path mode) walk sub-tree. Default true.
    %            'FieldId'        - (path mode) keep files with _<FieldId>_ in
    %                               the basename. Default '' (no filter).
    %            'Filter'         - (path mode) keep files with _<Filter>_.
    %                               Default '' (no filter).
    %            'MaxVisits'      - (path mode) cap. Default Inf.
    %            'Verbose'        - Print progress + summary. Default true.
    %            'ExpectedFracBoth' - The geometric floor of FracBoth for the
    %                               geometry at hand (see Interpretation
    %                               above): the fraction of matched pairs
    %                               flagged in BOTH crops even when the flag
    %                               is correct. Quoted in the summary, and a
    %                               measured FracBoth exceeding it by more
    %                               than 'FracBothTol' is reported as
    %                               suspicious. 0.11 is the value for the
    %                               standard LAST grid ([6388 9600] cropped
    %                               into 4x6 subimages of 1716^2, with
    %                               FlagHalfSize=3); other geometries need
    %                               their own floor. Set to NaN to skip the
    %                               check. Default is 0.11.
    %            'FracBothTol'    - Tolerance above 'ExpectedFracBoth' before
    %                               the summary reports the run as
    %                               suspicious. Default is 0.02.
    % Output : - Report struct with fields:
    %            .CropPairs   - [Ninterface x 2] crop-index pairs (from
    %                           LASToverlaps); columns are (crop1, crop2) and
    %                           the NCrop1/NCrop2 tallies follow this order.
    %            .PerInterface - struct of 1xNinterface vectors:
    %                .NMatched .NBoth .NCrop1 .NCrop2 .NNeither .NVisits
    %                (NVisits = number of visits in which the interface was
    %                 evaluable; 1 for the AstroImage-array form).
    %            .Pool        - scalar totals over all interfaces (and visits):
    %                .NMatched .NBoth .NCrop1 .NCrop2 .NNeither .NOne
    %                .FracBoth .FracOne .FracNone .NInterfaces .NVisits
    %            .Sources     - (KeepSources) table, one row per matched pair:
    %                Visit, Interface, Crop1, SrcNum1, X1, Y1, XFULL1, YFULL1,
    %                RA1, Dec1, Ovlp1, Crop2, SrcNum2, X2, Y2, XFULL2, YFULL2,
    %                RA2, Dec2, Ovlp2. Subscript 1 = first crop of the
    %                interface pair, 2 = second. SrcNum* = 1-based row index
    %                of the source within its own crop catalog. Ovlp* are 0/1.
    %                Empty table when KeepSources=false. With 'AllColumns' the
    %                curated columns are replaced by the full per-member
    %                catalog (every column suffixed _1/_2) plus the metadata
    %                Visit, Interface, Crop1, Crop2, Ovlp1, Ovlp2; the in-crop
    %                source index is then CROPSRCIDX_1/_2 and the full-frame
    %                coordinates XFULL_1/YFULL_1 etc.
    %            .PerVisit    - (path mode) 1xNvisit struct: .Stem,
    %                           .NCropsPresent, .NMatched, .NBoth, .NCrop1,
    %                           .NCrop2, .NNeither.
    %            .BitName .BitIndex .CroppingScheme .Args
    % Author : D. Kovaleva (Jul 2026)
    % See also: pipeline.last.quality.overlapSources,
    %           pipeline.last.quality.LASToverlaps,
    %           pipeline.last.quality.photCalib.batchOverlapSources.
    % Example:
    %   % --- One loaded visit (AI = 1x24 crops with CatData):
    %   R = pipeline.last.quality.masks.overlapFlagTally(AI);
    %   fprintf('both=%.1f%%\n', 100*R.Pool.FracBoth);
    %
    %   % --- Keep per-source detail (native X/Y, XFULL/YFULL, RA/Dec, 0/1):
    %   R = pipeline.last.quality.masks.overlapFlagTally(AI, 'KeepSources', true);
    %   S = R.Sources;                    % table, one row per matched pair
    %
    %   % --- All visits of a field under a tree:
    %   R = pipeline.last.quality.masks.overlapFlagTally( ...
    %           '/euclid/last/data/LAST.01.05.03/2025/06', ...
    %           'FieldId','1716.c', 'Filter','clear', 'MaxVisits',20);
    arguments
        Input
        Args.CroppingScheme (1,:) char {mustBeMember(Args.CroppingScheme,{'new','old'})} = 'new'
        Args.MatchRadius    (1,1) double = 1
        Args.MagRange       (1,2) double = [-Inf Inf]
        Args.OverlapBitName (1,:) char = 'Overlap'
        Args.ColFlags       (1,:) char = 'FLAGS'
        Args.ColMag         (1,:) char = 'MAG_APER_3'
        Args.ColX           (1,:) char = 'X'
        Args.ColY           (1,:) char = 'Y'
        Args.KeyCCDSEC      (1,:) char = 'ORIGSEC'
        Args.KeepSources          logical = false
        Args.KeepOnlyFlagged      logical = false
        Args.AllColumns           logical = false
        Args.BitDictName    (1,:) char = 'BitMask.Image.Default'
        Args.NCrops         (1,1) double {mustBePositive, mustBeInteger} = 24
        Args.FilePattern    (1,:) char = 'LAST*_sci_coadd_Image_1.fits'
        Args.Recursive            logical = true
        Args.FieldId        (1,:) char = ''
        Args.Filter         (1,:) char = ''
        Args.MaxVisits      (1,1) double = Inf
        Args.Verbose              logical = true
        Args.ExpectedFracBoth (1,1) double = 0.11
        Args.FracBothTol      (1,1) double = 0.02
    end

    % --- Interface table + Overlap bit value -----------------------------
    Ind = pipeline.last.quality.LASToverlaps('CroppingScheme', Args.CroppingScheme);
    Nifc = size(Ind, 1);

    BD = BitDictionary(Args.BitDictName);
    [BitInd, ~] = BD.name2bit({Args.OverlapBitName});
    BitInd = double(BitInd(1));
    if isnan(BitInd)
        error('pipeline:last:quality:masks:overlapFlagTally:BadBit', ...
              'Bit name "%s" not found in dictionary %s', ...
              Args.OverlapBitName, Args.BitDictName);
    end
    BitVal = bitshift(uint32(1), uint32(BitInd));

    % AllColumns implies KeepSources (it only shapes the Sources table).
    if Args.AllColumns
        Args.KeepSources = true;
    end

    % Per-interface accumulators (summed over visits).
    Acc = struct('NMatched', zeros(1,Nifc), 'NBoth', zeros(1,Nifc), ...
                 'NCrop1', zeros(1,Nifc), 'NCrop2', zeros(1,Nifc), ...
                 'NNeither', zeros(1,Nifc), 'NVisits', zeros(1,Nifc));
    PerVisit = struct('Stem',{},'NCropsPresent',{},'NMatched',{}, ...
                      'NBoth',{},'NCrop1',{},'NCrop2',{},'NNeither',{});
    SrcAll  = i_emptySrc();   % curated per-pair rows (struct-of-arrays)
    SrcTabs = {};             % AllColumns: cell of per-interface pair tables

    % --- Dispatch: AstroImage array (one visit) vs path (many visits) ----
    if isa(Input, 'AstroImage')
        [Acc, VisRow, Src] = i_tallyVisit(Input, Ind, BitVal, Acc, 1, Args);
        VisRow.Stem = '(AstroImage input)';
        VisRow.NCropsPresent = numel(Input);
        PerVisit(1) = VisRow;
        if Args.AllColumns
            SrcTabs = [SrcTabs, Src];
        else
            SrcAll = i_appendSrc(SrcAll, Src);
        end
        if Args.Verbose
            i_printInterfaces(Ind, Acc);
        end
    elseif ischar(Input) || isstring(Input)
        BaseDir = char(Input);
        Visits = i_discoverVisits(BaseDir, Args.FilePattern, Args.Recursive, ...
                                  Args.FieldId, Args.Filter);
        if isempty(Visits)
            error('pipeline:last:quality:masks:overlapFlagTally:NoVisits', ...
                  'No visits matched pattern %s under %s', Args.FilePattern, BaseDir);
        end
        if isfinite(Args.MaxVisits) && Args.MaxVisits < numel(Visits)
            Visits = Visits(1:Args.MaxVisits);
        end
        NV = numel(Visits);
        if Args.Verbose
            fprintf('overlapFlagTally: %d visit(s) under %s\n', NV, BaseDir);
        end
        for IV = 1:NV
            Vis = Visits(IV);
            AI = i_loadVisitTile(Vis, Args.NCrops);
            [Acc, VisRow, Src] = i_tallyVisit(AI, Ind, BitVal, Acc, IV, Args);
            VisRow.Stem = Vis.Stem;
            VisRow.NCropsPresent = numel(Vis.Files);
            PerVisit(IV) = VisRow;
            if Args.AllColumns
                SrcTabs = [SrcTabs, Src]; %#ok<AGROW>
            else
                SrcAll = i_appendSrc(SrcAll, Src);
            end
            if Args.Verbose
                fprintf('[%d/%d] %s  %d crops  matched=%d  both=%d  one=%d  none=%d\n', ...
                    IV, NV, Vis.Stem, numel(Vis.Files), VisRow.NMatched, ...
                    VisRow.NBoth, VisRow.NCrop1+VisRow.NCrop2, VisRow.NNeither);
            end
        end
    else
        error('pipeline:last:quality:masks:overlapFlagTally:BadInput', ...
              'Input must be an AstroImage array or a directory path.');
    end

    % --- Assemble Report --------------------------------------------------
    Report.CropPairs      = Ind;
    Report.PerInterface   = Acc;
    Report.Pool           = i_poolFrom(Acc);
    if Args.AllColumns
        Report.Sources    = i_vertcatUnion(SrcTabs);
    else
        Report.Sources    = struct2table(SrcAll);
    end
    Report.PerVisit       = PerVisit;
    Report.BitName        = Args.OverlapBitName;
    Report.BitIndex       = BitInd;
    Report.CroppingScheme = Args.CroppingScheme;
    Report.Args           = Args;

    if Args.Verbose
        P = Report.Pool;
        Den = max(P.NMatched, 1);
        fprintf(['overlapFlagTally POOL: matched=%d over %d interface-visit(s)\n' ...
                 '  both=%d (%.2f%%)  one=%d (%.2f%%)  none=%d (%.2f%%)\n'], ...
                 P.NMatched, P.NVisits, ...
                 P.NBoth, 100*P.NBoth/Den, P.NOne, 100*P.NOne/Den, ...
                 P.NNeither, 100*P.NNeither/Den);
        % NBoth has a geometric floor (4-crop corners + the FLAGS cutout), so it
        % is not expected to vanish - see the Interpretation in the help. NNeither,
        % in contrast, must be exactly 0 for unique sections that tile the frame.
        if ~isnan(Args.ExpectedFracBoth)
            fprintf('  expected FracBoth floor = %.3f (4-crop corners + FlagHalfSize) ; measured = %.3f\n', ...
                     Args.ExpectedFracBoth, P.FracBoth);
            if P.FracBoth > Args.ExpectedFracBoth + Args.FracBothTol
                fprintf('  NB: FracBoth exceeds the floor by more than %.3f -> check the Overlap flagging.\n', Args.FracBothTol);
            end
        end
        if P.NNeither > 0
            fprintf('  NB: NNeither>0 -> %d pair(s) flagged in NEITHER crop; the unique sections should tile the frame.\n', P.NNeither);
        end
    end
end


% ==== Local helpers =====================================================

function [Acc, VisRow, Src] = i_tallyVisit(AI, Ind, BitVal, Acc, VisitIdx, Args)
    % Tally one visit's crops into the per-interface accumulator Acc, return
    % the visit's pooled counts VisRow and (if KeepSources) the per-pair
    % source rows Src (struct of equal-length column vectors).
    Nifc = size(Ind, 1);
    Nai  = numel(AI);
    ReqC1 = {Args.ColFlags, Args.ColMag, 'RA', 'Dec'};
    ReqC2 = {Args.ColFlags, 'RA', 'Dec'};
    Vn  = [0 0 0 0];    % [both crop1 crop2 neither] for this visit
    if Args.AllColumns
        Src = {};              % cell of per-interface pair tables
    else
        Src = i_emptySrc();    % struct-of-arrays (curated columns)
    end
    for K = 1:Nifc
        A = Ind(K,1);
        B = Ind(K,2);
        n = [0 0 0 0];
        nm = 0;
        Evaluable = false;
        if A <= Nai && B <= Nai
            C1 = AI(A).CatData;
            C2 = AI(B).CatData;
            HasData = ~isempty(C1) && ~isempty(C2) && ...
                      ~isempty(C1.Catalog) && ~isempty(C2.Catalog);
            if HasData
                VN1 = C1.Table.Properties.VariableNames;
                VN2 = C2.Table.Properties.VariableNames;
                if all(ismember(ReqC1, VN1)) && all(ismember(ReqC2, VN2))
                    c1 = C1.copy;
                    c2 = C2.copy;
                    if Args.KeepSources
                        % Stamp the in-crop source index and add XFULL/YFULL
                        % BEFORE matching, so MS carries crop-A's values
                        % aligned to the matched pairs (missing ORIGSEC ->
                        % XFULL/YFULL columns absent -> NaN downstream).
                        c1 = i_stampIndex(c1);
                        c2 = i_stampIndex(c2);
                        c1 = i_addXYfull(c1, AI(A).HeaderData, Args);
                        c2 = i_addXYfull(c2, AI(B).HeaderData, Args);
                    end
                    MS = imProc.match.match(c1, c2, 'Radius', Args.MatchRadius);
                    Mag = MS.Table.(Args.ColMag);
                    M = isfinite(Mag) & Mag >= Args.MagRange(1) & Mag <= Args.MagRange(2);
                    if any(M)
                        F1 = MS.Table.(Args.ColFlags)(M);  F1(isnan(F1)) = 0;
                        F2 = c2.Table.(Args.ColFlags)(M);  F2(isnan(F2)) = 0;
                        O1 = bitand(uint32(F1), BitVal) ~= 0;
                        O2 = bitand(uint32(F2), BitVal) ~= 0;
                        n  = [sum(O1 & O2), sum(O1 & ~O2), sum(~O1 & O2), sum(~O1 & ~O2)];
                        nm = sum(M);
                        Evaluable = true;
                        if Args.KeepSources
                            if Args.AllColumns
                                Src{end+1} = i_pairTable(MS.Table, c2.Table, ...
                                    M, O1, O2, A, B, K, VisitIdx, Args); %#ok<AGROW>
                            else
                                Rows = i_pairRows(MS.Table, c2.Table, M, O1, O2, ...
                                                  A, B, K, VisitIdx, Args);
                                Src = i_appendSrc(Src, Rows);
                            end
                        end
                    end
                end
            end
        end
        Acc.NMatched(K) = Acc.NMatched(K) + nm;
        Acc.NBoth(K)    = Acc.NBoth(K)    + n(1);
        Acc.NCrop1(K)   = Acc.NCrop1(K)   + n(2);
        Acc.NCrop2(K)   = Acc.NCrop2(K)   + n(3);
        Acc.NNeither(K) = Acc.NNeither(K) + n(4);
        Acc.NVisits(K)  = Acc.NVisits(K)  + double(Evaluable);
        Vn = Vn + n;
    end
    VisRow = struct('Stem','', 'NCropsPresent',Nai, ...
                    'NMatched', sum(Vn), 'NBoth', Vn(1), ...
                    'NCrop1', Vn(2), 'NCrop2', Vn(3), 'NNeither', Vn(4));
end


function Rows = i_pairRows(T1, T2, M, O1, O2, CropA, CropB, Iface, VisitIdx, Args)
    % Build per-pair source rows for one interface. T1 = crop-A table (matched
    % order), T2 = crop-B reference table; M selects the matched pairs;
    % Iface = interface row index in LASToverlaps. XFULL/YFULL are read from
    % the 'XFULL'/'YFULL' columns added upstream by imProc.cat.addXYfull
    % (NaN if the crop's ORIGSEC was unavailable).
    Np = sum(M);
    Rows = struct( ...
        'Visit',    repmat(VisitIdx, Np, 1), ...
        'Interface',repmat(Iface,    Np, 1), ...
        'Crop1',    repmat(CropA,    Np, 1), ...
        'SrcNum1',  i_colM(T1,'CROPSRCIDX',M), ...
        'X1', i_colM(T1,Args.ColX,M), 'Y1', i_colM(T1,Args.ColY,M), ...
        'XFULL1', i_colM(T1,'XFULL',M), 'YFULL1', i_colM(T1,'YFULL',M), ...
        'RA1', i_colM(T1,'RA',M), 'Dec1', i_colM(T1,'Dec',M), ...
        'Ovlp1', double(O1(:)), ...
        'Crop2',    repmat(CropB,    Np, 1), ...
        'SrcNum2',  i_colM(T2,'CROPSRCIDX',M), ...
        'X2', i_colM(T2,Args.ColX,M), 'Y2', i_colM(T2,Args.ColY,M), ...
        'XFULL2', i_colM(T2,'XFULL',M), 'YFULL2', i_colM(T2,'YFULL',M), ...
        'RA2', i_colM(T2,'RA',M), 'Dec2', i_colM(T2,'Dec',M), ...
        'Ovlp2', double(O2(:)) );
    if Args.KeepOnlyFlagged
        Keep = (O1(:) | O2(:));
        Rows = i_selectSrc(Rows, Keep);
    end
end


function Tk = i_pairTable(T1, T2, M, O1, O2, CropA, CropB, Iface, VisitIdx, Args)
    % Wide per-pair table for one interface holding EVERY catalog column of
    % both members (suffixed _1 / _2) plus pair metadata. T1 = crop-A table
    % (matched order), T2 = crop-B reference; M selects the matched pairs.
    Np = sum(M);
    A1 = T1(M, :);   A1.Properties.VariableNames = strcat(A1.Properties.VariableNames, '_1');
    A2 = T2(M, :);   A2.Properties.VariableNames = strcat(A2.Properties.VariableNames, '_2');
    Meta = table(repmat(VisitIdx,Np,1), repmat(Iface,Np,1), ...
                 repmat(CropA,Np,1),    repmat(CropB,Np,1), ...
                 double(O1(:)),         double(O2(:)), ...
        'VariableNames', {'Visit','Interface','Crop1','Crop2','Ovlp1','Ovlp2'});
    Tk = [Meta, A1, A2];
    if Args.KeepOnlyFlagged
        Tk = Tk(O1(:) | O2(:), :);
    end
end


function T = i_vertcatUnion(Cells)
    % Vertically concatenate per-interface pair tables, unioning columns with
    % NaN-fill for any schema mismatch (LAST catalog columns are numeric).
    Cells = Cells(~cellfun(@isempty, Cells));
    if isempty(Cells)
        T = table();
        return;
    end
    AllCols = Cells{1}.Properties.VariableNames;
    for K = 2:numel(Cells)
        AllCols = union(AllCols, Cells{K}.Properties.VariableNames, 'stable');
    end
    for K = 1:numel(Cells)
        Missing = setdiff(AllCols, Cells{K}.Properties.VariableNames, 'stable');
        for MI = 1:numel(Missing)
            Cells{K}.(Missing{MI}) = nan(height(Cells{K}), 1);
        end
        Cells{K} = Cells{K}(:, AllCols);
    end
    T = vertcat(Cells{:});
end


function Cat = i_stampIndex(Cat)
    % Stamp 'CROPSRCIDX' = 1-based row index of each source within its own
    % crop catalog. Added before matching so it survives the cross-match
    % reorder and can be read back per matched pair. overlapFlagTally does
    % not filter/reorder the catalog before this, so the index equals the
    % source's row number in the on-disk crop catalog.
    if ~any(strcmp(Cat.ColNames, 'CROPSRCIDX'))
        N = size(Cat.Catalog, 1);
        Cat = Cat.insertMultiCol((1:N).', {'CROPSRCIDX'}, {''});
    end
end


function Cat = i_addXYfull(Cat, Hdr, Args)
    % Add XFULL/YFULL columns to a (copied) AstroCatalog via the canonical
    % imProc.cat.addXYfull, using Hdr for ORIGSEC. Leaves Cat unchanged if
    % the header/ORIGSEC is missing (columns then absent -> NaN downstream).
    try
        Tmp = AstroImage;
        Tmp.CatData    = Cat;    % handle: addXYfull mutates this copy in place
        Tmp.HeaderData = Hdr;
        [~, Tmp] = imProc.cat.addXYfull(Tmp, 'ColX', Args.ColX, 'ColY', Args.ColY, ...
                       'KeyCCDSEC', Args.KeyCCDSEC, 'CreateNewObj', false);
        Cat = Tmp.CatData;
    catch
        % ORIGSEC absent/unreadable -> no XFULL/YFULL for this crop.
    end
end


function v = i_colM(T, name, M)
    % Column 'name' from table T restricted to mask M; NaN-filled if absent.
    if ismember(name, T.Properties.VariableNames)
        col = double(T.(name));
        v = col(M);
    else
        v = nan(sum(M), 1);
    end
    v = v(:);
end


function S = i_emptySrc()
    % Empty per-pair source struct (0x1 column vectors), field order fixed.
    F = {'Visit','Interface','Crop1','SrcNum1','X1','Y1','XFULL1','YFULL1','RA1','Dec1', ...
         'Ovlp1','Crop2','SrcNum2','X2','Y2','XFULL2','YFULL2','RA2','Dec2','Ovlp2'};
    KV = [F; repmat({zeros(0,1)}, 1, numel(F))];
    S = struct(KV{:});
end


function S = i_appendSrc(S, Add)
    % Vertically concatenate two per-pair source structs (same fields).
    F = fieldnames(S);
    for I = 1:numel(F)
        S.(F{I}) = [S.(F{I}); Add.(F{I})];
    end
end


function S = i_selectSrc(S, Keep)
    % Row-select a per-pair source struct by logical mask Keep.
    F = fieldnames(S);
    for I = 1:numel(F)
        S.(F{I}) = S.(F{I})(Keep);
    end
end


function Pool = i_poolFrom(Acc)
    % Sum the per-interface accumulator into scalar pool stats.
    Pool.NMatched  = sum(Acc.NMatched);
    Pool.NBoth     = sum(Acc.NBoth);
    Pool.NCrop1    = sum(Acc.NCrop1);
    Pool.NCrop2    = sum(Acc.NCrop2);
    Pool.NNeither  = sum(Acc.NNeither);
    Pool.NOne      = Pool.NCrop1 + Pool.NCrop2;
    Den            = max(Pool.NMatched, 1);
    Pool.FracBoth  = Pool.NBoth    / Den;
    Pool.FracOne   = Pool.NOne     / Den;
    Pool.FracNone  = Pool.NNeither / Den;
    Pool.NInterfaces = sum(Acc.NVisits > 0);
    Pool.NVisits     = sum(Acc.NVisits);
end


function i_printInterfaces(Ind, Acc)
    % Per-interface one-line print (AstroImage single-visit mode).
    for K = 1:size(Ind,1)
        fprintf('iface %2d (crops %2d-%2d): matched=%5d  both=%5d  c1=%5d  c2=%5d  none=%5d\n', ...
            K, Ind(K,1), Ind(K,2), Acc.NMatched(K), Acc.NBoth(K), ...
            Acc.NCrop1(K), Acc.NCrop2(K), Acc.NNeither(K));
    end
end


function Visits = i_discoverVisits(BaseDir, Pattern, Recursive, FieldId, Filter)
    % Glob files matching Pattern under BaseDir; group by visit stem via the
    % LAST filename convention '<stem>_<crop>_sci_coadd_<Cat|Image>_<N>.fits'.
    if Recursive
        D = dir(fullfile(BaseDir, '**', Pattern));
    else
        D = dir(fullfile(BaseDir, Pattern));
    end
    D = D(~[D.isdir]);
    Visits = struct('Stem', {}, 'Files', {}, 'CropIds', {});
    if isempty(D)
        return;
    end
    Files = fullfile({D.folder}, {D.name});
    Names = {D.name};
    if ~isempty(FieldId)
        Keep = contains(Names, ['_' FieldId '_']);
        Files = Files(Keep);  Names = Names(Keep);
    end
    if ~isempty(Filter)
        Keep = contains(Names, ['_' Filter '_']);
        Files = Files(Keep);  Names = Names(Keep);
    end
    if isempty(Files)
        return;
    end
    NF = numel(Files);
    Stems   = cell(1, NF);
    CropIdx = nan(1, NF);
    for K = 1:NF
        [~, FName, ~] = fileparts(Names{K});
        Tok = regexp(FName, '^(.+?)_(\d+)_sci_coadd_(?:Image|Cat)_\d+$', 'tokens', 'once');
        if isempty(Tok)
            Stems{K} = FName;
        else
            Stems{K}   = Tok{1};
            CropIdx(K) = str2double(Tok{2});
        end
    end
    [Us, ~, IUs] = unique(Stems);
    Visits = repmat(struct('Stem','','Files',{{}},'CropIds',[]), 1, numel(Us));
    for K = 1:numel(Us)
        Sel = find(IUs == K);
        [SortedCrops, Order] = sort(CropIdx(Sel));
        Visits(K).Stem    = Us{K};
        Visits(K).Files   = Files(Sel(Order));
        Visits(K).CropIds = SortedCrops(:).';
    end
end


function AI = i_loadVisitTile(Vis, NCrops)
    % Load one visit's present crops and place each at its crop-index slot
    % (1..NCrops) so AI(c).CatData is crop c. Missing crops stay as empty
    % AstroImage placeholders (their interfaces evaluate to zero matches).
    Present = AstroImage.readProducts(Vis.Files, 'ExtraOutProduct', "Cat");
    AI = AstroImage([1, NCrops]);
    for K = 1:numel(Vis.CropIds)
        C = Vis.CropIds(K);
        if isfinite(C) && C >= 1 && C <= NCrops
            AI(C) = Present(K);
        end
    end
end
