function [PC_array, Pool] = selectCalibratorsJoint(PC_array, AI_array, Args)
    % Joint-visit calibrator selection across an entire 24-crop image.
    % Pools per-crop candidate tables into one field-level table, runs the
    % optional audit and the quality cuts ONCE over the pooled set, then
    % partitions the survivors back to per-crop SourceData / SpecData.
    %
    % Same selection arguments as selectCalibrators — see that method's
    % header for documentation. Joint-mode benefits:
    %   - LAST nearest-neighbour audit spans crop boundaries (fixes
    %     duplicates near the seams that per-crop audit misses).
    %   - Audit + quality cuts run once instead of 24 times.
    %   - Cross-crop bookkeeping (XFULL, YFULL, CropID) is preserved on
    %     each PC(i).SourceData for downstream joint-fit code.
    % Input  : - PC_array - Nx1 PhotCalibTrans array (one per crop), with
    %                       metadata already populated. SourceData / SpecData
    %                       fields are overwritten by this method.
    %          - AI_array - Nx1 AstroImage array (one per crop, in the same
    %                       order as PC_array). HeaderData of each AI must
    %                       carry the ORIGSEC keyword for CCDSEC-based
    %                       crop->full coordinate translation.
    %          * Args - struct or key/val with the same fields as
    %                   selectCalibrators (CalibCatName, SearchRadius,
    %                   MagRange, MinSN, MaxSN, FilterBadFlags, BadBitNames,
    %                   FluxColName, MagColName, FilterNegFlux, MinSN2,
    %                   SpFluxCol, AttachBP_RP, AuditCalibrators + audit
    %                   thresholds, Verbose, match_catsHTMArgs). See
    %                   selectCalibrators for defaults.
    % Output : - PC_array - same array with SourceData, SpecData, CalFound
    %                       populated per crop. Empty SourceData for crops
    %                       whose pooled survivors were zero.
    %          - Pool    - the post-cut pooled MATLAB table (XFULL, YFULL,
    %                       LocalX, LocalY, CropID, CalibInd, MatchDistRad,
    %                       Nmatch + all observed columns). Useful for joint-
    %                       fit code that wants the unified view.
    % Author : D. Kovaleva (April 2026)
    % Example: [PC, Pool] = PhotCalibTrans.selectCalibratorsJoint(PC, AI, ...
    %              'CalibCatName', 'GAIADR3spec', 'SearchRadius', 2, ...
    %              'AuditCalibrators', true);

    arguments
        PC_array
        AI_array
        Args.SearchRadius             = 2
        Args.MagRange                 = [11.5 16.0]
        Args.MinSN                    = 5
        Args.MaxSN                    = 1000
        Args.FilterBadFlags  logical  = true
        Args.FluxColName              = 'FLUX_APER_3'
        Args.MagColName               = 'MAG_APER_3'
        Args.FilterNegFlux   logical  = true
        Args.MinSN2                   = 10
        Args.CalibCatName             = 'GAIADR3spec'
        Args.SpFluxCol                = [7, 349, 350, 692]
        Args.BadBitNames              = {'Saturated','NaN','Negative','CR_DeltaHT','NearEdge'}
        Args.match_catsHTMArgs        = {}
        Args.AuditCalibrators logical = false
        Args.AuditCatName             = 'GAIADR3'
        Args.AuditBPRPExcessFactorMax = 1.3
        Args.AuditBPRPMax             = 1.5
        Args.AuditLASTNearestDist     = 20
        Args.AuditLASTDeltaMag        = 2
        Args.AttachBP_RP     logical  = true
        Args.Verbose         logical  = false
    end

    Ncrops = numel(PC_array);
    assert(numel(AI_array) == Ncrops, ...
        'PhotCalibTrans:selectCalibratorsJoint:LengthMismatch', ...
        'PC_array (%d) and AI_array (%d) must have the same length.', ...
        Ncrops, numel(AI_array));

    % ----- Stage 0: add XFULL/YFULL columns to each crop's CatData ---------
    % imProc.cat.addXYfull reads ORIGSEC from each AI's header and writes
    % XFULL/YFULL columns into AI(i).CatData (in place). All downstream
    % primitives then receive them through the regular .Table machinery.
    [~, AI_array] = imProc.cat.addXYfull(AI_array);

    % ----- Stage 1: per-crop discovery -------------------------------------
    CandsCell    = cell(Ncrops, 1);
    FieldTabCell = cell(Ncrops, 1);
    CatHCell     = cell(Ncrops, 1);
    for I = 1:Ncrops
        Cat = AI_array(I).CatData;
        [CandsCell{I}, FieldTabCell{I}, CatHCell{I}] = ...
            PhotCalibTrans.findCalibCandidates(Cat, ...
                'CalibCatName',      Args.CalibCatName, ...
                'SearchRadius',      Args.SearchRadius, ...
                'match_catsHTMArgs', Args.match_catsHTMArgs, ...
                'Verbose',           Args.Verbose, ...
                'Logger',            PC_array(I));
    end

    % ----- Stage 2: pool across crops --------------------------------------
    [Pool, FieldPool, CatHCell] = PhotCalibTrans.poolCalibCandidates( ...
        CandsCell, FieldTabCell, CatHCell);

    if Args.Verbose
        fprintf('  Joint pool: %d candidates across %d crops\n', ...
                height(Pool), Ncrops);
    end

    % ----- Stage 3: audit on the pooled table (optional) -------------------
    if Args.AuditCalibrators && ~isempty(Pool) && height(Pool) > 0
        Doubtful = PhotCalibTrans.auditCalibCandidates(Pool, FieldPool, ...
            'AuditCatName',             Args.AuditCatName, ...
            'SearchRadius',             Args.SearchRadius, ...
            'AuditBPRPMax',             Args.AuditBPRPMax, ...
            'AuditBPRPExcessFactorMax', Args.AuditBPRPExcessFactorMax, ...
            'AuditLASTNearestDist',     Args.AuditLASTNearestDist, ...
            'AuditLASTDeltaMag',        Args.AuditLASTDeltaMag, ...
            'MagColName',               Args.MagColName, ...
            'Verbose',                  Args.Verbose, ...
            'Logger',                   PC_array(1));
        NumDoubtful = sum(Doubtful);
        Pool = Pool(~Doubtful, :);
        if Args.Verbose
            fprintf('  Joint audit: %d flagged doubtful, %d remain\n', ...
                    NumDoubtful, height(Pool));
        end
    end

    % ----- Stage 4: quality cuts on the pooled table -----------------------
    if ~isempty(Pool) && height(Pool) > 0
        KeepMask = PhotCalibTrans.applyCalibQuality(Pool, ...
            'MagRange',       Args.MagRange, ...
            'MagColName',     Args.MagColName, ...
            'FilterBadFlags', Args.FilterBadFlags, ...
            'BadBitNames',    Args.BadBitNames, ...
            'MinSN',          Args.MinSN, ...
            'MaxSN',          Args.MaxSN, ...
            'FluxColName',    Args.FluxColName, ...
            'FilterNegFlux',  Args.FilterNegFlux, ...
            'MinSN2',         Args.MinSN2, ...
            'Verbose',        Args.Verbose);
        Pool = Pool(KeepMask, :);
    end

    if Args.Verbose
        fprintf('  Joint pool after cuts: %d survivors\n', height(Pool));
    end

    % ----- Stage 5: partition survivors back to per-crop SourceData --------
    PerCropCands = PhotCalibTrans.partitionByCrop(Pool, Ncrops, ...
        'DropPoolColumns', false);   % keep XFULL/YFULL/CropID for joint-fit code

    for I = 1:Ncrops
        Cands_i = PerCropCands{I};
        CatH_i  = CatHCell{I};
        if isempty(Cands_i) || height(Cands_i) == 0
            PC_array(I).SourceData = [];
            PC_array(I).SpecData   = [];
            PC_array(I).CalFound   = false;
            continue;
        end
        [SourceTab, SpecData, CalFound] = buildCalibratorData( ...
            Cands_i, CatH_i, Args, PC_array(I));
        PC_array(I).SourceData = AstroCatalog(SourceTab);
        PC_array(I).SpecData   = SpecData;
        PC_array(I).CalFound   = CalFound;
    end
end

% =========================================================================
function [SourceTab, SpecData, CalFound] = buildCalibratorData(Cands, CatH, Args, Obj)
    % Build per-crop SourceData table + SpecData struct from a survivor
    % sub-table and its catsHTM matched reference. Mirrors the construction
    % block at the end of selectCalibrators verbatim — kept here as a local
    % helper so selectCalibratorsJoint stays self-contained.
    RAD = constant.RAD;

    CalIdx     = double(Cands.CalibInd);
    Nmatch     = Cands.Nmatch;
    DistArcsec = convert.angular('rad', 'arcsec', Cands.MatchDistRad);

    CalArr = CatH.Catalog;
    CalTab = CalArr(CalIdx, :);

    FluxIni  = Args.SpFluxCol(1);
    FluxEnd  = Args.SpFluxCol(2);
    EFluxIni = Args.SpFluxCol(3);
    EFluxEnd = Args.SpFluxCol(4);

    SpecFlux = double(CalTab(:, FluxIni:FluxEnd));
    SpecErr  = double(CalTab(:, EFluxIni:EFluxEnd));

    Cal_RA  = double(CalTab(:, 1)) * RAD;
    Cal_Dec = double(CalTab(:, 2)) * RAD;

    Obs_X    = Cands.X;
    Obs_Y    = Cands.Y;
    Obs_RA   = Cands.RA;
    Obs_Dec  = Cands.Dec;
    Obs_Flux = Cands.(Args.FluxColName);

    HasAirmassCol = ismember('AIRMASS', Cands.Properties.VariableNames);
    if HasAirmassCol
        Obs_Airmass = Cands.AIRMASS;
    end

    FluxErrColName = strrep(Args.FluxColName, 'FLUX', 'FLUXERR');
    if ismember(FluxErrColName, Cands.Properties.VariableNames)
        Obs_FluxErr = Cands.(FluxErrColName);
    else
        Obs_FluxErr = sqrt(abs(Obs_Flux));
        Obj.msgLog(LogLevel.Warning, sprintf( ...
            'selectCalibratorsJoint: %s not found, using sqrt(flux) for errors', FluxErrColName));
    end

    Nsources_before = length(Obs_Flux);
    InvalidFlux  = isnan(Obs_Flux) | isinf(Obs_Flux) | (Obs_Flux <= 0);
    InvalidXY    = isnan(Obs_X) | isinf(Obs_X) | isnan(Obs_Y) | isinf(Obs_Y);
    InvalidRADec = isnan(Obs_RA) | isinf(Obs_RA) | isnan(Obs_Dec) | isinf(Obs_Dec);
    ValidCalibMask = ~InvalidFlux & ~InvalidXY & ~InvalidRADec;
    Nvalid = sum(ValidCalibMask);

    if Nvalid < Nsources_before
        Obs_X       = Obs_X(ValidCalibMask);
        Obs_Y       = Obs_Y(ValidCalibMask);
        Obs_RA      = Obs_RA(ValidCalibMask);
        Obs_Dec     = Obs_Dec(ValidCalibMask);
        Obs_Flux    = Obs_Flux(ValidCalibMask);
        Obs_FluxErr = Obs_FluxErr(ValidCalibMask);
        DistArcsec  = DistArcsec(ValidCalibMask);
        Nmatch      = Nmatch(ValidCalibMask);
        Cal_RA      = Cal_RA(ValidCalibMask);
        Cal_Dec     = Cal_Dec(ValidCalibMask);
        SpecFlux    = SpecFlux(ValidCalibMask, :);
        SpecErr     = SpecErr(ValidCalibMask, :);
        if HasAirmassCol
            Obs_Airmass = Obs_Airmass(ValidCalibMask);
        end
    end

    if Nvalid == 0
        SourceTab = table();
        SpecData = [];
        CalFound = false;
        return;
    end

    SpecData = struct();
    SpecData.CalData = struct('RA', Cal_RA, 'Dec', Cal_Dec);
    SpecData.SpecWvl = (3360:20:10200)';
    SpecData.Spec    = SpecFlux;
    SpecData.SpecErr = SpecErr;

    SourceTab = table(Obs_Flux, Obs_FluxErr, Obs_X, Obs_Y, Obs_RA, Obs_Dec, DistArcsec, Nmatch, ...
        'VariableNames', {'Flux','FluxErr','X','Y','RA','Dec','MatchDistance','NumMatches'});
    if HasAirmassCol
        SourceTab.AIRMASS = Obs_Airmass;
    end
    if Args.AttachBP_RP
        [BPRPv, BPv, RPv] = PhotCalibTrans.fetchGaiaBPRP(Obs_RA, Obs_Dec, ...
            Args.AuditCatName, Args.SearchRadius, Obj);
        SourceTab.BP_RP  = BPRPv;
        SourceTab.MAG_BP = BPv;
        SourceTab.MAG_RP = RPv;
    end

    % Carry the joint-fit bookkeeping columns through to SourceData
    if ismember('XFULL', Cands.Properties.VariableNames)
        SourceTab.XFULL  = Cands.XFULL(ValidCalibMask);
        SourceTab.YFULL  = Cands.YFULL(ValidCalibMask);
        SourceTab.CropID = Cands.CropID(ValidCalibMask);
    end

    CalFound = true;
end
