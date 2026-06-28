function [DoubtfulMask, Reason] = auditCalibCandidates(CandTab, FieldTab, Args)
    % Step-0 calibrator audit: reject doubtful calibrator candidates.
    % Four OR-combined rules, each vectorized over the candidate set.
    % Promoted (April 2026) from the private photCalibTransAuditCalibrators
    % helper so per-crop and pooled-field workflows can share the same code.
    % Input  : - CandTab  - MATLAB table of CALIBRATOR CANDIDATES to audit.
    %                       Required columns:
    %                         RA, Dec [deg], X, Y [pixels], <Args.MagColName>.
    %                       NaN allowed in any column — that candidate is
    %                       simply not penalized by the affected rule.
    %          - FieldTab - MATLAB table of the FULL source population over
    %                       which the LAST nearest-neighbour audit (rules c, d)
    %                       searches. Same columns required as CandTab.
    %                       Pass the same handle as CandTab when no separate
    %                       field is available; self-exclusion uses X/Y
    %                       proximity (<1 px).
    %          * Args - struct or key/val with:
    %             .AuditCatName             - char. catsHTM cat name for the
    %                                         photometric Gaia audit (default
    %                                         'GAIADR3').
    %             .SearchRadius             - arcsec, cone-search radius for
    %                                         the Gaia audit match (default 2).
    %             .AuditBPRPMax             - reject if matched Gaia bp_rp
    %                                         exceeds this. Default 1.5.
    %             .AuditBPRPExcessFactorMax - reject if matched Gaia
    %                                         phot_bp_rp_excess_factor
    %                                         exceeds this. Default 1.3.
    %             .AuditLASTNearestDist     - arcsec; reject if NN distance
    %                                         in the LAST field is below this.
    %                                         Default 20.
    %             .AuditLASTDeltaMag        - mag; reject if |NearMag-Magcand|
    %                                         is below this. Default 2.
    %             .MagColName               - char column name in the tables
    %                                         used for the LAST NN delta-mag.
    %                                         Default 'MAG_APER_3'.
    %             .Verbose                  - logical. Default false.
    %             .Logger                   - optional handle implementing
    %                                         msgLog(LogLevel, fmt, ...) for
    %                                         the Gaia-match failure warning
    %                                         path. Empty => fall back to
    %                                         MATLAB warning(). Default [].
    % Output : - DoubtfulMask - logical, height(CandTab) x 1. true => reject.
    %          - Reason       - string array, height(CandTab) x 1. "" for
    %                           kept rows, otherwise the first rule that
    %                           rejected the row: "BPRPExcess" | "BPRP" |
    %                           "LASTNN_dist" | "LASTNN_dmag".
    % Author : D. Kovaleva (April 2026; promoted from private helper).
    % Example: [Doubtful, R] = PhotCalibTrans.auditCalibCandidates( ...
    %              CandTab, FieldTab, ...
    %              'AuditCatName', 'GAIADR3', 'MagColName', 'MAG_APER_3', ...
    %              'AuditBPRPMax', 1.5, 'AuditBPRPExcessFactorMax', 1.3);

    arguments
        CandTab
        FieldTab
        Args.AuditCatName               = 'GAIADR3'
        Args.SearchRadius               = 2
        Args.AuditBPRPMax               = 1.5
        Args.AuditBPRPExcessFactorMax   = 1.3
        Args.AuditLASTNearestDist       = 20
        Args.AuditLASTDeltaMag          = 2
        Args.MagColName                 = 'MAG_APER_3'
        Args.Verbose logical            = false
        Args.Logger                     = []
    end

    Ncand = height(CandTab);
    DoubtfulMask = false(Ncand, 1);
    Reason       = repmat("", Ncand, 1);
    if Ncand == 0
        return;
    end

    % ---- Gaia photometric audit (vectorized) ----
    Sub = AstroCatalog;
    Sub.Catalog  = [CandTab.RA, CandTab.Dec];
    Sub.ColNames = {'RA', 'Dec'};
    Sub.ColUnits = {'deg', 'deg'};

    try
        [~, ~, ResIndA, CatA] = imProc.match.match_catsHTM(Sub, Args.AuditCatName, ...
            'Radius', Args.SearchRadius, 'RadiusUnits', 'arcsec');

        AuditNear = nan(Ncand, 1);
        N1 = min(Ncand, numel(ResIndA.Obj2_IndInObj1));
        AuditNear(1:N1) = ResIndA.Obj2_IndInObj1(1:N1);
        ValidGaia = isfinite(AuditNear);

        BPRPCol    = findColIdxLocal(CatA.ColNames, {'bp_rp'});
        BPCol      = findColIdxLocal(CatA.ColNames, {'phot_bp_mean_mag','Mag_BP','MagBP'});
        RPCol      = findColIdxLocal(CatA.ColNames, {'phot_rp_mean_mag','Mag_RP','MagRP'});
        BPRPExcCol = findColIdxLocal(CatA.ColNames, {'phot_bp_rp_excess_factor'});

        BPRPv   = nan(Ncand, 1);
        BPRPExc = nan(Ncand, 1);
        if any(ValidGaia)
            NiSel = AuditNear(ValidGaia);
            if BPRPCol > 0
                BPRPv(ValidGaia) = double(CatA.Catalog(NiSel, BPRPCol));
            elseif BPCol > 0 && RPCol > 0
                BPRPv(ValidGaia) = double(CatA.Catalog(NiSel, BPCol)) ...
                                 - double(CatA.Catalog(NiSel, RPCol));
            end
            if BPRPExcCol > 0
                BPRPExc(ValidGaia) = double(CatA.Catalog(NiSel, BPRPExcCol));
            end
        end

        % Apply rules in priority order: BPRPExcess > BPRP (Reason records first hit)
        ExcessHit = isfinite(BPRPExc) & BPRPExc > Args.AuditBPRPExcessFactorMax;
        BPRPHit   = isfinite(BPRPv)   & BPRPv   > Args.AuditBPRPMax;
        DoubtfulMask = DoubtfulMask | ExcessHit | BPRPHit;
        Reason(ExcessHit & Reason == "")             = "BPRPExcess";
        Reason(BPRPHit   & Reason == "")             = "BPRP";

        if Args.Verbose
            fprintf('    audit Gaia (%s): %d rejected (BPRP>%.2f or excess>%.2f)\n', ...
                Args.AuditCatName, sum(ExcessHit | BPRPHit), ...
                Args.AuditBPRPMax, Args.AuditBPRPExcessFactorMax);
        end
    catch ME
        if ~isempty(Args.Logger) && ismethod(Args.Logger, 'msgLog')
            Args.Logger.msgLog(LogLevel.Warning, ...
                sprintf('auditCalibCandidates: Gaia match failed (%s) - skipping Gaia checks', ME.message));
        else
            warning('auditCalibCandidates:GaiaMatchFailed', ...
                'Gaia match failed (%s) - skipping Gaia checks', ME.message);
        end
        if Args.Verbose
            fprintf('    audit Gaia: skipped (%s)\n', ME.message);
        end
    end

    % ---- LAST nearest-neighbour audit (vectorized) ----
    Required = {'RA', 'Dec', 'X', 'Y', Args.MagColName};
    FieldNames = FieldTab.Properties.VariableNames;
    HaveAll = all(ismember(Required, FieldNames));
    if HaveAll
        ArcsecPerRad = (180/pi) * 3600;

        % Candidates
        RAcand  = double(CandTab.RA)  * pi/180;
        Deccand = double(CandTab.Dec) * pi/180;
        Xcand   = double(CandTab.X);
        Ycand   = double(CandTab.Y);
        Magcand = double(CandTab.(Args.MagColName));

        % Full field
        AllRArad  = double(FieldTab.RA)  * pi/180;
        AllDecrad = double(FieldTab.Dec) * pi/180;
        AllX      = double(FieldTab.X);
        AllY      = double(FieldTab.Y);
        AllMag    = double(FieldTab.(Args.MagColName));

        % [Ncand x Nfield] pairwise great-circle distance
        DistMat = celestial.coo.sphere_dist_fast( ...
            RAcand, Deccand, AllRArad.', AllDecrad.') * ArcsecPerRad;
        % Self-exclusion: within 1 px of the candidate's own (X, Y)
        D2 = (AllX.' - Xcand).^2 + (AllY.' - Ycand).^2;
        DistMat(D2 < 1) = Inf;

        [NearDist, NearIdx] = min(DistMat, [], 2);   % [Ncand x 1]
        NearMag = AllMag(NearIdx);
        DeltaMag = abs(NearMag - Magcand);

        ValidLast = isfinite(NearDist) & isfinite(NearMag) & isfinite(Magcand);
        DistHit  = ValidLast & NearDist < Args.AuditLASTNearestDist;
        DMagHit  = ValidLast & DeltaMag < Args.AuditLASTDeltaMag;
        NewByLast = (DistHit | DMagHit) & ~DoubtfulMask;
        DoubtfulMask = DoubtfulMask | DistHit | DMagHit;
        Reason(DistHit & Reason == "") = "LASTNN_dist";
        Reason(DMagHit & Reason == "") = "LASTNN_dmag";

        if Args.Verbose
            fprintf('    audit LAST NN: %d additionally rejected (dist<%.1f arcsec or |dmag|<%.2f)\n', ...
                sum(NewByLast), Args.AuditLASTNearestDist, Args.AuditLASTDeltaMag);
        end
    else
        if ~isempty(Args.Logger) && ismethod(Args.Logger, 'msgLog')
            Args.Logger.msgLog(LogLevel.Warning, ...
                sprintf('auditCalibCandidates: missing column(s) for LAST NN check (need RA, Dec, X, Y, %s) - skipping', Args.MagColName));
        else
            warning('auditCalibCandidates:MissingColumns', ...
                'missing column(s) for LAST NN check (need RA, Dec, X, Y, %s) - skipping', Args.MagColName);
        end
        if Args.Verbose
            fprintf('    audit LAST NN: skipped (missing columns)\n');
        end
    end
end

% =========================================================================
function idx = findColIdxLocal(ColNames, Candidates)
    idx = 0;
    for I = 1:numel(Candidates)
        f = find(strcmp(ColNames, Candidates{I}), 1);
        if ~isempty(f); idx = f; return; end
    end
end
