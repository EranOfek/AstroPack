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
    %             .AuditCatName             - DEPRECATED (Jun 2026): kept for
    %                                         backwards compat only. The
    %                                         GAIADR3spec regen attached
    %                                         bp_rp / phot_bp_rp_excess_factor
    %                                         directly to every candidate row
    %                                         via findCalibCandidates, so the
    %                                         secondary Gaia cone match this
    %                                         arg used to drive is no longer
    %                                         executed.
    %             .SearchRadius             - DEPRECATED (Jun 2026): see above.
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
        % Column used for the LAST NN delta-mag comparison. Kept separate
        % from MagColName because MagColName may be set to a Gaia column
        % (e.g. 'phot_g_mean_mag') that is only present on candidates —
        % the NN check needs a column present on BOTH candidates and the
        % full field. Default 'MAG_APER_3' works for LAST throughout.
        Args.AuditNNMagCol              = 'MAG_APER_3'
        Args.Verbose logical            = false
        Args.Logger                     = []
    end

    Ncand = height(CandTab);
    DoubtfulMask = false(Ncand, 1);
    Reason       = repmat("", Ncand, 1);
    if Ncand == 0
        return;
    end

    % ---- Gaia photometric audit (direct column reads) ----
    % After the GAIADR3spec regen (Jun 2026), every candidate row coming
    % from findCalibCandidates carries the Gaia tail columns. No secondary
    % cone match is needed; just read off the existing columns.
    CandVarNames = CandTab.Properties.VariableNames;
    BPRPv   = nan(Ncand, 1);
    BPRPExc = nan(Ncand, 1);
    if ismember('bp_rp', CandVarNames)
        BPRPv = double(CandTab.bp_rp);
    elseif all(ismember({'phot_bp_mean_mag','phot_rp_mean_mag'}, CandVarNames))
        BPRPv = double(CandTab.phot_bp_mean_mag) - double(CandTab.phot_rp_mean_mag);
    end
    if ismember('phot_bp_rp_excess_factor', CandVarNames)
        BPRPExc = double(CandTab.phot_bp_rp_excess_factor);
    end

    % Apply rules in priority order: BPRPExcess > BPRP (Reason records first hit)
    ExcessHit = isfinite(BPRPExc) & BPRPExc > Args.AuditBPRPExcessFactorMax;
    BPRPHit   = isfinite(BPRPv)   & BPRPv   > Args.AuditBPRPMax;
    DoubtfulMask = DoubtfulMask | ExcessHit | BPRPHit;
    Reason(ExcessHit & Reason == "")             = "BPRPExcess";
    Reason(BPRPHit   & Reason == "")             = "BPRP";

    if Args.Verbose
        fprintf('    audit Gaia (tail cols): %d rejected (BPRP>%.2f or excess>%.2f)\n', ...
            sum(ExcessHit | BPRPHit), ...
            Args.AuditBPRPMax, Args.AuditBPRPExcessFactorMax);
    end

    % ---- LAST nearest-neighbour audit (vectorized) ----
    Required = {'RA', 'Dec', 'X', 'Y', Args.AuditNNMagCol};
    FieldNames = FieldTab.Properties.VariableNames;
    HaveAll = all(ismember(Required, FieldNames)) && ...
              ismember(Args.AuditNNMagCol, CandTab.Properties.VariableNames);
    if HaveAll
        ArcsecPerRad = (180/pi) * 3600;

        % Candidates
        RAcand  = double(CandTab.RA)  * pi/180;
        Deccand = double(CandTab.Dec) * pi/180;
        Xcand   = double(CandTab.X);
        Ycand   = double(CandTab.Y);
        Magcand = double(CandTab.(Args.AuditNNMagCol));

        % Full field
        AllRArad  = double(FieldTab.RA)  * pi/180;
        AllDecrad = double(FieldTab.Dec) * pi/180;
        AllX      = double(FieldTab.X);
        AllY      = double(FieldTab.Y);
        AllMag    = double(FieldTab.(Args.AuditNNMagCol));

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
                sprintf('auditCalibCandidates: missing column(s) for LAST NN check (need RA, Dec, X, Y, %s) - skipping', Args.AuditNNMagCol));
        else
            warning('auditCalibCandidates:MissingColumns', ...
                'missing column(s) for LAST NN check (need RA, Dec, X, Y, %s) - skipping', Args.AuditNNMagCol);
        end
        if Args.Verbose
            fprintf('    audit LAST NN: skipped (missing columns)\n');
        end
    end
end
