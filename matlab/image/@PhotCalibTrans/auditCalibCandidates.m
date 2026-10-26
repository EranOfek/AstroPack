function [DoubtfulMask, Reason, Fcont] = auditCalibCandidates(CandTab, FieldTab, Args)
    % Step-0 calibrator audit: reject doubtful calibrator candidates.
    % Six OR-combined rules, each vectorized over the candidate set.
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
    %             .AuditBPRPMax             - reject if matched Gaia bp_rp
    %                                         exceeds this. Default 1.5.
    %             .AuditBPRPExcessFactorMax - reject if matched Gaia
    %                                         phot_bp_rp_excess_factor
    %                                         exceeds this. Default 1.3.
    %             .AuditClassprobMin        - reject if matched Gaia
    %                                         classprob_dsc_combmod_star is
    %                                         below this (i.e. not confidently
    %                                         a star). NaN (default) disables
    %                                         the rule. Set 0.9 to mirror the
    %                                         legacy path's classprob filter.
    %             .AuditLASTNearestDist     - arcsec; reject if NN distance
    %                                         in the LAST field is below this.
    %                                         Default 20.
    %             .AuditLASTDeltaMag        - mag; reject if |NearMag-Magcand|
    %                                         is below this. Default 2.
    %             .MagColName               - char column name in the tables
    %                                         used for the LAST NN delta-mag.
    %                                         Default 'MAG_APER_3'.
    %             --- Gaia-predicted aperture contamination (issue #1454) ---
    %             .AuditContamCat           - Gaia DR3 sources around the
    %                                         candidates: an AstroCatalog
    %                                         (RA/Dec in its own units) or a
    %                                         table with RA, Dec [deg], each
    %                                         with AuditContamMagCol. It must
    %                                         be complete to faint magnitudes
    %                                         (the full GAIADR3, not the
    %                                         GAIADR3spec XP subset): the
    %                                         contaminating light of a crowded
    %                                         field comes mostly from stars
    %                                         too faint to have XP spectra.
    %                                         Empty => Fcont is not computed.
    %                                         Default [].
    %             .AuditContamMax           - reject if Fcont, the light of
    %                                         the Gaia neighbours falling into
    %                                         the photometric aperture as a
    %                                         fraction of the candidate's own
    %                                         light in it, exceeds this.
    %                                         NaN (default) disables the
    %                                         rule and Fcont is not computed;
    %                                         Inf computes Fcont without
    %                                         rejecting.
    %             .AuditContamFWHM          - FWHM [arcsec] of the Gaussian
    %                                         PSF used for the light falling
    %                                         into the aperture (the header
    %                                         FWHM). NaN => Fcont is NaN.
    %                                         Default NaN.
    %             .AuditContamAperRad       - radius [pix] of the photometric
    %                                         aperture of the calibration flux.
    %                                         Default 6 (FLUX_APER_3 of LAST).
    %             .AuditContamPixScale      - [arcsec/pix]. NaN => estimated
    %                                         from the candidates' X/Y and
    %                                         RA/Dec. Default NaN.
    %             .AuditContamNeighRad      - [arcsec] neighbours farther than
    %                                         this are ignored. Default 30.
    %             .AuditContamMagCol        - Gaia G column, on both the cone
    %                                         and CandTab.
    %                                         Default 'phot_g_mean_mag'.
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
    %                           "Classprob" | "GaiaContam" | "LASTNN_dist" |
    %                           "LASTNN_dmag".
    %          - Fcont        - double, height(CandTab) x 1. The predicted
    %                           aperture contamination of each candidate; NaN
    %                           where it could not be computed (no cone, no
    %                           FWHM, no Gaia G of the candidate).
    % Author : D. Kovaleva (April 2026; promoted from private helper).
    % Example: [Doubtful, R] = PhotCalibTrans.auditCalibCandidates( ...
    %              CandTab, FieldTab, ...
    %              'MagColName', 'MAG_APER_3', ...
    %              'AuditBPRPMax', 1.5, 'AuditBPRPExcessFactorMax', 1.3);
    %          % with the Gaia contamination rule:
    %          [Doubtful, R, Fcont] = PhotCalibTrans.auditCalibCandidates( ...
    %              CandTab, FieldTab, 'AuditContamCat', GaiaCone, ...
    %              'AuditContamMax', 0.02, 'AuditContamFWHM', 4.9);

    arguments
        CandTab
        FieldTab
        Args.AuditBPRPMax               = 1.5
        Args.AuditBPRPExcessFactorMax   = 1.3
        Args.AuditClassprobMin          = NaN   % NaN disables the classprob rule
        Args.AuditLASTNearestDist       = 20
        Args.AuditLASTDeltaMag          = 2
        Args.MagColName                 = 'MAG_APER_3'
        % Column used for the LAST NN delta-mag comparison. Kept separate
        % from MagColName because MagColName may be set to a Gaia column
        % (e.g. 'phot_g_mean_mag') that is only present on candidates —
        % the NN check needs a column present on BOTH candidates and the
        % full field. Default 'MAG_APER_3' works for LAST throughout.
        Args.AuditNNMagCol              = 'MAG_APER_3'
        Args.AuditContamCat             = []
        Args.AuditContamMax             = NaN   % NaN disables the contamination rule
        Args.AuditContamFWHM            = NaN   % arcsec
        Args.AuditContamAperRad         = 6     % pix
        Args.AuditContamPixScale        = NaN   % arcsec/pix; NaN => from the candidates
        Args.AuditContamNeighRad        = 30    % arcsec
        Args.AuditContamMagCol          = 'phot_g_mean_mag'
        Args.Verbose logical            = false
        Args.Logger                     = []
    end

    Ncand = height(CandTab);
    DoubtfulMask = false(Ncand, 1);
    Reason       = repmat("", Ncand, 1);
    Fcont        = nan(Ncand, 1);
    if Ncand == 0
        return;
    end

    % ---- Gaia photometric audit (direct column reads) ----
    % After the GAIADR3spec regen (Jun 2026), every candidate row coming
    % from findCalibCandidates carries the Gaia tail columns. No secondary
    % cone match is needed; just read off the existing columns.
    CandVarNames = CandTab.Properties.VariableNames;
    BPRPv     = nan(Ncand, 1);
    BPRPExc   = nan(Ncand, 1);
    ClassProb = nan(Ncand, 1);
    if ismember('bp_rp', CandVarNames)
        BPRPv = double(CandTab.bp_rp);
    elseif all(ismember({'phot_bp_mean_mag','phot_rp_mean_mag'}, CandVarNames))
        BPRPv = double(CandTab.phot_bp_mean_mag) - double(CandTab.phot_rp_mean_mag);
    end
    if ismember('phot_bp_rp_excess_factor', CandVarNames)
        BPRPExc = double(CandTab.phot_bp_rp_excess_factor);
    end
    if ismember('classprob_dsc_combmod_star', CandVarNames)
        ClassProb = double(CandTab.classprob_dsc_combmod_star);
    end

    % Apply rules in priority order: BPRPExcess > BPRP > Classprob
    % (Reason records the first hit). The classprob rule is disabled when
    % AuditClassprobMin is NaN (default).
    ExcessHit = isfinite(BPRPExc) & BPRPExc > Args.AuditBPRPExcessFactorMax;
    BPRPHit   = isfinite(BPRPv)   & BPRPv   > Args.AuditBPRPMax;
    ClassHit  = isfinite(ClassProb) & isfinite(Args.AuditClassprobMin) & ...
                ClassProb < Args.AuditClassprobMin;
    DoubtfulMask = DoubtfulMask | ExcessHit | BPRPHit | ClassHit;
    Reason(ExcessHit & Reason == "")             = "BPRPExcess";
    Reason(BPRPHit   & Reason == "")             = "BPRP";
    Reason(ClassHit  & Reason == "")             = "Classprob";

    if Args.Verbose
        fprintf('    audit Gaia (tail cols): %d rejected (BPRP>%.2f, excess>%.2f, classprob<%.2f)\n', ...
            sum(ExcessHit | BPRPHit | ClassHit), ...
            Args.AuditBPRPMax, Args.AuditBPRPExcessFactorMax, Args.AuditClassprobMin);
    end

    % ---- Gaia-predicted aperture contamination (issue #1454) ----
    % In a crowded field LAST does not resolve the neighbours blended into
    % a calibrator's aperture, so the LAST NN rules below miss them. Their
    % light is predicted from Gaia instead.
    if ~isempty(Args.AuditContamCat) && ~isnan(Args.AuditContamMax)
        Fcont = gaiaContamination(CandTab, Args);
        ContamHit = isfinite(Fcont) & isfinite(Args.AuditContamMax) & ...
                    Fcont > Args.AuditContamMax;
        NewByContam = ContamHit & ~DoubtfulMask;
        DoubtfulMask = DoubtfulMask | ContamHit;
        Reason(ContamHit & Reason == "") = "GaiaContam";
        if Args.Verbose
            fprintf('    audit Gaia contamination: median Fcont %.3f, %d additionally rejected (Fcont>%.3f)\n', ...
                median(Fcont, 'omitnan'), sum(NewByContam), Args.AuditContamMax);
        end
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

function Fcont = gaiaContamination(CandTab, Args)
    % Light of the Gaia neighbours in each candidate's aperture, relative to the candidate's own
    %   For a circular Gaussian PSF of FWHM Args.AuditContamFWHM, each
    %   neighbour within AuditContamNeighRad adds 10^(-0.4*dG) times the
    %   fraction of its PSF falling into the aperture, over that fraction
    %   for a centered source. The candidate itself is the closest Gaia
    %   source within SelfDist whose G agrees with its own within SelfDMag.
    % Input  : - Candidate table (RA, Dec [deg], X, Y, AuditContamMagCol).
    %          - auditCalibCandidates Args.
    % Output : - Fcont, height(CandTab) x 1; NaN where not computable.

    SelfDist   = 2;      % arcsec
    SelfDMag   = 0.5;    % mag
    ARCSEC_RAD = 180/pi*3600;

    Ncand   = height(CandTab);
    Fcont   = nan(Ncand, 1);
    MagCol  = Args.AuditContamMagCol;
    Names   = CandTab.Properties.VariableNames;
    Cone    = Args.AuditContamCat;
    if isa(Cone, 'AstroCatalog')
        HaveConeMag = ~isemptyCatalog(Cone) && ismember(MagCol, Cone.ColNames);
    else
        HaveConeMag = istable(Cone) && all(ismember({'RA','Dec',MagCol}, Cone.Properties.VariableNames));
    end
    if ~isfinite(Args.AuditContamFWHM) || ~HaveConeMag || ...
       ~all(ismember({'RA','Dec',MagCol}, Names))
        return;
    end

    % candidates
    CRA  = double(CandTab.RA)  * pi/180;
    CDec = double(CandTab.Dec) * pi/180;
    CMag = double(CandTab.(MagCol));
    Valid = isfinite(CRA) & isfinite(CDec) & isfinite(CMag);
    if ~any(Valid)
        return;
    end
    % projection center: the mean direction (safe across RA = 0)
    V = [mean(cos(CDec(Valid)).*cos(CRA(Valid))), mean(cos(CDec(Valid)).*sin(CRA(Valid))), mean(sin(CDec(Valid)))];
    Center = [mod(atan2(V(2), V(1)), 2*pi), atan2(V(3), hypot(V(1), V(2)))];

    % Gaia sources within the candidates' circle plus the neighbour radius
    if isa(Cone, 'AstroCatalog')
        [GRA, GDec] = Cone.getLonLat('rad');
        GMag = double(Cone.getCol(MagCol));
    else
        GRA  = double(Cone.RA)  * pi/180;
        GDec = double(Cone.Dec) * pi/180;
        GMag = double(Cone.(MagCol));
    end
    RadMax = max(celestial.coo.sphere_dist_fast(Center(1), Center(2), CRA(Valid), CDec(Valid))) + ...
             Args.AuditContamNeighRad / ARCSEC_RAD;
    InCone = isfinite(GMag) & ...
             celestial.coo.sphere_dist_fast(Center(1), Center(2), double(GRA), double(GDec)) <= RadMax;
    GMag = GMag(InCone);
    [Gx, Gy] = celestial.proj.pr_gnomonic(double(GRA(InCone)), double(GDec(InCone)), ARCSEC_RAD, Center);
    [Cx, Cy] = celestial.proj.pr_gnomonic(CRA, CDec, ARCSEC_RAD, Center);

    % pixel scale [arcsec/pix]: from consecutive candidate pairs when not given
    Scale = Args.AuditContamPixScale;
    if ~isfinite(Scale) && all(ismember({'X','Y'}, Names))
        Iv   = find(Valid);
        I1   = Iv(1:end-1);
        I2   = Iv(2:end);
        DAng = hypot(Cx(I1) - Cx(I2), Cy(I1) - Cy(I2));
        DPix = hypot(double(CandTab.X(I1)) - double(CandTab.X(I2)), double(CandTab.Y(I1)) - double(CandTab.Y(I2)));
        Scale = median(DAng(DPix > 10) ./ DPix(DPix > 10), 'omitnan');
    end
    if ~isfinite(Scale)
        return;
    end
    if isempty(GMag)
        % no Gaia source around: nothing contaminates
        Fcont(Valid) = 0;
        return;
    end

    % aperture fraction of a Gaussian PSF offset by D [arcsec]
    Sig = Args.AuditContamFWHM / (2*sqrt(2*log(2)));
    Dg  = (0:0.05:Args.AuditContamNeighRad + 0.05).';
    EE  = encircledGauss(Dg, Args.AuditContamAperRad * Scale, Sig);

    % candidate-neighbour pairs
    [Gy, Ord] = sort(Gy);
    Gx   = Gx(Ord);
    GMag = GMag(Ord);
    IndV = find(Valid);
    Ind  = VO.search.search_sortedY_multi([Gx, Gy], Cx(IndV), Cy(IndV), -Args.AuditContamNeighRad);
    Ic   = repelem(IndV, [Ind.Nmatch].');
    Ig   = vertcat(Ind.Ind);
    D    = vertcat(Ind.Dist);
    dG   = GMag(Ig) - CMag(Ic);

    % the candidate itself: per candidate, the lowest-score pair if it is
    % close enough (a G mismatch pushes the score out of reach)
    Score = D + 100 * (abs(dG) > SelfDMag);
    [~, OrdP] = sortrows([Ic, Score]);
    First    = [true; diff(Ic(OrdP)) ~= 0];
    SelfPair = OrdP(First);
    SelfPair = SelfPair(Score(SelfPair) < SelfDist);
    Keep = true(numel(D), 1);
    Keep(SelfPair) = false;

    Contrib = 10.^(-0.4 * dG(Keep)) .* interp1(Dg, EE, D(Keep)) ./ EE(1);
    Sum     = accumarray(Ic(Keep), Contrib, [Ncand 1]);
    Fcont(Valid) = Sum(Valid);
end

function F = encircledGauss(D, R, Sig)
    % Fraction of a circular Gaussian (sigma Sig) centered at distance D inside a circle of radius R
    %   F(D) = int_0^R (r/Sig^2) exp(-(r^2+D^2)/(2 Sig^2)) I0(r D/Sig^2) dr,
    %   with the exponentially scaled Bessel function for stability.
    Rg = linspace(0, R, 400);
    F  = trapz(Rg, (Rg ./ Sig^2) .* exp(-(Rg - D).^2 ./ (2*Sig^2)) .* besseli(0, Rg .* D ./ Sig^2, 1), 2);
end
