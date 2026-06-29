function [BPRPv, BPv, RPv] = fetchGaiaBPRP(RA_deg, Dec_deg, AuditCatName, SearchRadius_arcsec, Logger)
    % Match candidate calibrators against the photometric Gaia catsHTM
    % catalog and return per-source BP-RP, BP and RP magnitudes (NaN
    % padded for un-matched candidates).
    %
    % Safe under failure: on any error, logs a warning (via Logger.msgLog
    % if available, else warning()) and returns all-NaN vectors. Caller is
    % responsible for skipping when AttachBP_RP=false.
    %
    % Promoted (April 2026) from the private fetchGaiaBPRP_ helper so the
    % joint-visit orchestrator selectCalibratorsJoint can reuse it.
    % Input  : - RA_deg              - Nx1 RA  vector [deg].
    %          - Dec_deg             - Nx1 Dec vector [deg].
    %          - AuditCatName        - char; catsHTM Gaia photometric catalog
    %                                  (default 'GAIADR3').
    %          - SearchRadius_arcsec - cone radius (default 2).
    %          - Logger              - optional handle with msgLog method
    %                                  (e.g., a PhotCalibTrans). Empty =>
    %                                  fallback to warning(). Default [].
    % Output : - BPRPv - Nx1 BP-RP colour, NaN where match failed.
    %          - BPv   - Nx1 BP mag, NaN where match failed.
    %          - RPv   - Nx1 RP mag, NaN where match failed.
    % Author : D. Kovaleva (April 2026)
    % Example: [BPRP, BP, RP] = PhotCalibTrans.fetchGaiaBPRP(RA, Dec, ...
    %              'GAIADR3', 2, PC);
    arguments
        RA_deg
        Dec_deg
        AuditCatName             = 'GAIADR3'
        SearchRadius_arcsec      = 2
        Logger                   = []
    end

    Ncand = numel(RA_deg);
    BPRPv = nan(Ncand, 1);
    BPv   = nan(Ncand, 1);
    RPv   = nan(Ncand, 1);
    if Ncand == 0
        return;
    end

    Sub = AstroCatalog;
    Sub.Catalog  = [double(RA_deg(:)), double(Dec_deg(:))];
    Sub.ColNames = {'RA', 'Dec'};
    Sub.ColUnits = {'deg', 'deg'};

    try
        [~, ~, ResIndA, CatA] = imProc.match.match_catsHTM(Sub, AuditCatName, ...
            'Radius', SearchRadius_arcsec, 'RadiusUnits', 'arcsec');

        Near = nan(Ncand, 1);
        N1   = min(Ncand, numel(ResIndA.Obj2_IndInObj1));
        Near(1:N1) = ResIndA.Obj2_IndInObj1(1:N1);
        Valid = isfinite(Near);
        if ~any(Valid); return; end

        BPRPCol = findColIdxLocal(CatA.ColNames, {'bp_rp'});
        BPCol   = findColIdxLocal(CatA.ColNames, {'phot_bp_mean_mag','Mag_BP','MagBP'});
        RPCol   = findColIdxLocal(CatA.ColNames, {'phot_rp_mean_mag','Mag_RP','MagRP'});

        NiSel = Near(Valid);
        if BPCol > 0
            BPv(Valid) = double(CatA.Catalog(NiSel, BPCol));
        end
        if RPCol > 0
            RPv(Valid) = double(CatA.Catalog(NiSel, RPCol));
        end
        if BPRPCol > 0
            BPRPv(Valid) = double(CatA.Catalog(NiSel, BPRPCol));
        elseif BPCol > 0 && RPCol > 0
            BPRPv(Valid) = BPv(Valid) - RPv(Valid);
        end
    catch ME
        if ~isempty(Logger) && ismethod(Logger, 'msgLog')
            Logger.msgLog(LogLevel.Warning, sprintf( ...
                'fetchGaiaBPRP: Gaia (%s) match failed (%s) - BP_RP/MAG_BP/MAG_RP set to NaN', ...
                AuditCatName, ME.message));
        else
            warning('PhotCalibTrans:fetchGaiaBPRP:MatchFailed', ...
                'Gaia (%s) match failed (%s) - BP_RP/MAG_BP/MAG_RP set to NaN', ...
                AuditCatName, ME.message);
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
