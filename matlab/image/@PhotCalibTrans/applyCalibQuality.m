function [KeepMask, Reason] = applyCalibQuality(Cands, Args)
    % Apply per-source quality cuts to a calibrator-candidate table.
    % Rules, all OR-skipped when the required column is absent:
    %   (1) MagRange         - <Args.GMagColName> (Gaia G) in [MagRange(1), MagRange(2)]
    %   (2) FilterBadFlags   - FLAGS column has no Args.BadBitNames bits set
    %   (3) SN range         - SN in [MinSN, MaxSN]
    %   (4) FilterNegFlux    - <Args.FluxColName> > 0, and (if present) FLUX_PSF > 0
    %   (5) MinSN2           - SN_2 >= MinSN2 (skipped if MinSN2<=0)
    %   (6) UniqueMatch      - Nmatch == 1 (skipped if Nmatch absent)
    % Empty Cands => empty mask / reason; runs in O(height(Cands)).
    % Input  : - Cands - MATLAB table from findCalibCandidates (or any
    %                    pooled equivalent). Required columns depend on which
    %                    rules are active.
    %          * Args - struct or key/val with:
    %             .MagRange         - [MinMag, MaxMag] on Gaia G. Default [12 16].
    %             .GMagColName      - Gaia G column for the MagRange cut.
    %                                 Default 'phot_g_mean_mag'.
    %             .FilterBadFlags   - logical, default true.
    %             .BadBitNames      - cell of bit names. Default
    %                                 {'Saturated','NaN','Negative',
    %                                  'CR_DeltaHT','NearEdge'}.
    %             .MinSN            - default 5.
    %             .MaxSN            - default 1000.
    %             .FluxColName      - char, default 'FLUX_APER_3'.
    %             .FilterNegFlux    - logical, default true.
    %             .MinSN2           - default 10. Set <=0 to skip.
    %             .Verbose          - logical, default false.
    % Output : - KeepMask - logical, height(Cands) x 1. true => passes every
    %                       active cut.
    %          - Reason   - string array, height(Cands) x 1. "" for kept rows,
    %                       otherwise the first rule that rejected the row:
    %                       "MagRange" | "FLAGS" | "SN" | "NegFlux" |
    %                       "NegFluxPSF" | "SN_2" | "NonUnique".
    % Author : D. Kovaleva (April 2026)
    % Example: [Keep, Reason] = PhotCalibTrans.applyCalibQuality(Cands, ...
    %              'MagRange', [12 16], 'MinSN', 5, 'MaxSN', 1000, ...
    %              'FilterBadFlags', true);

    arguments
        Cands
        Args.MagRange         = [12 16]
        Args.GMagColName      = 'phot_g_mean_mag'   % Gaia G column for the MagRange cut
        Args.FilterBadFlags logical = true
        Args.BadBitNames      = {'Saturated','NaN','Negative','CR_DeltaHT','NearEdge'}
        Args.MinSN            = 5
        Args.MaxSN            = 1000
        Args.FluxColName      = 'FLUX_APER_3'
        Args.FilterNegFlux logical = true
        Args.MinSN2           = 10
        Args.Verbose logical  = false
    end

    Nrows    = height(Cands);
    KeepMask = true(Nrows, 1);
    Reason   = repmat("", Nrows, 1);
    if Nrows == 0
        return;
    end

    VarNames = Cands.Properties.VariableNames;

    % --- (1) Magnitude range on Gaia G (phot_g_mean_mag) ---
    if ismember(Args.GMagColName, VarNames)
        Hit = ~(Cands.(Args.GMagColName) >= Args.MagRange(1) & ...
                Cands.(Args.GMagColName) <= Args.MagRange(2));
        applyCut(Hit, "MagRange");
        if Args.Verbose
            fprintf('  Gaia-G magnitude filter (%g-%g): %d sources passed\n', ...
                    Args.MagRange(1), Args.MagRange(2), sum(KeepMask));
        end
    end

    % --- (2) Bad FLAGS ---
    if Args.FilterBadFlags && ismember('FLAGS', VarNames)
        Flags = Cands.FLAGS;
        BadValue = isnan(Flags) | isinf(Flags) | Flags < 0 | Flags ~= floor(Flags);
        Flags(BadValue) = 0;
        BD = BitDictionary('BitMask.Image.Default');
        [~, ~, BadBitMask] = BD.name2bit(Args.BadBitNames);
        Hit = BadValue | bitand(uint32(Flags), uint32(BadBitMask)) > 0;
        applyCut(Hit, "FLAGS");
        if Args.Verbose
            fprintf('  FLAGS filter: %d sources passed\n', sum(KeepMask));
        end
    end

    % --- (3) S/N range ---
    if ismember('SN', VarNames)
        Hit = ~(Cands.SN >= Args.MinSN & Cands.SN <= Args.MaxSN);
        applyCut(Hit, "SN");
        if Args.Verbose
            fprintf('  S/N filter (%g-%g): %d sources passed\n', ...
                    Args.MinSN, Args.MaxSN, sum(KeepMask));
        end
    end

    % --- (4) Negative flux ---
    if Args.FilterNegFlux && ismember(Args.FluxColName, VarNames)
        Hit = ~(Cands.(Args.FluxColName) > 0);
        applyCut(Hit, "NegFlux");
        if Args.Verbose
            fprintf('  Negative flux filter (%s): %d sources passed\n', ...
                    Args.FluxColName, sum(KeepMask));
        end
    end

    % --- (4b) Negative PSF flux (Python parity: FLUX_PSF > 0) ---
    if Args.FilterNegFlux && ismember('FLUX_PSF', VarNames)
        Hit = ~(Cands.FLUX_PSF > 0);
        applyCut(Hit, "NegFluxPSF");
        if Args.Verbose
            fprintf('  Negative PSF-flux filter: %d sources passed\n', sum(KeepMask));
        end
    end

    % --- (5) SN_2 minimum ---
    if Args.MinSN2 > 0 && ismember('SN_2', VarNames)
        Hit = ~(Cands.SN_2 >= Args.MinSN2);
        applyCut(Hit, "SN_2");
        if Args.Verbose
            fprintf('  SN_2 filter (>=%g): %d sources passed\n', ...
                    Args.MinSN2, sum(KeepMask));
        end
    end

    % --- (6) Unique match ---
    if ismember('Nmatch', VarNames)
        Hit = ~(Cands.Nmatch == 1);
        applyCut(Hit, "NonUnique");
        if Args.Verbose
            fprintf('  Unique match filter: %d sources passed\n', sum(KeepMask));
        end
    end

    function applyCut(Hit, Name)
        % Update KeepMask and (first-hit-only) Reason
        if any(Hit)
            NewlyHit = Hit & Reason == "";
            Reason(NewlyHit) = Name;
            KeepMask = KeepMask & ~Hit;
        end
    end
end
