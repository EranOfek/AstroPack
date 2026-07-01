function [Cands, FieldTab, CatH] = findCalibCandidates(Cat, Args)
    % Cone-match an observed source catalog against a reference (catsHTM)
    % calibrator catalog. Returns the rows with successful matches as a
    % candidate table, the full input field for downstream NN searches,
    % and the matched reference rows from catsHTM. No quality cuts here.
    % Input  : - Cat - AstroCatalog with observed sources (must have RA, Dec
    %                  columns in degrees). Other columns (X, Y, Flux*, FLAGS,
    %                  SN, ...) pass through to the candidate table.
    %          * Args - struct or key/val with:
    %             .CalibCatName       - char; catsHTM cat name with reference
    %                                   spectra (default 'GAIADR3spec').
    %             .SearchRadius       - arcsec; cone-search radius (default 2).
    %             .match_catsHTMArgs  - cell of extra forwards to
    %                                   imProc.match.match_catsHTM (default {}).
    %             .Verbose            - logical, default false.
    %             .Logger             - optional handle implementing msgLog;
    %                                   used to surface "missing RA/Dec" as a
    %                                   warning. Empty => MATLAB warning().
    %                                   Default [].
    % Output : - Cands - MATLAB table, one row per matched source. Carries
    %                   every column of Cat.Table plus:
    %                     CalibInd     - row index of the matched calibrator
    %                                    in CatH.Catalog.
    %                     MatchDistRad - match distance [radians].
    %                     Nmatch       - number of calibrator matches for the
    %                                    source (1 means unique).
    %                   Empty table when Cat has no RA/Dec or no matches.
    %          - FieldTab - MATLAB table; the full Cat.Table verbatim. Used
    %                       as the "field population" for auditCalibCandidates'
    %                       LAST nearest-neighbour rule. In the joint-visit
    %                       workflow this is concatenated across crops.
    %          - CatH   - AstroCatalog returned by imProc.match.match_catsHTM
    %                   (reference rows from CalibCatName for the cone). Used
    %                   downstream to read XP spectra. Empty AstroCatalog when
    %                   no matches.
    % Author : D. Kovaleva (April 2026)
    % Example: [Cands, FieldTab, CatH] = PhotCalibTrans.findCalibCandidates( ...
    %              Cat, 'CalibCatName', 'GAIADR3spec', 'SearchRadius', 2);

    arguments
        Cat
        Args.CalibCatName       = 'GAIADR3spec'
        Args.SearchRadius       = 2
        Args.match_catsHTMArgs  = {}
        Args.Verbose logical    = false
        Args.Logger             = []
    end

    FieldTab = Cat.Table;
    Cands    = FieldTab([], :);       % preserve column schema, zero rows
    CatH     = AstroCatalog;

    AllColNames = FieldTab.Properties.VariableNames;
    HasRADec = ismember('RA', AllColNames) && ismember('Dec', AllColNames);
    if ~HasRADec
        if ~isempty(Args.Logger) && ismethod(Args.Logger, 'msgLog')
            Args.Logger.msgLog(LogLevel.Warning, ...
                'findCalibCandidates: Catalog missing RA/Dec columns - cannot match. 0 candidates.');
        else
            warning('PhotCalibTrans:findCalibCandidates:NoRADec', ...
                'Catalog missing RA/Dec columns - cannot match. 0 candidates.');
        end
        return;
    end

    Nsources_initial = height(FieldTab);
    if Args.Verbose
        fprintf('  Matching %d sources with %s (radius=%.1f arcsec)...\n', ...
                Nsources_initial, Args.CalibCatName, Args.SearchRadius);
    end

    [~, ~, ResInd, CatH] = imProc.match.match_catsHTM(Cat, Args.CalibCatName, ...
        'Radius',      Args.SearchRadius, ...
        'RadiusUnits', 'arcsec', ...
        Args.match_catsHTMArgs{:});

    CalIdxAll  = ResInd.Obj2_IndInObj1;
    DistRadAll = ResInd.Obj2_Dist;
    NmatchAll  = ResInd.Obj2_NmatchObj1;
    HasMatchMask = ~isnan(CalIdxAll);

    if Args.Verbose
        fprintf('  Found %d/%d sources with %s matches\n', ...
                sum(HasMatchMask), Nsources_initial, Args.CalibCatName);
    end

    if ~any(HasMatchMask)
        return;
    end

    % Build candidate sub-table with match diagnostics tacked on
    Cands = FieldTab(HasMatchMask, :);
    Cands.CalibInd     = double(CalIdxAll(HasMatchMask));
    Cands.MatchDistRad = DistRadAll(HasMatchMask);
    Cands.Nmatch       = NmatchAll(HasMatchMask);
end
