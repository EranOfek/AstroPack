function Result = findVisits(DataPath, Args)
    % Find LAST visit directories filtered by coordinates, field, time, or
    % header values (AIRMASS, FWHM).
    % Description: Discovers LAST visit subdirectories under DataPath and
    %              filters them by any combination of:
    %                * sky coordinates: point-in-footprint (Coo Nx2 [RA Dec])
    %                  and/or RA / Dec range overlap, via the LAST_FieldsID
    %                  table
    %                * explicit LAST field numbers (FieldNumber)
    %                * Julian-Date window (JDRange) and/or wall-clock window
    %                  (DateRange, UTC, parsed by datetime())
    %                * AIRMASS or FWHM interval, read from one peek FITS
    %                  per candidate visit (adds AIRMASS/FWHM columns to
    %                  the output). Cheap-first: the header read runs only
    %                  after the coord/field/time filters have shrunk the
    %                  candidate list, so it costs O(#candidates), not
    %                  O(#files under DataPath).
    %              Every filter that is specified must be satisfied
    %              (intersection); filters that are left empty impose no
    %              constraint. At least one filter must be specified.
    %
    %              Assumes the modern per-visit-subdirectory layout (one
    %              directory per visit, hence one field per directory). The
    %              field number and JD of a visit are read by peeking at a
    %              single file via AstroFileName.
    %
    % Input  : - DataPath char/string — root directory searched (recursively
    %            by default) for LAST visit directories.
    %                    * ...,key,val,...
    %            'Coo'        - Nx2 numeric [RA Dec] target coordinates in
    %                           degrees. Default [].
    %            'RARange'    - [RAmin RAmax] deg: keep all fields whose
    %                           footprint overlaps this RA span. Default []
    %                           (no RA constraint). RAmin <= RAmax, no
    %                           0/360 wrap.
    %            'DecRange'   - [DecMin DecMax] deg: keep all fields whose
    %                           footprint overlaps this Dec span. Default [].
    %            'FieldNumber'- Numeric vector of LAST field numbers to
    %                           keep. Default [] (no field-number filter).
    %                           Combine with Coo / RARange / DecRange to
    %                           union coord-derived and explicit fields.
    %            'JDRange'    - [JDmin JDmax] keep visits with
    %                           JDmin <= JD <= JDmax. Default [].
    %            'DateRange'  - 1x2 datetime (or string accepted by
    %                           datetime()), UTC, converted to JD and
    %                           intersected with JDRange. Default [].
    %            'AirmassRange' - [Amin Amax] keep visits whose peek FITS
    %                           carries an AIRMASS header value in this
    %                           range. Default [] (no AIRMASS filter).
    %                           Also causes an 'AIRMASS' column to be
    %                           attached to the output.
    %            'FWHMRange'  - [Fmin Fmax] keep visits whose peek FITS
    %                           carries an FWHM header value in this
    %                           range. Default [] (no FWHM filter).
    %                           Also causes a 'FWHM' column to be
    %                           attached to the output. Note: FWHM is
    %                           per-crop in LAST; the value read here is
    %                           whatever crop the peek file happens to
    %                           be, so it is a proxy for the visit
    %                           median, not a rigorous per-crop mask.
    %            'AirmassKey' - Header keyword for AIRMASS. Default
    %                           'AIRMASS'.
    %            'FWHMKey'    - Header keyword for FWHM. Default 'FWHM'.
    %            'HeaderHDU'  - HDU index to open first when reading
    %                           AIRMASS / FWHM from the peek FITS.
    %                           Default 1 (LAST image header carries the
    %                           telescope keywords for both proc and
    %                           coadd; only PT_* photometric keys live in
    %                           the coadd catalog HDU 3). HDU 3 is tried
    %                           as an automatic fallback if the key is
    %                           missing at the primary HDU, so oddball
    %                           coadd layouts still resolve. Override to
    %                           point at a different primary HDU.
    %            'FieldTable' - Path to the LAST_FieldsID file (columns
    %                           RA,Dec,MinRA,MaxRA,MinDec,MaxDec,...) OR a
    %                           table already read from it. The field
    %                           number is the data-row index. Required when
    %                           Coo/RARange/DecRange is used; optional
    %                           otherwise (FieldRA/FieldDec come back NaN
    %                           if absent).
    %            'Tol'        - Footprint box tolerance [deg]. Default 1e-6.
    %                           Widens each field box so a coordinate on a
    %                           ring boundary (the table edges carry
    %                           ~4e-15 deg of rounding dust, e.g. at Dec=0)
    %                           is not lost between adjacent fields.
    %            'Recursive'  - Search DataPath subdirectories at any depth.
    %                           Default true.
    %            'FileType'   - Which product to look for when discovering
    %                           visits / peeking the FieldID:
    %                           'any' | 'coadd' | 'proc'. Default 'any'.
    %            'Verbose'    - Print progress. Default true.
    % Output : - Result table, one row per matching visit:
    %            .VisitPath   - full directory path (string).
    %            .FieldNumber - LAST field number (= LAST_FieldsID row).
    %            .FieldRA     - field-centre RA  [deg] from the table
    %                           (NaN when no FieldTable was supplied).
    %            .FieldDec    - field-centre Dec [deg] from the table
    %                           (NaN when no FieldTable was supplied).
    %            .ProjName    - LAST project/mount/camera token (for a
    %                           future camera filter).
    %            .JD          - visit Julian Date.
    %            .AIRMASS     - visit AIRMASS from the peek FITS (present
    %                           only when 'AirmassRange' was supplied).
    %            .FWHM        - visit FWHM from the peek FITS (present
    %                           only when 'FWHMRange' was supplied).
    %            Sorted by FieldNumber then JD.
    % Author : D. Kovaleva (May 2026)
    % Example: % By coordinates (former findVisitsByCoo behaviour):
    %          FT = '/home/dana/Downloads/Telegram Desktop/LAST_FieldsID.txt';
    %          R  = pipeline.last.load.findVisits( ...
    %                   '/archimedes/test1000/', ...
    %                   'Coo', [215.0 42.3], 'FieldTable', FT);
    %
    %          % By field number, no FieldTable needed:
    %          R = pipeline.last.load.findVisits('/archimedes/test1000/', ...
    %                   'FieldNumber', [350 351 410]);
    %
    %          % By date window (UTC):
    %          R = pipeline.last.load.findVisits('/archimedes/test1000/', ...
    %                   'DateRange', ["2025-05-04 20:00", "2025-05-05 04:00"]);
    %
    %          % Combined — a specific field in a JD window:
    %          R = pipeline.last.load.findVisits('/archimedes/test1000/', ...
    %                   'FieldNumber', 350, 'JDRange', [2460800 2460810]);
    %
    %          % Coord + Dec band + date window all at once:
    %          R = pipeline.last.load.findVisits('/archimedes/test1000/', ...
    %                   'FieldTable', FT, 'RARange', [200 230], ...
    %                   'DecRange', [30 50], ...
    %                   'DateRange', ["2025-05-01", "2025-05-31"]);
    %
    %          % Airmass window on a specific field (adds AIRMASS column):
    %          R = pipeline.last.load.findVisits('/archimedes/test1000/', ...
    %                   'FieldNumber', 350, 'AirmassRange', [1.0 1.4]);
    %
    %          % FWHM window over a JD span (adds FWHM column):
    %          R = pipeline.last.load.findVisits('/archimedes/test1000/', ...
    %                   'JDRange', [2460800 2460810], 'FWHMRange', [1.5 3.5]);
    %
    %          % Both header filters at once on coadd files:
    %          R = pipeline.last.load.findVisits('/archimedes/test1000/', ...
    %                   'FieldNumber', 350, 'FileType', 'coadd', ...
    %                   'AirmassRange', [1.0 1.3], 'FWHMRange', [1.8 3.0]);

    arguments
        DataPath           {mustBeTextScalar}
        Args.Coo           double                            = []
        Args.RARange       double                            = []
        Args.DecRange      double                            = []
        Args.FieldNumber   double                            = []
        Args.JDRange       double                            = []
        Args.DateRange                                       = []
        Args.AirmassRange  double                            = []
        Args.FWHMRange     double                            = []
        Args.AirmassKey    (1,:) char                        = 'AIRMASS'
        Args.FWHMKey       (1,:) char                        = 'FWHM'
        Args.HeaderHDU                                       = []
        Args.FieldTable                                      = ''
        Args.Tol           (1,1) double {mustBeNonnegative}  = 1e-6
        Args.Recursive     logical                           = true
        Args.FileType      (1,:) char ...
            {mustBeMember(Args.FileType, {'any','coadd','proc'})} = 'any'
        Args.Verbose       logical                           = true
    end

    DataPath = char(DataPath);
    if ~isfolder(DataPath)
        error('findVisits:NoDir', 'DataPath not found: %s', DataPath);
    end

    Coo = Args.Coo;
    if ~isempty(Coo) && size(Coo, 2) ~= 2
        error('findVisits:BadCoo', ...
            'Coo must be an Nx2 [RA Dec] array (deg).');
    end
    if ~isempty(Args.RARange) && (numel(Args.RARange) ~= 2 || ...
            Args.RARange(1) > Args.RARange(2))
        error('findVisits:BadRARange', ...
            'RARange must be [RAmin RAmax] with RAmin <= RAmax (deg, no 0/360 wrap).');
    end
    if ~isempty(Args.DecRange) && (numel(Args.DecRange) ~= 2 || ...
            Args.DecRange(1) > Args.DecRange(2))
        error('findVisits:BadDecRange', ...
            'DecRange must be [DecMin DecMax] with DecMin <= DecMax (deg).');
    end
    if ~isempty(Args.JDRange) && (numel(Args.JDRange) ~= 2 || ...
            Args.JDRange(1) > Args.JDRange(2))
        error('findVisits:BadJDRange', ...
            'JDRange must be [JDmin JDmax] with JDmin <= JDmax.');
    end
    if ~isempty(Args.AirmassRange) && (numel(Args.AirmassRange) ~= 2 || ...
            Args.AirmassRange(1) > Args.AirmassRange(2))
        error('findVisits:BadAirmassRange', ...
            'AirmassRange must be [Amin Amax] with Amin <= Amax.');
    end
    if ~isempty(Args.FWHMRange) && (numel(Args.FWHMRange) ~= 2 || ...
            Args.FWHMRange(1) > Args.FWHMRange(2))
        error('findVisits:BadFWHMRange', ...
            'FWHMRange must be [Fmin Fmax] with Fmin <= Fmax.');
    end

    UseCoo   = ~isempty(Coo) || ~isempty(Args.RARange) || ~isempty(Args.DecRange);
    UseField = ~isempty(Args.FieldNumber);
    HaveJD   = ~isempty(Args.JDRange) || ~isempty(Args.DateRange);
    UseAir   = ~isempty(Args.AirmassRange);
    UseFwhm  = ~isempty(Args.FWHMRange);
    if ~(UseCoo || UseField || HaveJD || UseAir || UseFwhm)
        error('findVisits:NoFilter', ...
            ['Provide at least one filter: Coo, RARange, DecRange, ', ...
             'FieldNumber, JDRange, DateRange, AirmassRange or FWHMRange.']);
    end

    % Default HeaderHDU=1: AIRMASS and FWHM live in the image header (HDU 1)
    % for both coadd and proc — only the PT_* photometric keys live in the
    % coadd catalog HDU 3. If the key is missing at HDU 1 we fall back to
    % HDU 3 below (covers oddball layouts where it got copied there).
    if isempty(Args.HeaderHDU); Args.HeaderHDU = 1; end

    % --- merge DateRange into JDRange (intersection) -------------------
    JDLim = Args.JDRange(:).';
    if ~isempty(Args.DateRange)
        DT = i_toDatetime(Args.DateRange);
        if numel(DT) ~= 2 || DT(1) > DT(2)
            error('findVisits:BadDateRange', ...
                'DateRange must be a 1x2 datetime/string with start <= end.');
        end
        DR = [i_jdFromDatetime(DT(1)), i_jdFromDatetime(DT(2))];
        if isempty(JDLim)
            JDLim = DR;
        else
            JDLim = [max(JDLim(1), DR(1)), min(JDLim(2), DR(2))];
            if JDLim(1) > JDLim(2)
                error('findVisits:JDDateConflict', ...
                    'JDRange and DateRange have no overlap.');
            end
        end
    end
    UseJD = ~isempty(JDLim);

    % --- field table (required only when coords are used) --------------
    if UseCoo
        FT = i_loadFieldTable(Args.FieldTable, true);
    else
        FT = i_loadFieldTable(Args.FieldTable, false);
    end
    HaveFT = ~isempty(FT);
    Nfl    = 0;  if HaveFT; Nfl = height(FT); end
    Tol    = Args.Tol;

    % --- target fields: coords + explicit field numbers ----------------
    % The footprint is the [MinRA,MaxRA] x [MinDec,MaxDec] box. Tol widens
    % each box slightly: the LAST_FieldsID ring edges carry ~4e-15 deg of
    % rounding dust (e.g. the Dec=0 boundary is stored as +-4.37e-15), so a
    % coordinate landing exactly on a boundary would otherwise fall in the
    % microscopic gap between two adjacent fields.
    TargetFields = [];

    if ~isempty(Coo)
        RA  = mod(Coo(:,1), 360);
        Dec = Coo(:,2);
        for Ic = 1:size(Coo, 1)
            Hit = find(FT.MinRA  - Tol <= RA(Ic)  & RA(Ic)  <= FT.MaxRA  + Tol & ...
                       FT.MinDec - Tol <= Dec(Ic) & Dec(Ic) <= FT.MaxDec + Tol, 1);
            if ~isempty(Hit)
                TargetFields(end+1,1) = Hit;                       %#ok<AGROW>
            elseif Args.Verbose
                fprintf('  coord %d (RA=%.4f Dec=%.4f): no field footprint contains it\n', ...
                    Ic, RA(Ic), Dec(Ic));
            end
        end
    end
    if ~isempty(Args.RARange) || ~isempty(Args.DecRange)
        RR = [0 360];   if ~isempty(Args.RARange);  RR = Args.RARange(:).';  end
        DR = [-90 90];  if ~isempty(Args.DecRange); DR = Args.DecRange(:).'; end
        Ov = (FT.MinRA  - Tol <= RR(2)) & (FT.MaxRA  + Tol >= RR(1)) & ...
             (FT.MinDec - Tol <= DR(2)) & (FT.MaxDec + Tol >= DR(1));
        TargetFields = [TargetFields; find(Ov)];
    end
    if UseField
        TargetFields = [TargetFields; Args.FieldNumber(:)];
    end
    TargetFields = unique(TargetFields);
    if (UseCoo || UseField) && isempty(TargetFields)
        error('findVisits:NoField', ...
            'No field matches the requested coords / field-number set.');
    end
    if Args.Verbose && ~isempty(TargetFields)
        if numel(TargetFields) <= 20
            fprintf('findVisits: target -> %d field(s): %s\n', ...
                numel(TargetFields), mat2str(TargetFields(:).'));
        else
            fprintf('findVisits: target -> %d field(s)\n', numel(TargetFields));
        end
    end

    % --- discover visit directories (field-restricted, fast) ----------
    % Glob only files whose name carries a target field number, so the
    % cost scales with the matching visits, not the whole DataPath tree.
    % When TargetFields is empty (pure-JD query), fall back to globbing
    % every LAST sci file under DataPath.
    [VisitDirs, PeekFiles] = i_discoverVisits(DataPath, TargetFields, ...
        Args.FileType, Args.Recursive, Args.Verbose);
    if isempty(VisitDirs)
        if Args.Verbose
            fprintf('findVisits: no LAST files matching the filter under %s\n', DataPath);
        end
        Result = i_emptyResult(UseAir, UseFwhm);
        return;
    end

    % --- parse field number + JD per visit (AstroFileName) -------------
    AFN     = AstroFileName(PeekFiles);
    FieldNo = str2double(string(AFN.FieldID));  FieldNo = FieldNo(:);
    ProjN   = string(AFN.ProjName);             ProjN   = ProjN(:);
    JD      = AFN.julday();                     JD      = JD(:);

    Keep = true(size(FieldNo));
    if ~isempty(TargetFields)
        Keep = Keep & ismember(FieldNo, TargetFields);
    end
    if HaveFT
        Keep = Keep & FieldNo >= 1 & FieldNo <= Nfl;
    end
    if UseJD
        Keep = Keep & JD >= JDLim(1) & JD <= JDLim(2);
    end

    if ~any(Keep)
        if Args.Verbose
            fprintf('findVisits: 0 of %d visit dir(s) match the filters.\n', ...
                numel(VisitDirs));
        end
        Result = i_emptyResult(UseAir, UseFwhm);
        return;
    end

    Idx = find(Keep);

    % --- Optional AIRMASS / FWHM filter (opens one FITS per candidate) --
    Airmass = [];
    FWHM    = [];
    if UseAir || UseFwhm
        Airmass = nan(numel(Idx), 1);
        FWHM    = nan(numel(Idx), 1);
        % Primary HDU + fallback (HDU 3) for the coadd oddball case where
        % the key was copied into the catalog table header.
        HdusToTry = unique([Args.HeaderHDU, 3], 'stable');
        for J = 1:numel(Idx)
            Done = false;
            for Ih = 1:numel(HdusToTry)
                if Done; continue; end
                H = HdusToTry(Ih);
                AH = [];
                try
                    AH = AstroHeader(PeekFiles{Idx(J)}, H);
                catch
                end
                if ~isempty(AH)
                    if UseAir && isnan(Airmass(J))
                        V = AH.getVal(Args.AirmassKey);
                        if isnumeric(V) && isscalar(V); Airmass(J) = double(V); end
                    end
                    if UseFwhm && isnan(FWHM(J))
                        V = AH.getVal(Args.FWHMKey);
                        if isnumeric(V) && isscalar(V); FWHM(J) = double(V); end
                    end
                end
                Done = (~UseAir || ~isnan(Airmass(J))) && ...
                       (~UseFwhm || ~isnan(FWHM(J)));
            end
        end
        KeepMore = true(numel(Idx), 1);
        if UseAir
            KeepMore = KeepMore & Airmass >= Args.AirmassRange(1) & ...
                                  Airmass <= Args.AirmassRange(2);
        end
        if UseFwhm
            KeepMore = KeepMore & FWHM >= Args.FWHMRange(1) & ...
                                  FWHM <= Args.FWHMRange(2);
        end
        if Args.Verbose
            fprintf('findVisits: AIRMASS/FWHM filter kept %d of %d\n', ...
                sum(KeepMore), numel(Idx));
        end
        Idx     = Idx(KeepMore);
        Airmass = Airmass(KeepMore);
        FWHM    = FWHM(KeepMore);
        if isempty(Idx)
            Result = i_emptyResult(UseAir, UseFwhm);
            return;
        end
    end

    FieldRA  = nan(numel(Idx), 1);
    FieldDec = nan(numel(Idx), 1);
    if HaveFT
        InFT = FieldNo(Idx) >= 1 & FieldNo(Idx) <= Nfl;
        FieldRA(InFT)  = FT.RA(FieldNo(Idx(InFT)));
        FieldDec(InFT) = FT.Dec(FieldNo(Idx(InFT)));
    end
    Vars = {'VisitPath','FieldNumber','FieldRA','FieldDec','ProjName','JD'};
    Cols = {string(VisitDirs(Idx)), FieldNo(Idx), FieldRA, FieldDec, ...
            ProjN(Idx),             JD(Idx)};
    if UseAir;  Vars{end+1} = 'AIRMASS'; Cols{end+1} = Airmass; end
    if UseFwhm; Vars{end+1} = 'FWHM';    Cols{end+1} = FWHM;    end
    Result = table(Cols{:}, 'VariableNames', Vars);
    Result = sortrows(Result, {'FieldNumber','JD'});

    if Args.Verbose
        fprintf('findVisits: %d visit(s) match.\n', height(Result));
    end
end

% =========================================================================
function FT = i_loadFieldTable(Spec, Required)
    % Resolve the LAST_FieldsID field table. Returns [] when not required
    % and not supplied.
    if istable(Spec)
        FT = Spec;
    elseif (ischar(Spec) || isstring(Spec)) && ~isempty(char(Spec)) ...
            && isfile(char(Spec))
        FT = readtable(char(Spec));
    elseif Required
        error('findVisits:NoFieldTable', ...
            ['Args.FieldTable must be a path to the LAST_FieldsID file, ', ...
             'or a table already read from it (columns RA, Dec, MinRA, ', ...
             'MaxRA, MinDec, MaxDec).']);
    else
        FT = [];
        return;
    end
    Need = {'RA','Dec','MinRA','MaxRA','MinDec','MaxDec'};
    Miss = Need(~ismember(Need, FT.Properties.VariableNames));
    if ~isempty(Miss)
        error('findVisits:BadFieldTable', ...
            'Field table is missing column(s): %s', strjoin(Miss, ', '));
    end
end

% =========================================================================
function DT = i_toDatetime(X)
    % Accept datetime, string, char, or cellstr; return a column datetime.
    if isa(X, 'datetime')
        DT = X(:);
    else
        DT = datetime(string(X(:)));
    end
end

% =========================================================================
function JD = i_jdFromDatetime(DT)
    % datetime -> JD via POSIX epoch; no toolboxes required.
    JD = posixtime(DT) / 86400 + 2440587.5;
end

% =========================================================================
function [VisitDirs, PeekFiles] = i_discoverVisits(DataPath, TargetFields, ...
        FileType, Recursive, Verbose)
    % Field-restricted visit discovery (or unrestricted when TargetFields
    % is empty).
    % Globs only files whose name contains a target field number, so the
    % walk result scales with the matching visits, not the whole tree.
    % On UNIX uses the OS `find` (fast); elsewhere falls back to dir().
    % The name glob is intentionally loose — AstroFileName re-verifies the
    % exact field number downstream.
    switch FileType
        case 'coadd'; Lev = 'coadd';
        case 'proc';  Lev = 'proc';
        otherwise;    Lev = '';
    end
    if isempty(TargetFields)
        if isempty(Lev)
            NamePat = "LAST*_*_*_sci_*.fits";
        else
            NamePat = "LAST*_*_*_sci_" + Lev + "_*.fits";
        end
    else
        NamePat = strings(1, numel(TargetFields));
        for K = 1:numel(TargetFields)
            if isempty(Lev)
                NamePat(K) = sprintf('LAST*_%d_*_sci_*.fits', TargetFields(K));
            else
                NamePat(K) = sprintf('LAST*_%d_*_sci_%s_*.fits', TargetFields(K), Lev);
            end
        end
    end

    if isunix
        NameExpr = "\( " + strjoin("-name '" + NamePat + "'", " -o ") + " \)";
        if Recursive
            DepthExpr = "";
        else
            DepthExpr = "-maxdepth 1";
        end
        Cmd = sprintf('find %s %s -type f %s', ...
            i_shellQuote(DataPath), DepthExpr, NameExpr);
        [St, Out] = system(Cmd);
        if St ~= 0
            error('findVisits:FindFailed', ...
                'find failed (status %d): %s', St, strtrim(Out));
        end
        Files = splitlines(string(Out));
        Files = Files(strlength(Files) > 0);
    else
        Files = strings(0,1);
        for K = 1:numel(NamePat)
            if Recursive
                D = dir(fullfile(DataPath, '**', char(NamePat(K))));
            else
                D = dir(fullfile(DataPath, char(NamePat(K))));
            end
            D = D(~[D.isdir]);
            if ~isempty(D)
                Files = [Files; string(fullfile({D.folder}.', {D.name}.'))]; %#ok<AGROW>
            end
        end
    end

    if isempty(Files)
        VisitDirs = strings(0,1);
        PeekFiles = {};
        return;
    end

    % one peek file per visit directory
    DirOf = fileparts(Files);
    [VisitDirs, Ia] = unique(DirOf, 'stable');
    VisitDirs = VisitDirs(:);
    PeekFiles = cellstr(Files(Ia(:)));
    if Verbose
        fprintf('findVisits: %d candidate file(s) -> %d visit dir(s)\n', ...
            numel(Files), numel(VisitDirs));
    end
end

% =========================================================================
function Q = i_shellQuote(P)
    % Single-quote a path for /bin/sh, escaping embedded single quotes.
    Q = ['''', strrep(char(P), '''', '''\'''''), ''''];
end

% =========================================================================
function T = i_emptyResult(HasAir, HasFwhm)
    % Zero-row Result table with the documented column types. AIRMASS and
    % FWHM columns are appended when the corresponding filter was used.
    if nargin < 1; HasAir  = false; end
    if nargin < 2; HasFwhm = false; end
    Vars  = {'VisitPath','FieldNumber','FieldRA','FieldDec','ProjName','JD'};
    Types = {'string','double','double','double','string','double'};
    if HasAir;  Vars{end+1} = 'AIRMASS'; Types{end+1} = 'double'; end
    if HasFwhm; Vars{end+1} = 'FWHM';    Types{end+1} = 'double'; end
    T = table('Size', [0 numel(Vars)], ...
              'VariableTypes', Types, 'VariableNames', Vars);
end
