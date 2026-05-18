function Result = findVisitsByCoo(DataPath, Coo, Args)
    % Find LAST visit directories that observed given sky coordinates
    % Description: Selects LAST survey fields from the LAST_FieldsID table
    %              by sky position — either point-in-footprint (the field
    %              whose MinRA/MaxRA/MinDec/MaxDec box contains a [RA,Dec]
    %              point) or range overlap (every field whose footprint box
    %              overlaps a requested RA and/or Dec range) — then scans
    %              DataPath for visit directories and keeps those whose
    %              filename FieldID matches one of the selected fields. The
    %              field number is the LAST_FieldsID row index and is parsed
    %              from filenames with AstroFileName.
    %
    %              Assumes the modern per-visit-subdirectory layout (one
    %              directory per visit, hence one field per directory). The
    %              field of a visit is read by peeking at a single file.
    %
    % Input  : - DataPath char/string — root directory searched (recursively
    %            by default) for LAST visit directories.
    %          - Coo — Nx2 numeric [RA Dec] target coordinates in degrees.
    %            May be [] when selecting by RARange / DecRange instead.
    %                    * ...,key,val,...
    %            'FieldTable' - Path to the LAST_FieldsID file (columns
    %                           RA,Dec,MinRA,MaxRA,MinDec,MaxDec,...) OR a
    %                           table already read from it. The field
    %                           number is the data-row index. Required —
    %                           no default (avoids a hardcoded path).
    %            'RARange'    - [RAmin RAmax] deg: keep all fields whose
    %                           footprint overlaps this RA span. Default []
    %                           (no RA constraint). RAmin <= RAmax, no
    %                           0/360 wrap.
    %            'DecRange'   - [DecMin DecMax] deg: keep all fields whose
    %                           footprint overlaps this Dec span. Default []
    %                           (no Dec constraint). RARange and DecRange
    %                           may be given alone or together; point and
    %                           range targets are unioned.
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
    %            .FieldRA     - field-centre RA  [deg] from the table.
    %            .FieldDec    - field-centre Dec [deg] from the table.
    %            .ProjName    - LAST project/mount/camera token (for a
    %                           future camera filter).
    %            .JD          - visit Julian Date (for a future date
    %                           filter). Airmass/zenith would be added
    %                           later from the coadd AstroHeader.
    %            Sorted by FieldNumber then JD.
    % Author : D. Kovaleva (May 2026)
    % Example: % Visits that observed RA=215.0, Dec=42.3 deg:
    %          FT = '/home/dana/Downloads/Telegram Desktop/LAST_FieldsID.txt';
    %          R  = pipeline.last.load.findVisitsByCoo( ...
    %                   '/archimedes/.../2025/05/04', [215.0 42.3], ...
    %                   'FieldTable', FT);
    %
    %          % Several coordinates at once; feed the result onward:
    %          R = pipeline.last.load.findVisitsByCoo(DataPath, ...
    %                   [215.0 42.3; 18.0 -87.0], 'FieldTable', FT);
    %          for k = 1:height(R)
    %              MS = pipeline.last.load.loadMergedMat( ...
    %                       'MergedMatDir', char(R.VisitPath(k)), 'Verbose', false);
    %          end
    %
    %          % All visits whose field overlaps an RA x Dec box (Coo = []):
    %          R = pipeline.last.load.findVisitsByCoo(DataPath, [], ...
    %                   'FieldTable', FT, ...
    %                   'RARange', [200 230], 'DecRange', [30 50]);
    %
    %          % RA range only (any Dec):
    %          R = pipeline.last.load.findVisitsByCoo(DataPath, [], ...
    %                   'FieldTable', FT, 'RARange', [200 230]);

    arguments
        DataPath          {mustBeTextScalar}
        Coo               double                            = []
        Args.FieldTable                                     = ''
        Args.RARange      double                            = []
        Args.DecRange     double                            = []
        Args.Tol          (1,1) double {mustBeNonnegative}  = 1e-6
        Args.Recursive    logical                           = true
        Args.FileType     (1,:) char ...
            {mustBeMember(Args.FileType, {'any','coadd','proc'})} = 'any'
        Args.Verbose      logical                           = true
    end

    DataPath = char(DataPath);
    if ~isfolder(DataPath)
        error('findVisitsByCoo:NoDir', 'DataPath not found: %s', DataPath);
    end
    if ~isempty(Coo) && size(Coo, 2) ~= 2
        error('findVisitsByCoo:BadCoo', ...
            'Coo must be an Nx2 [RA Dec] array (deg), or [] when using RARange/DecRange.');
    end
    if ~isempty(Args.RARange) && (numel(Args.RARange) ~= 2 || ...
            Args.RARange(1) > Args.RARange(2))
        error('findVisitsByCoo:BadRARange', ...
            'RARange must be [RAmin RAmax] with RAmin <= RAmax (deg, no 0/360 wrap).');
    end
    if ~isempty(Args.DecRange) && (numel(Args.DecRange) ~= 2 || ...
            Args.DecRange(1) > Args.DecRange(2))
        error('findVisitsByCoo:BadDecRange', ...
            'DecRange must be [DecMin DecMax] with DecMin <= DecMax (deg).');
    end
    UseRange = ~isempty(Args.RARange) || ~isempty(Args.DecRange);
    if isempty(Coo) && ~UseRange
        error('findVisitsByCoo:NoTarget', ...
            'Provide Coo (Nx2 point coordinates) and/or RARange / DecRange.');
    end

    % --- field table ---------------------------------------------------
    FT  = i_loadFieldTable(Args.FieldTable);
    Nfl = height(FT);
    Tol = Args.Tol;     % degrees; closes the ~1e-14 rounding gaps at edges

    % --- target fields: point-in-footprint and/or range overlap --------
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

    if UseRange
        % unconstrained axis defaults to the full sphere span
        RR = [0 360];   if ~isempty(Args.RARange);  RR = Args.RARange(:).';  end
        DR = [-90 90];  if ~isempty(Args.DecRange); DR = Args.DecRange(:).'; end
        % keep every field whose footprint box overlaps the requested box
        Ov = (FT.MinRA  - Tol <= RR(2)) & (FT.MaxRA  + Tol >= RR(1)) & ...
             (FT.MinDec - Tol <= DR(2)) & (FT.MaxDec + Tol >= DR(1));
        TargetFields = [TargetFields; find(Ov)];
    end

    TargetFields = unique(TargetFields);
    if isempty(TargetFields)
        error('findVisitsByCoo:NoField', ...
            'No field footprint matches the requested coordinates / ranges.');
    end
    if Args.Verbose
        if numel(TargetFields) <= 20
            fprintf('findVisitsByCoo: target -> %d field(s): %s\n', ...
                numel(TargetFields), mat2str(TargetFields(:).'));
        else
            fprintf('findVisitsByCoo: target -> %d field(s)\n', numel(TargetFields));
        end
    end

    % --- discover visit directories (field-restricted, fast) ----------
    % Glob only files whose name carries a target field number, so the
    % cost scales with the matching visits, not the whole DataPath tree.
    [VisitDirs, PeekFiles] = i_discoverVisits(DataPath, TargetFields, ...
        Args.FileType, Args.Recursive, Args.Verbose);
    if isempty(VisitDirs)
        error('findVisitsByCoo:NoFiles', ...
            'No LAST files for the target field(s) found under %s', DataPath);
    end

    % --- parse the field number of each visit (AstroFileName) ----------
    AFN     = AstroFileName(PeekFiles);
    FieldNo = str2double(string(AFN.FieldID));
    FieldNo = FieldNo(:);
    ProjN   = string(AFN.ProjName);
    ProjN   = ProjN(:);
    JD      = AFN.julday();
    JD      = JD(:);

    Keep = ismember(FieldNo, TargetFields) & FieldNo >= 1 & FieldNo <= Nfl;
    if ~any(Keep)
        if Args.Verbose
            fprintf('findVisitsByCoo: 0 of %d visit dir(s) match the target field(s).\n', ...
                numel(VisitDirs));
        end
        Result = i_emptyResult();
        return;
    end

    Idx = find(Keep);
    Result = table( ...
        string(VisitDirs(Idx)), FieldNo(Idx), ...
        FT.RA(FieldNo(Idx)),    FT.Dec(FieldNo(Idx)), ...
        ProjN(Idx),             JD(Idx), ...
        'VariableNames', ...
        {'VisitPath','FieldNumber','FieldRA','FieldDec','ProjName','JD'});
    Result = sortrows(Result, {'FieldNumber','JD'});

    if Args.Verbose
        fprintf('findVisitsByCoo: %d visit(s) match.\n', height(Result));
    end
end

% =========================================================================
function FT = i_loadFieldTable(Spec)
    % Resolve the LAST_FieldsID field table from a path or a ready table.
    if istable(Spec)
        FT = Spec;
    elseif (ischar(Spec) || isstring(Spec)) && ~isempty(char(Spec)) ...
            && isfile(char(Spec))
        FT = readtable(char(Spec));
    else
        error('findVisitsByCoo:NoFieldTable', ...
            ['Args.FieldTable must be a path to the LAST_FieldsID file, ', ...
             'or a table already read from it (columns RA, Dec, MinRA, ', ...
             'MaxRA, MinDec, MaxDec).']);
    end
    Need = {'RA','Dec','MinRA','MaxRA','MinDec','MaxDec'};
    Miss = Need(~ismember(Need, FT.Properties.VariableNames));
    if ~isempty(Miss)
        error('findVisitsByCoo:BadFieldTable', ...
            'Field table is missing column(s): %s', strjoin(Miss, ', '));
    end
end

% =========================================================================
function [VisitDirs, PeekFiles] = i_discoverVisits(DataPath, TargetFields, ...
        FileType, Recursive, Verbose)
    % Field-restricted visit discovery.
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
    NamePat = strings(1, numel(TargetFields));
    for K = 1:numel(TargetFields)
        if isempty(Lev)
            NamePat(K) = sprintf('LAST*_%d_*_sci_*.fits', TargetFields(K));
        else
            NamePat(K) = sprintf('LAST*_%d_*_sci_%s_*.fits', TargetFields(K), Lev);
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
            error('findVisitsByCoo:FindFailed', ...
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
        fprintf('findVisitsByCoo: %d candidate file(s) -> %d visit dir(s)\n', ...
            numel(Files), numel(VisitDirs));
    end
end

% =========================================================================
function Q = i_shellQuote(P)
    % Single-quote a path for /bin/sh, escaping embedded single quotes.
    Q = ['''', strrep(char(P), '''', '''\'''''), ''''];
end

% =========================================================================
function T = i_emptyResult()
    % Zero-row Result table with the documented column types.
    T = table('Size', [0 6], ...
        'VariableTypes', {'string','double','double','double','string','double'}, ...
        'VariableNames', {'VisitPath','FieldNumber','FieldRA','FieldDec', ...
                          'ProjName','JD'});
end
