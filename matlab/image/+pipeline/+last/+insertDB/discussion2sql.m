function [SQL, Spec, Info] = discussion2sql(Source, Args)
    % Build a ClickHouse CREATE TABLE script from a table-design discussion text
    %     The discussion text (e.g. AstroPack discussions #1409 image_visit,
    %     #1413 image_proc) holds one markdown row per header keyword:
    %       | `KEYWORD` | ✅/❌ | `Type` | default | resolution | ... |
    %     Only the ✅ rows become columns. Image-section keywords
    %     (type "4 × `Int16`") become 4 columns <kw>_xmin/_xmax/_ymin/_ymax.
    %     Rows of the "Columns computed or corrected at ingestion" table
    %     marked "(new, ...)" are added as computed columns.
    %     The table name is taken from "a new ClickHouse table, `name`" and
    %     the sorting key from "ORDER BY name" (and "PARTITION BY name", if
    %     the text has it).
    %     A discussion that proposes two tables in one column list (e.g. #1411
    %     src_visit and src_proc, catalogue columns) is read with 'TableTag':
    %     a ✅ row is kept if its range cell (6th) has "<TableTag>:" or names
    %     no table; the columns are catalogue columns (Kind "catalog", Keyword
    %     = the catalogue column name, renamed columns from "`old`, which is an
    %     SQL keyword and becomes `new`"); and the computed columns are read
    %     from the "Columns added at ingestion" table, where `name` (`table`)
    %     keeps a column for that table only, and of names joined by "/" the
    %     one containing TableTag is kept.
    %     Data-skipping indexes and projections are read from the optional
    %     "### Indexes" and "### Projections" tables of the text:
    %       | `name` | `expression` | `type` | granularity | table | purpose |
    %       | `name` | `SELECT ... ORDER BY col` | table | purpose |
    %     A row with an empty table cell is used for every table of the
    %     discussion, otherwise only for the named table. The index type must be
    %     minmax, set(N), bloom_filter[(p)], tokenbf_v1(...) or ngrambf_v1(...),
    %     and every column named in an expression or query must be a column of
    %     the table. They are written into CREATE TABLE and, for a table that
    %     already exists, into a separate ALTER script (ADD + MATERIALIZE).
    % Input  : - Discussion number in EranOfek/AstroPack (fetched with gh),
    %            or a file name of the discussion markdown text.
    %          * ...,key,val,...
    %            'DbName'    - Database name for the CREATE statement.
    %                          Default is 'last_test'.
    %            'TableName' - Table name. If empty, take it from the text.
    %                          Default is [].
    %            'Engine'    - Table engine. Default is 'MergeTree'.
    %            'OrderBy'   - Sorting key. If empty, take it from the text.
    %                          Default is [].
    %            'TableTag'  - For two-table discussions: the tag of the table in the
    %                          range cells ("visit" or "proc"). Requires 'TableName'.
    %                          Default is "" (one-table discussion).
    %            'OutFile'   - If not empty, write the SQL script to this file.
    %                          Default is [].
    %            'Indexes'   - Include the indexes and projections of the text.
    %                          false: the table without them, e.g. as a baseline
    %                          for comparing queries. Default is true.
    %            'IndexFile' - If not empty, write the ALTER script that adds the
    %                          indexes and projections to an existing table to
    %                          this file. Default is [].
    %            'Materialize' - Add MATERIALIZE lines to the ALTER script, which
    %                          build the indexes for the rows already in the table.
    %                          false: only rows inserted later are indexed.
    %                          Default is true.
    %            'Repo'      - GitHub owner/name. Default is 'EranOfek/AstroPack'.
    % Output : - SQL script (char).
    %          - Column specification table, one row per DB column:
    %            Column, Keyword ("" for computed columns), Type, Default,
    %            Kind ("header", "sec4", "catalog" or "computed"), SecPart (1..4 for sec4).
    %          - Structure with .TableName, .OrderBy, .PartitionBy, .Text (discussion text),
    %            .Indexes (table: Name, Expression, Type, Granularity),
    %            .Projections (table: Name, Query) and .AlterSQL (the ALTER script, char).
    % Author : A.M. Krassilchtchikov (2026 Oct)
    % Example: [SQL, Spec] = pipeline.last.insertDB.discussion2sql(1409);
    %          [SQL, Spec] = pipeline.last.insertDB.discussion2sql('~/v1t/image_proc/discussion_proc.md', 'OutFile','image_proc.sql');
    %          [SQL, Spec] = pipeline.last.insertDB.discussion2sql(1411, 'TableName','src_proc', 'TableTag',"proc");
    %          SQL = pipeline.last.insertDB.discussion2sql('image_visit_1409.md', 'OutFile','image_visit.sql', 'IndexFile','image_visit_indexes.sql');

    arguments
        Source
        Args.DbName    = 'last_test';
        Args.TableName = [];
        Args.Engine    = 'MergeTree';
        Args.OrderBy   = [];
        Args.TableTag  = "";
        Args.OutFile   = [];
        Args.Indexes   logical = true;
        Args.IndexFile = [];
        Args.Materialize logical = true;
        Args.Repo      = 'EranOfek/AstroPack';
    end

    Text = readDiscussion(Source, Args.Repo);
    IsCat = strlength(string(Args.TableTag)) > 0;
    if IsCat && isempty(Args.TableName)
        error('discussion2sql:noTable', 'TableTag needs TableName');
    end

    % table name and sorting key
    if isempty(Args.TableName)
        Tok = regexp(Text, 'a new ClickHouse table, `(\w+)`', 'tokens', 'once');
        if isempty(Tok)
            error('discussion2sql:noTable', 'Table name not found in the discussion text');
        end
        Args.TableName = Tok{1};
    end
    KeyText = regexprep(Text, '`SELECT[^`]*`', '');   % not the ORDER BY of a projection query
    if isempty(Args.OrderBy)
        Tok = regexp(KeyText, 'ORDER BY (\w+)', 'tokens', 'once');
        if isempty(Tok)
            error('discussion2sql:noOrderBy', 'ORDER BY not found in the discussion text');
        end
        Args.OrderBy = Tok{1};
    end
    Tok = regexp(KeyText, 'PARTITION BY (\w+)', 'tokens', 'once');
    if isempty(Tok)
        PartitionBy = "";
    else
        PartitionBy = string(Tok{1});
    end

    % catalogue columns renamed because their name is an SQL keyword
    Renamed = regexp(Text, '`(\w+)`, which is an SQL keyword and becomes `(\w+)`', 'tokens');

    Lines = splitlines(Text);

    % keyword rows: | `KW` | ✅ | type | default | ...
    Column = strings(0,1); Keyword = strings(0,1); Type = strings(0,1);
    Default = strings(0,1); Kind = strings(0,1); SecPart = zeros(0,1);
    SecSuffix = ["_xmin", "_xmax", "_ymin", "_ymax"];
    for Il = 1:numel(Lines)
        Tok = regexp(Lines{Il}, '^\| `([^`]+)` \| (✅|❌) \| ([^|]*) \| ([^|]*) \|', 'tokens', 'once');
        if isempty(Tok) || ~strcmp(Tok{2}, '✅')
            continue
        end
        Kw   = string(Tok{1});
        Name = lower(strrep(Kw, '-', '_'));
        Def  = sqlDefault(strtrim(Tok{4}));
        if IsCat
            % keep the row if its range cell names this table, or no table
            Cells = strsplit(Lines{Il}, '|');
            Range = string(Cells{7});
            if contains(Range, ["visit:", "proc:"]) && ~contains(Range, string(Args.TableTag) + ":")
                continue
            end
            for Ir = 1:numel(Renamed)
                if Name == Renamed{Ir}{2}
                    Kw = string(Renamed{Ir}{1});
                end
            end
            T = regexp(Tok{3}, '`([^`]+)`', 'tokens', 'once');
            Column(end+1,1) = Name; Keyword(end+1,1) = Kw; Type(end+1,1) = string(T{1}); %#ok<AGROW>
            Default(end+1,1) = Def; Kind(end+1,1) = "catalog"; SecPart(end+1,1) = 0; %#ok<AGROW>
        elseif contains(Tok{3}, '_xmin')
            % image section: 4 x Int16
            for Ip = 1:4
                Column(end+1,1) = Name + SecSuffix(Ip); Keyword(end+1,1) = Kw; Type(end+1,1) = "Int16"; %#ok<AGROW>
                Default(end+1,1) = Def; Kind(end+1,1) = "sec4"; SecPart(end+1,1) = Ip; %#ok<AGROW>
            end
        else
            T = regexp(Tok{3}, '`([^`]+)`', 'tokens', 'once');
            Column(end+1,1) = Name; Keyword(end+1,1) = Kw; Type(end+1,1) = string(T{1}); %#ok<AGROW>
            Default(end+1,1) = Def; Kind(end+1,1) = "header"; SecPart(end+1,1) = 0; %#ok<AGROW>
        end
    end
    if isempty(Column)
        error('discussion2sql:noRows', 'No ✅ keyword rows found in the discussion text');
    end

    % computed columns: | `name` (new, not in the header...) | `Type` | ...
    if IsCat
        [Column, Keyword, Type, Default, Kind, SecPart] = addedColumns(Lines, string(Args.TableName), string(Args.TableTag), ...
                                                              Column, Keyword, Type, Default, Kind, SecPart);
    else
        for Il = 1:numel(Lines)
            Tok = regexp(Lines{Il}, '^\| `(\w+)` \(new[^|]*\| `([^`]+)` \|', 'tokens', 'once');
            if isempty(Tok)
                continue
            end
            Column(end+1,1) = string(Tok{1}); Keyword(end+1,1) = ""; Type(end+1,1) = string(Tok{2}); %#ok<AGROW>
            Default(end+1,1) = typeDefault(Tok{2}); Kind(end+1,1) = "computed"; SecPart(end+1,1) = 0; %#ok<AGROW>
        end
    end

    if numel(unique(Column)) < numel(Column)
        error('discussion2sql:dupColumn', 'Duplicate column names in the discussion text');
    end
    if ~ismember(Args.OrderBy, Column)
        error('discussion2sql:badOrderBy', 'ORDER BY column %s is not a ✅ column', Args.OrderBy);
    end

    Spec = table(Column, Keyword, Type, Default, Kind, SecPart);

    % indexes and projections
    Indexes     = readIndexes(Lines, string(Args.TableName), Column);
    Projections = readProjections(Lines, string(Args.TableName), Column);
    if ~Args.Indexes
        Indexes     = Indexes([],:);
        Projections = Projections([],:);
    end
    if numel(unique([Indexes.Name; Projections.Name])) < height(Indexes) + height(Projections)
        error('discussion2sql:dupIndex', 'Duplicate index or projection names in the discussion text');
    end

    % SQL script
    Ncol = numel(Column);
    ColLines = strings(Ncol,1);
    for Icol = 1:Ncol
        if Kind(Icol) == "computed"
            Comment = "computed at ingestion";
        elseif Kind(Icol) == "catalog"
            Comment = "catalogue column " + Keyword(Icol);
        else
            Comment = "FITS keyword " + Keyword(Icol);
        end
        ColLines(Icol) = sprintf('    `%s` %s DEFAULT %s COMMENT ''%s''', Column(Icol), Type(Icol), Default(Icol), Comment);
    end
    FullName = sprintf('%s.%s', Args.DbName, Args.TableName);
    IdxDef   = compose("INDEX %s %s TYPE %s GRANULARITY %d", Indexes.Name, Indexes.Expression, Indexes.Type, Indexes.Granularity);
    ProjDef  = compose("PROJECTION %s (%s)", Projections.Name, Projections.Query);
    ColLines = [ColLines; "    " + IdxDef(:); "    " + ProjDef(:)];
    AlterLines = strings(0,1);
    for Ii = 1:height(Indexes)
        AlterLines(end+1,1) = sprintf('ALTER TABLE %s ADD INDEX IF NOT EXISTS %s;', FullName, extractAfter(IdxDef(Ii), "INDEX ")); %#ok<AGROW>
        if Args.Materialize
            AlterLines(end+1,1) = sprintf('ALTER TABLE %s MATERIALIZE INDEX %s;', FullName, Indexes.Name(Ii)); %#ok<AGROW>
        end
    end
    for Ip = 1:height(Projections)
        AlterLines(end+1,1) = sprintf('ALTER TABLE %s ADD PROJECTION IF NOT EXISTS %s;', FullName, extractAfter(ProjDef(Ip), "PROJECTION ")); %#ok<AGROW>
        if Args.Materialize
            AlterLines(end+1,1) = sprintf('ALTER TABLE %s MATERIALIZE PROJECTION %s;', FullName, Projections.Name(Ip)); %#ok<AGROW>
        end
    end
    if isempty(AlterLines)
        AlterSQL = '';
    else
        AlterSQL = char(strjoin(AlterLines, newline) + newline);
    end
    if PartitionBy == ""
        Partition = '';
    else
        if ~ismember(PartitionBy, Column)
            error('discussion2sql:badPartitionBy', 'PARTITION BY column %s is not a ✅ column', PartitionBy);
        end
        Partition = sprintf('PARTITION BY %s\n', PartitionBy);
    end
    SQL = sprintf('CREATE TABLE IF NOT EXISTS %s.%s\n(\n%s\n)\nENGINE = %s\n%sORDER BY %s;\n', ...
                  Args.DbName, Args.TableName, strjoin(ColLines, sprintf(',\n')), Args.Engine, Partition, Args.OrderBy);

    if ~isempty(Args.OutFile)
        FID = fopen(Args.OutFile, 'w', 'n', 'UTF-8');
        fprintf(FID, '%s', SQL);
        fclose(FID);
    end
    if ~isempty(Args.IndexFile)
        if isempty(AlterSQL)
            warning('discussion2sql:noIndexes', 'No indexes or projections for %s; %s not written', FullName, Args.IndexFile);
        else
            FID = fopen(Args.IndexFile, 'w', 'n', 'UTF-8');
            fprintf(FID, '%s', AlterSQL);
            fclose(FID);
        end
    end

    Info.TableName = Args.TableName;
    Info.OrderBy   = Args.OrderBy;
    Info.PartitionBy = PartitionBy;
    Info.Text      = Text;
    Info.Indexes   = Indexes;
    Info.Projections = Projections;
    Info.AlterSQL  = AlterSQL;
end

% internal functions

function Text = readDiscussion(Source, Repo)
    % Read the discussion text from GitHub (number) or from a file (name)
    if isnumeric(Source)
        Parts = strsplit(Repo, '/');
        TmpFile = [tempname '.md'];
        Cmd = sprintf(['gh api graphql -f query=''{repository(owner:"%s",name:"%s"){discussion(number:%d){body}}}'' ' ...
                       '--jq .data.repository.discussion.body > %s'], Parts{1}, Parts{2}, Source, TmpFile);
        [Status, Out] = system(Cmd);
        if Status ~= 0
            error('discussion2sql:gh', 'gh failed for discussion %d: %s', Source, Out);
        end
        FileName = TmpFile;
    else
        FileName = char(Source);
        TmpFile = [];
    end
    FID  = fopen(FileName, 'r', 'n', 'UTF-8');
    if FID < 0
        error('discussion2sql:read', 'Cannot read %s', FileName);
    end
    Text = fread(FID, [1 Inf], '*char');
    fclose(FID);
    Text = strrep(Text, char(13), '');
    if ~isempty(TmpFile)
        delete(TmpFile);
    end
end

function [Column, Keyword, Type, Default, Kind, SecPart] = addedColumns(Lines, TableName, TableTag, Column, Keyword, Type, Default, Kind, SecPart)
    % Computed columns of a two-table discussion, from the "Columns added at ingestion" table:
    %   | `a` (`tab1`), `b` (`tab2`) | `Type` | ...   or   | `a` / `b` | `Type` | ...   or   | `a`, `b` | `T1`, `T2` | ...
    Start = find(startsWith(Lines, '### Columns added at ingestion'), 1);
    if isempty(Start)
        error('discussion2sql:noAdded', 'No "Columns added at ingestion" section in the discussion text');
    end
    for Il = Start+1:numel(Lines)
        if startsWith(Lines{Il}, '###')
            break
        end
        Tok = regexp(Lines{Il}, '^\| (`[^|]*) \| (`[^|]*`) \|', 'tokens', 'once');
        if isempty(Tok)
            continue
        end
        Types = string(regexp(Tok{2}, '`([^`]+)`', 'tokens'));
        if contains(Tok{1}, '/')
            Parts = strtrim(strsplit(Tok{1}, '/'));
            Parts = Parts(contains(Parts, TableTag));
        else
            Parts = strtrim(strsplit(Tok{1}, ','));
        end
        for Ip = 1:numel(Parts)
            Nm  = regexp(Parts{Ip}, '^`(\w+)`', 'tokens', 'once');
            Tag = regexp(Parts{Ip}, '\(`(\w+)`\)', 'tokens', 'once');
            if isempty(Nm) || (~isempty(Tag) && string(Tag{1}) ~= TableName)
                continue
            end
            T = Types(min(Ip, numel(Types)));
            Column(end+1,1) = string(Nm{1}); Keyword(end+1,1) = ""; Type(end+1,1) = T; %#ok<AGROW>
            Default(end+1,1) = typeDefault(T); Kind(end+1,1) = "computed"; SecPart(end+1,1) = 0; %#ok<AGROW>
        end
    end
end

function Idx = readIndexes(Lines, TableName, Column)
    % Data-skipping indexes of the table, from the "### Indexes" table:
    %   | `name` | `expression` | `type` | granularity | table | purpose |
    Name = strings(0,1); Expression = strings(0,1); Type = strings(0,1); Granularity = zeros(0,1);
    Rows = sectionRows(Lines, '### Indexes');
    for Ir = 1:numel(Rows)
        C = Rows{Ir};
        if numel(C) < 5
            error('discussion2sql:badIndex', 'An index row needs at least 5 cells: %s', strjoin(C, ' | '));
        end
        if ~forTable(C{5}, TableName)
            continue
        end
        N = codeCell(C{1}, 'index name');
        if isempty(regexp(N, '^\w+$', 'once'))
            error('discussion2sql:badIndex', 'Bad index name: %s', N);
        end
        E = codeCell(C{2}, 'index expression');
        T = codeCell(C{3}, 'index type');
        if isempty(regexp(T, '^(minmax|set\(\d+\)|bloom_filter(\(0?\.\d+\))?|tokenbf_v1\([^)]*\)|ngrambf_v1\([^)]*\))$', 'once'))
            error('discussion2sql:badIndex', 'Index %s: unknown type %s', N, T);
        end
        G = str2double(C{4});
        if ~(isfinite(G) && G >= 1 && G == round(G))
            error('discussion2sql:badIndex', 'Index %s: granularity must be a positive integer: %s', N, C{4});
        end
        checkColumns(E, Column, "Index " + N);
        Name(end+1,1) = N; Expression(end+1,1) = E; Type(end+1,1) = T; Granularity(end+1,1) = G; %#ok<AGROW>
    end
    Idx = table(Name, Expression, Type, Granularity);
end

function Proj = readProjections(Lines, TableName, Column)
    % Projections of the table, from the "### Projections" table:
    %   | `name` | `SELECT ... ORDER BY col` | table | purpose |
    Name = strings(0,1); Query = strings(0,1);
    Rows = sectionRows(Lines, '### Projections');
    for Ir = 1:numel(Rows)
        C = Rows{Ir};
        if numel(C) < 3
            error('discussion2sql:badProjection', 'A projection row needs at least 3 cells: %s', strjoin(C, ' | '));
        end
        if ~forTable(C{3}, TableName)
            continue
        end
        N = codeCell(C{1}, 'projection name');
        if isempty(regexp(N, '^\w+$', 'once'))
            error('discussion2sql:badProjection', 'Bad projection name: %s', N);
        end
        Q = codeCell(C{2}, 'projection query');
        if isempty(regexpi(Q, '^SELECT\s.*\s(ORDER|GROUP) BY\s', 'once'))
            error('discussion2sql:badProjection', 'Projection %s: the query must be SELECT ... ORDER BY or GROUP BY: %s', N, Q);
        end
        checkColumns(Q, Column, "Projection " + N);
        Name(end+1,1) = N; Query(end+1,1) = Q; %#ok<AGROW>
    end
    Proj = table(Name, Query);
end

function Rows = sectionRows(Lines, Heading)
    % Cells of the table rows (first cell in backticks) of a "### Heading" section
    Rows = {};
    Start = find(strcmp(strtrim(Lines), Heading));
    if isempty(Start)
        return
    elseif numel(Start) > 1
        error('discussion2sql:dupSection', 'More than one "%s" section in the discussion text', Heading);
    end
    for Il = Start+1:numel(Lines)
        if startsWith(Lines{Il}, '#')
            break
        end
        if startsWith(Lines{Il}, '| `')
            C = strsplit(Lines{Il}, '|');
            Rows{end+1} = strtrim(string(C(2:end-1))); %#ok<AGROW>
        end
    end
end

function Keep = forTable(Cell, TableName)
    % An empty table cell means every table of the discussion
    Tab = strtrim(erase(string(Cell), '`'));
    Keep = Tab == "" || Tab == TableName;
end

function Str = codeCell(Cell, What)
    % Text of a cell written in backticks
    Tok = regexp(Cell, '^`([^`]+)`$', 'tokens', 'once');
    if isempty(Tok)
        error('discussion2sql:badCell', 'The %s must be written in backticks: %s', What, Cell);
    end
    Str = strtrim(string(Tok{1}));
end

function checkColumns(Expr, Column, What)
    % Every name in an expression or query that is not a function, an SQL word,
    % an alias or a quoted text must be a column of the table
    SqlWords = ["select", "order", "group", "by", "as", "and", "or", "not", "asc", "desc", "in", "is", "null", "true", "false"];
    Expr = regexprep(Expr, '''[^'']*''', '');      % quoted texts
    Expr = regexprep(Expr, '(?i)\<AS\s+\w+', '');  % aliases
    Words = string(regexp(Expr, '[\w.]+\s*\(?', 'match'));
    Words = Words(~endsWith(Words, '(') & ~contains(Words, '.') & ~startsWith(Words, digitsPattern));  % functions, numbers
    Words = strtrim(Words);
    Words = Words(~ismember(lower(Words), SqlWords));
    Bad =Words(~ismember(Words, Column));
    if ~isempty(Bad)
        error('discussion2sql:badColumn', '%s: not a column of the table: %s', What, strjoin(unique(Bad), ', '));
    end
end

function Def = sqlDefault(Str)
    % Default cell of the discussion table -> SQL default value
    Str = string(Str);
    if Str == "nan"
        Def = "nan";
    elseif Str == "''"
        Def = "''";
    elseif startsWith(Str, "0")
        Def = "0";       % '0' and '0 (1970-01-01)'
    else
        error('discussion2sql:badDefault', 'Unknown default value: %s', Str);
    end
end

function Def = typeDefault(Type)
    % Default value of a computed column, by type
    if startsWith(Type, 'Float')
        Def = "nan";
    elseif contains(Type, 'String')
        Def = "''";
    else
        Def = "0";
    end
end
