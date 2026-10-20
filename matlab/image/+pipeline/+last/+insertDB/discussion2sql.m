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
    %     the sorting key from "ORDER BY name".
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
    %            'OutFile'   - If not empty, write the SQL script to this file.
    %                          Default is [].
    %            'Repo'      - GitHub owner/name. Default is 'EranOfek/AstroPack'.
    % Output : - SQL script (char).
    %          - Column specification table, one row per DB column:
    %            Column, Keyword ("" for computed columns), Type, Default,
    %            Kind ("header", "sec4" or "computed"), SecPart (1..4 for sec4).
    %          - Structure with .TableName, .OrderBy, .Text (discussion text).
    % Author : A.M. Krassilchtchikov (2026 Oct)
    % Example: [SQL, Spec] = pipeline.last.insertDB.discussion2sql(1409);
    %          [SQL, Spec] = pipeline.last.insertDB.discussion2sql('~/v1t/image_proc/discussion_proc.md', 'OutFile','image_proc.sql');

    arguments
        Source
        Args.DbName    = 'last_test';
        Args.TableName = [];
        Args.Engine    = 'MergeTree';
        Args.OrderBy   = [];
        Args.OutFile   = [];
        Args.Repo      = 'EranOfek/AstroPack';
    end

    Text = readDiscussion(Source, Args.Repo);

    % table name and sorting key
    if isempty(Args.TableName)
        Tok = regexp(Text, 'a new ClickHouse table, `(\w+)`', 'tokens', 'once');
        if isempty(Tok)
            error('discussion2sql:noTable', 'Table name not found in the discussion text');
        end
        Args.TableName = Tok{1};
    end
    if isempty(Args.OrderBy)
        Tok = regexp(Text, 'ORDER BY (\w+)', 'tokens', 'once');
        if isempty(Tok)
            error('discussion2sql:noOrderBy', 'ORDER BY not found in the discussion text');
        end
        Args.OrderBy = Tok{1};
    end

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
        if contains(Tok{3}, '_xmin')
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
    for Il = 1:numel(Lines)
        Tok = regexp(Lines{Il}, '^\| `(\w+)` \(new[^|]*\| `([^`]+)` \|', 'tokens', 'once');
        if isempty(Tok)
            continue
        end
        Column(end+1,1) = string(Tok{1}); Keyword(end+1,1) = ""; Type(end+1,1) = string(Tok{2}); %#ok<AGROW>
        Default(end+1,1) = typeDefault(Tok{2}); Kind(end+1,1) = "computed"; SecPart(end+1,1) = 0; %#ok<AGROW>
    end

    if numel(unique(Column)) < numel(Column)
        error('discussion2sql:dupColumn', 'Duplicate column names in the discussion text');
    end
    if ~ismember(Args.OrderBy, Column)
        error('discussion2sql:badOrderBy', 'ORDER BY column %s is not a ✅ column', Args.OrderBy);
    end

    Spec = table(Column, Keyword, Type, Default, Kind, SecPart);

    % SQL script
    Ncol = numel(Column);
    ColLines = strings(Ncol,1);
    for Icol = 1:Ncol
        if Kind(Icol) == "computed"
            Comment = "computed at ingestion";
        else
            Comment = "FITS keyword " + Keyword(Icol);
        end
        ColLines(Icol) = sprintf('    `%s` %s DEFAULT %s COMMENT ''%s''', Column(Icol), Type(Icol), Default(Icol), Comment);
    end
    SQL = sprintf('CREATE TABLE IF NOT EXISTS %s.%s\n(\n%s\n)\nENGINE = %s\nORDER BY %s;\n', ...
                  Args.DbName, Args.TableName, strjoin(ColLines, sprintf(',\n')), Args.Engine, Args.OrderBy);

    if ~isempty(Args.OutFile)
        FID = fopen(Args.OutFile, 'w', 'n', 'UTF-8');
        fprintf(FID, '%s', SQL);
        fclose(FID);
    end

    Info.TableName = Args.TableName;
    Info.OrderBy   = Args.OrderBy;
    Info.Text      = Text;
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
