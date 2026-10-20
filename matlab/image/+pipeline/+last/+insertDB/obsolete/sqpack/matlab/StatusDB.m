classdef StatusDB < handle
%STATUSDB  Per-folder status table in PostgreSQL (Database Toolbox native interface).
%
%   One row per leaf folder, keyed by its path relative to the data root:
%       folder 'w2026/06/01/visit_001' | status (free text) | updated | updated_by
%   Every change is also recorded in <table>_log by a trigger.
%
%   st = StatusDB('Server','euclid', 'Database','obs', 'User','pipeline', 'Root','/data');
%   st.createSchema();                                  % once (idempotent)
%   st.write('w2026/06/01/visit_001', 'done');
%   s   = st.read('w2026/06/01/visit_001');
%   T   = st.asTable('w2026/06/01');                    % table, RowNames = folder
%   got = st.claim([], 'queued', 'running', 'Limit', 10);   % parallel-safe work queue
%   rep = st.importStatusFiles('/data', 'w2026/06', 'Originals', 'rename');
%   st.check();                                         % diagnostics + self test
%   clear st                                            % closes the connection
%
%   Keys may be relative ('w2026/06/01/visit_001'), autofs-style
%   ('w2026_06_01_visit_001') or absolute under Root ('/data/w2026/06/01/visit_001').
%
%   Constructor options (name/value):
%     Server, Port, Database, User   default PGHOST, PGPORT, PGDATABASE, PGUSER/$USER
%     Password                        else getSecret(PasswordSecret), else PGPASSWORD
%     PasswordSecret                  vault name, default "pgstatus" (setSecret, R2024a+)
%     DataSource                      saved Database Toolbox data source instead of Server/...
%     Connection                      reuse an open connection (not closed by this object)
%     Table      'visit_status'       lower-case [schema.]name
%     Root       ''                   lets methods accept absolute paths as keys
%     Depth      4                    levels of a folder key (wYYYY/MM/DD/visit); 0 = any
%     Debug      false                print every SQL statement with timing
%                                     (or setenv('PGSTATUS_DEBUG','1'))
%
%   Methods:
%     createSchema  read  write  list  asTable  writeTable  claim  remove
%     history  summary  importStatusFiles  check  folderKey  close
%   help StatusDB.<method> for details.
%
%   Quoting: the native interface has no prepared statements (JDBC only), so
%   values are quoted by the class. The constructor refuses to run unless the
%   server has standard_conforming_strings = on (the PostgreSQL default).

    properties (SetAccess = private)
        conn                    % Database Toolbox connection object
        table                   % status table name
        logTable                % history table name (<table>_log)
        root                    % data root for absolute-path keys ('' = none)
        depth                   % number of levels in a folder key
        serverVersion           % e.g. 160015
    end
    properties
        debug = false           % true: print each SQL statement and its time
    end
    properties (Access = private)
        ownsConn = true
    end

    methods
        % ================================================================ lifecycle
        function obj = StatusDB(varargin)
            p = inputParser;
            p.addParameter('Server', envor('PGHOST', 'localhost'));
            p.addParameter('Port', str2double(envor('PGPORT', '5432')));
            p.addParameter('Database', envor('PGDATABASE', ''));
            p.addParameter('User', envor('PGUSER', envor('USER', '')));
            p.addParameter('Password', []);
            p.addParameter('PasswordSecret', 'pgstatus');
            p.addParameter('DataSource', '');
            p.addParameter('Connection', []);
            p.addParameter('Table', 'visit_status');
            p.addParameter('Root', '');
            p.addParameter('Depth', 4);
            p.addParameter('Debug', false);
            p.parse(varargin{:});
            o = p.Results;

            obj.table    = pg_ident(o.Table);
            obj.logTable = [obj.table '_log'];
            obj.root     = regexprep(sq_char(o.Root), '/+$', '');
            obj.depth    = o.Depth;
            obj.debug    = logical(o.Debug);

            if ~isempty(o.Connection)
                obj.conn = o.Connection;
                obj.ownsConn = false;
            else
                obj.conn = StatusDB.openConnection(o);
                obj.ownsConn = true;
            end

            R = pg_query(obj, ['SELECT current_setting(''standard_conforming_strings'') AS scs, ' ...
                               'current_setting(''server_version_num'') AS ver'], {'scs', 'ver'});
            if isempty(R.scs) || ~strcmp(R.scs{1}, 'on')
                obj.close();
                error('statusdb:unsafe', ['standard_conforming_strings is not "on" on this ' ...
                      'server; StatusDB refuses to run (string quoting would be unsafe).']);
            end
            obj.serverVersion = str2double(R.ver{1});
            if obj.serverVersion < 110000
                warning('statusdb:oldServer', ...
                    'PostgreSQL %d is older than 11; the schema triggers need 11+.', obj.serverVersion);
            end
        end

        function close(obj)
        %CLOSE  Close the connection (only if this object opened it).
            if obj.ownsConn && ~isempty(obj.conn)
                try
                    close(obj.conn);
                catch
                end
            end
            obj.conn = [];
        end

        function delete(obj)
            obj.close();
        end

        function createSchema(obj, varargin)
        %CREATESCHEMA  Create/update table, indexes, triggers (idempotent).
        %   st.createSchema()                    with history log (default)
        %   st.createSchema('History', false)    no <table>_log
        %   st.createSchema('Print', true)       print the SQL instead of running it
            p = inputParser;
            p.addParameter('History', true);
            p.addParameter('Print', false);
            p.parse(varargin{:});
            stmts = pg_sql_schema(obj.table, p.Results.History);
            for k = 1:numel(stmts)
                if p.Results.Print
                    fprintf('%s;\n\n', stmts{k});
                else
                    pg_exec(obj, stmts{k});
                end
            end
        end

        % ============================================================ read / write
        function folders = folderKey(obj, keys)
        %FOLDERKEY  Canonical key(s) 'w2026/06/01/visit' (cellstr) for any accepted key form.
            folders = pg_folder(obj, keys);
        end

        function txt = read(obj, keys, varargin)
        %READ  Status text of one folder (char) or many (cellstr, input order kept).
        %   s = st.read('w2026/06/01/visit_001')
        %   C = st.read(folders, 'Default','', 'ErrorIfMissing',false, 'ChunkSize',2000)
            p = inputParser;
            p.addParameter('Default', '');
            p.addParameter('ErrorIfMissing', false);
            p.addParameter('ChunkSize', 2000);
            p.parse(varargin{:});
            o = p.Results;
            [~, isMulti] = sq_cellkeys(keys);
            folders = pg_folder(obj, keys);
            uf = unique(folders);
            found = {};
            vals = {};
            for i0 = 1:o.ChunkSize:numel(uf)
                chunk = uf(i0:min(i0 + o.ChunkSize - 1, numel(uf)));
                lits = cellfun(@pg_lit, chunk, 'UniformOutput', false);
                R = pg_query(obj, ['SELECT folder, status FROM ' obj.table ...
                                   ' WHERE folder IN (' strjoin(lits, ',') ')'], {'folder', 'status'});
                found = [found; R.folder]; %#ok<AGROW>
                vals  = [vals;  R.status]; %#ok<AGROW>
            end
            [tf, loc] = ismember(folders, found);
            if o.ErrorIfMissing && ~all(tf)
                miss = folders(~tf);
                error('statusdb:missing', 'No status row for %d folder(s), e.g. %s.', numel(miss), miss{1});
            end
            out = repmat({sq_char(o.Default)}, numel(folders), 1);
            out(tf) = vals(loc(tf));
            if isMulti
                txt = out;
            else
                txt = out{1};
            end
        end

        function n = write(obj, keys, texts, varargin)
        %WRITE  Set (upsert) the status of one or many folders.
        %   st.write(key, 'done')
        %   st.write(folders, 'queued')                  same text for all
        %   st.write(folders, texts)                     one text each
        %   st.write(..., 'Mode','append')               add a line (atomic in the server)
        %   st.write(..., 'Mode','insert_only')          never modify existing rows
        %   n = st.write(...)                            rows sent
        %   'replace' (default) leaves unchanged rows untouched, so 'updated' and the
        %   history move only on real changes. 'ChunkSize' rows per statement (2000).
        %   Duplicate folders: last text wins (append: texts joined). Trailing
        %   newlines are removed.
            p = inputParser;
            p.addParameter('Mode', 'replace');
            p.addParameter('ChunkSize', 2000);
            p.parse(varargin{:});
            o = p.Results;
            mode = validatestring(sq_char(o.Mode), {'replace', 'append', 'insert_only'});

            folders = pg_folder(obj, keys);
            [t, ~] = sq_cellkeys(texts);
            if numel(t) == 1
                t = repmat(t, numel(folders), 1);
            elseif numel(t) ~= numel(folders)
                error('statusdb:badInput', '%d folders but %d texts.', numel(folders), numel(t));
            end
            t = regexprep(t, '[\r\n]+$', '');

            [uf, ~, j] = unique(folders);
            if numel(uf) < numel(folders)
                ut = cell(numel(uf), 1);
                seen = false(numel(uf), 1);
                for k = 1:numel(folders)
                    if strcmp(mode, 'append') && seen(j(k))
                        ut{j(k)} = [ut{j(k)} sprintf('\n') t{k}];
                    else
                        ut{j(k)} = t{k};
                    end
                    seen(j(k)) = true;
                end
                folders = uf;
                t = ut;
            end
            for i0 = 1:o.ChunkSize:numel(folders)
                i1 = min(i0 + o.ChunkSize - 1, numel(folders));
                pg_exec(obj, pg_sql_upsert(obj.table, folders(i0:i1), t(i0:i1), mode));
            end
            n = numel(folders);
        end

        function n = remove(obj, keys, varargin)
        %REMOVE  Delete the status rows of the given folders (logged in history).
        %   n = st.remove(folders, 'ChunkSize', 2000)
            p = inputParser;
            p.addParameter('ChunkSize', 2000);
            p.parse(varargin{:});
            folders = unique(pg_folder(obj, keys));
            cs = p.Results.ChunkSize;
            for i0 = 1:cs:numel(folders)
                chunk = folders(i0:min(i0 + cs - 1, numel(folders)));
                lits = strjoin(cellfun(@pg_lit, chunk, 'UniformOutput', false), ',');
                pg_exec(obj, ['DELETE FROM ' obj.table ' WHERE folder IN (' lits ')']);
            end
            n = numel(folders);
        end

        % ================================================================ queries
        function S = list(obj, sub, varargin)
        %LIST  Rows under a folder prefix as a struct array (works in Octave too).
        %   S = st.list('w2026/06/01')
        %   S = st.list('w2026/06', 'Status', 'failed', 'Limit', 100)
        %   Prefix stops at a level boundary ('w2026/06/01' does not match
        %   'w2026/06/010'); '_' and '%' are literal. Fields: folder, status,
        %   updated ('yyyy-mm-ddTHH:MM:SS.FFF' UTC), updatedBy. Byte-order sorted.
            if nargin < 2
                sub = '';
            end
            p = inputParser;
            p.addParameter('Status', {});
            p.addParameter('Limit', 0);
            p.parse(varargin{:});
            o = p.Results;
            sql = ['SELECT folder, status, ' ...
                   'to_char(updated AT TIME ZONE ''UTC'', ''YYYY-MM-DD"T"HH24:MI:SS.MS'') AS updated, ' ...
                   'updated_by FROM ' obj.table pg_where(sub, o.Status) ' ORDER BY folder'];
            if o.Limit > 0
                sql = sprintf('%s LIMIT %d', sql, round(o.Limit));
            end
            R = pg_query(obj, sql, {'folder', 'status', 'updated', 'updated_by'});
            if isempty(R.folder)
                S = struct('folder', {}, 'status', {}, 'updated', {}, 'updatedBy', {});
            else
                S = struct('folder', R.folder, 'status', R.status, 'updated', R.updated, ...
                           'updatedBy', R.updated_by);
            end
            S = S(:);
        end

        function T = asTable(obj, sub, varargin)
        %ASTABLE  Rows as a MATLAB table keyed by folder (RowNames). Same options as LIST.
        %   T = st.asTable('w2026/06/01');
        %   T{"w2026/06/01/visit_001","Status"};  T(T.Status=="failed",:)
        %   Columns: Folder, Status, UpdatedBy (string), Updated (datetime, UTC).
            if nargin < 2
                sub = '';
            end
            if isempty(which('table'))
                error('statusdb:noTable', 'asTable needs MATLAB tables; use list() in Octave.');
            end
            S = obj.list(sub, varargin{:});
            Folder    = reshape(string({S.folder}), [], 1);
            Status    = reshape(string({S.status}), [], 1);
            UpdatedBy = reshape(string({S.updatedBy}), [], 1);
            if isempty(S)
                Updated = NaT(0, 1, 'TimeZone', 'UTC');
            else
                Updated = datetime({S.updated}', 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS', ...
                                   'TimeZone', 'UTC');
            end
            T = table(Folder, Status, Updated, UpdatedBy);
            if ~isempty(S)
                T.Properties.RowNames = cellstr(Folder);
            end
        end

        function n = writeTable(obj, T, varargin)
        %WRITETABLE  Write a table's Status column back (Folder variable or RowNames).
        %   T.Status(T.Status=="failed") = "queued";  st.writeTable(T);
        %   Rows with <missing> Status are skipped; in 'replace' mode only rows
        %   whose text differs are modified. Options as WRITE ('Mode').
            if ~isa(T, 'table') || ~any(strcmp(T.Properties.VariableNames, 'Status'))
                error('statusdb:badInput', 'T must be a table with a Status variable.');
            end
            if any(strcmp(T.Properties.VariableNames, 'Folder'))
                keys = string(T.Folder);
            elseif ~isempty(T.Properties.RowNames)
                keys = string(T.Properties.RowNames);
            else
                error('statusdb:badInput', 'T needs a Folder variable or row names.');
            end
            status = string(T.Status);
            keep = ~ismissing(status) & ~ismissing(keys);
            n = 0;
            if any(keep)
                n = obj.write(cellstr(keys(keep)), cellstr(status(keep)), varargin{:});
            end
        end

        function claimed = claim(obj, keys, fromStatus, toStatus, varargin)
        %CLAIM  Atomic compare-and-set; with KEYS empty, a parallel-safe work queue.
        %   got = st.claim(folders, 'queued', 'running')            only rows now 'queued'
        %   got = st.claim([], 'queued', 'running', 'Limit', 10)    take up to 10
        %   got = st.claim([], {'queued','failed'}, 'running', 'Limit', 1, 'Prefix', 'w2026/06')
        %   Returns the folders actually changed (cellstr, sorted). The queue form uses
        %   SELECT ... FOR UPDATE SKIP LOCKED: concurrent workers never get the same folder.
        %
        %   Worker loop:
        %     while true
        %         got = st.claim([], 'queued', 'running');
        %         if isempty(got), break, end
        %         try, process(got{1}); st.write(got{1}, 'done');
        %         catch e, st.write(got{1}, ['failed: ' e.message]); end
        %     end
            p = inputParser;
            p.addParameter('Limit', 1);
            p.addParameter('Prefix', '');
            p.parse(varargin{:});
            o = p.Results;
            [fromList, ~] = sq_cellkeys(fromStatus);
            fromSql = strjoin(cellfun(@pg_lit, fromList, 'UniformOutput', false), ',');
            toSql = pg_lit(regexprep(sq_char(toStatus), '[\r\n]+$', ''));
            tbl = obj.table;
            if isempty(keys)
                sql = sprintf(['WITH c AS (SELECT folder FROM %s%s ORDER BY folder LIMIT %d ' ...
                               'FOR UPDATE SKIP LOCKED), ' ...
                               'u AS (UPDATE %s t SET status = %s FROM c WHERE t.folder = c.folder ' ...
                               'RETURNING t.folder) SELECT folder FROM u ORDER BY folder'], ...
                              tbl, pg_where(o.Prefix, fromList), max(1, round(o.Limit)), tbl, toSql);
            else
                folders = unique(pg_folder(obj, keys));
                lits = strjoin(cellfun(@pg_lit, folders, 'UniformOutput', false), ',');
                sql = sprintf(['WITH u AS (UPDATE %s t SET status = %s WHERE t.folder IN (%s) ' ...
                               'AND t.status IN (%s) RETURNING t.folder) ' ...
                               'SELECT folder FROM u ORDER BY folder'], tbl, toSql, lits, fromSql);
            end
            R = pg_query(obj, sql, {'folder'});
            claimed = R.folder;
        end

        function H = history(obj, key, varargin)
        %HISTORY  Change log of one folder, oldest first ('Limit', N = newest N).
        %   Fields: folder, old_status, new_status, changed (UTC), changed_by.
        %   Table in MATLAB, struct array in Octave.
            p = inputParser;
            p.addParameter('Limit', 0);
            p.parse(varargin{:});
            folder = pg_folder(obj, key);
            cols = {'folder', 'old_status', 'new_status', 'changed', 'changed_by'};
            inner = ['SELECT id, folder, coalesce(old_status, '''') AS old_status, ' ...
                     'coalesce(new_status, '''') AS new_status, ' ...
                     'to_char(changed AT TIME ZONE ''UTC'', ''YYYY-MM-DD"T"HH24:MI:SS.MS'') AS changed, ' ...
                     'changed_by FROM ' obj.logTable ' WHERE folder = ' pg_lit(folder{1}) ' ORDER BY id DESC'];
            if p.Results.Limit > 0
                inner = sprintf('%s LIMIT %d', inner, round(p.Results.Limit));
            end
            R = pg_query(obj, ['SELECT ' strjoin(cols, ', ') ' FROM (' inner ') h ORDER BY id'], cols);
            if ~isempty(which('table'))
                H = table(string(R.folder), string(R.old_status), string(R.new_status), ...
                          string(R.changed), string(R.changed_by), 'VariableNames', cols);
            elseif isempty(R.folder)
                H = struct('folder', {}, 'old_status', {}, 'new_status', {}, 'changed', {}, 'changed_by', {});
            else
                H = struct('folder', R.folder, 'old_status', R.old_status, 'new_status', R.new_status, ...
                           'changed', R.changed, 'changed_by', R.changed_by);
            end
        end

        function C = summary(obj, sub)
        %SUMMARY  Folders per status under a prefix; prints when no output is requested.
        %   st.summary('w2026/06');  C = st.summary('w2026/06/01');  (table or struct)
            if nargin < 2
                sub = '';
            end
            R = pg_query(obj, ['SELECT status, count(*)::text AS n FROM ' obj.table pg_where(sub, {}) ...
                               ' GROUP BY status ORDER BY count(*) DESC, status'], {'status', 'n'});
            cnt = str2double(R.n);
            if nargout == 0
                if isempty(sub), sub = '(all)'; end
                fprintf('%s: %d folders\n', sub, sum(cnt));
                for k = 1:numel(cnt)
                    fprintf('  %10d  %s\n', cnt(k), strtok(R.status{k}, sprintf('\n')));
                end
                return
            end
            if ~isempty(which('table'))
                C = table(string(R.status), cnt(:), 'VariableNames', {'status', 'count'});
            elseif isempty(cnt)
                C = struct('status', {}, 'count', {});
            else
                C = struct('status', R.status, 'count', num2cell(cnt(:)));
            end
        end

        % ============================================================ file import
        function rep = importStatusFiles(obj, root, sub, varargin)
        %IMPORTSTATUSFILES  Scan a folder tree for status files, insert them, then
        %   optionally rename or delete the originals.
        %
        %   rep = st.importStatusFiles('/data', 'w2026/06')                 % a month
        %   rep = st.importStatusFiles('/data', 'w2026', 'DryRun', true)    % just look
        %   rep = st.importStatusFiles('/data', 'w2026', 'Originals', 'rename')
        %
        %   For each leaf folder <night>/<visit> it looks for files matching Pattern:
        %     <night>/<visit>/<file>          inside the folder
        %     <night>/<visit>.meta/<file>     next to a SquashFS-packed folder
        %     <night>/<visit><ext>            sibling file, if Pattern is '*<ext>'
        %                                     (e.g. visit_001.status for '*.status')
        %   Works night by night (bounded memory, one progress line per night).
        %   Symlinks are not followed, so packed folders are never mounted.
        %
        %   Options:
        %     Pattern       '*.status'     find -name glob, e.g. 'status.txt'
        %     Mode          'insert_only'  never overwrite rows already in the DB;
        %                   'replace'      the files win
        %     Originals     'keep' | 'rename' | 'delete'. Applied ONLY to files whose
        %                   text is confirmed in the database after the write.
        %     RenameSuffix  '.imported'    renamed files no longer match Pattern,
        %                                  so re-runs skip them
        %     OnMultiple    'newest' (default) | 'join' | 'skip' when one folder has
        %                   several matching files ('newest': others are left alone)
        %     DryRun        false          scan and report only
        %     MaxNights     0              stop after N nights (0 = all), for pilots
        %     Verbose       true           one line per night
        %
        %   REP: nights, files, folders, written, verified, differ, multiple, renamed,
        %        deleted, errors (cellstr), differExamples (cellstr)
        %   'differ' = folders whose DB text is not the file text (with insert_only:
        %   the DB already had another value); their files are never renamed/deleted.
            if nargin < 3
                sub = '';
            end
            p = inputParser;
            p.addParameter('Pattern', '*.status');
            p.addParameter('Mode', 'insert_only');
            p.addParameter('Originals', 'keep');
            p.addParameter('RenameSuffix', '.imported');
            p.addParameter('OnMultiple', 'newest');
            p.addParameter('DryRun', false);
            p.addParameter('MaxNights', 0);
            p.addParameter('Verbose', true);
            p.parse(varargin{:});
            o = p.Results;
            mode      = validatestring(sq_char(o.Mode), {'insert_only', 'replace'});
            originals = validatestring(sq_char(o.Originals), {'keep', 'rename', 'delete'});
            onMulti   = validatestring(sq_char(o.OnMultiple), {'newest', 'join', 'skip'});
            pattern   = sq_char(o.Pattern);
            suffix    = sq_char(o.RenameSuffix);
            if isempty(pattern) || any(pattern == '/')
                error('statusdb:badInput', 'Pattern must be a file-name glob, got "%s".', pattern);
            end
            if strcmp(originals, 'rename') && isempty(suffix)
                error('statusdb:badInput', 'RenameSuffix must not be empty.');
            end
            sibExt = '';
            if numel(pattern) > 1 && pattern(1) == '*' && ~any(ismember(pattern(2:end), '*?['))
                sibExt = pattern(2:end);
            end

            root = regexprep(sq_char(root), '/+$', '');
            nights = find_nights(root, sub, obj.depth);
            if o.MaxNights > 0 && numel(nights) > o.MaxNights
                nights = nights(1:o.MaxNights);
            end
            rep = struct('nights', numel(nights), 'files', 0, 'folders', 0, 'written', 0, ...
                         'verified', 0, 'differ', 0, 'multiple', 0, 'renamed', 0, 'deleted', 0, ...
                         'errors', {{}}, 'differExamples', {{}});
            t0 = tic;
            for in = 1:numel(nights)
                night = nights{in};
                nd = [root '/' night];
                % ---- leaves present in this night (no symlink following) --------
                [~, out] = system(sprintf('find %s -mindepth 1 -maxdepth 1 -printf ''%%y\\t%%f\\n'' 2>/dev/null', ...
                                          sq_shq(nd)));
                leafSet = {};
                for ln = regexp(out, '\n', 'split')
                    e = ln{1};
                    if numel(e) < 3 || e(3) == '.', continue, end
                    nm = e(3:end);
                    if any(e(1) == 'dl') && ~endsWith(nm, '.meta')
                        leafSet{end+1} = nm; %#ok<AGROW>
                    elseif e(1) == 'f' && endsWith(nm, '.sqfs')
                        leafSet{end+1} = nm(1:end-5); %#ok<AGROW>
                    end
                end
                % ---- matching status files ----------------------------------------
                [~, out] = system(sprintf(['find %s -mindepth 1 -maxdepth 2 -type f -name %s ' ...
                                           '-printf ''%%T@\\t%%P\\n'' 2>/dev/null'], sq_shq(nd), sq_shq(pattern)));
                keys = {}; files = {}; mt = [];
                for ln = regexp(out, '\n', 'split')
                    e = ln{1};
                    tab = find(e == sprintf('\t'), 1);
                    if isempty(tab), continue, end
                    rel = e(tab+1:end);
                    parts = regexp(rel, '/', 'split');
                    if any(cellfun(@(s) isempty(s) || s(1) == '.', parts)), continue, end
                    if numel(parts) == 1
                        if isempty(sibExt) || numel(parts{1}) <= numel(sibExt) || ~endsWith(parts{1}, sibExt)
                            continue
                        end
                        leaf = parts{1}(1:end-numel(sibExt));
                        if ~any(strcmp(leafSet, leaf))
                            rep.errors{end+1, 1} = sprintf('%s/%s: no folder "%s" for this sibling file, skipped', ...
                                                           night, rel, leaf);
                            continue
                        end
                    else
                        leaf = parts{1};
                        if endsWith(leaf, '.meta'), leaf = leaf(1:end-5); end
                    end
                    try
                        k = pg_folder(obj, [night '/' leaf]);
                    catch err
                        rep.errors{end+1, 1} = sprintf('%s/%s: %s', night, rel, err.message);
                        continue
                    end
                    keys{end+1, 1} = k{1}; %#ok<AGROW>
                    files{end+1, 1} = [nd '/' rel]; %#ok<AGROW>
                    mt(end+1, 1) = str2double(e(1:tab-1)); %#ok<AGROW>
                end
                rep.files = rep.files + numel(files);
                if isempty(files)
                    if o.Verbose
                        fprintf('%-24s %6d files\n', night, 0);
                    end
                    continue
                end
                % ---- one text per folder --------------------------------------------
                [uk, ~, g] = unique(keys);
                selKeys = {}; selText = {}; selFiles = {};
                for i = 1:numel(uk)
                    idx = find(g == i);
                    if numel(idx) > 1
                        rep.multiple = rep.multiple + 1;
                        switch onMulti
                            case 'skip'
                                rep.errors{end+1, 1} = sprintf('%s: %d status files, skipped (OnMultiple=skip)', ...
                                                               uk{i}, numel(idx));
                                continue
                            case 'newest'
                                [~, m] = max(mt(idx));
                                idx = idx(m);
                            case 'join'
                                [~, order] = sort(mt(idx));
                                idx = idx(order);
                        end
                    end
                    try
                        txt = cellfun(@(f) regexprep(sq_readtext(f), '[\r\n]+$', ''), files(idx), ...
                                      'UniformOutput', false);
                    catch err
                        rep.errors{end+1, 1} = sprintf('%s: %s', uk{i}, err.message);
                        continue
                    end
                    selKeys{end+1, 1} = uk{i}; %#ok<AGROW>
                    selText{end+1, 1} = strjoin(txt(:)', sprintf('\n')); %#ok<AGROW>
                    selFiles{end+1, 1} = files(idx); %#ok<AGROW>
                end
                rep.folders = rep.folders + numel(selKeys);
                nw = 0; nv = 0; nd_ = 0; nr = 0; ndel = 0;
                if ~o.DryRun && ~isempty(selKeys)
                    nw = obj.write(selKeys, selText, 'Mode', mode);
                    cur = obj.read(selKeys);
                    ok = strcmp(cur, selText);
                    nv = sum(ok);
                    nd_ = sum(~ok);
                    bad = selKeys(~ok);
                    rep.differExamples = [rep.differExamples; bad(1:min(end, 10 - numel(rep.differExamples)))];
                    % ---- originals: only for verified folders ------------------------
                    if ~strcmp(originals, 'keep')
                        for i = find(ok(:))'
                            for f = selFiles{i}'
                                try
                                    if strcmp(originals, 'delete')
                                        delete(f{1});
                                        ndel = ndel + 1;
                                    else
                                        dst = [f{1} suffix];
                                        c = 1;
                                        while exist(dst, 'file')
                                            dst = sprintf('%s%s.%d', f{1}, suffix, c);
                                            c = c + 1;
                                        end
                                        [s, msg] = movefile(f{1}, dst);
                                        if ~s, error('statusdb:io', '%s', msg); end
                                        nr = nr + 1;
                                    end
                                catch err
                                    rep.errors{end+1, 1} = sprintf('%s: cannot %s: %s', f{1}, originals, err.message);
                                end
                            end
                        end
                    end
                end
                rep.written = rep.written + nw;
                rep.verified = rep.verified + nv;
                rep.differ = rep.differ + nd_;
                rep.renamed = rep.renamed + nr;
                rep.deleted = rep.deleted + ndel;
                if o.Verbose
                    fprintf('%-24s %6d files %6d folders %6d written %6d verified %6d differ %6d renamed %6d deleted [%.0f s]\n', ...
                            night, numel(files), numel(selKeys), nw, nv, nd_, nr, ndel, toc(t0));
                end
            end
            if o.Verbose
                tag = '';
                if o.DryRun, tag = ' (dry run, nothing changed)'; end
                fprintf(['import%s: %d nights, %d files, %d folders, %d written, %d verified, ' ...
                         '%d differ, %d renamed, %d deleted, %d with several files, %d errors\n'], ...
                        tag, rep.nights, rep.files, rep.folders, rep.written, rep.verified, rep.differ, ...
                        rep.renamed, rep.deleted, rep.multiple, numel(rep.errors));
                for k = 1:min(5, numel(rep.errors))
                    fprintf('  error: %s\n', rep.errors{k});
                end
            end
        end

        % ================================================================ debug
        function R = check(obj, varargin)
        %CHECK  Diagnose the database setup and run a round-trip self test.
        %   st.check();  R = st.check('SelfTest', true, 'Pings', 10);
        %   Reports client/toolbox, server, encodings, standard_conforming_strings,
        %   table/log table, folder collation ("C" needed for fast prefix queries),
        %   triggers, privileges, row estimate, size, latency. The self test writes,
        %   reads, appends, claims and deletes a throw-away 'zz-statusdb-selftest/...'
        %   folder with quotes, backslashes, %, _, newlines and non-ASCII text.
        %   R.warnings lists every problem found.
            p = inputParser;
            p.addParameter('SelfTest', true);
            p.addParameter('Pings', 10);
            p.parse(varargin{:});
            o = p.Results;
            W = {};
            tbl = obj.table;
            fprintf('\n== StatusDB.check (table %s)\n', tbl);

            if exist('OCTAVE_VERSION', 'builtin') ~= 0
                show('client', ['Octave ' OCTAVE_VERSION]);
            else
                show('client', ['MATLAB ' version]);
                v = ver('database');
                if isempty(v)
                    W{end+1} = 'Database Toolbox not installed';
                else
                    show('toolbox', sprintf('%s %s, license %s', v(1).Name, v(1).Version, ...
                         yn(license('test', 'Database_Toolbox'))));
                end
            end

            S = pg_query(obj, ['SELECT current_database() AS db, current_user AS usr, ' ...
                'current_setting(''server_version'') AS ver, current_setting(''server_encoding'') AS senc, ' ...
                'current_setting(''client_encoding'') AS cenc, current_setting(''TimeZone'') AS tz, ' ...
                'current_setting(''standard_conforming_strings'') AS scs'], ...
                {'db', 'usr', 'ver', 'senc', 'cenc', 'tz', 'scs'});
            R.server = structfun(@(c) c{1}, S, 'UniformOutput', false);
            show('server', ['PostgreSQL ' R.server.ver]);
            show('database', sprintf('%s as %s', R.server.db, R.server.usr));
            show('encoding', sprintf('server %s, client %s, TimeZone %s', R.server.senc, R.server.cenc, R.server.tz));
            show('std strings', R.server.scs);
            if ~strcmp(R.server.scs, 'on')
                W{end+1} = 'standard_conforming_strings is off: quoting is unsafe';
            end
            if ~strcmpi(R.server.senc, 'UTF8') || ~strcmpi(R.server.cenc, 'UTF8')
                W{end+1} = 'encoding is not UTF8 on both sides: non-ASCII status text may be altered';
            end

            E = pg_query(obj, sprintf(['SELECT coalesce(to_regclass(%s)::text, '''') AS t, ' ...
                                       'coalesce(to_regclass(%s)::text, '''') AS l'], ...
                                       pg_lit(tbl), pg_lit(obj.logTable)), {'t', 'l'});
            R.tableExists = ~isempty(E.t{1});
            R.logExists = ~isempty(E.l{1});
            R.collation = '';
            show('table', yn(R.tableExists));
            show('history', yn(R.logExists));
            if ~R.tableExists
                W{end+1} = sprintf('table %s missing: run st.createSchema()', tbl);
            else
                reg = [pg_lit(tbl) '::regclass'];
                C = pg_query(obj, ['SELECT coalesce(c.collname, ''default'') AS coll FROM pg_attribute a ' ...
                                   'LEFT JOIN pg_collation c ON c.oid = a.attcollation ' ...
                                   'WHERE a.attrelid = ' reg ' AND a.attname = ''folder'''], {'coll'});
                R.collation = C.coll{1};
                show('folder coll', R.collation);
                if ~strcmp(R.collation, 'C')
                    W{end+1} = 'folder column is not COLLATE "C": prefix queries cannot use the index';
                end
                T = pg_query(obj, ['SELECT tgname AS name FROM pg_trigger WHERE tgrelid = ' reg ...
                                   ' AND NOT tgisinternal ORDER BY tgname'], {'name'});
                R.triggers = T.name;
                show('triggers', strjoin(T.name', ', '));
                base = regexprep(tbl, '^.*\.', '');
                if ~any(strcmp(T.name, [base '_touch']))
                    W{end+1} = 'touch trigger missing: "updated" will not change (re-run createSchema)';
                end
                if R.logExists && ~any(strcmp(T.name, [base '_log']))
                    W{end+1} = 'log table exists but log trigger is missing: history is not recorded';
                end
                P = pg_query(obj, ['SELECT has_table_privilege(' reg ', ''SELECT'')::text AS s, ' ...
                                   'has_table_privilege(' reg ', ''INSERT'')::text AS i, ' ...
                                   'has_table_privilege(' reg ', ''UPDATE'')::text AS u, ' ...
                                   'has_table_privilege(' reg ', ''DELETE'')::text AS d'], {'s', 'i', 'u', 'd'});
                priv = {'SELECT', 'INSERT', 'UPDATE', 'DELETE'};
                okp = strcmp([P.s; P.i; P.u; P.d], 'true');
                show('privileges', strjoin(priv(okp), ' '));
                if ~all(okp)
                    W{end+1} = ['missing privileges: ' strjoin(priv(~okp), ' ')];
                end
                N = pg_query(obj, ['SELECT reltuples::bigint::text AS n, ' ...
                                   'pg_size_pretty(pg_total_relation_size(' reg ')) AS sz ' ...
                                   'FROM pg_class WHERE oid = ' reg], {'n', 'sz'});
                n = str2double(N.n{1});
                if n < 0
                    show('rows', sprintf('not analyzed yet, size %s', N.sz{1}));
                else
                    show('rows', sprintf('~%d (estimate), size %s', n, N.sz{1}));
                end
            end

            t = zeros(1, o.Pings);
            for k = 1:o.Pings
                t0 = tic;
                pg_query(obj, 'SELECT 1 AS x', {'x'});
                t(k) = toc(t0);
            end
            R.latencyMs = 1000 * median(t);
            show('latency', sprintf('%.2f ms per query (median of %d)', R.latencyMs, o.Pings));

            R.selfTest = 'skipped';
            if o.SelfTest && R.tableExists
                [~, tok] = fileparts(tempname);
                parts = [{'zz-statusdb-selftest'}, repmat({'0'}, 1, max(obj.depth - 2, 0)), ...
                         {regexprep(tok, '[^A-Za-z0-9]', '')}];
                key = strjoin(parts(1:max(obj.depth, 1)), '/');
                uni = native2unicode(uint8([195 188 110 195 175 99 195 182 100 195 169]), 'UTF-8');
                tricky = sprintf('it''s %% _ \\ "q" \t tab\nline2 %s', uni);
                try
                    obj.write(key, tricky);
                    ok1 = strcmp(obj.read(key), tricky);
                    obj.write(key, 'more', 'Mode', 'append');
                    ok2 = strcmp(obj.read(key), [tricky sprintf('\n') 'more']);
                    obj.write(key, 'queued');
                    c1 = obj.claim(key, 'queued', 'running');
                    c2 = obj.claim(key, 'queued', 'running');
                    ok3 = numel(c1) == 1 && isempty(c2) && strcmp(obj.read(key), 'running');
                    ok4 = numel(obj.list(key)) == 1;
                    obj.remove(key);
                    ok5 = isempty(obj.read(key));
                    if R.logExists
                        try
                            pg_exec(obj, ['DELETE FROM ' obj.logTable ' WHERE folder = ' pg_lit(key)]);
                        catch
                        end
                    end
                    checks = {'quoting/unicode', ok1; 'append', ok2; 'claim', ok3; 'list', ok4; 'remove', ok5};
                    failed = checks(~[checks{:, 2}], 1);
                    if isempty(failed)
                        R.selfTest = 'passed';
                    else
                        R.selfTest = ['FAILED: ' strjoin(failed', ', ')];
                        W{end+1} = ['self test failed: ' strjoin(failed', ', ')];
                    end
                catch err
                    R.selfTest = ['ERROR: ' err.message];
                    W{end+1} = ['self test error: ' err.message];
                    try, obj.remove(key); catch, end %#ok<NOCOM>
                end
                show('self test', R.selfTest);
            end

            R.warnings = W;
            if isempty(W)
                fprintf('  %-12s %s\n', 'result', 'OK');
            else
                for k = 1:numel(W)
                    fprintf('  %-12s %s\n', 'WARNING', W{k});
                end
            end
            fprintf('\n');
            if nargout == 0
                clear R
            end
        end
    end

    methods (Static, Access = private)
        function conn = openConnection(o)
            if isempty(which('postgresql'))
                error('statusdb:noToolbox', ['postgresql() not found: Database Toolbox is required ' ...
                      '(license(''test'',''Database_Toolbox'')).']);
            end
            pw = o.Password;
            if isempty(pw) && ~isempty(which('isSecret')) && isSecret(o.PasswordSecret)
                pw = getSecret(o.PasswordSecret);
            end
            if isempty(pw)
                pw = getenv('PGPASSWORD');
            end
            if isempty(pw)
                error('statusdb:noPassword', ['No password: run setSecret("%s") once (R2024a+), ' ...
                      'set PGPASSWORD, or pass ''Password''.'], o.PasswordSecret);
            end
            user = sq_char(o.User);
            if ~isempty(o.DataSource)
                conn = postgresql(sq_char(o.DataSource), user, pw);
            else
                if isempty(o.Database)
                    error('statusdb:badInput', 'Give ''Database'' (or set PGDATABASE).');
                end
                conn = postgresql(user, pw, 'Server', sq_char(o.Server), ...
                                  'PortNumber', o.Port, 'DatabaseName', sq_char(o.Database));
            end
            if isobject(conn) && isprop(conn, 'Message') && ~isempty(conn.Message)
                error('statusdb:connect', 'Connection failed: %s', conn.Message);
            end
        end
    end
end

% ======================================================================= local functions

function nights = find_nights(root, sub, depth)
% Night folders (depth-1 levels, e.g. 'w2026/06/01') under ROOT/SUB, sorted, real dirs only.
sub = regexprep(regexprep(sq_char(sub), '^/+', ''), '/+$', '');
subParts = {};
if ~isempty(sub)
    subParts = regexp(sub, '/', 'split');
end
nightLevel = depth - 1;
if nightLevel < 1
    error('statusdb:badInput', 'importStatusFiles needs Depth >= 2.');
end
if numel(subParts) > nightLevel
    error('statusdb:badInput', ...
        'SUB "%s" is below night level; give a night (e.g. w2026/06/01) or a higher level.', sub);
end
start = root;
if ~isempty(sub)
    start = [root '/' sub];
end
if ~isfolder(start)
    warning('statusdb:noFolder', 'Nothing to scan: %s does not exist.', start);
    nights = {};
    return
end
if numel(subParts) == nightLevel
    nights = {sub};
    return
end
m = nightLevel - numel(subParts);
[~, out] = system(sprintf('find %s -mindepth %d -maxdepth %d -type d -printf ''%%P\\n'' 2>/dev/null | LC_ALL=C sort', ...
                          sq_shq(start), m, m));
rel = regexp(out, '\n', 'split');
rel = rel(~cellfun(@isempty, rel));
rel = rel(~cellfun(@(r) any(cellfun(@(s) isempty(s) || s(1) == '.', regexp(r, '/', 'split'))), rel));
if isempty(sub)
    nights = rel(:);
else
    nights = strcat([sub '/'], rel(:));
end
end

function v = envor(name, default)
v = getenv(name);
if isempty(v)
    v = default;
end
end

function show(label, val)
fprintf('  %-12s %s\n', label, val);
end

function s = yn(tf)
if tf, s = 'yes'; else, s = 'no'; end
end
