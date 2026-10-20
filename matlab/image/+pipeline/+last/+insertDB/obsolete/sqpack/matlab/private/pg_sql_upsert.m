function sql = pg_sql_upsert(tbl, folders, texts, mode)
%PG_SQL_UPSERT  One INSERT ... ON CONFLICT statement for many (folder, status) rows.
%   mode: 'replace'      set status; rows whose text is unchanged are not touched
%         'append'       append text as a new line to the existing status
%         'insert_only'  insert missing folders, never modify existing rows
%   FOLDERS must be unique (PostgreSQL rejects updating one row twice in a statement).
tbl = pg_ident(tbl);
rows = cell(numel(folders), 1);
for k = 1:numel(folders)
    rows{k} = ['(' pg_lit(folders{k}) ',' pg_lit(texts{k}) ')'];
end
head = ['INSERT INTO ' tbl ' AS t (folder, status) VALUES ' strjoin(rows, ',') ' ON CONFLICT (folder) '];
switch mode
    case 'replace'
        sql = [head 'DO UPDATE SET status = EXCLUDED.status ' ...
                    'WHERE t.status IS DISTINCT FROM EXCLUDED.status'];
    case 'append'
        sql = [head 'DO UPDATE SET status = t.status || chr(10) || EXCLUDED.status'];
    case 'insert_only'
        sql = [head 'DO NOTHING'];
    otherwise
        error('statusdb:badInput', 'Unknown upsert mode "%s".', mode);
end
end
