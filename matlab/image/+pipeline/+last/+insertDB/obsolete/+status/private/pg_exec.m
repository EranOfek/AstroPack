function pg_exec(db, sql)
%PG_EXEC  Run a statement that returns no rows (Database Toolbox EXECUTE).
%   Set PGSTATUS_DEBUG=1 in the environment (setenv) or db.debug=true to echo SQL.
t0 = tic;
try
    execute(db.conn, sql);
catch e
    error('statusdb:sql', '%s\n  SQL: %s', e.message, pg_short(sql));
end
pg_trace(db, 'exec', sql, toc(t0), NaN);
end

function s = pg_short(sql)
s = regexprep(sql, '\s+', ' ');
if numel(s) > 400
    s = [s(1:400) ' ...'];
end
end
