function pg_trace(db, what, sql, sec, nrows)
%PG_TRACE  Echo SQL and timing when debugging is on (db.debug or PGSTATUS_DEBUG=1).
on = ~isempty(getenv('PGSTATUS_DEBUG'));
if ~on
    if isstruct(db)
        on = isfield(db, 'debug') && db.debug;
    else
        on = db.debug;                       % StatusDB object
    end
end
if ~on
    return
end
s = regexprep(sql, '\s+', ' ');
if numel(s) > 300
    s = sprintf('%s ... [%d chars]', s(1:300), numel(sql));
end
if isnan(nrows)
    fprintf('[StatusDB %s %.3fs] %s\n', what, sec, s);
else
    fprintf('[StatusDB %s %.3fs, %d rows] %s\n', what, sec, nrows, s);
end
end
