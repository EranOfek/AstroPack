%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function id = check_id(id, what)
%MATLAB_TABLE_DUMP.INTERNAL.CHECK_ID  Validate a batch, producer or consumer id (docs/QUEUE.md section 2).
if isa(id, 'string') && isscalar(id), id = char(id); end
if ~ischar(id) || size(id, 1) > 1
    error('matlab_table_dump:badId', 'The %s id must be text.', what);
end
if strcmp(what, 'batch')
    ok = ~isempty(regexp(id, '^[A-Za-z0-9._-]{1,200}$', 'once'));
    rule = '^[A-Za-z0-9._-]{1,200}$';
else
    ok = ~isempty(regexp(id, '^[A-Za-z0-9_-]{1,64}$', 'once'));
    rule = '^[A-Za-z0-9_-]{1,64}$';
end
if ~ok
    error('matlab_table_dump:badId', 'Invalid %s id ''%s'': must match %s.', what, id, rule);
end
end
