function w = pg_where(sub, status)
%PG_WHERE  ' WHERE ...' clause for a folder prefix and optional status list ('' if none).
c = {};
sub = regexprep(regexprep(sq_char(sub), '^/+', ''), '/+$', '');
if ~isempty(sub)
    c{end+1} = ['(folder = ' pg_lit(sub) ' OR folder LIKE ' pg_like_prefix([sub '/']) ')'];
end
if ~isempty(status)
    [st, ~] = sq_cellkeys(status);
    lits = cellfun(@pg_lit, st, 'UniformOutput', false);
    c{end+1} = ['status IN (' strjoin(lits, ',') ')'];
end
if isempty(c)
    w = '';
else
    w = [' WHERE ' strjoin(c, ' AND ')];
end
end
