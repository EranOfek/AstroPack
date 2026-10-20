function R = pg_query(db, sql, cols)
%PG_QUERY  Run a SELECT (Database Toolbox FETCH) and return text columns.
%   R = pg_query(db, sql, {'folder','status'}) returns a struct whose fields
%   are cellstr column vectors (one per requested column), whatever FETCH
%   returned (table with string/cell/char/numeric/datetime variables, or an
%   empty result). All SQL in StatusDB casts its output columns to text.
t0 = tic;
try
    T = fetch(db.conn, sql);
catch e
    s = regexprep(sql, '\s+', ' ');
    if numel(s) > 400, s = [s(1:400) ' ...']; end
    error('statusdb:sql', '%s\n  SQL: %s', e.message, s);
end

if isa(T, 'table')
    names = T.Properties.VariableNames;
    get = @(n) T.(n);
elseif isstruct(T)
    names = fieldnames(T);
    get = @(n) T.(n);
else
    names = {};
    get = [];
end

R = struct();
nrows = NaN;
for k = 1:numel(cols)
    c = cols{k};
    hit = find(strcmpi(names, c), 1);
    if isempty(hit)
        v = cell(0, 1);
    else
        v = to_cellstr(get(names{hit}));
    end
    R.(c) = v;
    nrows = numel(v);
end
pg_trace(db, 'fetch', sql, toc(t0), nrows);
end

function c = to_cellstr(v)
if isempty(v)
    c = cell(0, 1);
    return
end
if isstring(v)
    v(ismissing(v)) = "";
    c = cellstr(v(:));
elseif iscell(v)
    c = v(:);
    for k = 1:numel(c)
        x = c{k};
        if isstring(x)
            if ismissing(x), x = ''; else, x = char(x); end
        elseif isnumeric(x) || islogical(x)
            if isempty(x), x = ''; else, x = num2str(x); end
        end
        c{k} = x;
    end
elseif ischar(v)
    c = cellstr(v);
elseif isnumeric(v) || islogical(v)
    c = arrayfun(@num2str, v(:), 'UniformOutput', false);
else                                    % datetime, categorical, ...
    c = cellstr(string(v(:)));
end
end
