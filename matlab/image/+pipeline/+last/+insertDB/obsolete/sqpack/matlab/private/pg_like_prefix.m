function q = pg_like_prefix(prefix)
%PG_LIKE_PREFIX  LIKE pattern literal matching folders that start with PREFIX.
%   Escapes LIKE wildcards so '_' in visit names is matched literally
%   (backslash is LIKE's default escape character).
p = sq_char(prefix);
p = strrep(p, '\', '\\');
p = strrep(p, '%', '\%');
p = strrep(p, '_', '\_');
q = pg_lit([p '%']);
end
