function q = pg_lit(s)
%PG_LIT  Quote a char/string value as a PostgreSQL string literal.
%   Doubles single quotes. Requires standard_conforming_strings = on (checked
%   by StatusDB), so backslashes are ordinary characters.
s = sq_char(s);
if any(s == char(0))
    error('statusdb:badInput', 'Text contains a NUL character, which PostgreSQL text cannot store.');
end
q = ['''' strrep(s, '''', '''''') ''''];
end
