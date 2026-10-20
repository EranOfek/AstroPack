function q = pg_ident(name)
%PG_IDENT  Validate a (optionally schema-qualified) table name and return it.
%   Only plain lower-case identifiers are accepted, e.g. 'visit_status' or
%   'pipeline.visit_status', so table names can be pasted into SQL safely.
name = sq_char(name);
if isempty(regexp(name, '^[a-z_][a-z0-9_]*(\.[a-z_][a-z0-9_]*)?$', 'once'))
    error('statusdb:badInput', ...
        'Table name "%s" must be lower-case letters, digits and _ (optionally schema.table).', name);
end
q = name;
end
