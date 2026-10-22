%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function o = options(args, defaults)
%MATLAB_TABLE_DUMP.INTERNAL.OPTIONS  Parse Name,Value pairs against a struct of defaults (case-insensitive).
%   Works the same in MATLAB and Octave (no inputParser, no arguments blocks).
o = defaults;
names = fieldnames(defaults);
if mod(numel(args), 2) ~= 0
    error('matlab_table_dump:badOption', 'Options must be Name,Value pairs.');
end
for i = 1:2:numel(args)
    key = args{i};
    if isa(key, 'string'), key = char(key); end
    if ~ischar(key)
        error('matlab_table_dump:badOption', 'Option names must be text.');
    end
    hit = find(strcmpi(names, key), 1);
    if isempty(hit)
        error('matlab_table_dump:badOption', 'Unknown option ''%s''. Known: %s.', key, strjoin(names', ', '));
    end
    o.(names{hit}) = args{i + 1};
end
end
