function folders = pg_folder(db, keys)
%PG_FOLDER  Normalize folder keys to the canonical 'w2026/06/01/visit_001' form.
%   Accepts (char, string, string array or cellstr):
%     'w2026/06/01/visit_001'          relative key (canonical)
%     '/data/w2026/06/01/visit_001'    absolute path under db.root
%     'w2026_06_01_visit_001'          autofs key (only the last level may contain '_')
%   Returns a cellstr column. db.depth = 0 disables the level-count check.
[keys, ~] = sq_cellkeys(keys);
folders = cell(numel(keys), 1);
depth = db.depth;
for k = 1:numel(keys)
    key = regexprep(strtrim(keys{k}), '/+$', '');
    if isempty(key)
        error('statusdb:badKey', 'Empty folder key.');
    end
    if key(1) == '/'
        if isempty(db.root)
            error('statusdb:badKey', ...
                'Absolute path "%s" given but no Root was given to StatusDB.', key);
        end
        pre = [db.root '/'];
        if ~strncmp(key, pre, numel(pre))
            error('statusdb:badKey', '"%s" is not under Root "%s".', key, db.root);
        end
        key = key(numel(pre)+1:end);
    elseif ~any(key == '/') && depth > 1
        pat = ['^' repmat('([^_/]+)_', 1, depth-1) '(.+)$'];
        tok = regexp(key, pat, 'tokens', 'once');
        if isempty(tok)
            error('statusdb:badKey', '"%s" is neither a path nor an autofs key with %d levels.', ...
                  key, depth);
        end
        key = strjoin(tok, '/');
    end
    parts = regexp(key, '/', 'split');      % no collapsing: 'a//b' is invalid
    if depth > 0 && numel(parts) ~= depth
        error('statusdb:badKey', '"%s" has %d levels, expected %d.', key, numel(parts), depth);
    end
    if any(cellfun(@(s) isempty(s) || s(1) == '.', parts))
        error('statusdb:badKey', 'Invalid folder key "%s".', key);
    end
    if any(key == char(0)) || any(key == sprintf('\n'))
        error('statusdb:badKey', 'Folder key contains control characters.');
    end
    folders{k} = key;
end
end
