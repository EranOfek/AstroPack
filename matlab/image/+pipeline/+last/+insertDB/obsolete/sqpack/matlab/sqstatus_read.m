function [txt, info] = sqstatus_read(root, key, varargin)
%SQSTATUS_READ  Read the status text of a leaf folder (packed or not).
%
%   txt = sqstatus_read(root, key)
%   txt = sqstatus_read(root, key, 'StatusName','status.txt', 'Default','', ...
%                       'ErrorIfMissing',false, 'Raw',false)
%   [txt, info] = sqstatus_read(...)          % info from SQSTATUS_RESOLVE
%   C = sqstatus_read(root, {k1, k2, ...})    % several keys -> cell array of char
%
%   KEY: 'w2026/06/01/visit_001', an absolute path under ROOT, or the autofs key
%   'w2026_06_01_visit_001'. Char or string.
%
%   Returns char text. Trailing newlines are removed unless 'Raw' is true.
%   A missing status file returns 'Default' (''), or errors with
%   'sqstatus:missing' when 'ErrorIfMissing' is true.
%
%   Example:
%       s = sqstatus_read('/data', 'w2026/06/01/visit_001');
%       if strcmp(s, 'done'), ... end
%
%   See also SQSTATUS_WRITE, SQSTATUS_TABLE, SQSTATUS_RESOLVE.

p = inputParser;
p.KeepUnmatched = true;                     % StatusName/Depth/MountRoot -> resolve
p.addParameter('Default', '');
p.addParameter('ErrorIfMissing', false);
p.addParameter('Raw', false);
p.parse(varargin{:});
o = p.Results;
fwd = sq_nv(p.Unmatched);

[keys, isMulti] = sq_cellkeys(key);
out = cell(numel(keys), 1);
for k = 1:numel(keys)
    info = sqstatus_resolve(root, keys{k}, fwd{:});
    if isfile(info.statusFile)
        t = sq_readtext(info.statusFile);
        if ~o.Raw
            t = regexprep(t, '[\r\n]+$', '');
        end
    elseif o.ErrorIfMissing
        error('sqstatus:missing', 'No status file for %s (looked at %s).', ...
            info.folder, info.statusFile);
    else
        t = o.Default;
    end
    out{k} = t;
end

if isMulti
    txt = out;
else
    txt = out{1};
end
end

