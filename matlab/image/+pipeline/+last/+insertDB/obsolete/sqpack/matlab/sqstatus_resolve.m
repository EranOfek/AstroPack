function info = sqstatus_resolve(root, key, varargin)
%SQSTATUS_RESOLVE  Locate a leaf folder and its status file, packed or not.
%
%   info = sqstatus_resolve(root, key)
%   info = sqstatus_resolve(root, key, 'StatusName','status.txt', 'Depth',4, 'MountRoot','/sq')
%
%   KEY identifies the leaf folder and may be any of (char or string):
%       'w2026/06/01/visit_001'          path relative to ROOT (the table key)
%       '/data/w2026/06/01/visit_001'    absolute path under ROOT
%       'w2026_06_01_visit_001'          autofs key (only the last level may contain '_')
%
%   Status location rule (shared with sqpack.py):
%       <leaf>.meta/<StatusName>   if <leaf>.meta/ or <leaf>.sqfs exists (packed leaf)
%       <leaf>/<StatusName>        otherwise (ordinary folder)
%   The rule never looks through the <leaf> symlink, so it never triggers an automount.
%
%   INFO fields:
%       root, folder (relative key), autofsKey, leaf, image, metaDir, mountPoint,
%       packed (image exists), statusDir, statusFile, statusName
%
%   Example:
%       info = sqstatus_resolve('/data', 'w2026/06/01/visit_001');
%       info.statusFile   % -> /data/w2026/06/01/visit_001.meta/status.txt when packed
%
%   See also SQSTATUS_READ, SQSTATUS_WRITE, SQSTATUS_LIST, SQSTATUS_DEBUG.

p = inputParser;
p.addParameter('StatusName', 'status.txt');
p.addParameter('Depth', 4);
p.addParameter('MountRoot', '/sq');
p.parse(varargin{:});
o = p.Results;

root       = regexprep(sq_char(root), '/+$', '');
key        = strtrim(sq_char(key));
statusName = sq_char(o.StatusName);
mountRoot  = regexprep(sq_char(o.MountRoot), '/+$', '');
depth      = o.Depth;

if isempty(root) || root(1) ~= '/'
    error('sqstatus:badRoot', 'Root must be an absolute path, got "%s".', root);
end
if isempty(statusName) || any(statusName == '/')
    error('sqstatus:badInput', 'StatusName must be a plain file name, got "%s".', statusName);
end
key = regexprep(key, '/+$', '');
if isempty(key)
    error('sqstatus:badKey', 'Empty folder key.');
end

% ---- key -> relative folder path -------------------------------------------
if key(1) == '/'
    pre = [root '/'];
    if ~strncmp(key, pre, numel(pre))
        error('sqstatus:badKey', '"%s" is not under root "%s".', key, root);
    end
    rel = key(numel(pre)+1:end);
elseif any(key == '/')
    rel = key;
else
    % autofs key: first Depth-1 levels are separated by '_', the last level is the rest
    pat = ['^' repmat('([^_]+)_', 1, depth-1) '(.+)$'];
    tok = regexp(key, pat, 'tokens', 'once');
    if isempty(tok)
        error('sqstatus:badKey', ...
            '"%s" is neither a path nor an autofs key with %d levels.', key, depth);
    end
    rel = strjoin(tok, '/');
end

parts = regexp(rel, '/', 'split');      % no collapsing: 'a//b' is invalid
if numel(parts) ~= depth
    error('sqstatus:badKey', '"%s" has %d levels, expected %d (e.g. w2026/06/01/visit).', ...
        rel, numel(parts), depth);
end
for k = 1:depth
    pk = parts{k};
    if isempty(pk) || pk(1) == '.'
        error('sqstatus:badKey', 'Invalid level "%s" in "%s".', pk, rel);
    end
    if k < depth && any(pk == '_')
        error('sqstatus:badKey', ...
            'Level "%s" contains "_"; only the last level may (autofs key encoding).', pk);
    end
end
if endsWith(parts{end}, '.meta') || endsWith(parts{end}, '.sqfs')
    error('sqstatus:badKey', 'Give the folder name, not its .meta/.sqfs companion: "%s".', rel);
end

% ---- paths -------------------------------------------------------------------
info.root       = root;
info.folder     = rel;
info.autofsKey  = strjoin(parts, '_');
info.leaf       = [root '/' rel];
info.image      = [info.leaf '.sqfs'];
info.metaDir    = [info.leaf '.meta'];
info.mountPoint = [mountRoot '/' info.autofsKey];
info.packed     = isfile(info.image);
info.statusName = statusName;
if info.packed || isfolder(info.metaDir)
    info.statusDir = info.metaDir;
else
    info.statusDir = info.leaf;
end
info.statusFile = [info.statusDir '/' statusName];
end
