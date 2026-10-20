function S = sqstatus_list(root, sub, varargin)
%SQSTATUS_LIST  Find leaf folders under ROOT/SUB and read their status files.
%
%   S = sqstatus_list(root, sub)
%   S = sqstatus_list(root, sub, 'StatusName','status.txt', 'Depth',4, 'ReadStatus',true)
%
%   SUB narrows the scan, e.g. 'w2026/06/01' (one night), 'w2026/06' (a month)
%   or '' (everything under ROOT; slow with millions of folders).
%
%   Both ordinary folders and packed leaves (<visit>.sqfs) are found. The scan
%   uses `find` without following symlinks, so it never triggers automounts.
%   Hidden entries, *.meta, *.sqfs.tmp and .packold folders are ignored.
%
%   S is a struct array (works in MATLAB and Octave) with fields:
%       folder      'w2026/06/01/visit_001'  (the key)
%       status      status text, trailing newlines removed ('' if missing)
%       exists      true if the status file exists
%       packed      true if <visit>.sqfs exists
%       modified    datenum of the status file (NaN if missing)
%       statusFile  full path of the status file
%
%   See also SQSTATUS_TABLE, SQSTATUS_READ.

if nargin < 2 || isempty(sub)
    sub = '';
end
p = inputParser;
p.addParameter('StatusName', 'status.txt');
p.addParameter('Depth', 4);
p.addParameter('MountRoot', '/sq');
p.addParameter('ReadStatus', true);
p.parse(varargin{:});
o = p.Results;

root = regexprep(sq_char(root), '/+$', '');
sub  = regexprep(regexprep(sq_char(sub), '^/+', ''), '/+$', '');
subParts = {};
if ~isempty(sub)
    subParts = strsplit(sub, '/');
end
m = o.Depth - numel(subParts);
if m < 1
    error('sqstatus:badInput', ...
        'SUB "%s" is already at leaf depth; use sqstatus_read for a single folder.', sub);
end
start = root;
if ~isempty(sub)
    start = [root '/' sub];
end

empty = struct('folder', {}, 'status', {}, 'exists', {}, 'packed', {}, ...
               'modified', {}, 'statusFile', {});
if ~isfolder(start)
    warning('sqstatus:noFolder', 'Nothing to scan: %s does not exist.', start);
    S = empty;
    return
end

% ---- enumerate leaves without following symlinks ---------------------------
cmd = sprintf('find %s -mindepth %d -maxdepth %d \\( -type d -o -type f \\) -printf ''%%y\\t%%P\\n'' 2>/dev/null', ...
              sq_shq(start), m, m);
[~, out] = system(cmd);
lines = regexp(out, '\n', 'split');
folders = cell(numel(lines), 1);
n = 0;
for k = 1:numel(lines)
    ln = lines{k};
    if numel(ln) < 3 || ln(2) ~= sprintf('\t')
        continue
    end
    typ = ln(1);
    rel = ln(3:end);
    parts = strsplit(rel, '/');
    if any(cellfun(@(s) isempty(s) || s(1) == '.', parts))
        continue                                   % hidden: .packold, temp files
    end
    if typ == 'd' && ~endsWith(rel, '.meta')
        leafRel = rel;
    elseif typ == 'f' && endsWith(rel, '.sqfs')
        leafRel = rel(1:end-5);
    else
        continue
    end
    if ~isempty(sub)
        leafRel = [sub '/' leafRel];
    end
    n = n + 1;
    folders{n} = leafRel;
end
folders = unique(folders(1:n));                    % sorted; dedupes .sqfs + folder

% ---- status of each leaf -----------------------------------------------------
S = empty;
for k = numel(folders):-1:1
    try
        info = sqstatus_resolve(root, folders{k}, 'StatusName', o.StatusName, ...
                                'Depth', o.Depth, 'MountRoot', o.MountRoot);
    catch e
        warning('sqstatus:skip', 'Skipping %s: %s', folders{k}, e.message);
        continue
    end
    r.folder = info.folder;
    r.status = '';
    r.exists = isfile(info.statusFile);
    r.packed = info.packed;
    r.modified = NaN;
    r.statusFile = info.statusFile;
    if r.exists
        d = dir(info.statusFile);
        r.modified = d.datenum;
        if o.ReadStatus
            r.status = regexprep(sq_readtext(info.statusFile), '[\r\n]+$', '');
        end
    end
    S(k) = r;
end
S = S(~cellfun(@isempty, {S.folder}));             % drop skipped entries
S = S(:);
end
