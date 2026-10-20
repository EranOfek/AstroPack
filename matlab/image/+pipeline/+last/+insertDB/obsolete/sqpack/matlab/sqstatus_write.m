function file = sqstatus_write(root, key, text, varargin)
%SQSTATUS_WRITE  Write (atomically) the status text of a leaf folder.
%
%   sqstatus_write(root, key, text)
%   sqstatus_write(root, key, text, 'StatusName','status.txt', 'Append',false)
%   file = sqstatus_write(...)            % path that was written
%
%   KEY: 'w2026/06/01/visit_001', an absolute path under ROOT, or the autofs key.
%   TEXT: char or string; multi-line text is fine. A trailing newline is added
%   on disk if missing (SQSTATUS_READ strips it again).
%
%   Packed leaves (<leaf>.sqfs exists) are written to <leaf>.meta/<StatusName>;
%   the .meta folder is created if needed. Ordinary folders are written in place.
%   The leaf folder itself is never created: a missing leaf is an error.
%
%   Atomic: the text goes to a temp file in the same folder, which is then
%   renamed over the status file, so readers never see a half-written file.
%   'Append',true reads, appends and rewrites; concurrent appenders can race.
%
%   Example:
%       sqstatus_write('/data', 'w2026/06/01/visit_001', 'done');
%       sqstatus_write('/data', 'w2026_06_01_visit_001', 'reprocessed 2026-10-14', 'Append', true);
%
%   See also SQSTATUS_READ, SQSTATUS_WRITE_TABLE.

p = inputParser;
p.KeepUnmatched = true;
p.addParameter('Append', false);
p.parse(varargin{:});
o = p.Results;
fwd = sq_nv(p.Unmatched);

info = sqstatus_resolve(root, key, fwd{:});
text = sq_char(text);
nl = sprintf('\n');

if info.packed
    if ~isfolder(info.metaDir)
        [ok, msg] = mkdir(info.metaDir);
        if ~ok
            error('sqstatus:io', 'Cannot create %s: %s', info.metaDir, msg);
        end
    end
elseif ~isfolder(info.statusDir)
    error('sqstatus:noFolder', 'Folder %s does not exist (not creating it).', info.statusDir);
end

if o.Append && isfile(info.statusFile)
    old = sq_readtext(info.statusFile);
    if ~isempty(old) && old(end) ~= nl
        old = [old nl];
    end
    text = [old text];
end
if isempty(text) || text(end) ~= nl
    text = [text nl];
end

% unique temp name in the same directory -> rename is atomic on one filesystem
[~, tok] = fileparts(tempname);
tmp = [info.statusDir '/.' info.statusName '.tmp.' tok];
fid = fopen(tmp, 'w');
if fid < 0
    error('sqstatus:io', 'Cannot write in %s (permissions? read-only mount?).', info.statusDir);
end
try
    fwrite(fid, unicode2native(text, 'UTF-8'), 'uint8');
    if fclose(fid) ~= 0
        error('sqstatus:io', 'Error closing %s.', tmp);
    end
    [ok, msg] = movefile(tmp, info.statusFile, 'f');
    if ~ok
        error('sqstatus:io', 'Cannot rename %s -> %s: %s', tmp, info.statusFile, msg);
    end
catch e
    if any(fopen('all') == fid)
        fclose(fid);
    end
    if isfile(tmp)
        delete(tmp);
    end
    rethrow(e);
end
file = info.statusFile;
end
