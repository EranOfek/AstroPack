function R = sqstatus_debug(root, key, varargin)
%SQSTATUS_DEBUG  Diagnose one leaf folder: layout, status file, symlink, mount.
%
%   sqstatus_debug(root, key)
%   R = sqstatus_debug(root, key, 'StatusName','status.txt', 'MountRoot','/sq', ...
%                      'TestWrite',true, 'Probe',false, 'ProbeFiles',20)
%
%   Prints a report and returns it as a struct (R.warnings lists problems).
%   Checks, without triggering an automount:
%     - how KEY resolves (relative key, autofs key, all derived paths)
%     - whether the leaf is a real folder, a symlink (and to where) or missing
%     - image, .meta folder, status file (size, time, first lines)
%     - leftovers of interrupted runs (*.sqfs.tmp, .packold, status temp files)
%     - whether the status folder is writable ('TestWrite', creates and deletes a temp file)
%     - whether the image is currently mounted
%   'Probe',true additionally lists the image (unsquashfs) and reads up to
%   'ProbeFiles' data files through the normal path, which DOES mount it.
%
%   See also SQ_CHECK_ENV, SQ_READBENCH, SQSTATUS_RESOLVE.

p = inputParser;
p.addParameter('StatusName', 'status.txt');
p.addParameter('Depth', 4);
p.addParameter('MountRoot', '/sq');
p.addParameter('TestWrite', true);
p.addParameter('Probe', false);
p.addParameter('ProbeFiles', 20);
p.parse(varargin{:});
o = p.Results;

W = {};
info = sqstatus_resolve(root, key, 'StatusName', o.StatusName, ...
                        'Depth', o.Depth, 'MountRoot', o.MountRoot);
R.info = info;

fprintf('\n== sqstatus_debug: %s\n', info.folder);
show('autofs key', info.autofsKey);
show('leaf', info.leaf);

% ---- leaf type without following the symlink --------------------------------
[st, typ] = system(['stat -c %F ' sq_shq(info.leaf) ' 2>/dev/null']);
typ = strtrim(typ);
if st ~= 0
    typ = 'missing';
end
R.leafType = typ;
show('leaf type', typ);
R.linkTarget = '';
if strcmp(typ, 'symbolic link')
    [~, tgt] = system(['readlink ' sq_shq(info.leaf)]);
    R.linkTarget = strtrim(tgt);
    show('symlink ->', R.linkTarget);
    if ~strcmp(R.linkTarget, info.mountPoint)
        W{end+1} = sprintf('symlink points to %s, expected %s (MountRoot or key mismatch)', ...
                           R.linkTarget, info.mountPoint);
    end
end

% ---- image / meta -------------------------------------------------------------
R.imageBytes = NaN;
if info.packed
    d = dir(info.image);
    R.imageBytes = d.bytes;
    show('image', sprintf('%s  (%.1f MB, %s)', info.image, d.bytes/1e6, ...
                          datestr(d.datenum, 'yyyy-mm-dd HH:MM')));
else
    show('image', '(none: not packed)');
end
show('meta dir', yesno(isfolder(info.metaDir), info.metaDir));
show('packed', yesno(info.packed, ''));

if info.packed && strcmp(typ, 'directory')
    W{end+1} = 'image exists but leaf is still a real folder: packing was interrupted (re-run sqpack.py pack)';
end
if ~info.packed && strcmp(typ, 'symbolic link')
    W{end+1} = 'leaf is a symlink but the image is missing: data is unreachable!';
end
if ~info.packed && strcmp(typ, 'missing')
    W{end+1} = 'neither folder nor image exists';
end
if isfile([info.leaf '.sqfs.tmp'])
    W{end+1} = 'leftover .sqfs.tmp (interrupted pack; sqpack.py redoes it)';
end
[parent, name] = sq_split(info.leaf);
if isfolder([parent '/.' name '.packold'])
    W{end+1} = 'leftover .packold folder (sqpack.py removes it on the next run)';
end

% ---- status file -------------------------------------------------------------
show('status file', info.statusFile);
R.statusExists = isfile(info.statusFile);
R.status = '';
if R.statusExists
    d = dir(info.statusFile);
    R.status = regexprep(sq_readtext(info.statusFile), '[\r\n]+$', '');
    show('status size', sprintf('%d bytes, modified %s', d.bytes, ...
                                datestr(d.datenum, 'yyyy-mm-dd HH:MM:SS')));
    lines = regexp(R.status, '\r?\n', 'split');
    for k = 1:min(5, numel(lines))
        show(ifelse(k == 1, 'status text', ''), ['| ' lines{k}]);
    end
    if numel(lines) > 5
        show('', sprintf('| ... (%d lines)', numel(lines)));
    end
else
    show('status text', '(no status file)');
end
if isfolder(info.statusDir) && ~(strcmp(info.statusDir, info.leaf) && strcmp(typ, 'symbolic link'))
    tmps = dir([info.statusDir '/.' info.statusName '.tmp.*']);
    if ~isempty(tmps)
        W{end+1} = sprintf('%d leftover status temp file(s) in %s (crashed writer)', ...
                           numel(tmps), info.statusDir);
    end
end

% ---- writable? -----------------------------------------------------------------
R.writable = NaN;
if o.TestWrite
    probeDir = info.statusDir;
    if info.packed && ~isfolder(probeDir)
        probeDir = parent;                % .meta would be created here on first write
    end
    if isfolder(probeDir)
        [~, tok] = fileparts(tempname);
        f = [probeDir '/.sqstatus_debug.' tok];
        fid = fopen(f, 'w');
        R.writable = fid >= 0;
        if fid >= 0
            fclose(fid);
            delete(f);
        end
        show('writable', yesno(R.writable, probeDir));
        if ~R.writable
            W{end+1} = sprintf('cannot write in %s', probeDir);
        end
    end
end

% ---- mount state ---------------------------------------------------------------
[st, ~] = system(sprintf('grep -qF %s /proc/mounts', sq_shq([' ' info.mountPoint ' '])));
R.mounted = st == 0;
show('mounted now', yesno(R.mounted, info.mountPoint));

% ---- optional probe (mounts the image) ----------------------------------------
if o.Probe && info.packed
    [st, out] = system(['unsquashfs -l ' sq_shq(info.image) ' 2>&1']);
    if st == 0
        nimg = numel(regexp(out, 'squashfs-root/[^\n]+', 'match'));
        show('image files', sprintf('%d entries', nimg));
    else
        W{end+1} = ['unsquashfs failed: ' strtrim(out)];
    end
    t0 = tic;
    d = dir(info.leaf);                    % follows the symlink -> automount
    d = d(~[d.isdir]);
    tm = toc(t0);
    if isempty(d)
        W{end+1} = sprintf('no files visible through %s (autofs not set up? see sq_check_env)', info.leaf);
    else
        nr = min(o.ProbeFiles, numel(d));
        t1 = tic; nb = 0;
        for k = 1:nr
            fid = fopen([info.leaf '/' d(k).name], 'r');
            if fid >= 0
                nb = nb + numel(fread(fid, Inf, '*uint8'));
                fclose(fid);
            end
        end
        tr = toc(t1);
        show('probe', sprintf('listed %d files in %.3f s; read %d files (%.1f MB) in %.3f s', ...
                              numel(d), tm, nr, nb/1e6, tr));
    end
end

R.warnings = W;
if isempty(W)
    fprintf('  %-14s %s\n', 'result', 'OK');
else
    for k = 1:numel(W)
        fprintf('  %-14s %s\n', 'WARNING', W{k});
    end
end
fprintf('\n');
if nargout == 0
    clear R
end
end

function show(label, val)
fprintf('  %-14s %s\n', label, val);
end

function s = yesno(tf, extra)
if tf
    s = strtrim(['yes  ' extra]);
else
    s = strtrim(['no   ' extra]);
end
end

function v = ifelse(c, a, b)
if c, v = a; else, v = b; end
end

function [parent, name] = sq_split(path)
i = find(path == '/', 1, 'last');
parent = path(1:i-1);
name = path(i+1:end);
end
