%DEMO_STATUSDB  Walk-through of the StatusDB class.
%
%   Edit PGARGS below (or define it before running), then:  demo_statusdb
%   Uses its own table 'visit_status_demo' and drops it at the end
%   (set KEEP_DEMO = true beforehand to keep it for a look in psql).

if ~exist('pgargs', 'var')
    pgargs = {'Server', 'euclid', 'Database', 'obs', 'User', 'pipeline'};
end
if ~exist('KEEP_DEMO', 'var')
    KEEP_DEMO = false;
end
isOctave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
if isOctave && isempty(which('postgresql'))
    addpath(fullfile(fileparts(mfilename('fullpath')), 'test_mock'));   % Octave: psql-based stand-in
end

%% 1. Connect and create the table (idempotent)
st = StatusDB(pgargs{:}, 'Table', 'visit_status_demo', 'Root', '/data');
st.createSchema();                        % table + history log + triggers

%% 2. One folder: the key is the folder path relative to the data root
st.write('w2026/06/01/visit_001', 'done');
fprintf('\n2. visit_001: "%s" / "%s" / "%s"\n', ...
        st.read('w2026/06/01/visit_001'), ...          % relative key
        st.read('w2026_06_01_visit_001'), ...          % autofs-style key
        st.read('/data/w2026/06/01/visit_001'));       % absolute path (needs Root)
fprintf('   missing folder -> "%s"\n', st.read('w2026/06/01/nope', 'Default', 'unknown'));

%% 3. A whole night at once (one SQL statement per 2000 rows)
night = arrayfun(@(k) sprintf('w2026/06/02/visit_%03d', k), (1:480)', 'UniformOutput', false);
st.write(night, 'queued');
fprintf('\n3. queued %d visits of w2026/06/02\n', numel(night));

%% 4. Parallel-safe work queue
%  Run this loop in several MATLAB sessions / parfor workers at once: claim uses
%  SELECT ... FOR UPDATE SKIP LOCKED, so no visit is processed twice.
nDone = 0; nFail = 0;
while true
    got = st.claim([], 'queued', 'running', 'Limit', 50, 'Prefix', 'w2026/06/02');
    if isempty(got), break, end
    ok = rand(numel(got), 1) > 0.05;                        % pretend ~5% fail
    if any(ok),  st.write(got(ok), 'done');  end
    if any(~ok), st.write(got(~ok), 'failed: no sources detected'); end
    nDone = nDone + sum(ok); nFail = nFail + sum(~ok);
end
fprintf('\n4. processed: %d done, %d failed\n', nDone, nFail);

%% 5. Summary per status
fprintf('\n5. summary:\n');
st.summary('w2026/06');

%% 6. Table keyed by folder (MATLAB) or struct list (Octave), requeue the failures
if ~isOctave
    T = st.asTable('w2026/06/02', 'Status', 'failed: no sources detected');
    disp(head(T, 3));
    T.Status(:) = "queued";
    fprintf('6. requeued %d failed visits via the table\n', st.writeTable(T));
else
    F = st.list('w2026/06/02', 'Status', 'failed: no sources detected');
    if ~isempty(F), st.write({F.folder}, 'queued'); end
    fprintf('\n6. requeued %d failed visits (struct list; tables need MATLAB)\n', numel(F));
end

%% 7. Append a line instead of replacing (atomic in the server), then history
k = 'w2026/06/01/visit_001';
st.write(k, 'reprocessed with new PSF', 'Mode', 'append');
fprintf('\n7. visit_001 now:\n%s\n   history:\n', st.read(k));
disp(st.history(k));

%% 8. Scan a folder tree for *.status files, import, rename the originals
root = [tempname '_demo'];
mk = @(f, t) system(sprintf('mkdir -p "$(dirname ''%s'')" && printf ''%%s\\n'' ''%s'' > ''%s''', f, t, f));
mk([root '/w2026/06/03/visit_a/visit_a.status'], 'done (from file)');     % inside the folder
mk([root '/w2026/06/03/visit_b.meta/run.status'], 'failed (from .meta)'); % packed folder
system(sprintf('ln -s /sq/w2026_06_03_visit_b ''%s/w2026/06/03/visit_b''', root));
mk([root '/w2026/06/03/visit_c/cat.fits'], 'data');
mk([root '/w2026/06/03/visit_c.status'], 'queued (sibling file)');        % sibling file
fprintf('\n8. dry run, then import with Originals=rename:\n');
st.importStatusFiles(root, 'w2026', 'DryRun', true);
rep = st.importStatusFiles(root, 'w2026', 'Originals', 'rename');
system(sprintf('cd ''%s'' && find . -name "*.status*" | sort', root));
system(['rm -rf ''' root '''']);

%% 9. Diagnose the setup (also runs a quoting/claim/remove self test)
st.check();

%% Clean up
if ~KEEP_DEMO
    execute(st.conn, ['DROP TABLE IF EXISTS ' st.table ', ' st.logTable ' CASCADE']);
    execute(st.conn, ['DROP FUNCTION IF EXISTS ' st.table '_touch_fn(), ' st.table '_log_fn()']);
    fprintf('demo table dropped\n');
end
clear st                                  % closes the connection
