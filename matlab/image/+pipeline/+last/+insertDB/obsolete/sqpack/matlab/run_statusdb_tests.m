function results = run_statusdb_tests(varargin)
%RUN_STATUSDB_TESTS  Self-contained tests for StatusDB and the sqstatus_* file tools.
%
%   On euclid (MATLAB + Database Toolbox):
%       results = run_statusdb_tests('Server','euclid', 'Database','obs', 'User','pipeline')
%   Arguments go to the StatusDB constructor (password from setSecret("pgstatus")
%   or PGPASSWORD). The tests create a scratch table statusdb_test_<random>
%   (plus its _log table and trigger functions) and drop everything at the end;
%   your real status table is never touched. Folder-tree tests use tempdir.
%
%   In GNU Octave (no Database Toolbox) the stand-in in test_mock/ runs the SQL
%   through psql; MATLAB-only tests (tables) are skipped there.
%
%   RESULTS: struct array with name, passed (true/false/[] = skipped), message, seconds.

here = fileparts(mfilename('fullpath'));
isOctave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
if isOctave && isempty(which('postgresql'))
    addpath(fullfile(here, 'test_mock'));
end

[~, tok] = fileparts(tempname);
tname = ['statusdb_test_' lower(regexprep(tok, '[^A-Za-z0-9]', ''))];
st = StatusDB(varargin{:}, 'Table', tname, 'Root', '/data');
cleaner = onCleanup(@() drop_scratch(st));
st.createSchema();

tests = {
    'key normalization',              @t_keys
    'quoting and unicode round trip', @t_quoting
    'read/write single and multi',    @t_single
    'bulk write, chunks, duplicates', @t_bulk
    'unchanged rows not touched',     @t_unchanged
    'append mode',                    @t_append
    'insert_only mode',               @t_insert_only
    'list: prefix, filters, limit',   @t_list
    'summary counts',                 @t_summary
    'claim: compare-and-set, queue',  @t_claim
    'remove and history',             @t_history
    'tables (MATLAB only)',           @t_tables
    'object lifecycle',               @t_lifecycle
    'scan: dry run',                  @t_scan_dry
    'scan: insert, verify, rename',   @t_scan_rename
    'scan: delete, replace mode',     @t_scan_delete
    'scan: several files per folder', @t_scan_multiple
    'scan: after real sqpack.py',     @t_scan_packed
    'file tools (sqstatus_*)',        @t_files
    'sqstatus_debug folder report',   @t_debug
    'check() self test',              @t_check
    };

fprintf('\nrun_statusdb_tests on table %s (%s)\n\n', tname, ifelse(isOctave, 'Octave + psql stand-in', 'MATLAB'));
results = struct('name', tests(:, 1), 'passed', [], 'message', '', 'seconds', 0);
for k = 1:size(tests, 1)
    pg_exec(st, ['TRUNCATE ' st.table ', ' st.logTable]);
    t0 = tic;
    try
        msg = tests{k, 2}(st);
        if ischar(msg) && strncmp(msg, 'SKIP', 4)
            results(k).passed = [];
            results(k).message = msg;
        else
            results(k).passed = true;
        end
    catch e
        results(k).passed = false;
        results(k).message = e.message;
    end
    results(k).seconds = toc(t0);
    if isempty(results(k).passed)
        tag = 'skip';
    elseif results(k).passed
        tag = 'PASS';
    else
        tag = 'FAIL';
    end
    fprintf('  %-4s  %-34s %6.2f s  %s\n', tag, results(k).name, results(k).seconds, ...
            strtok(results(k).message, sprintf('\n')));
end
np = sum(cellfun(@(x) isequal(x, true), {results.passed}));
nf = sum(cellfun(@(x) isequal(x, false), {results.passed}));
fprintf('\n%d passed, %d failed, %d skipped\n\n', np, nf, numel(results) - np - nf);
end

% ======================================================================== DB tests

function msg = t_keys(st)
msg = '';
f = st.folderKey({'w2026/06/01/visit_001', 'w2026_06_01_visit_001', ...
                  '/data/w2026/06/01/visit_001', 'w2026/06/01/visit_001/'});
chk(all(strcmp(f, 'w2026/06/01/visit_001')), 'all key forms normalize to the same folder');
chk(strcmp(st.folderKey('w2026_06_01_a_b_c'), 'w2026/06/01/a_b_c'), 'autofs key keeps _ in last level');
bad = {'w2026/06/visit', 'w2026//06/01/v', 'w2026/06/01/.hidden', '/other/w2026/06/01/v', 'w2026_06', ''};
for k = 1:numel(bad)
    expect_error(@() st.folderKey(bad{k}), 'statusdb:badKey', bad{k});
end
expect_error(@() st.write('w2026/06/01/v', sprintf('a%cb', char(0))), 'statusdb:badInput', 'NUL');
expect_error(@() StatusDB('Connection', st.conn, 'Table', 'Bad-Name'), 'statusdb:badInput', 'table name');
end

function msg = t_quoting(st)
msg = '';
uni = native2unicode(uint8([206 177 206 178 32 226 156 147 32 195 188]), 'UTF-8');   % 'αβ ✓ ü'
texts = {'it''s', 'back\slash \n not a newline', '50% done_ok', sprintf('multi\nline\ttab'), ...
         uni, '', 'two '''' quotes', 'x''); DROP TABLE x; --', '$f$ dollar $$', '"double"'};
folders = arrayfun(@(k) sprintf('w2026/06/01/q%02d', k), 1:numel(texts), 'UniformOutput', false);
st.write(folders, texts);
got = st.read(folders);
for k = 1:numel(texts)
    chk(strcmp(got{k}, texts{k}), sprintf('round trip of text %d: [%s] -> [%s]', k, texts{k}, got{k}));
end
end

function msg = t_single(st)
msg = '';
st.write('w2026/06/01/a', 'queued');
chk(strcmp(st.read('w2026/06/01/a'), 'queued'), 'read after write');
st.write('w2026_06_01_a', sprintf('done\n\n'));
chk(strcmp(st.read('/data/w2026/06/01/a'), 'done'), 'overwrite, trailing newlines stripped');
chk(strcmp(st.read('w2026/06/01/zz', 'Default', 'n/a'), 'n/a'), 'missing -> Default');
expect_error(@() st.read('w2026/06/01/zz', 'ErrorIfMissing', true), 'statusdb:missing', 'missing');
st.write('w2026/06/01/b', 'failed');
c = st.read({'w2026/06/01/b', 'w2026/06/01/zz', 'w2026/06/01/a', 'w2026/06/01/b'});
chk(isequal(c, {'failed'; ''; 'done'; 'failed'}), 'multi read keeps order, duplicates, missing');
if ~exist('OCTAVE_VERSION', 'builtin')        % string class: MATLAB only
    s = st.read(["w2026/06/01/a"; "w2026/06/01/b"]);
    chk(isequal(s, {'done'; 'failed'}), 'string array keys');
    st.write("w2026/06/01/c", "via string");
    chk(strcmp(st.read("w2026/06/01/c"), 'via string'), 'string inputs');
end
end

function msg = t_bulk(st)
msg = '';
n = 5000;
folders = arrayfun(@(k) sprintf('w2026/06/%02d/v%05d', 1 + mod(k, 2), k), (1:n)', 'UniformOutput', false);
texts = arrayfun(@(k) sprintf('s%d', mod(k, 7)), (1:n)', 'UniformOutput', false);
chk(st.write(folders, texts, 'ChunkSize', 2000) == n, 'rows sent');
chk(isequal(st.read(folders, 'ChunkSize', 1500), texts), 'bulk round trip');
chk(numel(st.list('w2026/06')) == n, 'list sees all rows');
st.write({'w2026/06/01/d', 'w2026/06/01/d', 'w2026/06/01/d'}, {'first', 'second', 'last'});
chk(strcmp(st.read('w2026/06/01/d'), 'last'), 'duplicates in one call: last wins');
st.write(folders(1:10), 'same for all');
chk(all(strcmp(st.read(folders(1:10)), 'same for all')), 'scalar text broadcast');
expect_error(@() st.write(folders(1:3), {'a', 'b'}), 'statusdb:badInput', 'count mismatch');
end

function msg = t_unchanged(st)
msg = '';
k = 'w2026/06/01/u';
st.write(k, 'x');
u1 = st.list(k);
pause(0.05);
st.write(k, 'x');
u2 = st.list(k);
chk(strcmp(u1.updated, u2.updated), 'same text: updated unchanged');
pause(0.05);
st.write(k, 'y');
u3 = st.list(k);
chk(~strcmp(u2.updated, u3.updated) && strcmp(u3.status, 'y'), 'new text: updated moves');
chk(numel(hist_struct(st, k)) == 2, 'history has 2 entries');
end

function msg = t_append(st)
msg = '';
k = 'w2026/06/01/log';
st.write(k, 'started', 'Mode', 'append');
st.write(k, 'step 1', 'Mode', 'append');
st.write({k, k}, {'step 2', 'step 3'}, 'Mode', 'append');
chk(strcmp(st.read(k), sprintf('started\nstep 1\nstep 2\nstep 3')), 'appended lines');
end

function msg = t_insert_only(st)
msg = '';
st.write('w2026/06/01/a', 'db value');
st.write({'w2026/06/01/a', 'w2026/06/01/new'}, {'file value', 'file value'}, 'Mode', 'insert_only');
chk(strcmp(st.read('w2026/06/01/a'), 'db value'), 'existing row kept');
chk(strcmp(st.read('w2026/06/01/new'), 'file value'), 'missing row inserted');
end

function msg = t_list(st)
msg = '';
st.write({'w2026/06/01/visit_001', 'w2026/06/01/visitX001', 'w2026/06/010/a', ...
          'w2026/06/02/b', 'w2026/07/01/c'}, {'done', 'failed', 'done', 'failed', 'queued'});
L = st.list('w2026/06/01');
chk(isequal({L.folder}, {'w2026/06/01/visitX001', 'w2026/06/01/visit_001'}), ...
    'night prefix stops at level boundary, byte-order sorted');
chk(numel(st.list('w2026/06/01/visit_001')) == 1, 'exact folder');
chk(numel(st.list('w2026/06')) == 4, 'month prefix');
chk(numel(st.list('')) == 5, 'empty prefix = all');
F = st.list('w2026', 'Status', 'failed');
chk(isequal({F.folder}, {'w2026/06/01/visitX001', 'w2026/06/02/b'}), 'status filter');
chk(numel(st.list('w2026', 'Status', {'failed', 'queued'})) == 3, 'status list');
chk(numel(st.list('w2026', 'Limit', 2)) == 2, 'limit');
chk(isempty(st.list('w2025')), 'no match -> empty');
chk(~isempty(regexp(L(1).updated, '^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d\.\d{3}$', 'once')), 'updated format');
% LIKE wildcards must be literal: needs '_' in a non-last level, so a Depth-0 view
st0 = StatusDB('Connection', st.conn, 'Table', st.table, 'Depth', 0);
st0.write({'x_y/1', 'xzy/1', 'x%y/1'}, 'v');
A = st0.list('x_y');
B = st0.list('x%y');
chk(isequal({A.folder}, {'x_y/1'}), '_ in prefix is literal');
chk(isequal({B.folder}, {'x%y/1'}), '% in prefix is literal');
end

function msg = t_summary(st)
msg = '';
st.write({'w2026/06/01/a', 'w2026/06/01/b', 'w2026/06/01/c', 'w2026/06/02/d'}, ...
         {'done', 'done', 'failed', 'done'});
C = st.summary('w2026/06/01');
if isa(C, 'table')
    s = cellstr(C.status); n = C.count;
else
    s = {C.status}; n = [C.count];
end
chk(isequal(s(:)', {'done', 'failed'}) && isequal(n(:)', [2 1]), 'counts per status');
end

function msg = t_claim(st)
msg = '';
q = arrayfun(@(k) sprintf('w2026/06/01/j%d', k), 1:5, 'UniformOutput', false);
st.write(q, 'queued');
st.write('w2026/06/02/other', 'queued');
got = st.claim(q(1:2), 'queued', 'running');
chk(isequal(got(:)', q(1:2)), 'CAS on keys claims both');
chk(isempty(st.claim(q(1:2), 'queued', 'running')), 'second CAS claims nothing');
got1 = st.claim([], 'queued', 'running', 'Limit', 2, 'Prefix', 'w2026/06/01');
got2 = st.claim([], 'queued', 'running', 'Limit', 5, 'Prefix', 'w2026/06/01');
chk(numel(got1) == 2 && numel(got2) == 1 && isempty(intersect(got1, got2)), 'queue: no double claim');
chk(strcmp(st.read('w2026/06/02/other'), 'queued'), 'prefix respected');
got = st.claim([], {'queued', 'failed'}, 'running', 'Limit', 10);
chk(isequal(got, {'w2026/06/02/other'}), 'fromStatus list, no prefix');
chk(isempty(st.claim([], 'queued', 'running')), 'empty queue -> empty');
end

function msg = t_history(st)
msg = '';
k = 'w2026/06/01/h';
st.write(k, 'a');
st.write(k, 'b');
st.write(k, 'c');
st.remove(k);
chk(isempty(st.read(k)), 'removed');
H = hist_struct(st, k);
chk(numel(H) == 4, sprintf('4 history rows (got %d)', numel(H)));
chk(isequal({H.old_status}, {'', 'a', 'b', 'c'}) && isequal({H.new_status}, {'a', 'b', 'c', ''}), ...
    'old/new sequence');
H2 = hist_struct(st, k, 'Limit', 2);
chk(isequal({H2.new_status}, {'c', ''}), 'Limit returns newest, oldest first');
end

function msg = t_tables(st)
msg = '';
if isempty(which('table'))
    msg = 'SKIP (no MATLAB tables)';
    return
end
st.write({'w2026/06/01/a', 'w2026/06/01/b', 'w2026/06/01/c'}, {'done', 'failed', 'failed'});
T = st.asTable('w2026/06/01');
chk(height(T) == 3 && isstring(T.Status) && isdatetime(T.Updated), 'table shape and types');
chk(T{"w2026/06/01/b", "Status"} == "failed", 'row-name indexing');
T.Status(T.Status == "failed") = "queued";
st.writeTable(T);
chk(isequal(st.read({'w2026/06/01/b'; 'w2026/06/01/c'}), {'queued'; 'queued'}), 'write back');
chk(height(st.asTable('w2025')) == 0, 'empty table');
H = st.history('w2026/06/01/b');
chk(isa(H, 'table') && height(H) == 2, 'history table');
end

function msg = t_lifecycle(st)
msg = '';
v = StatusDB('Connection', st.conn, 'Table', st.table);   % borrows the connection
v.write('w2026/06/01/life', 'ok');
clear v                                                  % must NOT close the shared connection
chk(strcmp(st.read('w2026/06/01/life'), 'ok'), 'borrowed connection survives clear');
chk(isa(st, 'handle') && ~isempty(st.conn), 'handle object keeps its connection');
end

% =================================================================== scan / import

function msg = t_scan_dry(st)
msg = '';
root = make_scan_tree();
c = onCleanup(@() system(['rm -rf ' sq_shq(root)]));
before = list_files(root);
[~, rep] = quiet(@() st.importStatusFiles(root, 'w2026/06', 'DryRun', true));
chk(rep.nights == 2, sprintf('nights (%d)', rep.nights));
chk(rep.files == 5 && rep.folders == 4 && rep.multiple == 1, ...
    sprintf('files %d, folders %d, multiple %d', rep.files, rep.folders, rep.multiple));
chk(numel(rep.errors) == 1 && ~isempty(strfind(rep.errors{1}, 'ghost')), 'orphan sibling reported');
chk(rep.written == 0 && isempty(st.list('')), 'nothing written');
chk(isequal(before, list_files(root)), 'no file touched');
end

function msg = t_scan_rename(st)
msg = '';
root = make_scan_tree();
c = onCleanup(@() system(['rm -rf ' sq_shq(root)]));
st.write('w2026/06/01/inside', 'newer in db');                 % must win (insert_only)
[~, rep] = quiet(@() st.importStatusFiles(root, 'w2026/06', 'Originals', 'rename'));
chk(rep.written == 4 && rep.verified == 3 && rep.differ == 1, ...
    sprintf('written %d verified %d differ %d', rep.written, rep.verified, rep.differ));
chk(strcmp(rep.differExamples{1}, 'w2026/06/01/inside'), 'differing folder reported');
chk(strcmp(st.read('w2026/06/01/inside'), 'newer in db'), 'insert_only kept DB value');
chk(isfile([root '/w2026/06/01/inside/run.status']), 'differing file NOT renamed');
chk(strcmp(st.read('w2026/06/01/packed'), 'failed'), 'packed (.meta) status');
chk(strcmp(st.read('w2026/06/02/vB'), sprintf('queued\nline 2')), 'sibling status, multi-line');
chk(strcmp(st.read('w2026/06/01/multi'), 'newest'), 'OnMultiple=newest');
chk(isfile([root '/w2026/06/01/packed.meta/x.status.imported']) && ...
    isfile([root '/w2026/06/02/vB.status.imported']) && ...
    isfile([root '/w2026/06/01/multi/b.status.imported']), 'verified files renamed');
chk(isfile([root '/w2026/06/01/multi/a.status']), 'older duplicate left alone (newest)');
chk(isfile([root '/w2026/06/01/inside/status.txt']), 'non-matching file untouched');
[~, rep2] = quiet(@() st.importStatusFiles(root, 'w2026/06', 'Originals', 'rename'));
chk(rep2.files == 2 && rep2.written == 2 && rep2.renamed == 0 && rep2.verified == 0, ...
    sprintf('re-run sees only unrenamed files (%d files, %d written, %d renamed)', ...
            rep2.files, rep2.written, rep2.renamed));
end

function msg = t_scan_delete(st)
msg = '';
root = make_scan_tree();
c = onCleanup(@() system(['rm -rf ' sq_shq(root)]));
st.write('w2026/06/01/inside', 'old db value');
[~, rep] = quiet(@() st.importStatusFiles(root, 'w2026/06/01', 'Mode', 'replace', ...
                                          'Originals', 'delete', 'OnMultiple', 'join'));
chk(rep.nights == 1 && rep.verified == 3 && rep.deleted == 4, ...
    sprintf('night only; verified %d, deleted %d', rep.verified, rep.deleted));
chk(strcmp(st.read('w2026/06/01/inside'), 'from file'), 'replace: file wins');
chk(strcmp(st.read('w2026/06/01/multi'), sprintf('older\nnewest')), 'OnMultiple=join, oldest first');
chk(~isfile([root '/w2026/06/01/inside/run.status']) && ...
    ~isfile([root '/w2026/06/01/multi/a.status']) && ~isfile([root '/w2026/06/01/multi/b.status']), ...
    'files deleted');
chk(isfile([root '/w2026/06/02/vB.status']), 'other night untouched');
end

function msg = t_scan_multiple(st)
msg = '';
root = make_scan_tree();
c = onCleanup(@() system(['rm -rf ' sq_shq(root)]));
[~, rep] = quiet(@() st.importStatusFiles(root, 'w2026/06/01', 'OnMultiple', 'skip', 'Originals', 'delete'));
chk(isempty(st.read('w2026/06/01/multi')) && rep.multiple == 1, 'skip: nothing written for that folder');
chk(isfile([root '/w2026/06/01/multi/a.status']) && isfile([root '/w2026/06/01/multi/b.status']), ...
    'skip: files kept');
[~, rep] = quiet(@() st.importStatusFiles(root, 'w2026/06/01', 'Pattern', 'status.txt', 'Mode', 'replace'));
chk(rep.files == 1 && strcmp(st.read('w2026/06/01/inside'), 'legacy txt'), 'exact-name Pattern');
expect_error(@() st.importStatusFiles(root, 'w2026/06/01/inside'), 'statusdb:badInput', 'below night');
end

function msg = t_scan_packed(st)
msg = '';
here = fileparts(mfilename('fullpath'));
sqpack = fullfile(here, '..', 'sqpack.py');
[s1, ~] = system('command -v mksquashfs && command -v python3');
if s1 ~= 0 || ~isfile(sqpack)
    msg = 'SKIP (needs mksquashfs, python3 and ../sqpack.py)';
    return
end
root = [tempname '_pack'];
c = onCleanup(@() system(['rm -rf ' sq_shq(root)]));
for v = {'w2026/06/03/vA', 'w2026/06/03/vB'}
    d = [root '/' v{1}];
    mkdir(d);
    for k = 1:5
        fid = fopen(sprintf('%s/cat_%d.fits', d, k), 'w'); fwrite(fid, rand(514, 4), 'double'); fclose(fid);
    end
    fid = fopen([d '/pipeline.status'], 'w'); fprintf(fid, 'done %s\n', v{1}); fclose(fid);
end
[s, out] = system(sprintf('python3 %s pack --root %s --exclude ''*.status'' --min-age-min 0 --log /dev/null 2>&1', ...
                          sq_shq(sqpack), sq_shq(root)));
chk(s == 0, ['sqpack.py failed: ' out]);
chk(isfile([root '/w2026/06/03/vA.meta/pipeline.status']), 'status moved to .meta by sqpack');
[~, rep] = quiet(@() st.importStatusFiles(root, 'w2026', 'Originals', 'rename'));
chk(rep.verified == 2 && rep.renamed == 2, sprintf('verified %d renamed %d', rep.verified, rep.renamed));
chk(strcmp(st.read('w2026/06/03/vB'), 'done w2026/06/03/vB'), 'status from packed leaf');
end

% ====================================================================== file tools

function msg = t_files(~)
msg = '';
root = make_tree();
c = onCleanup(@() system(['rm -rf ' sq_shq(root)]));
chk(strcmp(sqstatus_read(root, 'w2026/06/01/visit_001'), 'done'), 'plain read');
sqstatus_write(root, 'w2026_06_01_visit_001', 'reprocess');
chk(strcmp(sqstatus_read(root, 'w2026/06/01/visit_001'), 'reprocess'), 'plain write');
chk(isempty(dir([root '/w2026/06/01/visit_001/.status.txt.tmp.*'])), 'no temp files left');
i = sqstatus_resolve(root, 'w2026/06/01/packed_1');
chk(i.packed && strcmp(i.statusFile, [root '/w2026/06/01/packed_1.meta/status.txt']), 'packed -> .meta');
chk(strcmp(sqstatus_read(root, 'w2026/06/01/packed_1'), 'failed'), 'packed read');
sqstatus_write(root, 'w2026/06/01/packed_1', 'queued', 'Append', true);
chk(strcmp(sqstatus_read(root, 'w2026/06/01/packed_1'), sprintf('failed\nqueued')), 'packed append');
S = sqstatus_list(root, 'w2026/06');
chk(isequal({S.folder}, {'w2026/06/01/nostatus', 'w2026/06/01/packed_1', 'w2026/06/01/visit_001', ...
                         'w2026/06/02/v.2 b'}), 'list finds plain+packed, skips hidden/tmp/meta');
chk(isequal([S.exists], [false true true true]), 'exists flags');
expect_error(@() sqstatus_write(root, 'w2026/06/09/ghost', 'x'), 'sqstatus:noFolder', 'no leaf');
end

function msg = t_debug(~)
msg = '';
root = make_tree();
c = onCleanup(@() system(['rm -rf ' sq_shq(root)]));
[~, R] = quiet(@() sqstatus_debug(root, 'w2026/06/01/visit_001'));
chk(isempty(R.warnings) && strcmp(R.leafType, 'directory') && R.writable, 'plain leaf: OK');
[~, R] = quiet(@() sqstatus_debug(root, 'w2026/06/01/packed_1'));
chk(isempty(R.warnings) && strcmp(R.leafType, 'symbolic link') && ...
    strcmp(R.linkTarget, '/sq/w2026_06_01_packed_1') && strcmp(R.status, 'failed'), 'packed leaf: OK');
mkdir([root '/w2026/06/01/.packed_1.packold']);
fid = fopen([root '/w2026/06/01/visit_001.sqfs'], 'w'); fclose(fid);
[~, R] = quiet(@() sqstatus_debug(root, 'w2026/06/01/packed_1', 'MountRoot', '/elsewhere'));
chk(numel(R.warnings) == 2, sprintf('packold + wrong mount root flagged (%d warnings)', numel(R.warnings)));
[~, R] = quiet(@() sqstatus_debug(root, 'w2026/06/01/visit_001'));
chk(any(~cellfun(@isempty, regexp(R.warnings, 'interrupted'))), 'image + real folder flagged');
end

function msg = t_check(st)
msg = '';
[~, R] = quiet(@() st.check('Pings', 3));
chk(strcmp(R.selfTest, 'passed'), ['self test: ' R.selfTest]);
chk(R.tableExists && R.logExists && strcmp(R.collation, 'C'), 'schema detected');
chk(isempty(R.warnings), ['warnings: ' strjoin(R.warnings, '; ')]);
end

% ========================================================================= helpers

function [out, R] = quiet(fn)
% run a function that prints a report; capture the printout (MATLAB and Octave)
out = evalc('R = fn();');
end

function H = hist_struct(st, k, varargin)
H = st.history(k, varargin{:});
if isa(H, 'table')
    H = table2struct(H);
    f = fieldnames(H);
    for i = 1:numel(H)
        for j = 1:numel(f), H(i).(f{j}) = char(H(i).(f{j})); end
    end
end
H = H(:)';
end

function root = make_scan_tree()
% *.status files in every supported place, plus things that must be ignored
root = [tempname '_scan'];
n1 = [root '/w2026/06/01'];
n2 = [root '/w2026/06/02'];
put([n1 '/inside/run.status'], 'from file');                 % inside the folder
put([n1 '/inside/status.txt'], 'legacy txt');                % other name: ignored by '*.status'
put([n1 '/inside/cat_1.fits'], 'data');
mkdir([n1 '/packed.meta']);                                   % packed: image + .meta + symlink
put([n1 '/packed.sqfs'], 'hsqs');
put([n1 '/packed.meta/x.status'], 'failed');
system(sprintf('ln -s /sq/w2026_06_01_packed %s', sq_shq([n1 '/packed'])));
put([n1 '/multi/a.status'], 'older');                         % two files, different times
put([n1 '/multi/b.status'], 'newest');
system(sprintf('touch -d "2020-01-01 00:00" %s', sq_shq([n1 '/multi/a.status'])));
put([n2 '/vB/cat_1.fits'], 'data');                           % sibling file next to the folder
put([n2 '/vB.status'], sprintf('queued\nline 2\n'));
put([n2 '/ghost.status'], 'orphan');                          % sibling without a folder
put([n1 '/.hidden/h.status'], 'hidden');                      % hidden: ignored
put([root '/w2026/06/.trash/x/y.status'], 'hidden night');    % hidden night: ignored
end

function root = make_tree()
% status.txt layout for the sqstatus_* file tools
root = [tempname '_tree'];
put([root '/w2026/06/01/visit_001/status.txt'], 'done');
put([root '/w2026/06/01/visit_001/cat_1.fits'], 'data');
put([root '/w2026/06/01/nostatus/cat_1.fits'], 'data');
put([root '/w2026/06/02/v.2 b/status.txt'], sprintf('queued\nline 2\n'));
mkdir([root '/w2026/06/01/packed_1.meta']);
put([root '/w2026/06/01/packed_1.sqfs'], 'hsqs');
put([root '/w2026/06/01/packed_1.meta/status.txt'], sprintf('failed\n'));
system(sprintf('ln -s /sq/w2026_06_01_packed_1 %s', sq_shq([root '/w2026/06/01/packed_1'])));
mkdir([root '/w2026/06/01/.old_1.packold']);
put([root '/w2026/06/01/x.sqfs.tmp'], '');
mkdir([root '/w2026/06/.hidden/v']);
end

function put(f, txt)
d = fileparts(f);
if ~isfolder(d), mkdir(d); end
fid = fopen(f, 'w');
fwrite(fid, txt);
fclose(fid);
end

function L = list_files(root)
[~, out] = system(sprintf('cd %s && find . | LC_ALL=C sort', sq_shq(root)));
L = out;
end

function chk(cond, msg)
if ~cond
    error('test:fail', '%s', msg);
end
end

function expect_error(fn, id, what)
try
    fn();
catch e
    if ~strcmp(e.identifier, id)
        error('test:fail', '%s: expected error %s, got %s (%s)', what, id, e.identifier, e.message);
    end
    return
end
error('test:fail', '%s: expected error %s, none raised', what, id);
end

function drop_scratch(st)
try
    t = st.table;
    pg_exec(st, ['DROP TABLE IF EXISTS ' t ', ' st.logTable ' CASCADE']);
    pg_exec(st, ['DROP FUNCTION IF EXISTS ' t '_touch_fn(), ' t '_log_fn()']);
    st.close();
catch e
    warning('test:cleanup', 'Could not drop scratch table: %s', e.message);
end
end

function v = ifelse(c, a, b)
if c, v = a; else, v = b; end
end
