function nfail = run_tests(pattern)
%RUN_TESTS  Unit tests of the Matlab client. Works in Matlab R2020+ and in GNU Octave 7+.
%   io.chlog.tests.run_tests              all tests (matlab/util on path)
%   run_tests('outage')                   tests whose name contains "outage"
%   Octave from a shell:  octave --no-gui --eval "n = run_tests; exit(n > 0)"
%   The tests use the mock adapter and a fake clock. No database and no Python are needed.
if nargin < 1, pattern = ''; end
tests = {@t_ts_string, @t_json_str_utf16_units, @t_json_str_utf8_bytes, @t_row_line_matches_python_contract, ...
         @t_attrs_json, @t_config_defaults_and_overrides, @t_config_struct_init, @t_node_empty_uses_host, @t_log_and_flush_to_mock, @t_fields_and_levels, ...
         @t_outage_spools_then_replays_exactly_once, @t_lost_offset_never_duplicates, @t_torn_last_line_ignored, ...
         @t_poison_row_isolated, @t_row_formats, @t_log_never_throws, @t_perf_helper, @t_front_end_before_init, ...
         @t_spool_full_drops_trc_dbg, @t_file_names_match_python_and_cpp};
nfail = 0; nran = 0;
for k = 1:numel(tests)
    name = func2str(tests{k});
    if ~isempty(pattern) && isempty(strfind(name, pattern)), continue; end %#ok<STREMP>
    nran = nran + 1;
    try
        tests{k}();
        fprintf('PASS %s\n', name);
    catch err
        nfail = nfail + 1;
        id = err.identifier;
        if isempty(id), id = 'MATLAB:error'; end
        fprintf('FAIL %s\n     [%s] %s\n', name, id, strrep(err.message, char(10), ' '));
    end
end
fprintf('\n%d tests, %d failed\n', nran, nfail);
end

% ---- helpers ---------------------------------------------------------------------------------------------
function dir = fresh_dir(tag)
dir = fullfile(tempdir, sprintf('chlog_mtest_%s_%d_%d', tag, io.chlog.ChLogger.envHelper('pid'), round(rand * 1e6)));
if exist(dir, 'dir'), rmdir(dir, 's'); end
mkdir(dir);
end

function c = fake_clock(startMs)
% A clock that advances 1 ms per call, kept in a persistent-like handle via a struct in a closure variable.
state = struct('t', startMs);
c = @() tick_clock();
    function v = tick_clock()
        v = state.t; state.t = state.t + 1;
    end
end

function lines = read_lines(path)
if ~exist(path, 'file'), lines = {}; return; end
t = fileread(path);
lines = strsplit(strtrim(t), char(10));
lines = lines(~cellfun(@isempty, lines));
end

function n = spool_rows(dir)
n = 0;
d = dir_listing(dir);
for k = 1:numel(d), n = n + numel(read_lines(d{k})); end
end

function paths = dir_listing(spool)
d = dir(fullfile(spool, '*.jsonl'));
paths = {};
for k = 1:numel(d)
    if isempty(regexp(d(k).name, '^mock_', 'once')), paths{end + 1} = fullfile(spool, d(k).name); end %#ok<AGROW>
end
end

function client = new_client(spool, varargin)
ov = struct('client', struct('node', 'test-node', 'env', 'unit', 'build', 't1', 'spool_dir', spool, ...
                             'batch_rows', 50, 'flush_interval_ms', 100000, 'replay_chunk_rows', 10, ...
                             'retry_min_s', 0.001, 'retry_max_s', 0.002));
p = struct('Overrides', [], 'InsertFcn', [], 'Matlab', []);
for k = 1:2:numel(varargin), p.(varargin{k}) = varargin{k + 1}; end
if ~isempty(p.Matlab), ov.matlab = p.Matlab; end
if ~isempty(p.Overrides), ov = merge_test_override(ov, p.Overrides); end
cfg = io.chlog.ChLogger.loadConfig('', ov);
clk = fake_clock(1793534400000);                       % 2026-11-01 12:00:00.000 UTC
if isempty(p.InsertFcn), p.InsertFcn = @io.chlog.tests.test_adapter_mock; end
client = io.chlog.ChLogger(cfg, 'Process', 'mtest', 'InsertFcn', p.InsertFcn, 'ClockMs', clk, 'UseTimer', false);
end

function check(cond, msg)
if ~cond, error('check failed: %s', msg); end
end


function base = merge_test_override(base, extra)
if ~isstruct(extra), return; end
names = fieldnames(extra);
for k = 1:numel(names)
    n = names{k};
    if isfield(base, n) && isstruct(base.(n)) && isstruct(extra.(n))
        base.(n) = merge_test_override(base.(n), extra.(n));
    else
        base.(n) = extra.(n);
    end
end
end

% ---- the tests ---------------------------------------------------------------------------------------------
function t_ts_string()
check(strcmp(io.chlog.ChLogger.tsString(1793534400123), '2026-11-01 12:00:00.123'), 'ts 2026-11-01');
check(strcmp(io.chlog.ChLogger.tsString(0), '1970-01-01 00:00:00.000'), 'epoch');
check(strcmp(io.chlog.ChLogger.tsString(1782907200000), '2026-07-01 12:00:00.000'), 'july');
check(strcmp(io.chlog.ChLogger.tsString(951782400000), '2000-02-29 00:00:00.000'), 'leap day 2000');
check(strcmp(io.chlog.ChLogger.tsString(4102444799999), '2099-12-31 23:59:59.999'), 'end of 2099');
check(strcmp(io.chlog.ChLogger.tsString(1793534400999.7), '2026-11-01 12:00:00.999'), 'floors to the millisecond');
end

function t_json_str_utf16_units()
% Matlab path: a char is a UTF-16 code unit.
check(strcmp(io.chlog.ChLogger.jsonUnits([97 34 98 92 99 10 100 9 101]), '"a\"b\\c\nd\te"'), 'escapes');
check(strcmp(io.chlog.ChLogger.jsonUnits(8364), '"\u20ac"'), 'euro sign code unit');
check(strcmp(io.chlog.ChLogger.jsonUnits([55357 56832]), '"\ud83d\ude00"'), 'surrogate pair kept');
check(strcmp(io.chlog.ChLogger.jsonUnits([1 127]), '"\u0001\u007f"'), 'control and DEL');
check(strcmp(io.chlog.ChLogger.jsonUnits(1114111), '"\udbff\udfff"'), 'code point above 65535 becomes a pair');
check(strcmp(io.chlog.ChLogger.jsonUnits([]), '""'), 'empty');
end

function t_json_str_utf8_bytes()
% Octave path: a char is one byte of UTF-8.
check(strcmp(io.chlog.ChLogger.jsonStr(char([226 130 172]), true), '"\u20ac"'), 'euro utf8 bytes');
check(strcmp(io.chlog.ChLogger.jsonStr(char([240 159 152 128]), true), '"\ud83d\ude00"'), 'emoji bytes');
check(strcmp(io.chlog.ChLogger.jsonStr(char(255), true), '"\ufffd"'), 'invalid byte');
check(strcmp(io.chlog.ChLogger.jsonStr(char([192 175]), true), '"\ufffd\ufffd"'), 'overlong form is invalid');
check(strcmp(io.chlog.ChLogger.jsonStr('plain', true), '"plain"'), 'ascii');
out = io.chlog.ChLogger.jsonStr(['x' char([226 130 172]) char(255)], true);
check(all(double(out) >= 32 & double(out) < 127), 'output is ascii');
end

function row = sample_row()
row = struct('ts', '2026-11-01 12:00:00.123', 'level', 'INF', 'node', 'n', 'process', 'p', 'pid', 7, 'tid', [], ...
             'run_id', 'r', 'seq', 3, 'perf_us', [], 'perf_op', '', 'subsystem', '', 'logger', '', 'event_code', 0, ...
             'corr_id', '', 'env', '', 'build', '', 'src', 'f.m:1', 'message', 'x', 'attrs', '{"k":"v"}');
end

function t_row_line_matches_python_contract()
want = ['{"ts":"2026-11-01 12:00:00.123","level":"INF","node":"n","process":"p","pid":7,"tid":null,"run_id":"r","seq":3,' ...
        '"perf_us":null,"perf_op":"","subsystem":"","logger":"","event_code":0,"corr_id":"","env":"","build":"",' ...
        '"src":"f.m:1","message":"x","attrs":{"k":"v"}}'];
check(strcmp(io.chlog.ChLogger.rowLine(sample_row()), want), 'exact line');
r = sample_row(); r.perf_us = 1500; r.tid = 12;
l = io.chlog.ChLogger.rowLine(r);
check(~isempty(strfind(l, '"tid":12,')) && ~isempty(strfind(l, '"perf_us":1500,')), 'numbers'); %#ok<STREMP>
d = jsondecode(l);
check(isempty(jsondecode(io.chlog.ChLogger.rowLine(sample_row())).perf_us), 'null decodes to empty');
check(d.seq == 3, 'valid JSON');
end

function t_attrs_json()
check(strcmp(io.chlog.ChLogger.attrsJson([]), '{}'), 'empty');
check(strcmp(io.chlog.ChLogger.attrsJson(struct('a', 'x', 'n', 5.5, 'b', true, 'i', 3)), '{"a":"x","n":"5.5","b":"true","i":"3"}'), 'values become text');
end

function t_config_defaults_and_overrides()
cfg = io.chlog.ChLogger.loadConfig('', struct('client', struct('node', 'ccs-01'), 'clickhouse', struct('port', 9000)));
check(strcmp(cfg.client.node, 'ccs-01') && cfg.clickhouse.port == 9000, 'overrides');
check(cfg.client.batch_rows == 1000 && strcmp(cfg.clickhouse.database, 'chlog'), 'defaults kept');
check(strcmp(cfg.matlab.insertFcn, 'http'), 'http is the default adapter');
f = [tempname '.json'];
fid = fopen(f, 'w'); fwrite(fid, uint8(['{"client":{"spool_max_mb":0.5},"matlab":{"rowFormat":"jsonlines"}}'])); fclose(fid);
cfg = io.chlog.ChLogger.loadConfig(f);
delete(f);
check(cfg.client.spool_max_mb == 0.5 && strcmp(cfg.matlab.rowFormat, 'jsonlines') && cfg.client.batch_rows == 1000, 'file merged');
threw = false;
try, io.chlog.ChLogger.loadConfig('/no/such/ChLogger.yaml'); catch, threw = true; end
check(threw, 'missing file is an error');
% the shared AstroPack config must load
shared = fullfile(fileparts(mfilename('fullpath')), '..', '..', '..', '..', '..', 'config', 'ChLogger.yaml');
if exist(shared, 'file')
    cfg = io.chlog.ChLogger.loadConfig(shared);
    check(strcmp(cfg.clickhouse.database, 'chlog') && cfg.client.batch_rows > 0, 'shared ChLogger.yaml');
end
end

function t_config_struct_init()
% AstroPack-style path: full config struct, no JSON file on disk.
spool = fresh_dir('structcfg');
cfgStruct = io.chlog.ChLogger.loadConfig('', struct('client', struct('spool_dir', spool, 'process', 'struct_test')));
L = io.chlog.ChLogger.open(cfgStruct, 'Process', 'struct_test', 'InsertFcn', @io.chlog.tests.test_adapter_mock);
L.log('INF', 'from struct config');
L.close();
check(exist(spool, 'dir') == 7, 'struct init created spool');
end

function t_node_empty_uses_host()
% Empty client.node in config resolves to envHelper('host') at construction time.
spool = fresh_dir('nodehost');
cfg = io.chlog.ChLogger.loadConfig('', struct('client', struct('node', '', 'spool_dir', spool, ...
    'batch_rows', 50, 'flush_interval_ms', 100000)));
want = io.chlog.ChLogger.envHelper('host');
L = io.chlog.ChLogger(cfg, 'Process', 'mtest', 'InsertFcn', @io.chlog.tests.test_adapter_mock, 'UseTimer', false);
check(strcmp(L.node, want), sprintf('empty node -> host (%s)', want));
L.log('INF', 'node check');
L.close();
lines = read_lines(fullfile(spool, 'mock_inserted.jsonl'));
check(numel(lines) >= 1 && strcmp(jsondecode(lines{1}).node, want), 'row node column');
end

function t_log_and_flush_to_mock()
spool = fresh_dir('basic');
c = new_client(spool);
for i = 1:120, c.log('INF', sprintf('row %d', i)); end
c.flush();
lines = read_lines(fullfile(spool, 'mock_inserted.jsonl'));
check(numel(lines) == 120, sprintf('120 rows inserted, got %d', numel(lines)));
seqs = cellfun(@(l) jsondecode(l).seq, lines);
check(isequal(seqs(:)', 0:119), 'seq 0..119 with no gap');
first = jsondecode(lines{1});
check(strcmp(first.ts, '2026-11-01 12:00:00.001') || strcmp(first.ts, '2026-11-01 12:00:00.000') || strncmp(first.ts, '2026-11-01 12:00:00', 19), 'ts from the log call');
tsAll = cellfun(@(l) jsondecode(l).ts, lines, 'UniformOutput', false);
check(issorted(tsAll), 'ts never goes backwards');
check(strcmp(first.node, 'test-node') && strcmp(first.process, 'mtest') && strcmp(first.env, 'unit'), 'identity columns');
check(isempty(first.tid), 'thread id is NULL in Matlab');
check(c.stats().spool_bytes == 0 && c.stats().sent_rows == 120, 'nothing spooled');
c.close();
end

function t_fields_and_levels()
spool = fresh_dir('fields');
c = new_client(spool);
c.log('warning', 'slow', 'PerfUs', 2500.4, 'PerfOp', 'cmd.execute', 'CorrId', 'cmd-77123', 'Subsystem', 'CMD', 'EventCode', 42, ...
      'Attrs', struct('channel', 3));
c.log('nonsense', 'x');
c.log('ERR', 'urgent');
c.flush();
lines = read_lines(fullfile(spool, 'mock_inserted.jsonl'));
a = jsondecode(lines{1});
check(strcmp(a.level, 'WRN') && a.perf_us == 2500 && strcmp(a.perf_op, 'cmd.execute') && strcmp(a.corr_id, 'cmd-77123'), 'fields');
check(a.event_code == 42 && strcmp(a.attrs.channel, '3'), 'attrs');
check(strcmp(jsondecode(lines{2}).level, 'INF'), 'unknown level becomes INF');
check(strcmp(jsondecode(lines{3}).level, 'ERR'), 'ERR kept');
check(~isempty(strfind(a.src, 'run_tests.m')) || ~isempty(strfind(a.src, '.m:')), 'src names the caller'); %#ok<STREMP>
c.close();
end

function t_outage_spools_then_replays_exactly_once()
spool = fresh_dir('outage');
c = new_client(spool);
fid = fopen(fullfile(spool, 'mock_fail.flag'), 'w'); fclose(fid);          % ClickHouse is down
N = 75;
for i = 1:N, c.log('INF', sprintf('row %d', i)); end
c.flush();
check(c.stats().degraded, 'degraded after a failed insert');
check(spool_rows(spool) == N, sprintf('%d rows on disk, got %d', N, spool_rows(spool)));
check(isempty(read_lines(fullfile(spool, 'mock_inserted.jsonl'))), 'nothing inserted during the outage');
delete(fullfile(spool, 'mock_fail.flag'));                                  % database is back
pause(0.01);
c.log('INF', 'after recovery');
c.flush();
c.replay();
lines = read_lines(fullfile(spool, 'mock_inserted.jsonl'));
check(numel(lines) == N + 1, sprintf('all rows delivered once, got %d', numel(lines)));
check(numel(unique(lines)) == numel(lines), 'no duplicates');
check(spool_rows(spool) == 0 && isempty(dir_listing(spool)), 'spool is empty again');
check(isempty(dir(fullfile(spool, '*.off'))), 'no offset files left');
ts = cellfun(@(l) jsondecode(l).ts, lines, 'UniformOutput', false);
check(strncmp(ts{1}, '2026-11-01 12:00:00', 19), 'replayed rows keep their original ts');
check(c.stats().replayed_rows == N && ~c.stats().degraded, 'stats');
c.close();
end

function t_lost_offset_never_duplicates()
spool = fresh_dir('offset');
c = new_client(spool);
fid = fopen(fullfile(spool, 'mock_fail.flag'), 'w'); fclose(fid);
for i = 1:25, c.log('INF', sprintf('row %d', i)); end
c.flush();
delete(fullfile(spool, 'mock_fail.flag'));
c.replay();                                                                  % everything delivered, 3 chunks
check(numel(read_lines(fullfile(spool, 'mock_inserted.jsonl'))) == 25, 'first pass');
% Simulate a crash between "server accepted chunk 1" and "offset saved": rebuild the segment and replay again.
seg = fullfile(spool, sprintf('test-node_mtest_%d_%s.1.jsonl', c.pid, c.runId));
lines = read_lines(fullfile(spool, 'mock_inserted.jsonl'));
fid = fopen(seg, 'wb'); fwrite(fid, uint8([strjoin(lines, char(10)) char(10)])); fclose(fid);
c.replay();                                                                  % same tokens: the mock must ignore them
check(numel(read_lines(fullfile(spool, 'mock_inserted.jsonl'))) == 25, 'duplicate chunks are ignored by token');
c.close();
end

function t_torn_last_line_ignored()
spool = fresh_dir('torn');
c = new_client(spool, 'Matlab', struct('rowFormat', 'jsonlines'));      % the file holds toy rows, so pass the text through
seg = fullfile(spool, sprintf('test-node_mtest_%d_%s.1.jsonl', c.pid, c.runId));
fid = fopen(seg, 'wb'); fwrite(fid, uint8(sprintf('{"a":1}\n{"a":2}\n{"a":3'))); fclose(fid);
c.replay();
check(numel(read_lines(fullfile(spool, 'mock_inserted.jsonl'))) == 2, 'two complete lines delivered, torn tail ignored');
c.close();
end

function t_poison_row_isolated()
global TEST_ACCEPTED
TEST_ACCEPTED = {};
spool = fresh_dir('poison');
% One row per batch so the poison adapter sees a single message per call (bisect is tested elsewhere).
c = new_client(spool, 'InsertFcn', @io.chlog.tests.test_adapter_poison, 'Overrides', struct('client', struct('batch_rows', 1)));
for i = 1:10
    if i == 4, c.log('INF', 'POISON'); else, c.log('INF', sprintf('ok %d', i)); end
end
c.flush();
check(numel(TEST_ACCEPTED) == 9, sprintf('9 good rows accepted, got %d', numel(TEST_ACCEPTED)));
check(c.stats().rejected == 1, 'one rejected');
rej = read_lines(fullfile(spool, 'rejected', sprintf('test-node_mtest_%d.jsonl', c.pid)));
check(numel(rej) == 1 && ~isempty(strfind(rej{1}, 'POISON')), 'bad row stored for a human'); %#ok<STREMP>
c.flush();                                                                   % sends the 9003 marker row
check(any(cellfun(@(m) ~isempty(strfind(m, 'rejected rows')), TEST_ACCEPTED)), 'marker row says rows were rejected'); %#ok<STREMP>
c.close();
end

function t_row_formats()
global TEST_SPY
for fmt = {'struct', 'jsonlines', 'table'}
    if strcmp(fmt{1}, 'table') && ~exist('struct2table', 'file'), continue; end
    spool = fresh_dir(['fmt_' fmt{1}]);
    c = new_client(spool, 'InsertFcn', @io.chlog.tests.test_adapter_spy, 'Matlab', struct('rowFormat', fmt{1}));
    c.log('INF', 'a'); c.log('INF', 'b', 'PerfUs', 7);
    c.flush();
    switch fmt{1}
        case 'struct'
            check(strcmp(TEST_SPY.class, 'struct') && TEST_SPY.n == 2, 'struct array of 2');
            r = TEST_SPY.rows;
            check(ischar(r(1).ts) && ischar(r(1).attrs) && isempty(r(1).tid) && isempty(r(1).perf_us) && r(2).perf_us == 7, 'field types');
            check(strcmp(r(1).attrs, '{}') && strcmp(r(1).message, 'a') && r(2).seq == 1, 'values');
        case 'jsonlines'
            check(strcmp(TEST_SPY.class, 'char'), 'one char vector');
            check(numel(strsplit(strtrim(TEST_SPY.rows), char(10))) == 2 && TEST_SPY.rows(end) == char(10), 'two lines, trailing newline');
        case 'table'
            check(strcmp(TEST_SPY.class, 'table') && TEST_SPY.n == 2, 'table');
    end
    check(isempty(TEST_SPY.token), 'live batch has no token');
    c.close();
end
end

function t_log_never_throws()
spool = fresh_dir('never');
c = new_client(spool);
c.log([], []);
c.log(42, 12345);
c.log('INF', {'a cell is not text'});
c.log('INF', 'x', 'Attrs', struct('nested', struct('a', 1)));
c.log('INF', 'x', 'Attrs', 'not a struct', 'PerfUs', 'oops');
c.log('INF', char(zeros(1, 5)));
c.log('INF', repmat('a', 1, 20000));
c.flush();
lines = read_lines(fullfile(spool, 'mock_inserted.jsonl'));
check(numel(lines) >= 3, 'rows with odd input were still logged');
check(any(cellfun(@(l) ~isempty(strfind(l, '...[truncated]')), lines)), 'long message truncated'); %#ok<STREMP>
for k = 1:numel(lines), jsondecode(lines{k}); end                             % every line is valid JSON
c.close();
end

function t_perf_helper()
spool = fresh_dir('perf');
L = io.chlog.ChLogger.open('', 'Process', 'perf_test', 'UseTimer', false, 'InsertFcn', @io.chlog.tests.test_adapter_mock, ...
    'Overrides', struct('client', struct('node', 'n1', 'spool_dir', spool)));
out = L.perf('t.op', @() pause_and_return(0.02), 'CorrId', 'cmd-1');
check(out == 42, 'function result passed through');
threw = false;
try, L.perf('t.fail', @() error('my:id', 'boom')); catch, threw = true; end
check(threw, 'error is rethrown');
L.flush();
lines = read_lines(fullfile(spool, 'mock_inserted.jsonl'));
a = jsondecode(lines{1}); b = jsondecode(lines{2});
check(strcmp(a.perf_op, 't.op') && a.perf_us >= 15000 && strcmp(a.corr_id, 'cmd-1'), 'timing row');
check(strcmp(b.level, 'ERR') && ~isempty(strfind(b.message, 'failed')), 'failure logged as ERR'); %#ok<STREMP>
s = L.stats();
check(s.sent_rows == 2, 'stats via front end');
L.close();
end

function v = pause_and_return(sec)
pause(sec); v = 42;
end

function t_front_end_before_init()
io.chlog.ChLogger.getSingleton([]);
io.chlog.ChLogger.logMessage('INF', 'nobody listening');
check(io.chlog.ChLogger.flushAll(), 'flush without a client is fine');
io.chlog.ChLogger.closeAll();
s = io.chlog.ChLogger.statsAll();
check(isstruct(s) && isempty(fieldnames(s)), 'empty stats');
end

function t_spool_full_drops_trc_dbg()
spool = fresh_dir('full');
c = new_client(spool, 'Overrides', []);
c = new_client_full(spool);
fid = fopen(fullfile(spool, 'mock_fail.flag'), 'w'); fclose(fid);
for i = 1:5, c.log('INF', sprintf('fill %d', i)); end
c.flush();
for i = 1:3, c.log('TRC', 'noise'); end
c.flush();
check(c.stats().spool_dropped == 3, sprintf('3 dropped, got %d', c.stats().spool_dropped));
delete(fullfile(spool, 'mock_fail.flag'));
c.replay(); c.flush(); c.flush();
lines = read_lines(fullfile(spool, 'mock_inserted.jsonl'));
check(any(cellfun(@(l) ~isempty(strfind(l, '"event_code":9001')), lines)), 'marker row 9001 says rows were dropped'); %#ok<STREMP>
check(sum(cellfun(@(l) ~isempty(strfind(l, 'fill')), lines)) == 5, 'the INF rows survived'); %#ok<STREMP>
c.close();
end

function c = new_client_full(spool)
ov = struct('client', struct('node', 'test-node', 'spool_dir', spool, 'batch_rows', 50, 'flush_interval_ms', 100000, ...
                             'replay_chunk_rows', 10, 'retry_min_s', 0.001, 'retry_max_s', 0.002, 'spool_max_mb', 0.0005));
c = io.chlog.ChLogger(io.chlog.ChLogger.loadConfig('', ov), 'Process', 'mtest', 'InsertFcn', @io.chlog.tests.test_adapter_mock, ...
    'ClockMs', fake_clock(1793534400000), 'UseTimer', false);
end

function t_file_names_match_python_and_cpp()
spool = fresh_dir('names');
c = new_client(spool);
fid = fopen(fullfile(spool, 'mock_fail.flag'), 'w'); fclose(fid);
c.log('INF', 'x'); c.flush();
d = dir(fullfile(spool, '*.jsonl'));
names = {d.name};
names = names(cellfun(@isempty, regexp(names, '^mock_', 'once')));
check(numel(names) == 1, 'one spool segment');
pat = '^test-node_mtest_\d+_[0-9a-f-]{36}\.1\.jsonl$';
check(~isempty(regexp(names{1}, pat, 'once')), ['segment name ' names{1} ' follows <node>_<process>_<pid>_<run>.<n>.jsonl']);
c.close();
end
