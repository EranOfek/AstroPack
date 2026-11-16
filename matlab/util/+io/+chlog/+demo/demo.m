function demo(SpoolDir, NRows, Mode)
%DEMO  Offline chlog demo (mock insert adapter, no ClickHouse).
%   io.chlog.demo.demo(spoolDir, nRows, mode)
%   mode: normal | outage | down
if nargin < 1 || isempty(SpoolDir), SpoolDir = fullfile(tempdir, 'chlog_demo_matlab'); end
if nargin < 2 || isempty(NRows), NRows = 100; end
if nargin < 3 || isempty(Mode), Mode = 'normal'; end
if ~exist(SpoolDir, 'dir'), mkdir(SpoolDir); end
Flag = fullfile(SpoolDir, 'mock_fail.flag');
if exist(Flag, 'file'), delete(Flag); end
for F = {'mock_inserted.jsonl', 'mock_tokens.txt'}
    P = fullfile(SpoolDir, F{1});
    if exist(P, 'file'), delete(P); end
end
Ov = struct('client', struct('node', 'demo-node', 'spool_dir', SpoolDir, 'flush_interval_ms', 200, ...
    'retry_min_s', 0.2, 'retry_max_s', 0.5, 'replay_chunk_rows', 25));
L = io.chlog.ChLogger.open('', 'Process', 'demo_matlab', 'InsertFcn', @io.chlog.tests.test_adapter_mock, 'Overrides', Ov);
fprintf('node=%s process=%s pid=%d run_id=%s\nspool dir: %s\n', L.node, L.process, L.pid, L.runId, L.spoolDir);
if strcmp(Mode, 'down'), fclose(fopen(Flag, 'w')); end
for I = 1:NRows
    if strcmp(Mode, 'outage') && I == floor(NRows / 3)
        fprintf('>>> database goes down\n');
        fclose(fopen(Flag, 'w'));
    elseif strcmp(Mode, 'outage') && I == floor(2 * NRows / 3)
        fprintf('>>> database is back\n');
        delete(Flag);
    end
    L.perf('demo.step', @() pause(0.002), 'CorrId', sprintf('cmd-%d', I), 'Message', sprintf('step %d', I));
    if mod(I, 25) == 0
        L.log('WRN', sprintf('checkpoint %d', I), 'Subsystem', 'DEMO', 'Attrs', struct('i', I, 'ratio', I / NRows));
    end
    pause(0.004);
end
L.log('ERR', 'demo finished, this ERR row is sent at once', 'Subsystem', 'DEMO');
L.flush();
for K = 1:100
    S = L.stats();
    if ~S.degraded && S.spool_bytes == 0, break; end
    if strcmp(Mode, 'down'), break; end
    pause(0.1);
    L.flush();
end
S = L.stats();
Expected = NRows + floor(NRows / 25) + 1;
fprintf('expected=%d sent=%d spooled=%d replayed=%d spool_bytes=%d degraded=%d\n', Expected, S.sent_rows, ...
    S.spooled_rows, S.replayed_rows, S.spool_bytes, S.degraded);
L.close();
MockPath = fullfile(SpoolDir, 'mock_inserted.jsonl');
fprintf('rows the mock database received: %d\n', numel(strsplit(strtrim(fileread_or_empty(MockPath)), char(10))));
end


function T = fileread_or_empty(Path)
if exist(Path, 'file'), T = fileread(Path); else, T = ''; end
end
