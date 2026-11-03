function demo_real(CfgFile, Process, NRows, SpoolDir)
%DEMO_REAL  io.chlog client against a real ClickHouse (HTTP :8123).
%   io.chlog.demo.demo_real(cfgFile, process, nRows, spoolDir)
%   Prints RESULT {...json...} for cross-repo verify scripts.
if nargin < 1 || isempty(CfgFile), CfgFile = io.chlog.ChLogger.defaultConfigPath(); end
if nargin < 2 || isempty(Process), Process = 'demo_matlab_real'; end
if nargin < 3 || isempty(NRows), NRows = 100; end
if nargin < 4 || isempty(SpoolDir), SpoolDir = fullfile(tempdir, ['chlog_' Process]); end
if ~exist(SpoolDir, 'dir'), mkdir(SpoolDir); end
Ov = struct('client', struct('spool_dir', SpoolDir, 'flush_interval_ms', 200, ...
    'retry_min_s', 0.2, 'retry_max_s', 0.5, 'replay_chunk_rows', 25), ...
    'matlab', struct('insertFcn', 'http', 'rowFormat', 'jsonlines'));
L = io.chlog.ChLogger.open(CfgFile, 'Process', Process, 'Overrides', Ov);
fprintf('node=%s process=%s pid=%d run_id=%s adapter=http\nspool dir: %s\n', ...
    L.node, L.process, L.pid, L.runId, L.spoolDir);
for I = 1:NRows
    L.perf('demo.step', @() pause(0.002), 'CorrId', sprintf('cmd-%d', I), 'Message', sprintf('step %d', I));
    if mod(I, 25) == 0
        L.log('WRN', sprintf('checkpoint %d', I), 'Subsystem', 'DEMO', 'Attrs', struct('i', I, 'ratio', I / NRows));
    end
    pause(0.004);
end
L.log('ERR', 'demo finished, this ERR row is sent at once', 'Subsystem', 'DEMO');
L.flush();
S = L.stats();
fprintf('RESULT {"run_id":"%s","process":"%s","degraded":%d,"spool_bytes":%d,"adapter":"http"}\n', ...
    L.runId, Process, S.degraded, S.spool_bytes);
L.close();
end
