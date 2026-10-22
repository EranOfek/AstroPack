%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function [readyPath, info] = publish(root, T, batchId, varargin)
%COLBATCH.PUBLISH  Write a batch into a colbatch queue folder (docs/QUEUE.md P0-P4).
%
%   p = colbatch.publish(root, T, batchId)
%   [p, info] = colbatch.publish(root, T, batchId, Name, Value, ...)
%
%   1. waits while the queue holds MaxDepth batches (ready + in flight)  - backpressure
%   2. writes  <root>/tmp/<ProducerId>.<batchId>.colb                   - invisible to readers
%   3. renames it to <root>/ready/<batchId>.colb                        - atomic commit
%
%   Options (plus every colbatch.write option except BatchId):
%     'ProducerId'  'matlab' (default); ^[A-Za-z0-9_-]{1,64}$, unique per running producer
%     'MaxDepth'    4 (default)
%     'Timeout'     Inf (default) seconds to wait for space; then error colbatch:queueFull
%     'Durable'     false (default); true = fsync file and folder (needs Engine 'mex')
%
%   The first publish of each (root, ProducerId) in a MATLAB session creates the
%   folders and deletes this producer's leftovers in tmp/ (from a crash).
%   info: colbatch.write info plus wait_seconds, write_seconds, commit_seconds.

[o, writeArgs] = split_options(varargin);
producer = colbatch.internal.check_id(o.ProducerId, 'producer');
batchId = colbatch.internal.check_id(batchId, 'batch');
root = char(root);
dirs = colbatch.internal.queue_init(root, producer);

waited = colbatch.wait_for_space(root, o.MaxDepth, o.Timeout);

tmpPath = fullfile(dirs.tmp, [producer '.' batchId '.colb']);
readyPath = fullfile(dirs.ready, [batchId '.colb']);
if o.Durable
    writeArgs = [writeArgs, {'Fsync', true}];
end
t1 = tic;
try
    info = colbatch.write(tmpPath, T, 'BatchId', batchId, writeArgs{:});
catch err
    if exist(tmpPath, 'file'), delete(tmpPath); end
    rethrow(err);
end
tWrite = toc(t1);

t2 = tic;
if colbatch.internal.batch_exists(root, batchId)
    delete(tmpPath);
    error('colbatch:duplicate', 'Batch %s is already in the queue (batch ids must be unique).', batchId);
end
colbatch.internal.move_file(tmpPath, readyPath);
if o.Durable
    colbatch_mex('fsyncdir', dirs.ready);
end
info.wait_seconds = waited;
info.write_seconds = tWrite;
info.commit_seconds = toc(t2);
info.path = readyPath;
end

function [o, rest] = split_options(args)
o = struct('ProducerId', 'matlab', 'MaxDepth', 4, 'Timeout', Inf, 'Durable', false);
rest = {};
names = fieldnames(o);
if mod(numel(args), 2) ~= 0
    error('colbatch:badOption', 'Options must be Name,Value pairs.');
end
for i = 1:2:numel(args)
    hit = find(strcmpi(names, char(args{i})), 1);
    if isempty(hit)
        rest(end + 1:end + 2) = args(i:i + 1); %#ok<AGROW>
    else
        o.(names{hit}) = args{i + 1};
    end
end
if o.Durable && ~any(strcmpi(rest(1:2:end), 'Engine'))
    rest = [rest, {'Engine', 'mex'}];
end
end
