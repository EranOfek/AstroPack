%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function tf = batch_exists(root, batchId)
%MATLAB_TABLE_DUMP.INTERNAL.BATCH_EXISTS  True if batchId is in ready/ or in any inflight/<consumer>/ (step P3).
name = [batchId '.mtd'];
tf = exist(fullfile(root, 'ready', name), 'file') == 2;
if tf, return; end
d = dir(fullfile(root, 'inflight'));
for i = 1:numel(d)
    if d(i).isdir && d(i).name(1) ~= '.' && exist(fullfile(root, 'inflight', d(i).name, name), 'file') == 2
        tf = true;
        return
    end
end
end
