%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function n = depth(root)
%MATLAB_TABLE_DUMP.DEPTH  Number of batches waiting or in flight: ready/*.mtd + inflight/*/*.mtd.
root = char(root);
n = numel(dir(fullfile(root, 'ready', '*.mtd')));
d = dir(fullfile(root, 'inflight'));
for i = 1:numel(d)
    if d(i).isdir && d(i).name(1) ~= '.'
        n = n + numel(dir(fullfile(root, 'inflight', d(i).name, '*.mtd')));
    end
end
end
