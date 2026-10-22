%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function n = depth(root)
%COLBATCH.DEPTH  Number of batches waiting or in flight: ready/*.colb + inflight/*/*.colb.
root = char(root);
n = numel(dir(fullfile(root, 'ready', '*.colb')));
d = dir(fullfile(root, 'inflight'));
for i = 1:numel(d)
    if d(i).isdir && d(i).name(1) ~= '.'
        n = n + numel(dir(fullfile(root, 'inflight', d(i).name, '*.colb')));
    end
end
end
