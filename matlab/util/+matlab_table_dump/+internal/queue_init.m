%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function dirs = queue_init(root, producer)
%MATLAB_TABLE_DUMP.INTERNAL.QUEUE_INIT  Create the queue folders and run step P0 once per (root, producer).
%   P0 (docs/QUEUE.md): delete tmp/<producer>.* left behind by a crash of this producer.
persistent done
if isempty(done)
    done = {};
end
dirs = struct('tmp', fullfile(root, 'tmp'), 'ready', fullfile(root, 'ready'), ...
    'inflight', fullfile(root, 'inflight'), 'done', fullfile(root, 'done'), 'failed', fullfile(root, 'failed'));
key = [root '|' producer];
if any(strcmp(done, key))
    return
end
names = fieldnames(dirs);
for i = 1:numel(names)
    d = dirs.(names{i});
    if ~exist(d, 'dir')
        [ok, msg] = mkdir(d);
        if ~ok
            error('matlab_table_dump:io', 'Cannot create %s: %s', d, msg);
        end
    end
end
left = dir(fullfile(dirs.tmp, [producer '.*']));
for i = 1:numel(left)
    delete(fullfile(dirs.tmp, left(i).name));
end
done{end + 1} = key;
end
