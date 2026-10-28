%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function move_file(src, dst)
%MATLAB_TABLE_DUMP.INTERNAL.MOVE_FILE  Atomic rename within one filesystem (queue commit, step P4).
%   Uses matlab_table_dump_mex (rename(2) / MoveFileExW) when built, Octave's rename
%   built-in in Octave, and movefile otherwise. movefile renames within a
%   filesystem but is slower and may fall back to copy+delete across
%   filesystems -- keep the whole queue on one filesystem (it is, under one root).
if exist('matlab_table_dump_mex', 'file') == 3
    matlab_table_dump_mex('rename', src, dst);
elseif exist('OCTAVE_VERSION', 'builtin') ~= 0
    [err, msg] = rename(src, dst); %#ok<RENAM>
    if err ~= 0
        error('matlab_table_dump:io', 'rename %s -> %s failed: %s', src, dst, msg);
    end
else
    [ok, msg] = movefile(src, dst);
    if ~ok
        error('matlab_table_dump:io', 'rename %s -> %s failed: %s', src, dst, msg);
    end
end
end
