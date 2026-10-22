%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function [cols, metaOffset] = layout(cols)
%MATLAB_TABLE_DUMP.INTERNAL.LAYOUT  Assign 64-byte aligned offsets to every buffer (docs/FORMAT.md L3, L6).
%   Buffers are placed in column order starting at byte 64; the metadata
%   starts right after the last buffer.
pos = 64;
for i = 1:numel(cols)
    cols(i).offsets = zeros(1, numel(cols(i).buffers));
    for j = 1:numel(cols(i).buffers)
        pos = ceil(pos / 64) * 64;
        cols(i).offsets(j) = pos;
        pos = pos + cols(i).nbytes(j);
    end
end
metaOffset = pos;
end
