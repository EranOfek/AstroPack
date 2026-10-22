%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function write_m(path, cols, metaBytes, metaOffset, fileNbytes, int64Mode)
%COLBATCH.INTERNAL.WRITE_M  Pure-M writer: one vectorized fwrite per buffer (no per-row work).
%
%   fwrite writes each column straight from MATLAB's array in its own type,
%   so the only copy is the kernel copy into the page cache (or tmpfs).
%   Errors (disk full, permission) delete the partial file.

fid = fopen(path, 'w', 'ieee-le');
if fid < 0
    error('colbatch:io', 'Cannot create %s.', path);
end
ok = false;
try
    header = zeros(1, 64, 'uint8');
    header(1:8) = uint8('COLBATCH');
    header(9:12) = le_bytes(uint32(1));          % version; bytes 13..16 (flags) stay 0
    must_write(fid, header, 'uint8', 64, path);
    pos = 64;
    for i = 1:numel(cols)
        for j = 1:numel(cols(i).buffers)
            off = cols(i).offsets(j);
            if off > pos
                must_write(fid, zeros(1, off - pos, 'uint8'), 'uint8', off - pos, path);
            end
            data = cols(i).buffers{j};
            prec = cols(i).precisions{j};
            if strcmp(int64Mode, 'typecast') && any(strcmp(prec, {'int64', 'uint64'}))
                must_write(fid, typecast(data(:), 'uint8'), 'uint8', cols(i).nbytes(j), path);
            else
                must_write(fid, data, prec, numel(data), path);
            end
            pos = off + cols(i).nbytes(j);
        end
    end
    must_write(fid, metaBytes, 'uint8', numel(metaBytes), path);
    trailer = [le_bytes(uint64(metaOffset)), le_bytes(uint64(numel(metaBytes))), ...
        le_bytes(uint64(fileNbytes)), uint8('COLBEND1')];
    must_write(fid, trailer, 'uint8', 32, path);
    ok = true;
catch err
end
st = fclose(fid);
if ~ok || st ~= 0
    if exist(path, 'file'), delete(path); end
    if ~ok, rethrow(err); end
    error('colbatch:io', 'Closing %s failed (disk full?).', path);
end
end

function must_write(fid, data, precision, count, path)
if count == 0, return; end
n = fwrite(fid, data, precision);
if n ~= numel(data)
    error('colbatch:io', 'Short write to %s (disk full?).', path);
end
end

function b = le_bytes(v)
% Little-endian bytes of an integer scalar (independent of the host byte order).
b = typecast(v, 'uint8');
[~, ~, endian] = computer;
if endian == 'B'
    b = fliplr(b);
end
end
