%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function v = xor64(bytes)
%COLBATCH.INTERNAL.XOR64  Fingerprint shared with Python/C++: XOR of little-endian uint64 words.
%   bytes: uint8 vector. The last partial word is zero-padded. Returns uint64.
bytes = uint8(bytes(:)');
pad = mod(-numel(bytes), 8);
w = typecast([bytes, zeros(1, pad, 'uint8')], 'uint64');
[~, ~, endian] = computer;
if endian == 'B'
    w = swapbytes(w);
end
if isempty(w)
    v = uint64(0);
    return
end
while numel(w) > 1                         % pairwise tree reduction (vectorized)
    if mod(numel(w), 2) == 1
        w(end + 1) = 0; %#ok<AGROW>
    end
    w = bitxor(w(1:2:end), w(2:2:end));
end
v = w;
end
