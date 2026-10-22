%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function c = crc32(data, method)
%MATLAB_TABLE_DUMP.INTERNAL.CRC32  CRC-32 of an array's bytes as a double (docs/FORMAT.md section 2.1).
%
%   c = matlab_table_dump.internal.crc32(data)            % fastest method available
%   c = matlab_table_dump.internal.crc32(data, method)    % 'mex', 'java' or 'm' (tests)
%
%   The zlib/PNG CRC-32, equal to Python zlib.crc32 and C++ matlab_table_dump::crc32.
%   'mex'  matlab_table_dump_mex (no copy of the data)
%   'java' java.util.zip.CRC32 (MATLAB with a JVM; copies the data once)
%   'm'    table-driven M code, one loop step per byte: small data only
if nargin < 2
    if exist('matlab_table_dump_mex', 'file') == 3
        method = 'mex';
    elseif exist('OCTAVE_VERSION', 'builtin') == 0 && usejava('jvm')
        method = 'java';
    else
        method = 'm';
    end
end
if strcmp(method, 'mex')
    c = matlab_table_dump_mex('crc32', data);
    return
end
if islogical(data), data = uint8(data); end
bytes = typecast(data(:), 'uint8');
if isempty(bytes)
    c = 0;
elseif strcmp(method, 'java')
    j = java.util.zip.CRC32();
    j.update(bytes);
    c = double(j.getValue());
else
    c = crc_m(bytes);
end
end

function c = crc_m(bytes)
persistent T
if isempty(T)
    T = uint32(0:255);
    for k = 1:8
        T = bitxor(bitshift(T, -1), uint32(3988292384) * bitand(T, uint32(1)));   % 0xEDB88320
    end
end
r = uint32(4294967295);
for i = 1:numel(bytes)
    r = bitxor(bitshift(r, -8), T(double(bitand(bitxor(r, uint32(bytes(i))), uint32(255))) + 1));
end
c = double(bitxor(r, uint32(4294967295)));
end
