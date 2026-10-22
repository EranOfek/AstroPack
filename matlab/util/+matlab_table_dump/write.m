%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function info = write(path, T, varargin)
%MATLAB_TABLE_DUMP.WRITE  Write a table (or struct of columns) to one matlab_table_dump file.
%
%   info = matlab_table_dump.write(path, T, 'BatchId', id)
%   info = matlab_table_dump.write(path, T, 'BatchId', id, Name, Value, ...)
%
%   T is a MATLAB table, or a scalar struct whose fields are the columns
%   (works in Octave too). Every column must have the same number of rows.
%
%   Column types (docs/FORMAT.md section 3):
%     double, single, int8..int64, uint8..uint64  -> written as-is (exact bits)
%     logical                                      -> bool (one byte per value)
%     N-by-k numeric/logical                       -> shape [N, k], column-major
%     string, cellstr                              -> utf8 (<missing> -> NULL)
%     categorical                                  -> codes + categories
%     datetime                                     -> timestamp_us (UTC; NaT -> NULL)
%     struct('codes',c,'categories',{...})         -> categorical (pre-encoded)
%     struct('timestamp_us',int64 us)              -> timestamp_us (pre-encoded)
%   Anything else (complex, sparse, cell, char matrix, nested struct/table,
%   duration, >2-D) raises matlab_table_dump:unsupportedType. Nothing is coerced.
%
%   Options:
%     'BatchId'    (required) ^[A-Za-z0-9._-]{1,200}$
%     'Engine'     'm' (default, fwrite) or 'mex' (matlab_table_dump_mex; see matlab_table_dump.build_mex)
%     'Validity'   N-by-2 cell {dataColumn, logicalColumn; ...}: logical column
%                  marks which rows of dataColumn are present (false = NULL)
%     'Attrs'      scalar struct of scalar strings/numbers/logicals
%     'Fsync'      false (default); true flushes to disk (Engine 'mex' only)
%     'Producer'   text stored in the metadata (default 'matlab-m'/'matlab-mex')
%     'CreatedUtc' ISO time text (default: now)
%     'Int64Mode'  'native' (default) or 'typecast' (fallback if matlab_table_dump.selftest
%                  shows that fwrite does not write 64-bit integers exactly)
%     'Checksum'   false (default); true stores a CRC-32 of every buffer in the
%                  metadata so readers can prove they got the bytes MATLAB had
%                  (one extra pass over the data; fast with matlab_table_dump_mex built)
%
%   info: struct with fields path, batch_id, n_rows, file_nbytes, meta_offset,
%   columns (names), engine, seconds.

t0 = tic;
o = matlab_table_dump.internal.options(varargin, struct( ...
    'BatchId', '', 'Engine', 'm', 'Validity', {{}}, 'Attrs', struct(), 'Fsync', false, ...
    'Producer', '', 'CreatedUtc', '', 'Int64Mode', 'native', 'Checksum', false));
batchId = matlab_table_dump.internal.check_id(o.BatchId, 'batch');
engine = lower(char(o.Engine));
if ~any(strcmp(engine, {'m', 'mex'}))
    error('matlab_table_dump:badOption', 'Engine must be ''m'' or ''mex''.');
end
if o.Fsync && strcmp(engine, 'm')
    error('matlab_table_dump:badOption', 'Fsync needs Engine ''mex'' (fwrite cannot flush to stable storage).');
end
if isempty(o.Producer)
    o.Producer = ['matlab-' engine];
end

[cols, nRows] = matlab_table_dump.internal.encode(T, o.Validity);
[cols, metaOffset] = matlab_table_dump.internal.layout(cols);
if o.Checksum
    for i = 1:numel(cols)
        cols(i).crc = cellfun(@matlab_table_dump.internal.crc32, cols(i).buffers);
    end
end
metaBytes = matlab_table_dump.internal.meta_json(batchId, nRows, cols, o.Attrs, o.Producer, o.CreatedUtc);
fileNbytes = metaOffset + numel(metaBytes) + 32;

path = char(path);
if strcmp(engine, 'mex')
    if exist('matlab_table_dump_mex', 'file') ~= 3
        error('matlab_table_dump:noMex', 'matlab_table_dump_mex is not built: run matlab_table_dump.build_mex.');
    end
    bufs = {};
    offs = [];
    for i = 1:numel(cols)
        for j = 1:numel(cols(i).buffers)
            bufs{end + 1} = cols(i).buffers{j}; %#ok<AGROW>
            offs(end + 1) = cols(i).offsets(j); %#ok<AGROW>
        end
    end
    matlab_table_dump_mex('write', path, bufs, offs, metaBytes, metaOffset, logical(o.Fsync));
else
    matlab_table_dump.internal.write_m(path, cols, metaBytes, metaOffset, fileNbytes, o.Int64Mode);
end

info = struct('path', path, 'batch_id', batchId, 'n_rows', nRows, 'file_nbytes', fileNbytes, ...
    'meta_offset', metaOffset, 'columns', {{cols.name}}, 'engine', engine, 'seconds', toc(t0));
end
