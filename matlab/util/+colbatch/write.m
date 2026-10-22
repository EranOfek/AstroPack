%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function info = write(path, T, varargin)
%COLBATCH.WRITE  Write a table (or struct of columns) to one colbatch file.
%
%   info = colbatch.write(path, T, 'BatchId', id)
%   info = colbatch.write(path, T, 'BatchId', id, Name, Value, ...)
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
%   duration, >2-D) raises colbatch:unsupportedType. Nothing is coerced.
%
%   Options:
%     'BatchId'    (required) ^[A-Za-z0-9._-]{1,200}$
%     'Engine'     'm' (default, fwrite) or 'mex' (colbatch_mex; see colbatch.build_mex)
%     'Validity'   N-by-2 cell {dataColumn, logicalColumn; ...}: logical column
%                  marks which rows of dataColumn are present (false = NULL)
%     'Attrs'      scalar struct of scalar strings/numbers/logicals
%     'Fsync'      false (default); true flushes to disk (Engine 'mex' only)
%     'Producer'   text stored in the metadata (default 'matlab-m'/'matlab-mex')
%     'CreatedUtc' ISO time text (default: now)
%     'Int64Mode'  'native' (default) or 'typecast' (fallback if colbatch.selftest
%                  shows that fwrite does not write 64-bit integers exactly)
%     'Checksum'   false (default); true stores a CRC-32 of every buffer in the
%                  metadata so readers can prove they got the bytes MATLAB had
%                  (one extra pass over the data; fast with colbatch_mex built)
%
%   info: struct with fields path, batch_id, n_rows, file_nbytes, meta_offset,
%   columns (names), engine, seconds.

t0 = tic;
o = colbatch.internal.options(varargin, struct( ...
    'BatchId', '', 'Engine', 'm', 'Validity', {{}}, 'Attrs', struct(), 'Fsync', false, ...
    'Producer', '', 'CreatedUtc', '', 'Int64Mode', 'native', 'Checksum', false));
batchId = colbatch.internal.check_id(o.BatchId, 'batch');
engine = lower(char(o.Engine));
if ~any(strcmp(engine, {'m', 'mex'}))
    error('colbatch:badOption', 'Engine must be ''m'' or ''mex''.');
end
if o.Fsync && strcmp(engine, 'm')
    error('colbatch:badOption', 'Fsync needs Engine ''mex'' (fwrite cannot flush to stable storage).');
end
if isempty(o.Producer)
    o.Producer = ['matlab-' engine];
end

[cols, nRows] = colbatch.internal.encode(T, o.Validity);
[cols, metaOffset] = colbatch.internal.layout(cols);
if o.Checksum
    for i = 1:numel(cols)
        cols(i).crc = cellfun(@colbatch.internal.crc32, cols(i).buffers);
    end
end
metaBytes = colbatch.internal.meta_json(batchId, nRows, cols, o.Attrs, o.Producer, o.CreatedUtc);
fileNbytes = metaOffset + numel(metaBytes) + 32;

path = char(path);
if strcmp(engine, 'mex')
    if exist('colbatch_mex', 'file') ~= 3
        error('colbatch:noMex', 'colbatch_mex is not built: run colbatch.build_mex.');
    end
    bufs = {};
    offs = [];
    for i = 1:numel(cols)
        for j = 1:numel(cols(i).buffers)
            bufs{end + 1} = cols(i).buffers{j}; %#ok<AGROW>
            offs(end + 1) = cols(i).offsets(j); %#ok<AGROW>
        end
    end
    colbatch_mex('write', path, bufs, offs, metaBytes, metaOffset, logical(o.Fsync));
else
    colbatch.internal.write_m(path, cols, metaBytes, metaOffset, fileNbytes, o.Int64Mode);
end

info = struct('path', path, 'batch_id', batchId, 'n_rows', nRows, 'file_nbytes', fileNbytes, ...
    'meta_offset', metaOffset, 'columns', {{cols.name}}, 'engine', engine, 'seconds', toc(t0));
end
