%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function [cols, nRows] = encode(T, validityPairs)
%COLBATCH.INTERNAL.ENCODE  Turn table/struct columns into buffers for the writer (docs/FORMAT.md).
%
%   cols(i) fields:
%     name, dtype, shape, kind, categories, validity
%     buffers     cell of arrays written byte-for-byte (numeric data is NOT copied:
%                 MATLAB passes the table variable by reference)
%     precisions  fwrite precision of each buffer
%     nbytes      size of each buffer
%     keys        metadata keys {offsetKey, nbytesKey} of each buffer
%     offsets     filled in by colbatch.internal.layout
%     crc         CRC-32 of each buffer; filled in by colbatch.write when 'Checksum' is on
%
%   Refuses anything it cannot write exactly (GR: never coerce silently).

if isa(T, 'table')
    names = T.Properties.VariableNames;
    getcol = @(k) T.(names{k});
    nRows = height(T);
elseif isstruct(T) && isscalar(T)
    names = fieldnames(T)';
    getcol = @(k) T.(names{k});
    nRows = [];
else
    error('colbatch:unsupportedInput', 'Input must be a table or a scalar struct of columns (got %s).', class(T));
end

isOctave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
template = struct('name', '', 'dtype', '', 'shape', [], 'kind', '', 'categories', {{}}, 'validity', '', ...
    'buffers', {{}}, 'precisions', {{}}, 'nbytes', [], 'keys', {{}}, 'offsets', [], 'crc', []);
cols = template([]);
allNames = names;

for k = 1:numel(names)
    name = names{k};
    x = getcol(k);
    rows = col_rows(x);
    if isempty(nRows)
        nRows = rows;
    elseif rows ~= nRows
        error('colbatch:rowMismatch', 'Column ''%s'' has %d rows, expected %d.', name, rows, nRows);
    end
    c = template;
    c.name = name;
    autoValid = [];

    if isnumeric(x) || islogical(x)
        check_plain(x, name);
        c.dtype = dtype_of(x);
        c.shape = shape_of(x);
        c = add_buffer(c, x, precision_of(x), {'offset', 'nbytes'});

    elseif isstruct(x) && isscalar(x) && isfield(x, 'codes')            % pre-encoded categorical
        codes = x.codes;
        if ~any(strcmp(class(codes), {'int8', 'int16', 'int32'})) || size(codes, 2) ~= 1
            error('colbatch:unsupportedType', 'Column ''%s'': categorical codes must be an int8/int16/int32 column.', name);
        end
        c = categorical_column(c, codes, x.categories, name);

    elseif isstruct(x) && isscalar(x) && isfield(x, 'timestamp_us')     % pre-encoded timestamps
        us = x.timestamp_us;
        if ~isa(us, 'int64') || size(us, 2) ~= 1
            error('colbatch:unsupportedType', 'Column ''%s'': timestamp_us must be an int64 column.', name);
        end
        c.dtype = 'int64'; c.shape = numel(us); c.kind = 'timestamp_us';
        c = add_buffer(c, us, 'int64', {'offset', 'nbytes'});

    elseif isa(x, 'string') || iscellstr(x)                              % utf8
        if size(x, 2) ~= 1 && ~(rows == 0)
            error('colbatch:unsupportedType', 'Column ''%s'': string columns must be N-by-1.', name);
        end
        missingRows = false(rows, 1);
        if isa(x, 'string')
            missingRows = ismissing(x(:));
            x(missingRows) = "";
            x = cellstr(x);
        end
        [offsets, bytes] = utf8_encode(x(:), isOctave, name);
        c.dtype = 'utf8'; c.shape = rows;
        c = add_buffer(c, offsets, 'int64', {'offsets_offset', 'offsets_nbytes'});
        c = add_buffer(c, bytes, 'uint8', {'offset', 'nbytes'});
        if any(missingRows), autoValid = ~missingRows; end

    elseif isa(x, 'categorical')
        if size(x, 2) ~= 1
            error('colbatch:unsupportedType', 'Column ''%s'': categorical columns must be N-by-1.', name);
        end
        cats = categories(x);
        codes = double(x) - 1;                      % 0-based
        codes(isundefined(x)) = -1;
        if numel(cats) <= 127
            codes = int8(codes);
        elseif numel(cats) <= 32767
            codes = int16(codes);
        else
            codes = int32(codes);
        end
        c = categorical_column(c, codes, cats, name);

    elseif isa(x, 'datetime')
        if size(x, 2) ~= 1
            error('colbatch:unsupportedType', 'Column ''%s'': datetime columns must be N-by-1.', name);
        end
        nat = isnat(x);
        us = int64(round(posixtime(x) * 1e6));      % unzoned datetimes are taken as UTC
        us(nat) = 0;
        c.dtype = 'int64'; c.shape = rows; c.kind = 'timestamp_us';
        c = add_buffer(c, us, 'int64', {'offset', 'nbytes'});
        if any(nat), autoValid = ~nat; end

    else
        error('colbatch:unsupportedType', ...
            'Column ''%s'' has unsupported type %s (see docs/FORMAT.md section 5).', name, class(x));
    end

    cols(end + 1) = c; %#ok<AGROW>
    if ~isempty(autoValid) && ~has_pair(validityPairs, name)
        vname = [name '__valid'];
        if any(strcmp(allNames, vname))
            error('colbatch:nameClash', 'Column ''%s'' needs validity column ''%s'', which already exists.', name, vname);
        end
        allNames{end + 1} = vname; %#ok<AGROW>
        v = template;
        v.name = vname; v.dtype = 'bool'; v.shape = rows;
        v = add_buffer(v, autoValid(:), 'uint8', {'offset', 'nbytes'});
        cols(end + 1) = v; %#ok<AGROW>
        cols(end - 1).validity = vname;
    end
end
if isempty(nRows), nRows = 0; end

% Explicit validity pairs {data, logical; ...}
if ~isempty(validityPairs)
    if ~iscell(validityPairs) || size(validityPairs, 2) ~= 2
        error('colbatch:badOption', 'Validity must be an N-by-2 cell {dataColumn, logicalColumn; ...}.');
    end
    allColNames = {cols.name};
    for p = 1:size(validityPairs, 1)
        d = char(validityPairs{p, 1});
        v = char(validityPairs{p, 2});
        di = find(strcmp(allColNames, d), 1);
        vi = find(strcmp(allColNames, v), 1);
        if isempty(di)
            error('colbatch:badValidity', 'Validity given for unknown column ''%s''.', d);
        end
        if isempty(vi) || ~strcmp(cols(vi).dtype, 'bool') || numel(cols(vi).shape) ~= 1 || vi == di
            error('colbatch:badValidity', 'Validity column ''%s'' must be another N-by-1 logical column.', v);
        end
        cols(di).validity = v;
    end
end
end

% ---------------------------------------------------------------------------
function r = col_rows(x)
if isstruct(x) && isscalar(x) && isfield(x, 'codes')
    r = size(x.codes, 1);
elseif isstruct(x) && isscalar(x) && isfield(x, 'timestamp_us')
    r = size(x.timestamp_us, 1);
else
    r = size(x, 1);
end
end

function tf = has_pair(pairs, name)
tf = ~isempty(pairs) && iscell(pairs) && size(pairs, 2) == 2 && any(strcmp(cellfun(@char, pairs(:, 1), 'UniformOutput', false), name));
end

function check_plain(x, name)
if issparse(x)
    error('colbatch:unsupportedType', 'Column ''%s'' is sparse (unsupported).', name);
end
if ~isreal(x)
    error('colbatch:unsupportedType', 'Column ''%s'' is complex (unsupported).', name);
end
if ndims(x) > 2
    error('colbatch:unsupportedType', 'Column ''%s'' has %d dimensions (max 2).', name, ndims(x));
end
end

function d = dtype_of(x)
if islogical(x)
    d = 'bool';
elseif isa(x, 'double')
    d = 'float64';
elseif isa(x, 'single')
    d = 'float32';
else
    d = class(x);       % int8..int64, uint8..uint64 have the same names in the format
end
end

function p = precision_of(x)
if islogical(x)
    p = 'uint8';
else
    p = class(x);
end
end

function s = shape_of(x)
if size(x, 2) == 1
    s = size(x, 1);
else
    s = [size(x, 1), size(x, 2)];
end
end

function c = add_buffer(c, data, precision, keys)
sizes = struct('double', 8, 'single', 4, 'int8', 1, 'uint8', 1, 'int16', 2, 'uint16', 2, ...
    'int32', 4, 'uint32', 4, 'int64', 8, 'uint64', 8, 'logical', 1);
c.buffers{end + 1} = data;
c.precisions{end + 1} = precision;
c.nbytes(end + 1) = numel(data) * sizes.(class(data));
c.keys{end + 1} = keys;
end

function c = categorical_column(c, codes, cats, name)
if isa(cats, 'string'), cats = cellstr(cats); end
if ~iscellstr(cats)
    error('colbatch:unsupportedType', 'Column ''%s'': categories must be text.', name);
end
k = numel(cats);
if ~isempty(codes) && (min(codes) < -1 || max(codes) >= k)
    error('colbatch:badCodes', 'Column ''%s'': categorical code out of range.', name);
end
c.dtype = class(codes); c.shape = numel(codes); c.kind = 'categorical'; c.categories = cats(:)';
c = add_buffer(c, codes, class(codes), {'offset', 'nbytes'});
end

function [offsets, bytes] = utf8_encode(cs, isOctave, name)
% Vectorized UTF-8 encoding of a cellstr column with int64 offsets (docs/FORMAT.md 3.1).
n = numel(cs);
if n == 0
    offsets = int64(0); bytes = zeros(0, 1, 'uint8');
    return
end
lens = cellfun('length', cs);
cs = cs(:)';
allc = [cs{:}];
if isempty(allc)
    offsets = zeros(n + 1, 1, 'int64'); bytes = zeros(0, 1, 'uint8');
    return
end
if isOctave
    % Octave char arrays already hold UTF-8 bytes.
    bytes = uint8(allc(:));
    offsets = int64([0; cumsum(lens(:))]);
else
    % MATLAB char is UTF-16: bytes per code unit = 1/2/3, surrogates 2 each (4 per pair).
    bytes = unicode2native(allc, 'UTF-8');
    bytes = bytes(:);
    u = double(allc);
    per = 1 + (u >= 128) + (u >= 2048) - (u >= 55296 & u <= 57343);
    cb = [0, cumsum(per)];
    ends = cumsum(lens(:))';
    offsets = int64([0, cb(ends + 1)])';
end
if offsets(end) ~= numel(bytes)
    error('colbatch:badText', 'Column ''%s'' contains text that is not valid Unicode.', name);
end
end
