%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function [data, meta, fingerprints] = read(path, varargin)
%MATLAB_TABLE_DUMP.READ  Read a matlab_table_dump file back into MATLAB (tests, debugging, MATLAB consumers).
%
%   [data, meta] = matlab_table_dump.read(path)
%   [data, meta, fp] = matlab_table_dump.read(path, 'Output', 'struct')
%
%   data:  table (MATLAB default) or scalar struct of columns ('Output','struct';
%          always struct in Octave; field names made valid with matlab.lang.makeValidName).
%          utf8 -> string (MATLAB) / cellstr (Octave); categorical -> categorical
%          (MATLAB) / int codes (Octave); timestamp_us -> datetime UTC (MATLAB) /
%          int64 (Octave); NULLs -> <missing>/NaT; numeric NULLs stay values with
%          their validity column alongside.
%   meta:  decoded metadata (jsondecode), columns as a cell array of structs.
%   fp:    struct array name/fingerprint (xor64 hex, same as Python/C++ tools).
%
%   Reads the whole file with fread: this is a convenience reader, not the fast path.

o = matlab_table_dump.internal.options(varargin, struct('Output', ''));
isOctave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
if isempty(o.Output)
    if isOctave, o.Output = 'struct'; else, o.Output = 'table'; end
end
fid = fopen(char(path), 'r', 'ieee-le');
if fid < 0
    error('matlab_table_dump:io', 'Cannot open %s.', char(path));
end
raw = fread(fid, Inf, '*uint8')';
fclose(fid);
size_ = numel(raw);
if size_ < 96
    error('matlab_table_dump:format', '%s: too small for a matlab_table_dump file (truncated?).', char(path));
end
if ~isequal(char(raw(1:8)), 'MTABDUMP')
    error('matlab_table_dump:format', '%s: bad magic (not a matlab_table_dump file).', char(path));
end
if u32(raw(9:12)) ~= 1 || u32(raw(13:16)) ~= 0
    error('matlab_table_dump:format', '%s: unsupported version or header flags.', char(path));
end
tr = raw(end - 31:end);
if ~isequal(char(tr(25:32)), 'MTABEND1')
    error('matlab_table_dump:format', '%s: missing end marker (truncated or still being written).', char(path));
end
mo = u64(tr(1:8)); mn = u64(tr(9:16)); fn = u64(tr(17:24));
if fn ~= size_ || mo < 64 || mo + mn + 32 ~= size_
    error('matlab_table_dump:format', '%s: inconsistent trailer (truncated?).', char(path));
end
mbytes = raw(mo + 1:mo + mn);
if isOctave
    text = char(mbytes);
else
    text = native2unicode(mbytes, 'UTF-8');
end
meta = jsondecode(text);
if ~isfield(meta, 'format') || ~strcmp(meta.format, 'matlab_table_dump') || meta.version ~= 1
    error('matlab_table_dump:format', '%s: metadata format/version mismatch.', char(path));
end
if isempty(meta.columns)
    meta.columns = {};
elseif isstruct(meta.columns)
    meta.columns = num2cell(meta.columns);
end
n = double(meta.n_rows);

data = struct();
names = {};
fingerprints = struct('name', {}, 'fingerprint', {});
for i = 1:numel(meta.columns)
    c = meta.columns{i};
    shape = double(c.shape(:)');
    k = 1;
    if numel(shape) == 2, k = shape(2); end
    vb = raw(c.offset + 1:c.offset + c.nbytes);
    fp = matlab_table_dump.internal.xor64(vb);
    check_crc(c, 'crc32', vb, path);
    switch c.dtype
        case 'utf8'
            ob = raw(c.offsets_offset + 1:c.offsets_offset + c.offsets_nbytes);
            check_crc(c, 'offsets_crc32', ob, path);
            fp = bitxor(fp, matlab_table_dump.internal.xor64(ob));
            offs = double(typecast(ob, 'int64'));
            vals = cell(n, 1);
            for r = 1:n
                b = vb(offs(r) + 1:offs(r + 1));
                if isOctave, vals{r} = char(b); else, vals{r} = native2unicode(b, 'UTF-8'); end
            end
            if ~isOctave, vals = string(vals); end
        case 'bool'
            vals = reshape(logical(vb), n, k);
        otherwise
            cls = c.dtype;
            if strcmp(cls, 'float64'), cls = 'double'; elseif strcmp(cls, 'float32'), cls = 'single'; end
            vals = reshape(typecast(vb, cls), n, k);
    end
    if isfield(c, 'kind') && ~isempty(c.kind) && ~isOctave
        if strcmp(c.kind, 'categorical')
            cats = c.categories;
            if ischar(cats), cats = {cats}; end
            vals = categorical(double(vals), 0:numel(cats) - 1, cats(:)');
        elseif strcmp(c.kind, 'timestamp_us')
            vals = datetime(double(vals) / 1e6, 'ConvertFrom', 'posixtime', 'TimeZone', 'UTC');
        end
    end
    fingerprints(end + 1) = struct('name', c.name, 'fingerprint', ['0x' dec2hex_u64(fp)]); %#ok<AGROW>
    names{end + 1} = c.name; %#ok<AGROW>
    data.(valid_name(c.name)) = vals;
end
% NULLs from validity columns for strings and timestamps (MATLAB types that can hold them)
if ~isOctave
    for i = 1:numel(meta.columns)
        c = meta.columns{i};
        if isfield(c, 'validity') && ~isempty(c.validity)
            v = data.(valid_name(c.validity));
            x = data.(valid_name(c.name));
            if isa(x, 'string'), x(~v) = missing; end
            if isa(x, 'datetime'), x(~v) = NaT; end
            data.(valid_name(c.name)) = x;
        end
    end
end
if strcmp(o.Output, 'table')
    fields = cellfun(@valid_name, names, 'UniformOutput', false);
    vals = cellfun(@(f) data.(f), fields, 'UniformOutput', false);
    data = table(vals{:}, 'VariableNames', names);
end
end

function check_crc(c, key, bytes, path)
% Optional per-buffer CRC-32 (docs/FORMAT.md section 2.1): checked whenever the writer stored one.
if isfield(c, key) && ~isempty(c.(key))
    got = matlab_table_dump.internal.crc32(bytes);
    if got ~= c.(key)
        error('matlab_table_dump:checksum', '%s: column ''%s'' %s is %08x, the writer recorded %08x (data corrupted).', ...
            char(path), c.name, key, got, c.(key));
    end
end
end

function v = u32(b)
v = double(typecast(uint8(b), 'uint32'));
end

function v = u64(b)
v = double(typecast(uint8(b), 'uint64'));
end

function s = valid_name(name)
s = matlab.lang.makeValidName(name);
end

function h = dec2hex_u64(v)
% Hex digits of a uint64 without going through double.
hi = uint32(bitshift(v, -32));
lo = uint32(bitand(v, uint64(4294967295)));
h = [sprintf('%08x', hi), sprintf('%08x', lo)];
end
