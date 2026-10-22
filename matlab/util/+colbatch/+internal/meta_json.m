%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
function bytes = meta_json(batchId, nRows, cols, attrs, producer, createdUtc)
%COLBATCH.INTERNAL.META_JSON  Build the metadata JSON (docs/FORMAT.md section 2) as UTF-8 bytes.
%   Built by hand rather than with jsonencode so the output is identical in
%   every MATLAB/Octave version and integers are always written as integers.
if isempty(createdUtc)
    createdUtc = utc_now();
end
parts = {'{"format":"colbatch","version":1,"batch_id":', jstr(batchId), ...
    ',"n_rows":', jint(nRows), ',"created_utc":', jstr(createdUtc), ',"producer":', jstr(producer)};
if isstruct(attrs) && ~isempty(fieldnames(attrs))
    parts{end + 1} = ',"attrs":';
    parts{end + 1} = attrs_json(attrs);
end
parts{end + 1} = ',"columns":[';
for i = 1:numel(cols)
    c = cols(i);
    if i > 1, parts{end + 1} = ','; end %#ok<AGROW>
    shape = jint(c.shape(1));
    if numel(c.shape) == 2
        shape = [shape ',' jint(c.shape(2))]; %#ok<AGROW>
    end
    s = ['{"name":' jstr(c.name) ',"dtype":"' c.dtype '","shape":[' shape ']'];
    for j = 1:numel(c.buffers)
        s = [s ',"' c.keys{j}{1} '":' jint(c.offsets(j)) ',"' c.keys{j}{2} '":' jint(c.nbytes(j))]; %#ok<AGROW>
        if ~isempty(c.crc)                       % 'offset' -> 'crc32', 'offsets_offset' -> 'offsets_crc32'
            s = [s ',"' c.keys{j}{1}(1:end - 6) 'crc32":' jint(c.crc(j))]; %#ok<AGROW>
        end
    end
    if ~isempty(c.kind)
        s = [s ',"kind":"' c.kind '"']; %#ok<AGROW>
    end
    if strcmp(c.kind, 'categorical')
        cats = cellfun(@jstr, c.categories, 'UniformOutput', false);
        s = [s ',"categories":[' strjoin(cats, ',') ']']; %#ok<AGROW>
    end
    if ~isempty(c.validity)
        s = [s ',"validity":' jstr(c.validity)]; %#ok<AGROW>
    end
    parts{end + 1} = [s '}']; %#ok<AGROW>
end
parts{end + 1} = ']}';
text = [parts{:}];
if exist('OCTAVE_VERSION', 'builtin') ~= 0
    bytes = uint8(text);                    % Octave strings are UTF-8 already
else
    bytes = unicode2native(text, 'UTF-8');
end
bytes = bytes(:)';
end

function s = jint(v)
if ~(v >= 0 && v == floor(v) && v < 2^53)
    error('colbatch:internal', 'JSON integer out of range: %g', double(v));
end
s = sprintf('%d', v);
end

function s = jstr(t)
% JSON string literal: escape backslash, quote and control characters.
t = char(t);
if ~any(t == '"' | t == '\' | t < 32)          % fast path: nothing to escape (the usual case)
    s = ['"' t '"'];
    return
end
out = cell(1, numel(t));
for i = 1:numel(t)
    ch = t(i);
    code = double(ch);
    if ch == '"'
        out{i} = '\"';
    elseif ch == '\'
        out{i} = '\\';
    elseif code < 32
        switch code
            case 10, out{i} = '\n';
            case 13, out{i} = '\r';
            case 9, out{i} = '\t';
            otherwise, out{i} = sprintf('\\u%04x', code);
        end
    else
        out{i} = ch;
    end
end
s = ['"' [out{:}] '"'];
end

function s = attrs_json(a)
names = fieldnames(a);
items = cell(1, numel(names));
for i = 1:numel(names)
    v = a.(names{i});
    if isa(v, 'string') && isscalar(v), v = char(v); end
    if ischar(v) && size(v, 1) <= 1
        val = jstr(v);
    elseif islogical(v) && isscalar(v)
        if v, val = 'true'; else, val = 'false'; end
    elseif isnumeric(v) && isscalar(v) && isreal(v)
        if isinteger(v)
            val = sprintf('%d', v);
        elseif ~isfinite(v)
            error('colbatch:badAttr', 'Attribute ''%s'' is not finite (JSON has no NaN/Inf).', names{i});
        elseif v == floor(v) && abs(v) < 2^53
            val = sprintf('%d', v);
        else
            val = sprintf('%.17g', v);
        end
    else
        error('colbatch:badAttr', 'Attribute ''%s'' must be a scalar text, number or logical.', names{i});
    end
    items{i} = [jstr(names{i}) ':' val];
end
s = ['{' strjoin(items, ',') '}'];
end

function t = utc_now()
if exist('OCTAVE_VERSION', 'builtin') ~= 0
    g = gmtime(time());
    t = sprintf('%04d-%02d-%02dT%02d:%02d:%02d.%03dZ', g.year + 1900, g.mon + 1, g.mday, g.hour, g.min, g.sec, floor(g.usec / 1000));
else
    t = char(datetime('now', 'TimeZone', 'UTC', 'Format', 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z'''));
end
end
