% src/ClickHouseClient.m
classdef ClickHouseClient < handle
    % ClickHouseClient  Connect to ClickHouse via native TCP.
    %
    %   client = ClickHouseClient(host, port, username, password)
    %   client = ClickHouseClient(host, port, username, password, options)
    %
    %   options fields:
    %     options.settings   - containers.Map of string->string server settings
    %     options.useragent  - string, custom client name
    %     options.maxRetries - integer >= 0, retry attempts on query/insert failure (default 3)
    %     options.compression - Compression enum: Compression.None | Compression.LZ4
    %                          | Compression.ZSTD. Native-protocol block compression.
    %                          Default Compression.LZ4 (was: no compression).
    %     options.tls        - struct with fields:
    %       .enabled           logical  (true to enable TLS)
    %       .skip_verification logical  (true to skip cert check, dev only)
    %       .ca_file           string   (path to CA certificate file)

    properties (Access = private)
        ptr        (1,1) uint64  = uint64(0)
        maxRetries (1,1) double  = 3
    end

    methods
        function obj = ClickHouseClient(host, port, username, password, options)
            % ClickHouseClient  Create a ClickHouse connection.
            %
            % NOTES ON OPTIONS:
            %   options.tls        - Supported. See class documentation.
            %   options.settings   - Parsed but NOT applied to the connection.
            %                        The clickhouse-cpp ClientOptions API does not
            %                        expose per-connection settings. No error is thrown.
            %   options.useragent  - Parsed but NOT applied. The clickhouse-cpp
            %                        ClientOptions API does not expose a client name
            %                        setter. No error is thrown.
            %   options.compression - Supported. Default Compression.LZ4. Set
            %                        Compression.None for no compression. See class doc.
            arguments
                host     (1,1) string
                port     (1,1) double  = 9000
                username (1,1) string  = "default"
                password (1,1) string  = ""
                options  (1,1) struct  = struct()
            end

            % maxRetries: gates the MATLAB-level catch retry AND configures the
            % C++ client (SetPingBeforeQuery / SetSendRetries). 0 = fail-fast.
            if isfield(options, 'maxRetries')
                obj.maxRetries = options.maxRetries;
            end
            options.maxRetries = obj.maxRetries;

            % compression: client-level native-protocol block compression.
            % Defaults to LZ4 (a behavior change from no-compression). Must be a
            % Compression enum value; converted to its int8 code for transport
            % across the MEX boundary (an enum value object cannot be read via
            % the C MEX API, but its int8 code can).
            if ~isfield(options, 'compression') || isempty(options.compression)
                options.compression = db.mex.Compression.LZ4;
            end
            if ~isa(options.compression, 'db.mex.Compression') || ~isscalar(options.compression)
                error("ClickHouse:badOption", ...
                    "options.compression must be a scalar Compression enum value " + ...
                    "(db.mex.Compression.None, db.mex.Compression.LZ4, or db.mex.Compression.ZSTD).");
            end
            options.compression = int8(options.compression);

            % Expand containers.Map settings to cell arrays for MEX
            if isfield(options, 'settings') && isa(options.settings, 'containers.Map')
                m = options.settings;
                options.settings_keys = keys(m);
                options.settings_vals = values(m);
                options = rmfield(options, 'settings');
            end

            obj.ptr = db.mex.clickhouse_mex('connect', ...
                char(host), port, char(username), char(password), options);
        end

        function result = ping(obj)
            % ping  Send SELECT 1 to verify connectivity. Returns true or throws.
            result = db.mex.clickhouse_mex('ping', obj.ptr);
        end

        function result = query(obj, sql)
            % query  Execute SQL. Returns a MATLAB table (empty for DDL).
            arguments
                obj
                sql (1,:) string
            end
            % Support multi-element string arrays (e.g. ["line1" "line2"])
            if numel(sql) > 1
                sql = strjoin(sql, '');
            end
            try
                s = db.mex.clickhouse_mex('query', obj.ptr, char(sql));
            catch ME
                if obj.maxRetries <= 0
                    rethrow(ME);
                end
                % One retry — SetPingBeforeQuery will reconnect automatically
                s = db.mex.clickhouse_mex('query', obj.ptr, char(sql));
            end
            % Handle DDL / zero-column result
            if isempty(fieldnames(s))
                result = table();
                return;
            end
            % Convert cell-string fields to string arrays
            fields = fieldnames(s);
            for i = 1:numel(fields)
                f = fields{i};
                v = s.(f);
                if iscellstr(v) %#ok<ISCLSTR>
                    s.(f) = string(v);
                elseif iscell(v) && ~isempty(v) && iscell(v{1})
                    % Array(String): cell of cell-of-char -> cell of string arrays
                    s.(f) = cellfun(@(c) string(c), v, 'UniformOutput', false);
                elseif iscell(v) && ~isempty(v) && ...
                    any(cellfun(@(x) ischar(x) || islogical(x), v))
                    % Nullable(String-like): char (non-null) and false logical sentinel (null).
                    % The logical sentinel disambiguates from Array(T) typed empty arrays,
                    % which are numeric (double([]), int8([]), etc.), never logical.
                    arr = strings(size(v));
                    for k = 1:numel(v)
                        if islogical(v{k})
                            arr(k) = missing;
                        else
                            arr(k) = string(v{k});
                        end
                    end
                    s.(f) = arr;
                end
            end
            result = struct2table(s);
        end

        function schema = describe(obj, tableName)
            % describe  Return the table schema (DESCRIBE TABLE result).
            %   Pass the returned table to insert() as the optional schema
            %   argument to skip the per-insert DESCRIBE round-trip when
            %   inserting into the same table repeatedly.
            arguments
                obj
                tableName (1,1) string
            end
            schema = obj.query("DESCRIBE TABLE " + tableName);
        end

        function insert(obj, tableName, data, schema)
            % insert  Insert rows into a ClickHouse table.
            %   data can be a MATLAB table or a scalar struct of arrays.
            %   schema (optional) — DESCRIBE TABLE result from obj.describe().
            %   If omitted, it is fetched on demand. Pass it explicitly to
            %   skip the round-trip on repeated inserts into the same table.
            %   Must be a table with a 'type' column; anything else throws
            %   ClickHouse:badSchema.
            arguments
                obj
                tableName (1,1) string
                data
                schema = []
            end
            if istable(data)
                data = table2struct(data, 'ToScalar', true);
            end
            % Normalise string fields: string array -> cellstr (simpler in MEX)
            % Also normalise Array(String) columns: cell of string -> cell of cellstr
            fields = fieldnames(data);
            for i = 1:numel(fields)
                f = fields{i};
                v = data.(f);
                if isstring(v)
                    % Nullable(String): keep missing as [] sentinel for MEX
                    if any(ismissing(v))
                        c = cell(size(v));
                        for k = 1:numel(v)
                            if ismissing(v(k)), c{k} = []; else, c{k} = char(v(k)); end
                        end
                        data.(f) = c;
                    else
                        data.(f) = cellstr(v);
                    end
                elseif strcmp(class(v), 'missing')
                    % Scalar or array of missing → all-null sentinel cell array
                    c = cell(numel(v), 1);
                    for k = 1:numel(v), c{k} = []; end
                    data.(f) = c;
                elseif ischar(v)
                    data.(f) = cellstr(v);
                elseif iscell(v)
                    % Array(String) column: cell of string arrays -> cell of cellstr
                    % so MEX sees cell-of-cell-of-char
                    needs_conv = false;
                    for k = 1:numel(v)
                        if isstring(v{k})
                            needs_conv = true;
                            break;
                        elseif iscell(v{k}) && ~isempty(v{k}) && isstring(v{k}{1})
                            needs_conv = true;
                            break;
                        end
                    end
                    if needs_conv
                        for k = 1:numel(v)
                            if isstring(v{k})
                                v{k} = cellstr(v{k});
                            elseif iscell(v{k})
                                v{k} = cellfun(@char, v{k}, 'UniformOutput', false);
                            end
                        end
                        data.(f) = v;
                    end
                end
            end
            % Schema source: caller-provided (fast path, skips round-trip) or
            % fetched on demand. The schema drives MEX-layer type hints —
            % ClickHouse rejects plain ColumnFloat64 into Nullable(Float64),
            % needs DateTime64 precision, LowCardinality inner type, etc.
            if nargin < 4
                try
                    schema = obj.describe(tableName);
                catch
                    schema = table();   % silent: proceed without hints
                end
            elseif ~istable(schema) || ~ismember('type', schema.Properties.VariableNames)
                error("ClickHouse:badSchema", ...
                    "schema must be a table returned by obj.describe(); got class=%s.", ...
                    class(schema));
            end
            if height(schema) > 0
                desc = schema;
                data_fields = fieldnames(data);
                nullable_hint    = {};
                datetime64_hint  = struct();
                date_type_hint   = struct();
                nullable_int_hint = struct();
                lc_hint          = struct();
                ipv4_hint        = {};
                ipv6_hint        = {};
                enum_hint        = struct();
                fixedstring_hint = struct();
                decimal_hint     = struct();
                for i = 1:height(desc)
                    col_name = char(desc.name(i));
                    type_str = char(desc.type(i));
                    if ~ismember(col_name, data_fields), continue; end
                    if startsWith(type_str, 'Nullable(')
                        nullable_hint{end+1} = col_name; %#ok<AGROW>
                    end
                    % Detect DateTime64(N) or Nullable(DateTime64(N))
                    tok = regexp(type_str, 'DateTime64\((\d+)', 'tokens', 'once');
                    if ~isempty(tok)
                        datetime64_hint.(col_name) = str2double(tok{1});
                    end
                    % Detect Date / Date32 / DateTime (non-DateTime64)
                    if ~isempty(regexp(type_str, '^(Nullable\()?Date\)?\s*$', 'once')) || ...
                            strcmp(type_str, 'Date') || strcmp(type_str, 'Nullable(Date)')
                        date_type_hint.(col_name) = 'Date';
                    elseif strcmp(type_str, 'Date32') || strcmp(type_str, 'Nullable(Date32)')
                        date_type_hint.(col_name) = 'Date32';
                    elseif ~isempty(regexp(type_str, '^(Nullable\()?DateTime(\(|''|\s*$)', 'once')) && ...
                            isempty(regexp(type_str, 'DateTime64', 'once'))
                        date_type_hint.(col_name) = 'DateTime';
                    end
                    % Detect Nullable(Int*/UInt*) for integer nullable insert
                    ni_tok = regexp(type_str, '^Nullable\((Int8|Int16|Int32|Int64|UInt8|UInt16|UInt32|UInt64)\)$', 'tokens', 'once');
                    if ~isempty(ni_tok)
                        nullable_int_hint.(col_name) = ni_tok{1};
                    end
                    % Detect LowCardinality columns
                    lc_tok = regexp(type_str, '^LowCardinality\((\w+)', 'tokens', 'once');
                    if ~isempty(lc_tok)
                        lc_hint.(col_name) = lc_tok{1};
                    end
                    % Strip Nullable wrapper for remaining detection
                    bare_type = regexprep(type_str, '^Nullable\((.+)\)$', '$1');
                    % IPv4 / IPv6
                    if strcmp(bare_type, 'IPv4')
                        ipv4_hint{end+1} = col_name; %#ok<AGROW>
                    elseif strcmp(bare_type, 'IPv6')
                        ipv6_hint{end+1} = col_name; %#ok<AGROW>
                    end
                    % FixedString(N)
                    fs_tok = regexp(bare_type, '^FixedString\((\d+)\)$', 'tokens', 'once');
                    if ~isempty(fs_tok)
                        fixedstring_hint.(col_name) = str2double(fs_tok{1});
                    end
                    % Enum8/Enum16 — store the bare type string for MEX parsing
                    if startsWith(bare_type, 'Enum8(') || startsWith(bare_type, 'Enum16(')
                        enum_hint.(col_name) = bare_type;
                    end
                    % Decimal32/64/128(S) and Decimal(P,S)
                    dec_tok = regexp(bare_type, '^Decimal(32|64|128)\((\d+)\)$', 'tokens', 'once');
                    if ~isempty(dec_tok)
                        dec_bits = dec_tok{1}; dec_scale = str2double(dec_tok{2});
                        if strcmp(dec_bits,'32'), dec_prec = 9;
                        elseif strcmp(dec_bits,'64'), dec_prec = 18;
                        else, dec_prec = 38; end
                        decimal_hint.(col_name) = [dec_prec, dec_scale];
                    else
                        dec_tok2 = regexp(bare_type, '^Decimal\((\d+),\s*(\d+)\)$', 'tokens', 'once');
                        if ~isempty(dec_tok2)
                            decimal_hint.(col_name) = [str2double(dec_tok2{1}), str2double(dec_tok2{2})];
                        end
                    end
                end
                if ~isempty(nullable_hint)
                    data.ch_nullable_hint = nullable_hint;
                end
                if ~isempty(fieldnames(datetime64_hint))
                    data.ch_datetime64_hint = datetime64_hint;
                end
                if ~isempty(fieldnames(date_type_hint))
                    data.ch_date_type_hint = date_type_hint;
                end
                if ~isempty(fieldnames(nullable_int_hint))
                    data.ch_nullable_int_hint = nullable_int_hint;
                end
                if ~isempty(fieldnames(lc_hint))
                    data.ch_lc_hint = lc_hint;
                end
                if ~isempty(ipv4_hint)
                    data.ch_ipv4_hint = ipv4_hint;
                end
                if ~isempty(ipv6_hint)
                    data.ch_ipv6_hint = ipv6_hint;
                end
                if ~isempty(fieldnames(enum_hint))
                    data.ch_enum_hint = enum_hint;
                end
                if ~isempty(fieldnames(fixedstring_hint))
                    data.ch_fixedstring_hint = fixedstring_hint;
                end
                if ~isempty(fieldnames(decimal_hint))
                    data.ch_decimal_hint = decimal_hint;
                end
            end
            try
                db.mex.clickhouse_mex('insert', obj.ptr, char(tableName), data);
            catch ME
                if obj.maxRetries <= 0
                    rethrow(ME);
                end
                % One retry — SetPingBeforeQuery will reconnect automatically
                db.mex.clickhouse_mex('insert', obj.ptr, char(tableName), data);
            end
        end

        function delete(obj)
            % Destructor - called automatically when object goes out of scope.
            if obj.ptr ~= uint64(0)
                db.mex.clickhouse_mex('delete', obj.ptr);
                obj.ptr = uint64(0);
            end
        end

        function v = version(~)
            % version  Return the clickhouse-matlab library version as a string.
            v = string(db.mex.clickhouse_mex('version'));
        end
    end
end
