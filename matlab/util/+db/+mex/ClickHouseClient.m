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
    %     options.tls        - struct with fields:
    %       .enabled           logical  (true to enable TLS)
    %       .skip_verification logical  (true to skip cert check, dev only)
    %       .ca_file           string   (path to CA certificate file)

    properties (Access = private)
        ptr (1,1) uint64 = uint64(0)
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
            arguments
                host     (1,1) string
                port     (1,1) double  = 9000
                username (1,1) string  = "default"
                password (1,1) string  = ""
                options  (1,1) struct  = struct()
            end

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
            s = db.mex.clickhouse_mex('query', obj.ptr, char(sql));
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
                elseif iscell(v) && ~isempty(v) && any(cellfun(@ischar, v))
                    % Nullable(String): cell with char elements (non-null) and [] sentinels (null)
                    arr = strings(size(v));
                    for k = 1:numel(v)
                        if isnumeric(v{k}) && isempty(v{k})
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

        function insert(obj, tableName, data)
            % insert  Insert rows into a ClickHouse table.
            %   data can be a MATLAB table or a scalar struct of arrays.
            arguments
                obj
                tableName (1,1) string
                data
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
            % Query table schema to detect Nullable columns.
            % ClickHouse (particularly recent versions) rejects a plain ColumnFloat64
            % inserted into a Nullable(Float64) column. We pass a ch_nullable_hint cell
            % array of column names so MEX can force-wrap them in ColumnNullable.
            try
                desc = obj.query("DESCRIBE TABLE " + tableName);
                if height(desc) > 0 && ismember('type', desc.Properties.VariableNames)
                    data_fields = fieldnames(data);
                    nullable_hint = {};
                    for i = 1:height(desc)
                        col_name = char(desc.name(i));
                        type_str = char(desc.type(i));
                        if startsWith(type_str, 'Nullable(') && ismember(col_name, data_fields)
                            nullable_hint{end+1} = col_name; %#ok<AGROW>
                        end
                    end
                    if ~isempty(nullable_hint)
                        data.ch_nullable_hint = nullable_hint;
                    end
                end
            catch
                % Proceed without hint if DESCRIBE fails (e.g. no SELECT privilege)
            end
            db.mex.clickhouse_mex('insert', obj.ptr, char(tableName), data);
        end

        function delete(obj)
            % Destructor - called automatically when object goes out of scope.
            if obj.ptr ~= uint64(0)
                db.mex.clickhouse_mex('delete', obj.ptr);
                obj.ptr = uint64(0);
            end
        end
    end
end
