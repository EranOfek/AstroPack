%==========================================================================
% Project     : AstroPack / io.chlog
% Filename    : io.chlog.ChLogger.m
% Author      : Chen Tishler
% Created     : 03/11/2026
% Updated     : 03/11/2026
% Description : ClickHouse logging client for Matlab (HTTP JSONEachRow adapter).
%               Same spool/replay contract as the Python and C++ clients (docs/DESIGN.md).
%==========================================================================


classdef ChLogger < handle
    % ClickHouse logging client for Matlab R2020 and later.
    %
    % Typical Usage:
    %   cfg = io.chlog.ChLogger.loadConfig('path/to/chlog.json');
    %   L = io.chlog.ChLogger.open(cfg, 'Process', 'my_app');
    %   L.log('INF', 'command accepted', 'CorrId', 'cmd-1');
    %   L.perf('cmd.execute', @() runStep(), 'CorrId', 'cmd-1');
    %   L.flush();
    %   L.close();
    %
    %   How it behaves (the same rules as the Python and C++ clients):
    %     * chlog(...) never throws. ts is the time of the call (UTC, ms), never the insert time.
    %     * Rows are collected in memory and sent in batches by the insert adapter (cfg.matlab.insertFcn),
    %       when batch_rows is reached, when flush_interval_ms passed, at once for ERR/CRT, and by a timer
    %       while Matlab is idle. Matlab has one thread, so there is no background log thread.
    %     * If the adapter says it failed, the rows are appended to a local spool file and replayed later
    %       with their ORIGINAL ts. Delivery is at-least-once; every replay chunk carries a token
    %       "<segment>:<offset>:<chunk_rows>" that the adapter should hand to ClickHouse as
    %       insert_deduplication_token, so a re-sent chunk is stored once.
    %     * Spool files have the same names and format as the Python and C++ clients. If this Matlab session
    %       dies, the janitor (python -m chlog.janitor, or chlog_janitor) replays the files.
    %     * The client never deletes old logs in ClickHouse. Retention belongs to the single log admin process.
    %
    %   Adapter contract (docs/GUIDE.md "Matlab"):  [ok, errMsg] = insertFcn(rows, token, cfg)
    %     rows   struct array / table / one char vector of JSON lines, see cfg.matlab.rowFormat
    %     token  '' for a live batch, else the deduplication token of a replay chunk
    %     ok     true when ClickHouse confirmed the whole batch
    %   An adapter may declare fewer inputs (rows, token) and may return a third output, reject (true = the
    %   data itself is bad, do not retry: the client isolates the bad row into spool/rejected and moves on).

    properties (SetAccess = private)
        cfg
        node
        process
        runId
        pid
        spoolDir
    end
    properties (Access = private)
        insertFcn
        nIn = 2
        nOut = 2
        lines = {}
        levels = {}
        nBuf = 0
        seq = 0
        lastFlushMs = 0
        degraded = false
        backoffS = 5
        retryAtMs = 0
        segN = 0
        curPath = ''
        curBytes = 0
        clockFcn
        sentRows = 0
        spooledRows = 0
        replayedRows = 0
        rejectedRows = 0
        spoolDropped = 0
        markerReported = [0 0]
        lastError = ''
        timerObj = []
        busy = false
        closed = false
        batchRows
        flushMs
        chunkRows
        maxMsg
        spoolMaxBytes
        segmentBytes
        retryMinS
        retryMaxS
        rowFormat
        prefix
    end

    methods (Static)
        function C = getSingleton(NewLogger)
            % Return or set the process-wide Logger (optional NewLogger to install).
            persistent Current
            if nargin > 0, Current = NewLogger; end
            C = Current;
        end


        function Tf = isValidHandle(C)
            % True when C is a live Logger handle (Octave-safe).
            Tf = ~isempty(C);
            if Tf
                try
                    Tf = isvalid(C);
                catch
                    Tf = true;
                end
            end
        end


        function Obj = open(Cfg, varargin)
            % Start the logger for this Matlab session (replaces a previous open logger).
            %
            % :param Cfg: struct, YAML/JSON path, [] to load CHLOG_CONFIG / ChLogger.yaml, or '' with
            %   Overrides for offline built-in defaults (demo and unit tests).
            % Name-value: Process, Node, InsertFcn, Overrides, UseTimer, ClockMs.
            % :return: io.chlog.ChLogger handle.
            Old = io.chlog.ChLogger.getSingleton();
            if io.chlog.ChLogger.isValidHandle(Old), Old.close(); end
            Opts = struct('Process', '', 'Node', '', 'InsertFcn', [], 'Overrides', [], 'UseTimer', true, 'ClockMs', []);
            for K = 1:2:numel(varargin)
                Opts.(varargin{K}) = varargin{K + 1};
            end
            % Empty char without Overrides means "default YAML"; empty char with Overrides is offline defaults.
            CfgArg = Cfg;
            if ischar(Cfg) && isempty(Cfg) && isempty(Opts.Overrides)
                CfgArg = [];
            end
            Loaded = io.chlog.ChLogger.loadConfig(CfgArg, Opts.Overrides);
            Obj = io.chlog.ChLogger(Loaded, 'Process', Opts.Process, 'Node', Opts.Node, ...
                'InsertFcn', Opts.InsertFcn, 'UseTimer', Opts.UseTimer, 'ClockMs', Opts.ClockMs);
            io.chlog.ChLogger.getSingleton(Obj);
        end


        function Cfg = loadConfig(PathOrStruct, Overrides)
            % Load logger configuration (defaults, ChLogger.yaml, JSON file, or struct).
            % '' (char) alone: built-in defaults only (unit tests). [] or omitted: CHLOG_CONFIG / ChLogger.yaml.
            % client.node: empty or whitespace -> constructor uses envHelper('host').
            Cfg = struct();
            Cfg.clickhouse = struct('host', '127.0.0.1', 'port', 8123, 'native_port', 9000, ...
                'database', 'chlog', 'user', 'chlog', 'password', '', 'connect_timeout_s', 2, 'read_timeout_s', 10);
            Cfg.client = struct('node', '', 'process', '', 'env', '', 'build', '', 'batch_rows', 1000, ...
                'flush_interval_ms', 1000, 'queue_max_rows', 100000, 'max_message_bytes', 16384, ...
                'spool_dir', './spool', 'spool_max_mb', 2048, 'spool_segment_mb', 16, ...
                'replay_chunk_rows', 1000, 'retry_min_s', 5, 'retry_max_s', 30);
            Cfg.matlab = struct('insertFcn', 'http', 'rowFormat', 'jsonlines');
            defaultsOnly = (nargin >= 1) && ischar(PathOrStruct) && isempty(PathOrStruct);
            hasOverrides = (nargin >= 2) && ~isempty(Overrides);
            if ~defaultsOnly
                if nargin < 1 || (isempty(PathOrStruct) && ~ischar(PathOrStruct))
                    PathOrStruct = getenv('CHLOG_CONFIG');
                end
                if isempty(PathOrStruct)
                    PathOrStruct = io.chlog.ChLogger.defaultConfigPath();
                end
                if isstruct(PathOrStruct)
                    Cfg = logger_merge_struct(Cfg, PathOrStruct);
                elseif ~isempty(PathOrStruct)
                    Path = char(PathOrStruct);
                    if isempty(Path) || ~exist(Path, 'file')
                        if hasOverrides
                            warning('io.chlog:config', ...
                                'ChLogger config file not found%s; using built-in defaults with Overrides.', ...
                                logger_config_path_suffix(Path));
                        else
                            if isempty(Path)
                                Root = Configuration.getSysConfigPath();
                                error('io.chlog:config', ...
                                    'no ChLogger.yaml under config or config/local (root: %s)', Root);
                            end
                            error('io.chlog:config', 'config file not found: %s', Path);
                        end
                    else
                        [~, ~, Ext] = fileparts(Path);
                        Ext = lower(Ext);
                        if any(strcmp(Ext, {'.yaml', '.yml'}))
                            Loaded = logger_load_chlogger_yaml_sections(Path);
                            Cfg = logger_merge_struct(Cfg, Loaded);
                        else
                            Text = fileread(Path);
                            if numel(Text) >= 3 && all(double(Text(1:3)) == [239 187 191]), Text = Text(4:end); end
                            try
                                Loaded = jsondecode(Text);
                            catch Err
                                error('io.chlog:config', 'config file %s is not valid JSON: %s', Path, Err.message);
                            end
                            Cfg = logger_merge_struct(Cfg, Loaded);
                        end
                    end
                end
            end
            if hasOverrides, Cfg = logger_merge_struct(Cfg, Overrides); end
            if isempty(Cfg.clickhouse.password)
                Pw = getenv('CHLOG_CH_PASSWORD');
                if ~isempty(Pw), Cfg.clickhouse.password = Pw; end
            end
        end


        function Path = defaultConfigPath()
            % Resolve config/local/ChLogger.yaml, local .yml, or config/ChLogger.yaml; '' if none exist.
            Root = Configuration.getSysConfigPath();
            Candidates = {fullfile(Root, 'local', 'ChLogger.yaml'), ...
                fullfile(Root, 'local', 'ChLogger.yml'), fullfile(Root, 'ChLogger.yaml')};
            Path = '';
            for K = 1:numel(Candidates)
                if isfile(Candidates{K})
                    Path = Candidates{K};
                    return;
                end
            end
        end


        function Int = msgLoggerIntegrationSettings()
            % MsgLogger fan-out: level, process, and chlog cfg from ChLogger.yaml.
            Path = io.chlog.ChLogger.defaultConfigPath();
            Int = struct('msgLoggerLevel', LogLevel.Info, 'process', 'AstroPack', 'cfgStruct', struct());
            if isempty(Path)
                Int.cfgStruct = io.chlog.ChLogger.loadConfig(struct());
                return;
            end
            Block = logger_read_chlogger_yaml_block(Path);
            if isfield(Block, 'MsgLoggerLevel') && ~isempty(Block.MsgLoggerLevel)
                try
                    Parsed = eval(char(Block.MsgLoggerLevel)); %#ok<EVLDIR>
                    if isa(Parsed, 'LogLevel')
                        Int.msgLoggerLevel = Parsed;
                    else
                        fprintf(2, 'ChLogger: MsgLoggerLevel did not yield LogLevel: %s\n', ...
                            char(Block.MsgLoggerLevel));
                    end
                catch Ex
                    fprintf(2, 'ChLogger: invalid MsgLoggerLevel %s (%s), using Info\n', ...
                        char(Block.MsgLoggerLevel), Ex.message);
                end
            end
            if isfield(Block, 'Process') && ~isempty(Block.Process)
                Int.process = char(Block.Process);
            end
            Int.cfgStruct = io.chlog.ChLogger.loadConfig(Path);
        end


        function S = tsString(Ms)
            % Milliseconds since 1970 UTC -> wire ts string (yyyy-mm-dd HH:MM:SS.mmm).
            Ms = floor(Ms);
            Days = floor(Ms / 86400000);
            MsDay = Ms - Days * 86400000;
            Z = Days + 719468;
            Era = floor(Z / 146097);
            Doe = Z - Era * 146097;
            Yoe = floor((Doe - floor(Doe / 1460) + floor(Doe / 36524) - floor(Doe / 146096)) / 365);
            Y = Yoe + Era * 400;
            Doy = Doe - (365 * Yoe + floor(Yoe / 4) - floor(Yoe / 100));
            Mp = floor((5 * Doy + 2) / 153);
            D = Doy - floor((153 * Mp + 2) / 5) + 1;
            if Mp < 10, M = Mp + 3; else, M = Mp - 9; end
            if M <= 2, Y = Y + 1; end
            Hh = floor(MsDay / 3600000);
            Mi = floor(mod(MsDay, 3600000) / 60000);
            Ss = floor(mod(MsDay, 60000) / 1000);
            Mmm = mod(MsDay, 1000);
            S = sprintf('%04d-%02d-%02d %02d:%02d:%02d.%03d', Y, M, D, Hh, Mi, Ss, Mmm);
        end


        function S = jsonStr(Text, Bytes)
            % JSON string literal for a Matlab/Octave char vector.
            if nargin < 2, Bytes = io.chlog.ChLogger.envHelper('is_octave'); end
            if isempty(Text), S = '""'; return; end
            if ~ischar(Text), Text = char(Text); end
            U = double(Text(:)');
            if Bytes, U = logger_utf8_to_code_points(U); end
            S = io.chlog.ChLogger.jsonUnits(U);
        end


        function S = jsonUnits(Units)
            % JSON string literal from UTF-16 code units (wire escaping).
            U = double(Units(:)');
            if isempty(U), S = '""'; return; end
            Plain = (U >= 32 & U <= 126 & U ~= 34 & U ~= 92);
            if all(Plain)
                S = ['"' char(U) '"'];
                return;
            end
            Parts = cell(1, numel(U));
            for K = 1:numel(U)
                C = U(K);
                if Plain(K)
                    Parts{K} = char(C);
                elseif C == 34, Parts{K} = '\"';
                elseif C == 92, Parts{K} = '\\';
                elseif C == 10, Parts{K} = '\n';
                elseif C == 13, Parts{K} = '\r';
                elseif C == 9,  Parts{K} = '\t';
                elseif C == 8,  Parts{K} = '\b';
                elseif C == 12, Parts{K} = '\f';
                elseif C < 65536
                    Parts{K} = sprintf('\\u%04x', C);
                else
                    C = C - 65536;
                    Parts{K} = sprintf('\\u%04x\\u%04x', 55296 + floor(C / 1024), 56320 + mod(C, 1024));
                end
            end
            S = ['"' strjoin(Parts, '') '"'];
        end


        function Line = rowLine(Row)
            % One log row struct -> one JSON line (docs/DESIGN.md 5.1).
            Line = ['{"ts":' io.chlog.ChLogger.jsonStr(Row.ts) ...
                ',"level":' io.chlog.ChLogger.jsonStr(Row.level) ...
                ',"node":' io.chlog.ChLogger.jsonStr(Row.node) ...
                ',"process":' io.chlog.ChLogger.jsonStr(Row.process) ...
                ',"pid":' logger_wire_num(Row.pid) ...
                ',"tid":' logger_wire_num(Row.tid) ...
                ',"run_id":' io.chlog.ChLogger.jsonStr(Row.run_id) ...
                ',"seq":' logger_wire_num(Row.seq) ...
                ',"perf_us":' logger_wire_num(Row.perf_us) ...
                ',"perf_op":' io.chlog.ChLogger.jsonStr(Row.perf_op) ...
                ',"subsystem":' io.chlog.ChLogger.jsonStr(Row.subsystem) ...
                ',"logger":' io.chlog.ChLogger.jsonStr(Row.logger) ...
                ',"event_code":' logger_wire_num(Row.event_code) ...
                ',"corr_id":' io.chlog.ChLogger.jsonStr(Row.corr_id) ...
                ',"env":' io.chlog.ChLogger.jsonStr(Row.env) ...
                ',"build":' io.chlog.ChLogger.jsonStr(Row.build) ...
                ',"src":' io.chlog.ChLogger.jsonStr(Row.src) ...
                ',"message":' io.chlog.ChLogger.jsonStr(Row.message) ...
                ',"attrs":' Row.attrs '}'];
        end


        function S = attrsJson(Attrs)
            % Struct of extras -> JSON text for the attrs Map column.
            if nargin < 1 || isempty(Attrs) || ~isstruct(Attrs)
                S = '{}';
                return;
            end
            Names = fieldnames(Attrs);
            Parts = cell(1, numel(Names));
            for K = 1:numel(Names)
                V = Attrs(1).(Names{K});
                if ischar(V)
                    T = V;
                elseif islogical(V) && isscalar(V)
                    if V, T = 'true'; else, T = 'false'; end
                elseif isnumeric(V) && isscalar(V)
                    T = sprintf('%.15g', V);
                else
                    try
                        T = mat2str(V);
                    catch
                        T = '?';
                    end
                end
                Parts{K} = [io.chlog.ChLogger.jsonStr(Names{K}) ':' io.chlog.ChLogger.jsonStr(T)];
            end
            S = ['{' strjoin(Parts, ',') '}'];
        end


        function V = envHelper(What)
            % OS helpers: now_ms, pid, host, uuid, is_octave.
            switch What
                case 'is_octave'
                    V = exist('OCTAVE_VERSION', 'builtin') ~= 0;
                case 'now_ms'
                    if io.chlog.ChLogger.envHelper('is_octave')
                        V = floor(time() * 1000);
                    elseif usejava('jvm')
                        V = double(java.lang.System.currentTimeMillis());
                    else
                        V = floor(posixtime(datetime('now', 'TimeZone', 'UTC')) * 1000);
                    end
                case 'pid'
                    V = 0;
                    try
                        if io.chlog.ChLogger.envHelper('is_octave')
                            V = getpid();
                        else
                            V = feature('getpid');
                        end
                    catch
                    end
                case 'host'
                    V = getenv('COMPUTERNAME');
                    if isempty(V), V = getenv('HOSTNAME'); end
                    if isempty(V)
                        try
                            [St, Out] = system('hostname');
                            if St == 0, V = strtrim(Out); end
                        catch
                        end
                    end
                    if isempty(V), V = 'node'; end
                case 'uuid'
                    V = '';
                    try
                        if ~io.chlog.ChLogger.envHelper('is_octave') && usejava('jvm')
                            V = char(java.util.UUID.randomUUID().toString());
                        end
                    catch
                    end
                    if isempty(V)
                        T = io.chlog.ChLogger.envHelper('now_ms');
                        P = io.chlog.ChLogger.envHelper('pid');
                        C = floor(mod(cputime * 1e6, 2^32));
                        V = sprintf('%08x-%04x-4%03x-a%03x-%012x', mod(floor(T / 65536), 2^32), mod(T, 65536), ...
                            mod(C, 4096), mod(P, 4096), mod(floor(T * 4096 + C), 2^47));
                    end
                    V = lower(V);
                otherwise
                    error('chlog:env', 'unknown request %s', What);
            end
        end


        function [Ok, ErrMsg, Reject] = insertHttp(Rows, Token, Cfg)
            % HTTP insert adapter (port 8123, JSONEachRow). Never throws.
            Ok = false; ErrMsg = ''; Reject = false;
            try
                Ch = Cfg.clickhouse;
                if ~ischar(Rows), error('chlog:insertHttp needs rowFormat jsonlines (got %s)', class(Rows)); end
                Q = struct('database', Ch.database, ...
                    'query', sprintf('INSERT INTO %s.logs FORMAT JSONEachRow', Ch.database), ...
                    'date_time_input_format', 'best_effort', ...
                    'input_format_skip_unknown_fields', '1');
                if isempty(Token)
                    Q.async_insert = '1';
                    Q.wait_for_async_insert = '1';
                else
                    Q.async_insert = '0';
                    Q.insert_deduplication_token = Token;
                end
                Uri = matlab.net.URI(sprintf('http://%s:%d/', Ch.host, Ch.port), matlab.net.QueryParameter(Q));
                Fields = matlab.net.http.HeaderField('Connection', 'close');
                if ~isempty(Ch.user), Fields(end + 1) = matlab.net.http.HeaderField('X-ClickHouse-User', Ch.user); end
                if ~isempty(Ch.password), Fields(end + 1) = matlab.net.http.HeaderField('X-ClickHouse-Key', Ch.password); end
                Body = matlab.net.http.MessageBody();
                Body.Payload = unicode2native(Rows, 'UTF-8');
                Req = matlab.net.http.RequestMessage('POST', Fields, Body);
                Opts = matlab.net.http.HTTPOptions('ConnectTimeout', Ch.connect_timeout_s, ...
                    'ResponseTimeout', Ch.read_timeout_s, 'ConvertResponse', false);
                Resp = Req.send(Uri, Opts);
                Code = double(Resp.StatusCode);
                if Code == 200
                    Ok = true;
                    return;
                end
                Txt = '';
                try, Txt = char(Resp.Body.Data(:)'); catch, end %#ok<NOCOM>
                ErrMsg = sprintf('HTTP %d %s', Code, strtrim(Txt(1:min(numel(Txt), 300))));
                Reject = (Code == 400);
            catch Err
                ErrMsg = [Err.identifier ': ' Err.message];
            end
        end


        function logMessage(Level, Message, varargin)
            % Log via the process singleton; no-op when no logger is open.
            C = io.chlog.ChLogger.getSingleton();
            if io.chlog.ChLogger.isValidHandle(C), C.log(Level, Message, varargin{:}); end
        end


        function Ok = flushAll()
            % Flush the process singleton; true when nothing pending or no logger.
            C = io.chlog.ChLogger.getSingleton();
            Ok = true;
            if io.chlog.ChLogger.isValidHandle(C), Ok = C.flush(); end
        end


        function closeAll()
            % Close the process singleton if open.
            C = io.chlog.ChLogger.getSingleton();
            if io.chlog.ChLogger.isValidHandle(C), C.close(); end
        end


        function S = statsAll()
            % Stats from the process singleton, or empty struct.
            C = io.chlog.ChLogger.getSingleton();
            if ~io.chlog.ChLogger.isValidHandle(C), S = struct(); else, S = C.stats(); end
        end


    end

    methods
        function obj = ChLogger(Cfg, varargin)
            % Constructor. Name-value: Process, Node, InsertFcn, ClockMs, UseTimer.
            % Node column: non-empty open('Node') or client.node, else OS hostname.
            P = struct('Process', '', 'Node', '', 'InsertFcn', [], 'ClockMs', [], 'UseTimer', true);
            for K = 1:2:numel(varargin)
                P.(varargin{K}) = varargin{K + 1};
            end
            obj.cfg = Cfg;
            C = Cfg.client;
            obj.node = logger_pick(P.Node, C.node, io.chlog.ChLogger.envHelper('host'));
            obj.process = logger_pick(P.Process, C.process, 'matlab_app');
            obj.pid = io.chlog.ChLogger.envHelper('pid');
            obj.runId = io.chlog.ChLogger.envHelper('uuid');
            obj.clockFcn = P.ClockMs;
            if isempty(obj.clockFcn), obj.clockFcn = @() io.chlog.ChLogger.envHelper('now_ms'); end
            obj.batchRows = C.batch_rows;
            obj.flushMs = C.flush_interval_ms;
            obj.chunkRows = C.replay_chunk_rows;
            obj.maxMsg = C.max_message_bytes;
            obj.spoolMaxBytes = C.spool_max_mb * 1024 * 1024;
            obj.segmentBytes = C.spool_segment_mb * 1024 * 1024;
            obj.retryMinS = C.retry_min_s;
            obj.retryMaxS = C.retry_max_s;
            obj.backoffS = obj.retryMinS;
            obj.rowFormat = Cfg.matlab.rowFormat;
            obj.spoolDir = logger_absolute_path(C.spool_dir);
            obj.prefix = [logger_safe_name(obj.node) '_' logger_safe_name(obj.process)];
            if ~exist(obj.spoolDir, 'dir'), mkdir(obj.spoolDir); end
            if ~isempty(P.InsertFcn)
                obj.insertFcn = P.InsertFcn;
            else
                obj.insertFcn = logger_resolve_insert(Cfg.matlab.insertFcn);
            end
            try
                n = nargin(obj.insertFcn);
                if n < 0 || n >= 3, obj.nIn = 3; else, obj.nIn = 2; end
            catch
                obj.nIn = 3;
            end
            try
                n = nargout(obj.insertFcn);
                if n >= 3, obj.nOut = 3; end
            catch
            end
            obj.lastFlushMs = obj.clockFcn();
            if P.UseTimer && ~io.chlog.ChLogger.envHelper('is_octave') && exist('timer', 'file')
                try
                    obj.timerObj = timer('ExecutionMode', 'fixedSpacing', 'Period', max(0.2, obj.flushMs / 1000), ...
                                         'BusyMode', 'drop', 'Name', 'chlog', 'TimerFcn', @(~, ~) obj.tick());
                    start(obj.timerObj);
                catch
                    obj.timerObj = [];
                end
            end
        end

        function varargout = perf(obj, Op, Fcn, varargin)
            % Run Fcn, log one row with perf_us, return Fcn outputs. Rethrows after logging ERR on failure.
            O = struct('Message', '', 'Level', 'INF');
            Rest = {};
            for K = 1:2:numel(varargin)
                if isfield(O, varargin{K}), O.(varargin{K}) = varargin{K + 1}; else, Rest = [Rest varargin(K:K + 1)]; end %#ok<AGROW>
            end
            Msg = O.Message;
            if isempty(Msg), Msg = Op; end
            T0 = tic;
            try
                [varargout{1:max(nargout, 0)}] = Fcn();
            catch Err
                Us = round(toc(T0) * 1e6);
                obj.log('ERR', [Msg ' (failed: ' Err.identifier ')'], 'PerfUs', Us, 'PerfOp', Op, Rest{:});
                rethrow(Err);
            end
            Us = round(toc(T0) * 1e6);
            obj.log(O.Level, Msg, 'PerfUs', Us, 'PerfOp', Op, Rest{:});
        end


        function log(obj, level, message, varargin)
            % Log one row. Options: PerfUs, PerfOp, CorrId, Subsystem, Logger, EventCode, Attrs, Src.
            try
                if obj.closed, return; end
                o = struct('PerfUs', [], 'PerfOp', '', 'CorrId', '', 'Subsystem', '', 'Logger', '', ...
                           'EventCode', 0, 'Attrs', [], 'Src', '');
                for k = 1:2:numel(varargin)
                    o.(varargin{k}) = varargin{k + 1};
                end
                if isempty(o.Src), o.Src = logger_caller_src(); end
                obj.append(logger_norm_level(level), message, o, false);
                Now = obj.clockFcn();
                if logger_is_urgent(obj.levels{obj.nBuf}) || obj.nBuf >= obj.batchRows || Now - obj.lastFlushMs >= obj.flushMs
                    obj.flush();
                end
            catch err
                obj.lastError = ['log error: ' err.message];
            end
        end

        function ok = flush(obj)
            %FLUSH  Send everything collected so far (or spool it if the adapter fails). Returns true if nothing is pending.
            ok = false;
            if obj.busy, return; end
            obj.busy = true;
            cleanup = onCleanup(@() obj.release());
            try
                obj.lastFlushMs = obj.clockFcn();
                if obj.nBuf > 0
                    ls = obj.lines(1:obj.nBuf);
                    lv = obj.levels(1:obj.nBuf);
                    obj.lines = {}; obj.levels = {}; obj.nBuf = 0;
                    obj.sendBatch(ls, lv);
                end
                obj.maintain(2);
                obj.emitMarkers();
                ok = obj.nBuf == 0;
            catch err
                obj.lastError = ['flush error: ' err.message];
            end
            clear cleanup
        end

        function replay(obj, maxChunks)
            %REPLAY  Deliver spool files of this session now (all of them when maxChunks is omitted).
            if nargin < 2, maxChunks = Inf; end
            obj.flush();
            obj.replayOwn(maxChunks);
        end

        function close(obj)
            %CLOSE  Flush, stop the timer. Rows that could not be sent stay in the spool for the janitor.
            if obj.closed, return; end
            try
                obj.flush();
                obj.flush();                         % second pass: sends marker rows added by the first
            catch
            end
            obj.closed = true;
            obj.stopTimer();
        end

        function s = stats(obj)
            s = struct('seq', obj.seq, 'queued', obj.nBuf, 'sent_rows', obj.sentRows, 'spooled_rows', obj.spooledRows, ...
                       'replayed_rows', obj.replayedRows, 'rejected', obj.rejectedRows, 'spool_dropped', obj.spoolDropped, ...
                       'degraded', obj.degraded, 'spool_bytes', obj.spoolBytes(), 'last_error', obj.lastError, ...
                       'run_id', obj.runId, 'node', obj.node, 'process', obj.process, 'pid', obj.pid);
        end

        function delete(obj)
            try, obj.stopTimer(); catch, end
        end
    end

    methods (Access = private)
        function release(obj)
            obj.busy = false;
        end

        function stopTimer(obj)
            if ~isempty(obj.timerObj)
                try
                    stop(obj.timerObj);
                    delete(obj.timerObj);
                catch
                end
                obj.timerObj = [];
            end
        end

        function tick(obj)
            % Timer callback: flush rows that waited long enough, and retry the spool.
            try
                if obj.closed || obj.busy, return; end
                if obj.nBuf > 0 || obj.hasSpool()
                    obj.flush();
                end
            catch
            end
        end

        function append(obj, level, message, o, force) %#ok<INUSD>
            % Assign ts and seq, build the JSON line, and queue it. This is the only place rows are made.
            if isnumeric(message) || islogical(message), message = num2str(message); end
            if ~ischar(message), message = char(message); end
            if numel(message) > obj.maxMsg
                message = [message(1:obj.maxMsg) '...[truncated]'];
            end
            Row = struct('ts', io.chlog.ChLogger.tsString(obj.clockFcn()), 'level', level, 'node', obj.node, 'process', obj.process, ...
                'pid', obj.pid, 'tid', [], 'run_id', obj.runId, 'seq', obj.seq, 'perf_us', [], ...
                'perf_op', logger_to_char(o.PerfOp), 'subsystem', logger_to_char(o.Subsystem), 'logger', logger_to_char(o.Logger), ...
                'event_code', o.EventCode, 'corr_id', logger_to_char(o.CorrId), 'env', obj.cfg.client.env, ...
                'build', obj.cfg.client.build, 'src', o.Src, 'message', message, 'attrs', io.chlog.ChLogger.attrsJson(o.Attrs));
            if ~isempty(o.PerfUs), Row.perf_us = round(o.PerfUs); end
            obj.seq = obj.seq + 1;
            obj.nBuf = obj.nBuf + 1;
            obj.lines{obj.nBuf} = io.chlog.ChLogger.rowLine(Row);
            obj.levels{obj.nBuf} = level;
        end

        % ---- sending ------------------------------------------------------------------------------------
        function sendBatch(obj, ls, lv)
            nowMs = obj.clockFcn();
            if obj.degraded && nowMs < obj.retryAtMs
                obj.spool(ls, lv);
                return;
            end
            [st, err, rej] = obj.sendIsolating(ls, '');
            obj.rejectedRows = obj.rejectedRows + rej;
            if strcmp(st, 'ok')
                obj.connected();
                obj.sentRows = obj.sentRows + numel(ls) - rej;
            else
                obj.enterDegraded(err);
                obj.spool(ls, lv);
            end
        end

        function [status, err, rejected] = sendIsolating(obj, ls, token)
            % Send; if the adapter says the DATA is bad, bisect to find the bad row(s) and put them in spool/rejected.
            [status, err] = obj.callAdapter(ls, token);
            rejected = 0;
            if strcmp(status, 'ok') || strcmp(status, 'retry')
                return;
            end
            if numel(ls) == 1
                obj.rejectSink(ls{1}, err);
                status = 'ok'; err = ''; rejected = 1;
                return;
            end
            mid = floor(numel(ls) / 2);
            [s1, e1, r1] = obj.sendIsolating(ls(1:mid), logger_subtoken(token, 'a'));
            if strcmp(s1, 'retry'), status = 'retry'; err = e1; rejected = r1; return; end
            [s2, e2, r2] = obj.sendIsolating(ls(mid + 1:end), logger_subtoken(token, 'b'));
            status = s2; err = e2; rejected = r1 + r2;
        end

        function [status, err] = callAdapter(obj, ls, token)
            status = 'retry'; err = '';
            try
                rows = obj.formatRows(ls);
                if obj.nIn >= 3, args = {rows, token, obj.cfg}; else, args = {rows, token}; end
                rej = false;
                if obj.nOut >= 3
                    [ok, err, rej] = obj.insertFcn(args{:});
                else
                    [ok, err] = obj.insertFcn(args{:});
                end
                if isempty(err), err = ''; end
                if ok
                    status = 'ok';
                elseif rej
                    status = 'reject';
                end
            catch ex
                status = 'retry';
                err = ['adapter error: ' ex.message];
            end
        end

        function rows = formatRows(obj, ls)
            switch obj.rowFormat
                case 'jsonlines'
                    rows = [strjoin(ls, char(10)) char(10)];
                case {'struct', 'table'}
                    rows = logger_lines_to_struct(ls);
                    if strcmp(obj.rowFormat, 'table'), rows = struct2table(rows); end
                otherwise
                    error('chlog:rowFormat', 'cfg.matlab.rowFormat must be struct, table or jsonlines');
            end
        end

        function connected(obj)
            obj.degraded = false;
            obj.backoffS = obj.retryMinS;
        end

        function enterDegraded(obj, err)
            obj.lastError = err;
            if ~obj.degraded
                obj.backoffS = obj.retryMinS;
            else
                obj.backoffS = min(obj.backoffS * 2, obj.retryMaxS);
            end
            obj.degraded = true;
            obj.retryAtMs = obj.clockFcn() + obj.backoffS * 1000;
        end

        function rejectSink(obj, line, err)
            d = fullfile(obj.spoolDir, 'rejected');
            if ~exist(d, 'dir'), mkdir(d); end
            Who = logger_safe_name(sprintf('%s_%s_%d', obj.node, obj.process, obj.pid));
            logger_append_text(fullfile(d, [Who '.jsonl']), [line char(10)]);
            err = strrep(err, char(10), ' ');
            logger_append_text(fullfile(d, [Who '.errors.log']), [err(1:min(end, 400)) char(10)]);
        end

        % ---- spool ----------------------------------------------------------------------------------------
        function spool(obj, ls, lv)
            % Append rows to this session's spool file. When the node's spool is full, TRC/DBG go first.
            total = obj.spoolBytes();
            keep = true(1, numel(ls));
            if total >= obj.spoolMaxBytes
                keep = ~ismember(lv, {'TRC', 'DBG'});
            end
            if total >= obj.spoolMaxBytes * 1.25
                keep = false(1, numel(ls));
            end
            obj.spoolDropped = obj.spoolDropped + sum(~keep);
            ls = ls(keep);
            if isempty(ls), return; end
            if ~isempty(obj.curPath) && obj.curBytes >= obj.segmentBytes
                obj.curPath = '';
            end
            if isempty(obj.curPath)
                obj.segN = obj.segN + 1;
                obj.curPath = fullfile(obj.spoolDir, sprintf('%s_%d_%s.%d.jsonl', obj.prefix, obj.pid, obj.runId, obj.segN));
                obj.curBytes = 0;
            end
            text = [strjoin(ls, char(10)) char(10)];
            logger_append_text(obj.curPath, text);
            obj.curBytes = obj.curBytes + numel(text);
            obj.spooledRows = obj.spooledRows + numel(ls);
        end

        function n = spoolBytes(obj)
            % Size of all spool segments in the directory (any process of this node), like the other clients.
            n = 0;
            d = dir(fullfile(obj.spoolDir, '*.jsonl*'));
            for k = 1:numel(d)
                if ~d(k).isdir && ~isempty(regexp(d(k).name, '_\d+_[0-9a-fA-F-]{36}\.\d+\.jsonl', 'once'))
                    n = n + d(k).bytes;
                end
            end
        end

        function tf = hasSpool(obj)
            tf = ~isempty(obj.curPath) || ~isempty(obj.ownSegments());
        end

        function paths = ownSegments(obj)
            % Closed spool files of this session, oldest first.
            d = dir(fullfile(obj.spoolDir, sprintf('%s_%d_%s.*.jsonl', obj.prefix, obj.pid, obj.runId)));
            ns = zeros(1, numel(d)); paths = cell(1, numel(d));
            for k = 1:numel(d)
                paths{k} = fullfile(obj.spoolDir, d(k).name);
                tok = regexp(d(k).name, '\.(\d+)\.jsonl$', 'tokens', 'once');
                if ~isempty(tok), ns(k) = str2double(tok{1}); end
            end
            [~, order] = sort(ns);
            paths = paths(order);
        end

        function maintain(obj, maxChunks)
            if obj.degraded && obj.clockFcn() < obj.retryAtMs, return; end
            if obj.hasSpool(), obj.replayOwn(maxChunks); end
        end

        function replayOwn(obj, maxChunks)
            obj.curPath = ''; obj.curBytes = 0;          % close the current segment so it can be replayed
            paths = obj.ownSegments();
            for k = 1:numel(paths)
                if maxChunks <= 0, return; end
                [sent, rejected, finished, err, used] = obj.replaySegment(paths{k}, maxChunks);
                obj.replayedRows = obj.replayedRows + sent;
                obj.rejectedRows = obj.rejectedRows + rejected;
                if ~isempty(err), obj.enterDegraded(err); return; end
                obj.connected();
                if ~finished, return; end
                maxChunks = maxChunks - used;
            end
        end

        function [sent, rejected, finished, err, chunks] = replaySegment(obj, path, maxChunks)
            % Deliver a closed segment in chunks, oldest rows first. The byte offset of delivered data is saved
            % in <segment>.off after every accepted chunk, so a restart resumes where it stopped.
            sent = 0; rejected = 0; finished = false; err = ''; chunks = 0;
            [~, fname, ext] = fileparts(path);
            base = [fname ext];
            offPath = [path '.off'];
            startOff = logger_read_offset(offPath);
            fid = fopen(path, 'r');
            if fid < 0, err = ['cannot open ' path]; return; end
            fseek(fid, startOff, 'bof');
            data = fread(fid, Inf, '*uint8')';
            fclose(fid);
            nl = find(data == 10);                       % complete lines only: a torn last line is ignored
            pos = 0; li = 1;
            while true
                if chunks >= maxChunks, return; end
                if li > numel(nl), break; end
                last = min(li + obj.chunkRows - 1, numel(nl));
                chunkOffset = startOff + pos;            % where this chunk starts in the file: part of its token
                ls = cell(1, 0);
                for j = li:last
                    from = pos + 1;
                    to = nl(j) - 1;
                    txt = char(data(from:to));
                    if ~isempty(txt) && txt(end) == 13, txt(end) = []; end
                    if ~isempty(txt), ls{end + 1} = txt; end %#ok<AGROW>
                    pos = nl(j);
                end
                token = sprintf('%s:%.0f:%d', base, chunkOffset, obj.chunkRows);
                rej = 0;
                if ~isempty(ls)
                    [st, e, rej] = obj.sendIsolating(ls, token);
                    if strcmp(st, 'retry'), err = e; return; end
                end
                logger_write_offset(offPath, startOff + pos);
                sent = sent + numel(ls) - rej;
                rejected = rejected + rej;
                chunks = chunks + 1;
                li = last + 1;
            end
            delete(path);
            if exist(offPath, 'file'), delete(offPath); end
            finished = true;
        end

        function emitMarkers(obj)
            % One marker row per kind of loss, so loss is never silent (docs/DESIGN.md 5.4).
            if obj.degraded, return; end
            cur = [obj.spoolDropped obj.rejectedRows];
            specs = {1, 'spool was full, TRC/DBG rows dropped', 9001, 'spool_dropped'; ...
                     2, 'server rejected rows (see spool/rejected)', 9003, 'rejected'};
            for k = 1:size(specs, 1)
                i = specs{k, 1};
                if cur(i) > obj.markerReported(i)
                    a = struct(specs{k, 4}, cur(i) - obj.markerReported(i));
                    o = struct('PerfUs', [], 'PerfOp', '', 'CorrId', '', 'Subsystem', 'logging', 'Logger', 'chlog', ...
                               'EventCode', specs{k, 3}, 'Attrs', a, 'Src', '');
                    obj.append('WRN', specs{k, 2}, o, true);
                end
            end
            obj.markerReported = cur;
        end
    end
end

% ---------------------------------------------------------------------------------------------------------------
function Fcn = logger_resolve_insert(Mode)
% Map config insertFcn to a function handle (http, custom handle, or legacy string).
if isa(Mode, 'function_handle')
    Fcn = Mode;
    return;
end
Key = lower(strrep(char(Mode), 'chlog.', ''));
switch Key
    case {'http', 'insert_http', 'chlogger.inserthttp', 'logger.inserthttp'}
        Fcn = @io.chlog.ChLogger.insertHttp;
    case {'native', 'insert_native', 'chlogger.insertnative', 'logger.insertnative'}
        error('io.chlog:config', 'native insert is not supported in AstroPack; use http or a custom InsertFcn');
    otherwise
        Fcn = str2func(char(Mode));
end
end


function Base = logger_merge_struct(Base, Extra)
if ~isstruct(Extra), return; end
Names = fieldnames(Extra);
for K = 1:numel(Names)
    N = Names{K};
    if isfield(Base, N) && isstruct(Base.(N)) && isstruct(Extra.(N))
        Base.(N) = logger_merge_struct(Base.(N), Extra.(N));
    else
        Base.(N) = Extra.(N);
    end
end
end


function Loaded = logger_load_chlogger_yaml_sections(Path)
% Merge clickhouse, client, and matlab from ChLogger.yaml into a config struct.
Block = logger_read_chlogger_yaml_block(Path);
Loaded = struct();
Sections = {'clickhouse', 'client', 'matlab'};
for K = 1:numel(Sections)
    Sec = Sections{K};
    if isfield(Block, Sec) && ~isempty(Block.(Sec))
        Loaded.(Sec) = logger_yaml_struct_to_matlab(Block.(Sec));
    end
end
end


function Block = logger_read_chlogger_yaml_block(Path)
% Read the ChLogger root key from a YAML file (AstroPack config layout).
logger_ensure_yaml_path();
if ~isfile(Path)
    error('io.chlog:config', 'config file not found: %s', Path);
end
Raw = yaml.ReadYaml(char(Path));
if isstruct(Raw) && isfield(Raw, 'ChLogger')
    Block = logger_yaml_struct_to_matlab(Raw.ChLogger);
else
    error('io.chlog:config', 'YAML %s must contain a ChLogger: root key', Path);
end
end


function S = logger_yaml_struct_to_matlab(Y)
% Recursively convert yaml.ReadYaml struct values to plain MATLAB types.
if ~isstruct(Y)
    if isstring(Y)
        S = char(Y);
    else
        S = Y;
    end
    return;
end
Names = fieldnames(Y);
S = struct();
for K = 1:numel(Names)
    N = Names{K};
    V = Y.(N);
    if isstruct(V) && numel(V) == 1
        S.(N) = logger_yaml_struct_to_matlab(V);
    elseif isstring(V)
        S.(N) = char(V);
    else
        S.(N) = V;
    end
end
end


function logger_ensure_yaml_path()
% Add matlab/external so yaml.ReadYaml is available outside MsgLogger ctor.
persistent Added
if isempty(Added)
    Here = fileparts(mfilename('fullpath'));
    ExternalPath = fullfile(Here, '..', '..', '..', 'external');
    if isfolder(ExternalPath)
        addpath(ExternalPath);
    end
    Added = true;
end
end


function T = logger_wire_num(V)
if isempty(V), T = 'null'; else, T = sprintf('%.0f', V); end
end


function Out = logger_utf8_to_code_points(B)
Out = zeros(1, numel(B));
N = 0; I = 1; Len = numel(B);
Cont = @(K) K <= Len && B(K) >= 128 && B(K) < 192;
while I <= Len
    C = B(I); Cp = 65533; Adv = 1;
    if C < 128
        Cp = C;
    elseif C >= 194 && C <= 223 && Cont(I + 1)
        Cp = (C - 192) * 64 + (B(I + 1) - 128); Adv = 2;
    elseif C >= 224 && C <= 239 && Cont(I + 1) && Cont(I + 2)
        T = (C - 224) * 4096 + (B(I + 1) - 128) * 64 + (B(I + 2) - 128);
        if T >= 2048 && ~(T >= 55296 && T <= 57343), Cp = T; Adv = 3; end
    elseif C >= 240 && C <= 244 && Cont(I + 1) && Cont(I + 2) && Cont(I + 3)
        T = (C - 240) * 262144 + (B(I + 1) - 128) * 4096 + (B(I + 2) - 128) * 64 + (B(I + 3) - 128);
        if T >= 65536 && T <= 1114111, Cp = T; Adv = 4; end
    end
    N = N + 1; Out(N) = Cp; I = I + Adv;
end
Out = Out(1:N);
end


function Tf = logger_is_internal_frame(Name)
Internal = {'ChLogger', 'Logger', 'Client', 'init', 'log', 'config', 'close', 'flush', 'replay', 'stats', ...
    'perf', 'is_valid', 'env_helpers', 'ts_string', 'row_line', 'attrs_json', ...
    'json_str', 'json_units', 'insert_mock', 'insert_http', 'insert_native', 'insert_template'};
Tf = any(strcmp(Name, Internal));
end


function S = logger_config_path_suffix(Path)
% Short path fragment for config warning messages (empty Path -> empty suffix).
if nargin < 1 || isempty(Path)
    S = '';
else
    S = sprintf(': %s', Path);
end
end


function V = logger_pick(varargin)
% First non-empty string wins; whitespace-only counts as empty.
V = '';
for K = 1:nargin
    X = varargin{K};
    if ischar(X) || (isstring(X) && isscalar(X))
        X = strtrim(char(X));
    end
    if ~isempty(X), V = X; return; end
end
end


function S = logger_to_char(V)
if isempty(V), S = ''; elseif ischar(V), S = V; else, S = num2str(V); end
end


function Lvl = logger_norm_level(Level)
Lvl = upper(strtrim(logger_to_char(Level)));
Alias = {'TRACE', 'TRC'; 'DEBUG', 'DBG'; 'INFO', 'INF'; 'WARN', 'WRN'; 'WARNING', 'WRN'; 'ERROR', 'ERR'; 'CRITICAL', 'CRT'; 'FATAL', 'CRT'};
I = find(strcmp(Lvl, Alias(:, 1)), 1);
if ~isempty(I), Lvl = Alias{I, 2}; end
if ~ismember(Lvl, {'TRC', 'DBG', 'INF', 'WRN', 'ERR', 'CRT'}), Lvl = 'INF'; end
end


function Tf = logger_is_urgent(Level)
Tf = any(strcmp(Level, {'ERR', 'CRT'}));
end


function S = logger_safe_name(Text)
S = regexprep(char(Text), '[^A-Za-z0-9._\-]', '-');
if isempty(S), S = 'x'; end
end


function T = logger_subtoken(Token, Suffix)
if isempty(Token), T = ''; else, T = [Token '/' Suffix]; end
end


function P = logger_absolute_path(P)
if isempty(regexp(P, '^([A-Za-z]:[\\/]|[\\/]|\\\\)', 'once'))
    P = fullfile(pwd, P);
end
P = regexprep(P, '[\\/]\.([\\/]|$)', filesep);
end


function S = logger_caller_src()
S = '';
try
    St = dbstack('-completenames');
    for K = 1:numel(St)
        N = St(K).file;
        [~, Name] = fileparts(N);
        if ~logger_is_internal_frame(Name)
            S = sprintf('%s.m:%d', Name, St(K).line);
            return;
        end
    end
catch
end
end


function logger_append_text(Path, Text)
Fid = fopen(Path, 'ab');
if Fid < 0, error('chlog:spool', 'cannot open %s for writing', Path); end
C = onCleanup(@() fclose(Fid));
fwrite(Fid, uint8(Text), 'uint8');
clear C
end


function Off = logger_read_offset(Path)
Off = 0;
Fid = fopen(Path, 'r');
if Fid < 0, return; end
T = fread(Fid, Inf, '*char')';
fclose(Fid);
V = str2double(strtrim(T));
if ~isnan(V) && V >= 0, Off = V; end
end


function logger_write_offset(Path, Off)
Tmp = [Path '.tmp'];
Fid = fopen(Tmp, 'w');
if Fid < 0, error('chlog:spool', 'cannot write %s', Tmp); end
fprintf(Fid, '%.0f', Off);
fclose(Fid);
movefile(Tmp, Path, 'f');
end


function Rows = logger_lines_to_struct(Ls)
N = numel(Ls);
for K = 1:N
    R = jsondecode(Ls{K});
    if isfield(R, 'attrs') && isstruct(R.attrs), R.attrs = jsonencode(R.attrs); end
    if K == 1
        Rows = repmat(R, 1, N);
    else
        Rows(K) = R; %#ok<AGROW>
    end
end
end
