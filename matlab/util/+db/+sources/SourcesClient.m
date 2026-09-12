%==========================================================================
% Project     : AstroPack
% File        : db.sources.SourcesClient.m
% Author      : Chen Tishler
% Created     : 07/09/2026
% Updated     : 09/09/2026
% Description : HTTP client for Unique Sources FastAPI service (Layer 1).
%==========================================================================

classdef SourcesClient < handle
%SOURCESCLIENT  HTTP client for Unique Sources FastAPI service (Layer 1).
%
%   Pipeline users submit detection tables for association via insert/match jobs.
%   The service writes the sources (detection) table and the unique-sources catalog.
%   Defaults read US_BASE_URL and US_API_KEY from the environment.
%
%   Offline outbox: when the service is unreachable, mutating requests are persisted
%   under $ASTROPACK_DATA_PATH/sources/outbox/pending/ and replayed automatically
%   on the next insert/match/health or via flushPending().
%
%   QUICK START:
%     client = db.sources.SourcesClient();
%     client.Verbose = true;
%     client.health();
%     tbl = table(ra, dec, magnitude, flux, flags, timestamp, ...
%         'VariableNames', {'ra','dec','magnitude','flux','flags','timestamp'});
%     resp = client.insertSources(tbl, 'RequestId', 'visit-raw123');
%     if isfield(resp, 'queued') && resp.queued
%         % persisted locally; no job_id yet
%     else
%         job = client.waitJob(resp.job_id);
%     end
%
%   See also: db.sources.debug.debug_sources_client

    properties
        Verbose (1,1) logical = false
        LogFile (1,:) char = ''
    end


    properties (Access = private)
        base_url_ (1,:) char = 'http://127.0.0.1:8151'
        api_key_  (1,:) char = ''
        timeout_  (1,1) double = 120.0
    end


    methods
        function obj = SourcesClient(base_url, api_key, timeout)
            % SourcesClient  Construct client from args or US_* environment variables.
            %
            % Example:
            %   client = db.sources.SourcesClient();

            % Base URL from argument or US_BASE_URL env var (supports aliases local, euclid).
            if nargin >= 1 && ~isempty(base_url)
                obj.base_url_ = obj.strip_slash_(obj.resolveBaseUrl_(base_url));
            else
                envUrl = getenv('US_BASE_URL');
                if ~isempty(envUrl)
                    obj.base_url_ = obj.strip_slash_(obj.resolveBaseUrl_(envUrl));
                end
            end

            % API key from argument or US_API_KEY env var.
            if nargin >= 2
                obj.api_key_ = api_key;
            else
                obj.api_key_ = getenv('US_API_KEY');
            end

            % HTTP timeout override (seconds).
            if nargin >= 3
                obj.timeout_ = timeout;
            end
        end


        function resp = health(obj)
            % health  GET /health and auto-flush pending outbox entries.
            %
            % Example:
            %   resp = client.health();

            % Log and call the health endpoint.
            obj.log_('GET /health');
            resp = obj.do_get_('/health');
            obj.log_(sprintf('health: %s', jsonencode(resp)));

            % Replay any requests queued while the service was down.
            obj.flushPending();
        end


        function resp = getConfig(obj)
            % getConfig  GET /api/config service configuration.
            %
            % Example:
            %   cfg = client.getConfig();

            resp = obj.do_get_('/api/config');
        end


        function stats = flushPending(obj)
            % flushPending  Replay pending outbox requests (FIFO).
            %
            % Example:
            %   stats = client.flushPending();

            % Initialize counters returned to caller.
            stats = struct('sent', 0, 'failed', 0, 'stopped', false);
            pendingRoot = obj.pendingDir_();
            if ~isfolder(pendingRoot)
                return;
            end

            % List pending folders in FIFO order (timestamp prefix in folder name).
            entries = dir(pendingRoot);
            names = {entries([entries.isdir] & ~ismember({entries.name}, {'.', '..'})).name};
            names = sort(names);

            % Replay each pending entry until transport failure stops the drain.
            for i = 1:numel(names)
                pendingPath = fullfile(pendingRoot, names{i});
                try
                    obj.replayPendingDir_(pendingPath);
                    obj.removeDir_(pendingPath);
                    stats.sent = stats.sent + 1;
                catch ME
                    if obj.isTransportError_(ME)
                        obj.log_(sprintf('flushPending stopped (transport): %s', ME.message));
                        stats.stopped = true;
                        return;
                    end
                    if obj.isDuplicateError_(ME)
                        obj.log_(sprintf('flushPending duplicate treated as success: %s', names{i}));
                        obj.removeDir_(pendingPath);
                        stats.sent = stats.sent + 1;
                        continue;
                    end
                    obj.movePendingToFailed_(pendingPath);
                    stats.failed = stats.failed + 1;
                    obj.log_(sprintf('flushPending failed entry %s: %s', names{i}, ME.message));
                end
            end
        end


        function resp = insertParquetFile(obj, pqPath, varargin)
            % insertParquetFile  POST existing Parquet file to insert/upload.
            %
            % Example:
            %   resp = client.insertParquetFile(pqPath, 'RequestId', 'my-id');

            % Drain outbox before accepting a new insert.
            obj.flushPending();

            % Parse optional table/domain parameters.
            p = obj.parseInsertParams_(varargin{:});
            reqId = char(p.Results.RequestId);
            if isempty(reqId)
                error('SourcesClient:MissingRequestId', ...
                    'RequestId is required for idempotency');
            end
            if ~isfile(pqPath)
                error('SourcesClient:FileNotFound', ...
                    'Parquet file not found: %s', pqPath);
            end

            % Write-ahead outbox then POST (or queue on transport failure).
            obj.log_(sprintf('insertParquetFile request_id=%s path=%s', reqId, pqPath));
            t0 = tic;
            resp = obj.submitInsert_(pqPath, reqId, p.Results);
            elapsedMs = toc(t0) * 1000.0;
            if isfield(resp, 'queued') && resp.queued
                obj.log_(sprintf('insert queued in %.1f ms request_id=%s', elapsedMs, reqId));
            else
                obj.log_(sprintf('upload done in %.1f ms job_id=%s', elapsedMs, resp.job_id));
            end
        end


        function resp = insertSources(obj, srcTable, varargin)
            % insertSources  Write table to Parquet and POST insert/upload job.
            %
            % Example:
            %   resp = client.insertSources(tbl, 'RequestId', 'visit-001');

            % Drain outbox before accepting a new insert.
            obj.flushPending();

            % Parse optional table/domain parameters.
            p = obj.parseInsertParams_(varargin{:});
            reqId = char(p.Results.RequestId);
            if isempty(reqId)
                error('SourcesClient:MissingRequestId', ...
                    'RequestId is required for idempotency');
            end

            % Normalize table for Parquet (ClickHouse-style names, safe types).
            UploadTable = obj.prepareTableForParquet_(srcTable, p.Results.LowercaseColumns);

            % Write table to a temp Parquet file, submit, then remove temp dir.
            tmpDir = tempname;
            mkdir(tmpDir);
            pqPath = fullfile(tmpDir, char(p.Results.OriginalName));
            try
                parquetwrite(pqPath, UploadTable);
                resp = obj.submitInsert_(pqPath, reqId, p.Results);
            catch ME
                if exist(tmpDir, 'dir')
                    rmdir(tmpDir, 's');
                end
                rethrow(ME);
            end
            if exist(tmpDir, 'dir')
                rmdir(tmpDir, 's');
            end
        end


        function resp = matchSources(obj, varargin)
            % matchSources  Submit match/backfill job for id_uniq_src = 0 rows.
            %
            % Example:
            %   resp = client.matchSources('RequestId', 'match-001');

            % Drain outbox before accepting a new match job.
            obj.flushPending();

            % Parse match job parameters.
            p = inputParser;
            addParameter(p, 'RequestId', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Domain', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Detection', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'DetectionTable', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'UniqueTable', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'MaxRows', [], @(x) isempty(x) || isnumeric(x));
            parse(p, varargin{:});
            reqId = char(p.Results.RequestId);
            if isempty(reqId)
                error('SourcesClient:MissingRequestId', 'RequestId is required');
            end

            % Build JSON body with only non-empty optional fields.
            body = struct('request_id', reqId);
            if ~isempty(p.Results.Domain)
                body.domain = char(p.Results.Domain);
            end
            if ~isempty(p.Results.Detection)
                body.detection = char(p.Results.Detection);
            end
            if ~isempty(p.Results.DetectionTable)
                body.detection_table = char(p.Results.DetectionTable);
            end
            if ~isempty(p.Results.UniqueTable)
                body.unique_table = char(p.Results.UniqueTable);
            end
            if ~isempty(p.Results.MaxRows)
                body.max_rows = p.Results.MaxRows;
            end
            resp = obj.submitMatch_(body);
        end


        function resp = getJob(obj, jobId)
            % getJob  GET /api/jobs/{id} job status snapshot.
            %
            % Example:
            %   job = client.getJob(resp.job_id);

            path = sprintf('/api/jobs/%s', char(jobId));
            resp = obj.do_get_(path);
        end


        function job = waitJob(obj, jobId, varargin)
            % waitJob  Poll GET /api/jobs/{id} until done or failed.
            %
            % Example:
            %   job = client.waitJob(jobId, 'PollInterval', 2, 'Timeout', 900);

            % waitJob requires a live job_id (not an offline queued response).
            if isempty(jobId)
                error('SourcesClient:MissingJobId', ...
                    'job_id is empty — request may have been queued offline');
            end

            % Parse poll interval and overall timeout.
            p = inputParser;
            addParameter(p, 'PollInterval', 2.0, @isnumeric);
            addParameter(p, 'Timeout', 900.0, @isnumeric);
            parse(p, varargin{:});
            pollSec = p.Results.PollInterval;
            timeoutSec = p.Results.Timeout;
            jobIdStr = char(jobId);
            obj.log_(sprintf('waitJob job_id=%s timeout=%.0fs poll=%.1fs', ...
                jobIdStr, timeoutSec, pollSec));

            % Poll until terminal status or timeout.
            t0 = tic;
            while toc(t0) < timeoutSec
                job = obj.getJob(jobIdStr);
                status = char(job.status);
                if any(strcmp(status, {'done', 'failed'}))
                    if strcmp(status, 'failed')
                        errDetail = '';
                        if isfield(job, 'error') && ~isempty(job.error)
                            errDetail = char(job.error);
                        end
                        error('SourcesClient:JobFailed', ...
                            'Job %s failed: %s', jobIdStr, errDetail);
                    end
                    if isfield(job, 'result') && ~isempty(job.result)
                        r = job.result;
                        obj.log_(sprintf(['job done: n_sources=%d n_new=%d n_unchanged=%d ' ...
                            'n_changed=%d elapsed_ms=%.0f'], ...
                            r.n_sources, r.n_new, r.n_unchanged, r.n_changed, r.elapsed_ms));
                    else
                        obj.log_(sprintf('job done status=%s', status));
                    end
                    return;
                end
                pause(pollSec);
            end
            error('SourcesClient:JobTimeout', ...
                'Timeout waiting for job %s after %.0f s', jobIdStr, timeoutSec);
        end
    end


    methods (Access = private)
        function p = parseInsertParams_(obj, varargin) %#ok<INUSD>
            % parseInsertParams_  Shared inputParser for insertSources / insertParquetFile.

            p = inputParser;
            addParameter(p, 'RequestId', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Domain', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Detection', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'DetectionTable', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'UniqueTable', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'DetectionAs', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'UniqueAs', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'OriginalName', 'sources.parquet', @(x) ischar(x) || isstring(x));
            addParameter(p, 'LowercaseColumns', true, @islogical);
            parse(p, varargin{:});
        end


        function T = prepareTableForParquet_(obj, srcTable, lowercaseColumns) %#ok<INUSD>
            % prepareTableForParquet_  Coerce types and optionally lowercase names for upload.
            %
            %   LAST archive tables from imProc.db.insertCatalog may use uppercase
            %   names and uint64 IDs; Parquet/ClickHouse expect lowercase snake_case.

            T = srcTable;
            if isempty(T) || height(T) == 0
                return;
            end

            if lowercaseColumns
                T.Properties.VariableNames = lower(T.Properties.VariableNames);
            end

            VarNames = T.Properties.VariableNames;
            for i = 1:numel(VarNames)
                Col = T.(VarNames{i});
                T.(VarNames{i}) = obj.coerceColumnForParquet_(Col);
            end
        end


        function Col = coerceColumnForParquet_(obj, Col) %#ok<INUSD>
            % coerceColumnForParquet_  Map MATLAB column types to Parquet-safe types.

            if iscell(Col)
                if all(cellfun(@(x) ischar(x) || isstring(x) || (isscalar(x) && isnan(x)), Col))
                    Col = string(Col);
                end
                return;
            end

            if isstring(Col) || ischar(Col)
                if ischar(Col)
                    Col = string(Col);
                end
                return;
            end

            if isdatetime(Col)
                Col = int64(posixtime(Col));
                return;
            end

            if isduration(Col)
                Col = seconds(Col);
                return;
            end

            if islogical(Col)
                Col = int8(Col);
                return;
            end

            cls = class(Col);
            switch cls
                case 'uint64'
                    maxVal = intmax('int64');
                    if all(Col <= maxVal, 'all')
                        Col = int64(Col);
                    else
                        Col = double(Col);
                    end
                case {'uint32', 'uint16', 'uint8'}
                    Col = int64(Col);
                case 'single'
                    % keep single for reduced smoke tables; double is safer for mixed LAST cols
                    if any(isinf(Col) | isnan(Col))
                        Col = double(Col);
                    end
            end
        end


        function resp = submitInsert_(obj, pqPath, requestId, params)
            % submitInsert_  Write-ahead outbox insert then POST or queue offline.

            % Persist Parquet + meta.json before attempting HTTP.
            pendingPath = obj.createPendingInsert_(pqPath, requestId, params);
            try
                resp = obj.replayPendingDir_(pendingPath);
                obj.removeDir_(pendingPath);
            catch ME
                if obj.isTransportError_(ME)
                    obj.log_(sprintf('queued insert request_id=%s: %s', requestId, ME.message));
                    resp = obj.queuedResponse_(requestId);
                    return;
                end
                if obj.isDuplicateError_(ME)
                    obj.log_(sprintf('duplicate insert request_id=%s treated as success', requestId));
                    obj.removeDir_(pendingPath);
                    resp = struct('request_id', requestId, 'queued', false, ...
                        'job_id', '', 'duplicate', true);
                    return;
                end
                obj.movePendingToFailed_(pendingPath);
                rethrow(ME);
            end
        end


        function resp = submitMatch_(obj, body)
            % submitMatch_  Write-ahead outbox match then POST or queue offline.

            requestId = body.request_id;

            % Persist meta.json before attempting HTTP.
            pendingPath = obj.createPendingMatch_(body);
            try
                resp = obj.replayPendingDir_(pendingPath);
                obj.removeDir_(pendingPath);
            catch ME
                if obj.isTransportError_(ME)
                    obj.log_(sprintf('queued match request_id=%s: %s', requestId, ME.message));
                    resp = obj.queuedResponse_(requestId);
                    return;
                end
                if obj.isDuplicateError_(ME)
                    obj.log_(sprintf('duplicate match request_id=%s treated as success', requestId));
                    obj.removeDir_(pendingPath);
                    resp = struct('request_id', requestId, 'queued', false, ...
                        'job_id', '', 'duplicate', true);
                    return;
                end
                obj.movePendingToFailed_(pendingPath);
                rethrow(ME);
            end
        end


        function pendingPath = createPendingInsert_(obj, pqPath, requestId, params)
            % createPendingInsert_  Create pending folder with payload.parquet and meta.json.

            obj.ensureOutboxDirs_();
            folderName = obj.pendingFolderName_(requestId);
            pendingPath = fullfile(obj.pendingDir_(), folderName);
            if isfolder(pendingPath)
                error('SourcesClient:PendingExists', ...
                    'Pending outbox entry already exists for request_id=%s', requestId);
            end
            mkdir(pendingPath);

            % Copy Parquet into outbox (skip copy when source is already there).
            destPq = fullfile(pendingPath, 'payload.parquet');
            if ~strcmp(pqPath, destPq)
                copyfile(pqPath, destPq);
            end

            % Write sidecar metadata for replay.
            meta = obj.insertMetaStruct_(requestId, params);
            meta.op = 'insert';
            obj.writeMetaJson_(pendingPath, meta);
        end


        function pendingPath = createPendingMatch_(obj, body)
            % createPendingMatch_  Create pending folder with match meta.json only.

            obj.ensureOutboxDirs_();
            requestId = body.request_id;
            folderName = obj.pendingFolderName_(requestId);
            pendingPath = fullfile(obj.pendingDir_(), folderName);
            if isfolder(pendingPath)
                error('SourcesClient:PendingExists', ...
                    'Pending outbox entry already exists for request_id=%s', requestId);
            end
            mkdir(pendingPath);

            % Match jobs store JSON body in meta.json (no Parquet payload).
            meta = body;
            meta.op = 'match';
            obj.writeMetaJson_(pendingPath, meta);
        end


        function resp = replayPendingDir_(obj, pendingPath)
            % replayPendingDir_  POST one pending outbox entry to the service.

            metaPath = fullfile(pendingPath, 'meta.json');
            if ~isfile(metaPath)
                error('SourcesClient:BadPending', 'Missing meta.json in %s', pendingPath);
            end
            meta = jsondecode(fileread(metaPath));
            if ~isfield(meta, 'op')
                error('SourcesClient:BadPending', 'meta.json missing op in %s', pendingPath);
            end

            % Dispatch insert (multipart) or match (JSON POST) by op field.
            op = char(meta.op);
            switch op
                case 'insert'
                    pqPath = fullfile(pendingPath, 'payload.parquet');
                    if ~isfile(pqPath)
                        error('SourcesClient:BadPending', 'Missing payload.parquet in %s', pendingPath);
                    end
                    params = obj.metaToInsertParams_(meta);
                    resp = obj.insertSourcesParquetFile_(pqPath, meta.request_id, params);
                case 'match'
                    body = obj.metaToMatchBody_(meta);
                    resp = obj.do_post_('/api/jobs/match', body);
                otherwise
                    error('SourcesClient:BadPending', 'Unknown op %s in %s', op, pendingPath);
            end
        end


        function meta = insertMetaStruct_(obj, requestId, params) %#ok<INUSD>
            % insertMetaStruct_  Build meta.json struct for an insert outbox entry.

            meta = struct();
            meta.request_id = char(requestId);
            meta.domain = char(params.Domain);
            meta.detection = char(params.Detection);
            meta.detection_table = char(params.DetectionTable);
            meta.unique_table = char(params.UniqueTable);
            meta.detection_as = char(params.DetectionAs);
            meta.unique_as = char(params.UniqueAs);
            meta.original_name = char(params.OriginalName);
        end


        function params = metaToInsertParams_(obj, meta) %#ok<INUSD>
            % metaToInsertParams_  Restore insertParams struct from meta.json.

            params = struct();
            params.Domain = '';
            params.Detection = '';
            params.DetectionTable = '';
            params.UniqueTable = '';
            params.DetectionAs = '';
            params.UniqueAs = '';
            params.OriginalName = 'sources.parquet';
            if isfield(meta, 'domain'), params.Domain = char(meta.domain); end
            if isfield(meta, 'detection'), params.Detection = char(meta.detection); end
            if isfield(meta, 'detection_table'), params.DetectionTable = char(meta.detection_table); end
            if isfield(meta, 'unique_table'), params.UniqueTable = char(meta.unique_table); end
            if isfield(meta, 'detection_as'), params.DetectionAs = char(meta.detection_as); end
            if isfield(meta, 'unique_as'), params.UniqueAs = char(meta.unique_as); end
            if isfield(meta, 'original_name'), params.OriginalName = char(meta.original_name); end
        end


        function body = metaToMatchBody_(obj, meta) %#ok<INUSD>
            % metaToMatchBody_  Restore match JSON body from meta.json.

            body = struct('request_id', char(meta.request_id));
            if isfield(meta, 'domain') && ~isempty(meta.domain)
                body.domain = char(meta.domain);
            end
            if isfield(meta, 'detection') && ~isempty(meta.detection)
                body.detection = char(meta.detection);
            end
            if isfield(meta, 'detection_table') && ~isempty(meta.detection_table)
                body.detection_table = char(meta.detection_table);
            end
            if isfield(meta, 'unique_table') && ~isempty(meta.unique_table)
                body.unique_table = char(meta.unique_table);
            end
            if isfield(meta, 'max_rows') && ~isempty(meta.max_rows)
                body.max_rows = meta.max_rows;
            end
        end


        function writeMetaJson_(obj, pendingPath, meta) %#ok<INUSD>
            % writeMetaJson_  Atomically write meta.json into a pending folder.

            metaPath = fullfile(pendingPath, 'meta.json');
            fid = fopen(metaPath, 'w');
            if fid < 0
                error('SourcesClient:OutboxWriteFailed', 'Cannot write %s', metaPath);
            end
            fprintf(fid, '%s', jsonencode(meta));
            fclose(fid);
        end


        function resp = insertSourcesParquetFile_(obj, pqPath, requestId, params)
            % insertSourcesParquetFile_  Multipart POST /api/jobs/insert/upload.

            import matlab.net.http.io.FileProvider
            import matlab.net.http.io.MultipartFormProvider

            % Build multipart form: request_id, file, optional table/domain fields.
            args = {'request_id', string(requestId), 'file', FileProvider(pqPath)};
            if ~isempty(params.Domain)
                args = [args, {'domain', string(params.Domain)}]; %#ok<AGROW>
            end
            if ~isempty(params.Detection)
                args = [args, {'detection', string(params.Detection)}]; %#ok<AGROW>
            end
            if ~isempty(params.DetectionTable)
                args = [args, {'detection_table', string(params.DetectionTable)}]; %#ok<AGROW>
            end
            if ~isempty(params.UniqueTable)
                args = [args, {'unique_table', string(params.UniqueTable)}]; %#ok<AGROW>
            end
            if ~isempty(params.DetectionAs)
                args = [args, {'detection_as', string(params.DetectionAs)}]; %#ok<AGROW>
            end
            if ~isempty(params.UniqueAs)
                args = [args, {'unique_as', string(params.UniqueAs)}]; %#ok<AGROW>
            end

            % Send POST with API key and configured timeouts.
            provider = MultipartFormProvider(args{:});
            uri = matlab.net.URI([obj.base_url_, '/api/jobs/insert/upload']);
            req = matlab.net.http.RequestMessage('POST', [], provider);
            req = obj.add_api_key_(req);
            opts = matlab.net.http.HTTPOptions('ConnectTimeout', obj.timeout_, ...
                'ResponseTimeout', obj.timeout_);
            respMsg = obj.sendRequest_(req, uri, opts);
            obj.check_response_(respMsg);
            resp = respMsg.Body.Data;
        end


        function respMsg = sendRequest_(obj, req, uri, opts) %#ok<INUSD>
            % sendRequest_  Low-level matlab.net.http send wrapper.

            respMsg = req.send(uri, opts);
        end


        function resp = do_get_(obj, path)
            % do_get_  Authenticated GET to base_url + path.

            uri = matlab.net.URI([obj.base_url_, path]);
            req = matlab.net.http.RequestMessage('GET');
            req = obj.add_api_key_(req);
            opts = matlab.net.http.HTTPOptions('ConnectTimeout', obj.timeout_, ...
                'ResponseTimeout', obj.timeout_);
            respMsg = obj.sendRequest_(req, uri, opts);
            obj.check_response_(respMsg);
            resp = respMsg.Body.Data;
        end


        function resp = do_post_(obj, path, body)
            % do_post_  Authenticated JSON POST to base_url + path.

            uri = matlab.net.URI([obj.base_url_, path]);
            req = matlab.net.http.RequestMessage('POST', ...
                matlab.net.http.field.ContentTypeField('application/json'), body);
            req = obj.add_api_key_(req);
            opts = matlab.net.http.HTTPOptions('ConnectTimeout', obj.timeout_, ...
                'ResponseTimeout', obj.timeout_);
            respMsg = obj.sendRequest_(req, uri, opts);
            obj.check_response_(respMsg);
            resp = respMsg.Body.Data;
        end


        function req = add_api_key_(obj, req)
            % add_api_key_  Append api-key header when US_API_KEY is set.

            if ~isempty(obj.api_key_)
                req.Header(end+1) = matlab.net.http.field.GenericField('api-key', obj.api_key_);
            end
        end


        function check_response_(obj, respMsg) %#ok<INUSD>
            % check_response_  Throw SourcesClient:HttpError on non-2xx status.

            code = respMsg.StatusCode;
            okCodes = [matlab.net.http.StatusCode.OK, ...
                matlab.net.http.StatusCode.Created, ...
                matlab.net.http.StatusCode(202)];
            if ~any(code == okCodes)
                if ~isempty(respMsg.Body) && ~isempty(respMsg.Body.Data)
                    errMsg = jsonencode(respMsg.Body.Data);
                else
                    errMsg = char(respMsg.StatusLine);
                end
                error('SourcesClient:HttpError', '%s', errMsg);
            end
        end


        function tf = isTransportError_(obj, ME) %#ok<INUSD>
            % isTransportError_  True when failure is offline/transport (not HTTP 4xx/5xx).

            tf = ~strcmp(ME.identifier, 'SourcesClient:HttpError');
        end


        function tf = isDuplicateError_(obj, ME) %#ok<INUSD>
            % isDuplicateError_  True when service reports idempotent duplicate request_id.

            msg = lower(ME.message);
            patterns = {'duplicate', 'already exists', 'already processed', 'conflict', '409'};
            tf = false;
            for i = 1:numel(patterns)
                if contains(msg, patterns{i})
                    tf = true;
                    return;
                end
            end
        end


        function resp = queuedResponse_(obj, requestId) %#ok<INUSD>
            % queuedResponse_  Struct returned when request persisted offline.

            resp = struct('request_id', char(requestId), 'queued', true, 'job_id', '');
        end


        function root = outboxRoot_(obj) %#ok<INUSD>
            % outboxRoot_  $ASTROPACK_DATA_PATH/sources/outbox

            dataRoot = tools.os.getAstroPackDataPath();
            root = fullfile(dataRoot, 'sources', 'outbox');
        end


        function dirPath = pendingDir_(obj)
            % pendingDir_  Path to pending/ subfolder under outbox root.

            dirPath = fullfile(obj.outboxRoot_(), 'pending');
        end


        function dirPath = failedDir_(obj)
            % failedDir_  Path to failed/ subfolder under outbox root.

            dirPath = fullfile(obj.outboxRoot_(), 'failed');
        end


        function ensureOutboxDirs_(obj)
            % ensureOutboxDirs_  Create pending/ and failed/ if missing.

            pending = obj.pendingDir_();
            failed = obj.failedDir_();
            if ~isfolder(pending)
                mkdir(pending);
            end
            if ~isfolder(failed)
                mkdir(failed);
            end
        end


        function name = pendingFolderName_(obj, requestId) %#ok<INUSD>
            % pendingFolderName_  Timestamp + sanitized request_id folder name.

            ts = datestr(now, 'yyyymmddTHHMMSS');
            safeId = regexprep(char(requestId), '[^a-zA-Z0-9._-]', '_');
            if numel(safeId) > 80
                safeId = safeId(1:80);
            end
            name = sprintf('%s_%s', ts, safeId);
        end


        function movePendingToFailed_(obj, pendingPath)
            % movePendingToFailed_  Relocate pending entry after non-transport HTTP error.

            obj.ensureOutboxDirs_();
            [~, folderName] = fileparts(pendingPath);
            dest = fullfile(obj.failedDir_(), folderName);
            if isfolder(dest)
                dest = fullfile(obj.failedDir_(), [folderName '_' datestr(now, 'HHMMSSFFF')]);
            end
            movefile(pendingPath, dest);
        end


        function removeDir_(obj, dirPath) %#ok<INUSD>
            % removeDir_  Delete outbox folder after successful replay.

            if isfolder(dirPath)
                rmdir(dirPath, 's');
            end
        end


        function s = strip_slash_(obj, url) %#ok<INUSD>
            % strip_slash_  Remove trailing slash from base URL string.

            s = regexprep(strtrim(char(url)), '/$', '');
        end


        function url = resolveBaseUrl_(obj, value) %#ok<INUSD>
            % resolveBaseUrl_  Expand named hosts (local, euclid) to full URLs.

            key = lower(strtrim(char(value)));
            switch key
                case {'local', 'localhost'}
                    url = 'http://127.0.0.1:8151';
                case 'euclid'
                    url = 'http://euclid/unique-sources';
                otherwise
                    url = char(value);
            end
        end


        function log_(obj, msg)
            % log_  Write timestamped line to Verbose console and/or LogFile.

            if ~obj.Verbose && isempty(obj.LogFile)
                return;
            end
            ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
            line = sprintf('[%s] SourcesClient: %s\n', ts, msg);
            if obj.Verbose
                fprintf('%s', line);
            end
            if ~isempty(obj.LogFile)
                fid = fopen(obj.LogFile, 'a');
                if fid >= 0
                    fprintf(fid, '%s', line);
                    fclose(fid);
                end
            end
        end
    end
end
