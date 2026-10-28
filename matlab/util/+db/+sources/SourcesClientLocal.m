%% SYNCED from astro-clickhouse — edit SoT, then re-run sync
%==========================================================================
% Project     : Object Matcher Service
% File        : client_matlab/SourcesClientLocal.m
% Author      : Chen Tishler
% Created     : 21/10/2026
% Updated     : 21/10/2026
% Description : Local filedrop client for Object Matcher insert jobs (Route B).
%==========================================================================

classdef SourcesClientLocal < handle
%SOURCESCLIENTLOCAL  Enqueue insert jobs on Euclid NVMe without HTTP.
%
%   Writes staged Parquet, incoming manifest, and pending job JSON matching
%   object_matcher service/jobstore.py. Fire-and-forget: no waitJob or outbox.
%
%   client = db.sources.SourcesClientLocal();
%   resp = client.insertSources(tbl, 'RequestId', 'last-proc_src-visit1', ...
%       'Domain', 'last', 'DetectionTable', 'last.proc_src', 'Family', 'proc');
%
%   Environment: OM_DATA_DIR, OM_JOBS_DIR (optional OM pending via PendingDir).

    properties
        Verbose (1,1) logical = false
        LogFile (1,:) char = ''
        Perf (1,1) logical = false
    end


    properties (Access = private)
        data_dir_ (1,:) char = ''
        jobs_dir_ (1,:) char = ''
        pending_dir_ (1,:) char = ''
        backend_ (1,:) char = 'filedrop'
    end


    methods
        function obj = SourcesClientLocal(varargin)
            % SourcesClientLocal  Load paths from env and optional name-value args.
            %
            %   Name-value: DataDir, JobsDir, PendingDir, Backend ('filedrop'|'shm').

            p = inputParser;
            addParameter(p, 'DataDir', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'JobsDir', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'PendingDir', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Backend', 'filedrop', @(x) ischar(x) || isstring(x));
            parse(p, varargin{:});

            obj.backend_ = lower(strtrim(char(p.Results.Backend)));
            if strcmp(obj.backend_, 'shm')
                error('SourcesClientLocal:BackendNotReady', ...
                    'Backend ''shm'' is not implemented yet; use ''filedrop''.');
            end
            if ~strcmp(obj.backend_, 'filedrop')
                error('SourcesClientLocal:BadBackend', ...
                    'Backend must be ''filedrop'' or ''shm'' (got ''%s'')', obj.backend_);
            end

            obj.data_dir_ = char(p.Results.DataDir);
            if isempty(obj.data_dir_)
                obj.data_dir_ = getenv('OM_DATA_DIR');
            end
            obj.jobs_dir_ = char(p.Results.JobsDir);
            if isempty(obj.jobs_dir_)
                obj.jobs_dir_ = getenv('OM_JOBS_DIR');
            end
            obj.pending_dir_ = char(p.Results.PendingDir);
            if isempty(obj.pending_dir_) && ~isempty(obj.jobs_dir_)
                obj.pending_dir_ = fullfile(obj.jobs_dir_, 'pending');
            end
        end


        function resp = insertParquetFile(obj, pqPath, varargin)
            % insertParquetFile  Stage an existing Parquet file and enqueue a job.

            p = obj.parseInsertParams_(varargin{:});
            reqId = char(p.Results.RequestId);
            if isempty(reqId)
                error('SourcesClientLocal:MissingRequestId', 'RequestId is required');
            end
            if ~isfile(pqPath)
                error('SourcesClientLocal:FileNotFound', 'Parquet file not found: %s', pqPath);
            end

            obj.ensureDirs_();
            prepMs = 0;
            writeT0 = tic;
            [pqFinal, jobId, submitted, byteSize] = obj.stageParquetCopy_(pqPath, p.Results);
            writeMs = toc(writeT0) * 1000.0;
            handoffT0 = tic;
            resp = obj.finalizeEnqueue_(pqFinal, jobId, submitted, byteSize, reqId, p.Results, 0);
            handoffMs = toc(handoffT0) * 1000.0;
            timing = obj.makeTimingStruct_(prepMs, writeMs, handoffMs, byteSize, 0, 'filedrop');
            resp.timing = timing;
            obj.logPerfInsertLine_(reqId, timing);
        end


        function resp = insertSources(obj, srcTable, varargin)
            % insertSources  Coerce table, write Parquet, enqueue job (no HTTP).

            p = obj.parseInsertParams_(varargin{:});
            reqId = char(p.Results.RequestId);
            if isempty(reqId)
                error('SourcesClientLocal:MissingRequestId', 'RequestId is required');
            end
            obj.ensureDirs_();

            prepT0 = tic;
            uploadTable = db.sources.prepareTable(srcTable, p.Results.LowercaseColumns);
            prepareMs = toc(prepT0) * 1000.0;
            nRows = height(uploadTable);

            writeT0 = tic;
            [pqFinal, jobId, submitted, byteSize] = obj.stageParquetFromTable_(uploadTable, p.Results);
            writeMs = toc(writeT0) * 1000.0;

            handoffT0 = tic;
            resp = obj.finalizeEnqueue_(pqFinal, jobId, submitted, byteSize, reqId, p.Results, nRows);
            handoffMs = toc(handoffT0) * 1000.0;
            timing = obj.makeTimingStruct_(prepareMs, writeMs, handoffMs, byteSize, nRows, 'filedrop');
            resp.timing = timing;
            obj.logPerfInsertLine_(reqId, timing);
        end
    end


    methods (Access = private)
        function p = parseInsertParams_(obj, varargin) %#ok<INUSD>
            % parseInsertParams_  Shared insert name-value parameters.

            p = inputParser;
            addParameter(p, 'RequestId', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Domain', 'last', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Family', 'proc', @(x) ischar(x) || isstring(x));
            addParameter(p, 'Detection', 'proc', @(x) ischar(x) || isstring(x));
            addParameter(p, 'DetectionTable', 'last.proc_src', @(x) ischar(x) || isstring(x));
            addParameter(p, 'UniqueTable', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'OriginalName', 'sources.parquet', @(x) ischar(x) || isstring(x));
            addParameter(p, 'LowercaseColumns', true, @islogical);
            addParameter(p, 'BatchTag', '', @(x) ischar(x) || isstring(x));
            addParameter(p, 'DuplicateCheck', true, @islogical);
            addParameter(p, 'CreateIfMissing', true, @islogical);
            parse(p, varargin{:});
        end


        function ensureDirs_(obj)
            % ensureDirs_  Verify incoming and pending directories exist.

            if isempty(obj.data_dir_) || ~isfolder(obj.data_dir_)
                error('SourcesClientLocal:MissingDataDir', ...
                    'OM_DATA_DIR / DataDir is missing or not a folder: %s', obj.data_dir_);
            end
            if isempty(obj.pending_dir_) || ~isfolder(obj.pending_dir_)
                error('SourcesClientLocal:MissingPendingDir', ...
                    'Pending jobs folder missing: %s', obj.pending_dir_);
            end
        end


        function [pqFinal, jobId, submitted, byteSize] = stageParquetFromTable_(obj, uploadTable, params)
            % stageParquetFromTable_  Write Parquet via temp file then atomic rename.

            submitted = datetime('now', 'TimeZone', 'UTC');
            jobId = obj.newJobId_();
            stem = obj.incomingStem_(jobId, submitted);
            pqFinal = fullfile(obj.data_dir_, [stem '.parquet']);
            pqTmp = [pqFinal '.tmp'];
            tmpTable = fullfile(tempdir, [stem '_write.parquet']);
            try
                parquetwrite(tmpTable, uploadTable);
                copyfile(tmpTable, pqTmp);
                delete(tmpTable);
                obj.atomicMove_(pqTmp, pqFinal);
            catch ME
                if isfile(pqTmp)
                    delete(pqTmp);
                end
                if isfile(tmpTable)
                    delete(tmpTable);
                end
                rethrow(ME);
            end
            finfo = dir(pqFinal);
            byteSize = finfo.bytes;
            obj.log_(sprintf('staged parquet %s (%d bytes)', pqFinal, byteSize));
        end


        function [pqFinal, jobId, submitted, byteSize] = stageParquetCopy_(obj, pqPath, params) %#ok<INUSD>
            % stageParquetCopy_  Copy existing Parquet into incoming/ with canonical name.

            submitted = datetime('now', 'TimeZone', 'UTC');
            jobId = obj.newJobId_();
            stem = obj.incomingStem_(jobId, submitted);
            pqFinal = fullfile(obj.data_dir_, [stem '.parquet']);
            pqTmp = [pqFinal '.tmp'];
            copyfile(pqPath, pqTmp);
            obj.atomicMove_(pqTmp, pqFinal);
            finfo = dir(pqFinal);
            byteSize = finfo.bytes;
            obj.log_(sprintf('copied parquet to %s (%d bytes)', pqFinal, byteSize));
        end


        function resp = finalizeEnqueue_(obj, pqFinal, jobId, submitted, byteSize, requestId, params, nRows)
            % finalizeEnqueue_  Write manifest + pending job JSON; return fire-and-forget struct.

            contentHash = obj.sha256File_(pqFinal);
            manifest = obj.buildManifestStruct_(jobId, submitted, pqFinal, byteSize, contentHash, requestId, params);
            job = obj.buildJobStruct_(jobId, submitted, pqFinal, contentHash, requestId, params);

            stem = obj.incomingStem_(jobId, submitted);
            manifestPath = fullfile(obj.data_dir_, [stem '.json']);
            obj.writeJsonAtomic_(manifestPath, manifest);

            jobName = sprintf('%s_%s.json', obj.isoStamp_(submitted), jobId);
            jobPath = fullfile(obj.pending_dir_, jobName);
            obj.writeJsonAtomic_(jobPath, job);

            resp = struct( ...
                'job_id', jobId, ...
                'request_id', char(requestId), ...
                'content_hash', contentHash, ...
                'parquet_path', pqFinal, ...
                'queued', true, ...
                'rows', nRows);
            obj.log_(sprintf('enqueued job_id=%s request_id=%s', jobId, requestId));
        end


        function manifest = buildManifestStruct_(obj, jobId, submitted, pqPath, byteSize, contentHash, requestId, params) %#ok<INUSD>
            % buildManifestStruct_  IncomingManifest-compatible JSON struct.

            manifest = struct( ...
                'job_id', jobId, ...
                'request_id', char(requestId), ...
                'job_type', 'insert', ...
                'mode', 'path', ...
                'submitted_at', obj.isoDateTime_(submitted), ...
                'original_name', char(params.OriginalName), ...
                'payload_file', char(sprintf('%s.parquet', obj.incomingStem_(jobId, submitted))), ...
                'content_hash', contentHash, ...
                'byte_size', byteSize, ...
                'domain', char(params.Domain), ...
                'family', char(params.Family), ...
                'detection', char(params.Detection), ...
                'detection_table', char(params.DetectionTable), ...
                'create_if_missing', logical(params.CreateIfMissing), ...
                'duplicate_check', logical(params.DuplicateCheck));
            if ~isempty(params.BatchTag)
                manifest.batch_tag = char(params.BatchTag);
            end
            if ~isempty(params.UniqueTable)
                manifest.object_table = char(params.UniqueTable);
            end
        end


        function job = buildJobStruct_(obj, jobId, submitted, pqPath, contentHash, requestId, params) %#ok<INUSD>
            % buildJobStruct_  Pending JobRecord-compatible JSON struct.

            job = struct( ...
                'job_id', jobId, ...
                'status', 'pending', ...
                'mode', 'path', ...
                'job_type', 'insert', ...
                'request_id', char(requestId), ...
                'domain', char(params.Domain), ...
                'family', char(params.Family), ...
                'detection', char(params.Detection), ...
                'detection_table', char(params.DetectionTable), ...
                'parquet_path', char(pqPath), ...
                'original_name', char(params.OriginalName), ...
                'submitted_at', obj.isoDateTime_(submitted), ...
                'content_hash', contentHash, ...
                'duplicate_check', logical(params.DuplicateCheck), ...
                'create_if_missing', logical(params.CreateIfMissing));
            if ~isempty(params.BatchTag)
                job.batch_tag = char(params.BatchTag);
            end
            if ~isempty(params.UniqueTable)
                job.object_table = char(params.UniqueTable);
            end
        end


        function stem = incomingStem_(obj, jobId, submitted) %#ok<INUSD>
            % incomingStem_  Basename shared by parquet and manifest in incoming/.

            stem = sprintf('%s_%s', obj.isoStamp_(submitted), jobId);
        end


        function stamp = isoStamp_(obj, dt) %#ok<INUSD>
            % isoStamp_  Compact UTC stamp for filenames (YYYYMMDDTHHMMSSZ).

            stamp = char(dt, 'yyyyMMdd''T''HHmmss''Z''');
        end


        function s = isoDateTime_(obj, dt) %#ok<INUSD>
            % isoDateTime_  ISO-8601 UTC string for JSON datetime fields.

            s = char(dt, 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''');
        end


        function id = newJobId_(obj) %#ok<INUSD>
            % newJobId_  Random UUID string for job_id.

            id = char(java.util.UUID.randomUUID());
        end


        function hashStr = sha256File_(obj, path)
            % sha256File_  Return sha256:<hex> digest of file bytes.

            fid = fopen(path, 'rb');
            if fid < 0
                error('SourcesClientLocal:ReadFailed', 'Cannot read %s', path);
            end
            data = fread(fid, inf, 'uint8=>uint8');
            fclose(fid);
            md = java.security.MessageDigest.getInstance('SHA-256');
            md.update(data);
            bytes = md.digest();
            hexStr = lower(reshape(dec2hex(typecast(bytes, 'uint8'), 2)', 1, []));
            hashStr = ['sha256:' hexStr];
        end


        function writeJsonAtomic_(obj, destPath, payload)
            % writeJsonAtomic_  Write JSON via .tmp then rename.

            tmpPath = [destPath '.tmp'];
            txt = jsonencode(payload);
            fid = fopen(tmpPath, 'w');
            if fid < 0
                error('SourcesClientLocal:JsonWriteFailed', 'Cannot write %s', tmpPath);
            end
            fprintf(fid, '%s\n', txt);
            fclose(fid);
            obj.atomicMove_(tmpPath, destPath);
        end


        function atomicMove_(obj, src, dest) %#ok<INUSD>
            % atomicMove_  Rename temp artifact to final name.

            if isfile(dest)
                delete(dest);
            end
            movefile(src, dest);
        end


        function timing = makeTimingStruct_(obj, prepareMs, writeMs, handoffMs, byteSize, nRows, outcome) %#ok<INUSD>
            % makeTimingStruct_  Standard resp.timing for local enqueue.

            totalMs = prepareMs + writeMs + handoffMs;
            timing = struct( ...
                'prepare_ms', prepareMs, ...
                'write_ms', writeMs, ...
                'handoff_ms', handoffMs, ...
                'bytes', byteSize, ...
                'rows', nRows, ...
                'total_ms', totalMs, ...
                'outcome', outcome);
        end


        function logPerfInsertLine_(obj, requestId, timing)
            % logPerfInsertLine_  One fixed-key perf line when Perf is enabled.

            if ~obj.Perf
                return;
            end
            writeSec = max(timing.write_ms, 0) / 1000.0;
            if writeSec > 0 && timing.bytes > 0
                writeMbps = timing.bytes / 1e6 / writeSec;
            else
                writeMbps = 0;
            end
            msg = sprintf(['SourcesClientLocal perf request_id=%s backend=%s rows=%d ' ...
                'prepare_ms=%.0f write_ms=%.0f bytes=%d write_MBps=%.1f handoff_ms=%.0f total_ms=%.0f'], ...
                char(requestId), timing.outcome, timing.rows, ...
                timing.prepare_ms, timing.write_ms, timing.bytes, writeMbps, ...
                timing.handoff_ms, timing.total_ms);
            obj.logPerf_(msg);
        end


        function logPerf_(obj, msg)
            % logPerf_  Emit perf line when Perf is true.

            if ~obj.Perf
                return;
            end
            obj.log_(msg, true);
        end


        function log_(obj, msg, forcePrint)
            % log_  Timestamped line to Verbose console and/or LogFile.

            if nargin < 3
                forcePrint = false;
            end
            if ~forcePrint && ~obj.Verbose && isempty(obj.LogFile)
                return;
            end
            ts = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');
            line = sprintf('[%s] SourcesClientLocal: %s\n', ts, msg);
            if obj.Verbose || forcePrint
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
