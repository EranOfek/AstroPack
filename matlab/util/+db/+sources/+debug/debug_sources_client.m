%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debug_sources_client.m
% Author      : Chen Tishler
% Created     : 17/08/2026
% Updated     : 07/09/2026
% Description : HTTP client debug — health + small insert + waitJob or offline queue.
%==========================================================================

function debug_sources_client(varargin)
%DEBUG_SOURCES_CLIENT  Console walkthrough for db.sources.SourcesClient.
%
%   Flow: config → health (best-effort) → insert → waitJob OR offline queue.
%   When the API is down, health warns but insert still runs and queues to outbox.
%
%   db.sources.debug.debug_sources_client()
%   db.sources.debug.debug_sources_client('RequireHealth', true)  % strict / CI
%
%   Env: US_BASE_URL, US_API_KEY, ASTROPACK_DATA_PATH (optional).

    db.sources.debug.ensurePath_();

    p = inputParser;
    addParameter(p, 'RequireHealth', false, @islogical);
    parse(p, varargin{:});
    requireHealth = p.Results.RequireHealth;

    % Log file under ASTROPACK_DATA_PATH/sources/debug/logs/.
    dataRoot = db.sources.debug.debugDataRoot_();
    logDir = fullfile(dataRoot, 'logs');
    if ~isfolder(logDir)
        mkdir(logDir);
    end
    logFile = fullfile(logDir, sprintf('matlab_smoke_%s.log', datestr(now, 'yyyymmdd')));

    db.sources.debug.debug_log('section', 'SourcesClient debug (MATLAB)', logFile);

    % Resolve API base URL (env or default localhost).
    base = getenv('US_BASE_URL');
    if isempty(base)
        base = 'http://127.0.0.1:8151';
        db.sources.debug.debug_log('warn', sprintf('US_BASE_URL not set — using %s', base), logFile);
    end

    astroDataPath = tools.os.getAstroPackDataPath();
    db.sources.debug.debug_log('info', sprintf('US_BASE_URL=%s', base), logFile);
    db.sources.debug.debug_log('info', sprintf('ASTROPACK_DATA_PATH=%s', astroDataPath), logFile);
    db.sources.debug.debug_log('info', sprintf('log_file=%s', logFile), logFile);

    % Create client with verbose logging to file.
    client = db.sources.SourcesClient(base, '', 120.0);
    client.Verbose = true;
    client.LogFile = logFile;

    % Health is best-effort: probes API and flushes outbox when reachable.
    db.sources.debug.debug_log('section', 'Health', logFile);
    apiReachable = false;
    try
        h = client.health();
        apiReachable = true;
        db.sources.debug.debug_log('ok', sprintf('API reachable: %s', jsonencode(h)), logFile);
    catch ME
        if requireHealth
            db.sources.debug.debug_log('err', sprintf('Health failed (RequireHealth=true): %s', ME.message), logFile);
            db.sources.debug.debug_log('section', 'Summary', logFile);
            db.sources.debug.debug_log('err', 'FAILED — API health required but unreachable', logFile);
            return;
        end
        db.sources.debug.debug_log('warn', sprintf(['API not reachable at %s — continuing. ' ...
            'insertSources will queue to outbox if still down.'], base), logFile);
        db.sources.debug.debug_log('info', sprintf('health error: %s', ME.message), logFile);
    end

    db.sources.debug.debug_log('section', 'Outbox (before insert)', logFile);
    db.sources.debug.debug_outboxStatus_(logFile);

    % Build minimal 3-row sources table for smoke insert.
    db.sources.debug.debug_log('section', 'Build sources table', logFile);
    n = 3;
    ra = [100.0; 100.1; 100.2];
    dec = [30.0; 30.1; 30.2];
    magnitude = single([15.0; 15.5; 16.0]);
    flux = single([100; 110; 120]);
    flags = uint32([0; 0; 0]);
    timestamp = int64(repmat(1700000000, n, 1));
    tbl = table(ra, dec, magnitude, flux, flags, timestamp);
    db.sources.debug.debug_log('ok', sprintf('%d rows, %d columns', height(tbl), width(tbl)), logFile);

    % Submit insert job: live job_id, offline queue, or error.
    db.sources.debug.debug_log('section', 'Insert sources', logFile);
    requestId = sprintf('debug-matlab-smoke-%s', datestr(now, 'yyyymmddTHHMMSS'));
    outcome = 'FAILED';
    try
        resp = client.insertSources(tbl, 'RequestId', requestId);
        if isfield(resp, 'queued') && resp.queued
            db.sources.debug.debug_log('ok', sprintf(['Insert queued offline (success). ' ...
                'request_id=%s'], requestId), logFile);
            db.sources.debug.debug_log('info', 'No job_id yet — batch saved under outbox/pending/', logFile);
            db.sources.debug.debug_log('section', 'Outbox (after insert)', logFile);
            summary = db.sources.debug.debug_outboxStatus_(logFile);
            db.sources.debug.debug_log('info', ['When API + manager are running again, replay with: ' ...
                'client = db.sources.SourcesClient(); client.health();  % or client.flushPending()'], logFile);
            outcome = sprintf('QUEUED OFFLINE (%d pending)', summary.pendingCount);
        else
            db.sources.debug.debug_log('ok', jsonencode(resp), logFile);
            db.sources.debug.debug_log('section', 'Wait for job', logFile);
            job = client.waitJob(resp.job_id, 'PollInterval', 2, 'Timeout', 600);
            if isfield(job, 'result') && ~isempty(job.result)
                r = job.result;
                db.sources.debug.debug_log('ok', sprintf('n_new=%d n_unchanged=%d n_sources=%d', ...
                    r.n_new, r.n_unchanged, r.n_sources), logFile);
            end
            outcome = 'LIVE OK';
        end
    catch ME
        db.sources.debug.debug_log('err', sprintf('insertSources failed: %s', ME.message), logFile);
        outcome = 'FAILED';
    end

    db.sources.debug.debug_log('section', 'Summary', logFile);
    if apiReachable
        db.sources.debug.debug_log('info', sprintf('API was reachable at start: yes  outcome: %s', outcome), logFile);
    else
        db.sources.debug.debug_log('info', sprintf('API was reachable at start: no   outcome: %s', outcome), logFile);
    end
end
