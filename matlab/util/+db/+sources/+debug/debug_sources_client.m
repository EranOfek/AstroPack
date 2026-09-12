%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debug_sources_client.m
% Author      : Chen Tishler
% Created     : 17/08/2026
% Updated     : 09/09/2026
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

    C = db.sources.debug.debugConstants_();
    logFile = db.sources.debug.debugSetupLog_('matlab_smoke');

    db.sources.debug.debug_log('section', 'SourcesClient debug (MATLAB)', logFile);

    baseUrl = db.sources.debug.debugResolveBaseUrl_(logFile, true);
    db.sources.debug.debug_log('info', sprintf('US_BASE_URL=%s', baseUrl), logFile);
    db.sources.debug.debug_log('info', sprintf('ASTROPACK_DATA_PATH=%s', tools.os.getAstroPackDataPath()), logFile);
    db.sources.debug.debug_log('info', sprintf('log_file=%s', logFile), logFile);

    client = db.sources.debug.debugCreateClient_(baseUrl, logFile, C.SmokeClientTimeout);
    health = db.sources.debug.debugCheckHealth_(client, baseUrl, requireHealth, logFile, 'smoke');
    if health.abort
        return;
    end

    db.sources.debug.debug_log('section', 'Outbox (before insert)', logFile);
    db.sources.debug.debugOutboxStatus_(logFile);

    db.sources.debug.debug_log('section', 'Build sources table', logFile);
    tbl = buildSmokeTable_();
    db.sources.debug.debug_log('ok', sprintf('%d rows, %d columns', height(tbl), width(tbl)), logFile);

    db.sources.debug.debug_log('section', 'Insert sources', logFile);
    requestId = sprintf('debug-matlab-smoke-%s', datestr(now, 'yyyymmddTHHMMSS'));
    outcome = 'FAILED';
    try
        resp = client.insertSources(tbl, 'RequestId', requestId);
        [outcome, ~] = db.sources.debug.debugHandleInsertResponse_(client, resp, requestId, logFile, ...
            'Mode', 'smoke', 'DoWait', true, ...
            'WaitTimeout', C.SmokeWaitTimeout, 'PollInterval', C.PollInterval, ...
            'ApiReachable', health.reachable);
    catch ME
        db.sources.debug.debug_log('err', sprintf('insertSources failed: %s', ME.message), logFile);
        outcome = 'FAILED';
    end

    db.sources.debug.debug_log('section', 'Summary', logFile);
    if health.reachable
        db.sources.debug.debug_log('info', sprintf('API was reachable at start: yes  outcome: %s', outcome), logFile);
    else
        db.sources.debug.debug_log('info', sprintf('API was reachable at start: no   outcome: %s', outcome), logFile);
    end
end


function tbl = buildSmokeTable_()
%BUILDSMOKETABLE_  Minimal 3-row table for smoke insert.

    n = 3;
    ra = [100.0; 100.1; 100.2];
    dec = [30.0; 30.1; 30.2];
    magnitude = single([15.0; 15.5; 16.0]);
    flux = single([100; 110; 120]);
    flags = uint32([0; 0; 0]);
    timestamp = int64(repmat(1700000000, n, 1));
    tbl = table(ra, dec, magnitude, flux, flags, timestamp);
end
