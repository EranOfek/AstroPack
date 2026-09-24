%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debugHandleInsertResponse_.m
% Author      : Chen Tishler
% Created     : 09/09/2026
% Description : Unified insert response handling for debug walkthrough scripts.
%==========================================================================

function [outcome, apiReachable] = debugHandleInsertResponse_(client, resp, requestId, logFile, Args)
%DEBUGHANDLEINSERTRESPONSE_  Log and wait on insert/parquet upload response.
%
%   [outcome, apiReachable] = debugHandleInsertResponse_(client, resp, reqId, logFile, ...
%       'Mode', 'smoke', 'DoWait', true, 'WaitTimeout', 600, 'ApiReachable', false);

    arguments
        client
        resp
        requestId (1,:) char
        logFile (1,:) char = ''
        Args.DoWait (1,1) logical = true
        Args.WaitTimeout (1,1) double = 600.0
        Args.PollInterval (1,1) double = 2.0
        Args.Mode (1,:) char {mustBeMember(Args.Mode, {'smoke', 'continuous'})} = 'smoke'
        Args.ApiReachable (1,1) logical = false
    end

    outcome = 'FAILED';
    apiReachable = Args.ApiReachable;

    if isfield(resp, 'queued') && resp.queued
        if strcmp(Args.Mode, 'smoke')
            db.sources.debug.debug_log('ok', sprintf(['Insert queued offline (success). ' ...
                'request_id=%s'], requestId), logFile);
            db.sources.debug.debug_log('info', 'No job_id yet — batch saved under outbox/pending/', logFile);
            db.sources.debug.debug_log('section', 'Outbox (after insert)', logFile);
            summary = db.sources.debug.debugOutboxStatus_(logFile);
            db.sources.debug.debug_log('info', ['When API + manager are running again, replay with: ' ...
                'client = db.sources.SourcesClient(); client.health();  % or client.flushPending()'], logFile);
            outcome = sprintf('QUEUED OFFLINE (%d pending)', summary.pendingCount);
        else
            db.sources.debug.debug_log('ok', sprintf('Insert queued offline (success) request_id=%s', requestId), logFile);
            db.sources.debug.debugOutboxStatus_(logFile);
            outcome = 'QUEUED OFFLINE';
        end
        return;
    end

    if strcmp(Args.Mode, 'smoke')
        db.sources.debug.debug_log('ok', jsonencode(resp), logFile);
    end

    if ~Args.DoWait
        outcome = 'SUBMITTED';
        return;
    end

    if strcmp(Args.Mode, 'smoke')
        db.sources.debug.debug_log('section', 'Wait for job', logFile);
    end

    job = client.waitJob(resp.job_id, 'PollInterval', Args.PollInterval, 'Timeout', Args.WaitTimeout);
    if isfield(job, 'result') && ~isempty(job.result)
        R = job.result;
        if strcmp(Args.Mode, 'smoke')
            db.sources.debug.debug_log('ok', sprintf('n_new=%d n_unchanged=%d n_sources=%d', ...
                R.n_new, R.n_unchanged, R.n_sources), logFile);
        else
            db.sources.debug.debug_log('ok', sprintf(['result n_new=%d n_unchanged=%d n_changed=%d ' ...
                'detections=%d unique_inserted=%d'], ...
                R.n_new, R.n_unchanged, R.n_changed, ...
                R.all_sources_inserted, R.unique_sources_inserted), logFile);
        end
    end

    apiReachable = true;
    if strcmp(Args.Mode, 'smoke')
        outcome = 'LIVE OK';
    else
        outcome = 'LIVE OK';
    end
end
