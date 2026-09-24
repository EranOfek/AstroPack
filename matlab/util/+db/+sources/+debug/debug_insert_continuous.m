%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debug_insert_continuous.m
% Author      : Chen Tishler
% Created     : 19/08/2026
% Updated     : 09/09/2026
% Description : Continuous realistic insert — 1000 sources every N seconds via API.
%==========================================================================

function debug_insert_continuous(varargin)
%DEBUG_INSERT_CONTINUOUS  Submit realistic visit batches on a timer via MATLAB API client.
%
%   Health is best-effort: when API is down, batches queue to outbox instead of aborting.
%   Does not stop or interfere with Python continuous inserter.
%
%   db.sources.debug.debug_insert_continuous()
%   db.sources.debug.debug_insert_continuous('IntervalSec', 60, 'Rows', 1000, 'MaxRounds', 3)
%   db.sources.debug.debug_insert_continuous('MaxRounds', 0)  % until Ctrl+C
%   db.sources.debug.debug_insert_continuous('RequireHealth', true)  % strict / CI
%
%   request_id prefix: debug-matlab-continuous-NNNN-<hex>
%   Runtime data under $ASTROPACK_DATA_PATH/sources/debug/

    db.sources.debug.ensurePath_();

    p = inputParser;
    addParameter(p, 'IntervalSec', 60.0, @isnumeric);
    addParameter(p, 'Rows', 1000, @isnumeric);
    addParameter(p, 'Fields', 8, @isnumeric);
    addParameter(p, 'MaxRounds', 0, @isnumeric);
    addParameter(p, 'Wait', true, @islogical);
    addParameter(p, 'Seed', 1042, @isnumeric);
    addParameter(p, 'RequireHealth', false, @islogical);
    parse(p, varargin{:});

    intervalSec = p.Results.IntervalSec;
    rows = p.Results.Rows;
    nFields = p.Results.Fields;
    maxRounds = p.Results.MaxRounds;
    doWait = p.Results.Wait;
    seed = p.Results.Seed;
    requireHealth = p.Results.RequireHealth;

    C = db.sources.debug.debugConstants_();
    dataDir = db.sources.debug.debugDataRoot_();
    tmpDir = fullfile(dataDir, 'tmp');
    stateFile = fullfile(dataDir, 'continuous', 'catalog.json');
    if ~isfolder(tmpDir)
        mkdir(tmpDir);
    end
    logFile = db.sources.debug.debugSetupLog_('matlab_continuous');

    db.sources.debug.debug_log('section', 'Continuous realistic insert (MATLAB)', logFile);
    db.sources.debug.debug_log('ok', sprintf('interval=%.0fs rows=%d fields=%d', intervalSec, rows, nFields), logFile);
    if maxRounds == 0
        db.sources.debug.debug_log('ok', 'max_rounds=unlimited (Ctrl+C to stop)', logFile);
    else
        db.sources.debug.debug_log('ok', sprintf('max_rounds=%d', maxRounds), logFile);
    end
    db.sources.debug.debug_log('ok', sprintf('state_file=%s', stateFile), logFile);

    baseUrl = db.sources.debug.debugResolveBaseUrl_(logFile, false);
    db.sources.debug.debug_log('info', sprintf('US_BASE_URL=%s', baseUrl), logFile);
    db.sources.debug.debug_log('info', sprintf('ASTROPACK_DATA_PATH=%s', tools.os.getAstroPackDataPath()), logFile);

    client = db.sources.debug.debugCreateClient_(baseUrl, logFile, C.ContinuousClientTimeout);
    health = db.sources.debug.debugCheckHealth_(client, baseUrl, requireHealth, logFile, 'continuous');
    if health.abort
        return;
    end
    apiReachable = health.reachable;

    db.sources.debug.debug_log('section', 'Outbox (startup)', logFile);
    db.sources.debug.debugOutboxStatus_(logFile);

    state = db.sources.debug.debug_realistic_batch('load', stateFile, 'Seed', seed, 'Fields', nFields);
    if numel(state.fields) ~= nFields
        state.fields = db.sources.debug.debug_realistic_batch('defaultFields', nFields);
    end

    roundIdx = state.round_idx;
    roundsDone = 0;
    cleanupObj = onCleanup(@() saveContinuousStateOnExit_(stateFile, state, roundIdx, logFile)); %#ok<NASGU>

    try
        while maxRounds == 0 || roundsDone < maxRounds
            [roundIdx, state, apiReachable] = runContinuousRound_( ...
                client, state, roundIdx, rows, tmpDir, stateFile, logFile, C, ...
                doWait, apiReachable);
            roundsDone = roundsDone + 1;

            if maxRounds == 0 || roundsDone < maxRounds
                db.sources.debug.debug_log('ok', sprintf('sleep %.0fs until next batch (Ctrl+C to stop)', ...
                    intervalSec), logFile);
                pause(intervalSec);
            end
        end

        if maxRounds > 0
            db.sources.debug.debug_log('ok', sprintf('Finished %d round(s)', maxRounds), logFile);
        end

        db.sources.debug.debug_log('section', 'Outbox (shutdown)', logFile);
        summary = db.sources.debug.debugOutboxStatus_(logFile);
        if summary.pendingCount > 0
            db.sources.debug.debug_log('info', ['Pending batches remain — start API and run ' ...
                'client.health() or client.flushPending() to replay'], logFile);
        end

    catch ME
        if strcmp(ME.identifier, 'MATLAB:interruption')
            db.sources.debug.debug_log('warn', 'Interrupted — saving catalog state', logFile);
            state.round_idx = roundIdx;
            db.sources.debug.debug_realistic_batch('save', stateFile, state);
            db.sources.debug.debug_log('ok', sprintf('Stopped after round %d (Ctrl+C)', roundIdx), logFile);
        else
            db.sources.debug.debug_log('err', sprintf('Continuous insert failed: %s', ME.message), logFile);
            rethrow(ME);
        end
    end
end


function [roundIdx, state, apiReachable] = runContinuousRound_( ...
    client, state, roundIdx, rows, tmpDir, stateFile, logFile, C, doWait, apiReachable)
%RUNCONTINUOUSROUND_  Simulate one visit batch, upload parquet, insert, save state.

    sky = state.fields(mod(roundIdx, numel(state.fields)) + 1);
    [visitIndex, state] = db.sources.debug.debug_realistic_batch('bumpVisit', state, sky.name);

    db.sources.debug.debug_log('section', sprintf('Round %d: field=%s visit_index=%d', ...
        roundIdx + 1, sky.name, visitIndex), logFile);

    if ~apiReachable
        db.sources.debug.debug_log('info', 'API still offline — batch will queue locally if unreachable', logFile);
    end

    [tbl, state] = db.sources.debug.debug_realistic_batch('simulate', state, sky, ...
        'Rows', rows, 'VisitIndex', visitIndex);
    db.sources.debug.debug_log('ok', sprintf('generated %d rows (first_visit=%d)', ...
        height(tbl), visitIndex == 0), logFile);

    pqName = sprintf('matlab_continuous_%04d.parquet', roundIdx + 1);
    pqPath = fullfile(tmpDir, pqName);
    parquetwrite(pqPath, tbl);
    db.sources.debug.debug_log('ok', sprintf('wrote parquet %s (%d bytes)', pqPath, ...
        dir(pqPath).bytes), logFile);

    hexSuffix = dec2hex(randi(intmax('uint32'), 1, 1, 'uint32'), 8);
    reqId = sprintf('debug-matlab-continuous-%04d-%s', roundIdx + 1, hexSuffix);
    db.sources.debug.debug_log('ok', sprintf('request_id=%s', reqId), logFile);

    resp = client.insertParquetFile(pqPath, 'RequestId', reqId, 'OriginalName', pqName);
    [~, apiReachable] = db.sources.debug.debugHandleInsertResponse_(client, resp, reqId, logFile, ...
        'Mode', 'continuous', 'DoWait', doWait, ...
        'WaitTimeout', C.ContinuousWaitTimeout, 'PollInterval', C.PollInterval, ...
        'ApiReachable', apiReachable);

    roundIdx = roundIdx + 1;
    state.round_idx = roundIdx;
    db.sources.debug.debug_realistic_batch('save', stateFile, state);
end


function saveContinuousStateOnExit_(stateFile, state, roundIdx, logFile)
%SAVECONTINUOUSSTATEONEXIT_  onCleanup handler — persist catalog state on exit.

    state.round_idx = roundIdx;
    try
        db.sources.debug.debug_realistic_batch('save', stateFile, state);
        db.sources.debug.debug_log('ok', 'Catalog state saved on exit', logFile);
    catch
    end
end
