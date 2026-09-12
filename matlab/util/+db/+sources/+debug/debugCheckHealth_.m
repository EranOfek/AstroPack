%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debugCheckHealth_.m
% Author      : Chen Tishler
% Created     : 09/09/2026
% Description : Best-effort health probe for debug walkthrough scripts.
%==========================================================================

function health = debugCheckHealth_(client, baseUrl, requireHealth, logFile, style)
%DEBUGCHECKHEALTH_  Probe API health; honor RequireHealth strict mode.
%
%   health = db.sources.debug.debugCheckHealth_(client, baseUrl, false, logFile, 'smoke')
%   Returns struct with fields .reachable and .abort (true => caller should return).

    arguments
        client
        baseUrl (1,:) char
        requireHealth (1,1) logical
        logFile (1,:) char = ''
        style (1,:) char {mustBeMember(style, {'smoke', 'continuous'})} = 'smoke'
    end

    health = struct('reachable', false, 'abort', false);

    db.sources.debug.debug_log('section', 'Health', logFile);
    try
        H = client.health();
        health.reachable = true;
        db.sources.debug.debug_log('ok', sprintf('API reachable: %s', jsonencode(H)), logFile);
    catch ME
        if requireHealth
            db.sources.debug.debug_log('err', sprintf('Health failed (RequireHealth=true): %s', ME.message), logFile);
            if strcmp(style, 'smoke')
                db.sources.debug.debug_log('section', 'Summary', logFile);
                db.sources.debug.debug_log('err', 'FAILED — API health required but unreachable', logFile);
            end
            health.abort = true;
            return;
        end

        if strcmp(style, 'smoke')
            db.sources.debug.debug_log('warn', sprintf(['API not reachable at %s — continuing. ' ...
                'insertSources will queue to outbox if still down.'], baseUrl), logFile);
        else
            db.sources.debug.debug_log('warn', sprintf(['API not reachable at %s — continuing. ' ...
                'Each insert will queue to outbox while API is down.'], baseUrl), logFile);
        end
        db.sources.debug.debug_log('info', sprintf('health error: %s', ME.message), logFile);
    end
end
