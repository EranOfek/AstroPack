%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debug_log.m
% Author      : Chen Tishler
% Created     : 19/08/2026
% Updated     : 09/09/2026
% Description : Timestamped console logging for sources debug walkthroughs.
%==========================================================================

function debug_log(action, msg, logFile)
%DEBUG_LOG  Timestamped section / ok / warn / err banners for debug scripts.
%
%   db.sources.debug.debug_log('section', 'Health')
%   db.sources.debug.debug_log('ok', 'Upload complete')
%   db.sources.debug.debug_log('warn', 'Interrupted')
%   db.sources.debug.debug_log('err', 'Job failed')
%   db.sources.debug.debug_log('info', 'Outbox path: ...')
%
%   Optional third arg appends the same line to a log file.

    arguments
        action (1,:) char
        msg (1,:) char
        logFile (1,:) char = ''
    end

    ts = datestr(now, 'yyyy-mm-dd HH:MM:SS');
    switch lower(action)
        case 'section'
            line = sprintf('\n[%s] ========== %s ==========\n', ts, msg);
        case 'ok'
            line = sprintf('[%s] OK  %s\n', ts, msg);
        case 'warn'
            line = sprintf('[%s] WARN %s\n', ts, msg);
        case 'err'
            line = sprintf('[%s] ERR  %s\n', ts, msg);
        case 'info'
            line = sprintf('[%s] INFO %s\n', ts, msg);
        otherwise
            line = sprintf('[%s] %s\n', ts, msg);
    end

    fprintf('%s', line);
    if ~isempty(logFile)
        fid = fopen(logFile, 'a');
        if fid >= 0
            fprintf(fid, '%s', line);
            fclose(fid);
        end
    end
end
