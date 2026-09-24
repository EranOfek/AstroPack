%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debugSetupLog_.m
% Author      : Chen Tishler
% Created     : 09/09/2026
% Description : Create daily log file under sources/debug/logs.
%==========================================================================

function logFile = debugSetupLog_(logBasename)
%DEBUGSETUPLOG_  Ensure log dir exists and return daily log path.
%
% Example:
%   logFile = db.sources.debug.debugSetupLog_('matlab_smoke');

    arguments
        logBasename (1,:) char
    end

    logDir = fullfile(db.sources.debug.debugDataRoot_(), 'logs');
    if ~isfolder(logDir)
        mkdir(logDir);
    end
    logFile = fullfile(logDir, sprintf('%s_%s.log', logBasename, datestr(now, 'yyyymmdd')));
end
