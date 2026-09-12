%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debugResolveBaseUrl_.m
% Author      : Chen Tishler
% Created     : 09/09/2026
% Description : Resolve US_BASE_URL for debug walkthrough scripts.
%==========================================================================

function baseUrl = debugResolveBaseUrl_(logFile, warnIfDefault)
%DEBUGRESOLVEBASEURL_  Read US_BASE_URL or fall back to localhost default.
%
% Example:
%   baseUrl = db.sources.debug.debugResolveBaseUrl_(logFile, true);

    arguments
        logFile (1,:) char = ''
        warnIfDefault (1,1) logical = false
    end

    C = db.sources.debug.debugConstants_();
    baseUrl = getenv('US_BASE_URL');
    if isempty(baseUrl)
        baseUrl = C.DefaultBaseUrl;
        if warnIfDefault
            db.sources.debug.debug_log('warn', sprintf('US_BASE_URL not set — using %s', baseUrl), logFile);
        end
    end
end
