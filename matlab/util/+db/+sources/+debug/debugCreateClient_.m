%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debugCreateClient_.m
% Author      : Chen Tishler
% Created     : 09/09/2026
% Description : Construct verbose SourcesClient for debug scripts.
%==========================================================================

function client = debugCreateClient_(baseUrl, logFile, clientTimeout)
%DEBUGCREATECLIENT_  Build SourcesClient with verbose logging enabled.
%
% Example:
%   client = db.sources.debug.debugCreateClient_(baseUrl, logFile, 120.0);

    arguments
        baseUrl (1,:) char
        logFile (1,:) char
        clientTimeout (1,1) double
    end

    client = db.sources.SourcesClient(baseUrl, '', clientTimeout);
    client.Verbose = true;
    client.LogFile = logFile;
end
