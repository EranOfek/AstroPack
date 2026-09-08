%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debug_outboxStatus_.m
% Author      : Chen Tishler
% Created     : 07/09/2026
% Updated     : 07/09/2026
% Description : Log offline outbox pending/failed folder summary for debug scripts.
%==========================================================================

function summary = debug_outboxStatus_(logFile)
%DEBUGOUTBOXSTATUS_  Print and return outbox pending/failed counts and paths.
%
% Example:
%   s = db.sources.debug.debug_outboxStatus_(logFile);

    if nargin < 1
        logFile = '';
    end

    dataRoot = tools.os.getAstroPackDataPath();
    summary = struct();
    summary.pendingRoot = fullfile(dataRoot, 'sources', 'outbox', 'pending');
    summary.failedRoot = fullfile(dataRoot, 'sources', 'outbox', 'failed');
    summary.pendingCount = 0;
    summary.failedCount = 0;
    summary.pendingNames = {};
    summary.failedNames = {};

    % Count pending outbox folders (FIFO replay order = sorted folder names).
    if isfolder(summary.pendingRoot)
        entries = dir(summary.pendingRoot);
        names = {entries([entries.isdir] & ~ismember({entries.name}, {'.', '..'})).name};
        summary.pendingNames = sort(names);
        summary.pendingCount = numel(summary.pendingNames);
    end

    % Count failed outbox folders (HTTP errors moved here by SourcesClient).
    if isfolder(summary.failedRoot)
        entries = dir(summary.failedRoot);
        names = {entries([entries.isdir] & ~ismember({entries.name}, {'.', '..'})).name};
        summary.failedNames = sort(names);
        summary.failedCount = numel(summary.failedNames);
    end

    db.sources.debug.debug_log('info', sprintf('outbox pending: %s', summary.pendingRoot), logFile);
    db.sources.debug.debug_log('info', sprintf('outbox failed:  %s', summary.failedRoot), logFile);
    db.sources.debug.debug_log('info', sprintf('pending folders: %d  failed folders: %d', ...
        summary.pendingCount, summary.failedCount), logFile);

    maxShow = 5;
    if summary.pendingCount > 0
        showNames = summary.pendingNames(1:min(maxShow, summary.pendingCount));
        db.sources.debug.debug_log('info', sprintf('pending (up to %d): %s', ...
            numel(showNames), strjoin(showNames, ', ')), logFile);
        if summary.pendingCount > maxShow
            db.sources.debug.debug_log('info', sprintf('... and %d more pending folder(s)', ...
                summary.pendingCount - maxShow), logFile);
        end
    end
end
