function ingestLog(Logger, Level, varargin)
    % Write one line to the log file and the screen of an ingestion logger
    %     The text is formatted once and written as it is, so '%' and '\' in
    %     paths and error messages are kept (MsgLogger.msgLog formats the
    %     screen text twice and doubles the level brackets of warnings, issue #1456).
    %     Follows the logger's Enabled, Console, CurFileLevel and CurDispLevel;
    %     warnings and errors are always written, as in MsgLogger.
    % Input  : - A MsgLogger object (pipeline.last.insertDB.ingestLogger). If empty, do nothing.
    %          - A LogLevel.
    %          - The text, or a format and its arguments (as sprintf).
    % Output : null
    % Author : A.M. Krassilchtchikov (2026 Oct)
    % Example: pipeline.last.insertDB.ingestLog(L, LogLevel.Info, 'done %s', DataDir)

    if isempty(Logger) || ~Logger.Enabled || Level == LogLevel.None
        return
    end
    if isscalar(varargin)
        Msg = char(varargin{1});
    else
        Msg = sprintf(varargin{:});
    end
    Msg = regexprep(strtrim(Msg), '\s*\n\s*', ' / ');   % one line per message
    LevStr = MsgLogger.getLevelStr(Level);
    Must   = Logger.mustLog(Level);
    if Must || uint32(Level) <= uint32(Logger.CurFileLevel)
        Logger.LogF.write2(sprintf('[%s]', LevStr), '%s', Msg);
    end
    if Logger.Console && (Must || uint32(Level) <= uint32(Logger.CurDispLevel))
        fprintf('%s [%s] %s\n', datestr(now, 'HH:MM:SS.FFF'), LevStr, Msg);
    end
end
