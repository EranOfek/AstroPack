function Logger = ingestLogger(Args)
    % Logger of the DB table ingestion functions: a MsgLogger with one log file per night
    %     Used by insertArchiveImageHeaders2DB and insertArchiveSourceCatalogs2DB;
    %     write to it with pipeline.last.insertDB.ingestLog.
    %     The file of each night (noon to noon UTC, as io.files.getLogDate) is
    %     <LogDir>/<yyyy>/<yyyy-mm-dd>/<FileName>. The file size is not limited
    %     (LogFile cannot rotate a file in a day folder, issue #1456).
    %     The levels written to the file and to the screen are those of
    %     config/MsgLogger.yml (by default all to the file, Info and above to the screen).
    % Input  : * ...,key,val,...
    %            'LogDir'   - Folder of the log files. Default is '~/matlab/log/last/insertDB'.
    %            'FileName' - Log file name. Default is 'insertDB_tables.log'.
    %            'Console'  - If true, also print the lines on the screen. Default is true.
    % Output : - A MsgLogger object.
    % Author : A.M. Krassilchtchikov (2026 Oct)
    % Example: L = pipeline.last.insertDB.ingestLogger;
    %          pipeline.last.insertDB.insertArchiveSourceCatalogs2DB(RootDir, 'Logger',L)

    arguments
        Args.LogDir          = '~/matlab/log/last/insertDB';
        Args.FileName        = 'insertDB_tables.log';
        Args.Console logical = true;
    end

    LogDir = char(Args.LogDir);
    if startsWith(LogDir, '~')
        LogDir = [getenv('HOME'), LogDir(2:end)];
    end

    % MsgLogger writes its first line into the file it is given: give it the
    % file of tonight, then let LogFile choose the day folder for later lines
    Date    = io.files.getLogDate('Utc', true);
    DayFile = fullfile(LogDir, datestr(Date, 'yyyy'), datestr(Date, 'yyyy-mm-dd'), Args.FileName);
    Logger  = MsgLogger('FileName',DayFile, 'Console',Args.Console);
    Logger.LogF.FileName    = fullfile(LogDir, Args.FileName);
    Logger.LogF.DayFolder   = true;
    Logger.LogF.MaxFileSize = 0;
end
