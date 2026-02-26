%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.common.sparseLogFile.m
% Author      : Chen Tishler
% Created     : 02/11/2021
% Modified    : 10/02/2026
% Description : JSON file IPC class
%==========================================================================

function setLogFile(FileName, SubFolder)

    % Set log file name
    LF = LogFile.getSingleton('FileName', FileName, 'SubFolder', SubFolder);

    % Link to MsgLogger
    ML = MsgLogger.getSingleton();
    ML.LogF = LF;
    io.msgLog(LogLevel.Info, sprintf('%s started', FileName));    

    % Set the log level
    MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'file');
    MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'disp');            
end
