%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : soc_too_planner_service.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 09/02/2026
% Description : MATLAB service to process TooPlanner requests
%==========================================================================

function soc_too_planner_service()
    % TooPlanner Service - Main function

    % Get the SOC_PATH environment variable, if not set, use default
    SOC_PATH = getenv('SOC_PATH');
    if isempty(SOC_PATH)
        if ispc
            SOC_PATH = 'c:/soc';
        else
            SOC_PATH = '/home/soc/soc';
        end
    end

    % Set LogFile to use monthly log file
	fprintf('soc_too_matlab started, V1.00 (29/01/2026)\n');
    LF = LogFile.getSingleton('FileName', 'soc_too_calc_matlab', ...
        'SubFolder', 'too_planner/matlab', ...
        'UseMonthPrefix', true);
           
    % Link MsgLogger to the LogFile object
    ML = MsgLogger.getSingleton();
    ML.LogF = LF;
    io.msgLog(LogLevel.Info, 'soc_too_matlab started');
  

    % Set the log level
    MsgLogger.setLogLevel(LogLevel.Info, 'type', 'file');
    MsgLogger.setLogLevel(LogLevel.Info, 'type', 'disp');            

    % Set the input path
    InputPath = fullfile(SOC_PATH, 'runtime', 'exchange', 'too_planner', 'input');
    ProcessedPath = fullfile(SOC_PATH, 'runtime', 'exchange', 'too_planner', 'processed');    

    % Log the start of the main loop
    io.msgLog(LogLevel.Info, 'TooPlanner mainLoop started - Input folder: %s', strrep(InputPath, '\', '\\'));

    % Create the JsonFileIpc object
    Ipc = ultrasat.services.common.JsonFileIpc('InputPath', InputPath, 'ProcessedPath', ProcessedPath, ...
        'Callback', @ultrasat.services.too_planner.processRequest);

    Ipc.processLoop();
   
end

