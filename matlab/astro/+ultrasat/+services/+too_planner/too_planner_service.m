%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+too_planner/too_planner_service.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/02/2026
% Description : MATLAB service to process TooPlanner requests using JsonFileIpc
%==========================================================================

function too_planner_service()
    % TooPlanner service - main function
	fprintf('too_planner_service started\n');    

    % Get the SOC_PATH environment variable
    SOC_PATH = getenv('SOC_PATH');
    if isempty(SOC_PATH)
        fprintf('SOC_PATH env must be set, terminated\n');
        exit;
    end

    % Set log file name
    ultrasat.services.common.setLogFile('too_planner_service', 'matlab_services/too_planner/');

    % Set the input path
    InputPath = fullfile(SOC_PATH, 'runtime', 'exchange', 'too_planner', 'input');
    ProcessedPath = fullfile(SOC_PATH, 'runtime', 'exchange', 'too_planner', 'processed');    

    % Create the JsonFileIpc object
    io.msgLog(LogLevel.Info, 'creating JsonFileIpc');    
    jsonIpc = ultrasat.services.common.JsonFileIpc('InputPath', InputPath, 'ProcessedPath', ProcessedPath, ...
        'Callback', @ultrasat.services.too_planner.processRequest);

    % Blocking loop: process files in the input folder, call processRequest.m for each file
    io.msgLog(LogLevel.Info, 'calling processLoop...');
    jsonIpc.processLoop();
    
    io.msgLog(LogLevel.Info, 'too_planner_service terminated');
end
