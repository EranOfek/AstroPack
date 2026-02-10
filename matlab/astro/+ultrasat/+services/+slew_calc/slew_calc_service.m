%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : slew_calc_service.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/02/2026
% Description : MATLAB service to calculate slew time between targets
%==========================================================================

function slew_calc_service()
    % Slew calculator service - main function
	fprintf('slew_calc_service started\n');    

    % Get the SOC_PATH environment variable
    SOC_PATH = getenv('SOC_PATH');
    if isempty(SOC_PATH)
        fprintf('SOC_PATH env must be set, terminated\n');
        exit;
    end

    % Set log file name
    ultrasat.services.common.setLogFile('slew_calc_service', 'matlab_services/slew_calc/');

    % Set the input path
    InputPath = fullfile(SOC_PATH, 'runtime', 'exchange', 'slew_calc', 'input');
    ProcessedPath = fullfile(SOC_PATH, 'runtime', 'exchange', 'slew_calc', 'processed');    

    % Create the JsonFileIpc object
    io.msgLog(LogLevel.Info, 'creating JsonFileIpc');    
    jsonIpc = ultrasat.services.common.JsonFileIpc('InputPath', InputPath, 'ProcessedPath', ProcessedPath, ...
        'Callback', @ultrasat.services.slew_calc.processRequest);

    % Blocking loop: process files in the input folder, call processRequest.m for each file
    io.msgLog(LogLevel.Info, 'calling processLoop...');
    jsonIpc.processLoop();
    
    io.msgLog(LogLevel.Info, 'soc_slew_service terminated');
end
