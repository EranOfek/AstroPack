%==========================================================================
% Author    : Chen Tishler
% Created   : 11/12/2022 
% Updated   : 08/08/2024
%
% MATLAB R2023a is required.
%
% NOTE: To run in from within MATLAB, you have to change folder
%       to the folder of this file (i.e. app_snr/)
%
% This process polls the input folder for incoming json files, process them
% according to the command specified in the file, and write output json to
% the same folder.
%==========================================================================

function app_ultrasat_too_planner()
    % Application main function
    % When run as deplyed matlab application, special issues need to be
    % handled. CURRENTLY we only run in from within matlab.

    global SOC_PATH;

    % Set logfile name
	fprintf('app_ultrasat_too_planner started, V0.01 (07/08/2024)\n');
    LogFile.getSingleton('FileName', 'soc_too_planner_matlab');

    % @Todo
    % Complete here code for 'isdeply' etc. from app_snr
    % ...

    % Get the SOC_PATH environment variable, if empty set default
    SOC_PATH = getenv('SOC_PATH');
    if isempty(SOC_PATH)
        if ispc
            SOC_PATH = 'c:/soc';
        else
            SOC_PATH = '/var/opt/soc';
        end
    end

    % Create instance of Component to trigger Config loading
    fprintf('Creating Comp\n');
    Comp = Component();    
    fprintf('Config.Path: %s\n', Comp.Config.Path);
    fprintf('Comp.Config.Data.MsgLogger.FileName: %s\n', Comp.Config.Data.MsgLogger.FileName);

    % Run the main loop, this is a blocking function
    fprintf('Calling mainLoop\n');
    mainLoop();
    fprintf('Returned from mainLoop\n');
    
    % Returned from main loop, may happen in case of fatal error or when
    % FileProcessor reached its maximum defined run time. Then the process
    % should be reloaded by the calling script.
    fprintf('soc_snr_matlab done\n');
end

%==========================================================================

function Result = mainLoop()
    % mainLoop - Blocking function
    % Flow:
    % 
    %   mainLoop -> FileProcessor.process() -> fileProcessorCallback() -> 
    %       processRequest() -> processTooPlanner(folder_name)
    %

    global SOC_PATH;

    % Set logging levels
    MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'file');
    MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'disp');               
    io.msgLog(LogLevel.Test, 'mainLoop started');

    % Prepare path of input files
    InputPath = fullfile(SOC_PATH, 'tooplanner', 'input');

    % Create FileProcessor object
    fp = FileProcessor('InputPath', InputPath, 'InputMask', '*.json');
    fp.ProcessFileFunc = @fileProcessorCallback;
    fp.EnableDelete = true;

    % Enable watchdog file that will be updated periodically by fp.process()
    % This allows external process to monitor that this matlab process is running as expected
    fp.WatchdogFileName = 'tooplanner_matlab_watchdog.txt';
    fp.WatchdogInterval = 10;

    % The process will automatically terminate every specified number of
    % hours and will be reloaded by the calling script. This allows us
    % to make sure that we avoid 
    fp.MaxRunTime = hours(8);

    % Input loop will call fileProcessorCallback (below) for each input file found in the folder
    % Note: Blocking function
    fp.process('DelaySec', 0.1);
    
    io.msgStyle(LogLevel.Test, '@passed', 'mainLoop passed');                          
    Result = true;
end

%------------------------------------------------------------------------
% This function is called from the blocking function FileProcessor.process() above.
% It reads the input json file to struct, call processItem with this struct, and after
% completion write the output struct to json file (first as .tmp file and
% then rename it, so external process will see the output file only after
% writing is finihed).
function fileProcessorCallback(RequestFileName)
    io.msgLog(LogLevel.Info, 'fileProcessorCallback started: %s', RequestFileName);
      
    % Prepare name of Response file name
    TmpFileName = strcat(RequestFileName, '.out.tmp');
    OutFileName = strcat(RequestFileName, '.out');

    % Read input JSON file
    RequestJson = fileread(RequestFileName);

    % Parse JSON from string to struct
    io.msgLog(LogLevel.Info, 'Request JSON: %s', RequestJson);
    Request = jsondecode(RequestJson);
   
    % Do the processing, this may take a long time, depending on the
    % processing required
    try
        Response = processRequest(Request);
    catch Ex
        Response = struct;
        Response.result = 0;
        Response.message = sprintf('MATLAB: Exception calling processRequest: %s', Ex.message);        
    end

    % Convert the output struct to json text
    ResponseJson = jsonencode(Response);

    % Write output JSON file in same folder, first as '.tmp' file, then
    % rename to output extension
    io.msgLog(LogLevel.Info, 'Response: result: %d, message: %s', Response.result, Response.message);
    fid = fopen(TmpFileName, 'wt');
    fprintf(fid, ResponseJson);
    fclose(fid);

    % Rename final file to output extension
    try
        io.msgLog(LogLevel.Info, 'Rename output: %s -> %s', TmpFileName, OutFileName);
        movefile(TmpFileName, OutFileName);
    catch
        io.msgLog(LogLevel.Info, 'Failed to rename output: %s -> %s', TmpFileName, OutFileName);
    end

    io.msgLog(LogLevel.Info, 'fileProcessorCallback done: %s', RequestFileName);
end

%------------------------------------------------------------------------

function Result = processRequest(Request)
    % Process item, return result
    % See ultrasat.git/python/prj/src/webapps/webapp_snr/rest_snr_server1.py
    % 
    % Input   : Request - struct with fields:
    %               cmd             - string
    %               input_folder    - string, folder with input files
    %               timeout         - integer (seconds)
    %                      
    % Output  : Response - struct with fields:
    %               result          - 1=success
    %               message         - string, optional message
    %               output_folder   - string, folder with output files
    %
    % Author  : Chen Tishler (2021, 2024)
    % Example : 
    
    % Prepare output with message for case of exception
    Response = struct;
    Response.result = 0;       
    Response.message = 'MATLAB: Exception in processRequest';
    Response.output_folder = '';
    
    try
        Response.message = sprintf('MATLAB: cmd: %s', Request.cmd);
        HandlerResponse = [];

        % Check request type
        if strcmp(Request.cmd, 'too_planner')            
            HandlerResponse = handleTooPlanner(Request);
        else
            Response.message = sprintf(Response.message, 'MATLAB: unknown cmd: %s', Request.cmd);
        end

        % Set the response fields
        if ~isempty(HandlerResponse)
            Response.result = HandlerResponse.result;            
            Response.message = HandlerResponse.message;
        end
    catch Ex
        % Return message with exception text
        Response.message = sprintf('MATLAB: exception: %s', Ex.message);
    end
    
    % Return the response
    Result = Response;
end
