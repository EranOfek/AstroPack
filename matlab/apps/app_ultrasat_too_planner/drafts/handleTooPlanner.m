%==========================================================================
% Author    : Chen Tishler
% Created   : 11/12/2022 
% Updated   : 10/08/2024
%
% This file handles the request to run TOO Planner.
% Input: Path to folder
% Output: 
%==========================================================================

function handleTooPlanner(Request)
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
    % Author  : Chen Tishler, 08/08/2024
    %

    % The input and output are in the specified folder by Request.input_folder

    % Read input JSON file into a struct
    InputFileName = fullfile(Request.input_folder, 'input.json');
    InputJson = fileread(InputFileName);
    
    % Parse JSON from string to struct
    io.msgLog(LogLevel.Info, 'JSON: %s', InputJson);
    Input = jsondecode(InputJson);

    % @Todo - Change log file to file in this folder, so we will have
    % detailed log of the request, and change it back at the end
    SysLogger = LogFile().getSingleton();
    SaveLogFileName = SysLogger.getFileName();
    SysLogger.setFileName( fullfile(Request.input_folder, 'request.log') );

    % Prepare output struct, output folder is same as input
    Output = struct;
    Output.result = 0;
    Output.message = sprintf('handleTooPlanner started');    
    Output.output_folder = Request.input_folder;  

    % Convert the output struct to json text
    OutputJson = jsonencode(Output);

    % Write output JSON file in same folder, first as '.tmp' file, then
    % rename to output extension
    io.msgLog(LogLevel.Info, 'Response: result: %d, message: %s,  json_text: %s', Response.message, Response.result, Response.json_text);
    fid = fopen(TmpFileName, 'wt');
    fprintf(fid, out_json);
    fclose(fid);

    % Rename final file to output extension
    try
        io.msgLog(LogLevel.Info, 'Rename output %s -> %s', TmpFileName, OutFileName);
        movefile(TmpFileName, OutFileName);
    catch
    end

    % Restore log filename
    SysLogger.setFileName(SaveLogFileName);
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
    %               status          - 1=success
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
        
        % Check request type
        if strcmp(Request.cmd, 'too_planner')            
            Response = processTooPlanner(Request.input_folder);
        else
            strcpy(Response.message, 'MATLAB: unknown cmd');
        end
    catch Ex
        % Return message with exception text
        Response.message = sprintf('MATLAB: exception: %s', Ex.message);
    end
    
    % Return the response
    Result = Response;
end

%========================================================================

%========================================================================

function Result = processTooPlanner(Path)
    % Process SNR
    % See ultrasat.git/python/prj/src/webapps/webapp_snr/rest_snr_server1.py
    %    
    % Input   : - snr - struct 
    % 
    %                      
    % Output  : struct ResponseMessage with fields: message, result
    % Author  : Chen Tishler (2022)
    % Example : 
    
    % Decode text
    snr_input = jsondecode(json_text);
            
    out = struct;
    out.message = sprintf('MATLAB: processTooPlannerJson started');
    out.result = -1;
    out.json_text = '';
    
    % Do the actual SNR processing here
    [snr_out, message] = doProcessTooPlannerJson(snr_input);
    
    % Done
    out.message = message;
    snr_out.message = '';
    out.result = 0;    
    out.json_text = jsonencode(snr_out);
    out.json_text = strrep(out.json_text, '"', '\"');
    Result = out;
end

%------------------------------------------------------------------------

function [Result, Message] = doProcessTooPlannerJson(Params)
    % Process SNR
    % See ultrasat.git/python/prj/src/webapps/webapp_snr/rest_snr_server1.py
    
    % Input   : - Params - struct with these fields:
	%
    %   ExpTime
    %   NumImages
    %   R
    %   Source
    %   PicklesModels
    %   SnrMagnitude
    %   CalibFilterFamily
    %   CalibFilter
    %   MagnitudeSystem
    %   LimitingMagnitude
    %    
    %                      
    % Output  : - Result - struct with fields: 
    %   ResultSnr
    %   ResultLimitingMagnitude
    %           - Message - char with text message


    % The json contain the folder name of the actual files

    io.msgLog(LogLevel.Debug, 'doProcessSnr: started - Params:');
    disp(Params);
 
    % Calculate
    try
        if strcmp(Params.Source, 'PicklesModels')
            Params.Source = Params.PicklesModels;
            Params = rmfield(Params, 'PicklesModels');
        end
        
        if strcmp(Params.Source, 'BlackBody')
            Params.Source = strcat('Planck spectrum T=', Params.BlackBodyTemperature, '.000000');
            Params = rmfield(Params, 'BlackBodyTemperature');
        end
        
        io.msgLog(LogLevel.Debug, 'doProcessSnr: creating UltrasatPerf2GUI');
        UsatPerf2GUI = UltrasatPerf2GUI();
        
        io.msgLog(LogLevel.Debug, 'doProcessSnr: calling namedargs2cell');
        ArgsCell = namedargs2cell(Params);
        
        io.msgLog(LogLevel.Debug, 'doProcessSnr: calling calcSNR');
        Result = UsatPerf2GUI.calcSNR(ArgsCell{:});
        
        io.msgLog(LogLevel.Debug, 'doProcessSnr: calling calcSNR done');
    catch ex
        Result.message = sprintf("doProcessSnr: error: UG threw exception identifier='%s' with message='%s'", ex.identifier, ex.message);
    end

    % Prepare output
    disp(Result);
    Message = Result.message;
    Result = rmfield(Result, 'message');
    
    io.msgLog(LogLevel.Debug, 'doProcessSnr: done');    
end

