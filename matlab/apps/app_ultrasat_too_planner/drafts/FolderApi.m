
% Author    : Chen Tishler
% Created   : 09/08/2024
% Updated   : 09/08/2024
%
% 
% Input: Path to folder
% Output: 
%==========================================================================

classdef FolderApi < handle

   % Properties
    properties (SetAccess = public)
        InputFolder             %
        OutputFolder            %
        Result                  % 1=Success
        Message                 % 
        Request                 % Request struct sent by the caller
        Response                % Response struct to be returned to the caller

        LogFileName             % Log file name used to log processing of this API call
        InputFileName           % File name of input.json
        InputJson               % Input as json text
        Input                   % Input as struct, convertede from InputJson
        Output                  % Output struct
        OutputJson              % Output struct as json text
        OutputFileName          % Output file name              
        SaveLogFileName         % Saved log file name before setting to LogFileName
    end

    %--------------------------------------------------------
    methods % Constructor

        function Obj = FolderApi(Request)
            Obj.init(Request)
        end


        function delete(Obj)
            % Destructor - restore log file
            if ~isempty(Obj.SaveLogFileName)
                SysLogger = LogFile().getSingleton();
                SysLogger.setFileName(Obj.SaveLogFileName);
            end
        end        
    end


    methods %
        function Result = init(Obj, Request)

            Obj.Request = Request;

            % Prepare response struct
            Obj.Response = struct;
            Obj.Response.result = 0;       
            Obj.Response.message = 'MATLAB: Exception in processRequest';
            Obj.Response.output_folder = '';

            % Read input JSON file into a struct
            Obj.InputFileName = fullfile(Obj.Request.input_folder, 'input.json');
            Obj.InputJson = fileread(Obj.InputFileName);
            
            % Parse JSON from string to struct
            io.msgLog(LogLevel.Info, 'JSON: %s', Obj.InputJson);
            Obj.Input = jsondecode(Obj.InputJson);
        
            % Change log file to file in this folder, so we will have
            % detailed log of the request, and change it back at the end
            SysLogger = LogFile().getSingleton();
            Obj.SaveLogFileName = SysLogger.getFileName();
            Obj.LogFileName = fullfile(Obj.Request.input_folder, 'request.log');
            SysLogger.setFileName(Obj.LogFileName);
        
            % Prepare output struct, output folder is same as input
            Obj.Output = struct;
            Obj.Output.result = 0;
            Obj.Output.message = sprintf('handleTooPlanner started');    
            Obj.Output.output_folder = Obj.Request.input_folder;  

            Result = true;
        end


        function Result = writeOutput(Obj)
            % Write output file
    
            % Convert the output struct to json text
            Obj.OutputJson = jsonencode(Obj.Output);

            % Write output JSON file in same folder, first as '.tmp' file, then
            % rename to output extension
            io.msgLog(LogLevel.Info, 'Response: result: %d, message: %s', Obj.Response.result, Obj.Response.message);
            fid = fopen(TmpFileName, 'wt');
            fprintf(fid, out_json);
            fclose(fid);

            % Rename final file to output extension
            try
                io.msgLog(LogLevel.Info, 'Rename output: %s -> %s', TmpFileName, OutFileName);
                movefile(TmpFileName, OutFileName);
            catch
                io.msgLog(LogLevel.Info, 'Faile to rename output: %s -> %s', TmpFileName, OutFileName);
            end

            % Restore log filename
            if ~isempty(Obj.SaveLogFileName)
                SysLogger = LogFile().getSingleton();
                SysLogger.setFileName(Obj.SaveLogFileName);
                Obj.SaveLogFileName = [];
            end

            Result = true;
        end

    end    
end

