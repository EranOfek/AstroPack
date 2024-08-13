
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

        % Input
        InputFolder             %
        InputFileName           % File name of input.json
        InputJson               % Input as json text
        Input                   % Input as struct, convertede from InputJson

        % Output
        OutputFolder            %
        Output                  % Output struct
        OutputJson              % Output struct as json text
        OutputFileName          % Output file name              

        % Result to the caller
        Result                  % 1=Success
        Message                 % 

        % Logging
        LogFileName             % Log file name used to log processing of this API call
        SaveLogFileName         % Saved log file name before setting to LogFileName
    end

    %--------------------------------------------------------
    methods % Constructor

        function Obj = FolderApi(InputFolder)
            Obj.init(InputFolder)
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
        function Result = init(Obj, InputFolder)

            Obj.InputFolder = InputFolder;
            Obj.OutputFolder = InputFolder;

            % Read input JSON file into a struct
            Obj.InputFileName = fullfile(Obj.InputFolder, 'input.json');
            Obj.InputJson = fileread(Obj.InputFileName);
            
            % Parse JSON from string to struct
            io.msgLog(LogLevel.Info, 'JSON: %s', Obj.InputJson);
            Obj.Input = jsondecode(Obj.InputJson);
        
            % Change log file to file in this folder, so we will have
            % detailed log of the request, and change it back at the end
            SysLogger = LogFile().getSingleton();
            Obj.SaveLogFileName = SysLogger.getFileName();
            Obj.LogFileName = fullfile(Obj.InputFolder, 'request.log');
            SysLogger.setFileName(Obj.LogFileName);
        
            % Prepare output struct, to be filled by the handler
            Obj.Output = struct;
            Obj.Output.Result = 0;
            Obj.Output.Message = sprintf('started');

            Result = true;
        end


        function Result = writeOutput(Obj)
            % Write output file
    
            % Convert the output struct to json text
            Obj.OutputJson = jsonencode(Obj.Output);

            % Write output JSON file in same folder, first as '.tmp' file, then
            % rename to output extension
            io.msgLog(LogLevel.Info, 'Result: %d, message: %s', Obj.Result, Obj.Message);
            fid = fopen(TmpFileName, 'wt');
            fprintf(fid, Obj.OutputJson);
            fclose(fid);

            % Rename final file to output extension
            try
                io.msgLog(LogLevel.Info, 'Rename output: %s -> %s', TmpFileName, OutFileName);
                movefile(TmpFileName, OutFileName);
            catch
                io.msgLog(LogLevel.Info, 'Failed to rename output: %s -> %s', TmpFileName, OutFileName);
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

