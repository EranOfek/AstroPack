
classdef FileInterface < Component
    %
    % Author: Chen Tishler (2024)
    %   
    

   % Properties
    properties (SetAccess = public)
        InterfaceFolder             %
        Processor                   % File name of input.json
        MaxRunTime = hours(8);      % Optional

        %
        CmdHandlerFunc = []         % Function handle that points to the function for processing files (FileProcessor, FileName)        
    end

    %--------------------------------------------------------
    methods
        function Obj = FileInterface(InterfaceFolder)
            Obj.InterfaceFolder = InterfaceFolder;
            Obj.init();
        end


        function delete(Obj)
            % Destructor
        end        


        function Result = init(Obj)
            % mainLoop - Blocking function
            % Flow:
            % 
            %   mainLoop -> FileProcessor.process() -> fileProcessorCallback() -> 
            %       processRequest() -> processTooPlanner(folder_name)
            %

            % Set logging levels
            MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'file');
            MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'disp');               
            Obj.msgLog(LogLevel.Info, 'mainLoop started');
               
            % Create FileProcessor object
            Obj.Processor = FileProcessor('InputPath', Obj.InterfaceFolder, 'InputMask', '*.json');
            Obj.Processor.ProcessFileFunc = @Obj.fileProcessorCallback;
            Obj.Processor.EnableDelete = true;
        
            % Enable watchdog file that will be updated periodically by fp.process()
            % This allows external process to monitor that this matlab process is running as expected
            Obj.Processor.WatchdogFileName = 'tooplanner_matlab_watchdog.txt';
            Obj.Processor.WatchdogInterval = 10;
        
            % The process will automatically terminate every specified number of
            % hours and will be reloaded by the calling script. This allows us
            % to make sure that we avoid 
            Obj.Processor.MaxRunTime = Obj.MaxRunTime;

            Result = true;
        end

        
        function Result = mainLoop(Obj)
            % mainLoop - Blocking function
            % Flow:
            % 
            %   mainLoop -> FileProcessor.process() -> fileProcessorCallback() -> 
            %       processRequest() -> processTooPlanner(folder_name)
            %

            Obj.msgLog(LogLevel.Info, 'mainLoop started');            
            
            % Input loop will call fileProcessorCallback (below) for each input file found in the folder
            % Note: Blocking function
            Obj.Processor.process('DelaySec', 0.1);
            
            Obj.msgLog(LogLevel.Info, 'mainLoop done');                          
            Result = true;
        end
 

        function fileProcessorCallback(RequestFileName)
            % This function is called from the blocking function FileProcessor.process() above.
            % It reads the input json file to struct, call processItem with this struct, and after
            % completion write the output struct to json file (first as .tmp file and
            % then rename it, so external process will see the output file only after
            % writing is finihed).    
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
                Response = Obj.processRequest(Request);
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
   

        function Result = processRequest(Obj, Request)
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
            Response.message = sprintf('MATLAB: cmd: %s', Request.cmd);
            Response.output_folder = '';
            HandlerResponse = [];

            % Call handler in derived class
            try                       
                % Call user callback function (event)
                if ~isempty(Obj.CmdHandlerFunc)
                    HandlerResponse = Obj.CmdHandlerFunc(FileName);                    
                else
                    Obj.msgLog(LogLevel.Warning, 'FileInterface.CmdHandlerFunc is not set!');
                end
            catch Ex
                % Return message with exception text
                Response.message = sprintf('MATLAB: exception: %s', Ex.message);                
            end

            % Set the response fields
            if ~isempty(HandlerResponse)
                Response.result = HandlerResponse.result;            
                Response.message = HandlerResponse.message;
            end
            
            % Return the response
            Result = Response;
        end

    end    
end
