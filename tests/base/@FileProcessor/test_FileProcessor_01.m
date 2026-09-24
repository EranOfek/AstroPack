
function Result = unitTest()
    % FileProcessor.unitTest
    % Unit test for the FileProcessor class.
    %
    % This test function initializes a FileProcessor object with a predefined input path and input mask.
    % It sets a custom file processing callback and triggers the processing loop for a specified time.
    % The function also configures the message logging levels for both file and display outputs.
    %
    % The test primarily validates the file processing loop, ensuring it can handle input files correctly
    % and execute the custom callback function.
    %
    % Returns:
    %   Result (boolean): True if the test passes, indicating that the FileProcessor operates as expected.
    %
    % Author : Chen Tishler (2021)

    MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'file');
    MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'disp');            
    
    io.msgLog(LogLevel.Test, 'FileProcessor test started');

    InputPath = 'c:/soc/snr/input';

    % Create objcet
    fp = FileProcessor('InputPath', InputPath, 'InputMask', '*.json');
    fp.ProcessFileFunc = @fileProcessorCallback;

    % Input loop will call FileProcessorCallback (below) for each input
    % file found in the folder
    fp.process('DelaySec', 0.1, 'MaxProcessTime', 5);
    
    io.msgStyle(LogLevel.Test, '@passed', 'FileProcessor test passed');                          
    Result = true;
end

%------------------------------------------------------------------------

function Result = processItem(item)
    % Processes a given item and returns a result.
    % This function performs operations based on the 'op' field of the input item, 
    % such as addition, multiplication, subtraction, and division.
    %
    % Input:
    %   item (struct): Struct containing operation fields 'op', 'x', and 'y'.
    %
    % Returns:
    %   Result (struct): Struct containing fields 'message', 'result', and 'json_text'.
    %                    'message' contains processing information or error messages,
    %                    'result' contains the outcome of the operation, and 'json_text'
    %                    is reserved for additional data in JSON format.
    %
    % Author : Chen Tishler (2021)
    %

    % Prepare output
    out = struct;
    out.message = 'MATLAB: Exception in processItem';
    out.result = -1;   
    out.json_text = '';  
    
    try
        out.message = sprintf('MATLAB: op: %s', item.op);
        
        if strcmp(item.op, 'add')
            out.result = item.x + item.y;
        elseif strcmp(item.op, 'mul')
            out.result = item.x * item.y;
        elseif strcmp(item.op, 'sub')
            out.result = item.x - item.y;
        elseif strcmp(item.op, 'div')
            if item.y ~= 0
                out.result = item.x / item.y;
            else
                ME = MException('SNR:process', 'Division by zero');
                throw(ME);
            end
        elseif strcmp(item.op, 'snr')            
            % Moved to apps/app_snr/soc_snt_matlab.m
        else
            strcpy(out.message, 'MATLAB: unknown op');
        end
    catch Ex
        out.message = sprintf('MATLAB: exception: %s', Ex.message);
    end
    
    Result = out;
end

%------------------------------------------------------------------------

function fileProcessorCallback(FileName)
    % Callback function for FileProcessor.
    % This function is called for each file processed by FileProcessor. It reads a JSON file,
    % decodes it, processes the data, and writes the result to an output JSON file.
    %
    % Input:
    %   FileName (string): Path of the file to be processed.
    %
    % This function demonstrates the integration of custom processing logic into the FileProcessor workflow.
    % It exemplifies file reading, JSON parsing, data processing, and writing output in a structured format.
    %
    % Author : Chen Tishler (2021)

    io.msgLog(LogLevel.Info, 'FileProcessorCallback started: %s', FileName);
      
    TmpFileName = strcat(FileName, '.out.tmp');
    OutFileName = strcat(FileName, '.out');

    % Read input JSON file
    fid = fopen(FileName);
    raw = fread(fid, inf);
    str = char(raw');
    fclose(fid);
    
    % Parse JSON from string to struct
    io.msgLog(LogLevel.Info, 'JSON: %s', str);
    item = jsondecode(str);
   
    % Process
    try
        out = processItem(item);
    catch Ex
        out = struct;
        out.message = sprintf('MATLAB: Exception calling processItem: %s', Ex.message);        
        out.result = -1;           
    end

    % Write output JSON file
    io.msgLog(LogLevel.Info, 'Out.message: %s, result: %d, json_text: %s', out.message, out.result, out.json_text);
    out_json = jsonencode(out);
    fid = fopen(TmpFileName, 'wt');
    fprintf(fid, out_json);
    fclose(fid);

    % Rename final file to output extension
    try
        io.msgLog(LogLevel.Info, 'Rename output %s -> %s', TmpFileName, OutFileName);
        movefile(TmpFileName, OutFileName);
    catch
    end
end
