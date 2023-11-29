%==========================================================================
% Author: Chen Tishler, Created: 11/12/2022, Updated: 15/11/2023
%
% MATLAB R2023a is required.
%
% Build EXE file by running this batch from source folder:
% 
%       _build.bat
%
% Notes:
%    - mcc generates EXE file which is a self-extract ZIP, with all required source files.
%    - It is not possible to call addpath() in deployed application!
%    - snakeyaml-1.9.jar should be in the EXE folder.
%    - ASTROPACK_CONFIG_PATH environment must be set
%
% See: https://www.mathworks.com/help/compiler/create-and-install-a-standalone-application-from-matlab-code.html
% See: https://www.mathworks.com/help/compiler/mcc.html
% See: https://www.mathworks.com/help/compiler/isdeployed.html
%
% NOTE: To run in from within MATLAB, you have to change folder
%       to the folder of this file (i.e. app_snr/)
%==========================================================================

function soc_snr_matlab()
    % SNR App - Main function

    global SOC_PATH;

    % Set logfile name
	fprintf('soc_snr_matlab started, V1.04 (15/11/2023)\n');
    LogFile.getSingleton('FileName', 'soc_snr_matlab');
            
    fprintf('getPid: %d\n', tools.os.getPid());
    fprintf('getProcessName: %s\n', tools.os.getProcessName());
    fprintf('getCmdLine: %s\n', tools.os.getCmdLine());

    if isdeployed
        fprintf('soc_snr_matlab: isdeployed = TRUE\n');
        locate_externapp = which(fullfile('soc_snr_matlab.exe'));
        fprintf('which: %s\n', locate_externapp);
    end    
    
    if ismcc
        fprintf('soc_snr_matlab: ismcc = TRUE\n');
    end           

    % Get the SOC_PATH environment variable
    SOC_PATH = getenv('SOC_PATH');

    % Check if SOC_PATH is not empty
    if isempty(SOC_PATH)
        if ispc
            SOC_PATH = 'c:/soc';
        else
            SOC_PATH = '/var/opt/soc';
        end
    end

    % Test that we can find file 'Gal_Sc.txt'
    if isdeployed
        fprintf('\n\n================================== Check started\n');
        FName = 'Gal_Sc.txt';

        % Before loadMap - Check which() and fileMapFind()
        fprintf('Checking: Before loadMap - calling which( %s ) :\n', FName);
        which(FName)
        MatFile = fileMapFind(FName, 'Assert', false);
        fprintf('Checking: Before loadMap - fileMapFind( %s ) - %s\n\n', FName, MatFile);

        % Load the saved FileMap object, it was created by calling FileMap
        % from **startup.m**, and used to map files to folders, because
        % MATLAB's addpath() does not work from deployd code.
        FMap = FileMap.getSingleton();
        FMap.StorageFileName = fullfile(SOC_PATH, 'snr', 'snr_matlab', 'AstroPackFileMap_1.mat');
        FMap.loadMap();    
        fprintf('\nCalling FileMap.addPathFolders ...\n');
        FMap.addPathFolders();    
        fprintf('\nCalling FileMap.scanFolders ...\n');
        FMap.scanFolders();

        % After loadMap - Check which() and fileMapFind()
        fprintf('\nChecking: AFTER loadMap - calling which( %s ) :\n', FName);
        which(FName)    
        MatFile = fileMapFind(FName, 'Assert', false);
        fprintf('Checking: AFTER loadMap - fileMapFind( %s ) - %s\n', FName, MatFile);

        FName = 'AstFilterCat.mat';
        MatFile = fileMapFind(FName, 'Assert', false);
        fprintf('Checking: AFTER loadMap - fileMapFind( %s ) - %s\n', FName, MatFile);

        fprintf('\n\n================================== Check done\n\n\n');
    end

    % Create component to trigger Config loading
    fprintf('Creating Comp\n');
    Comp = Component();
    fprintf('Config.Path: %s\n', Comp.Config.Path);
    fprintf('Comp.Config.Data.MsgLogger.FileName: %s\n', Comp.Config.Data.MsgLogger.FileName);

    % Call UltrasatPerf() to force the mcc compiler linking this class and
    % its related sources. Otherwise we will get this error:
    %     09:50:58.133 [DBG] doProcessSnr: creating UltrasatPerf2GUI
    %     09:50:58.176 [DBG] UltrasatPerf2GUI: UltrasatPerf2GUI:load: c:\soc\snr\snr_matlab\P90_UP_test_60_ZP_Var_Cern_21.mat
    %     Warning: Variable 'UP' originally saved as a UltrasatPerf cannot be instantiated as an object and will be read in as a uint32.    
    fprintf('Calling: Perf = UltrasatPerf(Init, false);\n');    
    Perf = UltrasatPerf('Init', false);
    fprintf('UltrasatPerf done\n');
    
    %try
        %if ~isdeployed
            fprintf('Calling mainLoop\n');
            mainLoop();
            fprintf('Returned from mainLoop\n');
        %end
    %catch
        %fprintf('soc_snr_matlab exception\n');
    %end
    
    fprintf('soc_snr_matlab done\n');
end

%==========================================================================

function Result = mainLoop()
    % mainLoop - Blocking function
    global SOC_PATH;

    MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'file');
    MsgLogger.setLogLevel(LogLevel.Debug, 'type', 'disp');            
    
    io.msgLog(LogLevel.Test, 'SNR mainLoop started');

    InputPath = fullfile(SOC_PATH, 'snr', 'input');

    % Create objcet
    fp = FileProcessor('InputPath', InputPath, 'InputMask', '*.json');
    fp.ProcessFileFunc = @fileProcessorCallback;
    fp.EnableDelete = true;
    fp.WatchdogFileName = 'snr_matlab_watchdog.txt';
    fp.WatchdogInterval = 10;
    fp.MaxRunTime = hours(8);

    % Input loop will call FileProcessorCallback (below) for each input
    % file found in the folder
    % Note: Blocking function
    fp.process('DelaySec', 0.1);
    
    io.msgStyle(LogLevel.Test, '@passed', 'SNR mainLoop passed');                          
    Result = true;
end

%------------------------------------------------------------------------

function Result = processItem(item)
    % Process item, return result
    % See ultrasat.git/python/prj/src/webapps/webapp_snr/rest_snr_server1.py
    
    % Input   : - item - struct Item with op, x, y fields

    %           * Pairs of ...,key,val,...
    %             The following keys are available:            			            

    %                      
    % Output  : struct ResponseMessage with message, result fields
    % Author  : Chen Tishler (2021)
    % Example : 
    
    % Item 
    % ResponseMessage
    
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
            out = processSnrJson(item.json_text);
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

%------------------------------------------------------------------------

function Result = processSnrJson(json_text)
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
    out.message = sprintf('MATLAB: processSnr started');
    out.result = -1;
    out.json_text = '';
    
    % Do the actual SNR processing here
    [snr_out, message] = doProcessSnr(snr_input);
    
    % Done
    out.message = message;
    snr_out.message = '';
    out.result = 0;    
    out.json_text = jsonencode(snr_out);
    out.json_text = strrep(out.json_text, '"', '\"');
    Result = out;
end

%------------------------------------------------------------------------

function [Result, Message] = doProcessSnr(Params)
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
    %
    % Author  : Arie B. (2023)
    % Example : 

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

    %
    disp(Result);
    Message = Result.message;
    Result = rmfield(Result, 'message');
    
    io.msgLog(LogLevel.Debug, 'doProcessSnr: done');    
end

