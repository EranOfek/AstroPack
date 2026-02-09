%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : JsonFileIpc.m
% Author      : Chen Tishler
% Created     : 2026
% Description : JSON file IPC: read JSON -> Handler(item) -> write JSON .out
%               Subclasses FileProcessor; overrides processFileImpl.
%               Includes run() for blocking main loop.
%==========================================================================

classdef JsonFileIpc < FileProcessor
    % JsonFileIpc - Poll for *.json files, decode, call Handler(item), write .out
    %
    % Usage:
    %   ipc = ultrasat.services.common.JsonFileIpc( ...
    %       'InputPath', inputDir, 'ProcessedPath', processedDir, ...
    %       'Handler', @myProcessRequest);
    %   ipc.run();  % blocking
    %
    % Handler signature: outStruct = Handler(itemStruct) or Handler(itemStruct, UserParam)

    properties (SetAccess = public)
        Handler    = []   % function_handle: out = Handler(item) or Handler(item, UserParam)
        UserParam  = []   % optional; if set, call Handler(item, Obj.UserParam)
        DelaySec   = 0.1  % used by run() if not overridden in run(Args)
        MaxRunTime = []   % optional duration; used by run() if not overridden
    end

    methods
        function Obj = JsonFileIpc(Args)
            arguments
                Args.InputPath
                Args.ProcessedPath
                Args.Handler
                Args.UserParam = []
                Args.InputMask = '*.json'
                Args.EnableDelete = true
                Args.WatchdogFileName = []
                Args.WatchdogInterval = 10
                Args.DelaySec = 0.1
                Args.MaxRunTime = []
            end
            % Call base constructor
            Obj = Obj@FileProcessor('InputPath', Args.InputPath, ...
                'InputMask', Args.InputMask, ...
                'ProcessedPath', Args.ProcessedPath);
            Obj.setName('JsonFileIpc');
            Obj.Handler = Args.Handler;
            Obj.UserParam = Args.UserParam;
            Obj.EnableDelete = Args.EnableDelete;
            if ~isempty(Args.WatchdogFileName)
                Obj.WatchdogFileName = Args.WatchdogFileName;
            end
            if ~isempty(Args.WatchdogInterval)
                Obj.WatchdogInterval = Args.WatchdogInterval;
            end
            Obj.DelaySec = Args.DelaySec;
            Obj.MaxRunTime = Args.MaxRunTime;
        end

        function Result = processFileImpl(Obj, FileName)
            % Read JSON -> Handler(item) -> write FileName.out (via .out.tmp)
            % On any error, still write an error struct to .out so caller gets a response.
            Result = 0;
            TmpFileName = [FileName, '.out.tmp'];
            OutFileName = [FileName, '.out'];
            out = struct('message', 'MATLAB: unknown error', 'result', -1);

            % 1. Read file
            try
                fid = fopen(FileName);
                raw = fread(fid, inf);
                str = char(raw');
                fclose(fid);
            catch Ex
                Obj.msgLog(LogLevel.Error, 'JsonFileIpc: Error reading file %s: %s', FileName, Ex.message);
                out.message = sprintf('MATLAB: Exception reading file %s: %s', FileName, Ex.message);
                Obj.writeOut(TmpFileName, OutFileName, out);
                return;
            end

            % 2. Decode JSON
            try
                Obj.msgLog(LogLevel.Debug, 'JsonFileIpc: JSON: %s', str);
                item = jsondecode(str);
            catch Ex
                Obj.msgLog(LogLevel.Error, 'JsonFileIpc: Error parsing JSON from file %s: %s', FileName, Ex.message);
                out.message = sprintf('MATLAB: Exception parsing JSON from file %s: %s', FileName, Ex.message);
                Obj.writeOut(TmpFileName, OutFileName, out);
                return;
            end

            % 3. Call user handler
            try
                if isempty(Obj.UserParam)
                    out = Obj.Handler(item);
                else
                    out = Obj.Handler(item, Obj.UserParam);
                end
            catch Ex
                Obj.msgLog(LogLevel.Error, 'JsonFileIpc: Error calling Handler: %s', Ex.message);
                out = struct('message', sprintf('MATLAB: Exception in Handler: %s', Ex.message), 'result', -1);
            end

            % 4. Write output
            Obj.msgLog(LogLevel.Debug, 'JsonFileIpc: result: %d', out.result);
            Obj.writeOut(TmpFileName, OutFileName, out);
            Obj.msgLog(LogLevel.Info, 'JsonFileIpc done: %s', strrep(FileName, '\', '\\'));
        end

        function writeOut(Obj, TmpFileName, OutFileName, out)
            try
                out_json = jsonencode(out);
                fid = fopen(TmpFileName, 'wt');
                fprintf(fid, '%s', out_json);
                fclose(fid);
                movefile(TmpFileName, OutFileName, 'f');
            catch Ex
                Obj.msgLog(LogLevel.Error, 'JsonFileIpc: Error writing output %s: %s', OutFileName, Ex.message);
            end
        end

        function Result = run(Obj, Args)
            % Blocking main loop. Optionally override paths/options via Args.
            arguments
                Obj
                Args.InputPath = []
                Args.ProcessedPath = []
                Args.DelaySec = []
                Args.MaxRunTime = []
            end
            if ~isempty(Args.InputPath)
                Obj.InputPath = Args.InputPath;
            end
            if ~isempty(Args.ProcessedPath)
                Obj.ProcessedPath = Args.ProcessedPath;
            end
            DelaySec = Obj.DelaySec;
            if ~isempty(Args.DelaySec)
                DelaySec = Args.DelaySec;
            end
            MaxProcessTime = Inf;
            if ~isempty(Args.MaxRunTime)
                if isduration(Args.MaxRunTime)
                    MaxProcessTime = seconds(Args.MaxRunTime);
                else
                    MaxProcessTime = Args.MaxRunTime;
                end
            elseif ~isempty(Obj.MaxRunTime)
                if isduration(Obj.MaxRunTime)
                    MaxProcessTime = seconds(Obj.MaxRunTime);
                else
                    MaxProcessTime = Obj.MaxRunTime;
                end
            end
            Obj.msgLog(LogLevel.Info, 'JsonFileIpc run started - Input: %s', strrep(Obj.InputPath, '\', '\\'));
            Result = Obj.process('DelaySec', DelaySec, 'MaxProcessTime', MaxProcessTime);
            Obj.msgLog(LogLevel.Info, 'JsonFileIpc run finished');
        end
    end
end
