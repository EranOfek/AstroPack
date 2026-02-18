%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.Loggable.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 06/10/2025
% Description : Base class for logging.
%==========================================================================


classdef Loggable < handle
    %L OGGABLE A base class that provides logging to the console and a file.
    %   Classes that inherit from Loggable gain access to the msglog method,
    %   which handles timestamping, formatting, and writing log entries to
    %   both the MATLAB command window and a central log file.

    properties (Access = public)
        % The full path to the log file. Determined by the constructor.
        LogFilePath char = ''

        % A prefix string to identify which class is logging the message.
        % Subclasses should set this in their constructor.
        LogPrefix char = 'Loggable';

        LogBasePath char = ''
    end


    methods
        function obj = Loggable()
            % Construct an instance of the Loggable class.
            %   This constructor resolves the log file path and ensures the
            %   containing directory exists.

            obj.LogBasePath = getenv('SOC_PATH');
            if isempty(obj.LogBasePath)
                error('SOC_PATH env must be set (~/soc or c:/soc on Windows');
            end

            try
                logDir = fullfile(obj.resolveDefaultBasePath0(), 'log');
                if ~isfolder(logDir)
                    mkdir(logDir);
                end
                obj.LogFilePath = fullfile(logDir, 'matlab.log');
            catch ME
                warning('Could not resolve or create log directory. File logging will be disabled. Error: %s', ME.message);
                obj.LogFilePath = '';
            end
        end


        function msglog(obj, varargin)
            %MSGLOG Logs a formatted message to the console and a log file.
            %
            %   Usage:
            %       obj.msglog('Starting process...');
            %       obj.msglog('Found %d files in folder %s.', numFiles, folderName);
            %
            %   This method accepts variable arguments for fprintf-style formatting.

            % 1. Format the core message from the input arguments
            try
                coreMessage = sprintf(varargin{:});
            catch ME
                % If formatting fails, show the raw input instead
                disp('LOGGING ERROR: Could not format message. See details below.');
                disp(varargin);
                disp(ME);
                coreMessage = 'Error formatting log message.';
            end

            % 2. Construct the full, timestamped log entry
            dt = datetime('now', 'TimeZone', 'UTC');
            timestamp = datestr(dt, 'yyyy-mm-dd HH:MM:SS');
            fullLogEntry = sprintf('%s [%s] %s', timestamp, obj.LogPrefix, coreMessage);

            % 3. Print to the console
            fprintf('%s\n', fullLogEntry);

            NamespaceId = ultrasat.api.utils.PathUtils.NamespaceId();

            % @TDO - This is the module name for the log file
            moduleName = 'planner';
            fileName = 'planner';

            if isempty(NamespaceId)
                LogFileName = ultrasat.api.utils.PathUtils.getGlobalLogFilename(moduleName, fileName, 'DT', dt);
            else
                LogFileName = ultrasat.api.utils.PathUtils.getNamespaceLogFilename(moduleName, fileName, ...
                    'NamespaceId', NamespaceId, 'DT', dt);
            end

            % 4. Append to the log file, if the path is valid
            % if ~isempty(obj.LogFilePath)
            if ~isempty(LogFileName)
                try
                    LogFileName = fullfile(obj.LogBasePath, LogFileName);
                    logDir = fileparts(LogFileName);
                    if ~isfolder(logDir)
                        fprintf('Creating folder: %s\n', logDir);
                        mkdir(logDir);   % recursive, will create year/month etc.
                    end

                    % Open the file in append mode ('a') with UTF-8 encoding
                    fileID = fopen(LogFileName, 'a', 'n', 'UTF-8');
                    if fileID == -1
                        warning('Could not open log file for writing: %s', LogFileName);
                        return;
                    end
                    % Ensure the file is closed even if an error occurs
                    cleanup = onCleanup(@() fclose(fileID));

                    % Write the log entry followed by a newline character
                    fprintf(fileID, '%s\r\n', fullLogEntry);
                catch ME
                    warning('Failed to write to log file. Error: %s', ME.message);
                end
            end

            % 5. Check if message is error/exception → log to extra file
            obj.checkErrorAndLogExtra(fullLogEntry, dt);

            % 6. Forward all logs to LoggerApp
            ultrasat.api.utils.LogManager.logMessage(fullLogEntry);

            % 7. If this looks like an error or exception, also forward to ErrorLoggerApp and bring it to front
            lowerMsg = lower(coreMessage);
            if startsWith(lowerMsg, 'error') || contains(lowerMsg, 'exception') || ...
                    (contains(lowerMsg, 'error') && ~contains(lowerMsg, 'no error'))
                ultrasat.api.utils.LogManager.logError(fullLogEntry);
            end

        end


        function msgex(obj, msg, ME, varargin)
            % Log exception with message
            obj.logException(ME, false, varargin{:});
        end


        function logException(Exception, IncludeStackTrace, varargin)
            % Logs a formatted message and exception details to the console and a log file.
            %
            % :param LogFileName: Path to the log file.
            % :param Prefix: Custom prefix for the log message.
            % :param Exception: The caught exception object (from `catch ME`).
            % :param IncludeStackTrace: Boolean (true/false) to include stack trace.
            % :param varargin: Additional formatted message arguments.

            if nargin < 4
                IncludeStackTrace = true; % Default: include stack trace
            end

            % Generate timestamp
            dt = datetime('now', 'TimeZone', 'UTC');
            timestamp = datestr(dt, 'yyyy-mm-dd HH:MM:SS');

            % Construct the base log message
            logMessage = sprintf(varargin{:});

            % Exception details
            exceptionMsg = sprintf('EXCEPTION: %s | ID: %s', Exception.message, Exception.identifier);

            % If stack trace is enabled, format it
            stackTrace = '';
            if IncludeStackTrace
                for i = 1:numel(Exception.stack)
                    stackTrace = sprintf('%s\n%s: Line %d in %s', stackTrace, ...
                        Exception.stack(i).name, Exception.stack(i).line, Exception.stack(i).file);
                end
            end

            % Construct log output
            if IncludeStackTrace
                fullMessage = sprintf('%s - %s: %s\n%s\nSTACK TRACE:%s\n', ...
                    timestamp, Prefix, logMessage, exceptionMsg, stackTrace);
            else
                fullMessage = sprintf('%s - %s: %s | %s', ...
                    timestamp, Prefix, logMessage, exceptionMsg);
            end

            % Print to console
            fprintf('%s\n', fullMessage);

            % Append to log file
            obj.msglog(fullMessage);
        end


        function basePath = resolveDefaultBasePath0(obj)
            % Resolves the default base path for simulation data.
            % This function replicates the logic from the Python project to find the
            % correct directory for backend simulation files.
            %
            %   The logic is as follows:
            %   1. Check for the "SOC_PATH" environment variable. If it exists, use
            %      [SOC_PATH]/sim/backend.
            %   2. If not, check the operating system:
            %      - On Windows, default to "C:/soc/sim/backend".
            %      - On Linux/macOS, default to "~/soc/sim/backend" (home directory).
            %
            %   Returns:
            %       basePath (char): The resolved, absolute path.

            soc_env = getenv('SOC_PATH');

            if ~isempty(soc_env)
                % Use the path from the environment variable
                basePath = fullfile(soc_env, 'sim', 'backend');
            else
                % Fallback to OS-specific defaults
                if ispc
                    % Windows default
                    basePath = fullfile('C:', 'soc', 'sim', 'backend');
                else
                    % Linux, macOS, and other Unix-like systems default
                    home_dir = getenv('HOME');
                    if isempty(home_dir)
                        % A fallback just in case HOME is not set
                        home_dir = char(java.lang.System.getProperty('user.home'));
                    end
                    basePath = fullfile(home_dir, 'soc', 'sim', 'backend');
                end
            end
        end


        function checkErrorAndLogExtra(obj, logEntry, dt)
            % Checks if the log entry is error-related and writes to an extra error log file.

            lowerMsg = lower(logEntry);

            % Detect error/exception but ignore phrases like "no error"
            if startsWith(strtrim(lowerMsg), 'error') || ...
                contains(lowerMsg, 'exception') || contains(lowerMsg, 'fail') || ...
                contains(lowerMsg, 'trouble') || contains(lowerMsg, 'problem') || ...
                (contains(lowerMsg, 'error') && ~contains(lowerMsg, 'no error'))

                NamespaceId = ultrasat.api.utils.PathUtils.NamespaceId();
                moduleName = 'planner';
                fileName = 'planner_errors';

                if isempty(NamespaceId)
                    ErrorLogFileName = ultrasat.api.utils.PathUtils.getGlobalLogFilename(moduleName, fileName, 'DT', dt);
                else
                    ErrorLogFileName = ultrasat.api.utils.PathUtils.getNamespaceLogFilename(moduleName, fileName, ...
                        'NamespaceId', NamespaceId, 'DT', dt);
                end

                if ~isempty(ErrorLogFileName)
                    try
                        ErrorLogFileName = fullfile(obj.LogBasePath, ErrorLogFileName);
                        logDir = fileparts(ErrorLogFileName);
                        if ~isfolder(logDir)
                            mkdir(logDir);
                        end

                        fileID = fopen(ErrorLogFileName, 'a', 'n', 'UTF-8');
                        if fileID ~= -1
                            cleanup = onCleanup(@() fclose(fileID));
                            fprintf(fileID, '%s\r\n', logEntry);
                        end
                    catch ME
                        warning('Failed to write to error log file. Error: %s', ME.message);
                    end
                end
            end
        end

    end
end
