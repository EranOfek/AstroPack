%==========================================================================
% ULTRASAT 
%
% File:   ultrasat.MissionClientBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 16/02/2025
%
%==========================================================================

classdef Loggable < handle
    %LOGGABLE A base class that provides logging to the console and a file.
    %   Classes that inherit from Loggable gain access to the msglog method,
    %   which handles timestamping, formatting, and writing log entries to
    %   both the MATLAB command window and a central log file.

    properties (Access = protected)
        % The full path to the log file. Determined by the constructor.
        LogFilePath char = ''

        % A prefix string to identify which class is logging the message.
        % Subclasses should set this in their constructor.
        LogPrefix char = '[Loggable]'
    end


    methods
        function obj = Loggable()
            %LOGGABLE Construct an instance of the Loggable class.
            %   This constructor resolves the log file path and ensures the
            %   containing directory exists.
            try
                logDir = obj.resolveDefaultBasePath0();
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
            timestamp = datestr(now, 'yyyy-mm-dd HH:MM:SS');
            fullLogEntry = sprintf('%s %s %s', timestamp, obj.LogPrefix, coreMessage);

            % 3. Print to the console
            fprintf('%s\n', fullLogEntry);

            % 4. Append to the log file, if the path is valid
            if ~isempty(obj.LogFilePath)
                try
                    % Open the file in append mode ('a') with UTF-8 encoding
                    fileID = fopen(obj.LogFilePath, 'a', 'n', 'UTF-8');
                    if fileID == -1
                        warning('Could not open log file for writing: %s', obj.LogFilePath);
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


    end
end
