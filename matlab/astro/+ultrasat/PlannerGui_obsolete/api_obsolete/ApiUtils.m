%==========================================================================
% ULTRASAT 
%
% File:    ApiUtils.m
% Author:  Chen Tishler
% Created: 20/02/2025
% Updated: 20/02/2025
%
%==========================================================================

classdef ApiUtils < handle
    % ApiUtils - Utility class 
    
    methods
        function obj = ApiUtils()
            % Constructor for ApiUtils
        end
    end


    methods (Static)
       
        function msglog(LogFileName, Prefix, varargin)
            % Logs a formatted message to the console and appends it to a log file.
            %
            % :param LogFileName: Path to the log file.
            % :param Prefix: Custom prefix for the log message.
            % :param varargin: Formatted message arguments.
            
            % Generate timestamp
            timestamp = datestr(now, 'yyyy-mm-dd HH:MM:SS');
        
            % Construct the log message
            logMessage = sprintf(varargin{:});
            fullMessage = sprintf('%s - %s: %s', timestamp, Prefix, logMessage);
        
            % Print to console
            fprintf('%s\n', fullMessage);
        
            % Append to log file if a valid filename is provided
            if ~isempty(LogFileName)
                fid = fopen(LogFileName, 'a');  % Open for appending
                if fid ~= -1
                    fprintf(fid, '%s\n', fullMessage);
                    fclose(fid);
                else
                    warning('Failed to write to log file: %s', LogFileName);
                end
            end
        end


        function logException(LogFileName, Prefix, Exception, IncludeStackTrace, varargin)
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
            timestamp = datestr(now, 'yyyy-mm-dd HH:MM:SS');
        
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
            if ~isempty(LogFileName)
                fid = fopen(LogFileName, 'a');  % Open for appending
                if fid ~= -1
                    fprintf(fid, '%s\n', fullMessage);
                    fclose(fid);
                else
                    warning('Failed to write to log file: %s', LogFileName);
                end
            end
        end       
       
    end
end
