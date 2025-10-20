%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.LogManager.m
% Author      : Chen Tishler
% Created     : 16/10/2025
% Updated     : 20/10/2025
% Description : Global singleton to route log and error messages to GUI windows.
%==========================================================================

classdef LogManager
    % Global singleton to route log and error messages to GUI windows.

    methods (Static)
        function registerLoggerApps(loggerApp, errorLoggerApp)
            % Register the logger and error logger apps once at startup
            ultrasat.api.LogManager.persistentLoggerApp(loggerApp);
            ultrasat.api.LogManager.persistentErrorLoggerApp(errorLoggerApp);
        end


        function logMessage(message)
            % Forward message to LoggerApp, if available
            loggerApp = ultrasat.api.LogManager.persistentLoggerApp();
            if ~isempty(loggerApp) && isvalid(loggerApp)
                loggerApp.logMsg(message);
            end
        end


        function logError(message)
            % Forward message to ErrorLoggerApp, if available
            errorLoggerApp = ultrasat.api.LogManager.persistentErrorLoggerApp();
            if ~isempty(errorLoggerApp) && isvalid(errorLoggerApp)
                errorLoggerApp.logMsg(message);

                % Bring error window to front on error
                errorLoggerApp.UIFigure.Visible = 'on';
                figure(errorLoggerApp.UIFigure);
            end
        end
    end


    methods (Static, Access = private)
        function result = persistentLoggerApp(value)
            % Get (or set) the persistent 'staticLoggerApp'
            % This is the pattern in matlab to have class static properties
            persistent staticLoggerApp
            if nargin > 0 && ~isempty(value)
                staticLoggerApp = value;
            end
            result = staticLoggerApp;
        end


        function result = persistentErrorLoggerApp(value)
            % Get (or set) the persistent 'staticErrorLoggerApp'
            % This is the pattern in matlab to have class static properties
            persistent staticErrorLoggerApp
            if nargin > 0 && ~isempty(value)
                staticErrorLoggerApp = value;
            end
            result = staticErrorLoggerApp;
        end
    end
end
