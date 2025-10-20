classdef Logger < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure     matlab.ui.Figure
        Panel_3      matlab.ui.container.Panel
        TextArea     matlab.ui.control.TextArea
        Label        matlab.ui.control.Label
        Panel_2      matlab.ui.container.Panel
        LoggerLabel  matlab.ui.control.Label
    end

    methods (Static)
        function about()
            % Logger App
            %
            % This app provides a simple logging interface for displaying log messages.
            %
            % Features:
            % - Appends messages dynamically to a non-editable text area.
            % - Ensures UI updates immediately for real-time logging.
            % - Supports multi-line log storage and scrolling.
        end
    end    

    properties (Access = public)
        MainModule          % Reference to the main application module
    end
    

    methods (Access = public)
        
        function logMsg(app, message)
            % Appends a new log message to the TextArea and scrolls to the bottom.
            %
            % - Converts single-line logs to a cell array format if necessary.
            % - Maintains the previous logs while adding new entries.
            % - Uses drawnow to ensure immediate UI updates.
        
            % Get the current log
            currentLog = app.TextArea.Value;
        
            % Ensure currentLog is a cell array, convert single line to cell array
            if ischar(currentLog)
                currentLog = {currentLog}; 
            end
        
            % Append the new message
            updatedLog = [currentLog; message];
            app.TextArea.Value = updatedLog;
        
            % Scroll to the bottom, ensure UI updates immediately
            drawnow;
        end

    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 959 631];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [16 585 936 36];

            % Create LoggerLabel
            app.LoggerLabel = uilabel(app.Panel_2);
            app.LoggerLabel.HorizontalAlignment = 'center';
            app.LoggerLabel.FontSize = 18;
            app.LoggerLabel.FontWeight = 'bold';
            app.LoggerLabel.Position = [9 0 914 33];
            app.LoggerLabel.Text = 'Logger';

            % Create Panel_3
            app.Panel_3 = uipanel(app.UIFigure);
            app.Panel_3.Position = [19 13 933 563];

            % Create Label
            app.Label = uilabel(app.Panel_3);
            app.Label.HorizontalAlignment = 'right';
            app.Label.Position = [14 522 25 22];
            app.Label.Text = '';

            % Create TextArea
            app.TextArea = uitextarea(app.Panel_3);
            app.TextArea.Editable = 'off';
            app.TextArea.Position = [14 9 906 537];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Logger(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.UIFigure)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.UIFigure)
        end
    end
end