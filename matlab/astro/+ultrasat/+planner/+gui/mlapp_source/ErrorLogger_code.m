classdef ErrorLogger < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure        matlab.ui.Figure
        Panel_3         matlab.ui.container.Panel
        TextArea        matlab.ui.control.TextArea
        Label           matlab.ui.control.Label
        Panel_2         matlab.ui.container.Panel
        ShowLogButton   matlab.ui.control.Button
        HelpButton      matlab.ui.control.Button
        ClearLogButton  matlab.ui.control.Button
        ObservationPlannerErrorsExceptionsLabel  matlab.ui.control.Label
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

        % Close request function: UIFigure
        function UIFigureCloseRequest(app, event)
            app.UIFigure.Visible = 'off';               
        end

        % Button pushed function: ClearLogButton
        function ClearLogButtonPushed(app, event)
            app.TextArea.Value = '';
            drawnow;  % ensures the UI updates immediately
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('error_log');
        end

        % Button pushed function: ShowLogButton
        function ShowLogButtonPushed(app, event)
            app.MainModule.MainApp.showLogger();
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 934 606];
            app.UIFigure.Name = 'MATLAB App';
            app.UIFigure.CloseRequestFcn = createCallbackFcn(app, @UIFigureCloseRequest, true);

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [1 0 0];
            app.Panel_2.Position = [6 564 923 36];

            % Create ObservationPlannerErrorsExceptionsLabel
            app.ObservationPlannerErrorsExceptionsLabel = uilabel(app.Panel_2);
            app.ObservationPlannerErrorsExceptionsLabel.HorizontalAlignment = 'center';
            app.ObservationPlannerErrorsExceptionsLabel.FontSize = 18;
            app.ObservationPlannerErrorsExceptionsLabel.FontWeight = 'bold';
            app.ObservationPlannerErrorsExceptionsLabel.FontColor = [1 1 1];
            app.ObservationPlannerErrorsExceptionsLabel.Position = [9 0 904 33];
            app.ObservationPlannerErrorsExceptionsLabel.Text = 'Observation Planner - Errors & Exceptions';

            % Create ClearLogButton
            app.ClearLogButton = uibutton(app.Panel_2, 'push');
            app.ClearLogButton.ButtonPushedFcn = createCallbackFcn(app, @ClearLogButtonPushed, true);
            app.ClearLogButton.Tooltip = {'Edit the selected target'};
            app.ClearLogButton.Position = [8 3 101 29];
            app.ClearLogButton.Text = 'Clear Log';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel_2, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [836 4 64 26];
            app.HelpButton.Text = 'Help';

            % Create ShowLogButton
            app.ShowLogButton = uibutton(app.Panel_2, 'push');
            app.ShowLogButton.ButtonPushedFcn = createCallbackFcn(app, @ShowLogButtonPushed, true);
            app.ShowLogButton.Tooltip = {'Edit the selected target'};
            app.ShowLogButton.Position = [120 3 101 29];
            app.ShowLogButton.Text = 'Show Log';

            % Create Panel_3
            app.Panel_3 = uipanel(app.UIFigure);
            app.Panel_3.Position = [6 8 923 552];

            % Create Label
            app.Label = uilabel(app.Panel_3);
            app.Label.HorizontalAlignment = 'right';
            app.Label.FontName = 'Courier New';
            app.Label.FontColor = [1 0 0];
            app.Label.Position = [7 520 25 22];
            app.Label.Text = '';

            % Create TextArea
            app.TextArea = uitextarea(app.Panel_3);
            app.TextArea.Editable = 'off';
            app.TextArea.FontName = 'Courier New';
            app.TextArea.FontColor = [1 0 0];
            app.TextArea.Position = [7 7 906 537];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = ErrorLogger(varargin)

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