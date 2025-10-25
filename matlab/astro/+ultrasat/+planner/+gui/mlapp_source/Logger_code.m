classdef Logger < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                    matlab.ui.Figure
        Panel_3                     matlab.ui.container.Panel
        TextArea                    matlab.ui.control.TextArea
        Label                       matlab.ui.control.Label
        Panel_2                     matlab.ui.container.Panel
        ErrorLogButton              matlab.ui.control.Button
        ClearLogButton              matlab.ui.control.Button
        ObservationPlannerLogLabel  matlab.ui.control.Label
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

        % Button pushed function: ErrorLogButton
        function ErrorLogButtonPushed(app, event)
            app.MainModule.MainApp.showErrorLogger();
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 934 604];
            app.UIFigure.Name = 'MATLAB App';
            app.UIFigure.CloseRequestFcn = createCallbackFcn(app, @UIFigureCloseRequest, true);

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.6706 0.902 1];
            app.Panel_2.Position = [7 562 925 36];

            % Create ObservationPlannerLogLabel
            app.ObservationPlannerLogLabel = uilabel(app.Panel_2);
            app.ObservationPlannerLogLabel.HorizontalAlignment = 'center';
            app.ObservationPlannerLogLabel.FontSize = 18;
            app.ObservationPlannerLogLabel.FontWeight = 'bold';
            app.ObservationPlannerLogLabel.Position = [9 0 893 33];
            app.ObservationPlannerLogLabel.Text = 'Observation Planner - Log';

            % Create ClearLogButton
            app.ClearLogButton = uibutton(app.Panel_2, 'push');
            app.ClearLogButton.ButtonPushedFcn = createCallbackFcn(app, @ClearLogButtonPushed, true);
            app.ClearLogButton.Tooltip = {'Edit the selected target'};
            app.ClearLogButton.Position = [8 3 101 29];
            app.ClearLogButton.Text = 'Clear Log';

            % Create ErrorLogButton
            app.ErrorLogButton = uibutton(app.Panel_2, 'push');
            app.ErrorLogButton.ButtonPushedFcn = createCallbackFcn(app, @ErrorLogButtonPushed, true);
            app.ErrorLogButton.FontColor = [1 0 0];
            app.ErrorLogButton.Tooltip = {'Edit the selected target'};
            app.ErrorLogButton.Position = [121 3 101 29];
            app.ErrorLogButton.Text = 'Error Log';

            % Create Panel_3
            app.Panel_3 = uipanel(app.UIFigure);
            app.Panel_3.Position = [8 7 922 551];

            % Create Label
            app.Label = uilabel(app.Panel_3);
            app.Label.HorizontalAlignment = 'right';
            app.Label.Position = [13 514 25 22];
            app.Label.Text = '';

            % Create TextArea
            app.TextArea = uitextarea(app.Panel_3);
            app.TextArea.Editable = 'off';
            app.TextArea.FontName = 'Courier New';
            app.TextArea.Position = [7 7 906 537];

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