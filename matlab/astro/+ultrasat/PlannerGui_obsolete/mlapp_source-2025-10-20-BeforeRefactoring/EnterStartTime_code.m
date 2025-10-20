classdef EnterStartTime < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                      matlab.ui.Figure
        GroupEditField                matlab.ui.control.EditField
        GroupNumberLabel              matlab.ui.control.Label
        UITable                       matlab.ui.control.Table
        GroupStartTimeEditField       matlab.ui.control.EditField
        GroupStartTimeEditFieldLabel  matlab.ui.control.Label
        Panel_2                       matlab.ui.container.Panel
        EnterDDTStartTimeLabel        matlab.ui.control.Label
        Panel                         matlab.ui.container.Panel
        CancelButton                  matlab.ui.control.Button
        OKButton                      matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % EnterStartTime App
            %
            % This app is used when adding (building) a DDT plan to specify the 
            % start time for a selected group. Users enter the start time manually 
            % before proceeding.
            %
            % Features:
            % - Allows users to input a start time for a DDT observation.
            % - Displays relevant group information in a table.
            % - Ensures proper formatting of the entered timestamp.
        end
    end

    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the operation, e.g., 'OK' or 'Cancel'
        StartTime       % Entered start time as a string
        StartDT         % Parsed start time as a datetime object
    end
       

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: OKButton
        function SaveButtonPushed(app, event)
            % Validates and stores the user-provided start time, 
            % resuming the UI on success.
            %
            % - Converts the entered time string to a datetime object.
            % - Sets the status to 'OK' if successful.
            % - Displays an error message if the format is invalid.

            app.Status = [];
            app.StartTime = [];
            app.StartDT = [];

            Timestamp = app.GroupStartTimeEditField.Value;
            try
                app.StartDT = datetime(Timestamp);
                app.StartTime = Timestamp;
                app.Status = 'OK';
                uiresume(app.UIFigure);
            catch ME
                % Show message 'Invalid time format, example: 2028-01-01 12:00:00');
                app.MainModule.AppUtils.msgError(ME.message);
            end
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);
        end

        % Callback function
        function PanelSizeChanged(app, event)

        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 578 587];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [14 12 556 57];

            % Create OKButton
            app.OKButton = uibutton(app.Panel, 'push');
            app.OKButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.OKButton.FontWeight = 'bold';
            app.OKButton.FontColor = [0 0.4471 0.7412];
            app.OKButton.Position = [145 9 85 39];
            app.OKButton.Text = 'OK';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [315 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [14 540 556 35];

            % Create EnterDDTStartTimeLabel
            app.EnterDDTStartTimeLabel = uilabel(app.Panel_2);
            app.EnterDDTStartTimeLabel.HorizontalAlignment = 'center';
            app.EnterDDTStartTimeLabel.FontSize = 18;
            app.EnterDDTStartTimeLabel.FontWeight = 'bold';
            app.EnterDDTStartTimeLabel.Position = [10 2 531 33];
            app.EnterDDTStartTimeLabel.Text = 'Enter DDT Start Time';

            % Create GroupStartTimeEditFieldLabel
            app.GroupStartTimeEditFieldLabel = uilabel(app.UIFigure);
            app.GroupStartTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.GroupStartTimeEditFieldLabel.Position = [32 490 96 22];
            app.GroupStartTimeEditFieldLabel.Text = 'Group Start Time';

            % Create GroupStartTimeEditField
            app.GroupStartTimeEditField = uieditfield(app.UIFigure, 'text');
            app.GroupStartTimeEditField.Position = [143 490 229 22];

            % Create UITable
            app.UITable = uitable(app.UIFigure);
            app.UITable.ColumnName = {'Column 1'; 'Column 2'; 'Column 3'; 'Column 4'};
            app.UITable.RowName = {};
            app.UITable.Position = [14 80 556 338];

            % Create GroupNumberLabel
            app.GroupNumberLabel = uilabel(app.UIFigure);
            app.GroupNumberLabel.HorizontalAlignment = 'right';
            app.GroupNumberLabel.Position = [44 449 84 22];
            app.GroupNumberLabel.Text = 'Group Number';

            % Create GroupEditField
            app.GroupEditField = uieditfield(app.UIFigure, 'text');
            app.GroupEditField.Editable = 'off';
            app.GroupEditField.BackgroundColor = [1 0.9882 0.8196];
            app.GroupEditField.Position = [143 449 72 22];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = EnterStartTime(varargin)

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