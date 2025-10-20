classdef AddUniqueTarget < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure              matlab.ui.Figure
        Panel_3               matlab.ui.container.Panel
        DecEditField          matlab.ui.control.EditField
        DecEditFieldLabel     matlab.ui.control.Label
        RAEditField           matlab.ui.control.EditField
        RAEditFieldLabel      matlab.ui.control.Label
        NameEditField         matlab.ui.control.EditField
        NameEditFieldLabel    matlab.ui.control.Label
        Panel_2               matlab.ui.container.Panel
        AddUniqueTargetLabel  matlab.ui.control.Label
        Panel                 matlab.ui.container.Panel
        HelpButton            matlab.ui.control.Button
        CancelButton          matlab.ui.control.Button
        AddButton             matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % AddUniqueTarget App
            %
            % This app allows users to add a unique target by specifying its 
            % name, right ascension (RA), and declination (Dec). 
            %
            % Features:
            % - Provides input fields for target parameters.
            % - Ensures unique target indexing.
            % - Modal dialog confirms user action before adding the target.
        end
    end

    properties (Access = public)
        MainModule  % Reference to the main application module
        Status      % Status of the operation, e.g., 'Add' or 'Cancel'
        Index       % Unique index for the target, auto-incremented
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            % Initialize the application, setting up the main module reference
            % and handling the auto-incrementing target index.            
            app.MainModule = MainModule;
            
            if isempty(app.Index)
                app.Index = 0; % Set to 0 if empty
            else
                app.Index = app.Index + 1; % Increment if not empty
            end            
        end

        % Button pushed function: AddButton
        function AddButtonPushed(app, event)
            app.Status = 'Add';
            uiresume(app.UIFigure);
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('add_unique_target');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 440 294];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [14 12 412 57];

            % Create AddButton
            app.AddButton = uibutton(app.Panel, 'push');
            app.AddButton.ButtonPushedFcn = createCallbackFcn(app, @AddButtonPushed, true);
            app.AddButton.FontWeight = 'bold';
            app.AddButton.FontColor = [0 0.4471 0.7412];
            app.AddButton.Position = [69 9 85 39];
            app.AddButton.Text = 'Add';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [168 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [267 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [14 251 412 33];

            % Create AddUniqueTargetLabel
            app.AddUniqueTargetLabel = uilabel(app.Panel_2);
            app.AddUniqueTargetLabel.HorizontalAlignment = 'center';
            app.AddUniqueTargetLabel.FontSize = 18;
            app.AddUniqueTargetLabel.FontWeight = 'bold';
            app.AddUniqueTargetLabel.Position = [8 1 396 33];
            app.AddUniqueTargetLabel.Text = 'Add Unique Target';

            % Create Panel_3
            app.Panel_3 = uipanel(app.UIFigure);
            app.Panel_3.TitlePosition = 'centertop';
            app.Panel_3.Position = [22 84 396 152];

            % Create NameEditFieldLabel
            app.NameEditFieldLabel = uilabel(app.Panel_3);
            app.NameEditFieldLabel.HorizontalAlignment = 'right';
            app.NameEditFieldLabel.Position = [36 96 37 22];
            app.NameEditFieldLabel.Text = 'Name';

            % Create NameEditField
            app.NameEditField = uieditfield(app.Panel_3, 'text');
            app.NameEditField.Position = [88 96 229 22];
            app.NameEditField.Value = 'MyUniqueTarget';

            % Create RAEditFieldLabel
            app.RAEditFieldLabel = uilabel(app.Panel_3);
            app.RAEditFieldLabel.HorizontalAlignment = 'right';
            app.RAEditFieldLabel.Position = [48 62 25 22];
            app.RAEditFieldLabel.Text = 'RA';

            % Create RAEditField
            app.RAEditField = uieditfield(app.Panel_3, 'text');
            app.RAEditField.Position = [88 62 229 22];
            app.RAEditField.Value = '215';

            % Create DecEditFieldLabel
            app.DecEditFieldLabel = uilabel(app.Panel_3);
            app.DecEditFieldLabel.HorizontalAlignment = 'right';
            app.DecEditFieldLabel.Position = [47 28 26 22];
            app.DecEditFieldLabel.Text = 'Dec';

            % Create DecEditField
            app.DecEditField = uieditfield(app.Panel_3, 'text');
            app.DecEditField.Position = [88 28 229 22];
            app.DecEditField.Value = '60';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = AddUniqueTarget(varargin)

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