classdef DuplicatePlan < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                 matlab.ui.Figure
        Label                    matlab.ui.control.Label
        Panel_3                  matlab.ui.container.Panel
        UserNameEditField        matlab.ui.control.EditField
        UserNameEditFieldLabel   matlab.ui.control.Label
        PlanTitleEditField       matlab.ui.control.EditField
        PlanTitleEditFieldLabel  matlab.ui.control.Label
        Panel_2                  matlab.ui.container.Panel
        DuplicateCurrentObservingProgramLabel  matlab.ui.control.Label
        Panel                    matlab.ui.container.Panel
        HelpButton               matlab.ui.control.Button
        CancelButton             matlab.ui.control.Button
        DuplicateButton          matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % DuplicatePlan App
            %
            % This app provides a user interface for entering details when duplicating 
            % an observation plan. Users specify a new plan title and user name.
            % The actual duplication process is handled in PlannerMain.
            %
            % Features:
            % - Allows users to enter a new plan title.
            % - Enables assignment of a different user name.
            % - Works as a modal dialog for user confirmation.
        end
    end

    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the duplication process, e.g., 'Duplicate' or 'Cancel'
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: DuplicateButton
        function DuplicateButtonPushed(app, event)
            app.Status = 'Duplicate';
            uiresume(app.UIFigure);
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('duplicate_plan');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 504 335];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [14 8 477 57];

            % Create DuplicateButton
            app.DuplicateButton = uibutton(app.Panel, 'push');
            app.DuplicateButton.ButtonPushedFcn = createCallbackFcn(app, @DuplicateButtonPushed, true);
            app.DuplicateButton.FontWeight = 'bold';
            app.DuplicateButton.FontColor = [0 0 1];
            app.DuplicateButton.Position = [98 9 85 39];
            app.DuplicateButton.Text = 'Duplicate';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [197 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [300 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [14 292 477 33];

            % Create DuplicateCurrentObservingProgramLabel
            app.DuplicateCurrentObservingProgramLabel = uilabel(app.Panel_2);
            app.DuplicateCurrentObservingProgramLabel.HorizontalAlignment = 'center';
            app.DuplicateCurrentObservingProgramLabel.FontSize = 18;
            app.DuplicateCurrentObservingProgramLabel.FontWeight = 'bold';
            app.DuplicateCurrentObservingProgramLabel.Position = [8 1 458 33];
            app.DuplicateCurrentObservingProgramLabel.Text = 'Duplicate Current Observing Program';

            % Create Panel_3
            app.Panel_3 = uipanel(app.UIFigure);
            app.Panel_3.TitlePosition = 'centertop';
            app.Panel_3.Position = [14 93 477 151];

            % Create PlanTitleEditFieldLabel
            app.PlanTitleEditFieldLabel = uilabel(app.Panel_3);
            app.PlanTitleEditFieldLabel.HorizontalAlignment = 'right';
            app.PlanTitleEditFieldLabel.Position = [19 95 54 22];
            app.PlanTitleEditFieldLabel.Text = 'Plan Title';

            % Create PlanTitleEditField
            app.PlanTitleEditField = uieditfield(app.Panel_3, 'text');
            app.PlanTitleEditField.Placeholder = 'New plan title';
            app.PlanTitleEditField.Position = [88 95 229 22];
            app.PlanTitleEditField.Value = 'New plan title';

            % Create UserNameEditFieldLabel
            app.UserNameEditFieldLabel = uilabel(app.Panel_3);
            app.UserNameEditFieldLabel.HorizontalAlignment = 'right';
            app.UserNameEditFieldLabel.Position = [8 55 66 22];
            app.UserNameEditFieldLabel.Text = 'User Name';

            % Create UserNameEditField
            app.UserNameEditField = uieditfield(app.Panel_3, 'text');
            app.UserNameEditField.Position = [89 55 229 22];
            app.UserNameEditField.Value = 'MyUniqueTarget';

            % Create Label
            app.Label = uilabel(app.UIFigure);
            app.Label.FontWeight = 'bold';
            app.Label.FontColor = [0 0 1];
            app.Label.Position = [14 260 485 22];
            app.Label.Text = 'Duplicate behaves as Save As operation, and will open the new plan in draft mode.';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = DuplicatePlan(varargin)

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