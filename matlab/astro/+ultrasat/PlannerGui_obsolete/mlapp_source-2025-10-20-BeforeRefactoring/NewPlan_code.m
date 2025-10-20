classdef NewPlan < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                       matlab.ui.Figure
        PlanParametersPanel            matlab.ui.container.Panel
        PlannerNameEditField           matlab.ui.control.EditField
        PlannerNameEditFieldLabel      matlab.ui.control.Label
        EndTimeEditField               matlab.ui.control.EditField
        EndTimeEditFieldLabel          matlab.ui.control.Label
        StartTimeEditField             matlab.ui.control.EditField
        StartTimeEditFieldLabel        matlab.ui.control.Label
        TitleEditField                 matlab.ui.control.EditField
        TitleEditFieldLabel            matlab.ui.control.Label
        Panel_2                        matlab.ui.container.Panel
        CreateNewObservationPlanLabel  matlab.ui.control.Label
        Panel                          matlab.ui.container.Panel
        HelpButton                     matlab.ui.control.Button
        CancelButton                   matlab.ui.control.Button
        CreateButton                   matlab.ui.control.Button
        PlanTypeButtonGroup            matlab.ui.container.ButtonGroup
        TOOButton                      matlab.ui.control.RadioButton
        AllSSButton                    matlab.ui.control.RadioButton
        DDTButton                      matlab.ui.control.RadioButton
        LCSButton                      matlab.ui.control.RadioButton
        HCSButton                      matlab.ui.control.RadioButton
    end

    methods (Static)
        function about()
            % NewPlan App
            %
            % This app allows users to create a new observation plan by specifying 
            % its title, time range, planner name, and plan type.
            %
            % Features:
            % - Supports different plan types: HCS, LCS, DDT, AllSS, and TOO.
            % - Ensures mandatory fields like Planner Name are filled before creation.
            % - Provides a modal interface to confirm plan details before submission.
        end
    end

    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the operation, e.g., 'Cancel' or 'Create'
        PlanType        % Selected plan type, e.g., 'HCS', 'LCS', 'DDT', etc.
        Index           % Counter for naming newly created plans
    end    

    methods (Access = public)
        function beforeShow(app)
            % Prepares the UI before displaying the modal window.
            % Called from PlannerMain.showModal()
            %
            % - Disables the 'Create' button if the planner name is empty.
            % - Ensures correct default settings before user input.            
            
            if isempty(app.PlannerNameEditField.Value)
                app.CreateButton.Enable = false;
            else
                app.CreateButton.Enable = true; 
            end
        end
    end


    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            % Initializes the app by setting the main module reference and default values.
            %
            % - Sets up initial plan naming based on an index.
            % - Defines default start and end times.

            app.MainModule = MainModule;
            app.MainModule.AppUtils.center(app);

            if isempty(app.Index)
                app.Index = 1; % Set to 0 if empty
            else
                app.Index = app.Index + 1; % Increment if not empty
            end

            % Set default values for fields? here or by caller?
            app.TitleEditField.Value = sprintf('MyPlan_%d', app.Index);
            app.StartTimeEditField.Value = '2028-01-01 12:00:00';
            app.EndTimeEditField.Value = '2028-07-01 12:00:00';
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);
        end

        % Button pushed function: CreateButton
        function CreateButtonPushed(app, event)
            % Sets the selected plan type and confirms plan creation.
            %
            % - Determines the selected plan type from the button group.
            % - Sets the status to 'Create' and resumes the UI.

            app.PlanType = [];                                   
            if app.HCSButton.Value
                app.PlanType = 'HCS';
            end
            if app.LCSButton.Value
                app.PlanType = 'LCS';                
            end
            if app.DDTButton.Value
                app.PlanType = 'DDT';                
            end
            if app.AllSSButton.Value
                app.PlanType = 'AllSS';
            end
            if app.TOOButton.Value
                app.PlanType = 'TOO';
            end           

            app.Status = 'Create';
            uiresume(app.UIFigure);
        end

        % Value changed function: PlannerNameEditField
        function PlannerNameEditFieldValueChanged(app, event)
            % Enables or disables the 'Create' button based on planner name input            
            value = app.PlannerNameEditField.Value;
            value = strip(value);
            app.CreateButton.Enable = ~isempty(value);
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('new_plan');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 633 368];
            app.UIFigure.Name = 'MATLAB App';
            app.UIFigure.Resize = 'off';

            % Create PlanTypeButtonGroup
            app.PlanTypeButtonGroup = uibuttongroup(app.UIFigure);
            app.PlanTypeButtonGroup.TitlePosition = 'centertop';
            app.PlanTypeButtonGroup.Title = 'Plan Type';
            app.PlanTypeButtonGroup.Position = [17 81 169 228];

            % Create HCSButton
            app.HCSButton = uiradiobutton(app.PlanTypeButtonGroup);
            app.HCSButton.Text = 'HCS';
            app.HCSButton.Position = [16 169 58 22];
            app.HCSButton.Value = true;

            % Create LCSButton
            app.LCSButton = uiradiobutton(app.PlanTypeButtonGroup);
            app.LCSButton.Text = 'LCS';
            app.LCSButton.Position = [15 135 65 22];

            % Create DDTButton
            app.DDTButton = uiradiobutton(app.PlanTypeButtonGroup);
            app.DDTButton.Text = 'DDT';
            app.DDTButton.Position = [17 103 65 22];

            % Create AllSSButton
            app.AllSSButton = uiradiobutton(app.PlanTypeButtonGroup);
            app.AllSSButton.Text = 'AllSS';
            app.AllSSButton.Position = [16 75 65 22];

            % Create TOOButton
            app.TOOButton = uiradiobutton(app.PlanTypeButtonGroup);
            app.TOOButton.Text = 'TOO';
            app.TOOButton.Position = [16 40 65 22];

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [17 9 603 57];

            % Create CreateButton
            app.CreateButton = uibutton(app.Panel, 'push');
            app.CreateButton.ButtonPushedFcn = createCallbackFcn(app, @CreateButtonPushed, true);
            app.CreateButton.FontWeight = 'bold';
            app.CreateButton.FontColor = [0 0.4471 0.7412];
            app.CreateButton.Position = [169 8 85 39];
            app.CreateButton.Text = 'Create';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [275 8 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [384 8 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [16 326 599 33];

            % Create CreateNewObservationPlanLabel
            app.CreateNewObservationPlanLabel = uilabel(app.Panel_2);
            app.CreateNewObservationPlanLabel.HorizontalAlignment = 'center';
            app.CreateNewObservationPlanLabel.FontSize = 18;
            app.CreateNewObservationPlanLabel.FontWeight = 'bold';
            app.CreateNewObservationPlanLabel.Position = [8 0 579 33];
            app.CreateNewObservationPlanLabel.Text = 'Create New Observation Plan';

            % Create PlanParametersPanel
            app.PlanParametersPanel = uipanel(app.UIFigure);
            app.PlanParametersPanel.TitlePosition = 'centertop';
            app.PlanParametersPanel.Title = 'Plan Parameters';
            app.PlanParametersPanel.Position = [218 84 402 225];

            % Create TitleEditFieldLabel
            app.TitleEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.TitleEditFieldLabel.HorizontalAlignment = 'right';
            app.TitleEditFieldLabel.Position = [89 169 27 22];
            app.TitleEditFieldLabel.Text = 'Title';

            % Create TitleEditField
            app.TitleEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.TitleEditField.Position = [131 169 229 22];

            % Create StartTimeEditFieldLabel
            app.StartTimeEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.StartTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.StartTimeEditFieldLabel.Position = [57 135 60 22];
            app.StartTimeEditFieldLabel.Text = 'Start Time';

            % Create StartTimeEditField
            app.StartTimeEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.StartTimeEditField.Position = [132 135 229 22];

            % Create EndTimeEditFieldLabel
            app.EndTimeEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.EndTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.EndTimeEditFieldLabel.Position = [62 103 56 22];
            app.EndTimeEditFieldLabel.Text = 'End Time';

            % Create EndTimeEditField
            app.EndTimeEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.EndTimeEditField.Position = [133 103 229 22];

            % Create PlannerNameEditFieldLabel
            app.PlannerNameEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.PlannerNameEditFieldLabel.HorizontalAlignment = 'right';
            app.PlannerNameEditFieldLabel.Position = [36 51 82 22];
            app.PlannerNameEditFieldLabel.Text = 'Planner Name';

            % Create PlannerNameEditField
            app.PlannerNameEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.PlannerNameEditField.ValueChangedFcn = createCallbackFcn(app, @PlannerNameEditFieldValueChanged, true);
            app.PlannerNameEditField.Placeholder = 'Planner name, press <Enter> to apply';
            app.PlannerNameEditField.Position = [133 51 229 22];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = NewPlan(varargin)

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