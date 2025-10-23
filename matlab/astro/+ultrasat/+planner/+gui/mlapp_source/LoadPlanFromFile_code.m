classdef LoadPlanFromFile < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                matlab.ui.Figure
        BrowseButton            matlab.ui.control.Button
        EnterfilenametoloadfromorpastetextbelowLabel  matlab.ui.control.Label
        FileNameEditField       matlab.ui.control.EditField
        FileNameEditFieldLabel  matlab.ui.control.Label
        Panel_2                 matlab.ui.container.Panel
        LoadPlanFromFileLabel   matlab.ui.control.Label
        Panel                   matlab.ui.container.Panel
        HelpButton              matlab.ui.control.Button
        CancelButton            matlab.ui.control.Button
        LoadButton              matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % LoadPlanFromFile App
            %
            % This app allows users to load an observation plan from a `.mat` file.
            % The plan is loaded as a single MATLAB object for further processing.
            % The actual load operation is in PlannerMain.loadPlanFromFile().
            %
            % Features:
            % - Supports manual filename entry or file browser selection.
            % - Validates the file before loading.
            % - Notifies the user if the specified file is missing.
        end
    end    


    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the operation ('Load' or 'Cancel')
        FileName        % Selected file name for loading the plan
        Folder          % Default folder path for file selection
    end
        

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: LoadButton
        function LoadButtonPushed(app, event)
            % Handles the load button event to retrieve a plan from a file.
            %
            % - Checks if the filename is valid.
            % - Updates the status and resumes execution if loading is successful.            
            % - The actual load operation is in PlannerMain.loadPlanFromFile().            
            app.Status = [];
            app.FileName = [];

            FName = app.FileNameEditField.Value;
            if ~isempty(FName)
                if isfile(FName)
                    app.FileName = FName;
                    app.Status = 'Load';
                    uiresume(app.UIFigure);
                else
                    app.MainModule.AppUtils.msgError('Cannot find the specified file', 'File not found')
                end
            end
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);
        end

        % Button pushed function: BrowseButton
        function BrowseButtonPushed(app, event)
            % Opens a file browser to select a file for loading.
            %
            % - Launches an 'Open File' dialog for selecting a '.mat' file.
            % - Updates the file name field with the selected path.            
            [file, path] = uigetfile(...
                {'*.mat', 'MAT Files (*.mat)'; ...
                '*.*', 'All Files (*.*)'}, ...
                'Select a File', app.Folder);

            if file ~= 0
                app.FileNameEditField.Value = fullfile(path, file);
            end            
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('load_plan_from_file');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 583 248];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [15 15 556 57];

            % Create LoadButton
            app.LoadButton = uibutton(app.Panel, 'push');
            app.LoadButton.ButtonPushedFcn = createCallbackFcn(app, @LoadButtonPushed, true);
            app.LoadButton.FontWeight = 'bold';
            app.LoadButton.FontColor = [0 0.4471 0.7412];
            app.LoadButton.Position = [135 9 85 39];
            app.LoadButton.Text = 'Load';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [233 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [330 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [15 207 556 33];

            % Create LoadPlanFromFileLabel
            app.LoadPlanFromFileLabel = uilabel(app.Panel_2);
            app.LoadPlanFromFileLabel.HorizontalAlignment = 'center';
            app.LoadPlanFromFileLabel.FontSize = 18;
            app.LoadPlanFromFileLabel.FontWeight = 'bold';
            app.LoadPlanFromFileLabel.Position = [9 0 531 33];
            app.LoadPlanFromFileLabel.Text = 'Load Plan From File';

            % Create FileNameEditFieldLabel
            app.FileNameEditFieldLabel = uilabel(app.UIFigure);
            app.FileNameEditFieldLabel.HorizontalAlignment = 'right';
            app.FileNameEditFieldLabel.Position = [24 135 60 22];
            app.FileNameEditFieldLabel.Text = 'File Name';

            % Create FileNameEditField
            app.FileNameEditField = uieditfield(app.UIFigure, 'text');
            app.FileNameEditField.Position = [99 135 389 22];
            app.FileNameEditField.Value = 'C:/AstroPack/data/ULTRASAT/my_plan_1.mat';

            % Create EnterfilenametoloadfromorpastetextbelowLabel
            app.EnterfilenametoloadfromorpastetextbelowLabel = uilabel(app.UIFigure);
            app.EnterfilenametoloadfromorpastetextbelowLabel.FontWeight = 'bold';
            app.EnterfilenametoloadfromorpastetextbelowLabel.FontColor = [0 0 1];
            app.EnterfilenametoloadfromorpastetextbelowLabel.Position = [19 178 346 22];
            app.EnterfilenametoloadfromorpastetextbelowLabel.Text = 'Plan will be loaded from .mat file as single MATLAB object.';

            % Create BrowseButton
            app.BrowseButton = uibutton(app.UIFigure, 'push');
            app.BrowseButton.ButtonPushedFcn = createCallbackFcn(app, @BrowseButtonPushed, true);
            app.BrowseButton.Position = [504 135 68 23];
            app.BrowseButton.Text = 'Browse...';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = LoadPlanFromFile(varargin)

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