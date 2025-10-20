classdef SavePlanToFile < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                matlab.ui.Figure
        BrowseButton            matlab.ui.control.Button
        EnterfilenametoloadfromorpastetextbelowLabel  matlab.ui.control.Label
        FileNameEditField       matlab.ui.control.EditField
        FileNameEditFieldLabel  matlab.ui.control.Label
        Panel_2                 matlab.ui.container.Panel
        SavePlanToFileLabel     matlab.ui.control.Label
        Panel                   matlab.ui.container.Panel
        CancelButton            matlab.ui.control.Button
        SaveButton              matlab.ui.control.Button
    end

    methods (Static)    
        function about()        
            % SavePlanToFile App
            %
            % This app allows users to save an observation plan as a '.mat' file.
            % The plan is stored as a single MATLAB object for easy retrieval.
            % The actual save operation is in PlannerMain.savePlanToFile().
            %
            % Features:
            % - Supports manual filename entry or file browser selection.
            % - Prevents accidental overwrites by confirming if the file exists.
            % - Saves the plan in a structured format for future use.
        end
    end


    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the operation ('Save' or 'Cancel')
        FileName        % Selected file name for saving the plan
        Folder          % Default folder path for saving the file
    end    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: SaveButton
        function SaveButtonPushed(app, event)
            % Handles the save button event to save the plan to a file.
            %
            % - Checks if the filename is valid.
            % - Prevents overwriting existing files without confirmation.
            % - Updates the status and resumes execution if saving is successful.            
            app.Status = [];
            app.FileName = [];

            FName = app.FileNameEditField.Value;
            if ~isempty(FName)
                if isfile(FName)
                    % Show message - 'File already exist'
                    if strcmp(app.MainModule.AppUtils.askYesNo('File already exist, overwrite?'), 'Yes')
                        app.FileName = FName;
                        app.Status = 'Save';
                        uiresume(app.UIFigure);                        
                    end
                else
                    app.FileName = FName;
                    app.Status = 'Save';
                    uiresume(app.UIFigure);
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
            % Opens a file browser to select the save location.
            %
            % - Launches a 'Save As' dialog for selecting a `.mat` file.
            % - Updates the file name field with the selected path.            
            [filename, pathname] = uiputfile('*.mat', 'Save As', app.Folder);
            
            % Check if the user pressed 'Cancel'
            if filename ~= 0
                fullFilePath = fullfile(pathname, filename);
                app.FileNameEditField.Value = fullFilePath;
            end			            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 584 259];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [8 16 566 54];

            % Create SaveButton
            app.SaveButton = uibutton(app.Panel, 'push');
            app.SaveButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.SaveButton.FontWeight = 'bold';
            app.SaveButton.FontColor = [0 0.4471 0.7412];
            app.SaveButton.Position = [160 6 85 39];
            app.SaveButton.Text = 'Save';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [325 6 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [8 218 566 33];

            % Create SavePlanToFileLabel
            app.SavePlanToFileLabel = uilabel(app.Panel_2);
            app.SavePlanToFileLabel.HorizontalAlignment = 'center';
            app.SavePlanToFileLabel.FontSize = 18;
            app.SavePlanToFileLabel.FontWeight = 'bold';
            app.SavePlanToFileLabel.Position = [10 0 537 33];
            app.SavePlanToFileLabel.Text = 'Save Plan To File';

            % Create FileNameEditFieldLabel
            app.FileNameEditFieldLabel = uilabel(app.UIFigure);
            app.FileNameEditFieldLabel.HorizontalAlignment = 'right';
            app.FileNameEditFieldLabel.Position = [24 143 60 22];
            app.FileNameEditFieldLabel.Text = 'File Name';

            % Create FileNameEditField
            app.FileNameEditField = uieditfield(app.UIFigure, 'text');
            app.FileNameEditField.Position = [99 143 387 22];
            app.FileNameEditField.Value = 'C:/AstroPack/data/ULTRASAT/my_plan_1.mat';

            % Create EnterfilenametoloadfromorpastetextbelowLabel
            app.EnterfilenametoloadfromorpastetextbelowLabel = uilabel(app.UIFigure);
            app.EnterfilenametoloadfromorpastetextbelowLabel.FontWeight = 'bold';
            app.EnterfilenametoloadfromorpastetextbelowLabel.FontColor = [0 0 1];
            app.EnterfilenametoloadfromorpastetextbelowLabel.Position = [19 178 325 22];
            app.EnterfilenametoloadfromorpastetextbelowLabel.Text = 'Plan will be saved to .mat file as single MATLAB object.';

            % Create BrowseButton
            app.BrowseButton = uibutton(app.UIFigure, 'push');
            app.BrowseButton.ButtonPushedFcn = createCallbackFcn(app, @BrowseButtonPushed, true);
            app.BrowseButton.Position = [498 143 68 23];
            app.BrowseButton.Text = 'Browse...';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = SavePlanToFile(varargin)

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