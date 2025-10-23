classdef SaveUniqueTargetsToFile < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure           matlab.ui.Figure
        BrowseButton       matlab.ui.control.Button
        TextArea           matlab.ui.control.TextArea
        TextAreaLabel      matlab.ui.control.Label
        EnterfilenametoloadfromorpastetextbelowLabel  matlab.ui.control.Label
        FileNameEditField  matlab.ui.control.EditField
        Panel_2            matlab.ui.container.Panel
        SaveUniqueTargetsToLocalFileLabel  matlab.ui.control.Label
        Panel              matlab.ui.container.Panel
        HelpButton         matlab.ui.control.Button
        CancelButton       matlab.ui.control.Button
        SaveButton         matlab.ui.control.Button
    end

    
    methods (Static)
        function about()
            % SaveUniqueTargetsToTextFile App
            %
            % This app provides a user interface to save the unique targets table
            % to a text or CSV file. The actual save operation is performed via 
            % PlannerMain.saveUniqueTargetsToFile().
            %
            % Features:
            % - Displays the unique targets table before saving.
            % - Allows the user to specify a filename and folder for saving.
            % - Provides a browse option to select a destination file.
            % - Notifies the user if the file already exists and prompts for confirmation.
        end
    end

    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the operation, e.g., 'Cancel' or 'Save'
        FileName        % Selected file name for loading targets
        Folder          % Default directory for file selection        
    end


    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: SaveButton
        function SaveButtonPushed(app, event)
            % Handles the save button event to store unique targets data in a file.
            %
            % - Retrieves the file name entered by the user.
            % - If the file name is empty, prompts the user to enter a valid file name.
            % - Checks if the file already exists:
            %   - If it exists, prompts the user to confirm overwriting.
            %   - If confirmed, proceeds with saving.
            % - If the file does not exist, saves the data immediately.
            % - Updates the status to 'Save' and resumes execution (return to PlannerMain).
            
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
            else
                % Show message 'File does not exist'
            end
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);
        end

        % Button pushed function: BrowseButton
        function BrowseButtonPushed(app, event)
            % Opens a file dialog for selecting a save location.
            %
            % - Launches a 'Save As' dialog allowing the user to choose a file name and location.
            % - Filters file selection to `.mat` files by default.
            % - Updates the file name field with the selected path.
            % - If the user cancels the selection, no changes are made.
            [filename, pathname] = uiputfile(...
                {'*.txt', 'Text Files (*.txt)'; ...
                '*.csv', 'CSV Files (*.csv)'; ...
                '*.*', 'All Files (*.*)'}, ..., 
                'Save As', app.Folder);
            
            % Check if the user pressed 'Cancel'
            if filename ~= 0
                fullFilePath = fullfile(pathname, filename);
                app.FileNameEditField.Value = fullFilePath;
            end			                        
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('save_unique_targets_to_file');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 921 698];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [23 14 886 57];

            % Create SaveButton
            app.SaveButton = uibutton(app.Panel, 'push');
            app.SaveButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.SaveButton.FontWeight = 'bold';
            app.SaveButton.FontColor = [0 0 1];
            app.SaveButton.Position = [299 9 85 39];
            app.SaveButton.Text = 'Save';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [401 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [505 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [19 655 890 33];

            % Create SaveUniqueTargetsToLocalFileLabel
            app.SaveUniqueTargetsToLocalFileLabel = uilabel(app.Panel_2);
            app.SaveUniqueTargetsToLocalFileLabel.HorizontalAlignment = 'center';
            app.SaveUniqueTargetsToLocalFileLabel.FontSize = 18;
            app.SaveUniqueTargetsToLocalFileLabel.FontWeight = 'bold';
            app.SaveUniqueTargetsToLocalFileLabel.Position = [9 0 873 33];
            app.SaveUniqueTargetsToLocalFileLabel.Text = 'Save Unique Targets To Local File';

            % Create FileNameEditField
            app.FileNameEditField = uieditfield(app.UIFigure, 'text');
            app.FileNameEditField.Position = [91 583 606 22];
            app.FileNameEditField.Value = 'C:/AstroPack/data/ULTRASAT/unique_targets.csv';

            % Create EnterfilenametoloadfromorpastetextbelowLabel
            app.EnterfilenametoloadfromorpastetextbelowLabel = uilabel(app.UIFigure);
            app.EnterfilenametoloadfromorpastetextbelowLabel.FontWeight = 'bold';
            app.EnterfilenametoloadfromorpastetextbelowLabel.FontColor = [0 0 1];
            app.EnterfilenametoloadfromorpastetextbelowLabel.Position = [19 621 546 22];
            app.EnterfilenametoloadfromorpastetextbelowLabel.Text = 'Enter file name to load from, or paste text below, with the same format as the example below.';

            % Create TextAreaLabel
            app.TextAreaLabel = uilabel(app.UIFigure);
            app.TextAreaLabel.HorizontalAlignment = 'right';
            app.TextAreaLabel.Position = [82 521 25 22];
            app.TextAreaLabel.Text = '';

            % Create TextArea
            app.TextArea = uitextarea(app.UIFigure);
            app.TextArea.Editable = 'off';
            app.TextArea.FontName = 'Monospaced';
            app.TextArea.BackgroundColor = [1 0.9882 0.8196];
            app.TextArea.Position = [16 99 893 469];

            % Create BrowseButton
            app.BrowseButton = uibutton(app.UIFigure, 'push');
            app.BrowseButton.ButtonPushedFcn = createCallbackFcn(app, @BrowseButtonPushed, true);
            app.BrowseButton.Position = [724 583 68 23];
            app.BrowseButton.Text = 'Browse...';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = SaveUniqueTargetsToFile(varargin)

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