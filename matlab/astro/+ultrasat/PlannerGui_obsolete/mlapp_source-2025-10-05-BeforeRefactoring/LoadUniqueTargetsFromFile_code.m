classdef LoadUniqueTargetsFromFile < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                matlab.ui.Figure
        BrowseButton            matlab.ui.control.Button
        LoadButton              matlab.ui.control.Button
        EnterfilenametoloadfromfileorpastetextbelowLabel_2  matlab.ui.control.Label
        ExampleTextArea         matlab.ui.control.TextArea
        ExampleTextAreaLabel    matlab.ui.control.Label
        TextArea                matlab.ui.control.TextArea
        Label                   matlab.ui.control.Label
        EnterfilenametoloadfromorpastetextbelowLabel  matlab.ui.control.Label
        FileNameEditField       matlab.ui.control.EditField
        FileNameEditFieldLabel  matlab.ui.control.Label
        Panel_2                 matlab.ui.container.Panel
        LoadUniqueTargetsListFromCSVFileLabel  matlab.ui.control.Label
        Panel                   matlab.ui.container.Panel
        HelpButton              matlab.ui.control.Button
        OKButton                matlab.ui.control.Button
        CancelButton            matlab.ui.control.Button
    end

    methods (Static)
        function about()
            % LoadUniqueTargetsFromTextFile App
            %
            % This app allows users to load a list of unique astronomical targets
            % from a '.csv' or '.txt' file or manually enter the data in a text field.
            % The actual load operation is done in PlannerMain.loadUniqueTargetsFromFile().
            %
            % Features:
            % - Supports file selection via a browser or manual entry.
            % - Parses CSV-formatted target lists with RA, Dec, and Name fields.
            % - Provides an example format for user reference.
        end
    end


    properties (Access = public)
        MainModule          % Reference to the main application module
        Status              % Operation status ('Load', 'Cancel')
        FileName            % Selected file name for loading targets
        Folder              % Default directory for file selection
        Text                % Loaded text content from the file or manual input
    end   

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: LoadButton
        function LoadButtonPushed(app, event)
            % Handles the load button event to read a target list from a file.
            %
            % - Validates the file name field.
            % - Loads the file content into the text area.
            % - Updates the 'FileName' and 'Text' properties.
            % - Displays an error message if the file is not found.            
            app.Status = [];
            app.FileName = [];
            app.Text = [];
            FName = app.FileNameEditField.Value;
            if ~isempty(FName)
                if isfile(FName)
                    app.FileName = FName;
                    AText = fileread(FName);
                    app.TextArea.Value = cellstr(AText);
                    app.Text = AText;  
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

        % Button pushed function: OKButton
        function OKButtonPushed(app, event)
            app.Text = strjoin(app.TextArea.Value, newline);
            app.Status = 'Load';
            uiresume(app.UIFigure);            
        end

        % Button pushed function: BrowseButton
        function BrowseButtonPushed(app, event)
            % Opens a file browser to select a file for loading.
            %
            % - Displays a file selection dialog for '.txt' and '.csv' files.
            % - Updates the file name field if a file is selected.            
            [file, path] = uigetfile(...
                {'*.txt', 'Text Files (*.txt)'; ...
                '*.csv', 'CSV Files (*.csv)'; ...
                '*.*', 'All Files (*.*)'}, ...
                'Select a File', app.Folder);

            if file ~= 0
                app.FileNameEditField.Value = fullfile(path, file);
            end
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('load_unique_targets_from_file');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 921 725];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [11 12 898 57];

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [442 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create OKButton
            app.OKButton = uibutton(app.Panel, 'push');
            app.OKButton.ButtonPushedFcn = createCallbackFcn(app, @OKButtonPushed, true);
            app.OKButton.FontWeight = 'bold';
            app.OKButton.FontColor = [0 0.4471 0.7412];
            app.OKButton.Position = [345 9 85 39];
            app.OKButton.Text = 'OK';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [541 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [10 682 904 33];

            % Create LoadUniqueTargetsListFromCSVFileLabel
            app.LoadUniqueTargetsListFromCSVFileLabel = uilabel(app.Panel_2);
            app.LoadUniqueTargetsListFromCSVFileLabel.HorizontalAlignment = 'center';
            app.LoadUniqueTargetsListFromCSVFileLabel.FontSize = 18;
            app.LoadUniqueTargetsListFromCSVFileLabel.FontWeight = 'bold';
            app.LoadUniqueTargetsListFromCSVFileLabel.Position = [9 0 863 33];
            app.LoadUniqueTargetsListFromCSVFileLabel.Text = 'Load Unique Targets List From CSV File';

            % Create FileNameEditFieldLabel
            app.FileNameEditFieldLabel = uilabel(app.UIFigure);
            app.FileNameEditFieldLabel.HorizontalAlignment = 'right';
            app.FileNameEditFieldLabel.Position = [26 604 60 22];
            app.FileNameEditFieldLabel.Text = 'File Name';

            % Create FileNameEditField
            app.FileNameEditField = uieditfield(app.UIFigure, 'text');
            app.FileNameEditField.Position = [101 604 512 22];
            app.FileNameEditField.Value = 'C:/AstroPack/data/ULTRASAT/unique_targets.csv';

            % Create EnterfilenametoloadfromorpastetextbelowLabel
            app.EnterfilenametoloadfromorpastetextbelowLabel = uilabel(app.UIFigure);
            app.EnterfilenametoloadfromorpastetextbelowLabel.FontWeight = 'bold';
            app.EnterfilenametoloadfromorpastetextbelowLabel.FontColor = [0 0 1];
            app.EnterfilenametoloadfromorpastetextbelowLabel.Position = [19 642 546 22];
            app.EnterfilenametoloadfromorpastetextbelowLabel.Text = 'Enter file name to load from, or paste text below, with the same format as the example below.';

            % Create Label
            app.Label = uilabel(app.UIFigure);
            app.Label.HorizontalAlignment = 'right';
            app.Label.Position = [82 521 25 22];
            app.Label.Text = '';

            % Create TextArea
            app.TextArea = uitextarea(app.UIFigure);
            app.TextArea.FontName = 'Monospaced';
            app.TextArea.Position = [16 195 893 373];

            % Create ExampleTextAreaLabel
            app.ExampleTextAreaLabel = uilabel(app.UIFigure);
            app.ExampleTextAreaLabel.HorizontalAlignment = 'right';
            app.ExampleTextAreaLabel.Position = [19 158 52 22];
            app.ExampleTextAreaLabel.Text = 'Example';

            % Create ExampleTextArea
            app.ExampleTextArea = uitextarea(app.UIFigure);
            app.ExampleTextArea.Editable = 'off';
            app.ExampleTextArea.BackgroundColor = [0.9412 0.9412 0.9412];
            app.ExampleTextArea.Position = [86 100 817 82];
            app.ExampleTextArea.Value = {'RA,Dec,Name'; '321.46,-76.71,"Note: Numeric must start with text"'; '54.18,-86.81,"First"'; '247.89,-79.28,"Second"'; '321.46,-76.71,"The 12"'};

            % Create EnterfilenametoloadfromfileorpastetextbelowLabel_2
            app.EnterfilenametoloadfromfileorpastetextbelowLabel_2 = uilabel(app.UIFigure);
            app.EnterfilenametoloadfromfileorpastetextbelowLabel_2.FontWeight = 'bold';
            app.EnterfilenametoloadfromfileorpastetextbelowLabel_2.FontColor = [1 0 0];
            app.EnterfilenametoloadfromfileorpastetextbelowLabel_2.Position = [87 77 882 22];
            app.EnterfilenametoloadfromfileorpastetextbelowLabel_2.Text = 'readtable expects uniform data types within each column, and it might misinterpret the file structure.';

            % Create LoadButton
            app.LoadButton = uibutton(app.UIFigure, 'push');
            app.LoadButton.ButtonPushedFcn = createCallbackFcn(app, @LoadButtonPushed, true);
            app.LoadButton.FontWeight = 'bold';
            app.LoadButton.FontColor = [0 0.4471 0.7412];
            app.LoadButton.Position = [769 599 85 33];
            app.LoadButton.Text = 'Load';

            % Create BrowseButton
            app.BrowseButton = uibutton(app.UIFigure, 'push');
            app.BrowseButton.ButtonPushedFcn = createCallbackFcn(app, @BrowseButtonPushed, true);
            app.BrowseButton.Position = [679 604 68 23];
            app.BrowseButton.Text = 'Browse...';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = LoadUniqueTargetsFromFile(varargin)

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