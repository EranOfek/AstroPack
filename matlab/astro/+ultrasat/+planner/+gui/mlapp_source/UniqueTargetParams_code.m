classdef UniqueTargetParams < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                     matlab.ui.Figure
        UniqueTargetIndexEditField   matlab.ui.control.EditField
        UniqueTargetIndexEditFieldLabel  matlab.ui.control.Label
        ReadonlyParametersPanel      matlab.ui.container.Panel
        A_UEditField                 matlab.ui.control.EditField
        A_UEditFieldLabel            matlab.ui.control.Label
        CalObjEditField              matlab.ui.control.EditField
        CalObjEditFieldLabel         matlab.ui.control.Label
        HealpixArrayEditField        matlab.ui.control.EditField
        HealpixArrayEditFieldLabel   matlab.ui.control.Label
        FieldObjEditField            matlab.ui.control.EditField
        FieldObjEditFieldLabel       matlab.ui.control.Label
        ExtSurveysEditField          matlab.ui.control.EditField
        ExtSurveysEditFieldLabel     matlab.ui.control.Label
        RefImagesIDsEditField        matlab.ui.control.EditField
        RefimagesIDsEditFieldLabel   matlab.ui.control.Label
        EditableParametersPanel      matlab.ui.container.Panel
        DecEditField                 matlab.ui.control.EditField
        DecEditFieldLabel            matlab.ui.control.Label
        RAEditField                  matlab.ui.control.EditField
        RAEditFieldLabel             matlab.ui.control.Label
        NameEditField                matlab.ui.control.EditField
        NameEditFieldLabel           matlab.ui.control.Label
        Panel_2                      matlab.ui.container.Panel
        UniqueTargetPropertiesLabel  matlab.ui.control.Label
        Panel                        matlab.ui.container.Panel
        HelpButton                   matlab.ui.control.Button
        CancelButton                 matlab.ui.control.Button
        SaveButton                   matlab.ui.control.Button
    end

  methods (Static)
        function about()
            % UniqueTargetParams App
            %
            % This app displays and allows editing of parameters related to a 
            % unique astronomical target. Some parameters are editable, while 
            % others are read-only and derived from external data sources.
            % See PlannerMain.editUniqueTarget().
            %
            % Features:
            % - Displays target properties such as RA, Dec, and Name.
            % - Allows users to modify and save editable parameters.
            % - Shows read-only data, including calibration objects, surveys, and Healpix indices.
            % - Retrieves data from the main application module.
        end
    end


    properties (Access = public)
        MainModule          % Reference to the main application module
        Status              % Status of the operation ('Save' or 'Cancel')
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
        end

        % Button pushed function: SaveButton
        function SaveButtonPushed(app, event)
            app.Status = 'Save';
            uiresume(app.UIFigure);                        
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);                        
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('unique_target_params');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 632 591];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [10 14 618 54];

            % Create SaveButton
            app.SaveButton = uibutton(app.Panel, 'push');
            app.SaveButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.SaveButton.FontWeight = 'bold';
            app.SaveButton.FontColor = [0 0 1];
            app.SaveButton.Position = [178 8 85 39];
            app.SaveButton.Text = 'Save';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [283 8 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [387 8 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [8 547 611 36];

            % Create UniqueTargetPropertiesLabel
            app.UniqueTargetPropertiesLabel = uilabel(app.Panel_2);
            app.UniqueTargetPropertiesLabel.HorizontalAlignment = 'center';
            app.UniqueTargetPropertiesLabel.FontSize = 18;
            app.UniqueTargetPropertiesLabel.FontWeight = 'bold';
            app.UniqueTargetPropertiesLabel.Position = [10 6 588 24];
            app.UniqueTargetPropertiesLabel.Text = 'Unique Target Properties';

            % Create EditableParametersPanel
            app.EditableParametersPanel = uipanel(app.UIFigure);
            app.EditableParametersPanel.TitlePosition = 'centertop';
            app.EditableParametersPanel.Title = 'Editable Parameters';
            app.EditableParametersPanel.Position = [13 355 612 139];

            % Create NameEditFieldLabel
            app.NameEditFieldLabel = uilabel(app.EditableParametersPanel);
            app.NameEditFieldLabel.HorizontalAlignment = 'right';
            app.NameEditFieldLabel.Position = [36 83 37 22];
            app.NameEditFieldLabel.Text = 'Name';

            % Create NameEditField
            app.NameEditField = uieditfield(app.EditableParametersPanel, 'text');
            app.NameEditField.Position = [88 83 229 22];

            % Create RAEditFieldLabel
            app.RAEditFieldLabel = uilabel(app.EditableParametersPanel);
            app.RAEditFieldLabel.HorizontalAlignment = 'right';
            app.RAEditFieldLabel.Position = [48 49 25 22];
            app.RAEditFieldLabel.Text = 'RA';

            % Create RAEditField
            app.RAEditField = uieditfield(app.EditableParametersPanel, 'text');
            app.RAEditField.Position = [88 49 229 22];

            % Create DecEditFieldLabel
            app.DecEditFieldLabel = uilabel(app.EditableParametersPanel);
            app.DecEditFieldLabel.HorizontalAlignment = 'right';
            app.DecEditFieldLabel.Position = [47 15 26 22];
            app.DecEditFieldLabel.Text = 'Dec';

            % Create DecEditField
            app.DecEditField = uieditfield(app.EditableParametersPanel, 'text');
            app.DecEditField.Position = [88 15 229 22];

            % Create ReadonlyParametersPanel
            app.ReadonlyParametersPanel = uipanel(app.UIFigure);
            app.ReadonlyParametersPanel.TitlePosition = 'centertop';
            app.ReadonlyParametersPanel.Title = 'Read-only Parameters';
            app.ReadonlyParametersPanel.BackgroundColor = [0.902 0.902 0.902];
            app.ReadonlyParametersPanel.Position = [13 75 611 269];

            % Create RefimagesIDsEditFieldLabel
            app.RefimagesIDsEditFieldLabel = uilabel(app.ReadonlyParametersPanel);
            app.RefimagesIDsEditFieldLabel.HorizontalAlignment = 'right';
            app.RefimagesIDsEditFieldLabel.Position = [35 128 87 22];
            app.RefimagesIDsEditFieldLabel.Text = 'Ref images IDs';

            % Create RefImagesIDsEditField
            app.RefImagesIDsEditField = uieditfield(app.ReadonlyParametersPanel, 'text');
            app.RefImagesIDsEditField.Editable = 'off';
            app.RefImagesIDsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.RefImagesIDsEditField.Position = [137 128 229 22];

            % Create ExtSurveysEditFieldLabel
            app.ExtSurveysEditFieldLabel = uilabel(app.ReadonlyParametersPanel);
            app.ExtSurveysEditFieldLabel.HorizontalAlignment = 'right';
            app.ExtSurveysEditFieldLabel.Position = [53 88 69 22];
            app.ExtSurveysEditFieldLabel.Text = 'Ext Surveys';

            % Create ExtSurveysEditField
            app.ExtSurveysEditField = uieditfield(app.ReadonlyParametersPanel, 'text');
            app.ExtSurveysEditField.Editable = 'off';
            app.ExtSurveysEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ExtSurveysEditField.Position = [137 88 229 22];

            % Create FieldObjEditFieldLabel
            app.FieldObjEditFieldLabel = uilabel(app.ReadonlyParametersPanel);
            app.FieldObjEditFieldLabel.HorizontalAlignment = 'right';
            app.FieldObjEditFieldLabel.Position = [69 49 53 22];
            app.FieldObjEditFieldLabel.Text = 'Field Obj';

            % Create FieldObjEditField
            app.FieldObjEditField = uieditfield(app.ReadonlyParametersPanel, 'text');
            app.FieldObjEditField.Editable = 'off';
            app.FieldObjEditField.BackgroundColor = [1 0.9882 0.8196];
            app.FieldObjEditField.Position = [137 49 229 22];

            % Create HealpixArrayEditFieldLabel
            app.HealpixArrayEditFieldLabel = uilabel(app.ReadonlyParametersPanel);
            app.HealpixArrayEditFieldLabel.HorizontalAlignment = 'right';
            app.HealpixArrayEditFieldLabel.Position = [46 13 76 22];
            app.HealpixArrayEditFieldLabel.Text = 'Healpix Array';

            % Create HealpixArrayEditField
            app.HealpixArrayEditField = uieditfield(app.ReadonlyParametersPanel, 'text');
            app.HealpixArrayEditField.Editable = 'off';
            app.HealpixArrayEditField.BackgroundColor = [1 0.9882 0.8196];
            app.HealpixArrayEditField.Position = [137 13 229 22];

            % Create CalObjEditFieldLabel
            app.CalObjEditFieldLabel = uilabel(app.ReadonlyParametersPanel);
            app.CalObjEditFieldLabel.HorizontalAlignment = 'right';
            app.CalObjEditFieldLabel.Position = [79 166 42 22];
            app.CalObjEditFieldLabel.Text = 'CalObj';

            % Create CalObjEditField
            app.CalObjEditField = uieditfield(app.ReadonlyParametersPanel, 'text');
            app.CalObjEditField.Editable = 'off';
            app.CalObjEditField.BackgroundColor = [1 0.9882 0.8196];
            app.CalObjEditField.Position = [136 166 229 22];

            % Create A_UEditFieldLabel
            app.A_UEditFieldLabel = uilabel(app.ReadonlyParametersPanel);
            app.A_UEditFieldLabel.HorizontalAlignment = 'right';
            app.A_UEditFieldLabel.Position = [94 207 28 22];
            app.A_UEditFieldLabel.Text = 'A_U';

            % Create A_UEditField
            app.A_UEditField = uieditfield(app.ReadonlyParametersPanel, 'text');
            app.A_UEditField.Editable = 'off';
            app.A_UEditField.BackgroundColor = [1 0.9882 0.8196];
            app.A_UEditField.Position = [137 207 229 22];

            % Create UniqueTargetIndexEditFieldLabel
            app.UniqueTargetIndexEditFieldLabel = uilabel(app.UIFigure);
            app.UniqueTargetIndexEditFieldLabel.HorizontalAlignment = 'right';
            app.UniqueTargetIndexEditFieldLabel.Position = [22 513 112 22];
            app.UniqueTargetIndexEditFieldLabel.Text = 'Unique Target Index';

            % Create UniqueTargetIndexEditField
            app.UniqueTargetIndexEditField = uieditfield(app.UIFigure, 'text');
            app.UniqueTargetIndexEditField.Editable = 'off';
            app.UniqueTargetIndexEditField.BackgroundColor = [1 0.9882 0.8196];
            app.UniqueTargetIndexEditField.Position = [160 513 60 22];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = UniqueTargetParams(varargin)

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