classdef PlanTargetParams < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                       matlab.ui.Figure
        PlanTargetIndexEditField       matlab.ui.control.EditField
        PlanTargetIndexEditFieldLabel  matlab.ui.control.Label
        UniqueTargetParamsPanel        matlab.ui.container.Panel
        ExpectedRollEditField          matlab.ui.control.EditField
        ExpectedRollEditField_3Label   matlab.ui.control.Label
        DecEditField                   matlab.ui.control.EditField
        DecEditFieldLabel              matlab.ui.control.Label
        RAEditField                    matlab.ui.control.EditField
        RAEditFieldLabel               matlab.ui.control.Label
        GroupEditField                 matlab.ui.control.EditField
        GroupEditFieldLabel            matlab.ui.control.Label
        UniqueTargetIndexEditField     matlab.ui.control.EditField
        UniqueTargetIndexEditField_3Label  matlab.ui.control.Label
        NameEditField                  matlab.ui.control.EditField
        NameEditField_3Label           matlab.ui.control.Label
        EditableParametersPanel        matlab.ui.container.Panel
        Panel_3                        matlab.ui.container.Panel
        Tile4CheckBox                  matlab.ui.control.CheckBox
        Tile3CheckBox                  matlab.ui.control.CheckBox
        Tile2CheckBox                  matlab.ui.control.CheckBox
        Tile1CheckBox                  matlab.ui.control.CheckBox
        TilesLabel                     matlab.ui.control.Label
        EpochsPerVisitEditField        matlab.ui.control.NumericEditField
        EpochsPerVisitEditFieldLabel   matlab.ui.control.Label
        ExposureTimeEditField          matlab.ui.control.NumericEditField
        ExposureTimeEditFieldLabel     matlab.ui.control.Label
        secondsLabel                   matlab.ui.control.Label
        OtherPanel                     matlab.ui.container.Panel
        OverlapTargetsEditField        matlab.ui.control.EditField
        OverlapTargetsEditFieldLabel   matlab.ui.control.Label
        LimMagEditField                matlab.ui.control.EditField
        LimMagEditFieldLabel           matlab.ui.control.Label
        ZodyEditField                  matlab.ui.control.EditField
        ZodyEditFieldLabel             matlab.ui.control.Label
        DistancePanel                  matlab.ui.container.Panel
        HardObsEditField               matlab.ui.control.EditField
        HardObsEditFieldLabel          matlab.ui.control.Label
        NoCommEditField                matlab.ui.control.EditField
        NoCommEditFieldLabel           matlab.ui.control.Label
        EarthDistEditField             matlab.ui.control.EditField
        EarthDistEditFieldLabel        matlab.ui.control.Label
        SunDistEditField               matlab.ui.control.EditField
        SunDistEditFieldLabel          matlab.ui.control.Label
        MoonDistEditField              matlab.ui.control.EditField
        MoonDistEditFieldLabel         matlab.ui.control.Label
        TimePanel                      matlab.ui.container.Panel
        SlewTimeBeforeEditField        matlab.ui.control.EditField
        SlewTimeBeforeEditFieldLabel   matlab.ui.control.Label
        TotalDurationEditField         matlab.ui.control.EditField
        TotalDurationEditFieldLabel    matlab.ui.control.Label
        MJDendEditField                matlab.ui.control.EditField
        MJDendEditFieldLabel           matlab.ui.control.Label
        MJDstartEditField              matlab.ui.control.EditField
        MJDstartEditFieldLabel         matlab.ui.control.Label
        EndTimeEditField               matlab.ui.control.EditField
        EndTimeEditFieldLabel          matlab.ui.control.Label
        StartTimeEditField             matlab.ui.control.EditField
        StartTimeEditFieldLabel        matlab.ui.control.Label
        Panel_2                        matlab.ui.container.Panel
        PlanTargetPropertiesLabel      matlab.ui.control.Label
        Panel                          matlab.ui.container.Panel
        EditButton                     matlab.ui.control.Button
        CancelButton                   matlab.ui.control.Button
        SaveButton                     matlab.ui.control.Button
    end

    
    methods (Static)
        function about()
            % PlanTargetParams App
            %
            % This app allows users to view and edit parameters of a specific 
            % plan target within an observation plan. Most fields are read-only, 
            % but a few parameters can be modified before saving.
            %
            % Features:
            % - Displays detailed target properties, including coordinates and expected roll.
            % - Allows editing of select observation parameters.
            % - Provides an option to enable or disable tiles for observation.
            % - Retrieves and updates data through the PlannerMain module.
            %
            % Editable Fields (All other fields are read-only):
            % - ExposureTimeEditField - Exposure Time
            % - EpochsPerVisitEditField - Epochs Per Visit
            % - Tile?CheckBox- Tile selection for observation
        end
    end


    properties (Access = public)
        MainModule          % Reference to the main application module
        Status              % Status of the operation ('Save' or 'Cancel')
    end

    
    methods (Access = public)
        
        function beforeShow(app)
            % Toggles the edit mode for editable fields.
            %
            % - Enables or disables fields that can be modified.
            % - Adjusts the background color to indicate editable state.            
            app.setEditMode(false);
        end


        function setEditMode(app, EditMode)
            % Toggles the edit mode for editable fields.
            %
            % - Enables or disables fields that can be modified.
            % - Adjusts the background color to indicate editable state.

            %app.setFieldEditMode(app.TilesEditField, EditMode);
            app.setEditable(app.EpochsPerVisitEditField, EditMode);
            app.setEditable(app.ExposureTimeEditField, EditMode);
            app.Tile1CheckBox.Enable = EditMode;
            app.Tile2CheckBox.Enable = EditMode;
            app.Tile3CheckBox.Enable = EditMode;
            app.Tile4CheckBox.Enable = EditMode;
        end


        function setEditable(app, Field, EditMode)
            % Sets the editability of a specific UI field.
            %
            % - Changes the field's editability status.
            % - Updates the background color to indicate whether it is editable.            
            if EditMode
                Field.Editable = true;
                Field.BackgroundColor = [1.0, 1.0, 1.0];
            else
                Field.Editable = false;
                Field.BackgroundColor = [1.00, 0.99, 0.82];
            end        
        end

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

        % Button pushed function: EditButton
        function EditButtonPushed(app, event)
            % Enables edit mode for the applicable fields.
            %
            % - Allows modification of specific observation parameters.            
            app.setEditMode(true);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 897 592];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [11 9 880 57];

            % Create SaveButton
            app.SaveButton = uibutton(app.Panel, 'push');
            app.SaveButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.SaveButton.FontWeight = 'bold';
            app.SaveButton.FontColor = [0 0 1];
            app.SaveButton.Position = [394 9 85 39];
            app.SaveButton.Text = 'Save';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [507 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create EditButton
            app.EditButton = uibutton(app.Panel, 'push');
            app.EditButton.ButtonPushedFcn = createCallbackFcn(app, @EditButtonPushed, true);
            app.EditButton.Position = [276 9 85 39];
            app.EditButton.Text = 'Edit';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [17 551 873 33];

            % Create PlanTargetPropertiesLabel
            app.PlanTargetPropertiesLabel = uilabel(app.Panel_2);
            app.PlanTargetPropertiesLabel.HorizontalAlignment = 'center';
            app.PlanTargetPropertiesLabel.FontSize = 18;
            app.PlanTargetPropertiesLabel.FontWeight = 'bold';
            app.PlanTargetPropertiesLabel.Position = [8 1 856 33];
            app.PlanTargetPropertiesLabel.Text = 'Plan Target Properties';

            % Create TimePanel
            app.TimePanel = uipanel(app.UIFigure);
            app.TimePanel.TitlePosition = 'centertop';
            app.TimePanel.Title = 'Time';
            app.TimePanel.Position = [24 82 348 248];

            % Create StartTimeEditFieldLabel
            app.StartTimeEditFieldLabel = uilabel(app.TimePanel);
            app.StartTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.StartTimeEditFieldLabel.Position = [15 182 60 22];
            app.StartTimeEditFieldLabel.Text = 'Start Time';

            % Create StartTimeEditField
            app.StartTimeEditField = uieditfield(app.TimePanel, 'text');
            app.StartTimeEditField.Editable = 'off';
            app.StartTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.StartTimeEditField.Position = [116 182 200 22];

            % Create EndTimeEditFieldLabel
            app.EndTimeEditFieldLabel = uilabel(app.TimePanel);
            app.EndTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.EndTimeEditFieldLabel.Position = [19 150 56 22];
            app.EndTimeEditFieldLabel.Text = 'End Time';

            % Create EndTimeEditField
            app.EndTimeEditField = uieditfield(app.TimePanel, 'text');
            app.EndTimeEditField.Editable = 'off';
            app.EndTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.EndTimeEditField.Position = [116 150 200 22];

            % Create MJDstartEditFieldLabel
            app.MJDstartEditFieldLabel = uilabel(app.TimePanel);
            app.MJDstartEditFieldLabel.HorizontalAlignment = 'right';
            app.MJDstartEditFieldLabel.Position = [24 115 53 22];
            app.MJDstartEditFieldLabel.Text = 'MJDstart';

            % Create MJDstartEditField
            app.MJDstartEditField = uieditfield(app.TimePanel, 'text');
            app.MJDstartEditField.Editable = 'off';
            app.MJDstartEditField.BackgroundColor = [1 0.9882 0.8196];
            app.MJDstartEditField.Position = [116 115 200 22];

            % Create MJDendEditFieldLabel
            app.MJDendEditFieldLabel = uilabel(app.TimePanel);
            app.MJDendEditFieldLabel.HorizontalAlignment = 'right';
            app.MJDendEditFieldLabel.Position = [53 80 50 22];
            app.MJDendEditFieldLabel.Text = 'MJDend';

            % Create MJDendEditField
            app.MJDendEditField = uieditfield(app.TimePanel, 'text');
            app.MJDendEditField.Editable = 'off';
            app.MJDendEditField.BackgroundColor = [1 0.9882 0.8196];
            app.MJDendEditField.Position = [116 80 200 22];

            % Create TotalDurationEditFieldLabel
            app.TotalDurationEditFieldLabel = uilabel(app.TimePanel);
            app.TotalDurationEditFieldLabel.HorizontalAlignment = 'right';
            app.TotalDurationEditFieldLabel.Position = [24 48 76 22];
            app.TotalDurationEditFieldLabel.Text = 'TotalDuration';

            % Create TotalDurationEditField
            app.TotalDurationEditField = uieditfield(app.TimePanel, 'text');
            app.TotalDurationEditField.Editable = 'off';
            app.TotalDurationEditField.BackgroundColor = [1 0.9882 0.8196];
            app.TotalDurationEditField.Position = [116 48 200 22];

            % Create SlewTimeBeforeEditFieldLabel
            app.SlewTimeBeforeEditFieldLabel = uilabel(app.TimePanel);
            app.SlewTimeBeforeEditFieldLabel.HorizontalAlignment = 'right';
            app.SlewTimeBeforeEditFieldLabel.Position = [7 14 93 22];
            app.SlewTimeBeforeEditFieldLabel.Text = 'SlewTimeBefore';

            % Create SlewTimeBeforeEditField
            app.SlewTimeBeforeEditField = uieditfield(app.TimePanel, 'text');
            app.SlewTimeBeforeEditField.Editable = 'off';
            app.SlewTimeBeforeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SlewTimeBeforeEditField.Position = [116 14 200 22];

            % Create DistancePanel
            app.DistancePanel = uipanel(app.UIFigure);
            app.DistancePanel.TitlePosition = 'centertop';
            app.DistancePanel.Title = 'Distance';
            app.DistancePanel.Position = [391 103 212 225];

            % Create MoonDistEditFieldLabel
            app.MoonDistEditFieldLabel = uilabel(app.DistancePanel);
            app.MoonDistEditFieldLabel.HorizontalAlignment = 'right';
            app.MoonDistEditFieldLabel.Position = [13 165 56 22];
            app.MoonDistEditFieldLabel.Text = 'MoonDist';

            % Create MoonDistEditField
            app.MoonDistEditField = uieditfield(app.DistancePanel, 'text');
            app.MoonDistEditField.Editable = 'off';
            app.MoonDistEditField.BackgroundColor = [1 0.9882 0.8196];
            app.MoonDistEditField.Position = [84 165 103 22];

            % Create SunDistEditFieldLabel
            app.SunDistEditFieldLabel = uilabel(app.DistancePanel);
            app.SunDistEditFieldLabel.HorizontalAlignment = 'right';
            app.SunDistEditFieldLabel.Position = [23 135 47 22];
            app.SunDistEditFieldLabel.Text = 'SunDist';

            % Create SunDistEditField
            app.SunDistEditField = uieditfield(app.DistancePanel, 'text');
            app.SunDistEditField.Editable = 'off';
            app.SunDistEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SunDistEditField.Position = [85 135 102 22];

            % Create EarthDistEditFieldLabel
            app.EarthDistEditFieldLabel = uilabel(app.DistancePanel);
            app.EarthDistEditFieldLabel.HorizontalAlignment = 'right';
            app.EarthDistEditFieldLabel.Position = [16 101 54 22];
            app.EarthDistEditFieldLabel.Text = 'EarthDist';

            % Create EarthDistEditField
            app.EarthDistEditField = uieditfield(app.DistancePanel, 'text');
            app.EarthDistEditField.Editable = 'off';
            app.EarthDistEditField.BackgroundColor = [1 0.9882 0.8196];
            app.EarthDistEditField.Position = [85 101 101 22];

            % Create NoCommEditFieldLabel
            app.NoCommEditFieldLabel = uilabel(app.DistancePanel);
            app.NoCommEditFieldLabel.HorizontalAlignment = 'right';
            app.NoCommEditFieldLabel.Position = [14 59 56 22];
            app.NoCommEditFieldLabel.Text = 'NoComm';

            % Create NoCommEditField
            app.NoCommEditField = uieditfield(app.DistancePanel, 'text');
            app.NoCommEditField.Editable = 'off';
            app.NoCommEditField.BackgroundColor = [1 0.9882 0.8196];
            app.NoCommEditField.Position = [85 59 50 22];

            % Create HardObsEditFieldLabel
            app.HardObsEditFieldLabel = uilabel(app.DistancePanel);
            app.HardObsEditFieldLabel.HorizontalAlignment = 'right';
            app.HardObsEditFieldLabel.Position = [18 27 53 22];
            app.HardObsEditFieldLabel.Text = 'HardObs';

            % Create HardObsEditField
            app.HardObsEditField = uieditfield(app.DistancePanel, 'text');
            app.HardObsEditField.Editable = 'off';
            app.HardObsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.HardObsEditField.Position = [86 27 50 22];

            % Create OtherPanel
            app.OtherPanel = uipanel(app.UIFigure);
            app.OtherPanel.TitlePosition = 'centertop';
            app.OtherPanel.Title = 'Other';
            app.OtherPanel.Position = [618 159 273 169];

            % Create ZodyEditFieldLabel
            app.ZodyEditFieldLabel = uilabel(app.OtherPanel);
            app.ZodyEditFieldLabel.HorizontalAlignment = 'right';
            app.ZodyEditFieldLabel.Position = [73 109 32 22];
            app.ZodyEditFieldLabel.Text = 'Zody';

            % Create ZodyEditField
            app.ZodyEditField = uieditfield(app.OtherPanel, 'text');
            app.ZodyEditField.Editable = 'off';
            app.ZodyEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ZodyEditField.Position = [120 109 143 22];

            % Create LimMagEditFieldLabel
            app.LimMagEditFieldLabel = uilabel(app.OtherPanel);
            app.LimMagEditFieldLabel.HorizontalAlignment = 'right';
            app.LimMagEditFieldLabel.Position = [58 67 48 22];
            app.LimMagEditFieldLabel.Text = 'LimMag';

            % Create LimMagEditField
            app.LimMagEditField = uieditfield(app.OtherPanel, 'text');
            app.LimMagEditField.Editable = 'off';
            app.LimMagEditField.BackgroundColor = [1 0.9882 0.8196];
            app.LimMagEditField.Position = [121 67 142 22];

            % Create OverlapTargetsEditFieldLabel
            app.OverlapTargetsEditFieldLabel = uilabel(app.OtherPanel);
            app.OverlapTargetsEditFieldLabel.HorizontalAlignment = 'right';
            app.OverlapTargetsEditFieldLabel.Position = [20 25 86 22];
            app.OverlapTargetsEditFieldLabel.Text = 'OverlapTargets';

            % Create OverlapTargetsEditField
            app.OverlapTargetsEditField = uieditfield(app.OtherPanel, 'text');
            app.OverlapTargetsEditField.Editable = 'off';
            app.OverlapTargetsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.OverlapTargetsEditField.Position = [121 25 142 22];

            % Create EditableParametersPanel
            app.EditableParametersPanel = uipanel(app.UIFigure);
            app.EditableParametersPanel.Title = 'Editable Parameters';
            app.EditableParametersPanel.Position = [25 342 286 153];

            % Create secondsLabel
            app.secondsLabel = uilabel(app.EditableParametersPanel);
            app.secondsLabel.Position = [193 97 50 22];
            app.secondsLabel.Text = 'seconds';

            % Create ExposureTimeEditFieldLabel
            app.ExposureTimeEditFieldLabel = uilabel(app.EditableParametersPanel);
            app.ExposureTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.ExposureTimeEditFieldLabel.Position = [20 97 85 22];
            app.ExposureTimeEditFieldLabel.Text = 'Exposure Time';

            % Create ExposureTimeEditField
            app.ExposureTimeEditField = uieditfield(app.EditableParametersPanel, 'numeric');
            app.ExposureTimeEditField.Position = [120 97 50 22];

            % Create EpochsPerVisitEditFieldLabel
            app.EpochsPerVisitEditFieldLabel = uilabel(app.EditableParametersPanel);
            app.EpochsPerVisitEditFieldLabel.HorizontalAlignment = 'right';
            app.EpochsPerVisitEditFieldLabel.Position = [13 63 93 22];
            app.EpochsPerVisitEditFieldLabel.Text = 'Epochs Per Visit';

            % Create EpochsPerVisitEditField
            app.EpochsPerVisitEditField = uieditfield(app.EditableParametersPanel, 'numeric');
            app.EpochsPerVisitEditField.Position = [121 63 50 22];

            % Create Panel_3
            app.Panel_3 = uipanel(app.EditableParametersPanel);
            app.Panel_3.Position = [14 16 220 35];

            % Create TilesLabel
            app.TilesLabel = uilabel(app.Panel_3);
            app.TilesLabel.Position = [8 5 30 22];
            app.TilesLabel.Text = 'Tiles';

            % Create Tile1CheckBox
            app.Tile1CheckBox = uicheckbox(app.Panel_3);
            app.Tile1CheckBox.Text = '1';
            app.Tile1CheckBox.Position = [51 5 29 22];

            % Create Tile2CheckBox
            app.Tile2CheckBox = uicheckbox(app.Panel_3);
            app.Tile2CheckBox.Text = '2';
            app.Tile2CheckBox.Position = [90 5 29 22];

            % Create Tile3CheckBox
            app.Tile3CheckBox = uicheckbox(app.Panel_3);
            app.Tile3CheckBox.Text = '3';
            app.Tile3CheckBox.Position = [130 5 29 22];

            % Create Tile4CheckBox
            app.Tile4CheckBox = uicheckbox(app.Panel_3);
            app.Tile4CheckBox.Text = '4';
            app.Tile4CheckBox.Position = [169 5 29 22];

            % Create UniqueTargetParamsPanel
            app.UniqueTargetParamsPanel = uipanel(app.UIFigure);
            app.UniqueTargetParamsPanel.Title = 'Unique Target Params';
            app.UniqueTargetParamsPanel.BackgroundColor = [0.902 0.902 0.902];
            app.UniqueTargetParamsPanel.Position = [324 358 566 137];

            % Create NameEditField_3Label
            app.NameEditField_3Label = uilabel(app.UniqueTargetParamsPanel);
            app.NameEditField_3Label.HorizontalAlignment = 'right';
            app.NameEditField_3Label.Position = [8 85 37 22];
            app.NameEditField_3Label.Text = 'Name';

            % Create NameEditField
            app.NameEditField = uieditfield(app.UniqueTargetParamsPanel, 'text');
            app.NameEditField.Editable = 'off';
            app.NameEditField.BackgroundColor = [1 0.9882 0.8196];
            app.NameEditField.Position = [60 85 229 22];

            % Create UniqueTargetIndexEditField_3Label
            app.UniqueTargetIndexEditField_3Label = uilabel(app.UniqueTargetParamsPanel);
            app.UniqueTargetIndexEditField_3Label.HorizontalAlignment = 'right';
            app.UniqueTargetIndexEditField_3Label.Position = [6 47 112 22];
            app.UniqueTargetIndexEditField_3Label.Text = 'Unique Target Index';

            % Create UniqueTargetIndexEditField
            app.UniqueTargetIndexEditField = uieditfield(app.UniqueTargetParamsPanel, 'text');
            app.UniqueTargetIndexEditField.Editable = 'off';
            app.UniqueTargetIndexEditField.BackgroundColor = [1 0.9882 0.8196];
            app.UniqueTargetIndexEditField.Position = [133 47 51 22];

            % Create GroupEditFieldLabel
            app.GroupEditFieldLabel = uilabel(app.UniqueTargetParamsPanel);
            app.GroupEditFieldLabel.HorizontalAlignment = 'right';
            app.GroupEditFieldLabel.Position = [79 13 38 22];
            app.GroupEditFieldLabel.Text = 'Group';

            % Create GroupEditField
            app.GroupEditField = uieditfield(app.UniqueTargetParamsPanel, 'text');
            app.GroupEditField.Editable = 'off';
            app.GroupEditField.BackgroundColor = [1 0.9882 0.8196];
            app.GroupEditField.Position = [132 13 54 22];

            % Create RAEditFieldLabel
            app.RAEditFieldLabel = uilabel(app.UniqueTargetParamsPanel);
            app.RAEditFieldLabel.HorizontalAlignment = 'right';
            app.RAEditFieldLabel.Position = [315 81 25 22];
            app.RAEditFieldLabel.Text = 'RA';

            % Create RAEditField
            app.RAEditField = uieditfield(app.UniqueTargetParamsPanel, 'text');
            app.RAEditField.Editable = 'off';
            app.RAEditField.BackgroundColor = [1 0.9882 0.8196];
            app.RAEditField.Position = [355 81 134 22];

            % Create DecEditFieldLabel
            app.DecEditFieldLabel = uilabel(app.UniqueTargetParamsPanel);
            app.DecEditFieldLabel.HorizontalAlignment = 'right';
            app.DecEditFieldLabel.Position = [316 47 26 22];
            app.DecEditFieldLabel.Text = 'Dec';

            % Create DecEditField
            app.DecEditField = uieditfield(app.UniqueTargetParamsPanel, 'text');
            app.DecEditField.Editable = 'off';
            app.DecEditField.BackgroundColor = [1 0.9882 0.8196];
            app.DecEditField.Position = [357 47 137 22];

            % Create ExpectedRollEditField_3Label
            app.ExpectedRollEditField_3Label = uilabel(app.UniqueTargetParamsPanel);
            app.ExpectedRollEditField_3Label.HorizontalAlignment = 'right';
            app.ExpectedRollEditField_3Label.Position = [267 16 79 22];
            app.ExpectedRollEditField_3Label.Text = 'Expected Roll';

            % Create ExpectedRollEditField
            app.ExpectedRollEditField = uieditfield(app.UniqueTargetParamsPanel, 'text');
            app.ExpectedRollEditField.Editable = 'off';
            app.ExpectedRollEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ExpectedRollEditField.Position = [361 16 134 22];

            % Create PlanTargetIndexEditFieldLabel
            app.PlanTargetIndexEditFieldLabel = uilabel(app.UIFigure);
            app.PlanTargetIndexEditFieldLabel.HorizontalAlignment = 'right';
            app.PlanTargetIndexEditFieldLabel.Position = [40 517 98 22];
            app.PlanTargetIndexEditFieldLabel.Text = 'Plan Target Index';

            % Create PlanTargetIndexEditField
            app.PlanTargetIndexEditField = uieditfield(app.UIFigure, 'text');
            app.PlanTargetIndexEditField.Editable = 'off';
            app.PlanTargetIndexEditField.BackgroundColor = [1 0.9882 0.8196];
            app.PlanTargetIndexEditField.Position = [164 517 60 22];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlanTargetParams(varargin)

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