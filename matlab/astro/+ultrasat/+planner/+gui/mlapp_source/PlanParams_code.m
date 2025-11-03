classdef PlanParams < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                        matlab.ui.Figure
        LabelTopStatus                  matlab.ui.control.Label
        ValidationResultPanel           matlab.ui.container.Panel
        MinimumDistanceFromPanel        matlab.ui.container.Panel
        MoonMinDistSlewEditField        matlab.ui.control.EditField
        EarthMinDistSlewEditField       matlab.ui.control.EditField
        SunMinDistSlewEditField         matlab.ui.control.EditField
        MoonMinDistObsEditField         matlab.ui.control.EditField
        EarthMinDistObsEditField        matlab.ui.control.EditField
        SunMinDistObsEditField          matlab.ui.control.EditField
        DuringSlewLabel                 matlab.ui.control.Label
        DuringObservationLabel          matlab.ui.control.Label
        MoonLabel                       matlab.ui.control.Label
        EarthLabel                      matlab.ui.control.Label
        SunLabel                        matlab.ui.control.Label
        Label_4                         matlab.ui.control.Label
        GeneralPanel                    matlab.ui.container.Panel
        PlanTypeDropDown                matlab.ui.control.DropDown
        PlanTypeDropDownLabel           matlab.ui.control.Label
        ChangeButton                    matlab.ui.control.Button
        AstPlannerEditField             matlab.ui.control.EditField
        AstPlannerEditFieldLabel        matlab.ui.control.Label
        PlanStatusEditField             matlab.ui.control.EditField
        PlanStatusEditFieldLabel        matlab.ui.control.Label
        PlanTargetsEditField            matlab.ui.control.EditField
        PlanTargetsEditFieldLabel       matlab.ui.control.Label
        UniqueTargetsEditField          matlab.ui.control.EditField
        UniqueTargetsEditFieldLabel     matlab.ui.control.Label
        FoldersFilesPanel               matlab.ui.container.Panel
        CalObjFileEditField             matlab.ui.control.EditField
        CalObjFlieEditFieldLabel        matlab.ui.control.Label
        CalSubDirEditField              matlab.ui.control.EditField
        CalSubDirEditFieldLabel         matlab.ui.control.Label
        BaseDataDirEditField            matlab.ui.control.EditField
        BaseDataDirEditFieldLabel       matlab.ui.control.Label
        TabGroup2                       matlab.ui.container.TabGroup
        StatusTab                       matlab.ui.container.Tab
        SubmitTimeEditField_2           matlab.ui.control.EditField
        RetreiveLabel                   matlab.ui.control.Label
        SubmitStatusButton              matlab.ui.control.Button
        ValidationStatusButton          matlab.ui.control.Button
        BuildStatusButton               matlab.ui.control.Button
        SubmitStatusEditField           matlab.ui.control.EditField
        ValidationStatusEditField       matlab.ui.control.EditField
        BuildStatusEditField            matlab.ui.control.EditField
        SubmitTimeEditField             matlab.ui.control.EditField
        SubmitEditFieldLabel            matlab.ui.control.Label
        ValidationTimeEditField         matlab.ui.control.EditField
        ValidationEditFieldLabel        matlab.ui.control.Label
        BuildTimeEditField              matlab.ui.control.EditField
        BuildEditFieldLabel             matlab.ui.control.Label
        CheckParamsPanel                matlab.ui.container.Panel
        CheckTimesUpdateButton          matlab.ui.control.Button
        CheckEndTimeEditField           matlab.ui.control.EditField
        CheckEndTimeEditFieldLabel      matlab.ui.control.Label
        CheckStartTimeEditField         matlab.ui.control.EditField
        CheckStartTimeEditFieldLabel    matlab.ui.control.Label
        ConstantMissionParametersPanel  matlab.ui.container.Panel
        TimeZoneEditField               matlab.ui.control.EditField
        SystesmTimeZoneLabel            matlab.ui.control.Label
        ChangeConstantsButton           matlab.ui.control.Button
        degreesLabel_2                  matlab.ui.control.Label
        FieldOfViewRadiusEditField      matlab.ui.control.EditField
        FieldofViewRadiusEditFieldLabel  matlab.ui.control.Label
        secondsLabel_3                  matlab.ui.control.Label
        TileReadTimeEditField           matlab.ui.control.EditField
        TileReadTimeEditFieldLabel      matlab.ui.control.Label
        secondsLabel_2                  matlab.ui.control.Label
        SlewBufferEditField             matlab.ui.control.EditField
        SlewBufferEditFieldLabel        matlab.ui.control.Label
        TabGroup                        matlab.ui.container.TabGroup
        HCSTab                          matlab.ui.container.Tab
        Label_5                         matlab.ui.control.Label
        LCSTab                          matlab.ui.container.Tab
        LcsDailyWindowMaxDurationEditField  matlab.ui.control.EditField
        DailywindowmaxdurationEditFieldLabel  matlab.ui.control.Label
        LcsDailyWindowStartTimeEditField  matlab.ui.control.EditField
        DailywindowstarttimeEditFieldLabel  matlab.ui.control.Label
        DDTTab                          matlab.ui.container.Tab
        Label_6                         matlab.ui.control.Label
        AllSkyTab                       matlab.ui.container.Tab
        AllSkyHighGalacticLatDitherPatternDropDown  matlab.ui.control.DropDown
        HighGalacticLatDitherPatternDropDownLabel  matlab.ui.control.Label
        AllSkyHighLatVisitsEditField    matlab.ui.control.NumericEditField
        visitsHighGalacticLatLabel      matlab.ui.control.Label
        degreesLabel                    matlab.ui.control.Label
        AllSkyDailyWindowMaxDurationEditField  matlab.ui.control.EditField
        DailywindowmaxdurationEditFieldLabel_2  matlab.ui.control.Label
        AllSkyDailyWindowStartTimeEditField  matlab.ui.control.EditField
        DailywindowstarttimeEditFieldLabel_2  matlab.ui.control.Label
        AllSkyLowLatVisitsEditField     matlab.ui.control.NumericEditField
        visitsLowGalacticLatLabel       matlab.ui.control.Label
        AllSkyGalacticLatThresholdEditField  matlab.ui.control.NumericEditField
        HighGalacticLatthresholdLabel   matlab.ui.control.Label
        TOOTab                          matlab.ui.container.Tab
        TooWindowDurationEditField      matlab.ui.control.EditField
        TOOwindowdurationEditFieldLabel  matlab.ui.control.Label
        TooStartTimeEditField           matlab.ui.control.EditField
        TOOdailystarttimeLabel          matlab.ui.control.Label
        PlanParametersPanel             matlab.ui.container.Panel
        PkEditField                     matlab.ui.control.EditField
        PkEditFieldLabel                matlab.ui.control.Label
        Label_3                         matlab.ui.control.Label
        Label                           matlab.ui.control.Label
        Panel_3                         matlab.ui.container.Panel
        Tile4CheckBox                   matlab.ui.control.CheckBox
        Tile3CheckBox                   matlab.ui.control.CheckBox
        Tile2CheckBox                   matlab.ui.control.CheckBox
        Tile1CheckBox                   matlab.ui.control.CheckBox
        TilesLabel                      matlab.ui.control.Label
        secondsLabel                    matlab.ui.control.Label
        ExposureEditField               matlab.ui.control.EditField
        ExposureEditFieldLabel          matlab.ui.control.Label
        EpochsPerVisitEditField         matlab.ui.control.EditField
        EpochsperVisitEditFieldLabel    matlab.ui.control.Label
        EndTimeEditField                matlab.ui.control.EditField
        EndtimeEditFieldLabel           matlab.ui.control.Label
        StartTimeEditField              matlab.ui.control.EditField
        StarttimeEditFieldLabel         matlab.ui.control.Label
        TitleEditField                  matlab.ui.control.EditField
        TitleEditFieldLabel             matlab.ui.control.Label
        Panel_2                         matlab.ui.container.Panel
        PlanParametersLabel             matlab.ui.control.Label
        Panel                           matlab.ui.container.Panel
        HelpButton                      matlab.ui.control.Button
        CancelButton                    matlab.ui.control.Button
        SaveButton                      matlab.ui.control.Button
    end

    
    methods (Static)
        function about()
            % PlanParams App
            %
            % This app allows users to configure observation plan parameters 
            % for the ULTRASAT mission. It provides various settings related to 
            % observation times, exposure, plan status, and constraints.
            %
            % Features:
            % - Allows selection of the plan type (HCS, LCS, DDT, AllSky, TOO).
            % - Displays and modifies various observation parameters.
            % - Provides minimum distance constraints from celestial bodies.
            % - Supports plan validation, build, and submission status retrieval.
            % - Includes editable fields depending on plan state.
            %
            % Buttons and Actions:
            % - ChangeButton: Enables modification of the plan type.
            % - CheckTimesUpdateButton: Updates the check start and end times.
            % - BuildStatusButton: Retrieves the build status of the plan.
            % - ValidationStatusButton: Retrieves the validation status of the plan.
            % - SubmitStatusButton: Retrieves the submission status of the plan.
            % - ChangeConstantsButton: Allows modification of fundamental system constants.
            % - SaveButton: Saves the plan parameters.
            % - CancelButton: Cancels any modifications.
        end
    end

    
    properties (Access = public)
        MainModule      % Reference to the main application module
        Status          % Status of the plan modification ('Save' or 'Cancel')
        PlanType        % Current plan type (HCS, LCS, DDT, AllSky, TOO)
        ReadOnly        % Boolean flag indicating whether parameters are editable
        AppUtils        %
    end


    methods (Access = public)

        function beforeShow(app)
            % Called before displaying the plan parameters, from PlannerMain.showModal().
            % - Sets the active plan type tab.
            % - Enables or disables editable fields based on the plan state.
            % - Ensures only relevant fields are available for modification.            

            app.PlanType = app.MainModule.Planner.Type;

            % Show only the tab relevant for the current PlanType.
            app.setActivePlanTab();

            % Initialize fields - editable only when plan is not empty            
            app.initFields();
        end


        function initFields(app)
            % Initialize fields to defaults

            % Check if plan was built before (i.e. not empty)
            IsPlanEmpty = height(app.MainModule.Planner.Plan) == 0;            

            % Common fields
            app.PlanTypeDropDown.Enable = "off";
            app.setEditable(app.SlewBufferEditField, false);
            app.setEditable(app.TileReadTimeEditField, false);
            app.setEditable(app.FieldOfViewRadiusEditField, false);

            % Start Time, End Time - Editable until build
            app.setEditable(app.StartTimeEditField, IsPlanEmpty);
            app.setEditable(app.EndTimeEditField, IsPlanEmpty);
            
            % DDT
            if strcmp(app.PlanType, 'DDT')
                app.setEditable(app.EpochsPerVisitEditField, true);
                app.setEditable(app.ExposureEditField, true);
                %app.setEditable(app.Tile1CheckBox, true);
                %app.setEditable(app.Tile2CheckBox, true);
                %app.setEditable(app.Tile3CheckBox, true);
                %app.setEditable(app.Tile4CheckBox, true);	
            else
                app.setEditable(app.EpochsPerVisitEditField, IsPlanEmpty);
                app.setEditable(app.ExposureEditField, IsPlanEmpty);
                %app.setEditable(app.Tile1CheckBox, IsPlanEmpty);
                %app.setEditable(app.Tile2CheckBox, IsPlanEmpty);
                %app.setEditable(app.Tile3CheckBox, IsPlanEmpty);
                %app.setEditable(app.Tile4CheckBox, IsPlanEmpty);		
            end
            
            % LCS
            app.setEditable(app.LcsDailyWindowStartTimeEditField, IsPlanEmpty);
            app.setEditable(app.LcsDailyWindowMaxDurationEditField, IsPlanEmpty);
            
            % AllSky
            app.setEditable(app.AllSkyDailyWindowStartTimeEditField, IsPlanEmpty);
            app.setEditable(app.AllSkyDailyWindowMaxDurationEditField, IsPlanEmpty);
            app.setEditable(app.AllSkyGalacticLatThresholdEditField, IsPlanEmpty);
            app.setEditable(app.AllSkyLowLatVisitsEditField, IsPlanEmpty);
            app.setEditable(app.AllSkyHighLatVisitsEditField, IsPlanEmpty);
            app.setEditable(app.AllSkyHighGalacticLatDitherPatternDropDown, IsPlanEmpty);
            
            % TOO
            app.setEditable(app.TooStartTimeEditField, IsPlanEmpty);
            app.setEditable(app.TooWindowDurationEditField, IsPlanEmpty);                        
        end


        function setEditable(app, Field, EditMode)
            % Set the editability and background color of the specified UI field.
            %
            % - Enables editing with a white background when EditMode is true.
            % - Disables editing with a light yellow background when EditMode is false.
            if EditMode
                Field.Editable = true;
                Field.BackgroundColor = [1.0, 1.0, 1.0];
            else
                Field.Editable = false;
                Field.BackgroundColor = [1.00, 0.99, 0.82];
            end        
        end


        function setActivePlanTab(app)
            % Show only the tab relevant for the current PlanType.
            %
            % - Hides all tabs before selecting the appropriate one.
            % - Displays and selects the tab corresponding to the current PlanType.

            % First hides all tabs
            app.HCSTab.Parent = [];
            app.LCSTab.Parent = [];
            app.DDTTab.Parent = [];
            app.TOOTab.Parent = [];            
            app.AllSkyTab.Parent = [];            
        
            % Determine which tab to show based on PlanType
            switch app.PlanType
                case 'HCS'
                    app.HCSTab.Parent = app.TabGroup;
                    app.TabGroup.SelectedTab = app.HCSTab;
                case 'LCS'
                    app.LCSTab.Parent = app.TabGroup;
                    app.TabGroup.SelectedTab = app.LCSTab;
                case 'DDT'
                    app.DDTTab.Parent = app.TabGroup;
                    app.TabGroup.SelectedTab = app.DDTTab;
                case 'AllSS'
                    app.AllSkyTab.Parent = app.TabGroup;
                    app.TabGroup.SelectedTab = app.AllSkyTab;
                case 'TOO'
                    app.TOOTab.Parent = app.TabGroup;
                    app.TabGroup.SelectedTab = app.TOOTab;
            end
        end        
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, MainModule)
            app.MainModule = MainModule;
            app.MainModule.AppUtils.center(app);
            app.AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, app);
        end

        % Button pushed function: BuildStatusButton
        function BuildStatusButtonPushed(app, event)
            app.MainModule.MainApp.BuildHelper.showBuildStatusWindow(app.MainModule.MainApp);
        end

        % Button pushed function: ValidationStatusButton
        function ValidationStatusButtonPushed(app, event)
            app.MainModule.MainApp.ValidationHelper.showValidationStatusWindow(app.MainModule.MainApp);
        end

        % Button pushed function: ChangeButton
        function ChangeButtonPushed(app, event)
            % Prompts the user for confirmation before allowing plan type modification.
            % If confirmed, enables editing of the PlanTypeDropDown field.                        
            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to modify the Plan Type?', 'Confirm'), 'Yes')
                return;
            end

            % Allow edit PlanType drop-down
            app.PlanTypeDropDown.Enable = "on";
        end

        % Button pushed function: CheckTimesUpdateButton
        function CheckTimesUpdateButtonPushed(app, event)
            % Apply changes
            app.MainModule.MainApp.PlanParamsHelper.applyCheckTimes(app.MainModule.MainApp, app);            
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.Status = 'Cancel';
            uiresume(app.UIFigure);            
        end

        % Button pushed function: SaveButton
        function SaveButtonPushed(app, event)
            % Apply the parameters
            try
                % Call PlannerMain.ApplyPlanParams()
                Result = app.MainModule.MainApp.PlanParamsHelper.applyPlanParams(app.MainModule.MainApp, app);
                if Result
                    app.Status = 'Save';
                    uiresume(app.UIFigure);                                        
                end
            catch ME
                app.MainModule.MainApp.msgex('SaveButtonPushed', ME);
            end                                
        end

        % Button pushed function: ChangeConstantsButton
        function ChangeConstantsButtonPushed(app, event)
            % Prompts the user for confirmation before allowing edits to fundamental system constants.
            % If confirmed, enables editing of SlewBuffer, TileReadTime, and FieldOfViewRadius fields.                        
            if ~strcmp(app.AppUtils.askYesNo('These are fundimental system constants that are coordinated with IAA GCS and the camera designer. Are you sure you want to edit these values???', 'Confirm'), 'Yes')
                return;
            end
            
            % Enable edit
            app.setEditable(app.SlewBufferEditField, true);
            app.setEditable(app.TileReadTimeEditField, true);
            app.setEditable(app.FieldOfViewRadiusEditField, true);
        end

        % Button pushed function: SubmitStatusButton
        function SubmitStatusButtonPushed(app, event)
            app.MainModule.MainApp.SubmitHelper.showSubmitStatusWindow(app.MainModule.MainApp);
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.MainModule.MainApp.showHelp('plan_params');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1323 768];
            app.UIFigure.Name = 'MATLAB App';
            app.UIFigure.Resize = 'off';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.BackgroundColor = [0.8 0.8 0.8];
            app.Panel.Position = [16 19 1296 57];

            % Create SaveButton
            app.SaveButton = uibutton(app.Panel, 'push');
            app.SaveButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.SaveButton.FontWeight = 'bold';
            app.SaveButton.FontColor = [0 0 1];
            app.SaveButton.Position = [493 9 85 39];
            app.SaveButton.Text = 'Save';

            % Create CancelButton
            app.CancelButton = uibutton(app.Panel, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Position = [609 9 85 39];
            app.CancelButton.Text = 'Cancel';

            % Create HelpButton
            app.HelpButton = uibutton(app.Panel, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Position = [726 9 85 39];
            app.HelpButton.Text = 'Help';

            % Create Panel_2
            app.Panel_2 = uipanel(app.UIFigure);
            app.Panel_2.BorderColor = [0.651 0.651 0.651];
            app.Panel_2.BackgroundColor = [0.302 0.7451 0.9333];
            app.Panel_2.Position = [16 723 1301 33];

            % Create PlanParametersLabel
            app.PlanParametersLabel = uilabel(app.Panel_2);
            app.PlanParametersLabel.HorizontalAlignment = 'center';
            app.PlanParametersLabel.FontSize = 18;
            app.PlanParametersLabel.FontWeight = 'bold';
            app.PlanParametersLabel.Position = [8 0 1275 33];
            app.PlanParametersLabel.Text = 'Plan Parameters';

            % Create PlanParametersPanel
            app.PlanParametersPanel = uipanel(app.UIFigure);
            app.PlanParametersPanel.TitlePosition = 'centertop';
            app.PlanParametersPanel.Title = 'Plan Parameters';
            app.PlanParametersPanel.Position = [397 400 496 272];

            % Create TitleEditFieldLabel
            app.TitleEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.TitleEditFieldLabel.HorizontalAlignment = 'right';
            app.TitleEditFieldLabel.Position = [49 216 27 22];
            app.TitleEditFieldLabel.Text = 'Title';

            % Create TitleEditField
            app.TitleEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.TitleEditField.Position = [91 216 280 22];

            % Create StarttimeEditFieldLabel
            app.StarttimeEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.StarttimeEditFieldLabel.HorizontalAlignment = 'right';
            app.StarttimeEditFieldLabel.Position = [22 161 56 22];
            app.StarttimeEditFieldLabel.Text = 'Start time';

            % Create StartTimeEditField
            app.StartTimeEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.StartTimeEditField.Placeholder = 'YYYY-MM-DD HH:MM:SS';
            app.StartTimeEditField.Position = [93 161 229 22];

            % Create EndtimeEditFieldLabel
            app.EndtimeEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.EndtimeEditFieldLabel.HorizontalAlignment = 'right';
            app.EndtimeEditFieldLabel.Position = [25 132 52 22];
            app.EndtimeEditFieldLabel.Text = 'End time';

            % Create EndTimeEditField
            app.EndTimeEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.EndTimeEditField.Placeholder = 'YYYY-MM-DD HH:MM:SS';
            app.EndTimeEditField.Position = [92 132 229 22];

            % Create EpochsperVisitEditFieldLabel
            app.EpochsperVisitEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.EpochsperVisitEditFieldLabel.HorizontalAlignment = 'right';
            app.EpochsperVisitEditFieldLabel.Position = [19 78 92 22];
            app.EpochsperVisitEditFieldLabel.Text = 'Epochs per Visit';

            % Create EpochsPerVisitEditField
            app.EpochsPerVisitEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.EpochsPerVisitEditField.Position = [126 78 50 22];

            % Create ExposureEditFieldLabel
            app.ExposureEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.ExposureEditFieldLabel.HorizontalAlignment = 'right';
            app.ExposureEditFieldLabel.Position = [56 50 56 22];
            app.ExposureEditFieldLabel.Text = 'Exposure';

            % Create ExposureEditField
            app.ExposureEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.ExposureEditField.Position = [127 50 50 22];

            % Create secondsLabel
            app.secondsLabel = uilabel(app.PlanParametersPanel);
            app.secondsLabel.Position = [194 51 50 22];
            app.secondsLabel.Text = 'seconds';

            % Create Panel_3
            app.Panel_3 = uipanel(app.PlanParametersPanel);
            app.Panel_3.Position = [77 10 220 35];

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

            % Create Label
            app.Label = uilabel(app.PlanParametersPanel);
            app.Label.FontWeight = 'bold';
            app.Label.FontColor = [0 0 1];
            app.Label.Position = [16 184 347 22];
            app.Label.Text = 'Fields editable only when plan table is empty (before build)';

            % Create Label_3
            app.Label_3 = uilabel(app.PlanParametersPanel);
            app.Label_3.FontWeight = 'bold';
            app.Label_3.FontColor = [0 0 1];
            app.Label_3.Position = [14 100 430 22];
            app.Label_3.Text = 'Fields editable for non-DDT - only when plan table is empty (before build)';

            % Create PkEditFieldLabel
            app.PkEditFieldLabel = uilabel(app.PlanParametersPanel);
            app.PkEditFieldLabel.HorizontalAlignment = 'right';
            app.PkEditFieldLabel.FontWeight = 'bold';
            app.PkEditFieldLabel.Position = [370 216 25 22];
            app.PkEditFieldLabel.Text = 'Pk';

            % Create PkEditField
            app.PkEditField = uieditfield(app.PlanParametersPanel, 'text');
            app.PkEditField.Editable = 'off';
            app.PkEditField.BackgroundColor = [1 0.9882 0.8196];
            app.PkEditField.Position = [404 216 60 22];

            % Create TabGroup
            app.TabGroup = uitabgroup(app.UIFigure);
            app.TabGroup.Position = [16 212 370 230];

            % Create HCSTab
            app.HCSTab = uitab(app.TabGroup);
            app.HCSTab.Title = 'HCS';

            % Create Label_5
            app.Label_5 = uilabel(app.HCSTab);
            app.Label_5.FontWeight = 'bold';
            app.Label_5.FontColor = [0 0 1];
            app.Label_5.Position = [80 98 243 22];
            app.Label_5.Text = 'There are no additional fields for HCS';

            % Create LCSTab
            app.LCSTab = uitab(app.TabGroup);
            app.LCSTab.Title = 'LCS';

            % Create DailywindowstarttimeEditFieldLabel
            app.DailywindowstarttimeEditFieldLabel = uilabel(app.LCSTab);
            app.DailywindowstarttimeEditFieldLabel.HorizontalAlignment = 'right';
            app.DailywindowstarttimeEditFieldLabel.Position = [50 148 128 22];
            app.DailywindowstarttimeEditFieldLabel.Text = 'Daily window start time';

            % Create LcsDailyWindowStartTimeEditField
            app.LcsDailyWindowStartTimeEditField = uieditfield(app.LCSTab, 'text');
            app.LcsDailyWindowStartTimeEditField.Placeholder = 'HH:MM:SS';
            app.LcsDailyWindowStartTimeEditField.Position = [193 148 111 22];

            % Create DailywindowmaxdurationEditFieldLabel
            app.DailywindowmaxdurationEditFieldLabel = uilabel(app.LCSTab);
            app.DailywindowmaxdurationEditFieldLabel.HorizontalAlignment = 'right';
            app.DailywindowmaxdurationEditFieldLabel.Position = [30 114 148 22];
            app.DailywindowmaxdurationEditFieldLabel.Text = 'Daily window max duration';

            % Create LcsDailyWindowMaxDurationEditField
            app.LcsDailyWindowMaxDurationEditField = uieditfield(app.LCSTab, 'text');
            app.LcsDailyWindowMaxDurationEditField.Placeholder = 'HH:MM';
            app.LcsDailyWindowMaxDurationEditField.Position = [193 114 111 22];

            % Create DDTTab
            app.DDTTab = uitab(app.TabGroup);
            app.DDTTab.Title = 'DDT';

            % Create Label_6
            app.Label_6 = uilabel(app.DDTTab);
            app.Label_6.FontWeight = 'bold';
            app.Label_6.FontColor = [0 0 1];
            app.Label_6.Position = [80 98 243 22];
            app.Label_6.Text = 'There are no additional fields for DDT';

            % Create AllSkyTab
            app.AllSkyTab = uitab(app.TabGroup);
            app.AllSkyTab.Title = 'AllSky';

            % Create HighGalacticLatthresholdLabel
            app.HighGalacticLatthresholdLabel = uilabel(app.AllSkyTab);
            app.HighGalacticLatthresholdLabel.HorizontalAlignment = 'right';
            app.HighGalacticLatthresholdLabel.Position = [57 108 121 22];
            app.HighGalacticLatthresholdLabel.Text = 'Galactic Lat threshold';

            % Create AllSkyGalacticLatThresholdEditField
            app.AllSkyGalacticLatThresholdEditField = uieditfield(app.AllSkyTab, 'numeric');
            app.AllSkyGalacticLatThresholdEditField.Position = [193 108 100 22];

            % Create visitsLowGalacticLatLabel
            app.visitsLowGalacticLatLabel = uilabel(app.AllSkyTab);
            app.visitsLowGalacticLatLabel.HorizontalAlignment = 'right';
            app.visitsLowGalacticLatLabel.Position = [38 77 141 22];
            app.visitsLowGalacticLatLabel.Text = '# visits - Low Galactic Lat';

            % Create AllSkyLowLatVisitsEditField
            app.AllSkyLowLatVisitsEditField = uieditfield(app.AllSkyTab, 'numeric');
            app.AllSkyLowLatVisitsEditField.Position = [194 77 100 22];
            app.AllSkyLowLatVisitsEditField.Value = 1;

            % Create DailywindowstarttimeEditFieldLabel_2
            app.DailywindowstarttimeEditFieldLabel_2 = uilabel(app.AllSkyTab);
            app.DailywindowstarttimeEditFieldLabel_2.HorizontalAlignment = 'right';
            app.DailywindowstarttimeEditFieldLabel_2.Position = [48 171 128 22];
            app.DailywindowstarttimeEditFieldLabel_2.Text = 'Daily window start time';

            % Create AllSkyDailyWindowStartTimeEditField
            app.AllSkyDailyWindowStartTimeEditField = uieditfield(app.AllSkyTab, 'text');
            app.AllSkyDailyWindowStartTimeEditField.Placeholder = 'HH:MM:SS';
            app.AllSkyDailyWindowStartTimeEditField.Position = [191 171 113 22];

            % Create DailywindowmaxdurationEditFieldLabel_2
            app.DailywindowmaxdurationEditFieldLabel_2 = uilabel(app.AllSkyTab);
            app.DailywindowmaxdurationEditFieldLabel_2.HorizontalAlignment = 'right';
            app.DailywindowmaxdurationEditFieldLabel_2.Position = [29 140 148 22];
            app.DailywindowmaxdurationEditFieldLabel_2.Text = 'Daily window max duration';

            % Create AllSkyDailyWindowMaxDurationEditField
            app.AllSkyDailyWindowMaxDurationEditField = uieditfield(app.AllSkyTab, 'text');
            app.AllSkyDailyWindowMaxDurationEditField.Placeholder = 'HH:MM';
            app.AllSkyDailyWindowMaxDurationEditField.Position = [192 140 112 22];

            % Create degreesLabel
            app.degreesLabel = uilabel(app.AllSkyTab);
            app.degreesLabel.Position = [316 140 48 22];
            app.degreesLabel.Text = 'degrees';

            % Create visitsHighGalacticLatLabel
            app.visitsHighGalacticLatLabel = uilabel(app.AllSkyTab);
            app.visitsHighGalacticLatLabel.HorizontalAlignment = 'right';
            app.visitsHighGalacticLatLabel.Position = [36 46 144 22];
            app.visitsHighGalacticLatLabel.Text = '# visits - High Galactic Lat';

            % Create AllSkyHighLatVisitsEditField
            app.AllSkyHighLatVisitsEditField = uieditfield(app.AllSkyTab, 'numeric');
            app.AllSkyHighLatVisitsEditField.Position = [195 46 100 22];

            % Create HighGalacticLatDitherPatternDropDownLabel
            app.HighGalacticLatDitherPatternDropDownLabel = uilabel(app.AllSkyTab);
            app.HighGalacticLatDitherPatternDropDownLabel.HorizontalAlignment = 'right';
            app.HighGalacticLatDitherPatternDropDownLabel.Position = [5 17 174 22];
            app.HighGalacticLatDitherPatternDropDownLabel.Text = 'High Galactic Lat Dither Pattern';

            % Create AllSkyHighGalacticLatDitherPatternDropDown
            app.AllSkyHighGalacticLatDitherPatternDropDown = uidropdown(app.AllSkyTab);
            app.AllSkyHighGalacticLatDitherPatternDropDown.Items = {'2x2'};
            app.AllSkyHighGalacticLatDitherPatternDropDown.Position = [194 17 100 22];
            app.AllSkyHighGalacticLatDitherPatternDropDown.Value = '2x2';

            % Create TOOTab
            app.TOOTab = uitab(app.TabGroup);
            app.TOOTab.Title = 'TOO';

            % Create TOOdailystarttimeLabel
            app.TOOdailystarttimeLabel = uilabel(app.TOOTab);
            app.TOOdailystarttimeLabel.HorizontalAlignment = 'right';
            app.TOOdailystarttimeLabel.Position = [22 148 111 22];
            app.TOOdailystarttimeLabel.Text = 'TOO daily start time';

            % Create TooStartTimeEditField
            app.TooStartTimeEditField = uieditfield(app.TOOTab, 'text');
            app.TooStartTimeEditField.Placeholder = 'HH:MM:SS';
            app.TooStartTimeEditField.Position = [148 148 108 22];

            % Create TOOwindowdurationEditFieldLabel
            app.TOOwindowdurationEditFieldLabel = uilabel(app.TOOTab);
            app.TOOwindowdurationEditFieldLabel.HorizontalAlignment = 'right';
            app.TOOwindowdurationEditFieldLabel.Position = [12 114 121 22];
            app.TOOwindowdurationEditFieldLabel.Text = 'TOO window duration';

            % Create TooWindowDurationEditField
            app.TooWindowDurationEditField = uieditfield(app.TOOTab, 'text');
            app.TooWindowDurationEditField.Placeholder = 'HH:MM';
            app.TooWindowDurationEditField.Position = [148 114 108 22];

            % Create ConstantMissionParametersPanel
            app.ConstantMissionParametersPanel = uipanel(app.UIFigure);
            app.ConstantMissionParametersPanel.TitlePosition = 'centertop';
            app.ConstantMissionParametersPanel.Title = 'Constant Mission Parameters';
            app.ConstantMissionParametersPanel.BackgroundColor = [0.8 0.8 0.8];
            app.ConstantMissionParametersPanel.Position = [903 502 410 169];

            % Create SlewBufferEditFieldLabel
            app.SlewBufferEditFieldLabel = uilabel(app.ConstantMissionParametersPanel);
            app.SlewBufferEditFieldLabel.HorizontalAlignment = 'right';
            app.SlewBufferEditFieldLabel.Position = [52 112 66 22];
            app.SlewBufferEditFieldLabel.Text = 'Slew Buffer';

            % Create SlewBufferEditField
            app.SlewBufferEditField = uieditfield(app.ConstantMissionParametersPanel, 'text');
            app.SlewBufferEditField.Editable = 'off';
            app.SlewBufferEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SlewBufferEditField.Position = [133 112 50 22];

            % Create secondsLabel_2
            app.secondsLabel_2 = uilabel(app.ConstantMissionParametersPanel);
            app.secondsLabel_2.Position = [202 112 50 22];
            app.secondsLabel_2.Text = 'seconds';

            % Create TileReadTimeEditFieldLabel
            app.TileReadTimeEditFieldLabel = uilabel(app.ConstantMissionParametersPanel);
            app.TileReadTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.TileReadTimeEditFieldLabel.Position = [34 79 85 22];
            app.TileReadTimeEditFieldLabel.Text = 'Tile Read Time';

            % Create TileReadTimeEditField
            app.TileReadTimeEditField = uieditfield(app.ConstantMissionParametersPanel, 'text');
            app.TileReadTimeEditField.Editable = 'off';
            app.TileReadTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.TileReadTimeEditField.Position = [134 79 50 22];

            % Create secondsLabel_3
            app.secondsLabel_3 = uilabel(app.ConstantMissionParametersPanel);
            app.secondsLabel_3.Position = [202 79 50 22];
            app.secondsLabel_3.Text = 'seconds';

            % Create FieldofViewRadiusEditFieldLabel
            app.FieldofViewRadiusEditFieldLabel = uilabel(app.ConstantMissionParametersPanel);
            app.FieldofViewRadiusEditFieldLabel.HorizontalAlignment = 'right';
            app.FieldofViewRadiusEditFieldLabel.Position = [4 47 114 22];
            app.FieldofViewRadiusEditFieldLabel.Text = 'Field of View Radius';

            % Create FieldOfViewRadiusEditField
            app.FieldOfViewRadiusEditField = uieditfield(app.ConstantMissionParametersPanel, 'text');
            app.FieldOfViewRadiusEditField.Editable = 'off';
            app.FieldOfViewRadiusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.FieldOfViewRadiusEditField.Position = [133 47 50 22];

            % Create degreesLabel_2
            app.degreesLabel_2 = uilabel(app.ConstantMissionParametersPanel);
            app.degreesLabel_2.Position = [205 44 48 22];
            app.degreesLabel_2.Text = 'degrees';

            % Create ChangeConstantsButton
            app.ChangeConstantsButton = uibutton(app.ConstantMissionParametersPanel, 'push');
            app.ChangeConstantsButton.ButtonPushedFcn = createCallbackFcn(app, @ChangeConstantsButtonPushed, true);
            app.ChangeConstantsButton.Position = [302 113 85 27];
            app.ChangeConstantsButton.Text = 'Change';

            % Create SystesmTimeZoneLabel
            app.SystesmTimeZoneLabel = uilabel(app.ConstantMissionParametersPanel);
            app.SystesmTimeZoneLabel.HorizontalAlignment = 'right';
            app.SystesmTimeZoneLabel.Position = [16 14 102 22];
            app.SystesmTimeZoneLabel.Text = 'System TimeZone';

            % Create TimeZoneEditField
            app.TimeZoneEditField = uieditfield(app.ConstantMissionParametersPanel, 'text');
            app.TimeZoneEditField.Editable = 'off';
            app.TimeZoneEditField.BackgroundColor = [1 0.9882 0.8196];
            app.TimeZoneEditField.Position = [133 14 50 22];
            app.TimeZoneEditField.Value = 'UTC';

            % Create CheckParamsPanel
            app.CheckParamsPanel = uipanel(app.UIFigure);
            app.CheckParamsPanel.Title = 'Check Params';
            app.CheckParamsPanel.BackgroundColor = [0.9137 0.9529 0.9686];
            app.CheckParamsPanel.Position = [17 85 367 117];

            % Create CheckStartTimeEditFieldLabel
            app.CheckStartTimeEditFieldLabel = uilabel(app.CheckParamsPanel);
            app.CheckStartTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.CheckStartTimeEditFieldLabel.FontWeight = 'bold';
            app.CheckStartTimeEditFieldLabel.Position = [0 52 103 22];
            app.CheckStartTimeEditFieldLabel.Text = 'Check Start Time';

            % Create CheckStartTimeEditField
            app.CheckStartTimeEditField = uieditfield(app.CheckParamsPanel, 'text');
            app.CheckStartTimeEditField.Placeholder = 'YYYY-MM-DD HH:MM:SS';
            app.CheckStartTimeEditField.Position = [118 52 154 22];

            % Create CheckEndTimeEditFieldLabel
            app.CheckEndTimeEditFieldLabel = uilabel(app.CheckParamsPanel);
            app.CheckEndTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.CheckEndTimeEditFieldLabel.FontWeight = 'bold';
            app.CheckEndTimeEditFieldLabel.Position = [4 21 98 22];
            app.CheckEndTimeEditFieldLabel.Text = 'Check End Time';

            % Create CheckEndTimeEditField
            app.CheckEndTimeEditField = uieditfield(app.CheckParamsPanel, 'text');
            app.CheckEndTimeEditField.Placeholder = 'YYYY-MM-DD HH:MM:SS';
            app.CheckEndTimeEditField.Position = [117 21 155 22];

            % Create CheckTimesUpdateButton
            app.CheckTimesUpdateButton = uibutton(app.CheckParamsPanel, 'push');
            app.CheckTimesUpdateButton.ButtonPushedFcn = createCallbackFcn(app, @CheckTimesUpdateButtonPushed, true);
            app.CheckTimesUpdateButton.FontWeight = 'bold';
            app.CheckTimesUpdateButton.Visible = 'off';
            app.CheckTimesUpdateButton.Position = [285 48 73 27];
            app.CheckTimesUpdateButton.Text = 'Update';

            % Create TabGroup2
            app.TabGroup2 = uitabgroup(app.UIFigure);
            app.TabGroup2.Position = [397 212 496 181];

            % Create StatusTab
            app.StatusTab = uitab(app.TabGroup2);
            app.StatusTab.Title = 'Status';
            app.StatusTab.BackgroundColor = [0.9137 0.949 0.9137];

            % Create BuildEditFieldLabel
            app.BuildEditFieldLabel = uilabel(app.StatusTab);
            app.BuildEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.BuildEditFieldLabel.HorizontalAlignment = 'right';
            app.BuildEditFieldLabel.FontWeight = 'bold';
            app.BuildEditFieldLabel.Position = [6 127 35 22];
            app.BuildEditFieldLabel.Text = 'Build';

            % Create BuildTimeEditField
            app.BuildTimeEditField = uieditfield(app.StatusTab, 'text');
            app.BuildTimeEditField.Editable = 'off';
            app.BuildTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BuildTimeEditField.Position = [79 127 136 22];
            app.BuildTimeEditField.Value = '2025-01-01 00:00:00';

            % Create ValidationEditFieldLabel
            app.ValidationEditFieldLabel = uilabel(app.StatusTab);
            app.ValidationEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.ValidationEditFieldLabel.HorizontalAlignment = 'right';
            app.ValidationEditFieldLabel.FontWeight = 'bold';
            app.ValidationEditFieldLabel.Position = [6 96 62 22];
            app.ValidationEditFieldLabel.Text = 'Validation';

            % Create ValidationTimeEditField
            app.ValidationTimeEditField = uieditfield(app.StatusTab, 'text');
            app.ValidationTimeEditField.Editable = 'off';
            app.ValidationTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ValidationTimeEditField.Position = [78 96 136 22];

            % Create SubmitEditFieldLabel
            app.SubmitEditFieldLabel = uilabel(app.StatusTab);
            app.SubmitEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.SubmitEditFieldLabel.HorizontalAlignment = 'right';
            app.SubmitEditFieldLabel.FontWeight = 'bold';
            app.SubmitEditFieldLabel.Position = [6 64 46 22];
            app.SubmitEditFieldLabel.Text = 'Submit';

            % Create SubmitTimeEditField
            app.SubmitTimeEditField = uieditfield(app.StatusTab, 'text');
            app.SubmitTimeEditField.Editable = 'off';
            app.SubmitTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SubmitTimeEditField.Position = [78 64 136 22];

            % Create BuildStatusEditField
            app.BuildStatusEditField = uieditfield(app.StatusTab, 'text');
            app.BuildStatusEditField.Editable = 'off';
            app.BuildStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BuildStatusEditField.Position = [226 127 145 22];
            app.BuildStatusEditField.Value = 'OK';

            % Create ValidationStatusEditField
            app.ValidationStatusEditField = uieditfield(app.StatusTab, 'text');
            app.ValidationStatusEditField.Editable = 'off';
            app.ValidationStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ValidationStatusEditField.Position = [226 96 145 22];

            % Create SubmitStatusEditField
            app.SubmitStatusEditField = uieditfield(app.StatusTab, 'text');
            app.SubmitStatusEditField.Editable = 'off';
            app.SubmitStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SubmitStatusEditField.Position = [226 64 145 22];

            % Create BuildStatusButton
            app.BuildStatusButton = uibutton(app.StatusTab, 'push');
            app.BuildStatusButton.ButtonPushedFcn = createCallbackFcn(app, @BuildStatusButtonPushed, true);
            app.BuildStatusButton.Tooltip = {'Load unique targets from text file'};
            app.BuildStatusButton.Position = [377 127 36 23];
            app.BuildStatusButton.Text = '...';

            % Create ValidationStatusButton
            app.ValidationStatusButton = uibutton(app.StatusTab, 'push');
            app.ValidationStatusButton.ButtonPushedFcn = createCallbackFcn(app, @ValidationStatusButtonPushed, true);
            app.ValidationStatusButton.Tooltip = {'Load unique targets from text file'};
            app.ValidationStatusButton.Position = [378 96 36 23];
            app.ValidationStatusButton.Text = '...';

            % Create SubmitStatusButton
            app.SubmitStatusButton = uibutton(app.StatusTab, 'push');
            app.SubmitStatusButton.ButtonPushedFcn = createCallbackFcn(app, @SubmitStatusButtonPushed, true);
            app.SubmitStatusButton.Tooltip = {'Load unique targets from text file'};
            app.SubmitStatusButton.Position = [378 64 36 23];
            app.SubmitStatusButton.Text = '...';

            % Create RetreiveLabel
            app.RetreiveLabel = uilabel(app.StatusTab);
            app.RetreiveLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.RetreiveLabel.HorizontalAlignment = 'right';
            app.RetreiveLabel.Position = [6 29 50 22];
            app.RetreiveLabel.Text = 'Retreive';

            % Create SubmitTimeEditField_2
            app.SubmitTimeEditField_2 = uieditfield(app.StatusTab, 'text');
            app.SubmitTimeEditField_2.Editable = 'off';
            app.SubmitTimeEditField_2.BackgroundColor = [1 0.9882 0.8196];
            app.SubmitTimeEditField_2.Position = [79 29 136 22];

            % Create FoldersFilesPanel
            app.FoldersFilesPanel = uipanel(app.UIFigure);
            app.FoldersFilesPanel.Title = 'Folders & Files';
            app.FoldersFilesPanel.BackgroundColor = [0.8 0.8 0.8];
            app.FoldersFilesPanel.Position = [904 210 407 130];

            % Create BaseDataDirEditFieldLabel
            app.BaseDataDirEditFieldLabel = uilabel(app.FoldersFilesPanel);
            app.BaseDataDirEditFieldLabel.HorizontalAlignment = 'right';
            app.BaseDataDirEditFieldLabel.Position = [4 79 80 22];
            app.BaseDataDirEditFieldLabel.Text = 'Base Data Dir';

            % Create BaseDataDirEditField
            app.BaseDataDirEditField = uieditfield(app.FoldersFilesPanel, 'text');
            app.BaseDataDirEditField.Editable = 'off';
            app.BaseDataDirEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BaseDataDirEditField.Position = [100 77 291 22];

            % Create CalSubDirEditFieldLabel
            app.CalSubDirEditFieldLabel = uilabel(app.FoldersFilesPanel);
            app.CalSubDirEditFieldLabel.HorizontalAlignment = 'right';
            app.CalSubDirEditFieldLabel.Position = [18 44 66 22];
            app.CalSubDirEditFieldLabel.Text = 'Cal Sub Dir';

            % Create CalSubDirEditField
            app.CalSubDirEditField = uieditfield(app.FoldersFilesPanel, 'text');
            app.CalSubDirEditField.Editable = 'off';
            app.CalSubDirEditField.BackgroundColor = [1 0.9882 0.8196];
            app.CalSubDirEditField.Position = [100 42 290 22];

            % Create CalObjFlieEditFieldLabel
            app.CalObjFlieEditFieldLabel = uilabel(app.FoldersFilesPanel);
            app.CalObjFlieEditFieldLabel.HorizontalAlignment = 'right';
            app.CalObjFlieEditFieldLabel.Position = [18 10 68 22];
            app.CalObjFlieEditFieldLabel.Text = 'Cal Obj Flie';

            % Create CalObjFileEditField
            app.CalObjFileEditField = uieditfield(app.FoldersFilesPanel, 'text');
            app.CalObjFileEditField.Editable = 'off';
            app.CalObjFileEditField.BackgroundColor = [1 0.9882 0.8196];
            app.CalObjFileEditField.Position = [100 8 289 22];

            % Create GeneralPanel
            app.GeneralPanel = uipanel(app.UIFigure);
            app.GeneralPanel.Title = 'General';
            app.GeneralPanel.Position = [18 474 369 198];

            % Create UniqueTargetsEditFieldLabel
            app.UniqueTargetsEditFieldLabel = uilabel(app.GeneralPanel);
            app.UniqueTargetsEditFieldLabel.HorizontalAlignment = 'right';
            app.UniqueTargetsEditFieldLabel.Position = [10 112 96 22];
            app.UniqueTargetsEditFieldLabel.Text = '# Unique Targets';

            % Create UniqueTargetsEditField
            app.UniqueTargetsEditField = uieditfield(app.GeneralPanel, 'text');
            app.UniqueTargetsEditField.Editable = 'off';
            app.UniqueTargetsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.UniqueTargetsEditField.Position = [122 110 68 22];

            % Create PlanTargetsEditFieldLabel
            app.PlanTargetsEditFieldLabel = uilabel(app.GeneralPanel);
            app.PlanTargetsEditFieldLabel.HorizontalAlignment = 'right';
            app.PlanTargetsEditFieldLabel.Position = [23 80 82 22];
            app.PlanTargetsEditFieldLabel.Text = '# Plan Targets';

            % Create PlanTargetsEditField
            app.PlanTargetsEditField = uieditfield(app.GeneralPanel, 'text');
            app.PlanTargetsEditField.Editable = 'off';
            app.PlanTargetsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.PlanTargetsEditField.Position = [121 78 67 22];

            % Create PlanStatusEditFieldLabel
            app.PlanStatusEditFieldLabel = uilabel(app.GeneralPanel);
            app.PlanStatusEditFieldLabel.HorizontalAlignment = 'right';
            app.PlanStatusEditFieldLabel.Position = [38 47 66 22];
            app.PlanStatusEditFieldLabel.Text = 'Plan Status';

            % Create PlanStatusEditField
            app.PlanStatusEditField = uieditfield(app.GeneralPanel, 'text');
            app.PlanStatusEditField.Editable = 'off';
            app.PlanStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.PlanStatusEditField.Position = [120 45 107 22];

            % Create AstPlannerEditFieldLabel
            app.AstPlannerEditFieldLabel = uilabel(app.GeneralPanel);
            app.AstPlannerEditFieldLabel.HorizontalAlignment = 'right';
            app.AstPlannerEditFieldLabel.Position = [38 15 67 22];
            app.AstPlannerEditFieldLabel.Text = 'Ast Planner';

            % Create AstPlannerEditField
            app.AstPlannerEditField = uieditfield(app.GeneralPanel, 'text');
            app.AstPlannerEditField.Editable = 'off';
            app.AstPlannerEditField.BackgroundColor = [1 0.9882 0.8196];
            app.AstPlannerEditField.Position = [121 13 106 22];

            % Create ChangeButton
            app.ChangeButton = uibutton(app.GeneralPanel, 'push');
            app.ChangeButton.ButtonPushedFcn = createCallbackFcn(app, @ChangeButtonPushed, true);
            app.ChangeButton.Position = [261 140 85 27];
            app.ChangeButton.Text = 'Change';

            % Create PlanTypeDropDownLabel
            app.PlanTypeDropDownLabel = uilabel(app.GeneralPanel);
            app.PlanTypeDropDownLabel.HorizontalAlignment = 'right';
            app.PlanTypeDropDownLabel.Position = [49 142 58 22];
            app.PlanTypeDropDownLabel.Text = 'Plan Type';

            % Create PlanTypeDropDown
            app.PlanTypeDropDown = uidropdown(app.GeneralPanel);
            app.PlanTypeDropDown.Items = {'HCS', 'LCS', 'DDT', 'AllSky', 'TOO'};
            app.PlanTypeDropDown.Enable = 'off';
            app.PlanTypeDropDown.BackgroundColor = [1 1 1];
            app.PlanTypeDropDown.Position = [118 142 113 22];
            app.PlanTypeDropDown.Value = 'HCS';

            % Create Label_4
            app.Label_4 = uilabel(app.UIFigure);
            app.Label_4.FontWeight = 'bold';
            app.Label_4.FontColor = [0 0 1];
            app.Label_4.Position = [21 444 347 22];
            app.Label_4.Text = 'Fields editable only when plan table is empty (before build)';

            % Create MinimumDistanceFromPanel
            app.MinimumDistanceFromPanel = uipanel(app.UIFigure);
            app.MinimumDistanceFromPanel.Title = 'Minimum Distance From';
            app.MinimumDistanceFromPanel.BackgroundColor = [0.8 0.8 0.8];
            app.MinimumDistanceFromPanel.Position = [904 350 407 140];

            % Create SunLabel
            app.SunLabel = uilabel(app.MinimumDistanceFromPanel);
            app.SunLabel.Position = [10 75 26 22];
            app.SunLabel.Text = 'Sun';

            % Create EarthLabel
            app.EarthLabel = uilabel(app.MinimumDistanceFromPanel);
            app.EarthLabel.Position = [9 47 34 22];
            app.EarthLabel.Text = 'Earth';

            % Create MoonLabel
            app.MoonLabel = uilabel(app.MinimumDistanceFromPanel);
            app.MoonLabel.Position = [8 14 35 22];
            app.MoonLabel.Text = 'Moon';

            % Create DuringObservationLabel
            app.DuringObservationLabel = uilabel(app.MinimumDistanceFromPanel);
            app.DuringObservationLabel.FontWeight = 'bold';
            app.DuringObservationLabel.Position = [46 99 117 22];
            app.DuringObservationLabel.Text = 'During Observation';

            % Create DuringSlewLabel
            app.DuringSlewLabel = uilabel(app.MinimumDistanceFromPanel);
            app.DuringSlewLabel.FontWeight = 'bold';
            app.DuringSlewLabel.Position = [172 99 74 22];
            app.DuringSlewLabel.Text = 'During Slew';

            % Create SunMinDistObsEditField
            app.SunMinDistObsEditField = uieditfield(app.MinimumDistanceFromPanel, 'text');
            app.SunMinDistObsEditField.Editable = 'off';
            app.SunMinDistObsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SunMinDistObsEditField.Position = [66 77 50 22];

            % Create EarthMinDistObsEditField
            app.EarthMinDistObsEditField = uieditfield(app.MinimumDistanceFromPanel, 'text');
            app.EarthMinDistObsEditField.Editable = 'off';
            app.EarthMinDistObsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.EarthMinDistObsEditField.Position = [66 45 50 22];

            % Create MoonMinDistObsEditField
            app.MoonMinDistObsEditField = uieditfield(app.MinimumDistanceFromPanel, 'text');
            app.MoonMinDistObsEditField.Editable = 'off';
            app.MoonMinDistObsEditField.BackgroundColor = [1 0.9882 0.8196];
            app.MoonMinDistObsEditField.Position = [66 15 50 22];

            % Create SunMinDistSlewEditField
            app.SunMinDistSlewEditField = uieditfield(app.MinimumDistanceFromPanel, 'text');
            app.SunMinDistSlewEditField.Editable = 'off';
            app.SunMinDistSlewEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SunMinDistSlewEditField.Position = [180 77 50 22];

            % Create EarthMinDistSlewEditField
            app.EarthMinDistSlewEditField = uieditfield(app.MinimumDistanceFromPanel, 'text');
            app.EarthMinDistSlewEditField.Editable = 'off';
            app.EarthMinDistSlewEditField.BackgroundColor = [1 0.9882 0.8196];
            app.EarthMinDistSlewEditField.Position = [180 45 50 22];

            % Create MoonMinDistSlewEditField
            app.MoonMinDistSlewEditField = uieditfield(app.MinimumDistanceFromPanel, 'text');
            app.MoonMinDistSlewEditField.Editable = 'off';
            app.MoonMinDistSlewEditField.BackgroundColor = [1 0.9882 0.8196];
            app.MoonMinDistSlewEditField.Position = [180 14 50 22];

            % Create ValidationResultPanel
            app.ValidationResultPanel = uipanel(app.UIFigure);
            app.ValidationResultPanel.Title = 'Validation Result';
            app.ValidationResultPanel.BackgroundColor = [0.9686 0.9686 0.9294];
            app.ValidationResultPanel.Position = [398 85 494 117];

            % Create LabelTopStatus
            app.LabelTopStatus = uilabel(app.UIFigure);
            app.LabelTopStatus.BackgroundColor = [1 1 0.549];
            app.LabelTopStatus.HorizontalAlignment = 'center';
            app.LabelTopStatus.FontSize = 13;
            app.LabelTopStatus.FontWeight = 'bold';
            app.LabelTopStatus.FontAngle = 'italic';
            app.LabelTopStatus.FontColor = [0.102 0.102 0.4];
            app.LabelTopStatus.Position = [17 686 1296 22];
            app.LabelTopStatus.Text = 'Only selected parameters can be edited. Other values are automatically calculated or remain read-only.';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlanParams(varargin)

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