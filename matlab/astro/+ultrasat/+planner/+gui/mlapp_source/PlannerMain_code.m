classdef PlannerMain < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                        matlab.ui.Figure
        FileMenu                        matlab.ui.container.Menu
        NewMenu                         matlab.ui.container.Menu
        OpenMenu                        matlab.ui.container.Menu
        SaveMenu                        matlab.ui.container.Menu
        DuplicateMenu                   matlab.ui.container.Menu
        CloseMenu_2                     matlab.ui.container.Menu
        DeleteMenu                      matlab.ui.container.Menu
        SaveToLocalFileMenu             matlab.ui.container.Menu
        OpenFromLocalFileMenu           matlab.ui.container.Menu
        Menu_2                          matlab.ui.container.Menu
        ConnectLoginMenu                matlab.ui.container.Menu
        DisconnectLogoutMenu            matlab.ui.container.Menu
        ExitPlannerMenu                 matlab.ui.container.Menu
        TargetsMenu                     matlab.ui.container.Menu
        AddUniqueTargetMenu             matlab.ui.container.Menu
        EditUniqueTargetMenu            matlab.ui.container.Menu
        DeleteUniqueTargetMenu          matlab.ui.container.Menu
        ClearAllUniqueTargetsMenu       matlab.ui.container.Menu
        ViewUniqueTargetsTableMenu      matlab.ui.container.Menu
        SaveUniqueTargetsToFileMenu     matlab.ui.container.Menu
        LoadUniqueTargetsFromFileMenu   matlab.ui.container.Menu
        PlanMenu_2                      matlab.ui.container.Menu
        EditPlanTargetMenu              matlab.ui.container.Menu
        DeletePlanTargetMenu            matlab.ui.container.Menu
        ClearAllPlanTargetsMenu         matlab.ui.container.Menu
        ViewPlanTableMenu               matlab.ui.container.Menu
        Menu_3                          matlab.ui.container.Menu
        ParamsMenu                      matlab.ui.container.Menu
        PlanHistoryMenu                 matlab.ui.container.Menu
        Menu                            matlab.ui.container.Menu
        BuildMenu                       matlab.ui.container.Menu
        ValidateMenu                    matlab.ui.container.Menu
        SubmitMenu                      matlab.ui.container.Menu
        PlotsMenu                       matlab.ui.container.Menu
        ViewSkyMapPlotWindowMenu        matlab.ui.container.Menu
        ViewGraphsPlotWindowMenu        matlab.ui.container.Menu
        ClearPlotsMenu                  matlab.ui.container.Menu
        ViewMenu                        matlab.ui.container.Menu
        RefreshMenu                     matlab.ui.container.Menu
        LogWindowMenu                   matlab.ui.container.Menu
        ErrorLogWindowMenu              matlab.ui.container.Menu
        ToolsMenu                       matlab.ui.container.Menu
        SNRCalculatorMenu               matlab.ui.container.Menu
        HelpMenu                        matlab.ui.container.Menu
        ObservationPlannerHelpMenu      matlab.ui.container.Menu
        LogsHelpMenu                    matlab.ui.container.Menu
        AboutMenu                       matlab.ui.container.Menu
        PanelTopHeader                  matlab.ui.container.Panel
        LabelTopTime                    matlab.ui.control.Label
        LabelTopUser                    matlab.ui.control.Label
        LabelTopNamespace               matlab.ui.control.Label
        LabelTopStatus                  matlab.ui.control.Label
        TabGroup2                       matlab.ui.container.TabGroup
        StatusTab                       matlab.ui.container.Tab
        HelpStatusInfoButton            matlab.ui.control.Button
        SubmitStatusButton              matlab.ui.control.Button
        ValidationStatusButton          matlab.ui.control.Button
        BuildStatusButton               matlab.ui.control.Button
        SubmitShortStatusEditField      matlab.ui.control.EditField
        ValidationShortStatusEditField  matlab.ui.control.EditField
        BuildShortStatusEditField       matlab.ui.control.EditField
        SubmitTimeEditField             matlab.ui.control.EditField
        SubmitEditFieldLabel            matlab.ui.control.Label
        ValidationTimeEditField         matlab.ui.control.EditField
        ValidationEditFieldLabel        matlab.ui.control.Label
        BuildTimeEditField              matlab.ui.control.EditField
        BuildEditFieldLabel             matlab.ui.control.Label
        Panel_8                         matlab.ui.container.Panel
        RefreshApprovedTargetsButton_4  matlab.ui.control.Button
        RefreshApprovedTargetsButton_2  matlab.ui.control.Button
        StatusTextArea                  matlab.ui.control.TextArea
        StatusTextAreaLabel             matlab.ui.control.Label
        TabGroup                        matlab.ui.container.TabGroup
        PlanParamsTab                   matlab.ui.container.Tab
        PlanPkEditField                 matlab.ui.control.EditField
        PkEditFieldLabel                matlab.ui.control.Label
        HelpPlanParamsButton            matlab.ui.control.Button
        EndTimeEditField                matlab.ui.control.EditField
        EndTimeEditFieldLabel           matlab.ui.control.Label
        StartTimeEditField              matlab.ui.control.EditField
        StartTimeEditFieldLabel         matlab.ui.control.Label
        PlanTitleEditField              matlab.ui.control.EditField
        PlanTitleEditFieldLabel         matlab.ui.control.Label
        UserNameEditField               matlab.ui.control.EditField
        UserNameEditFieldLabel          matlab.ui.control.Label
        PlanTypeEditField               matlab.ui.control.EditField
        PlanTypeEditFieldLabel          matlab.ui.control.Label
        PlotGraphsDoubleClickUniqueTargetorPlanrowPanel  matlab.ui.container.Panel
        Panel_11                        matlab.ui.container.Panel
        OpenRefImagesButton             matlab.ui.control.Button
        OpenFieldObjButton              matlab.ui.control.Button
        OpenExtSurveysButton            matlab.ui.control.Button
        OpenCalObjTableButton           matlab.ui.control.Button
        HelpGraphsPlotWindowButton      matlab.ui.control.Button
        OpenGraphsPlotWindowButton      matlab.ui.control.Button
        ButtonGroup                     matlab.ui.container.ButtonGroup
        VisibilityPlotTimeUnitsDropDown  matlab.ui.control.DropDown
        PlanTypeEditFieldLabel_2        matlab.ui.control.Label
        PlotFlagVisibilityMoonCheckBox  matlab.ui.control.CheckBox
        PlotFlagVisibilityEarthCheckBox  matlab.ui.control.CheckBox
        PlotFlagVisibilitySunCheckBox   matlab.ui.control.CheckBox
        PlotCalibObjDropDown            matlab.ui.control.DropDown
        CalibrationStarButton           matlab.ui.control.RadioButton
        VisibilityButton                matlab.ui.control.RadioButton
        GraphPlotUniqueTargetDropDown   matlab.ui.control.DropDown
        cooSysLabel_3                   matlab.ui.control.Label
        AxesGraphsPlot                  matlab.ui.control.UIAxes
        PlotSkyMapCurrentlyshowsgeneralskymapPanel  matlab.ui.container.Panel
        HelpSkyMapPlotWindowButton      matlab.ui.control.Button
        OpenSkyMapPlotWindowButton      matlab.ui.control.Button
        Panel_10                        matlab.ui.container.Panel
        PlotCooSysDropDown              matlab.ui.control.DropDown
        cooSysLabel_4                   matlab.ui.control.Label
        UpdateSkyMapButton              matlab.ui.control.Button
        PlotFlagVisibleCheckBox         matlab.ui.control.CheckBox
        PlotFlagApprovedCheckBox        matlab.ui.control.CheckBox
        PlotFlagCalibrationCheckBox     matlab.ui.control.CheckBox
        PlotFlagExtinctionCheckBox      matlab.ui.control.CheckBox
        PlotFlagPlanCheckBox            matlab.ui.control.CheckBox
        PlotFlagUniqueCheckBox          matlab.ui.control.CheckBox
        AxesSkymapPlot                  matlab.ui.control.UIAxes
        ApprovedTargetsPanel            matlab.ui.container.Panel
        HelpUniqueTargetsWindowButton_3  matlab.ui.control.Button
        ShowApprovedTargetsWindowButton  matlab.ui.control.Button
        Panel_5                         matlab.ui.container.Panel
        RefreshApprovedTargetsButton_3  matlab.ui.control.Button
        RefreshApprovedTargetsButton    matlab.ui.control.Button
        UITableApprovedTargets          matlab.ui.control.Table
        PlanPanel                       matlab.ui.container.Panel
        HelpUniqueTargetsWindowButton_2  matlab.ui.control.Button
        ShowPlanRowsWindowButton        matlab.ui.control.Button
        Panel_4                         matlab.ui.container.Panel
        EditPlanTargetButton_6          matlab.ui.control.Button
        EditPlanTargetButton_3          matlab.ui.control.Button
        EditPlanTargetButton_2          matlab.ui.control.Button
        EditPlanTargetButton            matlab.ui.control.Button
        CheckPlanTargetsButton          matlab.ui.control.Button
        BuildButton                     matlab.ui.control.Button
        UITablePlanTargets              matlab.ui.control.Table
        UniqueTargetsPanel              matlab.ui.container.Panel
        UniqueTargetsClearOrderButton   matlab.ui.control.Button
        UniqueTargetsSetOrderButton     matlab.ui.control.Button
        HelpUniqueTargetsWindowButton   matlab.ui.control.Button
        ShowUniqueTargetsWindowButton   matlab.ui.control.Button
        Panel_6                         matlab.ui.container.Panel
        EditPlanTargetButton_5          matlab.ui.control.Button
        EditPlanTargetButton_4          matlab.ui.control.Button
        LoadUniqueTargetsButton         matlab.ui.control.Button
        EditUniqueTargetButton          matlab.ui.control.Button
        AddUniqueTargetButton           matlab.ui.control.Button
        UITableUniqueTargets            matlab.ui.control.Table
        PanelToolbar                    matlab.ui.container.Panel
        HelpButton                      matlab.ui.control.Button
        QAButton                        matlab.ui.control.Button
        RetractButton                   matlab.ui.control.Button
        DuplicateButton                 matlab.ui.control.Button
        ModifiedLabel                   matlab.ui.control.Label
        SNRCalcButton                   matlab.ui.control.Button
        LoginButton                     matlab.ui.control.Button
        ParamsButton                    matlab.ui.control.Button
        ValidateButton                  matlab.ui.control.Button
        SubmitButton                    matlab.ui.control.Button
        SaveButton                      matlab.ui.control.Button
        OpenButton                      matlab.ui.control.Button
        NewButton                       matlab.ui.control.Button
    end

    
    % =====================================================================
    %                              Properties
    % =====================================================================
    
    properties (Access = public)
        % Data
        MainModule                  % ultrasat.planner.guiutils.MainModule
        TimerSec                    % Timer to update the current time display

        % =================================================================
        %   Forms (AppDesigner Apps in ultrasat.planner.gui) - sorted abc
        % =================================================================        
        
        AboutApp                                % About
        AddUniqueTargetApp                      % Unique Target - Add
        AdjustGroupStartTimeApp                 %
        ApprovedTargetParamsApp                 %
        ApprovedTargetsApp                      %
        BuildStatusApp                          %        
        CalibObjTableApp                        % for Plot
        RefImagesTableApp                       % for Plot
        ExtSurveysTableApp                      % for Plot
        FieldObjTableApp                        % for Plot
        DuplicatePlanApp                        %
        EnterStartTimeApp                       %
        ErrorLoggerApp                          %                
        LoadPlanFromFileApp                     % Load file from disk
        LoadUniqueTargetsFromFileApp            % Load from text file        
        LoggerApp                               %
        LoginApp                                % Login
        NewPlanApp                              % Description
        OpenPlanApp                             % Plan - Open
        PlanHistoryApp                          %                
        PlanParamsApp                           %                
        PlanTargetParamsApp                     %
        PlanTargetsApp                          %
        PleaseWaitDlg                           % Please wait dialog        
        PlotSkyMapApp                           %
        PlotGraphsApp                           %        
        PreferencesApp                          %
        SavePlanToFileApp                       % Save to file
        SaveUniqueTargetsToFileApp              % Save to text file        
        SettingsApp                             % Settings        
        SubmitStatusApp                         %
        UniqueTargetParamsApp                   % Unique Target - Params
        UniqueTargetsApp                        % Unique Targets - Window
        ValidationStatusApp                     % Validation stauts & history
        MsgBoxApp                               %                 

        % =================================================================
        % Data
        Preferences                             % Refrence to app.MainModule.Preferences

        % Helper classes in astro/+ultrasat/+planner/+guiutils/
        AppUtils                                %         
        GuiHelper                               % = MainModule.GuiHelper

        % PlannerMain helpers
        AppHelper                               % PlannerMainAppHelper                
        ApprovedTargetsHelper                   % PlannerMainApprovedTargetsHelper        
        BuildHelper                             % PlannerMainBuildHelper
        NewPlanHelper                           % PlannerMainNewPlanHelper        
        PlanParamsHelper                        % PlannerMainPlanParamsHelper
        PlanTargetsHelper                       % PlannerMainPlanTargetsHelper
        PlotHelper                              % PlannerMainPlotHelper                
        SessionHelper                           % PlannerMainSessionHelper
        StatusHelper                            % PlannerMainStatusHelper
        StorageHelper                           % PlannerMainStorageHelper                
        SubmitHelper                            % PlannerMainSubmitHelper
        TablesHelper                            % PlannerMainTablesHelper
        ValidationHelper                        % PlannerMainValidationHelper        
        UniqueTargetsHelper                     % PlannerMainUniqueTargetsHelper                       
    end

    % =====================================================================
    %                         Init, Login, Log
    % =====================================================================
    methods (Access = public)
        function init(app)
            % Called from startupFcn() on application startup

            % Create MainModule that holds all common data
            app.UIFigure.Name = 'ULTRASAT Observation Planner, v0.2 (updated 19/02/2026)';

            % 
            app.checkMatlabVersion();
            app.checkSocPath();

            % Create MainModule that holds all common data and link objects
            app.MainModule = ultrasat.planner.guiutils.MainModule();
            app.MainModule.MainApp = app;
            app.GuiHelper = app.MainModule.GuiHelper;

            % Create AppUtils and set in MainModule
            app.AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule, app);
            app.MainModule.AppUtils = app.AppUtils;

            % Assign preferences loaded by MainModule
            app.Preferences = app.MainModule.Preferences;

            % Create hidden log window, link it to MainModule
            app.LoggerApp = ultrasat.planner.gui.Logger(app.MainModule);
            app.LoggerApp.UIFigure.Visible = 'off';
            app.MainModule.LoggerApp = app.LoggerApp;
            app.msglog('init started');

            % Create hidden errorlog window, link it to MainModule
            app.ErrorLoggerApp = ultrasat.planner.gui.ErrorLogger(app.MainModule);
            app.ErrorLoggerApp.UIFigure.Visible = 'off';
            app.MainModule.ErrorLoggerApp = app.ErrorLoggerApp;

            % Register both with the global LogManager so the windows can
            % be used from any class derived from Loggable
            ultrasat.api.utils.LogManager.registerLoggerApps(app.LoggerApp, app.ErrorLoggerApp);

            % Create helpers classes
            app.AppHelper               = ultrasat.planner.guiutils.PlannerMainAppHelper();            
            app.ApprovedTargetsHelper   = ultrasat.planner.guiutils.PlannerMainApprovedTargetsHelper();
            app.BuildHelper             = ultrasat.planner.guiutils.PlannerMainBuildHelper();
            app.NewPlanHelper           = ultrasat.planner.guiutils.PlannerMainNewPlanHelper();
            app.PlanParamsHelper        = ultrasat.planner.guiutils.PlannerMainPlanParamsHelper();
            app.PlanTargetsHelper       = ultrasat.planner.guiutils.PlannerMainPlanTargetsHelper();            
            app.SessionHelper           = ultrasat.planner.guiutils.PlannerMainSessionHelper();                        
            app.StorageHelper           = ultrasat.planner.guiutils.PlannerMainStorageHelper();                                   
            app.StatusHelper            = ultrasat.planner.guiutils.PlannerMainStatusHelper();                        
            app.PlotHelper              = ultrasat.planner.guiutils.PlannerMainPlotHelper();
            app.SubmitHelper            = ultrasat.planner.guiutils.PlannerMainSubmitHelper();                        
            app.TablesHelper            = ultrasat.planner.guiutils.PlannerMainTablesHelper();
            app.ValidationHelper        = ultrasat.planner.guiutils.PlannerMainValidationHelper();
            app.UniqueTargetsHelper     = ultrasat.planner.guiutils.PlannerMainUniqueTargetsHelper();

            % Create one second timer
            app.TimerSec = timer('ExecutionMode', 'fixedRate', 'Period', 1, 'TimerFcn', @(~,~) updateTimerSec(app));
            start(app.TimerSec);

            % Set initial state for buttons and menus
            app.SessionHelper.setLoginButtonStatus(app);
            app.SessionHelper.setButtons(app);

            app.msglog('init done');
        end


        function checkMatlabVersion(app)
            % Required version
            v = ver('MATLAB');            
            isOK = strcmp(v.Release, '(R2023a)');
        
            if ~isOK
                % Blocking popup
                uialert(app.UIFigure, sprintf(['This ULTRASAT Planner has only been tested with MATLAB R2023a.\n' ...
                             'Your version is: %s\n\n' ...
                             'Please use MATLAB R2023a for guaranteed compatibility.'], v.Release), ...
                             'MATLAB Version Not Supported');
            end
        end


        function checkSocPath(app)
            % Set default SOC_PATH if not exist
            soc_path = getenv('SOC_PATH');
            if ~isempty(soc_path)
                fprintf('PlannerMain: SOC_PATH = %s\n', soc_path);
            else
                if ispc
                    soc_path = 'c:/soc';
                else
                    soc_path = '~/soc';
                end
                setenv('SOC_PATH', soc_path);
                uialert(app.UIFigure, sprintf('SOC_PATH not found in env, setting to default: %s\n', soc_path), ...
                    'SOC_PATH not found in env');
            end               
        end


        function updateTimerSec(app)
            currentTime = datetime('now', 'TimeZone', 'UTC');
            formattedTime = strcat('UTC: ', datestr(currentTime, 'yyyy-mm-dd HH:MM:SS'));
            app.LabelTopTime.Text = formattedTime;

            if app.MainModule.IsVirtualTime
                app.LabelTopTime.FontColor = [1.00,0.07,0.65];  % Pink
            else
                app.LabelTopTime.FontColor = [0.65,0.65,0.65];  % Gray
            end
        end

        % =================================================================
        %                            User Login
        % =================================================================

        function Result = isAllowed(app, Action)
            % Return true if specified action is allowed for current logged-in user
            % @TODO - To be replaced with checking permissions like Delphi code
            Result = app.SessionHelper.isAllowed(app, Action);
        end 

    end

    % =====================================================================
    %           Plan - Open, Save, Close, Load, Duplicate Plan
    % =====================================================================    
    methods (Access = public)

        function showPlanAll(app)
            % Update GUI: Plan params, Unique Targets, Plan Targets, Approved Targets
            app.PlanParamsHelper.updatePlanParams(app);
            app.UniqueTargetsHelper.showUniqueTargets(app);
            app.PlanTargetsHelper.showPlanTargets(app);
            app.ApprovedTargetsHelper.showApprovedTargets(app);

            % Clear plots
            if ~app.hasPlanner()
                app.PlotHelper.clearPlots(app);
            end
        end        
    end
   
    % =====================================================================
    %                           Other Windows
    % =====================================================================

    methods (Access = public)    
        function showAboutWindow(app)
            % Show the About window
            if isempty(app.AboutApp) || ~isvalid(app.AboutApp)
                app.AboutApp = ultrasat.planner.gui.About(app.MainModule);                
            end
            app.showModal(app.AboutApp);
        end

        function showHelp(app, item)
            % Open website in browser window, use system default browser (-browser)
            if isempty(item)
                web('http://socsrv/ultrasat/docs/help/planner/index.html', '-browser');
                %web('https://docs.google.com/document/d/e/2PACX-1vTQKjJmBjzmcSXwaIRsq3FviYYpsW-Of7fewwcavCErBG7Pg589j3viLrUmNIr-NM-EfRWfQI4n0PdE/pub', '-browser');
            else
                item = ['http://socsrv/ultrasat/docs/help/planner/', item, '.html'];
                web(item, '-browser');
            end
        end

        function showGDriveQA(app)
            % Open website in browser window, use system default browser (-browser)
            web('https://docs.google.com/document/d/1rq6T1yOe_HB_Exww29YYcbgDvO3x1dg1gQCQtCrk9Y8/edit?usp=sharing', '-browser');
            % web('https://docs.google.com/document/d/1iXs3Z5SHNnA8vUEf557DT3qIo9_rqELBp_WYg2vrYi0/edit?usp=sharing', '-browser');
        end        

        function showSnrCalculator(app)
            % Open website in browser window, with out SNR Calculator, use system default browser (-browser)
            web('https://www.weizmann.ac.il/ultrasat/for-scientists/snr-calculator', '-browser');
            %web('https://snrapp.ultrasatsoc.org/');            
        end
    end

    % =====================================================================
    %                           Validate % Submit    
    % =====================================================================
    methods (Access = public)    

        function validate(app)
            % Validate plan by sending it to the Validation service
            app.ValidationHelper.validate(app);
        end        

        function updateValidateStatus(app)
            % Update the validation status field
            app.ValidationHelper.updateValidateStatus(app);
        end

        function submit(app)
            % Submit plan by sending it to Mission Control service
            % Debug: see files in D:\Ultrasat\AstroPack\matlab\astro\+ultrasat\+api\sim
            app.SubmitHelper.submit(app);
        end        

    end

    % =====================================================================
    %                               Status
    % =====================================================================    

    methods (Access = public)            

        function applyPlanStatus(app)
            app.StatusHelper.applyPlanStatus(app);
        end

        function setEditable(app, Editable)
            app.StatusHelper.setEditable(app, Editable);
        end

        function Result = isEditable(app)
            Result = app.StatusHelper.isEditable(app);
        end
        
        function Result = isEditableMsg(app)
            Result = app.StatusHelper.isEditableMsg(app);
        end        

        function setModified(app, logText)
            app.StatusHelper.setModified(app, logText);
        end

        function clearModified(app)
            app.StatusHelper.clearModified(app);
        end

        function Result = needSave(app, AskSave)
            Result = app.StatusHelper.needSave(app, AskSave);
        end

        function setStatus(app, Status, Text)
            app.StatusHelper.setStatus(app, Status, Text);
        end

        function setStatusEx(app, Title, ME)
            app.StatusHelper.setStatusEx(app, Title, ME);
        end

        function updateStatus(app)
            app.StatusHelper.updateStatus(app);
        end

        function setStatusField(app, EditField, Status, StatusText)
            app.StatusHelper.setStatusField(app, EditField, Status, StatusText);
        end

        function setTopLabel(app, Text, FontColor, BackgroundColor)
            app.StatusHelper.setTopLabel(app, Text, FontColor, BackgroundColor);
        end        
    end

    % =====================================================================
    %                   Low Level & Utility Functions
    % =====================================================================

    methods (Access = public)        

        function Result = checkSelectedTableRow(app, Tab, Index)
            % Check that index is valid row number in the specified table
            Result = ~isempty(Index) && Index >= 1 && Index <= height(Tab);
        end

        function Result = getSelectedTableRowAsStruct(app, Tab, Index)
            % Convert the selected table row to struct, return [] if Index
            % is not a valid row number in Tab
            Result = [];
            if ~isempty(Index) && Index >= 1 && Index <= height(Tab)
                Result = table2struct(Tab(Index, :));
            end                
        end               

        function Status = showModal(app, FormApp)
            % Helper: Show modal app window and return FormApp.Status
            % Call FormApp.beforeShow() if such function exists in FormApp
            % Note: FormApp should have 'Status' property
            Status = app.MainModule.GuiHelper.showModal(app, FormApp);
        end

        function handleCloseRequest(app, FormApp)
            % Used from showModal() above
            % Handle 'X' button close event
            if isvalid(FormApp)  % Ensure app is still valid before modifying properties
                FormApp.Status = 'Cancel';                
                uiresume(FormApp.UIFigure);  % Resume execution
                %delete(FormApp);  % Delete the app safely
            end
        end       

        % =================================================================
        %                    Low Level Utilities
        % =================================================================

        function msglog(app, s)
            % Log message to console & file
            app.MainModule.msglog(s);            
        end

        function msgex(app, s, ME)
            % Log exception to console & file, display popup message to the user
            app.setStatus('Error', ME.message)
            app.MainModule.msglog(sprintf('Exception: %s - %s', s, ME.message));
            app.AppUtils.msgError(ME.message, s);
        end        

        function debugSave(app, FileName, Obj)
            % Save the specified Obj to file in DebugPath
            FileName = fullfile(app.MainModule.DebugPath, FileName);
            app.msglog(sprintf('debugSave: %s', FileName));
            save(FileName, 'Obj')
        end

        function showPleaseWait(app, Message)
            % Shows a spinner popup with 'Please Wait' message
            app.PleaseWaitDlg = uiprogressdlg(app.UIFigure, 'Title', 'Please wait', 'Message', Message, 'Indeterminate', 'on');  
        end

        function closePleaseWait(app)
            % Close the Please Wait popup message
            if ~isempty(app.PleaseWaitDlg)
                close(app.PleaseWaitDlg);
                app.PleaseWaitDlg = [];
            end
        end

        function result = hasPlanner(app)
            % Helper: Return true if there is active planner object
            result = ~isempty(app.MainModule.Planner);
            if ~result
                %app.msglog('hasPlanner: None');
            end
        end

        function result = hasPlannerMsg(app)
            % Helper: Return true if there is active planner object
            result = ~isempty(app.MainModule.Planner);
            if ~result
                app.MainModule.AppUtils.msgOk('No observation plan is currently open. Please open or create a plan first.', 'No Active Plan');
                %app.msglog('hasPlanner: None');
            end
        end        

        function result = hasPlan(app)
            % Return true if there is active planner object and
            % plan targets list is not empty.
            result = ~isempty(app.MainModule.Planner) && (height(app.MainModule.Planner.Plan) > 0);
            if ~result
                app.msglog('hasPlan: None');
            end
        end        

        function addHistory(app, msg)
            % Add message to current plan history log
            if app.hasPlanner()
                app.MainModule.PlanData.addHistory(msg);
            end
        end        

        function savePreferences(app)
            % Save user preferences to local file
            app.Preferences.save();
        end        
    end

    % =====================================================================
    %
    %               AppDesigner Callbacks are Generated Below
    %
    % =====================================================================    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, varargin)
            % This function is automaticalled called on application startup
            fprintf('============ STARTING ULTRASAT OBSERVATION PLANNER GUI ============\n')
            
            % Initialize the app
            app.init();            
        end

        % Button pushed function: NewButton
        function NewButtonPushed(app, event)
            app.NewPlanHelper.createNewPlan(app);
        end

        % Menu selected function: ConnectLoginMenu
        function ConnectLoginMenuSelected(app, event)
            app.SessionHelper.login(app);
        end

        % Menu selected function: DisconnectLogoutMenu
        function DisconnectLogoutMenuSelected(app, event)
            app.SessionHelper.logout(app);
        end

        % Callback function
        function NewMenuSelected(app, event)
            app.NewPlanHelper.createNewPlan(app);
        end

        % Callback function
        function OpenMenuSelected(app, event)
            app.StorageHelper.openPlan(app);
        end

        % Callback function
        function SaveMenuSelected(app, event)
            app.StorageHelper.savePlan(app);
        end

        % Callback function
        function ExitPlannerMenuSelected(app, event)
            app.SessionHelper.exitPlanner(app);
        end

        % Callback function
        function UniqueTargetsMenuSelected(app, event)
            app.UniqueTargetsHelper.showUniqueTargetsWindow(app);
        end

        % Callback function
        function ParametersMenuSelected(app, event)
            app.PlanParamsHelper.showPlanParamsWindow(app);
        end

        % Callback function
        function ValidateMenuSelected(app, event)
            app.ValidationHelper.validate(app);
        end

        % Callback function
        function SubmitMenuSelected(app, event)
            app.SubmitHelper.submit(app);
        end

        % Callback function
        function SkyMapButtonPushed(app, event)
            app.PlotHelper.showSkyMapPlot(app);
        end

        % Callback function
        function GraphsButtonPushed(app, event)
            app.PlotHelper.showGraphsPlot(app);
        end

        % Menu selected function: AboutMenu
        function AboutMenuSelected(app, event)
            app.showAboutWindow();
        end

        % Button pushed function: AddUniqueTargetButton
        function AddUniqueTargetButtonPushed(app, event)
            app.UniqueTargetsHelper.addUniqueTarget(app);
        end

        % Button pushed function: EditUniqueTargetButton
        function EditUniqueTargetButtonPushed(app, event)
            app.UniqueTargetsHelper.editUniqueTarget(app);
        end

        % Callback function
        function RefreshUniqueTargetsButtonPushed(app, event)
            app.UniqueTargetsHelper.showUniqueTargets(app);
        end

        % Selection changed function: UITableUniqueTargets
        function UITableUniqueTargetsSelectionChanged(app, event)
            %app.UniqueTargetsIndices = event.Indices;
            selection = app.UITableUniqueTargets.Selection;
            if ~isempty(selection)
                app.UniqueTargetsHelper.uniqueTargetSelected(app, selection);
            end
        end

        % Button pushed function: SNRCalcButton
        function SNRCalcButtonPushed(app, event)
            app.showSnrCalculator();
        end

        % Callback function
        function RefreshPlanTargetsButtonPushed(app, event)
            app.PlanTargetsHelper.showPlanTargets();
        end

        % Button pushed function: RefreshApprovedTargetsButton
        function RefreshApprovedTargetsButtonPushed(app, event)
            app.ApprovedTargetsHelper.retrieveApprovedTargets(app);
        end

        % Menu selected function: ObservationPlannerHelpMenu
        function ObservationPlannerHelpMenuSelected(app, event)
            app.showHelp();
        end

        % Callback function
        function SaveMenuSelected2(app, event)
            app.StorageHelper.savePlan(app);
        end

        % Callback function
        function DeleteMenuSelected(app, event)
            app.StorageHelper.deletePlan(app);
        end

        % Callback function
        function SavePlantoFileMenuSelected(app, event)
            app.StorageHelper.savePlanToFile(app);
        end

        % Callback function
        function LoadPlanfromFileMenuSelected(app, event)
            app.StorageHelper.loadPlanFromFile(app);
        end

        % Menu selected function: AddUniqueTargetMenu
        function AddUniqueTargetMenuSelected(app, event)
            app.UniqueTargetsHelper.addUniqueTarget(app);
        end

        % Menu selected function: EditUniqueTargetMenu
        function EditUniqueTargetMenuSelected(app, event)
            app.UniqueTargetsHelper.editUniqueTarget(app);
        end

        % Menu selected function: DeleteUniqueTargetMenu
        function DeleteUniqueTargetMenuSelected(app, event)
            app.UniqueTargetsHelper.deleteUniqueTarget(app);
        end

        % Menu selected function: ClearAllUniqueTargetsMenu
        function ClearAllUniqueTargetsMenuSelected(app, event)
            app.UniqueTargetsHelper.clearUniqueTargets(app);
        end

        % Menu selected function: ViewUniqueTargetsTableMenu
        function ViewUniqueTargetsTableMenuSelected(app, event)
            app.UniqueTargetsHelper.showUniqueTargetsWindow(app);
        end

        % Menu selected function: SaveUniqueTargetsToFileMenu
        function SaveUniqueTargetsToFileMenuSelected(app, event)
            app.UniqueTargetsHelper.saveUniqueTargetsToFile(app);
        end

        % Menu selected function: LoadUniqueTargetsFromFileMenu
        function LoadUniqueTargetsFromFileMenuSelected(app, event)
            app.UniqueTargetsHelper.loadUniqueTargetsFromFile(app);
        end

        % Menu selected function: ViewSkyMapPlotWindowMenu
        function ViewSkyMapPlotWindowMenuSelected(app, event)
            app.PlotHelper.showSkyMapPlotWindow(app);
        end

        % Menu selected function: ViewGraphsPlotWindowMenu
        function ViewGraphsPlotWindowMenuSelected(app, event)
            app.PlotHelper.showGraphsPlotWindow(app);
        end

        % Menu selected function: EditPlanTargetMenu
        function EditPlanTargetMenuSelected(app, event)
            app.PlanTargetsHelper.editPlanTarget(app);
        end

        % Menu selected function: DeletePlanTargetMenu
        function DeletePlanTargetMenuSelected(app, event)
            app.PlanTargetsHelper.deletePlanTarget(app);
        end

        % Menu selected function: ClearAllPlanTargetsMenu
        function ClearAllPlanTargetsMenuSelected(app, event)
            app.PlanTargetsHelper.clearPlanTargets(app);
        end

        % Menu selected function: ViewPlanTableMenu
        function ViewPlanTableMenuSelected(app, event)
            app.PlanTargetsHelper.showPlanTargetsWindow(app);
        end

        % Button pushed function: OpenButton
        function OpenButtonPushed(app, event)
            app.StorageHelper.openPlan(app);
        end

        % Button pushed function: SaveButton
        function SaveButtonPushed(app, event)
            app.StorageHelper.savePlan(app);
        end

        % Button pushed function: ParamsButton
        function ParamsButtonPushed(app, event)
            app.PlanParamsHelper.showPlanParamsWindow(app);
        end

        % Button pushed function: ValidateButton
        function ValidateButtonPushed(app, event)
            app.ValidationHelper.validate(app);
        end

        % Button pushed function: SubmitButton
        function SubmitButtonPushed(app, event)
            app.SubmitHelper.submit(app);
        end

        % Button pushed function: BuildButton
        function BuildButtonPushed(app, event)
            app.BuildHelper.build(app);
        end

        % Button pushed function: EditPlanTargetButton
        function EditPlanTargetButtonPushed(app, event)
            app.PlanTargetsHelper.editPlanTarget(app);
        end

        % Button pushed function: LoginButton
        function LoginButtonPushed(app, event)
            if app.SessionHelper.isLogin(app)
                app.SessionHelper.logout(app);
            else
                app.SessionHelper.login(app);
            end
        end

        % Callback function
        function BuildMenuSelected(app, event)
            app.BuildHelper.build(app);
        end

        % Button pushed function: LoadUniqueTargetsButton
        function LoadUniqueTargetsButtonPushed(app, event)
            app.UniqueTargetsHelper.loadUniqueTargetsFromFile(app);
        end

        % Menu selected function: LogWindowMenu
        function LogWindowMenuSelected(app, event)
            app.SessionHelper.showLogger(app);
        end

        % Callback function
        function PlotCalibButtonPushed(app, event)
            app.PlotHelper.plotCalibObj(app);
        end

        % Button pushed function: RefreshApprovedTargetsButton_2
        function RefreshApprovedTargetsButton_2Pushed(app, event)
            app.MainModule.clearStatus();
            app.updateStatus();
        end

        % Button pushed function: CheckPlanTargetsButton
        function CheckPlanTargetsButtonPushed(app, event)
            app.PlanParamsHelper.checkPlanSelfConsistency(app);
        end

        % Selection changed function: UITablePlanTargets
        function UITablePlanTargetsSelectionChanged(app, event)
            selection = app.UITablePlanTargets.Selection;
            if ~isempty(selection)
                app.PlanTargetsHelper.planTargetSelected(app, selection);
            end
        end

        % Selection changed function: UITableApprovedTargets
        function UITableApprovedTargetsSelectionChanged(app, event)
            selection = app.UITableApprovedTargets.Selection;
            if ~isempty(selection)
                app.ApprovedTargetsHelper.approvedTargetSelected(app, selection);
            end
        end

        % Button pushed function: DuplicateButton
        function DuplicateButtonPushed(app, event)
            app.StorageHelper.duplicatePlan(app);
        end

        % Callback function
        function PlotVisibilityButtonPushed(app, event)
            app.PlotHelper.plotVisibility(app);
        end

        % Value changed function: PlotCalibObjDropDown
        function PlotCalibObjDropDownValueChanged(app, event)
            app.PlotHelper.plotCalibObj(app);
        end

        % Button pushed function: UpdateSkyMapButton
        function UpdateSkyMapButtonPushed(app, event)
            app.PlotHelper.showSkyMapPlot(app);
        end

        % Button pushed function: RefreshApprovedTargetsButton_3
        function RefreshApprovedTargetsButton_3Pushed(app, event)
            app.ApprovedTargetsHelper.clearApprovedTargets(app);
        end

        % Button pushed function: EditPlanTargetButton_6
        function EditPlanTargetButton_6Pushed(app, event)
            app.PlanTargetsHelper.adjustGroupStartTime(app);
        end

        % Menu selected function: RefreshMenu
        function RefreshMenuSelected(app, event)
            app.showPlanAll();
        end

        % Button pushed function: EditPlanTargetButton_5
        function EditPlanTargetButton_5Pushed(app, event)
            app.UniqueTargetsHelper.clearUniqueTargets(app);
        end

        % Button pushed function: EditPlanTargetButton_3
        function EditPlanTargetButton_3Pushed(app, event)
            app.PlanTargetsHelper.clearPlanTargets(app);
        end

        % Clicked callback: UITableUniqueTargets
        function UITableUniqueTargetsClicked(app, event)
            app.UniqueTargetsHelper.uniqueTargetClick(app);
        end

        % Double-clicked callback: UITableUniqueTargets
        function UITableUniqueTargetsDoubleClicked(app, event)
            app.UniqueTargetsHelper.uniqueTargetDoubleClick(app);
        end

        % Clicked callback: UITablePlanTargets
        function UITablePlanTargetsClicked(app, event)
            app.PlanTargetsHelper.planTargetClick(app);
        end

        % Double-clicked callback: UITablePlanTargets
        function UITablePlanTargetsDoubleClicked(app, event)
            app.PlanTargetsHelper.planTargetDoubleClick(app);
        end

        % Button pushed function: BuildStatusButton
        function BuildStatusButtonPushed(app, event)
            app.BuildHelper.showBuildStatusWindow(app);
        end

        % Button pushed function: ValidationStatusButton
        function ValidationStatusButtonPushed(app, event)
            app.ValidationHelper.showValidationStatusWindow(app);
        end

        % Button pushed function: SubmitStatusButton
        function SubmitStatusButtonPushed(app, event)
            app.SubmitHelper.showSubmitStatusWindow(app);
        end

        % Menu selected function: NewMenu
        function NewMenuSelected3(app, event)
            app.NewPlanHelper.createNewPlan(app);
        end

        % Menu selected function: OpenMenu
        function OpenMenuSelected3(app, event)
            app.StorageHelper.openPlan(app);
        end

        % Menu selected function: SaveMenu
        function SaveMenuSelected3(app, event)
            app.StorageHelper.savePlan(app);
        end

        % Menu selected function: DuplicateMenu
        function DuplicateMenuSelected(app, event)
            app.StorageHelper.duplicatePlan(app);
        end

        % Menu selected function: CloseMenu_2
        function CloseMenu_2Selected2(app, event)
            app.StorageHelper.closePlan(app);
        end

        % Menu selected function: DeleteMenu
        function DeleteMenuSelected2(app, event)
            app.StorageHelper.deletePlan(app);
        end

        % Menu selected function: SaveToLocalFileMenu
        function SaveToLocalFileMenuSelected(app, event)
            app.StorageHelper.savePlanToFile(app);
        end

        % Menu selected function: OpenFromLocalFileMenu
        function OpenFromLocalFileMenuSelected(app, event)
            app.StorageHelper.loadPlanFromFile(app);
        end

        % Menu selected function: ExitPlannerMenu
        function ExitPlannerMenuSelected2(app, event)
            app.SessionHelper.exitPlanner(app);
        end

        % Button pushed function: OpenCalObjTableButton
        function OpenCalObjTableButtonPushed(app, event)
            app.TablesHelper.showCalibObjTable(app);
        end

        % Button pushed function: OpenSkyMapPlotWindowButton
        function OpenSkyMapPlotWindowButtonPushed(app, event)
            app.PlotHelper.showSkyMapPlotWindow(app);
        end

        % Button pushed function: OpenGraphsPlotWindowButton
        function OpenGraphsPlotWindowButtonPushed(app, event)
            app.PlotHelper.showGraphsPlotWindow(app);
        end

        % Selection changed function: ButtonGroup
        function ButtonGroupSelectionChanged(app, event)
            app.PlotHelper.plotGraphs(app);
        end

        % Button pushed function: EditPlanTargetButton_2
        function EditPlanTargetButton_2Pushed(app, event)
            app.PlanTargetsHelper.deletePlanTarget(app);
        end

        % Button pushed function: QAButton
        function QAButtonPushed(app, event)
            app.showGDriveQA();
        end

        % Value changed function: PlanTitleEditField
        function PlanTitleEditFieldValueChanged(app, event)
            value = app.PlanTitleEditField.Value;
            if ~isempty(app.MainModule.Planner)
                app.MainModule.Planner.Title = value;
                app.msglog(sprintf('PlanTitleEditFieldValueChanged: %s', value));
            end
        end

        % Menu selected function: PlanHistoryMenu
        function PlanHistoryMenuSelected(app, event)
            app.PlanParamsHelper.showPlanHistory(app);
        end

        % Button pushed function: EditPlanTargetButton_4
        function EditPlanTargetButton_4Pushed(app, event)
            app.UniqueTargetsHelper.deleteUniqueTarget(app);
        end

        % Menu selected function: ClearPlotsMenu
        function ClearPlotsMenuSelected(app, event)
            app.PlotHelper.clearPlots(app);
        end

        % Button pushed function: ShowUniqueTargetsWindowButton
        function ShowUniqueTargetsWindowButtonPushed(app, event)
            app.UniqueTargetsHelper.showUniqueTargetsWindow(app);
        end

        % Button pushed function: ShowPlanRowsWindowButton
        function ShowPlanRowsWindowButtonPushed(app, event)
            app.PlanTargetsHelper.showPlanTargetsWindow(app);
        end

        % Button pushed function: ShowApprovedTargetsWindowButton
        function ShowApprovedTargetsWindowButtonPushed(app, event)
            app.ApprovedTargetsHelper.showApprovedTargetsWindow(app);
        end

        % Value changed function: EndTimeEditField
        function EndTimeEditFieldValueChanged(app, event)
            app.PlanParamsHelper.setPlanStartEndTime(app, app.StartTimeEditField.Value, app.EndTimeEditField.Value, app);
        end

        % Value changed function: StartTimeEditField
        function StartTimeEditFieldValueChanged2(app, event)
            app.PlanParamsHelper.setPlanStartEndTime(app, app.StartTimeEditField.Value, app.EndTimeEditField.Value, app);
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.showHelp('');
        end

        % Button pushed function: HelpUniqueTargetsWindowButton
        function HelpUniqueTargetsWindowButtonPushed(app, event)
            app.showHelp('unique_targets');
        end

        % Button pushed function: HelpUniqueTargetsWindowButton_2
        function HelpUniqueTargetsWindowButton_2Pushed(app, event)
            app.showHelp('build_plan');
        end

        % Button pushed function: HelpUniqueTargetsWindowButton_3
        function HelpUniqueTargetsWindowButton_3Pushed(app, event)
            app.showHelp('approved_targets');
        end

        % Button pushed function: HelpSkyMapPlotWindowButton
        function HelpSkyMapPlotWindowButtonPushed(app, event)
            app.showHelp('skymap_plot');
        end

        % Button down function: AxesGraphsPlot
        function AxesGraphsPlotButtonDown(app, event)
            app.showHelp('graphs_plot');
        end

        % Button pushed function: HelpPlanParamsButton
        function HelpPlanParamsButtonPushed(app, event)
           app.showHelp('plan_params'); 
        end

        % Button pushed function: HelpStatusInfoButton
        function HelpStatusInfoButtonPushed(app, event)
            app.showHelp('status_info');
        end

        % Button pushed function: HelpGraphsPlotWindowButton
        function HelpGraphsPlotWindowButtonPushed(app, event)
            app.showHelp('graphs_plot');
        end

        % Menu selected function: ErrorLogWindowMenu
        function ErrorLogWindowMenuSelected(app, event)
            app.SessionHelper.showErrorLogger(app);
        end

        % Menu selected function: LogsHelpMenu
        function LogsHelpMenuSelected(app, event)
            app.showHelp('logger');
        end

        % Value changed function: GraphPlotUniqueTargetDropDown
        function GraphPlotUniqueTargetDropDownValueChanged(app, event)
            app.UniqueTargetsHelper.uniqueTargetDropDownValueChanged(app);            
        end

        % Button pushed function: RefreshApprovedTargetsButton_4
        function RefreshApprovedTargetsButton_4Pushed(app, event)
            app.SessionHelper.showLogger(app);
        end

        % Button pushed function: UniqueTargetsSetOrderButton
        function UniqueTargetsSetOrderButtonPushed(app, event)
            app.UniqueTargetsHelper.setOrderColumnByGridSort(app);
        end

        % Button pushed function: UniqueTargetsClearOrderButton
        function UniqueTargetsClearOrderButtonPushed(app, event)
            app.UniqueTargetsHelper.clearOrderColumn(app);
        end

        % Button pushed function: OpenRefImagesButton
        function OpenRefImagesButtonPushed(app, event)
            app.TablesHelper.showRefImagesTable(app);
        end

        % Button pushed function: OpenExtSurveysButton
        function OpenExtSurveysButtonPushed(app, event)
            app.TablesHelper.showExtSurveysTable(app);
        end

        % Button pushed function: OpenFieldObjButton
        function OpenFieldObjButtonPushed(app, event)
            app.TablesHelper.showFieldObjTable(app);
        end

        % Value changed function: VisibilityPlotTimeUnitsDropDown
        function VisibilityPlotTimeUnitsDropDownValueChanged(app, event)
            app.PlotHelper.plotVisibility(app);
        end

        % Display data changed function: UITableUniqueTargets
        function UITableUniqueTargetsDisplayDataChanged(app, event)
         
        end

        % Value changed function: PlotFlagVisibilitySunCheckBox
        function PlotFlagVisibilitySunCheckBoxValueChanged(app, event)
            app.PlotHelper.plotVisibility(app);
        end

        % Value changed function: PlotFlagVisibilityEarthCheckBox
        function PlotFlagVisibilityEarthCheckBoxValueChanged(app, event)
            app.PlotHelper.plotVisibility(app);
        end

        % Value changed function: PlotFlagVisibilityMoonCheckBox
        function PlotFlagVisibilityMoonCheckBoxValueChanged(app, event)
            app.PlotHelper.plotVisibility(app);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1522 853];
            app.UIFigure.Name = 'MATLAB App';

            % Create FileMenu
            app.FileMenu = uimenu(app.UIFigure);
            app.FileMenu.Text = 'File';

            % Create NewMenu
            app.NewMenu = uimenu(app.FileMenu);
            app.NewMenu.MenuSelectedFcn = createCallbackFcn(app, @NewMenuSelected3, true);
            app.NewMenu.Text = 'New';

            % Create OpenMenu
            app.OpenMenu = uimenu(app.FileMenu);
            app.OpenMenu.MenuSelectedFcn = createCallbackFcn(app, @OpenMenuSelected3, true);
            app.OpenMenu.Text = 'Open';

            % Create SaveMenu
            app.SaveMenu = uimenu(app.FileMenu);
            app.SaveMenu.MenuSelectedFcn = createCallbackFcn(app, @SaveMenuSelected3, true);
            app.SaveMenu.Text = 'Save';

            % Create DuplicateMenu
            app.DuplicateMenu = uimenu(app.FileMenu);
            app.DuplicateMenu.MenuSelectedFcn = createCallbackFcn(app, @DuplicateMenuSelected, true);
            app.DuplicateMenu.Text = 'Duplicate';

            % Create CloseMenu_2
            app.CloseMenu_2 = uimenu(app.FileMenu);
            app.CloseMenu_2.MenuSelectedFcn = createCallbackFcn(app, @CloseMenu_2Selected2, true);
            app.CloseMenu_2.Text = 'Close';

            % Create DeleteMenu
            app.DeleteMenu = uimenu(app.FileMenu);
            app.DeleteMenu.MenuSelectedFcn = createCallbackFcn(app, @DeleteMenuSelected2, true);
            app.DeleteMenu.Text = 'Delete';

            % Create SaveToLocalFileMenu
            app.SaveToLocalFileMenu = uimenu(app.FileMenu);
            app.SaveToLocalFileMenu.MenuSelectedFcn = createCallbackFcn(app, @SaveToLocalFileMenuSelected, true);
            app.SaveToLocalFileMenu.Text = 'Save To Local File';

            % Create OpenFromLocalFileMenu
            app.OpenFromLocalFileMenu = uimenu(app.FileMenu);
            app.OpenFromLocalFileMenu.MenuSelectedFcn = createCallbackFcn(app, @OpenFromLocalFileMenuSelected, true);
            app.OpenFromLocalFileMenu.Text = 'Open From Local File';

            % Create Menu_2
            app.Menu_2 = uimenu(app.FileMenu);
            app.Menu_2.Text = '______________________';

            % Create ConnectLoginMenu
            app.ConnectLoginMenu = uimenu(app.FileMenu);
            app.ConnectLoginMenu.MenuSelectedFcn = createCallbackFcn(app, @ConnectLoginMenuSelected, true);
            app.ConnectLoginMenu.Text = 'Connect && Login';

            % Create DisconnectLogoutMenu
            app.DisconnectLogoutMenu = uimenu(app.FileMenu);
            app.DisconnectLogoutMenu.MenuSelectedFcn = createCallbackFcn(app, @DisconnectLogoutMenuSelected, true);
            app.DisconnectLogoutMenu.Text = 'Disconnect (Logout)';

            % Create ExitPlannerMenu
            app.ExitPlannerMenu = uimenu(app.FileMenu);
            app.ExitPlannerMenu.MenuSelectedFcn = createCallbackFcn(app, @ExitPlannerMenuSelected2, true);
            app.ExitPlannerMenu.Text = 'Exit Planner';

            % Create TargetsMenu
            app.TargetsMenu = uimenu(app.UIFigure);
            app.TargetsMenu.Text = 'Targets';

            % Create AddUniqueTargetMenu
            app.AddUniqueTargetMenu = uimenu(app.TargetsMenu);
            app.AddUniqueTargetMenu.MenuSelectedFcn = createCallbackFcn(app, @AddUniqueTargetMenuSelected, true);
            app.AddUniqueTargetMenu.Text = 'Add Unique Target';

            % Create EditUniqueTargetMenu
            app.EditUniqueTargetMenu = uimenu(app.TargetsMenu);
            app.EditUniqueTargetMenu.MenuSelectedFcn = createCallbackFcn(app, @EditUniqueTargetMenuSelected, true);
            app.EditUniqueTargetMenu.Text = 'Edit Unique Target';

            % Create DeleteUniqueTargetMenu
            app.DeleteUniqueTargetMenu = uimenu(app.TargetsMenu);
            app.DeleteUniqueTargetMenu.MenuSelectedFcn = createCallbackFcn(app, @DeleteUniqueTargetMenuSelected, true);
            app.DeleteUniqueTargetMenu.Text = 'Delete Unique Target';

            % Create ClearAllUniqueTargetsMenu
            app.ClearAllUniqueTargetsMenu = uimenu(app.TargetsMenu);
            app.ClearAllUniqueTargetsMenu.MenuSelectedFcn = createCallbackFcn(app, @ClearAllUniqueTargetsMenuSelected, true);
            app.ClearAllUniqueTargetsMenu.Text = 'Clear All Unique Targets';

            % Create ViewUniqueTargetsTableMenu
            app.ViewUniqueTargetsTableMenu = uimenu(app.TargetsMenu);
            app.ViewUniqueTargetsTableMenu.MenuSelectedFcn = createCallbackFcn(app, @ViewUniqueTargetsTableMenuSelected, true);
            app.ViewUniqueTargetsTableMenu.Text = 'View Unique Targets Table';

            % Create SaveUniqueTargetsToFileMenu
            app.SaveUniqueTargetsToFileMenu = uimenu(app.TargetsMenu);
            app.SaveUniqueTargetsToFileMenu.MenuSelectedFcn = createCallbackFcn(app, @SaveUniqueTargetsToFileMenuSelected, true);
            app.SaveUniqueTargetsToFileMenu.Text = 'Save Unique Targets To File';

            % Create LoadUniqueTargetsFromFileMenu
            app.LoadUniqueTargetsFromFileMenu = uimenu(app.TargetsMenu);
            app.LoadUniqueTargetsFromFileMenu.MenuSelectedFcn = createCallbackFcn(app, @LoadUniqueTargetsFromFileMenuSelected, true);
            app.LoadUniqueTargetsFromFileMenu.Text = 'Load Unique Targets From File';

            % Create PlanMenu_2
            app.PlanMenu_2 = uimenu(app.UIFigure);
            app.PlanMenu_2.Text = 'Plan';

            % Create EditPlanTargetMenu
            app.EditPlanTargetMenu = uimenu(app.PlanMenu_2);
            app.EditPlanTargetMenu.MenuSelectedFcn = createCallbackFcn(app, @EditPlanTargetMenuSelected, true);
            app.EditPlanTargetMenu.Text = 'Edit Plan Target';

            % Create DeletePlanTargetMenu
            app.DeletePlanTargetMenu = uimenu(app.PlanMenu_2);
            app.DeletePlanTargetMenu.MenuSelectedFcn = createCallbackFcn(app, @DeletePlanTargetMenuSelected, true);
            app.DeletePlanTargetMenu.Text = 'Delete Plan Target';

            % Create ClearAllPlanTargetsMenu
            app.ClearAllPlanTargetsMenu = uimenu(app.PlanMenu_2);
            app.ClearAllPlanTargetsMenu.MenuSelectedFcn = createCallbackFcn(app, @ClearAllPlanTargetsMenuSelected, true);
            app.ClearAllPlanTargetsMenu.Text = 'Clear All Plan Targets';

            % Create ViewPlanTableMenu
            app.ViewPlanTableMenu = uimenu(app.PlanMenu_2);
            app.ViewPlanTableMenu.MenuSelectedFcn = createCallbackFcn(app, @ViewPlanTableMenuSelected, true);
            app.ViewPlanTableMenu.Text = 'View Plan Table';

            % Create Menu_3
            app.Menu_3 = uimenu(app.PlanMenu_2);
            app.Menu_3.Text = '___________________';

            % Create ParamsMenu
            app.ParamsMenu = uimenu(app.PlanMenu_2);
            app.ParamsMenu.Text = 'Parameters';

            % Create PlanHistoryMenu
            app.PlanHistoryMenu = uimenu(app.PlanMenu_2);
            app.PlanHistoryMenu.MenuSelectedFcn = createCallbackFcn(app, @PlanHistoryMenuSelected, true);
            app.PlanHistoryMenu.Text = 'History';

            % Create Menu
            app.Menu = uimenu(app.PlanMenu_2);
            app.Menu.Text = '___________________';

            % Create BuildMenu
            app.BuildMenu = uimenu(app.PlanMenu_2);
            app.BuildMenu.Text = 'Build';

            % Create ValidateMenu
            app.ValidateMenu = uimenu(app.PlanMenu_2);
            app.ValidateMenu.Text = 'Validate';

            % Create SubmitMenu
            app.SubmitMenu = uimenu(app.PlanMenu_2);
            app.SubmitMenu.Text = 'Submit';

            % Create PlotsMenu
            app.PlotsMenu = uimenu(app.UIFigure);
            app.PlotsMenu.Text = 'Plots';

            % Create ViewSkyMapPlotWindowMenu
            app.ViewSkyMapPlotWindowMenu = uimenu(app.PlotsMenu);
            app.ViewSkyMapPlotWindowMenu.MenuSelectedFcn = createCallbackFcn(app, @ViewSkyMapPlotWindowMenuSelected, true);
            app.ViewSkyMapPlotWindowMenu.Text = 'View Sky Map Plot Window';

            % Create ViewGraphsPlotWindowMenu
            app.ViewGraphsPlotWindowMenu = uimenu(app.PlotsMenu);
            app.ViewGraphsPlotWindowMenu.MenuSelectedFcn = createCallbackFcn(app, @ViewGraphsPlotWindowMenuSelected, true);
            app.ViewGraphsPlotWindowMenu.Text = 'View Graphs Plot Window';

            % Create ClearPlotsMenu
            app.ClearPlotsMenu = uimenu(app.PlotsMenu);
            app.ClearPlotsMenu.MenuSelectedFcn = createCallbackFcn(app, @ClearPlotsMenuSelected, true);
            app.ClearPlotsMenu.Text = 'Clear Plots';

            % Create ViewMenu
            app.ViewMenu = uimenu(app.UIFigure);
            app.ViewMenu.Text = 'View';

            % Create RefreshMenu
            app.RefreshMenu = uimenu(app.ViewMenu);
            app.RefreshMenu.MenuSelectedFcn = createCallbackFcn(app, @RefreshMenuSelected, true);
            app.RefreshMenu.Text = 'Refresh';

            % Create LogWindowMenu
            app.LogWindowMenu = uimenu(app.ViewMenu);
            app.LogWindowMenu.MenuSelectedFcn = createCallbackFcn(app, @LogWindowMenuSelected, true);
            app.LogWindowMenu.Text = 'Log Window';

            % Create ErrorLogWindowMenu
            app.ErrorLogWindowMenu = uimenu(app.ViewMenu);
            app.ErrorLogWindowMenu.MenuSelectedFcn = createCallbackFcn(app, @ErrorLogWindowMenuSelected, true);
            app.ErrorLogWindowMenu.Text = 'Error Log Window';

            % Create ToolsMenu
            app.ToolsMenu = uimenu(app.UIFigure);
            app.ToolsMenu.Text = 'Tools';

            % Create SNRCalculatorMenu
            app.SNRCalculatorMenu = uimenu(app.ToolsMenu);
            app.SNRCalculatorMenu.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.SNRCalculatorMenu.Text = 'SNR Calculator';

            % Create HelpMenu
            app.HelpMenu = uimenu(app.UIFigure);
            app.HelpMenu.Text = 'Help';

            % Create ObservationPlannerHelpMenu
            app.ObservationPlannerHelpMenu = uimenu(app.HelpMenu);
            app.ObservationPlannerHelpMenu.MenuSelectedFcn = createCallbackFcn(app, @ObservationPlannerHelpMenuSelected, true);
            app.ObservationPlannerHelpMenu.Text = 'Observation Planner Help';

            % Create LogsHelpMenu
            app.LogsHelpMenu = uimenu(app.HelpMenu);
            app.LogsHelpMenu.MenuSelectedFcn = createCallbackFcn(app, @LogsHelpMenuSelected, true);
            app.LogsHelpMenu.Text = 'Logs Help';

            % Create AboutMenu
            app.AboutMenu = uimenu(app.HelpMenu);
            app.AboutMenu.MenuSelectedFcn = createCallbackFcn(app, @AboutMenuSelected, true);
            app.AboutMenu.Text = 'About';

            % Create PanelToolbar
            app.PanelToolbar = uipanel(app.UIFigure);
            app.PanelToolbar.BorderType = 'none';
            app.PanelToolbar.BackgroundColor = [0.8 0.8 0.8];
            app.PanelToolbar.Position = [2 767 1513 42];

            % Create NewButton
            app.NewButton = uibutton(app.PanelToolbar, 'push');
            app.NewButton.ButtonPushedFcn = createCallbackFcn(app, @NewButtonPushed, true);
            app.NewButton.Tooltip = {'Create new observation plan'};
            app.NewButton.Position = [8 8 85 30];
            app.NewButton.Text = 'New';

            % Create OpenButton
            app.OpenButton = uibutton(app.PanelToolbar, 'push');
            app.OpenButton.ButtonPushedFcn = createCallbackFcn(app, @OpenButtonPushed, true);
            app.OpenButton.Tooltip = {'Open existing observation plan from database'};
            app.OpenButton.Position = [102 8 85 30];
            app.OpenButton.Text = 'Open';

            % Create SaveButton
            app.SaveButton = uibutton(app.PanelToolbar, 'push');
            app.SaveButton.ButtonPushedFcn = createCallbackFcn(app, @SaveButtonPushed, true);
            app.SaveButton.Tooltip = {'Save observation plan to database'};
            app.SaveButton.Position = [196 8 85 30];
            app.SaveButton.Text = 'Save';

            % Create SubmitButton
            app.SubmitButton = uibutton(app.PanelToolbar, 'push');
            app.SubmitButton.ButtonPushedFcn = createCallbackFcn(app, @SubmitButtonPushed, true);
            app.SubmitButton.BackgroundColor = [0.651 0.9686 0.651];
            app.SubmitButton.FontWeight = 'bold';
            app.SubmitButton.Tooltip = {'Send plan to Mission Control for approval'};
            app.SubmitButton.Position = [651 8 85 30];
            app.SubmitButton.Text = 'Submit';

            % Create ValidateButton
            app.ValidateButton = uibutton(app.PanelToolbar, 'push');
            app.ValidateButton.ButtonPushedFcn = createCallbackFcn(app, @ValidateButtonPushed, true);
            app.ValidateButton.FontWeight = 'bold';
            app.ValidateButton.FontColor = [0 0 1];
            app.ValidateButton.Tooltip = {'Send plan for validation (may take up to 30 seconds)'};
            app.ValidateButton.Position = [550 8 85 30];
            app.ValidateButton.Text = 'Validate';

            % Create ParamsButton
            app.ParamsButton = uibutton(app.PanelToolbar, 'push');
            app.ParamsButton.ButtonPushedFcn = createCallbackFcn(app, @ParamsButtonPushed, true);
            app.ParamsButton.Tooltip = {'Show (and edit) observation plan parameters'};
            app.ParamsButton.Position = [430 8 85 30];
            app.ParamsButton.Text = 'Params';

            % Create LoginButton
            app.LoginButton = uibutton(app.PanelToolbar, 'push');
            app.LoginButton.ButtonPushedFcn = createCallbackFcn(app, @LoginButtonPushed, true);
            app.LoginButton.BackgroundColor = [1 1 0.549];
            app.LoginButton.FontWeight = 'bold';
            app.LoginButton.Tooltip = {'Connect to server and login'};
            app.LoginButton.Position = [847 7 88 30];
            app.LoginButton.Text = 'Login';

            % Create SNRCalcButton
            app.SNRCalcButton = uibutton(app.PanelToolbar, 'push');
            app.SNRCalcButton.ButtonPushedFcn = createCallbackFcn(app, @SNRCalcButtonPushed, true);
            app.SNRCalcButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.SNRCalcButton.Position = [1005 8 88 30];
            app.SNRCalcButton.Text = 'SNR Calc';

            % Create ModifiedLabel
            app.ModifiedLabel = uilabel(app.PanelToolbar);
            app.ModifiedLabel.Tooltip = {'Is current plan modified sience last saved?'};
            app.ModifiedLabel.Position = [946 10 50 22];
            app.ModifiedLabel.Text = 'Modified';

            % Create DuplicateButton
            app.DuplicateButton = uibutton(app.PanelToolbar, 'push');
            app.DuplicateButton.ButtonPushedFcn = createCallbackFcn(app, @DuplicateButtonPushed, true);
            app.DuplicateButton.Tooltip = {'Duplicate current observation plan'};
            app.DuplicateButton.Position = [289 8 85 29];
            app.DuplicateButton.Text = 'Duplicate';

            % Create RetractButton
            app.RetractButton = uibutton(app.PanelToolbar, 'push');
            app.RetractButton.FontWeight = 'bold';
            app.RetractButton.FontColor = [1 0 0];
            app.RetractButton.Enable = 'off';
            app.RetractButton.Visible = 'off';
            app.RetractButton.Tooltip = {'Send plan to Mission Control for approval'};
            app.RetractButton.Position = [751 8 85 30];
            app.RetractButton.Text = 'Retract !!!';

            % Create QAButton
            app.QAButton = uibutton(app.PanelToolbar, 'push');
            app.QAButton.ButtonPushedFcn = createCallbackFcn(app, @QAButtonPushed, true);
            app.QAButton.BackgroundColor = [0.0588 1 1];
            app.QAButton.FontSize = 16;
            app.QAButton.FontWeight = 'bold';
            app.QAButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.QAButton.Position = [1356 8 73 29];
            app.QAButton.Text = 'QA ';

            % Create HelpButton
            app.HelpButton = uibutton(app.PanelToolbar, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.HelpButton.Position = [1439 8 64 30];
            app.HelpButton.Text = 'Help';

            % Create UniqueTargetsPanel
            app.UniqueTargetsPanel = uipanel(app.UIFigure);
            app.UniqueTargetsPanel.TitlePosition = 'centertop';
            app.UniqueTargetsPanel.Title = 'Unique Targets';
            app.UniqueTargetsPanel.BackgroundColor = [0.8 0.749 0.851];
            app.UniqueTargetsPanel.Position = [13 446 993 186];

            % Create UITableUniqueTargets
            app.UITableUniqueTargets = uitable(app.UniqueTargetsPanel);
            app.UITableUniqueTargets.ColumnName = '';
            app.UITableUniqueTargets.RowName = {};
            app.UITableUniqueTargets.ColumnEditable = true;
            app.UITableUniqueTargets.DoubleClickedFcn = createCallbackFcn(app, @UITableUniqueTargetsDoubleClicked, true);
            app.UITableUniqueTargets.ClickedFcn = createCallbackFcn(app, @UITableUniqueTargetsClicked, true);
            app.UITableUniqueTargets.DisplayDataChangedFcn = createCallbackFcn(app, @UITableUniqueTargetsDisplayDataChanged, true);
            app.UITableUniqueTargets.SelectionChangedFcn = createCallbackFcn(app, @UITableUniqueTargetsSelectionChanged, true);
            app.UITableUniqueTargets.FontSize = 10;
            app.UITableUniqueTargets.Position = [6 8 905 152];

            % Create Panel_6
            app.Panel_6 = uipanel(app.UniqueTargetsPanel);
            app.Panel_6.Position = [916 8 72 149];

            % Create AddUniqueTargetButton
            app.AddUniqueTargetButton = uibutton(app.Panel_6, 'push');
            app.AddUniqueTargetButton.ButtonPushedFcn = createCallbackFcn(app, @AddUniqueTargetButtonPushed, true);
            app.AddUniqueTargetButton.FontWeight = 'bold';
            app.AddUniqueTargetButton.FontColor = [0 0 1];
            app.AddUniqueTargetButton.Tooltip = {'Add new unique target'};
            app.AddUniqueTargetButton.Position = [6 116 60 23];
            app.AddUniqueTargetButton.Text = 'Add';

            % Create EditUniqueTargetButton
            app.EditUniqueTargetButton = uibutton(app.Panel_6, 'push');
            app.EditUniqueTargetButton.ButtonPushedFcn = createCallbackFcn(app, @EditUniqueTargetButtonPushed, true);
            app.EditUniqueTargetButton.Tooltip = {'Edit the selected unique target'};
            app.EditUniqueTargetButton.Position = [6 65 60 22];
            app.EditUniqueTargetButton.Text = 'Edit';

            % Create LoadUniqueTargetsButton
            app.LoadUniqueTargetsButton = uibutton(app.Panel_6, 'push');
            app.LoadUniqueTargetsButton.ButtonPushedFcn = createCallbackFcn(app, @LoadUniqueTargetsButtonPushed, true);
            app.LoadUniqueTargetsButton.Tooltip = {'Load unique targets from text file'};
            app.LoadUniqueTargetsButton.Position = [6 91 60 22];
            app.LoadUniqueTargetsButton.Text = 'Load';

            % Create EditPlanTargetButton_4
            app.EditPlanTargetButton_4 = uibutton(app.Panel_6, 'push');
            app.EditPlanTargetButton_4.ButtonPushedFcn = createCallbackFcn(app, @EditPlanTargetButton_4Pushed, true);
            app.EditPlanTargetButton_4.Tooltip = {'Edit the selected target'};
            app.EditPlanTargetButton_4.Position = [6 36 60 23];
            app.EditPlanTargetButton_4.Text = 'Delete';

            % Create EditPlanTargetButton_5
            app.EditPlanTargetButton_5 = uibutton(app.Panel_6, 'push');
            app.EditPlanTargetButton_5.ButtonPushedFcn = createCallbackFcn(app, @EditPlanTargetButton_5Pushed, true);
            app.EditPlanTargetButton_5.Tooltip = {'Edit the selected target'};
            app.EditPlanTargetButton_5.Position = [6 8 60 23];
            app.EditPlanTargetButton_5.Text = 'Clear All';

            % Create ShowUniqueTargetsWindowButton
            app.ShowUniqueTargetsWindowButton = uibutton(app.UniqueTargetsPanel, 'push');
            app.ShowUniqueTargetsWindowButton.ButtonPushedFcn = createCallbackFcn(app, @ShowUniqueTargetsWindowButtonPushed, true);
            app.ShowUniqueTargetsWindowButton.FontSize = 9;
            app.ShowUniqueTargetsWindowButton.FontWeight = 'bold';
            app.ShowUniqueTargetsWindowButton.Position = [953 167 34 17];
            app.ShowUniqueTargetsWindowButton.Text = '...';

            % Create HelpUniqueTargetsWindowButton
            app.HelpUniqueTargetsWindowButton = uibutton(app.UniqueTargetsPanel, 'push');
            app.HelpUniqueTargetsWindowButton.ButtonPushedFcn = createCallbackFcn(app, @HelpUniqueTargetsWindowButtonPushed, true);
            app.HelpUniqueTargetsWindowButton.FontSize = 9;
            app.HelpUniqueTargetsWindowButton.FontWeight = 'bold';
            app.HelpUniqueTargetsWindowButton.Position = [915 167 34 17];
            app.HelpUniqueTargetsWindowButton.Text = '?';

            % Create UniqueTargetsSetOrderButton
            app.UniqueTargetsSetOrderButton = uibutton(app.UniqueTargetsPanel, 'push');
            app.UniqueTargetsSetOrderButton.ButtonPushedFcn = createCallbackFcn(app, @UniqueTargetsSetOrderButtonPushed, true);
            app.UniqueTargetsSetOrderButton.FontSize = 9;
            app.UniqueTargetsSetOrderButton.FontWeight = 'bold';
            app.UniqueTargetsSetOrderButton.Position = [13 167 66 17];
            app.UniqueTargetsSetOrderButton.Text = 'Set Order';

            % Create UniqueTargetsClearOrderButton
            app.UniqueTargetsClearOrderButton = uibutton(app.UniqueTargetsPanel, 'push');
            app.UniqueTargetsClearOrderButton.ButtonPushedFcn = createCallbackFcn(app, @UniqueTargetsClearOrderButtonPushed, true);
            app.UniqueTargetsClearOrderButton.FontSize = 9;
            app.UniqueTargetsClearOrderButton.FontWeight = 'bold';
            app.UniqueTargetsClearOrderButton.Position = [90 167 66 17];
            app.UniqueTargetsClearOrderButton.Text = 'Clear Order';

            % Create PlanPanel
            app.PlanPanel = uipanel(app.UIFigure);
            app.PlanPanel.TitlePosition = 'centertop';
            app.PlanPanel.Title = 'Plan';
            app.PlanPanel.BackgroundColor = [0.749 0.851 0.949];
            app.PlanPanel.Position = [13 239 993 202];

            % Create UITablePlanTargets
            app.UITablePlanTargets = uitable(app.PlanPanel);
            app.UITablePlanTargets.ColumnName = '';
            app.UITablePlanTargets.RowName = {};
            app.UITablePlanTargets.DoubleClickedFcn = createCallbackFcn(app, @UITablePlanTargetsDoubleClicked, true);
            app.UITablePlanTargets.ClickedFcn = createCallbackFcn(app, @UITablePlanTargetsClicked, true);
            app.UITablePlanTargets.SelectionChangedFcn = createCallbackFcn(app, @UITablePlanTargetsSelectionChanged, true);
            app.UITablePlanTargets.FontSize = 10;
            app.UITablePlanTargets.Position = [6 18 905 158];

            % Create Panel_4
            app.Panel_4 = uipanel(app.PlanPanel);
            app.Panel_4.Position = [915 5 72 172];

            % Create BuildButton
            app.BuildButton = uibutton(app.Panel_4, 'push');
            app.BuildButton.ButtonPushedFcn = createCallbackFcn(app, @BuildButtonPushed, true);
            app.BuildButton.FontWeight = 'bold';
            app.BuildButton.FontColor = [0 0 1];
            app.BuildButton.Tooltip = {'Build plan (depends on plan type)'};
            app.BuildButton.Position = [6 145 60 22];
            app.BuildButton.Text = 'Build';

            % Create CheckPlanTargetsButton
            app.CheckPlanTargetsButton = uibutton(app.Panel_4, 'push');
            app.CheckPlanTargetsButton.ButtonPushedFcn = createCallbackFcn(app, @CheckPlanTargetsButtonPushed, true);
            app.CheckPlanTargetsButton.FontSize = 11;
            app.CheckPlanTargetsButton.FontWeight = 'bold';
            app.CheckPlanTargetsButton.Tooltip = {'Perform plan consistency check'};
            app.CheckPlanTargetsButton.Position = [6 35 60 22];
            app.CheckPlanTargetsButton.Text = 'Check';

            % Create EditPlanTargetButton
            app.EditPlanTargetButton = uibutton(app.Panel_4, 'push');
            app.EditPlanTargetButton.ButtonPushedFcn = createCallbackFcn(app, @EditPlanTargetButtonPushed, true);
            app.EditPlanTargetButton.Tooltip = {'Edit the selected target'};
            app.EditPlanTargetButton.Position = [6 118 60 22];
            app.EditPlanTargetButton.Text = 'Edit';

            % Create EditPlanTargetButton_2
            app.EditPlanTargetButton_2 = uibutton(app.Panel_4, 'push');
            app.EditPlanTargetButton_2.ButtonPushedFcn = createCallbackFcn(app, @EditPlanTargetButton_2Pushed, true);
            app.EditPlanTargetButton_2.Tooltip = {'Edit the selected target'};
            app.EditPlanTargetButton_2.Position = [6 90 60 23];
            app.EditPlanTargetButton_2.Text = 'Delete';

            % Create EditPlanTargetButton_3
            app.EditPlanTargetButton_3 = uibutton(app.Panel_4, 'push');
            app.EditPlanTargetButton_3.ButtonPushedFcn = createCallbackFcn(app, @EditPlanTargetButton_3Pushed, true);
            app.EditPlanTargetButton_3.Tooltip = {'Edit the selected target'};
            app.EditPlanTargetButton_3.Position = [6 62 60 23];
            app.EditPlanTargetButton_3.Text = 'Clear All';

            % Create EditPlanTargetButton_6
            app.EditPlanTargetButton_6 = uibutton(app.Panel_4, 'push');
            app.EditPlanTargetButton_6.ButtonPushedFcn = createCallbackFcn(app, @EditPlanTargetButton_6Pushed, true);
            app.EditPlanTargetButton_6.Tooltip = {'Edit the selected target'};
            app.EditPlanTargetButton_6.Position = [6 7 60 23];
            app.EditPlanTargetButton_6.Text = 'Group';

            % Create ShowPlanRowsWindowButton
            app.ShowPlanRowsWindowButton = uibutton(app.PlanPanel, 'push');
            app.ShowPlanRowsWindowButton.ButtonPushedFcn = createCallbackFcn(app, @ShowPlanRowsWindowButtonPushed, true);
            app.ShowPlanRowsWindowButton.FontSize = 9;
            app.ShowPlanRowsWindowButton.FontWeight = 'bold';
            app.ShowPlanRowsWindowButton.Position = [954 183 34 17];
            app.ShowPlanRowsWindowButton.Text = '...';

            % Create HelpUniqueTargetsWindowButton_2
            app.HelpUniqueTargetsWindowButton_2 = uibutton(app.PlanPanel, 'push');
            app.HelpUniqueTargetsWindowButton_2.ButtonPushedFcn = createCallbackFcn(app, @HelpUniqueTargetsWindowButton_2Pushed, true);
            app.HelpUniqueTargetsWindowButton_2.FontSize = 9;
            app.HelpUniqueTargetsWindowButton_2.FontWeight = 'bold';
            app.HelpUniqueTargetsWindowButton_2.Position = [917 183 34 17];
            app.HelpUniqueTargetsWindowButton_2.Text = '?';

            % Create ApprovedTargetsPanel
            app.ApprovedTargetsPanel = uipanel(app.UIFigure);
            app.ApprovedTargetsPanel.BorderColor = [0.4902 0.4902 0.4902];
            app.ApprovedTargetsPanel.TitlePosition = 'centertop';
            app.ApprovedTargetsPanel.Title = 'Approved Targets';
            app.ApprovedTargetsPanel.BackgroundColor = [0.851 0.9216 0.851];
            app.ApprovedTargetsPanel.Position = [12 83 994 150];

            % Create UITableApprovedTargets
            app.UITableApprovedTargets = uitable(app.ApprovedTargetsPanel);
            app.UITableApprovedTargets.ColumnName = '';
            app.UITableApprovedTargets.RowName = {};
            app.UITableApprovedTargets.SelectionChangedFcn = createCallbackFcn(app, @UITableApprovedTargetsSelectionChanged, true);
            app.UITableApprovedTargets.FontSize = 10;
            app.UITableApprovedTargets.Position = [8 6 904 115];

            % Create Panel_5
            app.Panel_5 = uipanel(app.ApprovedTargetsPanel);
            app.Panel_5.Position = [917 8 72 111];

            % Create RefreshApprovedTargetsButton
            app.RefreshApprovedTargetsButton = uibutton(app.Panel_5, 'push');
            app.RefreshApprovedTargetsButton.ButtonPushedFcn = createCallbackFcn(app, @RefreshApprovedTargetsButtonPushed, true);
            app.RefreshApprovedTargetsButton.FontSize = 11;
            app.RefreshApprovedTargetsButton.Tooltip = {'Refeatch data from uplanner object'};
            app.RefreshApprovedTargetsButton.Position = [7 76 60 22];
            app.RefreshApprovedTargetsButton.Text = 'Retreive';

            % Create RefreshApprovedTargetsButton_3
            app.RefreshApprovedTargetsButton_3 = uibutton(app.Panel_5, 'push');
            app.RefreshApprovedTargetsButton_3.ButtonPushedFcn = createCallbackFcn(app, @RefreshApprovedTargetsButton_3Pushed, true);
            app.RefreshApprovedTargetsButton_3.FontSize = 11;
            app.RefreshApprovedTargetsButton_3.Tooltip = {'Refeatch data from uplanner object'};
            app.RefreshApprovedTargetsButton_3.Position = [7 48 60 22];
            app.RefreshApprovedTargetsButton_3.Text = 'Clear';

            % Create ShowApprovedTargetsWindowButton
            app.ShowApprovedTargetsWindowButton = uibutton(app.ApprovedTargetsPanel, 'push');
            app.ShowApprovedTargetsWindowButton.ButtonPushedFcn = createCallbackFcn(app, @ShowApprovedTargetsWindowButtonPushed, true);
            app.ShowApprovedTargetsWindowButton.FontSize = 9;
            app.ShowApprovedTargetsWindowButton.FontWeight = 'bold';
            app.ShowApprovedTargetsWindowButton.Position = [954 131 34 17];
            app.ShowApprovedTargetsWindowButton.Text = '...';

            % Create HelpUniqueTargetsWindowButton_3
            app.HelpUniqueTargetsWindowButton_3 = uibutton(app.ApprovedTargetsPanel, 'push');
            app.HelpUniqueTargetsWindowButton_3.ButtonPushedFcn = createCallbackFcn(app, @HelpUniqueTargetsWindowButton_3Pushed, true);
            app.HelpUniqueTargetsWindowButton_3.FontSize = 9;
            app.HelpUniqueTargetsWindowButton_3.FontWeight = 'bold';
            app.HelpUniqueTargetsWindowButton_3.Position = [914 131 34 17];
            app.HelpUniqueTargetsWindowButton_3.Text = '?';

            % Create PlotSkyMapCurrentlyshowsgeneralskymapPanel
            app.PlotSkyMapCurrentlyshowsgeneralskymapPanel = uipanel(app.UIFigure);
            app.PlotSkyMapCurrentlyshowsgeneralskymapPanel.TitlePosition = 'centertop';
            app.PlotSkyMapCurrentlyshowsgeneralskymapPanel.Title = 'Plot - Sky Map (Currently shows general skymap)';
            app.PlotSkyMapCurrentlyshowsgeneralskymapPanel.BackgroundColor = [0.8 0.8 0.8];
            app.PlotSkyMapCurrentlyshowsgeneralskymapPanel.Position = [1020 403 497 358];

            % Create AxesSkymapPlot
            app.AxesSkymapPlot = uiaxes(app.PlotSkyMapCurrentlyshowsgeneralskymapPanel);
            title(app.AxesSkymapPlot, 'Title')
            xlabel(app.AxesSkymapPlot, 'X')
            ylabel(app.AxesSkymapPlot, 'Y')
            zlabel(app.AxesSkymapPlot, 'Z')
            app.AxesSkymapPlot.FontName = 'Helvetica';
            app.AxesSkymapPlot.XTick = [0 0.2 0.4 0.6 0.8 1];
            app.AxesSkymapPlot.YTick = [0 0.2 0.4 0.6 0.8 1];
            app.AxesSkymapPlot.Position = [7 14 386 316];

            % Create Panel_10
            app.Panel_10 = uipanel(app.PlotSkyMapCurrentlyshowsgeneralskymapPanel);
            app.Panel_10.Position = [401 14 90 320];

            % Create PlotFlagUniqueCheckBox
            app.PlotFlagUniqueCheckBox = uicheckbox(app.Panel_10);
            app.PlotFlagUniqueCheckBox.Text = {'Unique'; 'Targets'};
            app.PlotFlagUniqueCheckBox.FontColor = [0 0 1];
            app.PlotFlagUniqueCheckBox.Position = [8 283 61 30];
            app.PlotFlagUniqueCheckBox.Value = true;

            % Create PlotFlagPlanCheckBox
            app.PlotFlagPlanCheckBox = uicheckbox(app.Panel_10);
            app.PlotFlagPlanCheckBox.Text = 'Plan';
            app.PlotFlagPlanCheckBox.FontColor = [0 0 1];
            app.PlotFlagPlanCheckBox.Position = [8 257 46 22];

            % Create PlotFlagExtinctionCheckBox
            app.PlotFlagExtinctionCheckBox = uicheckbox(app.Panel_10);
            app.PlotFlagExtinctionCheckBox.Text = 'Extinction';
            app.PlotFlagExtinctionCheckBox.Position = [8 228 74 22];
            app.PlotFlagExtinctionCheckBox.Value = true;

            % Create PlotFlagCalibrationCheckBox
            app.PlotFlagCalibrationCheckBox = uicheckbox(app.Panel_10);
            app.PlotFlagCalibrationCheckBox.Text = {'Calibration'; 'Stars'};
            app.PlotFlagCalibrationCheckBox.Position = [8 195 79 30];
            app.PlotFlagCalibrationCheckBox.Value = true;

            % Create PlotFlagApprovedCheckBox
            app.PlotFlagApprovedCheckBox = uicheckbox(app.Panel_10);
            app.PlotFlagApprovedCheckBox.Text = {'Approved'; 'Targets'};
            app.PlotFlagApprovedCheckBox.Position = [8 159 73 30];

            % Create PlotFlagVisibleCheckBox
            app.PlotFlagVisibleCheckBox = uicheckbox(app.Panel_10);
            app.PlotFlagVisibleCheckBox.Enable = 'off';
            app.PlotFlagVisibleCheckBox.Text = 'Visibility TBD';
            app.PlotFlagVisibleCheckBox.Position = [8 132 92 22];

            % Create UpdateSkyMapButton
            app.UpdateSkyMapButton = uibutton(app.Panel_10, 'push');
            app.UpdateSkyMapButton.ButtonPushedFcn = createCallbackFcn(app, @UpdateSkyMapButtonPushed, true);
            app.UpdateSkyMapButton.FontWeight = 'bold';
            app.UpdateSkyMapButton.FontColor = [0 0 1];
            app.UpdateSkyMapButton.Position = [17 15 56 23];
            app.UpdateSkyMapButton.Text = 'Plot!';

            % Create cooSysLabel_4
            app.cooSysLabel_4 = uilabel(app.Panel_10);
            app.cooSysLabel_4.HorizontalAlignment = 'center';
            app.cooSysLabel_4.Position = [12 77 64 36];
            app.cooSysLabel_4.Text = {'Coordinate'; 'System'};

            % Create PlotCooSysDropDown
            app.PlotCooSysDropDown = uidropdown(app.Panel_10);
            app.PlotCooSysDropDown.Items = {'Equatorial', 'Ecliptic', 'Galactic'};
            app.PlotCooSysDropDown.FontSize = 11;
            app.PlotCooSysDropDown.Position = [5 53 77 22];
            app.PlotCooSysDropDown.Value = 'Equatorial';

            % Create OpenSkyMapPlotWindowButton
            app.OpenSkyMapPlotWindowButton = uibutton(app.PlotSkyMapCurrentlyshowsgeneralskymapPanel, 'push');
            app.OpenSkyMapPlotWindowButton.ButtonPushedFcn = createCallbackFcn(app, @OpenSkyMapPlotWindowButtonPushed, true);
            app.OpenSkyMapPlotWindowButton.FontWeight = 'bold';
            app.OpenSkyMapPlotWindowButton.Position = [461 339 34 17];
            app.OpenSkyMapPlotWindowButton.Text = '*';

            % Create HelpSkyMapPlotWindowButton
            app.HelpSkyMapPlotWindowButton = uibutton(app.PlotSkyMapCurrentlyshowsgeneralskymapPanel, 'push');
            app.HelpSkyMapPlotWindowButton.ButtonPushedFcn = createCallbackFcn(app, @HelpSkyMapPlotWindowButtonPushed, true);
            app.HelpSkyMapPlotWindowButton.FontSize = 9;
            app.HelpSkyMapPlotWindowButton.FontWeight = 'bold';
            app.HelpSkyMapPlotWindowButton.Position = [424 339 34 17];
            app.HelpSkyMapPlotWindowButton.Text = '?';

            % Create PlotGraphsDoubleClickUniqueTargetorPlanrowPanel
            app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel = uipanel(app.UIFigure);
            app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel.TitlePosition = 'centertop';
            app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel.Title = 'Plot - Graphs (Double Click Unique Target or Plan row)';
            app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel.BackgroundColor = [0.902 0.902 0.902];
            app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel.Position = [1020 11 497 386];

            % Create AxesGraphsPlot
            app.AxesGraphsPlot = uiaxes(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            title(app.AxesGraphsPlot, 'Title')
            xlabel(app.AxesGraphsPlot, 'X')
            ylabel(app.AxesGraphsPlot, 'Y')
            zlabel(app.AxesGraphsPlot, 'Z')
            app.AxesGraphsPlot.FontName = 'Helvetica';
            app.AxesGraphsPlot.ButtonDownFcn = createCallbackFcn(app, @AxesGraphsPlotButtonDown, true);
            app.AxesGraphsPlot.Position = [12 15 476 234];

            % Create cooSysLabel_3
            app.cooSysLabel_3 = uilabel(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.cooSysLabel_3.HorizontalAlignment = 'right';
            app.cooSysLabel_3.Position = [8 336 115 22];
            app.cooSysLabel_3.Text = 'Unique Target Name';

            % Create GraphPlotUniqueTargetDropDown
            app.GraphPlotUniqueTargetDropDown = uidropdown(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.GraphPlotUniqueTargetDropDown.Items = {};
            app.GraphPlotUniqueTargetDropDown.ValueChangedFcn = createCallbackFcn(app, @GraphPlotUniqueTargetDropDownValueChanged, true);
            app.GraphPlotUniqueTargetDropDown.Position = [133 335 241 22];
            app.GraphPlotUniqueTargetDropDown.Value = {};

            % Create ButtonGroup
            app.ButtonGroup = uibuttongroup(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.ButtonGroup.SelectionChangedFcn = createCallbackFcn(app, @ButtonGroupSelectionChanged, true);
            app.ButtonGroup.ForegroundColor = [0 0 1];
            app.ButtonGroup.Position = [15 254 359 74];

            % Create VisibilityButton
            app.VisibilityButton = uiradiobutton(app.ButtonGroup);
            app.VisibilityButton.Text = 'Visibility';
            app.VisibilityButton.FontWeight = 'bold';
            app.VisibilityButton.FontColor = [0 0 1];
            app.VisibilityButton.Position = [8 40 71 22];
            app.VisibilityButton.Value = true;

            % Create CalibrationStarButton
            app.CalibrationStarButton = uiradiobutton(app.ButtonGroup);
            app.CalibrationStarButton.Text = 'Calibration Star';
            app.CalibrationStarButton.FontWeight = 'bold';
            app.CalibrationStarButton.FontColor = [0 0 1];
            app.CalibrationStarButton.Position = [8 6 111 22];

            % Create PlotCalibObjDropDown
            app.PlotCalibObjDropDown = uidropdown(app.ButtonGroup);
            app.PlotCalibObjDropDown.Items = {};
            app.PlotCalibObjDropDown.ValueChangedFcn = createCallbackFcn(app, @PlotCalibObjDropDownValueChanged, true);
            app.PlotCalibObjDropDown.Position = [122 7 155 22];
            app.PlotCalibObjDropDown.Value = {};

            % Create PlotFlagVisibilitySunCheckBox
            app.PlotFlagVisibilitySunCheckBox = uicheckbox(app.ButtonGroup);
            app.PlotFlagVisibilitySunCheckBox.ValueChangedFcn = createCallbackFcn(app, @PlotFlagVisibilitySunCheckBoxValueChanged, true);
            app.PlotFlagVisibilitySunCheckBox.Text = '';
            app.PlotFlagVisibilitySunCheckBox.Position = [86 33 25 22];
            app.PlotFlagVisibilitySunCheckBox.Value = true;

            % Create PlotFlagVisibilityEarthCheckBox
            app.PlotFlagVisibilityEarthCheckBox = uicheckbox(app.ButtonGroup);
            app.PlotFlagVisibilityEarthCheckBox.ValueChangedFcn = createCallbackFcn(app, @PlotFlagVisibilityEarthCheckBoxValueChanged, true);
            app.PlotFlagVisibilityEarthCheckBox.Text = '';
            app.PlotFlagVisibilityEarthCheckBox.Position = [113 33 25 22];
            app.PlotFlagVisibilityEarthCheckBox.Value = true;

            % Create PlotFlagVisibilityMoonCheckBox
            app.PlotFlagVisibilityMoonCheckBox = uicheckbox(app.ButtonGroup);
            app.PlotFlagVisibilityMoonCheckBox.ValueChangedFcn = createCallbackFcn(app, @PlotFlagVisibilityMoonCheckBoxValueChanged, true);
            app.PlotFlagVisibilityMoonCheckBox.Text = '';
            app.PlotFlagVisibilityMoonCheckBox.Position = [144 33 25 22];
            app.PlotFlagVisibilityMoonCheckBox.Value = true;

            % Create PlanTypeEditFieldLabel_2
            app.PlanTypeEditFieldLabel_2 = uilabel(app.ButtonGroup);
            app.PlanTypeEditFieldLabel_2.HorizontalAlignment = 'right';
            app.PlanTypeEditFieldLabel_2.Position = [75 52 92 22];
            app.PlanTypeEditFieldLabel_2.Text = 'Sun Earth Moon';

            % Create VisibilityPlotTimeUnitsDropDown
            app.VisibilityPlotTimeUnitsDropDown = uidropdown(app.ButtonGroup);
            app.VisibilityPlotTimeUnitsDropDown.Items = {'UTC', 'JD'};
            app.VisibilityPlotTimeUnitsDropDown.ValueChangedFcn = createCallbackFcn(app, @VisibilityPlotTimeUnitsDropDownValueChanged, true);
            app.VisibilityPlotTimeUnitsDropDown.Position = [174 43 79 22];
            app.VisibilityPlotTimeUnitsDropDown.Value = 'UTC';

            % Create OpenGraphsPlotWindowButton
            app.OpenGraphsPlotWindowButton = uibutton(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel, 'push');
            app.OpenGraphsPlotWindowButton.ButtonPushedFcn = createCallbackFcn(app, @OpenGraphsPlotWindowButtonPushed, true);
            app.OpenGraphsPlotWindowButton.FontWeight = 'bold';
            app.OpenGraphsPlotWindowButton.Position = [462 368 34 17];
            app.OpenGraphsPlotWindowButton.Text = '*';

            % Create HelpGraphsPlotWindowButton
            app.HelpGraphsPlotWindowButton = uibutton(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel, 'push');
            app.HelpGraphsPlotWindowButton.ButtonPushedFcn = createCallbackFcn(app, @HelpGraphsPlotWindowButtonPushed, true);
            app.HelpGraphsPlotWindowButton.FontSize = 9;
            app.HelpGraphsPlotWindowButton.FontWeight = 'bold';
            app.HelpGraphsPlotWindowButton.Position = [424 368 34 17];
            app.HelpGraphsPlotWindowButton.Text = '?';

            % Create Panel_11
            app.Panel_11 = uipanel(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.Panel_11.Position = [403 254 86 108];

            % Create OpenCalObjTableButton
            app.OpenCalObjTableButton = uibutton(app.Panel_11, 'push');
            app.OpenCalObjTableButton.ButtonPushedFcn = createCallbackFcn(app, @OpenCalObjTableButtonPushed, true);
            app.OpenCalObjTableButton.Position = [6 7 73 20];
            app.OpenCalObjTableButton.Text = 'CalObj';

            % Create OpenExtSurveysButton
            app.OpenExtSurveysButton = uibutton(app.Panel_11, 'push');
            app.OpenExtSurveysButton.ButtonPushedFcn = createCallbackFcn(app, @OpenExtSurveysButtonPushed, true);
            app.OpenExtSurveysButton.Position = [6 58 73 20];
            app.OpenExtSurveysButton.Text = 'ExtSurveys';

            % Create OpenFieldObjButton
            app.OpenFieldObjButton = uibutton(app.Panel_11, 'push');
            app.OpenFieldObjButton.ButtonPushedFcn = createCallbackFcn(app, @OpenFieldObjButtonPushed, true);
            app.OpenFieldObjButton.Position = [6 32 73 20];
            app.OpenFieldObjButton.Text = 'FieldObj';

            % Create OpenRefImagesButton
            app.OpenRefImagesButton = uibutton(app.Panel_11, 'push');
            app.OpenRefImagesButton.ButtonPushedFcn = createCallbackFcn(app, @OpenRefImagesButtonPushed, true);
            app.OpenRefImagesButton.Position = [6 82 73 20];
            app.OpenRefImagesButton.Text = 'RefImages';

            % Create TabGroup
            app.TabGroup = uitabgroup(app.UIFigure);
            app.TabGroup.Position = [13 638 504 123];

            % Create PlanParamsTab
            app.PlanParamsTab = uitab(app.TabGroup);
            app.PlanParamsTab.Title = 'Plan Params';

            % Create PlanTypeEditFieldLabel
            app.PlanTypeEditFieldLabel = uilabel(app.PlanParamsTab);
            app.PlanTypeEditFieldLabel.HorizontalAlignment = 'right';
            app.PlanTypeEditFieldLabel.Position = [18 68 58 22];
            app.PlanTypeEditFieldLabel.Text = 'Plan Type';

            % Create PlanTypeEditField
            app.PlanTypeEditField = uieditfield(app.PlanParamsTab, 'text');
            app.PlanTypeEditField.Editable = 'off';
            app.PlanTypeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.PlanTypeEditField.Position = [91 68 100 22];

            % Create UserNameEditFieldLabel
            app.UserNameEditFieldLabel = uilabel(app.PlanParamsTab);
            app.UserNameEditFieldLabel.HorizontalAlignment = 'right';
            app.UserNameEditFieldLabel.Position = [10 37 66 22];
            app.UserNameEditFieldLabel.Text = 'User Name';

            % Create UserNameEditField
            app.UserNameEditField = uieditfield(app.PlanParamsTab, 'text');
            app.UserNameEditField.Editable = 'off';
            app.UserNameEditField.BackgroundColor = [1 0.9882 0.8196];
            app.UserNameEditField.Position = [91 37 100 22];

            % Create PlanTitleEditFieldLabel
            app.PlanTitleEditFieldLabel = uilabel(app.PlanParamsTab);
            app.PlanTitleEditFieldLabel.HorizontalAlignment = 'right';
            app.PlanTitleEditFieldLabel.Position = [22 5 54 22];
            app.PlanTitleEditFieldLabel.Text = 'Plan Title';

            % Create PlanTitleEditField
            app.PlanTitleEditField = uieditfield(app.PlanParamsTab, 'text');
            app.PlanTitleEditField.ValueChangedFcn = createCallbackFcn(app, @PlanTitleEditFieldValueChanged, true);
            app.PlanTitleEditField.Position = [91 5 298 22];

            % Create StartTimeEditFieldLabel
            app.StartTimeEditFieldLabel = uilabel(app.PlanParamsTab);
            app.StartTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.StartTimeEditFieldLabel.Position = [217 68 60 22];
            app.StartTimeEditFieldLabel.Text = 'Start Time';

            % Create StartTimeEditField
            app.StartTimeEditField = uieditfield(app.PlanParamsTab, 'text');
            app.StartTimeEditField.ValueChangedFcn = createCallbackFcn(app, @StartTimeEditFieldValueChanged2, true);
            app.StartTimeEditField.Editable = 'off';
            app.StartTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.StartTimeEditField.Tooltip = {'Enter plan start time (i.e. 2024-12-04 00:00:00)'};
            app.StartTimeEditField.Position = [292 68 135 22];

            % Create EndTimeEditFieldLabel
            app.EndTimeEditFieldLabel = uilabel(app.PlanParamsTab);
            app.EndTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.EndTimeEditFieldLabel.Position = [220 37 56 22];
            app.EndTimeEditFieldLabel.Text = 'End Time';

            % Create EndTimeEditField
            app.EndTimeEditField = uieditfield(app.PlanParamsTab, 'text');
            app.EndTimeEditField.ValueChangedFcn = createCallbackFcn(app, @EndTimeEditFieldValueChanged, true);
            app.EndTimeEditField.Editable = 'off';
            app.EndTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.EndTimeEditField.Position = [291 37 136 22];

            % Create HelpPlanParamsButton
            app.HelpPlanParamsButton = uibutton(app.PlanParamsTab, 'push');
            app.HelpPlanParamsButton.ButtonPushedFcn = createCallbackFcn(app, @HelpPlanParamsButtonPushed, true);
            app.HelpPlanParamsButton.FontSize = 9;
            app.HelpPlanParamsButton.FontWeight = 'bold';
            app.HelpPlanParamsButton.Position = [435 72 34 17];
            app.HelpPlanParamsButton.Text = '?';

            % Create PkEditFieldLabel
            app.PkEditFieldLabel = uilabel(app.PlanParamsTab);
            app.PkEditFieldLabel.HorizontalAlignment = 'right';
            app.PkEditFieldLabel.FontWeight = 'bold';
            app.PkEditFieldLabel.Position = [401 7 25 22];
            app.PkEditFieldLabel.Text = 'Pk';

            % Create PlanPkEditField
            app.PlanPkEditField = uieditfield(app.PlanParamsTab, 'text');
            app.PlanPkEditField.Editable = 'off';
            app.PlanPkEditField.BackgroundColor = [1 0.9882 0.8196];
            app.PlanPkEditField.Position = [435 7 60 22];

            % Create Panel_8
            app.Panel_8 = uipanel(app.UIFigure);
            app.Panel_8.TitlePosition = 'centertop';
            app.Panel_8.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_8.Position = [13 1 989 76];

            % Create StatusTextAreaLabel
            app.StatusTextAreaLabel = uilabel(app.Panel_8);
            app.StatusTextAreaLabel.HorizontalAlignment = 'right';
            app.StatusTextAreaLabel.Position = [24 44 39 22];
            app.StatusTextAreaLabel.Text = 'Status';

            % Create StatusTextArea
            app.StatusTextArea = uitextarea(app.Panel_8);
            app.StatusTextArea.Editable = 'off';
            app.StatusTextArea.BackgroundColor = [0.9412 0.9412 0.9412];
            app.StatusTextArea.Position = [78 15 833 53];

            % Create RefreshApprovedTargetsButton_2
            app.RefreshApprovedTargetsButton_2 = uibutton(app.Panel_8, 'push');
            app.RefreshApprovedTargetsButton_2.ButtonPushedFcn = createCallbackFcn(app, @RefreshApprovedTargetsButton_2Pushed, true);
            app.RefreshApprovedTargetsButton_2.FontSize = 11;
            app.RefreshApprovedTargetsButton_2.Position = [927 43 52 25];
            app.RefreshApprovedTargetsButton_2.Text = 'Clear';

            % Create RefreshApprovedTargetsButton_4
            app.RefreshApprovedTargetsButton_4 = uibutton(app.Panel_8, 'push');
            app.RefreshApprovedTargetsButton_4.ButtonPushedFcn = createCallbackFcn(app, @RefreshApprovedTargetsButton_4Pushed, true);
            app.RefreshApprovedTargetsButton_4.FontSize = 11;
            app.RefreshApprovedTargetsButton_4.Position = [928 10 52 25];
            app.RefreshApprovedTargetsButton_4.Text = 'Log';

            % Create TabGroup2
            app.TabGroup2 = uitabgroup(app.UIFigure);
            app.TabGroup2.Position = [530 639 475 123];

            % Create StatusTab
            app.StatusTab = uitab(app.TabGroup2);
            app.StatusTab.Title = 'Status';

            % Create BuildEditFieldLabel
            app.BuildEditFieldLabel = uilabel(app.StatusTab);
            app.BuildEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.BuildEditFieldLabel.HorizontalAlignment = 'right';
            app.BuildEditFieldLabel.FontWeight = 'bold';
            app.BuildEditFieldLabel.Position = [6 69 35 22];
            app.BuildEditFieldLabel.Text = 'Build';

            % Create BuildTimeEditField
            app.BuildTimeEditField = uieditfield(app.StatusTab, 'text');
            app.BuildTimeEditField.Editable = 'off';
            app.BuildTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BuildTimeEditField.Position = [79 69 136 22];

            % Create ValidationEditFieldLabel
            app.ValidationEditFieldLabel = uilabel(app.StatusTab);
            app.ValidationEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.ValidationEditFieldLabel.HorizontalAlignment = 'right';
            app.ValidationEditFieldLabel.FontWeight = 'bold';
            app.ValidationEditFieldLabel.Position = [6 38 62 22];
            app.ValidationEditFieldLabel.Text = 'Validation';

            % Create ValidationTimeEditField
            app.ValidationTimeEditField = uieditfield(app.StatusTab, 'text');
            app.ValidationTimeEditField.Editable = 'off';
            app.ValidationTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ValidationTimeEditField.Position = [78 38 136 22];

            % Create SubmitEditFieldLabel
            app.SubmitEditFieldLabel = uilabel(app.StatusTab);
            app.SubmitEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.SubmitEditFieldLabel.HorizontalAlignment = 'right';
            app.SubmitEditFieldLabel.FontWeight = 'bold';
            app.SubmitEditFieldLabel.Position = [6 6 46 22];
            app.SubmitEditFieldLabel.Text = 'Submit';

            % Create SubmitTimeEditField
            app.SubmitTimeEditField = uieditfield(app.StatusTab, 'text');
            app.SubmitTimeEditField.Editable = 'off';
            app.SubmitTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SubmitTimeEditField.Position = [78 6 136 22];

            % Create BuildShortStatusEditField
            app.BuildShortStatusEditField = uieditfield(app.StatusTab, 'text');
            app.BuildShortStatusEditField.Editable = 'off';
            app.BuildShortStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BuildShortStatusEditField.Position = [226 69 145 22];

            % Create ValidationShortStatusEditField
            app.ValidationShortStatusEditField = uieditfield(app.StatusTab, 'text');
            app.ValidationShortStatusEditField.Editable = 'off';
            app.ValidationShortStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ValidationShortStatusEditField.Position = [226 38 145 22];

            % Create SubmitShortStatusEditField
            app.SubmitShortStatusEditField = uieditfield(app.StatusTab, 'text');
            app.SubmitShortStatusEditField.Editable = 'off';
            app.SubmitShortStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SubmitShortStatusEditField.Position = [226 6 145 22];

            % Create BuildStatusButton
            app.BuildStatusButton = uibutton(app.StatusTab, 'push');
            app.BuildStatusButton.ButtonPushedFcn = createCallbackFcn(app, @BuildStatusButtonPushed, true);
            app.BuildStatusButton.Tooltip = {'Load unique targets from text file'};
            app.BuildStatusButton.Position = [377 69 36 23];
            app.BuildStatusButton.Text = '...';

            % Create ValidationStatusButton
            app.ValidationStatusButton = uibutton(app.StatusTab, 'push');
            app.ValidationStatusButton.ButtonPushedFcn = createCallbackFcn(app, @ValidationStatusButtonPushed, true);
            app.ValidationStatusButton.Tooltip = {'Load unique targets from text file'};
            app.ValidationStatusButton.Position = [378 38 36 23];
            app.ValidationStatusButton.Text = '...';

            % Create SubmitStatusButton
            app.SubmitStatusButton = uibutton(app.StatusTab, 'push');
            app.SubmitStatusButton.ButtonPushedFcn = createCallbackFcn(app, @SubmitStatusButtonPushed, true);
            app.SubmitStatusButton.Tooltip = {'Load unique targets from text file'};
            app.SubmitStatusButton.Position = [378 6 36 23];
            app.SubmitStatusButton.Text = '...';

            % Create HelpStatusInfoButton
            app.HelpStatusInfoButton = uibutton(app.StatusTab, 'push');
            app.HelpStatusInfoButton.ButtonPushedFcn = createCallbackFcn(app, @HelpStatusInfoButtonPushed, true);
            app.HelpStatusInfoButton.FontSize = 9;
            app.HelpStatusInfoButton.FontWeight = 'bold';
            app.HelpStatusInfoButton.Position = [431 71 34 17];
            app.HelpStatusInfoButton.Text = '?';

            % Create PanelTopHeader
            app.PanelTopHeader = uipanel(app.UIFigure);
            app.PanelTopHeader.BorderType = 'none';
            app.PanelTopHeader.Position = [2 814 1513 38];

            % Create LabelTopStatus
            app.LabelTopStatus = uilabel(app.PanelTopHeader);
            app.LabelTopStatus.BackgroundColor = [1 1 0.0667];
            app.LabelTopStatus.HorizontalAlignment = 'center';
            app.LabelTopStatus.FontSize = 16;
            app.LabelTopStatus.FontWeight = 'bold';
            app.LabelTopStatus.FontColor = [0 0 1];
            app.LabelTopStatus.Visible = 'off';
            app.LabelTopStatus.Position = [344 3 498 30];
            app.LabelTopStatus.Text = 'The plan was submitted and cannot be modified.';

            % Create LabelTopNamespace
            app.LabelTopNamespace = uilabel(app.PanelTopHeader);
            app.LabelTopNamespace.BackgroundColor = [0 0 0];
            app.LabelTopNamespace.HorizontalAlignment = 'center';
            app.LabelTopNamespace.FontSize = 24;
            app.LabelTopNamespace.FontWeight = 'bold';
            app.LabelTopNamespace.FontColor = [1 1 1];
            app.LabelTopNamespace.Visible = 'off';
            app.LabelTopNamespace.Position = [1057 2 272 32];
            app.LabelTopNamespace.Text = 'OPER';

            % Create LabelTopUser
            app.LabelTopUser = uilabel(app.PanelTopHeader);
            app.LabelTopUser.BackgroundColor = [1 1 0.549];
            app.LabelTopUser.HorizontalAlignment = 'center';
            app.LabelTopUser.FontSize = 24;
            app.LabelTopUser.FontWeight = 'bold';
            app.LabelTopUser.FontColor = [0 0 1];
            app.LabelTopUser.Position = [1345 3 165 32];
            app.LabelTopUser.Text = 'Please login';

            % Create LabelTopTime
            app.LabelTopTime = uilabel(app.PanelTopHeader);
            app.LabelTopTime.BackgroundColor = [0.9412 0.9412 0.9412];
            app.LabelTopTime.HorizontalAlignment = 'center';
            app.LabelTopTime.FontSize = 24;
            app.LabelTopTime.FontWeight = 'bold';
            app.LabelTopTime.FontColor = [0.651 0.651 0.651];
            app.LabelTopTime.Position = [12 2 299 32];
            app.LabelTopTime.Text = 'UTC: 2025-01-01 00:00:00';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlannerMain(varargin)

            runningApp = getRunningApp(app);

            % Check for running singleton app
            if isempty(runningApp)

                % Create UIFigure and components
                createComponents(app)

                % Register the app with App Designer
                registerApp(app, app.UIFigure)

                % Execute the startup function
                runStartupFcn(app, @(app)startupFcn(app, varargin{:}))
            else

                % Focus the running singleton app
                figure(runningApp.UIFigure)

                app = runningApp;
            end

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