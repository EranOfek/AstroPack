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
        ToolsMenu                       matlab.ui.container.Menu
        SNRCalculatorMenu               matlab.ui.container.Menu
        HelpMenu                        matlab.ui.container.Menu
        ObservationPlannerHelpMenu      matlab.ui.container.Menu
        AboutMenu                       matlab.ui.container.Menu
        PanelTopHeader                  matlab.ui.container.Panel
        LabelTopTime                    matlab.ui.control.Label
        LabelTopUser                    matlab.ui.control.Label
        LabelTopNamespace               matlab.ui.control.Label
        LabelTopStatus                  matlab.ui.control.Label
        TabGroup2                       matlab.ui.container.TabGroup
        Tab                             matlab.ui.container.Tab
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
        RefreshApprovedTargetsButton_2  matlab.ui.control.Button
        StatusTextArea                  matlab.ui.control.TextArea
        StatusTextAreaLabel             matlab.ui.control.Label
        TabGroup                        matlab.ui.container.TabGroup
        PlanParamsTab                   matlab.ui.container.Tab
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
        HelpGraphsPlotWindowButton      matlab.ui.control.Button
        OpenGraphsPlotWindowButton      matlab.ui.control.Button
        OpenCalObjTableButton           matlab.ui.control.Button
        ButtonGroup                     matlab.ui.container.ButtonGroup
        CalibrationStarButton           matlab.ui.control.RadioButton
        VisibilityButton                matlab.ui.control.RadioButton
        GraphPlotUniqueTargetDropDown   matlab.ui.control.DropDown
        cooSysLabel_3                   matlab.ui.control.Label
        PlotCalibObjDropDown            matlab.ui.control.DropDown
        cooSysLabel_2                   matlab.ui.control.Label
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
        GDriveCommentsButton            matlab.ui.control.Button
        RetractButton                   matlab.ui.control.Button
        DuplicateButton                 matlab.ui.control.Button
        ModifiedLabel                   matlab.ui.control.Label
        ConnectionStatusEditField       matlab.ui.control.EditField
        ConnectionStatusEditFieldLabel  matlab.ui.control.Label
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
    
    properties (Access = private)
        % Data
        MainModule                  %
        LoggerApp                   %
        TimerSec                    % Timer object

        % =================================================================
        %   Forms (AppDesigner Apps in ultrasat.planner.gui) - sorted abc
        % =================================================================        
        
        AboutApp                                % About
        AddUniqueTargetApp                      % Unique Target - Add
        AdjustGroupStartTimeApp                 %
        ApprovedTargetParamsApp                 %
        ApprovedTargetsApp                      %
        BuildStatusApp                          %        
        CalibObjTableApp                        %
        DuplicatePlanApp                        %
        EnterStartTimeApp                       %
        LoadPlanFromFileApp                     % Load file from disk
        LoadUniqueTargetsFromFileApp            % Load from text file        
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
        
        % =================================================================
        % Data
        AllowEdit                               % = ~ReadOnly
        AllowEditMsg = 'Cannot edit plan with status submitted'
        Preferences                             % Refrence to app.MainModule.Preferences
        UniqueTargetCalibObj                    % Table returned by Planner.showCalibObj()        
    end


    properties (Access = public)
        AppUtils                                %
        MsgBoxApp                               %         
    end

    % =====================================================================
    %                         Init, Login, Log
    % =====================================================================
    methods (Access = public)
        function init(app)
            % Called from startupFcn() on application startup

            % Create MainModule that holds all common data
            app.UIFigure.Name = 'ULTRASAT Observation Planner';

            % Create MainModule that holds all common data and link objects
            app.MainModule = ultrasat.planner.gui.MainModule();
            app.MainModule.MainApp = app;

            % Create AppUtils and set in MainModule
            app.AppUtils = ultrasat.planner.gui.AppUtils(app.MainModule);
            app.AppUtils.App = app;
            app.MainModule.AppUtils = app.AppUtils;

            % Assign preferences loaded by MainModule
            app.Preferences = app.MainModule.Preferences;

            % Create log window
            app.LoggerApp = ultrasat.planner.gui.Logger(app.MainModule);
            app.LoggerApp.UIFigure.Visible = 'off';
            app.MainModule.LoggerApp = app.LoggerApp;
            app.msglog('init started');

            app.TimerSec = timer('ExecutionMode', 'fixedRate', 'Period', 1, 'TimerFcn', @(~,~) updateTimerSec(app));
            start(app.TimerSec);

            % Set initial state for buttons and menus
            app.setLoginButtonStatus();
            app.setButtons();

            app.msglog('init done');
        end


        function updateTimerSec(app)
            currentTime = datetime('now', 'TimeZone', 'UTC');
            formattedTime = strcat('UTC: ', datestr(currentTime, 'yyyy-mm-dd HH:MM:SS'));
            app.LabelTopTime.Text = formattedTime;
        end

        % =================================================================
        %                            User Login
        % =================================================================

        function login(app)
            % User login
            app.msglog('login');

            % Do nothing if already connected
            if ~isempty(app.MainModule.UserName)
                return;
            end

            % Check active planner user name
            if app.hasPlanner()
                if ~strcmp(app.AppUtils.askYesNo('Note: you have an active plan, after connection the AstPlanner field will be set to the logged-in user name. Would you like to continue?', 'Confirmation'), 'Yes')
                    return;
                end
            end          

            % Create app
            if isempty(app.LoginApp) || ~isvalid(app.LoginApp)
                app.LoginApp = ultrasat.planner.gui.Login(app.MainModule);
            end            
            LoginStatus = app.showModal(app.LoginApp);
            app.msglog( sprintf('login uiwait returned: isempty: %d, isvalid: %d', isempty(app.LoginApp), isvalid(app.LoginApp)) );

            app.setLoginButtonStatus();
            app.setButtons();

            % Set AstPlanner to connected user
            if app.hasPlanner()
                if ~strcmp(app.MainModule.Planner.AstPlanner, app.MainModule.UserName)
                    app.msglog(sprintf('Login: Setting AstPlanner field of open plan: %s, %s', app.MainModule.Planner.AstPlanner, app.MainModule.UserName));
                    app.MainModule.Planner.AstPlanner = app.MainModule.UserName;
                end
            end            
        end


        function logout(app)
            % User logout
            app.msglog('logout');

            % Do nothing if not connected
            if isempty(app.MainModule.UserName)
                return;
            end

            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to logout?', 'Confirm'), 'Yes')
                return;
            end

            app.MainModule.logout();
            app.setLoginButtonStatus();
            app.setButtons();            
        end        


        function setLoginButtonStatus(app)
            % Connect button
            if ~isempty(app.MainModule.UserName)
                app.LoginButton.Text = 'Connected';
                app.LoginButton.BackgroundColor = [0.00, 1.00, 0.00];  % Green
                app.LabelTopUser.Text = app.MainModule.UserName;
            else
                app.LoginButton.Text = 'Login';
                app.LoginButton.BackgroundColor = [1.00, 1.00, 0.07];  % Yellow
                app.LabelTopUser.Text = 'Please login';
            end            

            % Namespace & Username
            if ~isempty(app.MainModule)
                % Set Namespace label colors
                app.LabelTopNamespace.Text = app.MainModule.NamespaceDisplay;
                if strcmp(app.MainModule.NamespaceId, 'OPER') 
                    app.LabelTopNamespace.FontColor = [1.00,1.00,1.00];  % White on black
                    app.LabelTopNamespace.BackgroundColor = [0.00,0.00,0.00];  
                else
                    app.LabelTopNamespace.FontColor = [0.00,0.00,0.00];  % Black on yellow
                    app.LabelTopNamespace.BackgroundColor = [1.00,1.00,0.07];
                end
            end
        end


        function Result = isLogin(app, Args)
            % Return true is user is loggned in, show popup message if Args.Message is true
            arguments
                app
                Args.Message = false
            end
            % Return true if used is logged-in
            Result = ~isempty(app.MainModule.UserName);
            if ~Result
                app.msglog('isLogin: not loggedin');
                if Args.Message
                    uialert(app.UIFigure, 'Login to the server is required to proceed with this operation', 'Message', 'Icon', 'success');            
                end
            end
        end


        function Result = isAllowed(app, Action)
            % Return true if specified action is allowed for current logged-in user
            % @TODO - To be replaced with checking permissions like Delphi code
            Result = ~isempty(app.MainModule.UserName);            
            if ~Result
                app.msglog(sprintf('isAllowed: not allowed: %', Action));
            end            
        end 


        function setButtons(app)
            % Enable/disable buttons and menu options based on current login status.

            enable = app.isLogin();

            % Apply to buttons
            %app.OpenButton.Enable = enable;
            app.SaveButton.Enable = app.hasPlanner();
            app.ParamsButton.Enable = app.hasPlanner();
            app.DuplicateButton.Enable = app.hasPlanner();
            app.ValidateButton.Enable = enable && app.hasPlanner();
            app.SubmitButton.Enable = enable && app.hasPlanner();

            % Apply to menu
            %app.OpenMenu.Enable = enable;
            app.SaveMenu.Enable = app.hasPlanner();
            app.ParamsButton.Enable = app.hasPlanner();
            app.DuplicateMenu.Enable = app.hasPlanner();
            app.ValidateMenu.Enable = enable && app.hasPlanner();
            app.SubmitMenu.Enable = enable && app.hasPlanner();
        end


        function showLogger(app)
            % Show log window
            app.msglog('showLogger');

            % Create app            
            if isempty(app.LoggerApp) || ~isvalid(app.LoggerApp)
                app.LoggerApp = ultrasat.planner.gui.Logger(app.MainModule);
            end            
            app.LoggerApp.UIFigure.Visible = 'on';
        end        


        function exitPlanner(app)
            % Exit the planner GUI
            %answer = questdlg('Are you sure you want to exit the Observaion Planner?', 'Confirm exit', 'Yes', 'No', 'No');            
            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to exit the planner application?', 'Confirmation'), 'Yes')
                return;
            end          

            % Shut down the entire app
            app.delete();
        end
    end

    % =====================================================================
    %             Create New Plan - HCS, LCS, DDT, AllSS, ToO
    % =====================================================================    
    methods (Access = public)

        function createNewPlan(app)
            % Create new plan
            app.msglog('createNewPlan');            

            if app.MainModule.Modified
                if ~strcmp(app.AppUtils.askYesNo('Your changes are not saved. Do you want to discard them and create a new plan?', 'Save or discard'), 'Yes')
                    return;
                end
            end

            % Close existing plan
            app.closePlan();

            % Create app
            if isempty(app.NewPlanApp) || ~isvalid(app.NewPlanApp)
                app.NewPlanApp = ultrasat.planner.gui.NewPlan(app.MainModule);                
            end

            % Set PlannerName field value
            if app.isLogin()
                app.NewPlanApp.PlannerNameEditField.Value = app.MainModule.UserName;
                app.NewPlanApp.PlannerNameEditField.Enable = false;
            else
                app.NewPlanApp.PlannerNameEditField.Value = '';
                app.NewPlanApp.PlannerNameEditField.Enable = true;
            end

            if ~strcmp(app.showModal(app.NewPlanApp), 'Create')
                return;
            end

            app.msglog(sprintf('New plan type: %s ....', app.MainModule.PlanType));
            try
                app.doCreateNewPlan();
            catch ME
                app.msgex('createNewPlan', ME);
            end

            %
            app.setButtons();
        end


        function doCreateNewPlan(app)
            % Create new plan according to parameters in app.NewPlanApp
            PlanType = app.NewPlanApp.PlanType;
            app.msglog(sprintf('doCreateNewPlan: PlanType: %s', PlanType));
            
            % Create new PlanData instance
            app.MainModule.createPlanData();

            % Call the designated function according to PlanType
            if strcmp(PlanType, 'HCS')
                app.doCreateNewPlanHCS();
            elseif strcmp(PlanType, 'LCS')
                app.doCreateNewPlanLCS();
            elseif strcmp(PlanType, 'DDT')
                app.doCreateNewPlanDDT();                
            elseif strcmp(PlanType, 'AllSS')
                app.doCreateNewPlanAllSS();
            elseif strcmp(PlanType, 'TOO')
                app.doCreateNewPlanTOO();
            else
                error('doCreateNewPlan: Unknown PlanType: %s', PlanType)
            end

            % Update data and references
            app.MainModule.PlanData.planner = app.MainModule.Planner;
            app.MainModule.AfterBuild = false;

            % Update GUI
            app.SaveButton.Enable = 'off';
            if strcmp(PlanType, 'DDT')            
                app.BuildButton.Text = 'Add';
            else
                app.BuildButton.Text = 'Build';
            end

            %
            app.setModified('doCreateNewPlan');
            app.showUniqueTargets();
            app.showPlanTargets();
            app.setStatus('OK', 'New plan created successfully');
            app.msglog('doCreateNewPlan done');
        end


        function doCreateNewPlanHCS(app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanHCS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = app.getNewPlanUserName();

            % Create new uplanner instance            
            upHCS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'HCS', 'BaseDataDir', app.MainModule.BaseDataDir);
            app.setNewPlanDataFromCreateDialog(upHCS);

            app.MainModule.setPlanner(upHCS);
            app.setModified('doCreateNewPlanHCS');
            app.updatePlanParams();
            %app.debugSave('upHCS.mat', app.MainModule.Planner);
            app.msglog('doCreateNewPlanHCS done');
        end


        function doCreateNewPlanLCS(app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanLCS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = app.getNewPlanUserName();
            
            % Create new uplanner instance
            upLCS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'LCS', 'BaseDataDir', app.MainModule.BaseDataDir);
            app.setNewPlanDataFromCreateDialog(upLCS);

            app.MainModule.setPlanner(upLCS);
            app.setModified('doCreateNewPlanLCS');
            app.updatePlanParams();
            %app.debugSave('upLCS.mat', app.MainModule.Planner);
            app.msglog('doCreateNewPlanLCS done');
        end


        function doCreateNewPlanDDT(app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanDDT started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = app.getNewPlanUserName();            

            % Create new uplanner instance            
            upDDT = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'DDT', 'BaseDataDir', app.MainModule.BaseDataDir);
            app.setNewPlanDataFromCreateDialog(upDDT);

            app.MainModule.setPlanner(upDDT);
            app.setModified('doCreateNewPlanDDT');
            app.updatePlanParams();
            %app.debugSave('upDDT.mat', 'app.MainModule.Planner');
            app.msglog('doCreateNewPlanDDT done');
        end


        function doCreateNewPlanTOO(app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanTOO started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = app.getNewPlanUserName();            

            % Create new uplanner instance            
            upTOO = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'TOO', 'BaseDataDir', app.MainModule.BaseDataDir);
            app.setNewPlanDataFromCreateDialog(upHCS);            

            app.MainModule.setPlanner(upTOO);
            app.setModified('doCreateNewPlanDDT');
            app.updatePlanParams();
            %app.debugSave('upTOO.mat', 'app.MainModule.Planner');
            app.msglog('doCreateNewPlanTOO done');
        end


        function doCreateNewPlanAllSS(app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanAllSS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = app.getNewPlanUserName();

            % Create new uplanner instance            
            upAllSS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'AllSS', 'BaseDataDir', app.MainModule.BaseDataDir);
            app.setNewPlanDataFromCreateDialog(upHCS);

            app.MainModule.setPlanner(upAllSS);
            app.setModified('doCreateNewPlanDDT');
            app.updatePlanParams();
            %app.debugSave('upLCS.mat', 'app.MainModule.Planner');
            app.msglog('doCreateNewPlanAllSS done');
        end        


        function UserName = getNewPlanUserName(app)
            % Helper: Get logged-in user name, or user name entered in NewPlanApp dialog            
            if app.isLogin()
                UserName = app.MainModule.UserName;
            else
                UserName = app.NewPlanApp.PlannerNameEditField.Value;
            end            
        end


        function setNewPlanDataFromCreateDialog(app, Planner)
            % Helper: Set planner data from the create dialog: PlanTitle, StartTime, EndTime
            PlanTitle = app.MainModule.getFieldTitle( app.NewPlanApp.TitleEditField.Value );
            StartTime = app.MainModule.getFieldDateTime( app.NewPlanApp.StartTimeEditField.Value );
            EndTime = app.MainModule.getFieldDateTime( app.NewPlanApp.EndTimeEditField.Value );            

            Planner.Title = PlanTitle;
            Planner.StartTime = StartTime;
            Planner.EndTime = EndTime;            
        end

    end

    % =====================================================================
    %           Plan - Open, Save, Close, Load, Duplicate Plan
    % =====================================================================    
    methods (Access = public)

        function openPlan(app)
            % Load plan from database, requires login and server connection
            app.msglog('openPlan');    

            % User is not connected, suggset to load plan from local file
            if ~app.isLogin()
                if strcmp(app.AppUtils.askYesNo('You are not connected to the ULTRASAT DB, would you like to open a local file?', 'Open'), 'Yes')
                    app.loadPlanFromFile();
                end                
                return;
            end

            % Ask user to confirm
            if app.MainModule.Modified
                if ~strcmp(app.AppUtils.askYesNo('Your changes are not saved. Do you want to discard them and create a new plan?', 'Confirm'), 'Yes')
                    return;
                end
            end

            % Create app
            if isempty(app.OpenPlanApp) || ~isvalid(app.OpenPlanApp)
                app.OpenPlanApp = ultrasat.planner.gui.OpenPlan(app.MainModule);                
            end

            % Setup table
            app.OpenPlanApp.UITable.SelectionType = "row";
            app.OpenPlanApp.UITable.Multiselect = "off";            
            app.OpenPlanApp.UITable.RowName = "numbered";

            % Query backend database for saved plans
            response = app.MainModule.ApiClient.getPlansList([], [], []);
            if ~response.ok
                app.AppUtils.msgError('ApiClient.getPlansList returned empty list');
                return;
            end
            
            % Update the GUI table            
            % WHY this is required if OpenPlan has getList() func that does
            % the same ??? (25/09/2025)
            %Data = app.MainModule.plansToTopLevelTable(response.plans);
            %%Data = struct2table(plans, 'AsArray', true);
            %Data = app.MainModule.convertTableDatetimeToString(Data);
            %app.OpenPlanApp.UITable.Data = Data;
            %if ~isempty(Data)
            %    app.OpenPlanApp.UITable.ColumnName = Data.Properties.VariableNames;
            %end
            
            % Show app
            if strcmp(app.showModal(app.OpenPlanApp), 'Open')

                % Call the backend to load plan from database
                Pk = app.OpenPlanApp.Pk;
                response = app.MainModule.ApiClient.loadPlan(Pk);
                if response.ok
                    app.doOpenPlan(app.MainModule.ApiClient.PlanData);
                end

            end
            app.clearModified();
            app.setButtons();
        end


        function doOpenPlan(app, PlanData)
            % called from openPlan()
            app.msglog(sprintf('doOpenPlan: %d', PlanData.pk));

            % Check active planner user name
            if ~strcmp(PlanData.planner.AstPlanner, app.MainModule.UserName)
                Result = app.AppUtils.askYesNoCancel('The AstPlanner field in this plan differs from the currently logged-in user. Click Yes to duplicate the plan or No to open in read-only mode.', 'Confirmation');
                if strcmp(Result, 'Yes')
                    app.duplicatePlan();
                elseif strcmp(Result, 'No')
                    app.setReadOnly(true);
                else
                    app.closePlan();
                    return;
                end

                %app.msglog(sprintf('doOpenPlan: Setting AstPlanner field of open plan: %s, %s', app.MainModule.Planner.AstPlanner, app.MainModule.UserName));
            end

            app.MainModule.setPlanData(PlanData);
            app.updateStatus();
            app.showPlanAll();
        end
        

        function savePlan(app)
            % Save current plan to database, requires login and server connection
            app.msglog('savePlan');                        
            if ~app.hasPlanner(), return; end

            % User is not connected to server, suggest saving to local file
            if ~app.isLogin()
                if strcmp(app.AppUtils.askYesNo('You are not connected to the ULTRASAT DB, would you like to save to local file?', 'Save'), 'Yes')
                    app.savePlanToFile();
                end                
                return;
            end            

            % Call backend to save the plan in database
            app.showPleaseWait('Saving...');
            try
                app.MainModule.ApiClient.savePlan();
                app.clearModified();
            catch ME
                app.msgex('savePlan', ME);
            end

            app.closePleaseWait();            
            app.clearModified();
            app.MainModule.setStatus('OK', 'Plan saved successfully.');
        end


        function closePlan(app)
            %
            app.msglog('closePlan');
            if app.MainModule.Modified
                % Ask user to save current modified plan or to discard it
                Result = app.AppUtils.askSaveDiscardCancel('The plan is modified, save changes or discard?', 'Save or discard modified plan');
                if strcmp(Result, 'Save')
                    app.savePlan();
                end
                if strcmp(Result, 'Cancel')
                    return;
                end
            end

            app.doClosePlan();
            app.clearModified();
            app.setButtons();
        end


        function doClosePlan(app)
            %
            app.msglog('doClosePlan');
            app.MainModule.clearData();

            app.showPlanAll();
            app.clearModified();
            app.setButtons();            
        end


        function deletePlan(app)
            %
            app.msglog('deletePlan');
            if app.MainModule.Modified
                % Ask user to save current modified plan or to discard it
            end

            %app.clearModified();
        end


        function savePlanToFile(app)
            % Save current plan to text file, open dialog to ask user for
            % file name
            app.msglog('savePlanToFile');
            if ~app.hasPlanner(), return; end

            % Create app and set initial values from preferences
            if isempty(app.SavePlanToFileApp) || ~isvalid(app.SavePlanToFileApp)
                app.SavePlanToFileApp = ultrasat.planner.gui.SavePlanToFile(app.MainModule);                
                if ~isempty(app.Preferences.LocalPlanFolder)
                    app.SavePlanToFileApp.FileNameEditField.Value = app.Preferences.LocalPlanFileName;
                    app.SavePlanToFileApp.Folder = app.Preferences.LocalPlanFolder;
                end
            end

            % Show app
            if strcmp(app.showModal(app.SavePlanToFileApp), 'Save')
                app.showPleaseWait('Saving to file...');
                try
                    FileName = app.SavePlanToFileApp.FileName;

                    % Create local object for save()
                    % PlanData contains reference to app.MainModule.Planner;
                    PlanData = app.MainModule.PlanData;
                    save(FileName, 'PlanData');
                    app.setStatus('OK', sprintf('Plan saved to file: %s', FileName));

                    % Update preferences
                    app.Preferences.LocalPlanFileName = FileName;
                    app.Preferences.LocalPlanFolder = fileparts(FileName);
                    app.savePreferences();
                catch ME
                    app.msgex('savePlanToFile', ME);
                end
                app.closePleaseWait();            
            end            
        end


        function loadPlanFromFile(app)
            % Load plan from file as matlab object
            app.msglog('loadPlanFromFile');

            if app.MainModule.Modified
                % @Todo - Save or discard
            end

            % Create app and set initial values from preferences
            if isempty(app.LoadPlanFromFileApp) || ~isvalid(app.LoadPlanFromFileApp)
                app.LoadPlanFromFileApp = ultrasat.planner.gui.LoadPlanFromFile(app.MainModule);
                if ~isempty(app.Preferences.LocalPlanFolder)
                    app.LoadPlanFromFileApp.FileName = app.Preferences.LocalPlanFileName;
                    app.LoadPlanFromFileApp.Folder = app.Preferences.LocalPlanFolder;
                    app.LoadPlanFromFileApp.FileNameEditField.Value = app.Preferences.LocalPlanFileName;
                end
            end

            % Show app
            if strcmp(app.showModal(app.LoadPlanFromFileApp), 'Load')
                try
                    FileName = app.LoadPlanFromFileApp.FileName;
                    Data = load(FileName);
                    app.MainModule.setPlanData(Data.PlanData);
                    app.showPlanAll();
                    app.setStatus('OK', sprintf('Plan loaded from file: %s', FileName));

                    % Update preferences
                    app.Preferences.LocalPlanFileName = FileName;
                    app.Preferences.LocalPlanFolder = fileparts(FileName);
                    app.savePreferences();                    
                catch ME
                    app.msgex('loadPlanFromFile', ME);
                end
            end
            app.setButtons();

            % Check active planner user name
            if ~isempty(app.MainModule.UserName) && ~strcmp(app.MainModule.Planner.AstPlanner, app.MainModule.UserName)
                if ~strcmp(app.AppUtils.askYesNo('The AstPlanner field in this plan differs from the currently logged-in user. Click Yes to override AstPlanner or No to cancel opening.', 'Confirmation'), 'Yes')
                    return;
                end

                app.msglog(sprintf('loadPlanFromFile: Setting AstPlanner field of open plan: %s, %s', app.MainModule.Planner.AstPlanner, app.MainModule.User));
                app.MainModule.Planner.AstPlanner = app.MainModule.UserName;
            end                      
        end


        function duplicatePlan(app)
            % Duplicate the current observation plan.
            %
            % This function creates a copy of the current plan with a new title and user-assigned 
            % owner while ensuring necessary resets:
            %
            % - Prompts user confirmation by opening DuplicatePlanApp for input.
            % - Resets key fields: clears pk, id, and timestamps for a fresh entry.
            % - Tracks history by logging duplication details in PlanData.history.
            % - Resets submission & approval by clearing SubmitStatus and mission approvals.
            % - Updates UI to refresh the display and reflect the duplicated plan.
            %
            app.msglog('duplicatePlan');
            if ~app.hasPlanner(), return; end

            % @Todo
            if ~app.needSave(true)
                %return;
            end

            % Create app
            if isempty(app.DuplicatePlanApp) || ~isvalid(app.DuplicatePlanApp)
                app.DuplicatePlanApp = ultrasat.planner.gui.DuplicatePlan(app.MainModule);
            end

            app.DuplicatePlanApp.PlanTitleEditField.Value = sprintf('Duplicated on %s', ultrasat.api.ModelBase.nowUtcStr());
            app.DuplicatePlanApp.UserNameEditField.Value = app.MainModule.Planner.AstPlanner;

            % Show app
            if strcmp(app.showModal(app.DuplicatePlanApp), 'Duplicate')
                try
                    PlanData = app.MainModule.PlanData;
                    Planner = app.MainModule.Planner;

                    %Title = app.DuplicatePlanApp.PlanTitleEditField;
                    %UserName = app.DuplicatePlanApp.UserNameEditField; 

                    %OldPk = PlanData.pk;
                    %OldId = PlanData.id;
                    %OldAstPlanner = PlanData.ast_planner;

                    % Clear the pk field                    
                    PlanData.pk = [];
                    PlanData.id = [];

                    % Update fields and add history
                    PlanData.create_time = ultrasat.api.ModelBase.nowUtc();
                    PlanData.update_time = PlanData.create_time;
                    PlanData.history = struct();
                    PlanData.addHistory(sprintf('Duplicated from pk=%d, %s', OldPk, ultrasat.api.ModelBase.datetimeStr(PlanData.update_time)));
                   
                    % Reset the submit status
                    PlanData.metadata.SubmitStatus = PlanData.newStatusData();

                    % Reset status
                    Planner.Status = 'draft';
                    Planner.clearMissionApprovedPlan();

                    % What other status we need to clear? @Todo @Yossi

                    % Update display
                    app.showPlanAll();                    
                catch ME
                    app.msgex('duplicatePlan', ME);
                end
            end
        end


        function showPlanAll(app)
            % Update GUI: Plan params, Unique Targets, Plan Targets, Approved Targets
            app.updatePlanParams();
            app.showUniqueTargets();
            app.showPlanTargets();
            app.showApprovedTargets();

            % Clear plots
            if ~app.hasPlanner()
                app.clearPlots();
            end
        end
    end

    % =====================================================================    
    %                            Unique Targets    
    % =====================================================================
    methods (Access = public)
  
        function addUniqueTarget(app)
            % Add Unique-Target with addUniqTargets()
            app.msglog('addUniqueTarget');
            if ~app.hasPlanner(), return; end            
            if app.isReadOnlyMsg(), return; end

            % Create app
            if isempty(app.AddUniqueTargetApp) || ~isvalid(app.AddUniqueTargetApp)
                app.AddUniqueTargetApp = ultrasat.planner.gui.AddUniqueTarget(app.MainModule);                
            end

            % Show app
            if strcmp(app.showModal(app.AddUniqueTargetApp), 'Add')
                try
                    % Get field values
                    Name = app.MainModule.getFieldUniqueTargetName( app.AddUniqueTargetApp.NameEditField.Value );
                    RA = app.MainModule.getFieldRA( app.AddUniqueTargetApp.RAEditField.Value );
                    Dec = app.MainModule.getFieldDec( app.AddUniqueTargetApp.DecEditField.Value );
    
                    % Add to Planner
                    app.MainModule.Planner.addUniqTargets(RA, Dec, 'Name', Name);
                    app.setModified('addUniqueTarget');

                    % Refresh table
                    app.showUniqueTargets();
                catch ME
                    app.msgex('addUniqueTarget', ME);
                end
            end
        end


        function editUniqueTarget(app)
            % Edit Unique-Target with editUniqTarg()
            app.msglog('editUniqueTarget');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            % Get index of selected Unique Target
            Index = app.UITableUniqueTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end

            % Create app            
            if isempty(app.UniqueTargetParamsApp) || ~isvalid(app.UniqueTargetParamsApp)
                app.UniqueTargetParamsApp = ultrasat.planner.gui.UniqueTargetParams(app.MainModule);                
            end
            
            % Set field values - Currently there are 9 fields for Unique Target
            ParamsApp = app.UniqueTargetParamsApp;
            UniqTarg = app.MainModule.Planner.UniqTarg;
            app.setUniqueTargetParamsFields(UniqTarg, Index, ParamsApp);

            % Show the form, update values if closed with Save
            if strcmp(app.showModal(app.UniqueTargetParamsApp), 'Save')
                Name = app.MainModule.getFieldUniqueTargetName( ParamsApp.NameEditField.Value );
                RA = app.MainModule.getFieldRA( ParamsApp.RAEditField.Value );
                Dec = app.MainModule.getFieldDec( ParamsApp.DecEditField.Value );
                app.setModified('editUniqueTarget');
                try
                    app.MainModule.Planner.editUniqTarg(Index, 'Name', Name, 'RA', RA, 'Dec', Dec);
                    if app.checkPlanSelfConsistency()
                        app.msglog('editUniqueTarget successfully');
                    end
                catch ME
                    app.msgex('editUniqTarget', ME);
                end
                app.showPlanAll();
            end            
        end


        function setUniqueTargetParamsFields(app, UniqTarg, Index, ParamsApp)           
            % Helper: Set field values - Currently there are 9 fields for Unique Target
            ParamsApp.UniqueTargetIndexEditField.Value = int2str(Index);
            ParamsApp.NameEditField.Value = UniqTarg.Name(Index);
            ParamsApp.RAEditField.Value = app.MainModule.ra2Str( UniqTarg.RA(Index) );
            ParamsApp.DecEditField.Value = app.MainModule.dec2Str( UniqTarg.Dec(Index) );
            ParamsApp.A_UEditField.Value = app.MainModule.num2Str( UniqTarg.A_U(Index) );
            ParamsApp.CalObjEditField.Value = app.MainModule.length2Str( UniqTarg.CalObj(Index) );
            ParamsApp.RefImagesIDsEditField.Value = app.MainModule.length2Str( UniqTarg.RefImageIDs(Index) );
            ParamsApp.ExtSurveysEditField.Value = app.MainModule.length2Str( UniqTarg.ExtSurveys(Index) );
            ParamsApp.FieldObjEditField.Value = app.MainModule.length2Str( UniqTarg.FieldObj(Index) );
            ParamsApp.HealpixArrayEditField.Value = app.MainModule.length2Str( UniqTarg.HealpixArray(Index) );
        end        


        function deleteUniqueTarget(app)
            % Delete Unique-Target with delUniqTarg()
            app.msglog('deleteUniqueTarget');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            % Get index of selected Unique Target
            Index = app.UITableUniqueTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end

            % Ask user to confirm
            Name = app.MainModule.Planner.UniqTarg(Index);
            if ~strcmp(app.AppUtils.askYesNo(sprintf('Delete selected unique target (%s)?', Name)), 'Yes')
                return;
            end

            app.setModified('deleteUniqueTarget');
            try
                % Try to delete unique target, catch exception if it is
                % being used in the plan
                app.MainModule.Planner.delUniqTarg(Index, 'abort_if_in_plan', true);
            catch ME
                % Unqique target is being used in plan, ask user to confirm
                app.msgex('delUniqTarg', ME);
                if ~strcmp(app.AppUtils.askYesNo(sprintf('Unique target is used, deleting it will delete plan targets. Are you sure (%s)?', Name), 'Confirm'), 'Yes')
                    return;
                end                
            end

            % Force deleting the unique target and all targets that use it
            try
                app.MainModule.Planner.delUniqTarg(Index, 'abort_if_in_plan', false);
            catch ME
                app.msgex('delUniqTarg', ME);
            end
            app.showPlanAll();
        end


        function loadUniqueTargetsFromFile(app)
            % Load unique targets list from text file (csv). Open dialog to
            % ask user for file name or paste the text.
            app.msglog('loadUniqueTargetsFromFile');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            % Create app and set initial values from preferences
            if isempty(app.LoadUniqueTargetsFromFileApp) || ~isvalid(app.LoadUniqueTargetsFromFileApp)
                app.LoadUniqueTargetsFromFileApp = ultrasat.planner.gui.LoadUniqueTargetsFromFile(app.MainModule);                
                if ~isempty(app.Preferences.UniqueTargetsFileName)
                    app.LoadUniqueTargetsFromFileApp.FileNameEditField.Value = app.Preferences.UniqueTargetsFileName;
                    app.LoadUniqueTargetsFromFileApp.Folder = app.Preferences.UniqueTargetsFolder;
                end
            end

            % Show app
            if strcmp(app.showModal(app.LoadUniqueTargetsFromFileApp), 'Load')
                app.showPleaseWait('Loading unique targets...');
                try
                    % Write loaded/edited text from dialog to tempfile
                    Text = app.LoadUniqueTargetsFromFileApp.Text;
                    FileName = tempname;
                    fid = fopen(FileName, 'w');
                    fwrite(fid, Text);
                    fclose(fid);

                    % Load data from tempfile
                    % NOTE: readtable() expects uniform data types within each column, and it 
                    % might misinterpret the file structure.                    
                    Data = [];                    
                    if ~isempty(FileName) && isfile(FileName)
                        Data = readtable(FileName);
                    elseif ~isempty(Text)
                        Data = app.MainModule.loadTableFromCsvText(FileName);
                    end

                    % Add the loaded unique targets to planner
                    if ~isempty(Data)
                        app.MainModule.Planner.addUniqTargets(Data.RA, Data.Dec, 'Name', Data.Name);
                        app.setModified('loadUniqueTargetsFromFile');
                        app.showUniqueTargets();
                        app.setStatus('OK', 'Unique targets loaded successfully');

                        % Update preferences
                        app.Preferences.UniqueTargetsFileName = app.LoadUniqueTargetsFromFileApp.FileNameEditField.Value;
                        app.Preferences.UniqueTargetsFolder = fileparts(app.LoadUniqueTargetsFromFileApp.FileNameEditField.Value);
                        app.savePreferences();
                    end
                catch ME
                    app.msgex('loadUniqueTargetsFromFile', ME);
                end

                app.closePleaseWait();
            end
        end


        function saveUniqueTargetsToFile(app)
            % Save unique targets list to text file (csv). Open dialog to
            % ask user for file name or paste the text.
            app.msglog('saveUniqueTargetsToFile');
            if ~app.hasPlanner(), return; end            

            % Create app
            if isempty(app.SaveUniqueTargetsToFileApp) || ~isvalid(app.SaveUniqueTargetsToFileApp)
                app.SaveUniqueTargetsToFileApp = ultrasat.planner.gui.SaveUniqueTargetsToFile(app.MainModule);                

                if ~isempty(app.Preferences.UniqueTargetsFolder)
                    app.SaveUniqueTargetsToFileApp.Folder = app.Preferences.UniqueTargetsFolder;                    
                end                
            end

            % Save to temp file and load as text, display in the dialog
            try
                % Set auto-generated file name
                app.SaveUniqueTargetsToFileApp.FileNameEditField.Value = fullfile(app.SaveUniqueTargetsToFileApp.Folder, datestr(datetime('now', 'TimeZone', 'UTC'), 'yyyy-mm-dd_HH-MM-SS.txt'));

                TempFile = [tempname, '.txt'];
                app.MainModule.Planner.saveUniqTargCooList(TempFile);
                Text = fileread(TempFile);
                app.SaveUniqueTargetsToFileApp.TextArea.Value = Text;
            catch ME
                app.msgex('saveUniqTargCooList', ME);
            end                

            % Show app
            if strcmp(app.showModal(app.SaveUniqueTargetsToFileApp), 'Save')
                try
                    % Get field values
                    FileName = app.SaveUniqueTargetsToFileApp.FileName;
                    if ~isempty(FileName)
                        app.MainModule.Planner.saveUniqTargCooList(FileName);

                        % Update preferences
                        app.Preferences.UniqueTargetsFolder = fileparts(app.SaveUniqueTargetsToFileApp.FileNameEditField.Value);
                        app.savePreferences();                        
                    end
                catch ME
                    app.msgex('saveUniqueTargetsToFile', ME);
                end
            end
        end


        function clearUniqueTargets(app)
            % CLEAR ALL Unique-Targets with clearUniqueTargets()
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            % Ask user to confirm, should we ask again???
            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to delete ALL UNIQUE TARGETS ???', 'Delete all unique targets'), 'Yes')
                return;
            end

            try
                app.MainModule.Planner.clearUniqueTargets();
                app.showUniqueTargets();
            catch ME
                app.msgex('clearUniqueTargets', ME)
            end           
            app.showPlanAll();           
        end


        function uniqueTargetSelected(app, Index)
            % Helper: Called on Unique Target selection in table - @Todo
            Data = app.getSelectedTableRowAsStruct(app.MainModule.Planner.UniqTarg, Index);
            if ~isempty(Data)
                app.msglog(sprintf('uniqueTargetSelected: %d - %s', Index, Data.Name));
            end
        end


        function showUniqueTargets(app)
            % Helper: Update the Unique Targets GUI table with data from Planner
            % Update the display of Unique Targets table
            app.msglog('showUniqueTargets');
            if ~app.hasPlanner()
                app.UITableUniqueTargets.Data = [];
                return; 
            end

            % Setup GUI table properties
            app.UITableUniqueTargets.SelectionType = "row";
            app.UITableUniqueTargets.Multiselect = "off";
            app.UITableUniqueTargets.RowName = "numbered";

            % Add 'Order' column
            Data = app.MainModule.Planner.UniqTarg;
            Data = app.MainModule.convertTableDatetimeToString(Data);
            Data = addvars(Data, repmat("", height(Data), 1), 'Before', 1, 'NewVariableNames', 'Order');

            % Currently unused - add column of checkboxes
            %Data = addvars(Data, false(height(Data), 1), 'Before', 1, 'NewVariableNames', 'Checked');

            % Make only the first column editable, others non-editable
            nColumns = width(Data); 
            editableArray = false(1, nColumns); 
            editableArray(1) = true; 
            app.UITableUniqueTargets.ColumnEditable = editableArray;

            % Apply style to the entire 'Order' column (first column)
            s = uistyle("BackgroundColor",[1 0.85 0.4]); % Light orange color
            addStyle(app.UITableUniqueTargets, s, "column", 1);

            % Set table data
            app.UITableUniqueTargets.Data = Data;

            % Update also the table in the window
            if ~isempty(Data)
                app.UITableUniqueTargets.ColumnName = Data.Properties.VariableNames; 
            end

            % Extract unique values from the 'obj' column of the table
            Values = unique(app.MainModule.Planner.UniqTarg.Name, 'stable');
            if isempty(Values)
                app.GraphPlotUniqueTargetDropDown.Items = {};
                app.GraphPlotUniqueTargetDropDown.Value = {};
            else
                % Set the dropdown items to these values
                SaveValue = app.GraphPlotUniqueTargetDropDown.Value;
                app.GraphPlotUniqueTargetDropDown.Items = string(Values);

                % Set selected item in GraphPlot area
                if ~isempty(SaveValue) && any(ismember(app.MainModule.Planner.UniqTarg.Name, SaveValue))
                    app.GraphPlotUniqueTargetDropDown.Value = SaveValue;
                else
                    app.GraphPlotUniqueTargetDropDown.Value = Values{1};
                end            
            end

            % Update also the table in the window
            if ~isempty(app.UniqueTargetsApp) && isvalid(app.UniqueTargetsApp)            
                app.copyUITable(app.UITableUniqueTargets, app.UniqueTargetsApp.UITable);
            end            
        end


        function uniqueTargetClick(app)
            % Called on Unique-Target selection (single click) in the table
            Index = app.UITableUniqueTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end            
        end


        function uniqueTargetDoubleClick(app)
            % Called on Unique-Target double-click in the table
            UniqueTargetIndex = app.UITableUniqueTargets.Selection;
            if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                return
            end

            % Update drop-down with unique target double-clicked
            Planner = app.MainModule.Planner;
            Value = Planner.UniqTarg.Name(UniqueTargetIndex);            
            app.GraphPlotUniqueTargetDropDown.Value = Value;
            app.plotGraphs();
        end        

    end

    % =====================================================================
    %                            Plan Targets
    % =====================================================================
    methods (Access = public)

        function editPlanTarget(app)
            % Edit plan target by editPlanRow()
            app.msglog('editPlanTarget');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end            

            % Get index of selectred plan target
            Index = app.UITablePlanTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end

            % Create app
            if isempty(app.PlanTargetParamsApp) || ~isvalid(app.PlanTargetParamsApp)
                app.PlanTargetParamsApp = ultrasat.planner.gui.PlanTargetParams(app.MainModule);                
            end

            % Set field values - Currently there are 23 fields for Plan Target
            ParamsApp = app.PlanTargetParamsApp;
            Plan = app.MainModule.Planner.Plan;
            app.setPlanTargetParamsFiels(Plan, Index, ParamsApp);          
            ParamsApp.setEditMode(false);

            % Show app
            if strcmp(app.showModal(app.PlanTargetParamsApp), 'Save')
                try
                    % Apply the paramters from the dialog to the plan
                    app.applyPlanTargetParams(Index, ParamsApp);
                catch ME
                    app.msgex('editPlanRow', ME);
                end

                % Refresh GUI
                app.showPlanAll();
                app.setModified('editPlanTarget');
            end            
        end
 

        function setPlanTargetParamsFiels(app, Plan, Index, ParamsApp)
            % Helper: Set field values - Currently there are 23 fields for Plan Target
            app.msglog('setPlanTargetParamsFiels');

            ParamsApp.PlanTargetIndexEditField.Value = int2str(Index);

            % Editable fields
            ParamsApp.ExposureTimeEditField.Value = seconds(Plan.ExpTime(Index));  % Numeric field
            ParamsApp.EpochsPerVisitEditField.Value = Plan.Nexposures(Index);
            app.MainModule.updateCheckboxesFromTiles(ParamsApp, Plan.Tiles(Index));

            % String fields
            ParamsApp.NameEditField.Value = Plan.Name(Index);
            
            % Integer fields (uint8 → convert to string)
            ParamsApp.UniqueTargetIndexEditField.Value = num2str(Plan.UniqTargInd(Index));
            ParamsApp.GroupEditField.Value = num2str(Plan.Group(Index));
             
            % Double fields (convert to string for display)
            ParamsApp.RAEditField.Value = app.MainModule.ra2Str( Plan.RA(Index) );
            ParamsApp.DecEditField.Value = app.MainModule.dec2Str( Plan.Dec(Index));
            ParamsApp.ExpectedRollEditField.Value = num2str(Plan.ExpectedRoll(Index));
            
            % Datetime fields (convert to string using date format)
            ParamsApp.StartTimeEditField.Value = app.MainModule.DateTime2Str(Plan.Tstart(Index));
            ParamsApp.EndTimeEditField.Value = app.MainModule.DateTime2Str(Plan.Tend(Index));
             
            % Double fields (convert to string)
            ParamsApp.MJDstartEditField.Value = num2str(Plan.JDstart(Index));
            ParamsApp.MJDendEditField.Value = num2str(Plan.JDend(Index));
             
            % Duration fields (convert to string)            
            ParamsApp.TotalDurationEditField.Value = char(Plan.TotalDuration(Index));
            ParamsApp.SlewTimeBeforeEditField.Value = char(Plan.SlewTimeBefore(Index));
                       
            % Logical fields (convert to "Yes" / "No" or "1"/"0")
            ParamsApp.NoCommEditField.Value = string(Plan.NoComm(Index)); % "true"/"false"
            ParamsApp.HardObsEditField.Value = string(Plan.HardObs(Index));
            
            % Double fields (convert to string)
            ParamsApp.MoonDistEditField.Value = num2str(Plan.MoonDist(Index));
            ParamsApp.SunDistEditField.Value = num2str(Plan.SunDist(Index));
            ParamsApp.EarthDistEditField.Value = num2str(Plan.EarthDist(Index));
            ParamsApp.ZodyEditField.Value = num2str(Plan.Zody(Index));
            ParamsApp.LimMagEditField.Value = num2str(Plan.LimMag(Index));
            
            % Cell array field (convert to comma-separated string for display)
            ParamsApp.OverlapTargetsEditField.Value = app.MainModule.cell2Str(Plan.OverlapTargets);
        end


        function applyPlanTargetParams(app, Index, ParamsApp)
            % Helper: Apply plan parameters from dialog to plan
            app.msglog('applyPlanTargetParams');
            try
                Plan = app.MainModule.Planner.Plan;

                % Get editable parameters and apply - Currently there are 3 editable paramters
                ExpTime = seconds(ParamsApp.ExposureTimeEditField.Value);
                Nexposures = ParamsApp.EpochsPerVisitEditField.Value;
                Tiles = app.MainModule.getTilesFromCheckboxes(ParamsApp);

                % Send editPlanRow() only the modified values
                if ExpTime == Plan.ExpTime(Index)
                    ExpTime = seconds(inf);
                end
                if Nexposures == Plan.Nexposures(Index)
                    Nexposures = [];
                end
                if strcmp(Tiles, Plan.Tiles(Index))
                    Tiles = [];
                end

                % Update plan target
                app.MainModule.Planner.editPlanRow(Index, 'ExpTime', ExpTime, 'Tiles', Tiles, 'Nexposures', Nexposures);  

                %
                if app.checkPlanSelfConsistency()
                    app.msglog('applyPlanTargetParams successfully');
                end                    
            catch ME
                app.msgex('applyPlanTargetParams', ME);
            end            
        end


        function deletePlanTarget(app)
            % Delete plan target with delPlanRow()
            app.msglog('deletePlanTarget');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end            

            Index = app.UITablePlanTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end

            % Ask user confirmation
            Name = sprintf('%d', Index);  % app.MainModule.Planner.UniqTarg(Index);
            if ~strcmp(app.AppUtils.askYesNo(sprintf('Delete selected target (%s)?', Name), 'Confirm'), 'Yes')
                return;
            end

            app.setModified('deletePlanTarget');
            try
                app.MainModule.Planner.delPlanRow(Index);
            catch ME
                app.msgex('delPlanRow', ME);
                %if ~strcmp(app.AppUtils.askYesNo(sprintf('Unique target is used, deleting it will delete plan targets. Are you sure (%s)?', Name)), 'Yes')
                %    return;
                %end                
            end
            app.showPlanAll();
        end


        function clearPlanTargets(app)
            % Clear all plan targets with clearPlan()
            app.msglog('clearPlanTargets');            
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end            

            % Ask user confirmation
            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to delete ALL TARGETS ???', 'Delete all targets'), 'Yes')
                return;
            end

            try
                app.MainModule.Planner.clearPlan();                
            catch ME
                app.msgex('clearPlanTargets', ME)
            end                       
            app.showPlanAll();
        end
     

        function showPlanTargets(app)
            % Update the display of Plan Targets table
            app.msglog('showPlanTargets');
            if ~app.hasPlanner()
                app.UITablePlanTargets.Data = [];
                return; 
            end

            app.UITablePlanTargets.SelectionType = "row";
            app.UITablePlanTargets.Multiselect = "off";            
            app.UITablePlanTargets.RowName = "numbered";

            Data = app.MainModule.Planner.Plan;
            Data = app.MainModule.convertTableDatetimeToString(Data);
            
            app.UITablePlanTargets.Data = Data;
            if ~isempty(Data)
                app.UITablePlanTargets.ColumnName = Data.Properties.VariableNames; 
            end

            % --- Apply text color styling to the 'ValidationStatus' column ---
            % Find the column index for 'ValidationStatus'
            colIdx = find(strcmp(Data.Properties.VariableNames, 'ValidationStatus'), 1);       
            if ~isempty(colIdx) % Ensure the column exists       
                % Apply styles row by row based on the ValidationStatus value
                for row = 1:height(Data)
                    status = string(Data{row, colIdx}); % Read status as string
                    style = app.MainModule.getValidationStatusStyle(status);
                    addStyle(app.UITablePlanTargets, style, "cell", [row, colIdx]);
                end
            end

            % Update also the table in the window
            if ~isempty(app.PlanTargetsApp) && isvalid(app.PlanTargetsApp)            
                app.copyUITable(app.UITablePlanTargets, app.PlanTargetsApp.UITable);            
            end            
        end


        function adjustGroupStartTime(app)
            % Adjust group of targets with adjustGroupStartTime()
            app.msglog('adjustGroupStartTime');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end            

            % Create app
            if isempty(app.AdjustGroupStartTimeApp) || ~isvalid(app.AdjustGroupStartTimeApp)
                app.AdjustGroupStartTimeApp = ultrasat.planner.gui.AdjustGroupStartTime(app.MainModule);                
            end

            % Prepae data
            Planner = app.MainModule.Planner;
            uniqueGroups = unique(app.MainModule.Planner.Plan.Group);
            groupItems = cellstr(string(uniqueGroups));
            groupItems = ['All'; groupItems];
            app.AdjustGroupStartTimeApp.GroupDropDown.Items = groupItems;

            % Enable/disable options according to the existance of Approved Targets list
            if height(Planner.MissionApprovedPlan) == 0
                app.AdjustGroupStartTimeApp.RelativeButton.Enable = 'off';
                app.AdjustGroupStartTimeApp.ShiftTimeButton.Value = true;
            else
                app.AdjustGroupStartTimeApp.RelativeButton.Enable = 'on';
                app.AdjustGroupStartTimeApp.RelativeButton.Value = true;
            end

            % Show app
            if strcmp(app.showModal(app.AdjustGroupStartTimeApp), 'OK')
                try
                    % Apply
                    GroupList = app.AdjustGroupStartTimeApp.GroupList;
                    if strcmp(app.AdjustGroupStartTimeApp.Mode, 'Relative')
                        app.msglog('adjustGroupStartTime: Relative');
                        app.MainModule.Planner.adjustGroupStartTime('GroupList', GroupList);
                    elseif strcmp(app.AdjustGroupStartTimeApp.Mode, 'Shift')
                        app.msglog('adjustGroupStartTime: ShiftTime');
                        app.MainModule.Planner.adjustGroupStartTime('GroupList', GroupList, 'ShiftTime', app.AdjustGroupStartTimeApp.ShiftTime);
                    elseif strcmp(app.AdjustGroupStartTimeApp.Mode, 'StartTime')
                        app.msglog('adjustGroupStartTime: NewStartTime');
                        app.MainModule.Planner.adjustGroupStartTime('GroupList', GroupList, 'NewStartTime', app.AdjustGroupStartTimeApp.StartTime);
                    end                   
                catch ME
                    app.msgex('adjustGroupStartTime', ME)
                end
                app.showPlanAll();
            end
        end


        function planTargetSelected(app, Index)
            % Called on plan target selection (single click)
            if ~app.hasPlanner(), return; end
            Data = app.getSelectedTableRowAsStruct(app.MainModule.Planner.Plan, Index);
            if ~isempty(Data)
                app.msglog(sprintf('planTargetSelected: %d - %s', Index, Data.Name));
                app.showOverriddenApprovedTargets(Index);
            end
        end


        function planRowClick(app)
            % Called on plan target selection (single click)
            Index = app.UITablePlanTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end            

            % Select the Unique-Target
            UniqueTargetIndex = app.MainModule.Planner.Plan.UniqTargInd(Index);
            app.UITableUniqueTargets.Selection = UniqueTargetIndex;            
        end

    
        function planRowDoubleClick(app)
            % Called on plan target double click
            Index = app.UITablePlanTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end                        

            % Select the Unique-Target
            UniqueTargetIndex = app.MainModule.Planner.Plan.UniqTargInd(Index);
            app.UITableUniqueTargets.Selection = UniqueTargetIndex;
            app.uniqueTargetDoubleClick();
            app.plotGraphs();
        end
    end

    % =====================================================================
    %                               Windows
    % =====================================================================
    methods (Access = public)    

        function showUniqueTargetsWindow(app)
            % Show separate window with Unique Targets table
            app.msglog('showUniqueTargetsWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.UniqueTargetsApp) || ~isvalid(app.UniqueTargetsApp)
                app.UniqueTargetsApp = ultrasat.planner.gui.UniqueTargets(app.MainModule);
            end
            app.UniqueTargetsApp.UIFigure.Visible = 'on';
            if ~isempty(app.UniqueTargetsApp) && isvalid(app.UniqueTargetsApp)            
                app.copyUITable(app.UITableUniqueTargets, app.UniqueTargetsApp.UITable);
            end
        end        
       

        function showPlanTargetsWindow(app)
            % Show separate window with Plan Targets table
            app.msglog('showPlanTargetsWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.PlanTargetsApp) || ~isvalid(app.PlanTargetsApp)
                app.PlanTargetsApp = ultrasat.planner.gui.PlanTargets(app.MainModule);
            end
            app.PlanTargetsApp.UIFigure.Visible = 'on';
            if ~isempty(app.PlanTargetsApp) && isvalid(app.PlanTargetsApp)            
                app.copyUITable(app.UITablePlanTargets, app.PlanTargetsApp.UITable);            
            end
        end                


        function showApprovedTargetsWindow(app)
            % Show separate window with Approved Targets table
            app.msglog('showApprovedTargetsWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.ApprovedTargetsApp) || ~isvalid(app.ApprovedTargetsApp)
                app.ApprovedTargetsApp = ultrasat.planner.gui.ApprovedTargets(app.MainModule);
            end
            app.ApprovedTargetsApp.UIFigure.Visible = 'on';
            if ~isempty(app.ApprovedTargetsApp) && isvalid(app.ApprovedTargetsApp)
                app.copyUITable(app.UITableApprovedTargets, app.ApprovedTargetsApp.UITable);            
            end
        end                


        function copyUITable(app, SourceUITable, TargetUITable)
            % Copies data, column names, editability settings, and styles from SourceUITable to TargetUITable
            
            % Copy table data
            TargetUITable.Data = SourceUITable.Data;
            
            % Copy column names
            TargetUITable.ColumnName = SourceUITable.ColumnName;
            
            % Copy column editability settings
            TargetUITable.ColumnEditable = SourceUITable.ColumnEditable;
            
            % Remove existing styles from TargetUITable
            removeStyle(TargetUITable);
            
            % Retrieve styles from SourceUITable and reapply them to TargetUITable
            styles = get(SourceUITable, 'StyleConfigurations');

            % addStyle(app.UITableApprovedTarget, Style, "row", Targets);

            % Apply styles to TargetUITable
            for i = 1:height(styles)
                addStyle(TargetUITable, styles.Style(i), string(styles.Target(i)), styles.TargetIndex{i});
            end
        end
        
    end

    % =====================================================================
    %                               Build
    % =====================================================================
    methods (Access = public)

        function build(app)
            % Build plan according to plan type, calls doBuild...() below
            app.msglog('build');            
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end            

            %
            app.MainModule.AfterBuild = height(app.MainModule.Planner.Plan) > 0;
            if app.MainModule.AfterBuild
                if ~strcmp(app.AppUtils.askYesNo('Build was already executed, this will override you existing plan. Are you sure you want to execute build?', 'Confirm'), 'Yes')
                    return;
                end
            end

            app.showPleaseWait('Building your plan...');
            try
                PlanType = app.MainModule.PlanType;
                app.MainModule.clearStatus();
                app.updateStatus();
                app.msglog(sprintf('build: PlanType: %s', PlanType));

                if strcmp(PlanType, 'HCS')
                    app.doBuildHCS();
                elseif strcmp(PlanType, 'LCS')
                    app.doBuildLCS();
                elseif strcmp(PlanType, 'DDT')
                    app.doBuildDDT();
                elseif strcmp(PlanType, 'TOO')
                    app.doBuildTOO();
                elseif strcmp(PlanType, 'AllSS')
                    app.doBuildAllSS();
                end                    

                % Set AfterBuild=true for all plan types except DDT
                if ~strcmp(PlanType, 'DDT') && ~isempty(app.MainModule.Planner.Plan)
                    app.MainModule.AfterBuild = true;
                end
            catch ME
                app.MainModule.setStatus('Error', ME.message);
                app.msgex('build', ME);
            end

            % Close the "Please Wait" dialog
            app.closePleaseWait();

            % Update display
            app.setModified('build');  % Move call to other place?
            app.updateStatus();
            app.showPlanTargets();                                
        end


        function setBuildStatus(app, Status)
            app.MainModule.PlanData.setStatus('BuildStatus', Status);
        end


        function doBuildHCS(app)
            % Helper: Build HCS
            app.msglog('doBuildHCS started');
            if ~app.hasPlanner(), return; end
            
            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = app.getUniqueTargetsIndexByOrderColumn(app.UITableUniqueTargets.Data);
            if numel(SelectedRows) ~= 1
                app.AppUtils.msgError('HCS requires single unique target');
                return;
            end

            upHCS = app.MainModule.Planner;            
            upHCS.buildHCS('HCS_UniqTarg', SelectedRows);
            app.addHistory('BuildHCS Ok');
            app.setBuildStatus('OK');
            app.MainModule.setStatus('OK', 'Build: self consistency: OK');
            %app.debugSave('upHCS.mat', upHCS);
            app.msglog('doBuildHCS done');
        end


        function doBuildLCS(app)
            % Helper: Build LCS
            app.msglog('doBuildLCS started');
            if ~app.hasPlanner(), return; end

            upLCS = app.MainModule.Planner;

            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = app.getUniqueTargetsIndexByOrderColumn(app.UITableUniqueTargets.Data);
            upLCS.buildLCS('TargetList', SelectedRows);
          
            app.addHistory('BuildLCS Ok');
            app.setBuildStatus('OK');
            %app.debugSave('upLCS.mat', upLCS);
            app.msglog('doBuildLCS done');
        end


        function doBuildDDT(app)
            % Helper: Build DDT
            app.msglog('doBuildDDT started');
            if ~app.hasPlanner(), return; end

            upDDT = app.MainModule.Planner;
            
            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = app.getUniqueTargetsIndexByOrderColumn(app.UITableUniqueTargets.Data);
            if isempty(SelectedRows)
                return;
            end

            % Create app
            if isempty(app.EnterStartTimeApp) || ~isvalid(app.EnterStartTimeApp)
                app.EnterStartTimeApp = ultrasat.planner.gui.EnterStartTime(app.MainModule);
            end            

            % Set start time field from the planner
            app.EnterStartTimeApp.GroupStartTimeEditField.Value = app.MainModule.DateTime2Str(app.MainModule.Planner.StartTime);

            % Extract the selected Unique Targets and show them in the dialog
            SelectedData = app.UITableUniqueTargets.Data(SelectedRows, :);
            Data = SelectedData(:, {'Order', 'Name', 'RA', 'Dec'});
            app.EnterStartTimeApp.UITable.Data = Data;
            if ~isempty(Data)
                app.EnterStartTimeApp.UITable.ColumnName = Data.Properties.VariableNames;
            end

            % Generate group number from current plan
            if ~isempty(upDDT.Plan.Group)
                Group = max(upDDT.Plan.Group) + 1;
            else
                Group = 1;
            end
            app.EnterStartTimeApp.GroupEditField.Value = num2str(Group);

            % Show app
            if strcmp(app.showModal(app.EnterStartTimeApp), 'OK')

                % Get start time
                StartTime = app.MainModule.getFieldDateTime(app.EnterStartTimeApp.GroupStartTimeEditField.Value);
                app.msglog(sprintf('doBuildDDT: StartTime: %s ....', StartTime));
                try
                    % This is the actual 'build' of DDT
                    upDDT.addDDT2Plan(SelectedRows, StartTime, 'Group', Group);
                    app.addHistory('addDDT2Plan Ok');
                    app.setStatus('OK', 'build: addDDT2Plan successfully');
                catch ME
                    app.msgex('addDDT2Plan', ME);
                end
            end

            %app.debugSave('upDDT.mat', upDDT);
            app.msglog('doBuildDDT done');
        end


        function doBuildTOO(app)
            % Helper: Build TOO - @Todo @Yossi
            app.msglog('doBuildTOO started');
            if ~app.hasPlanner(), return; end

            try
                upTOO = app.MainModule.Planner;
    
                Fields = upTOO.UniqTarg(1);
                upTOO.buildTOO('RA', Fields.RA, 'Dec', Fields.Dec, 'Name', HCS_fields.Name);   
                %app.debugSave('upTOO.mat', upTOO);

            catch ME
                app.msgex('doBuildTOO', ME);
            end                
            app.msglog('doBuildTOO done');
        end


        function doBuildAllSS(app)
            % Helper: Build AllSS - @Todo @Yossi
            app.msglog('doBuildAllSS started');
            if ~app.hasPlanner(), return; end

            try

            catch ME
                app.msgex('doBuildAllSS', ME);
            end                

            app.msglog('doBuildAllSS done');
        end        
        

        function showBuildStatusWindow(app)
            % Show window with last build status
            app.msglog('showBuildStatusWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.BuildStatusApp) || ~isvalid(app.BuildStatusApp)
                app.BuildStatusApp = ultrasat.planner.gui.BuildStatus(app.MainModule);
            end

            % Set fields and show the app
            %app.BuildStatusApp.setData(app.MainModule.BuildStatus);
            app.showModal(app.BuildStatusApp);
        end


        function showValidationStatusWindow(app)
            % Show window with validation status
            app.msglog('showValidationStatusWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.ValidationStatusApp) || ~isvalid(app.ValidationStatusApp)
                app.ValidationStatusApp = ultrasat.planner.gui.ValidationStatus(app.MainModule);
            end            

            % Retrieve validation history from metadata
            ValidationHistory = app.MainModule.PlanData.metadata.ValidationResponse;        
            if isempty(ValidationHistory)
                app.msglog('No validation history available.');
                return;
            end
        
            % Extract struct from cell array if needed, convert to struct array
            if iscell(ValidationHistory)
                ValidationHistory = [ValidationHistory{:}];
            end

            % Setup table
            app.ValidationStatusApp.UITable.SelectionType = "row";
            app.ValidationStatusApp.UITable.Multiselect = "off";            
            app.ValidationStatusApp.UITable.RowName = "numbered";

            app.ValidationStatusApp.UITableHistory.SelectionType = "row";
            app.ValidationStatusApp.UITableHistory.Multiselect = "off";            
            app.ValidationStatusApp.UITableHistory.RowName = "numbered";            


            % Show latest validation response (first item in history)
            Response = ValidationHistory(1);
            app.showValidationResponse(Response);
        
            % Convert history to table (only keeping validation_time and status)
            HistoryData = struct2table(ValidationHistory, 'AsArray', true);
            HistoryData = HistoryData(:, {'validation_time', 'status'});
        
            % Assign history data to UITableHistory
            app.ValidationStatusApp.UITableHistory.Data = HistoryData;
        
            % Set column names for UITableHistory
            if ~isempty(HistoryData)
                app.ValidationStatusApp.UITableHistory.ColumnName = HistoryData.Properties.VariableNames;
            end

            app.showModal(app.ValidationStatusApp);
        end
        

        function validationHistorySelected(app)
            % Updates the displayed validation response based on selected row in history
            try
                % Retrieve validation history
                ValidationHistory = app.MainModule.PlanData.metadata.ValidationResponse;

                % Extract struct from cell array if needed, convert to struct array
                if iscell(ValidationHistory)
                    ValidationHistory = [ValidationHistory{:}];
                end

                % Ensure selection index is valid
                selection = app.ValidationStatusApp.UITableHistory.Selection;                
                if isempty(ValidationHistory) || (selection < 1) || (selection > numel(ValidationHistory))
                    app.msglog('Invalid history selection.');
                    return;
                end
        
                % Retrieve the selected validation response
                Response = ValidationHistory(selection);
        
                % Update display
                app.showValidationResponse(Response);
            catch ME
                app.msgex('validationHistorySelected', ME);
            end
        end


        function showValidationResponse(app, Response)
            % Update Validation app with details from response
            try
                % Reset fields to avoid stale data
                app.ValidationStatusApp.StartedEditField.Value = Response.validation_time;
                app.ValidationStatusApp.ElapsedEditField.Value = '';
                app.ValidationStatusApp.StatusEditField.Value = Response.status;
                app.ValidationStatusApp.StatusEditField.BackgroundColor = app.MainModule.getValidationStatusBackgroundColor(Response.status);

                % Convert Response to JSON and HTML for display
                ResponseText = jsonencode(Response, 'PrettyPrint', true);
                app.ValidationStatusApp.TextArea.Value = ResponseText;
                Html = app.MainModule.jsonToHtml(Response);
                app.ValidationStatusApp.HTML.HTMLSource = Html;
        
                % Ensure targets exist in Response before converting to table
                if isfield(Response, 'task') && isfield(Response.task, 'targets') && ~isempty(Response.task.targets)
                    Data = struct2table(Response.task.targets, 'AsArray', true);
                    app.ValidationStatusApp.UITable.Data = Data;
        
                    % Update column names if data exists
                    if ~isempty(Data)
                        app.ValidationStatusApp.UITable.ColumnName = Data.Properties.VariableNames;
                    end

                    colIdx = find(strcmp(Data.Properties.VariableNames, 'status'), 1);       
                    if ~isempty(colIdx) % Ensure the column exists       
                        % Apply styles row by row based on the status value
                        for row = 1:height(Data)
                            status = string(Data{row, colIdx}); % Read status as string
                            style = app.MainModule.getValidationStatusStyle(status);
                            addStyle(app.UITablePlanTargets, style, "cell", [row, colIdx]);
                        end
                    end                    
                else
                    app.ValidationStatusApp.UITable.Data = [];
                end
        
            catch ME
                app.msgex('showValidationResponse', ME);
            end
        end
                


        function showSubmitStatusWindow(app)
            % Show window with submit status
            app.msglog('dshowSubmitStatusWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.SubmitStatusApp) || ~isvalid(app.SubmitStatusApp)
                app.SubmitStatusApp = ultrasat.planner.gui.SubmitStatus(app.MainModule);
            end            

            % Set fields and show the app            
            %app.SubmitStatusApp.setData(app.MainModule.SubmitStatus);
            app.showModal(app.SubmitStatusApp);
        end        
    end
    
    % =====================================================================
    %                           Approved Targets
    % =====================================================================
    methods (Access = public)

        function retrieveApprovedTargets(app)
            % Retreive the list of approved taregts from the backend
            % User must be connected to server and logged-in
            app.msglog('retrieveApprovedTargets');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            % If build has been already executed, make sure that user is
            % aware of the meaning of this operaion
            if app.MainModule.AfterBuild
                if ~strcmp(app.AppUtils.askYesNo('Retreiving approved targets after build may result in inconsistent plan. Are you sure you want to retreive mission approved targets?', 'Confirm'), 'Yes')
                    return;
                end
            end

            % uplanner uses ApiClient class to send request to backend
            try
                app.MainModule.Planner.retrieveMissionApprovedPlan();
            catch ME
                app.msgex('retrieveApprovedTargets', ME);
            end

            % Update GUI with updated list of targets
            app.showApprovedTargets();
        end


        function showApprovedTargets(app)
            % Update the GUI of Approved Targets table
            app.msglog('showApprovedTargets');
            if ~app.hasPlanner()
                app.UITableApprovedTargets.Data = [];
                return; 
            end

            % Set table properties
            app.UITableApprovedTargets.SelectionType = "row";
            app.UITableApprovedTargets.Multiselect = "off";            
            app.UITableApprovedTargets.RowName = "numbered";

            % Set table data from Planner
            Data = app.MainModule.Planner.MissionApprovedPlan;
            Data = app.MainModule.convertTableDatetimeToString(Data);            
            app.UITableApprovedTargets.Data = Data;
            if ~isempty(Data)
                app.UITableApprovedTargets.ColumnName = Data.Properties.VariableNames;
            end

            % Update title above the table with current date
            app.ApprovedTargetsPanel.Title = sprintf('Approved Targets: (%s - %s)  - Retrieved: %s', ...
                ultrasat.api.ModelBase.datetimeStr(app.MainModule.ApiClient.ApprovedTargetsStartTime), ...
                ultrasat.api.ModelBase.datetimeStr(app.MainModule.ApiClient.ApprovedTargetsEndTime), ...
                ultrasat.api.ModelBase.datetimeStr(app.MainModule.Planner.RetrivedMissionTime));

            % Update also the table in the window
            if ~isempty(app.ApprovedTargetsApp) && isvalid(app.ApprovedTargetsApp)
                app.copyUITable(app.UITableApprovedTargets, app.ApprovedTargetsApp.UITable);            
            end            
        end


        function clearApprovedTargets(app)
            % Clear the list of approved targets
            app.msglog('clearApprovedTargets');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            app.MainModule.Planner.clearMissionApprovedPlan();
            app.showPlanAll();
        end


        function approvedTargetSelected(app, Index)
            % Called on selecting (single click) approved target from table
            Data = app.getSelectedTableRowAsStruct(app.MainModule.Planner.MissionApprovedPlan, Index);
            if ~isempty(Data)
                app.msglog(sprintf('approvedTargetSelected: %d - %s', Index, Data.Name));
            end
        end        


        function showOverriddenApprovedTargets(app, PlanTargetIndex)
            % Update the display with list of approved targets

            app.showApprovedTargets();
            PlanTarget = app.getSelectedTableRowAsStruct(app.MainModule.Planner.Plan, PlanTargetIndex);
            if isempty(PlanTarget)
                return;
            end

            % Get list of overlap targets.
            % Planner.Plan.OverlapTargets contains list of indexes of 
            % overlapped targets, this is calculated by the Planner object.
            Targets = PlanTarget.OverlapTargets;
            if ~isempty(Targets)

                % Mark the rows in light red color - [1, 0.6, 0.6]
                Style = uistyle("BackgroundColor", [1, 0.6, 0.6]);
                addStyle(app.UITableApprovedTarget, Style, "row", Targets);
            
                % Scroll table to the selected row
                scroll(app.UITableApprovedTargets, "row", Targets(1));
            end
        end

    end

    % =====================================================================
    %                         Plan Params Window
    % =====================================================================
    methods (Access = public)
        
        function showPlanParamsWindow(app)
            % Show window with Plan Parameters
            app.msglog('showPlanParamsWindow');
            if ~app.hasPlanner(), return; end            

            Planner = app.MainModule.Planner;

            % Create app
            if isempty(app.PlanParamsApp) || ~isvalid(app.PlanParamsApp)
                app.PlanParamsApp = ultrasat.planner.gui.PlanParams(app.MainModule);
            end
            app.PlanParamsApp.UIFigure.Visible = 'on';
            ParamsApp = app.PlanParamsApp;            

            % Make the form read-only if plan is already built
            app.MainModule.AfterBuild = height(Planner.Plan);
            app.setPlanParamsFields(ParamsApp);

            % Show app
            if strcmp(app.showModal(ParamsApp), 'Save')
                % Apply the parameters
                try
                    % Do we need to call it? it is called from PlanParams's
                    % Save button @Todo
                    app.applyPlanParams(ParamsApp);
                catch ME
                    app.msgex('showPlanParamsWindow', ME);
                end                    
            end
        end


        function setPlanParamsFields(app, ParamsApp)
            % Helper: Set PlanParams app fields from current planner
            % Called from showPlanParamsWindow

            % Get the Planner instance from the main module
            PlanData = app.MainModule.PlanData;
            Planner = app.MainModule.Planner;
        
            % Assign values to UI fields
            ParamsApp.TitleEditField.Value = Planner.Title;
            ParamsApp.PlanTypeDropDown.Value = Planner.Type;
            ParamsApp.StartTimeEditField.Value = app.MainModule.DateTime2Str(Planner.StartTime);
            ParamsApp.EndTimeEditField.Value = app.MainModule.DateTime2Str(Planner.EndTime);
            ParamsApp.ExposureEditField.Value = num2str(seconds(Planner.Exptime));
            ParamsApp.EpochsPerVisitEditField.Value = num2str(Planner.DefEpochsPerVisit);
            
            % Assign tile checkboxes
            tileNumbers = '1234';
            checkBoxes = [ParamsApp.Tile1CheckBox, ParamsApp.Tile2CheckBox, ParamsApp.Tile3CheckBox, ParamsApp.Tile4CheckBox];
            
            for i = 1:length(tileNumbers)
                checkBoxes(i).Value = ismember(tileNumbers(i), char(Planner.Tiles));
            end
            
            % Assign folders and files
            ParamsApp.BaseDataDirEditField.Value = Planner.BaseDataDir;
            ParamsApp.CalSubDirEditField.Value = Planner.CalibDir;
            ParamsApp.CalObjFileEditField.Value = '@TODO';  %Planner.CalibObj;
        
            % Assign Unique Targets & Plan Targets
            ParamsApp.PlanTargetsEditField.Value = num2str(Planner.N_planTargets);
            ParamsApp.UniqueTargetsEditField.Value = num2str(Planner.N_uniqueTargets);
        
            % Assign Check Times
            ParamsApp.CheckStartTimeEditField.Value = app.MainModule.DateTime2Str( Planner.CheckTimes(1) );
            ParamsApp.CheckEndTimeEditField.Value = app.MainModule.DateTime2Str( Planner.CheckTimes(2) );
        
            % Assign System Parameters
            ParamsApp.FieldOfViewRadiusEditField.Value = num2str(Planner.Rfov);
            ParamsApp.TileReadTimeEditField.Value = num2str(seconds(Planner.FullTileReadTime));
            ParamsApp.SlewBufferEditField.Value = num2str(seconds(Planner.DefSlewBuffer));
        
            % Assign LCSTab Parameters
            ParamsApp.LcsDailyWindowStartTimeEditField.Value = char(Planner.DailyWindowStartTime);
            ParamsApp.LcsDailyWindowMaxDurationEditField.Value = char(Planner.DailyWindowMaxDuration);
        
            % Assign AllSkyTab Parameters
            ParamsApp.AllSkyDailyWindowStartTimeEditField.Value = app.MainModule.DateTime2Str(Planner.DailyWindowStartTime);
            ParamsApp.AllSkyDailyWindowMaxDurationEditField.Value = num2str(hours(Planner.DailyWindowMaxDuration));
            ParamsApp.AllSkyGalacticLatTresholdEditField.Value = Planner.AllSSHighLatThresh;

            % @Yossi @Todo ??
            ParamsApp.AllSkyLatVisitsEditField.Value = Planner.LowLatVisits;
            ParamsApp.AllSkyLowLatVisitsEditField.Value = Planner.HighLatVisits;
            ParamsApp.AllSkyHighGalacticLatDitherPatternDropDown.Value = num2str(Planner.DitherPattern);
        
            % Assign TOOTab Parameters
            ParamsApp.TooStartTimeEditField.Value = app.MainModule.DateTime2Str(Planner.TOOStartTime);
            ParamsApp.TooWindowDurationEditField.Value = num2str(hours(Planner.TOOWindowDuration));
        
            % Assign Mission Status Fields
            ParamsApp.PlanStatusEditField.Value = Planner.Status;
            ParamsApp.AstPlannerEditField.Value = Planner.AstPlanner;

            % Status text
            app.setStatusField(ParamsApp.BuildStatusEditField, PlanData.metadata.BuildStatus.Status, PlanData.metadata.BuildStatus.Status);
            app.setStatusField(ParamsApp.ValidationStatusEditField, PlanData.metadata.ValidationStatus.Status, PlanData.metadata.ValidationStatus.Status);
            app.setStatusField(ParamsApp.SubmitStatusEditField, PlanData.metadata.SubmitStatus.Status, PlanData.metadata.SubmitStatus.Status);

            % Status times
            ParamsApp.BuildTimeEditField.Value = app.MainModule.DateTime2Str(Planner.ScheduledTime);            
            ParamsApp.ValidationTimeEditField.Value = app.MainModule.DateTime2Str(Planner.ValidatedTime);
            ParamsApp.SubmitTimeEditField.Value = app.MainModule.DateTime2Str(Planner.SubmittedTime);           
        
            % Assign Mission Distance Constraints            
            ParamsApp.SunMinDistObsEditField.Value = num2str(Planner.ObsSunDist);
            ParamsApp.MoonMinDistObsEditField.Value = num2str(Planner.ObsMoonDist);
            ParamsApp.EarthMinDistObsEditField.Value = num2str(Planner.ObsEarthDist);
            
            ParamsApp.SunMinDistSlewEditField.Value = '@Todo';
            ParamsApp.MoonMinDistSlewEditField.Value = '@Todo';
            ParamsApp.EarthMinDistSlewEditField.Value = '@Todo';

            % Assign Plan Buttons
            ParamsApp.SaveButton.Enable = true;
            ParamsApp.CancelButton.Enable = true;
        end


        function applyPlanParams(app, ParamsApp)
            % Helper: Apply plan parameters in current planner from PlanParams app
            % Called from showPlanParamsWindow            
            try
                app.doApplyPlanParams(ParamsApp);
            catch ME
                app.msgex('applyPlanParams', ME);
            end
        end


        function doApplyPlanParams(app, ParamsApp)
            % Helper: Apply plan parameters in current planner from PlanParams app
            % Called from showPlanParamsWindow

            Planner = app.MainModule.Planner;
            %try
                % General parameters to all plan types
                Planner.Title = ParamsApp.TitleEditField.Value;
                
                % Start & End times
                app.setPlanStartEndTime(ParamsApp.StartTimeEditField.Value, ParamsApp.EndTimeEditField.Value);

                % Other general parameters
                Planner.DefEpochsPerVisit = ParamsApp.EpochsPerVisitEditField.Value;
                Planner.Exptime = app.MainModule.getFieldDuration(ParamsApp.ExposureEditField.Value);
                
                % Apply per-type parameters
                if strcmp(Planner.Type, 'LCS')
                    Planner.DailyWindowStartTime = app.MainModule.getFieldDuration(ParamsApp.LcsDailyWindowStartTimeEditField.Value);
                    Planner.DailyWindowMaxDuration = app.MainModule.getFieldDuration(ParamsApp.LcsDailyWindowMaxDurationEditField.Value);
                elseif strcmp(Planner.Type, 'AllSS')                
                    Planner.DailyWindowStartTime = app.MainModule.getFieldDateTime(ParamsApp.AllSkyDailyWindowStartTimeEditField.Value);
                    Planner.DailyWindowMaxDuration = app.MainModule.getFieldDuration(ParamsApp.AllSkyDailyWindowMaxDurationEditField.Value);
                    Planner.AllSSHighLatThresh = ParamsApp.AllSkyGalacticLatTresholdEditField.Value;
                    Planner.LowLatVisits = ParamsApp.AllSkyLatVisitsEditField.Value;

                    % Future
                    %Planner.= ParamsApp.AllSkyLowLatVisitsEditField.Value;
                    %Planner.= ParamsApp.AllSkyHighGalacticLatDitherPatternDropDown.Value;
                elseif strcmp(Planner.Type, 'TOO')                
                    Planner.TOOStartTime = app.MainModule.getFieldDuration(ParamsApp.TooStartTimeEditField.Value);
                    Planner.TOOWindowDuration = app.MainModule.getFieldDuration(ParamsApp.TooWindowMaxDurationEditField.Value);
                end        

                % Apply check times
                app.applyCheckTimes(ParamsApp);                

                % @Future: Apply system constants from ParamsApp

            %catch ME
            %    app.msgex('applyPlanParams', ME);
            %end
        end


        function setPlanStartEndTime(app, StartTimeValue, EndTimeValue)
            %
            app.msglog('setPlanStartEndTime')            
            if ~app.hasPlanner(), return; end            
            if app.isReadOnlyMsg(), return; end

            try
                StartTime = app.MainModule.getFieldDateTime(StartTimeValue);
                EndTime = app.MainModule.getFieldDateTime(EndTimeValue);

                Planner = app.MainModule.Planner;
                Planner.StartTime = StartTime;
                Planner.EndTime = EndTime;
            catch ME
                app.msgex('setPlanStartEndTime', ME);
            end
        end


        function applyCheckTimes(app, ParamsApp)
            % Helper: Update Planner.CheckTimes with values from the edit fields
            % Note: Called from applyPlanParams() above
            % Note: REMOVED: Called from PlanParams.CheckTimesUpdateButtonPushed()
            app.msglog('applyCheckTimes')            
            if ~app.hasPlanner(), return; end            
            if app.isReadOnlyMsg(), return; end

            app.showPleaseWait('Updating CheckTimes...');
            try                                
                Planner = app.MainModule.Planner;
                StartTime = app.MainModule.getFieldDateTime(ParamsApp.CheckStartTimeEditField.Value);
                EndTime = app.MainModule.getFieldDateTime(ParamsApp.CheckEndTimeEditField.Value);

                % Call adjustCheckTimes() only if values have been changed
                if StartTime ~= Planner.CheckTimes(1) || EndTime ~= Planner.CheckTimes(2)
                    Planner.adjustCheckTimes(StartTime, EndTime);
                end
            catch ME
                app.msgex('applyCheckTimes', ME);
            end
            app.closePleaseWait();
        end        
        

        function showPlanHistory(app)
            app.msglog('showPlanHistory');
            if ~app.hasPlanner(), return; end            

            % Create app
            if isempty(app.PlanHistoryApp) || ~isvalid(app.PlanHistoryApp)
                app.PlanHistoryApp = ultrasat.planner.gui.PlanHistory(app.MainModule);
            end

            % Todo - set the table
            if true
                History = app.MainModule.PlanData.history;
                Data = struct2table(History, 'AsArray', true);
                Data = app.MainModule.convertTableDatetimeToString(Data);
                app.PlanHistoryApp.UITable.Data = Data;
                if ~isempty(Data)
                    app.PlanHistoryApp.UITable.ColumnName = Data.Properties.VariableNames;
                end
            end            

            % Show app
            app.showModal(app.PlanHistoryApp);
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
                web('https://docs.google.com/document/d/e/2PACX-1vTQKjJmBjzmcSXwaIRsq3FviYYpsW-Of7fewwcavCErBG7Pg589j3viLrUmNIr-NM-EfRWfQI4n0PdE/pub', '-browser');
            else
                item = ['http://socsrv/soc/data/help/planner/', item, '.html'];
                web(item, '-browser');
            end
        end


        function showGDriveQA(app)
            % Open website in browser window, use system default browser (-browser)
            web('https://docs.google.com/document/d/1iXs3Z5SHNnA8vUEf557DT3qIo9_rqELBp_WYg2vrYi0/edit?usp=sharing', '-browser');
        end        


        function showSnrCalculator(app)
            % Open website in browser window, with out SNR Calculator, use system default browser (-browser)
            web('https://www.weizmann.ac.il/ultrasat/for-scientists/snr-calculator', '-browser');
            %web('https://snrapp.ultrasatsoc.org/');            
        end

        % =================================================================        


    end

    % =====================================================================
    %                               Plots
    % =====================================================================    
    methods (Access = public)

        function showSkyMapPlot(app)
            % Update GUI plot with SkyMap
            app.msglog('showSkyMapPlot');
            if ~app.hasPlanner(), return; end            

            % Future? @Todo @Yossi
            % Get index of selected item - Currently unused - @Yossi
            % UniqueTargetIndex = app.UITableUniqueTargets.Selection;
            % if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
            %     return
            % end
            try
                % Update the plot embedded in this window
                app.doPlotSkyMap(app.AxesSkymapPlot);

                % Update also the plot in the standalone window
                if ~isempty(app.PlotSkyMapApp) && isvalid(app.PlotSkyMapApp)
                    app.doPlotSkyMap(app.PlotSkyMapApp.AxesSkymapPlot);
                end
            catch ME
                app.msgex('plotMapPlan', ME);
            end   
        end

        
        function doPlotSkyMap(app, AxesHandle)
            % Plot SkyMap on the specified Axes (embedded or stand-alone)
            try
                Planner = app.MainModule.Planner;
                cla(AxesHandle, 'reset');                
                Planner.plotMapPlan('AxesHandle', AxesHandle, ...
                    'disp_uniqTarg', app.PlotFlagUniqueCheckBox.Value, ...
                    'disp_plan',  app.PlotFlagPlanCheckBox.Value, ...
                    'ExtinctionMap',   app.PlotFlagExtinctionCheckBox.Value, ...
                    'CalObjMap', app.PlotFlagCalibrationCheckBox.Value, ...
                    'disp_MissAprvPlan', app.PlotFlagApprovedCheckBox.Value, ...
                    'vis_at_time_map', app.PlotFlagVisibleCheckBox.Value);  % , ...
                    % 'cooSys', app.PlotCooSysDropDown.Value, ...
                    % 'plotTstart', app.MainModule.getFieldDateTime(app.PlotStartTimeEditField.Value), ...
                    % 'plotTend', app.MainModule.getFieldDateTime(app.PlotEndTimeEditField.Value) );
        
                    % Currently not implemented:
                    % In addition, there are 3 lists that can be set to select a subsample from 
                    % the full sample: 'UniqTargInds', 'plan_rows', 'MissAprvPlan_rows'.
                    % If they are empty (the default) will plot the entire UniqTarg list / Plan / MissionApprovedPlan.                
            catch ME
                app.msgex('doPlotSkyMap', ME);
            end                   
        end


        function showSkyMapPlotWindow(app)
            % Show stand-alone window with SkyMap plot, the user need to
            % click teh Update button in the embedded plot in this 
            app.msglog('plotCalibObj');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.PlotSkyMapApp) || ~isvalid(app.PlotSkyMapApp)
                app.PlotSkyMapApp = ultrasat.planner.gui.PlotSkyMap(app.MainModule);                
            end
            app.PlotSkyMapApp.UIFigure.Visible = 'on';
        end

        % =================================================================      
        function plotGraphs(app)
            % Plot CalibObj or Visibility according to selected radio button
            try
                if ~app.hasPlanner()
                    app.clearPlots();
                    return;
                end

                % Plot CalibObj
                if app.CalibrationStarButton.Value
                    app.plotCalibObj();
                end

                % Plot Visibility
                if app.VisibilityButton.Value
                    app.plotVisibility();
                end                
            catch ME
                app.msgex('plotCalibObj', ME);
            end                
        end


        function clearPlots(app)
            % Clear the SkyMap and Graphs plots, on this window and the standalone windows.
            app.msglog('clearPlots');
            cla(app.AxesSkymapPlot, 'reset');
            cla(app.AxesGraphsPlot, 'reset');

            if ~isempty(app.PlotSkyMapApp) && isvalid(app.PlotSkyMapApp)
                cla(app.PlotSkyMapApp.AxesSkymapPlot, 'reset');
            end

            if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
            end
        end


        function plotCalibObj(app)
            % Plot Calibration Objects graph
            app.msglog('plotCalibObj');
            if ~app.hasPlanner(), return; end

            Planner = app.MainModule.Planner;

            % Get index of selected unique target in the drop-down
            UniqueTargetIndex = find(strcmp(app.GraphPlotUniqueTargetDropDown.Value, app.GraphPlotUniqueTargetDropDown.Items));

            % Get index of selected item from DropDown
            % app.GraphPlotUniqueTargetDropDown.Value = Planner.UniqTarg.Name(UniqueTargetIndex);

            %UniqueTargetIndex = app.UITableUniqueTargets.Selection;
            %if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
            %    return
            %end

            try
                % Get table of CalibObj, check that it is not empty
                % When calling showCalibObj('PlotSpectrum', false) the
                % function return table of CalibObj, and does not plot anything
                app.UniqueTargetCalibObj = Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', false);
                if isempty(app.UniqueTargetCalibObj) || height(app.UniqueTargetCalibObj) == 0
                    app.setStatus('Warning', 'showCalibObj returned none')
                    return
                end

                % Set selected value in Unique Targets drop-down (next to the plot)
                app.GraphPlotUniqueTargetDropDown.Value = Planner.UniqTarg.Name(UniqueTargetIndex);

                % Extract unique values from the 'obj' column of the table
                ObjValues = unique(app.UniqueTargetCalibObj.obj, 'stable');

                % Set the dropdown items to these values
                app.PlotCalibObjDropDown.Items = string(ObjValues);
                app.PlotCalibObjDropDown.Value = ObjValues{1};

                % Update the plot embedded in this window
                cla(app.AxesGraphsPlot, 'reset');                
                Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', 1, 'AxesHandle', app.AxesGraphsPlot);

                % Update also the plot in the standalone window
                if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                    cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');                
                    Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', 1, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot);
                end
            catch ME
                app.msgex('plotCalibObj', ME);
            end
        end


        function showCalibObjTable(app)
            % Create CalibObjTable window - app with table on it
            if isempty(app.CalibObjTableApp) || ~isvalid(app.CalibObjTableApp)
                app.CalibObjTableApp = ultrasat.planner.gui.CalibObjTable(app.MainModule);                
            end
            app.CalibObjTableApp.UIFigure.Visible = 'on';

            % Update the data in the table app
            if ~isempty(app.CalibObjTableApp)
                app.CalibObjTableApp.UITableData.Data = app.UniqueTargetCalibObj;
                if ~isempty(app.UniqueTargetCalibObj)
                    app.CalibObjTableApp.UITableData.ColumnName = app.UniqueTargetCalibObj.Properties.VariableNames;
                end
            end            
        end


        function uniqueTargetSelectedInPlot(app, UniqueTargetIndex)
            % Helper: 
            Planner = app.MainModule.Planner;

            %
            app.GraphPlotUniqueTargetDropDown.Value = Planner.UniqTarg.Name(UniqueTargetIndex);

            %
            app.UniqueTargetCalibObj = Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', false);
            if isempty(app.UniqueTargetCalibObj) || height(app.UniqueTargetCalibObj) == 0
                app.setStatus('Warning', 'showCalibObj returned none')
                return
            end

            if ~isempty(app.CalibObjTableApp)
                app.CalibObjTableApp.UITableData.Data = app.UniqueTargetCalibObj;
                app.CalibObjTableApp.UITableData.ColumnName = app.UniqueTargetCalibObj.Properties.VariableNames;
            end

            % Extract unique values from the 'obj' column of the table
            ObjValues = unique(app.UniqueTargetCalibObj.obj, 'stable');

            % Set the dropdown items to these values
            app.PlotCalibObjDropDown.Items = string(ObjValues);
            app.PlotCalibObjDropDown.Value = ObjValues{1};            
        end


        function plotCalibObjSub(app)
            % Called on selecting CalibObj in the drop-down next to the Graphs plot
            try
                if ~isempty(app.MainModule.Planner)
                    UniqueTargetIndex = app.UITableUniqueTargets.Selection;
                    if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                        return
                    end

                    % Update the plot embedded in this window
                    Value = app.PlotCalibObjDropDown.Value;
                    CalObjIndex = find(strcmp(app.UniqueTargetCalibObj.obj, Value));

                    % Update the plot embedded in this window
                    cla(app.AxesGraphsPlot, 'reset');                
                    app.MainModule.Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', CalObjIndex, 'AxesHandle', app.AxesGraphsPlot);
    
                    % Update also the plot in the standalone window
                    if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                        cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');                
                        app.MainModule.Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', CalObjIndex, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot);
                    end
                end
            catch ME
                app.msgex('plotCalibObjSub', ME);
            end                            
        end


        function plotVisibility(app)
            % Plot Visibility graph of currently select Unique Target
            if ~app.hasPlanner(), return; end
    
            Planner = app.MainModule.Planner;

            % Get index of selected unique target in the drop-down
            UniqueTargetIndex = find(strcmp(app.GraphPlotUniqueTargetDropDown.Value, app.GraphPlotUniqueTargetDropDown.Items));

            % Get index of selected item
            %UniqueTargetIndex = app.UITableUniqueTargets.Selection;
            %if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
            %    return
            %end

            try
                % Update the plot embedded in this window
                cla(app.AxesGraphsPlot, 'reset');                
                Planner.plotVisibility(UniqueTargetIndex, 'AxesHandle', app.AxesGraphsPlot);

                % Update also the plot in the standalone window
                if ~isempty(app.PlotGraphsApp)
                    cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');                
                    Planner.plotVisibility(UniqueTargetIndex, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot);
                end
            catch ME
                app.msgex('plotVisibility', ME);
            end
        end        


        function showGraphsPlotWindow(app)
            % Create app
            if isempty(app.PlotGraphsApp) || ~isvalid(app.PlotGraphsApp)
                app.PlotGraphsApp = ultrasat.planner.gui.PlotGraphs(app.MainModule);                
            end
            app.PlotGraphsApp.UIFigure.Visible = 'on';            
        end

    end

    % =====================================================================
    %                               
    % =====================================================================
    methods (Access = public)

        function updatePlanParams(app)
            % Helper: Update fields in top panel of with window with values from Plan parameters
            app.msglog('updatePlanParams');
            if ~app.hasPlanner(), return; end

            % Set fields
            Planner = app.MainModule.Planner;
            app.PlanTypeEditField.Value = Planner.Type;
            app.UserNameEditField.Value = Planner.AstPlanner;
            app.PlanTitleEditField.Value = Planner.Title;
            app.StartTimeEditField.Value = app.MainModule.DateTime2Str(Planner.StartTime);
            app.EndTimeEditField.Value = app.MainModule.DateTime2Str(Planner.EndTime);

            if app.isReadOnly()
                app.StartTimeEditField.Editable = "off";
                app.EndTimeEditField.Editable = "off";
                app.PlanTitleEditField.Editable = "off";
            else
                app.StartTimeEditField.Editable = "on";
                app.EndTimeEditField.Editable = "on";                
                app.PlanTitleEditField.Editable = "on";                
            end

            % Show message if plan was already submitted and cannot be modified
            if strcmp(Planner.Status, 'submitted')
                app.setTopLabel('The plan was submitted and cannot be modified.', [0.00,0.00,1.00], [1.00,1.00,0.07]);
            else
                app.setTopLabel('', [], []);
            end
        end

        
        function Result = checkPlanSelfConsistency(app)
            % Check plan for self consistency, update status display
            app.msglog('checkPlan')
            Result = false;
            try
                % Perform the check
                if height(app.MainModule.Planner.Plan) > 0
                    CheckStatus = app.MainModule.Planner.planSelfConsistencyCheck();
                end

                % Update display with status
                if CheckStatus
                    app.MainModule.setStatus('OK', 'self consistency: OK');
                    Result = true;
                else
                    app.MainModule.setStatus('Error', 'self consistency: issues found');
                end
            catch ME
                app.msgex('planSelfConsistencyCheck failed', ME);
                app.MainModule.setStatus('Error', sprintf('self consistency: exception: %s', ME.message));
            end            
        end

    end

    % =====================================================================
    %                           Validate % Submit    
    % =====================================================================
    methods (Access = public)    

        function validate(app)
            % Validate plan by sending it to the Validation service
            app.msglog('validate');
            if ~app.hasPlanner(), return; end            
            if app.isReadOnlyMsg(), return; end            
            if ~app.isLogin('Message', true), return; end
            
            % Ask user to confirm - currently not
            %if ~strcmp(app.AppUtils.askYesNo('Send plan with GCS Validator?', 'Confirm'), 'Yes')
            %    return;
            %end

            app.showPleaseWait('Validating your plan...');
            try
                app.MainModule.PlanData.addHistory('validation started');
                app.MainModule.Planner.validate();
                app.MainModule.PlanData.setStatus('ValidationStatus', 'OK');
            catch ME
                app.msgex('validate', ME);
            end
            app.closePleaseWait();
            app.updateStatus();
            app.AppUtils.msgOk('Validation completed, see detailed status in validation report window.')

            % User will open the status window
            % app.showValidationStatusWindow();
        end        


        function updateValidateStatus(app)
            % Update the validation status field
            if ~isempty(app.MainModule.Planner)
                app.setStatusField(app.ValidationShortStatusEditField, app.MainModule.ValidateStatus, app.MainModule.ValidateStatusText);
                app.setStatusField(app.ValidationTimeEditField, app.MainModule.ValidateStatus, app.MainModule.datetimeStr(app.MainModule.Planner.ValidatedTime));
            end

            % 
        end


        function submit(app)
            % Submit plan by sending it to Mission Control service
            % Debug: see files in D:\Ultrasat\AstroPack\matlab\astro\+ultrasat\+api\sim
            app.msglog('submit');
            if ~app.hasPlanner(), return; end            
            if app.isReadOnlyMsg(), return; end            
            if ~app.isLogin()
                return;
            end

            % Must save before submit, because backend need to access the
            % plan in the database.
            if app.MainModule.Modified
                if ~strcmp(app.AppUtils.askYesNo('The plan has been modified and not saved. You must save it before submitting. Do you want to save your changes?', 'Confirm'), 'Yes')
                    return;
                end                
            end
            app.savePlan();

            if ~strcmp(app.MainModule.Planner.Status, 'validated')
                if ~strcmp(app.AppUtils.askYesNo('The plan is not validated, or validation was not successful. Are you sure you want to submit this plan?', 'Confirm'), 'Yes')
                    return;
                end                
            end

            % Ask user for confirmation
            if ~strcmp(app.AppUtils.askYesNo('Submit this plan to Mission Control? Are you sure?', 'Confirm'), 'Yes')
                return;
            end

            app.showPleaseWait('Submitting your plan...');
            try
                % Send submit request to backend, uplanner.submit() calls
                % MissionClient.submitPlan().
                % After submit the plan should become read-only.
                app.MainModule.Planner.submit();
                app.MainModule.PlanData.setStatus('SubmitStatus', 'OK');
                app.addHistory('submit');
                app.setReadOnly(true);
            catch ME
                app.msgex('submit', ME);
            end
            app.closePleaseWait();
            app.updateStatus();
        end        
    end

    % =====================================================================
    %                               Status
    % =====================================================================    

    methods (Access = public)            

        function applyPlanStatus(app)
            % Helper:
            if app.hasPlanner()
                if strcmp(app.MainModule.Planner.Status, 'Submitted')
                    app.setReadOnly(true);
                else
                    app.setReadOnly(false);
                end
            else
            end
        end


        function setReadOnly(app, ReadOnly)
            % Helper: Setc/clear read-only status of the current plan
            app.AllowEdit = ~ReadOnly;            
        end


        function Result = isReadOnly(app)
            % Helper: Return true if currently in read-only mode
            Result = ~app.AllowEdit;
        end

        
        function Result = isReadOnlyMsg(app)
            % Helper: Return true if currently in read-only mode, show popup message
            Result = ~app.AllowEdit;
            if Result
                uialert(app.UIFigure, sprintf('Plan is read-only: %s', app.AllowEditMsg), 'Message', 'Icon', 'success');                            
            end
        end        


        function setModified(app, logText)
            % Helper: Mark the plan as modified (i.e. required to be saved/discarded)
            % if ~app.MainModule.Modified
                if nargin < 2 || isempty(logText)
                    logText = '';
                end
                app.msglog(sprintf('setModified: modified - %s', logText));
                app.MainModule.setModified();
                app.ModifiedLabel.Text = 'Modified';

                app.SaveButton.Enable = 'on';
            % end
        end


        function clearModified(app)
            % Helper: Clear the Modified flag and status
            if app.MainModule.Modified
                app.msglog('clearModified')
            end            
            app.MainModule.clearModified();
            app.ModifiedLabel.Text = '';
            app.SaveButton.Enable = 'off';
        end


        function Result = needSave(app, AskSave)
            % Helper: Check if current plan has been modified and need to be saved
            if app.MainModule.Modified
                if AskSave
                    if strcmp(app.AppUtils.askYesNo('Your changes are not saved. Save or discard?', 'Save or discard'), 'Yes')
                        try
                            app.savePlan();
                        catch ME
                            app.msgex('needSave', ME);
                        end
                        Result = true;
                    else
                        Result = true;
                    end
                else
                    Result = true;
                end
            else
                Result = false;
            end
        end


        function setStatus(app, Status, Text)
            % Helper: Update the status panel with new status
            app.msglog(sprintf('setStatus: %s - %s', Status, Text));
            app.MainModule.setStatus(Status, Text);
            app.updateStatus();
        end


        function setStatusEx(app, Title, ME)
            % Helper: Update the status panel with exception message
            app.MainModule.setStatus('Error', sprintf('%s - %s', Title, ME.message));
            app.updateStatus();
        end


        function updateStatus(app)
            % @Todo - ??

            app.setStatusField(app.StatusTextArea, app.MainModule.CurrentStatus, app.MainModule.StatusText);

            PlanData = app.MainModule.PlanData;
            Planner = app.MainModule.Planner;

            % Planner is not empty, set fields
            if ~isempty(Planner)
                app.BuildTimeEditField.Value = app.MainModule.DateTime2Str(Planner.ScheduledTime);
                app.ValidationTimeEditField.Value = app.MainModule.DateTime2Str(Planner.ValidatedTime);
                app.SubmitTimeEditField.Value = app.MainModule.DateTime2Str(Planner.SubmittedTime);

                %
                app.setStatusField(app.BuildShortStatusEditField, PlanData.metadata.BuildStatus.Status, PlanData.metadata.BuildStatus.Status);
                app.setStatusField(app.ValidationShortStatusEditField, PlanData.metadata.ValidationStatus.Status, PlanData.metadata.ValidationStatus.Status);
                app.setStatusField(app.SubmitShortStatusEditField, PlanData.metadata.SubmitStatus.Status, PlanData.metadata.SubmitStatus.Status);
                
            % Planner is empty, clear fields
            else
                app.BuildTimeEditField.Value = '';
                app.ValidationTimeEditField.Value = '';
                app.SubmitTimeEditField.Value = '';

                app.setStatusField(app.BuildShortStatusEditField, '', '');
                app.setStatusField(app.ValidationShortStatusEditField, '', '');
                app.setStatusField(app.SubmitShortStatusEditField, '', '');                
            end

            if strcmp(Planner.Status, 'submitted')
                app.setTopLabel('The plan was submitted and cannot be modified.', [0.00,0.00,1.00], [1.00,1.00,0.07]);
            else
                app.setTopLabel('', [], []);
            end            
        end


        function setStatusField(app, EditField, Status, StatusText)
            % Helper: Set the background color of the EditField based on the Status value.
            % Valid values for Status: OK, Warning, Error, (empty)

            if isempty(StatusText)
                StatusText = '';
            end
            EditField.Value = StatusText;

            % Logic for background color
            if strcmp(Status, 'OK')
                % Light gray-green for 'OK'
                EditField.BackgroundColor = [0.8, 0.9, 0.8];
            elseif strcmp(Status, 'Warning')
                % Light yellow for 'Warning'
                EditField.BackgroundColor = [1.0, 1.0, 0.8];                
            elseif ~isempty(Status)
                % Light red for non-empty status that is not 'OK'
                EditField.BackgroundColor = [1, 0.8, 0.8];
            else
                % Light gray for empty status
                EditField.BackgroundColor = [0.9, 0.9, 0.9];
            end
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


        function Result = getUniqueTargetsIndexByOrderColumn(app, Data)
            % Returns the row indices sorted by 'Order' column.
            % If only one row exists, returns 1.
            % If only one row has a non-empty 'Order' value, returns its index.
            % Otherwise, returns indices of rows with non-empty 'Order', sorted by value.
        
            % If only one row in the table, return index 1
            if height(Data) == 1
                Result = 1;
                return;
            end
        
            % Convert to string array for uniform processing
            OrderColumn = string(Data.Order);
        
            % Identify non-empty rows (ignoring whitespace and empty strings)
            trimmedOrder = strtrim(OrderColumn);
            isValid = ~(trimmedOrder == "" | trimmedOrder == " ");
            
            % If only one valid row with non-empty 'Order', return its index
            if sum(isValid) == 1
                Result = find(isValid);
                return;
            end
        
            % Handle case: all values are invalid or non-numeric
            validNumbers = str2double(trimmedOrder(isValid));
            if all(isnan(validNumbers))
                OrderColumn = string(1:height(Data))';
                trimmedOrder = OrderColumn;
                isValid = true(height(Data), 1);
            end
            
            % Now safely convert all to numbers, keeping invalid as NaN
            numericOrder = NaN(height(Data), 1);
            numericOrder(isValid) = str2double(trimmedOrder(isValid));
            
            % Get non-empty rows and sort
            nonEmptyRows = find(~isnan(numericOrder));
            [~, sortedIdx] = sort(numericOrder(nonEmptyRows));
            Result = nonEmptyRows(sortedIdx)';          
        end


        function Result = getUniqueTargetsIndexByOrderColumn0(app, Data)
            % Extract row indices for non-empty 'Order' values, sorted by 'Order'.
            % If only one row exists, returns 1.
            % If only one row has a non-empty 'Order' value, returns its index.
            % Otherwise, returns indices of rows with non-empty 'Order', sorted by value.
            
            % If only one row in the table, return index 1
            if height(Data) == 1
                Result = 1;
                return;
            end

            % Check if all values in 'Order' column are empty and replace with row numbers if needed
            if all(cellfun(@(x) isempty(strtrim(x)), Data.Order)) || all(isnan(str2double(Data.Order)))
                Data.Order = string(1:height(Data))'; 
            end            

            % Convert to cell array if necessary (handles both strings and chars)
            if iscell(Data.Order) || isstring(Data.Order)
                % Trim whitespace and convert empty strings to NaN for filtering
                trimmedOrder = strtrim(Data.Order);
                isValid = ~strcmp(trimmedOrder, "") & ~strcmp(trimmedOrder, " ");  % Check for truly empty strings
                Data.Order(~isValid) = NaN;  % Replace empty strings with NaN
                Data.Order = str2double(Data.Order); % Convert valid numeric strings to doubles
            end
        
            % Find non-empty (non-NaN) rows
            nonEmptyRows = find(~isnan(Data.Order));
        
            % Sort by 'Order' column
            [~, sortedIdx] = sort(Data.Order(nonEmptyRows));
        
            % Return sorted row indices
            Result = nonEmptyRows(sortedIdx);
            Result = Result';
        end
        

        function Status = showModal(app, FormApp)
            % Helper: Show modal app window and return FormApp.Status
            % Call FormApp.beforeShow() if such function exists in FormApp
            % Note: FormApp should have 'Status' property
            appName = class(FormApp);
            hasBeforeShow = ismethod(FormApp, 'beforeShow');
            app.msglog(sprintf('showModal: %s, hasBeforeShow: %d', appName, hasBeforeShow));

            % Call FormApp.beforeShow() if exists
            if hasBeforeShow
                app.msglog(sprintf('showModal: calling %s.beforeShow', appName));
                try
                    FormApp.beforeShow();
                catch ME
                    app.msgex('beforeShow', ME)
                end
            end

            % Override CloseRequestFcn to handle 'X' button click
            FormApp.UIFigure.CloseRequestFcn = @(src, event) app.handleCloseRequest(FormApp);

            % Show the app window as modal window
            uiwait(FormApp.UIFigure);

            % Hide the app window and get its Status property
            if isvalid(FormApp)
                FormApp.UIFigure.Visible = 'off';
                Status = FormApp.Status;
                app.msglog(sprintf('showModal: %s - returned, Status=%s', appName, Status));
            else
                Status = 'Cancel';  % Handle case where the user closed the app
                app.msglog(sprintf('showModal: %s - closed via X button, Status=%s', appName, Status));                
            end
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

        
        function setTopLabel(app, Text, FontColor, BackgroundColor)
            % Helper: Set text and colors of LabelTopStatus (located just below the main toolbar)
            % Hide the label if Text is empty.
            % Colors: Font: Blue: [0.00,0.00,1.00], Background: Yellow: [1.00,1.00,0.07]
            % Example: app.setTopLabel('The plan was submitted and cannot be modified.', [0.00,0.00,1.00], [1.00,1.00,0.07])
            if isempty(Text)
                app.LabelTopStatus.Visible = false;
            else
                app.LabelTopStatus.Text = Text;
                app.LabelTopStatus.FontColor = FontColor;
                app.LabelTopStatus.BackgroundColor = BackgroundColor;
                app.LabelTopStatus.Visible = true;                
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
            % Helper: Save the specified Obj to file in DebugPath
            FileName = fullfile(app.MainModule.DebugPath, FileName);
            app.msglog(sprintf('debugSave: %s', FileName));
            save(FileName, 'Obj')
        end


        function showPleaseWait(app, Message)
            % Helper: Shows a spinner popup with 'Please Wait' message
            app.PleaseWaitDlg = uiprogressdlg(app.UIFigure, 'Title', 'Please wait', 'Message', Message, 'Indeterminate', 'on');  
        end


        function closePleaseWait(app)
            % Helper: Close the Please Wait popup message
            if ~isempty(app.PleaseWaitDlg)
                close(app.PleaseWaitDlg);
                app.PleaseWaitDlg = [];
            end
        end


        function result = hasPlanner(app)
            % Helper: Return true if there is active planner object
            result = ~isempty(app.MainModule.Planner);
            if ~result
                app.msglog('hasPlanner: None');
            end
        end


        function result = hasPlan(app)
            % Helper: Return true if there is active planner object and
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
        function startupFcn(app)
            % This function is automaticalled called on application startup
            app.init();            
        end

        % Button pushed function: NewButton
        function NewButtonPushed(app, event)
            app.createNewPlan();
        end

        % Callback function
        function UniqueButtonPushed(app, event)

        end

        % Menu selected function: ConnectLoginMenu
        function ConnectLoginMenuSelected(app, event)
            app.login();
        end

        % Menu selected function: DisconnectLogoutMenu
        function DisconnectLogoutMenuSelected(app, event)
            app.logout();
        end

        % Callback function
        function NewMenuSelected(app, event)
            app.createNewPlan();
        end

        % Callback function
        function OpenMenuSelected(app, event)
            app.openPlan();           
        end

        % Callback function
        function SaveMenuSelected(app, event)
            app.savePlan();
        end

        % Callback function
        function ExitPlannerMenuSelected(app, event)
            app.exitPlanner();
        end

        % Callback function
        function UniqueTargetsMenuSelected(app, event)
            app.showUniqueTargetsWindow();
        end

        % Callback function
        function ParametersMenuSelected(app, event)
            app.showPlanParamsWindow();
        end

        % Callback function
        function ValidateMenuSelected(app, event)
            app.validate();
        end

        % Callback function
        function SubmitMenuSelected(app, event)
            app.submit();
        end

        % Size changed function: ApprovedTargetsPanel
        function ApprovedTargetsPanelSizeChanged(app, event)

        end

        % Size changed function: PlanPanel
        function PlanPanelSizeChanged(app, event)

        end

        % Size changed function: UniqueTargetsPanel
        function UniqueTargetsPanelSizeChanged(app, event)

        end

        % Callback function
        function SkyMapButtonPushed(app, event)
            app.showSkyMapPlot();
        end

        % Callback function
        function GraphsButtonPushed(app, event)
            app.showGraphsPlot();
        end

        % Menu selected function: AboutMenu
        function AboutMenuSelected(app, event)
            app.showAboutWindow();
        end

        % Callback function
        function AddUniqueButtonPushed(app, event)
            app.addUniqueTarget();
        end

        % Button pushed function: AddUniqueTargetButton
        function AddUniqueTargetButtonPushed(app, event)
            app.addUniqueTarget();
        end

        % Button pushed function: EditUniqueTargetButton
        function EditUniqueTargetButtonPushed(app, event)
            app.editUniqueTarget();
        end

        % Callback function
        function RefreshUniqueTargetsButtonPushed(app, event)
            app.showUniqueTargets();
        end

        % Selection changed function: UITableUniqueTargets
        function UITableUniqueTargetsSelectionChanged(app, event)
            %app.UniqueTargetsIndices = event.Indices;
            selection = app.UITableUniqueTargets.Selection;
            app.msglog(sprintf('Unique target selected: %d', selection));
            app.uniqueTargetSelected(selection);
        end

        % Button pushed function: SNRCalcButton
        function SNRCalcButtonPushed(app, event)
            app.showSnrCalculator();
        end

        % Callback function
        function RefreshPlanTargetsButtonPushed(app, event)
            app.showPlanTargets();
        end

        % Button pushed function: RefreshApprovedTargetsButton
        function RefreshApprovedTargetsButtonPushed(app, event)
            app.retrieveApprovedTargets();
        end

        % Menu selected function: ObservationPlannerHelpMenu
        function ObservationPlannerHelpMenuSelected(app, event)
            app.showHelp();
        end

        % Callback function
        function NewMenuSelected2(app, event)

        end

        % Callback function
        function OpenMenuSelected2(app, event)
            %
        end

        % Callback function
        function SaveMenuSelected2(app, event)
            app.savePlan();
        end

        % Callback function
        function CloseMenu_2Selected(app, event)
            %
        end

        % Callback function
        function DeleteMenuSelected(app, event)
            app.deletePlan();
        end

        % Callback function
        function SavePlantoFileMenuSelected(app, event)
            app.savePlanToFile();
        end

        % Callback function
        function LoadPlanfromFileMenuSelected(app, event)
            app.loadPlanFromFile();
        end

        % Menu selected function: AddUniqueTargetMenu
        function AddUniqueTargetMenuSelected(app, event)
            app.addUniqueTarget();
        end

        % Menu selected function: EditUniqueTargetMenu
        function EditUniqueTargetMenuSelected(app, event)
            app.editUniqueTarget();
        end

        % Menu selected function: DeleteUniqueTargetMenu
        function DeleteUniqueTargetMenuSelected(app, event)
            app.deleteUniqueTarget();
        end

        % Menu selected function: ClearAllUniqueTargetsMenu
        function ClearAllUniqueTargetsMenuSelected(app, event)
            app.clearUniqueTargets();
        end

        % Menu selected function: ViewUniqueTargetsTableMenu
        function ViewUniqueTargetsTableMenuSelected(app, event)
            app.showUniqueTargetsWindow();
        end

        % Menu selected function: SaveUniqueTargetsToFileMenu
        function SaveUniqueTargetsToFileMenuSelected(app, event)
            app.saveUniqueTargetsToFile();
        end

        % Menu selected function: LoadUniqueTargetsFromFileMenu
        function LoadUniqueTargetsFromFileMenuSelected(app, event)
            %
            app.loadUniqueTargetsFromFile();
        end

        % Menu selected function: ViewSkyMapPlotWindowMenu
        function ViewSkyMapPlotWindowMenuSelected(app, event)
            app.showSkyMapPlotWindow();
        end

        % Menu selected function: ViewGraphsPlotWindowMenu
        function ViewGraphsPlotWindowMenuSelected(app, event)
            app.showGraphsPlotWindow();
        end

        % Menu selected function: EditPlanTargetMenu
        function EditPlanTargetMenuSelected(app, event)
            app.editPlanTarget();
        end

        % Menu selected function: DeletePlanTargetMenu
        function DeletePlanTargetMenuSelected(app, event)
            %
            app.deletePlanTarget();
        end

        % Menu selected function: ClearAllPlanTargetsMenu
        function ClearAllPlanTargetsMenuSelected(app, event)
            app.clearPlanTargets();
        end

        % Menu selected function: ViewPlanTableMenu
        function ViewPlanTableMenuSelected(app, event)
            app.showPlanTargetsWindow();
        end

        % Callback function
        function NewButtonPushed2(app, event)
           
        end

        % Button pushed function: OpenButton
        function OpenButtonPushed(app, event)
            app.openPlan();            
        end

        % Button pushed function: SaveButton
        function SaveButtonPushed(app, event)
            app.savePlan();
        end

        % Button pushed function: ParamsButton
        function ParamsButtonPushed(app, event)
            app.showPlanParamsWindow();
        end

        % Button pushed function: ValidateButton
        function ValidateButtonPushed(app, event)
            app.validate();
        end

        % Button pushed function: SubmitButton
        function SubmitButtonPushed(app, event)
            app.submit();
        end

        % Callback function
        function GetApprovedButtonPushed(app, event)

        end

        % Button pushed function: BuildButton
        function BuildButtonPushed(app, event)
            app.build();
        end

        % Button pushed function: EditPlanTargetButton
        function EditPlanTargetButtonPushed(app, event)
            app.editPlanTarget();
        end

        % Button pushed function: LoginButton
        function LoginButtonPushed(app, event)
            if app.isLogin()
                app.logout();
            else
                app.login();
            end
        end

        % Callback function
        function BuildMenuSelected(app, event)
            app.build();
        end

        % Button pushed function: LoadUniqueTargetsButton
        function LoadUniqueTargetsButtonPushed(app, event)
            app.loadUniqueTargetsFromFile();
        end

        % Menu selected function: LogWindowMenu
        function LogWindowMenuSelected(app, event)
            app.showLogger();
        end

        % Callback function
        function PlotCalibButtonPushed(app, event)
            app.plotCalibObj();
        end

        % Button pushed function: RefreshApprovedTargetsButton_2
        function RefreshApprovedTargetsButton_2Pushed(app, event)
            app.MainModule.clearStatus();
            app.updateStatus();
        end

        % Button pushed function: CheckPlanTargetsButton
        function CheckPlanTargetsButtonPushed(app, event)
            app.checkPlanSelfConsistency();
        end

        % Selection changed function: UITablePlanTargets
        function UITablePlanTargetsSelectionChanged(app, event)
            selection = app.UITablePlanTargets.Selection;
            app.msglog(sprintf('Plan target selected: %d', selection));
            app.planTargetSelected(selection);                        
        end

        % Selection changed function: UITableApprovedTargets
        function UITableApprovedTargetsSelectionChanged(app, event)
            selection = app.UITableApprovedTargets.Selection;
            app.msglog(sprintf('Approved target selected: %d', selection));
            app.approvedTargetSelected(selection);            
        end

        % Button pushed function: DuplicateButton
        function DuplicateButtonPushed(app, event)
            app.duplicatePlan();
        end

        % Callback function
        function CheckTimesUpdateButtonPushed(app, event)
            app.updatePlannerCheckTimes();
        end

        % Callback function
        function PlotVisibilityButtonPushed(app, event)
            app.plotVisibility();
        end

        % Value changed function: PlotCalibObjDropDown
        function PlotCalibObjDropDownValueChanged(app, event)
            app.plotCalibObjSub();
        end

        % Button pushed function: UpdateSkyMapButton
        function UpdateSkyMapButtonPushed(app, event)
            app.showSkyMapPlot();
        end

        % Button pushed function: RefreshApprovedTargetsButton_3
        function RefreshApprovedTargetsButton_3Pushed(app, event)
            app.clearApprovedTargets();
        end

        % Button pushed function: EditPlanTargetButton_6
        function EditPlanTargetButton_6Pushed(app, event)
            app.adjustGroupStartTime();
        end

        % Menu selected function: RefreshMenu
        function RefreshMenuSelected(app, event)
            app.showPlanAll();
        end

        % Button pushed function: EditPlanTargetButton_5
        function EditPlanTargetButton_5Pushed(app, event)
            app.clearUniqueTargets();
        end

        % Button pushed function: EditPlanTargetButton_3
        function EditPlanTargetButton_3Pushed(app, event)
            app.clearPlanTargets();
        end

        % Clicked callback: UITableUniqueTargets
        function UITableUniqueTargetsClicked(app, event)
            %
            %displayRow = event.InteractionInformation.DisplayRow;
            %displayColumn = event.InteractionInformation.DisplayColumn;            
            app.uniqueTargetClick();
        end

        % Double-clicked callback: UITableUniqueTargets
        function UITableUniqueTargetsDoubleClicked(app, event)
            %
            %displayRow = event.InteractionInformation.DisplayRow;
            %displayColumn = event.InteractionInformation.DisplayColumn;
            app.uniqueTargetDoubleClick();
        end

        % Clicked callback: UITablePlanTargets
        function UITablePlanTargetsClicked(app, event)
            %displayRow = event.InteractionInformation.DisplayRow;
            %displayColumn = event.InteractionInformation.DisplayColumn;
            app.planRowClick();
        end

        % Double-clicked callback: UITablePlanTargets
        function UITablePlanTargetsDoubleClicked(app, event)
            %displayRow = event.InteractionInformation.DisplayRow;
            %displayColumn = event.InteractionInformation.DisplayColumn;
            app.planRowDoubleClick();            
        end

        % Button pushed function: BuildStatusButton
        function BuildStatusButtonPushed(app, event)
            app.showBuildStatusWindow();
        end

        % Button pushed function: ValidationStatusButton
        function ValidationStatusButtonPushed(app, event)
            app.showValidationStatusWindow();
        end

        % Button pushed function: SubmitStatusButton
        function SubmitStatusButtonPushed(app, event)
            app.showSubmitStatusWindow();
        end

        % Menu selected function: NewMenu
        function NewMenuSelected3(app, event)
            app.createNewPlan();
        end

        % Menu selected function: OpenMenu
        function OpenMenuSelected3(app, event)
            app.openPlan();
        end

        % Menu selected function: SaveMenu
        function SaveMenuSelected3(app, event)
            app.savePlan();
        end

        % Menu selected function: DuplicateMenu
        function DuplicateMenuSelected(app, event)
            app.duplicatePlan();
        end

        % Menu selected function: CloseMenu_2
        function CloseMenu_2Selected2(app, event)
            app.closePlan();
        end

        % Menu selected function: DeleteMenu
        function DeleteMenuSelected2(app, event)
            app.deletePlan();
        end

        % Menu selected function: SaveToLocalFileMenu
        function SaveToLocalFileMenuSelected(app, event)
            app.savePlanToFile();
        end

        % Menu selected function: OpenFromLocalFileMenu
        function OpenFromLocalFileMenuSelected(app, event)
            app.loadPlanFromFile();
        end

        % Menu selected function: ExitPlannerMenu
        function ExitPlannerMenuSelected2(app, event)
            app.exitPlanner();
        end

        % Button pushed function: OpenCalObjTableButton
        function OpenCalObjTableButtonPushed(app, event)
            app.showCalibObjTable();
        end

        % Button pushed function: OpenSkyMapPlotWindowButton
        function OpenSkyMapPlotWindowButtonPushed(app, event)
            app.showSkyMapPlotWindow();
        end

        % Button pushed function: OpenGraphsPlotWindowButton
        function OpenGraphsPlotWindowButtonPushed(app, event)
            app.showGraphsPlotWindow();
        end

        % Selection changed function: ButtonGroup
        function ButtonGroupSelectionChanged(app, event)
            %selectedButton = app.ButtonGroup.SelectedObject;
            app.plotGraphs();
        end

        % Button pushed function: EditPlanTargetButton_2
        function EditPlanTargetButton_2Pushed(app, event)
            app.deletePlanTarget();
        end

        % Button pushed function: GDriveCommentsButton
        function GDriveCommentsButtonPushed(app, event)
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
            app.showPlanHistory();
        end

        % Button pushed function: EditPlanTargetButton_4
        function EditPlanTargetButton_4Pushed(app, event)
            app.DeleteUniqueTarget();
        end

        % Menu selected function: ClearPlotsMenu
        function ClearPlotsMenuSelected(app, event)
            app.clearPlots();
        end

        % Button pushed function: ShowUniqueTargetsWindowButton
        function ShowUniqueTargetsWindowButtonPushed(app, event)
            app.showUniqueTargetsWindow();
        end

        % Button pushed function: ShowPlanRowsWindowButton
        function ShowPlanRowsWindowButtonPushed(app, event)
            app.showPlanTargetsWindow();
        end

        % Button pushed function: ShowApprovedTargetsWindowButton
        function ShowApprovedTargetsWindowButtonPushed(app, event)
            app.showApprovedTargetsWindow();
        end

        % Callback function
        function StartTimeEditFieldValueChanged(app, event)
            value = app.StartTimeEditField.Value;
            value = app.PlanTitleEditField.Value;
            if ~isempty(app.MainModule.Planner)
                app.MainModule.Planner.Title = value;
                app.msglog(sprintf('PlanTitleEditFieldValueChanged: %s', value));
            end            
        end

        % Value changed function: EndTimeEditField
        function EndTimeEditFieldValueChanged(app, event)
            app.setPlanStartEndTime(app.StartTimeEditField.Value, app.EndTimeEditField.Value);            
        end

        % Value changed function: StartTimeEditField
        function StartTimeEditFieldValueChanged2(app, event)
            app.setPlanStartEndTime(app.StartTimeEditField.Value, app.EndTimeEditField.Value);
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            app.showHelp('');
        end

        % Button pushed function: HelpUniqueTargetsWindowButton
        function HelpUniqueTargetsWindowButtonPushed(app, event)
            app.showHelp('add_unique_target');
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
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1522 839];
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

            % Create AboutMenu
            app.AboutMenu = uimenu(app.HelpMenu);
            app.AboutMenu.MenuSelectedFcn = createCallbackFcn(app, @AboutMenuSelected, true);
            app.AboutMenu.Text = 'About';

            % Create PanelToolbar
            app.PanelToolbar = uipanel(app.UIFigure);
            app.PanelToolbar.BorderType = 'none';
            app.PanelToolbar.BackgroundColor = [0.8 0.8 0.8];
            app.PanelToolbar.Position = [2 753 1513 42];

            % Create NewButton
            app.NewButton = uibutton(app.PanelToolbar, 'push');
            app.NewButton.ButtonPushedFcn = createCallbackFcn(app, @NewButtonPushed, true);
            app.NewButton.FontWeight = 'bold';
            app.NewButton.FontColor = [0 0.4471 0.7412];
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
            app.SubmitButton.FontWeight = 'bold';
            app.SubmitButton.Tooltip = {'Send plan to Mission Control for approval'};
            app.SubmitButton.Position = [651 8 85 30];
            app.SubmitButton.Text = 'Submit';

            % Create ValidateButton
            app.ValidateButton = uibutton(app.PanelToolbar, 'push');
            app.ValidateButton.ButtonPushedFcn = createCallbackFcn(app, @ValidateButtonPushed, true);
            app.ValidateButton.FontWeight = 'bold';
            app.ValidateButton.FontColor = [0.6353 0.0784 0.1843];
            app.ValidateButton.Tooltip = {'Send plan for validation (may take up to 30 seconds)'};
            app.ValidateButton.Position = [550 8 85 30];
            app.ValidateButton.Text = 'Validate';

            % Create ParamsButton
            app.ParamsButton = uibutton(app.PanelToolbar, 'push');
            app.ParamsButton.ButtonPushedFcn = createCallbackFcn(app, @ParamsButtonPushed, true);
            app.ParamsButton.Tooltip = {'Show (and edit) observation plan parameters'};
            app.ParamsButton.Position = [400 8 85 30];
            app.ParamsButton.Text = 'Params';

            % Create LoginButton
            app.LoginButton = uibutton(app.PanelToolbar, 'push');
            app.LoginButton.ButtonPushedFcn = createCallbackFcn(app, @LoginButtonPushed, true);
            app.LoginButton.BackgroundColor = [1 1 0.0706];
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

            % Create ConnectionStatusEditFieldLabel
            app.ConnectionStatusEditFieldLabel = uilabel(app.PanelToolbar);
            app.ConnectionStatusEditFieldLabel.HorizontalAlignment = 'right';
            app.ConnectionStatusEditFieldLabel.Position = [1105 14 103 22];
            app.ConnectionStatusEditFieldLabel.Text = 'Connection Status';

            % Create ConnectionStatusEditField
            app.ConnectionStatusEditField = uieditfield(app.PanelToolbar, 'text');
            app.ConnectionStatusEditField.Editable = 'off';
            app.ConnectionStatusEditField.HorizontalAlignment = 'center';
            app.ConnectionStatusEditField.FontWeight = 'bold';
            app.ConnectionStatusEditField.BackgroundColor = [0 1 1];
            app.ConnectionStatusEditField.Tooltip = {'Server connection & login status'};
            app.ConnectionStatusEditField.Position = [1223 7 124 29];
            app.ConnectionStatusEditField.Value = 'Backend Simulator';

            % Create ModifiedLabel
            app.ModifiedLabel = uilabel(app.PanelToolbar);
            app.ModifiedLabel.Tooltip = {'Is current plan modified sience last saved?'};
            app.ModifiedLabel.Position = [946 14 50 22];
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

            % Create GDriveCommentsButton
            app.GDriveCommentsButton = uibutton(app.PanelToolbar, 'push');
            app.GDriveCommentsButton.ButtonPushedFcn = createCallbackFcn(app, @GDriveCommentsButtonPushed, true);
            app.GDriveCommentsButton.BackgroundColor = [1 0 1];
            app.GDriveCommentsButton.FontWeight = 'bold';
            app.GDriveCommentsButton.FontColor = [1 1 0.0667];
            app.GDriveCommentsButton.Tooltip = {'Open SNR Calculator web application in browser window'};
            app.GDriveCommentsButton.Position = [1353 5 77 36];
            app.GDriveCommentsButton.Text = {'GDrive'; 'Comments'};

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
            app.UniqueTargetsPanel.BackgroundColor = [0.9294 0.851 0.9804];
            app.UniqueTargetsPanel.SizeChangedFcn = createCallbackFcn(app, @UniqueTargetsPanelSizeChanged, true);
            app.UniqueTargetsPanel.Position = [13 432 993 186];

            % Create UITableUniqueTargets
            app.UITableUniqueTargets = uitable(app.UniqueTargetsPanel);
            app.UITableUniqueTargets.ColumnName = '';
            app.UITableUniqueTargets.RowName = {};
            app.UITableUniqueTargets.ColumnEditable = true;
            app.UITableUniqueTargets.DoubleClickedFcn = createCallbackFcn(app, @UITableUniqueTargetsDoubleClicked, true);
            app.UITableUniqueTargets.ClickedFcn = createCallbackFcn(app, @UITableUniqueTargetsClicked, true);
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
            app.AddUniqueTargetButton.FontColor = [0 0.4471 0.7412];
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

            % Create PlanPanel
            app.PlanPanel = uipanel(app.UIFigure);
            app.PlanPanel.TitlePosition = 'centertop';
            app.PlanPanel.Title = 'Plan';
            app.PlanPanel.BackgroundColor = [0.302 0.749 0.9294];
            app.PlanPanel.SizeChangedFcn = createCallbackFcn(app, @PlanPanelSizeChanged, true);
            app.PlanPanel.Position = [13 225 993 202];

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
            app.BuildButton.FontColor = [0 0.4471 0.7412];
            app.BuildButton.Tooltip = {'Build plan (depends on plan type)'};
            app.BuildButton.Position = [6 145 60 22];
            app.BuildButton.Text = 'Build';

            % Create CheckPlanTargetsButton
            app.CheckPlanTargetsButton = uibutton(app.Panel_4, 'push');
            app.CheckPlanTargetsButton.ButtonPushedFcn = createCallbackFcn(app, @CheckPlanTargetsButtonPushed, true);
            app.CheckPlanTargetsButton.FontSize = 11;
            app.CheckPlanTargetsButton.FontWeight = 'bold';
            app.CheckPlanTargetsButton.FontColor = [0.851 0.3255 0.098];
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
            app.ApprovedTargetsPanel.BackgroundColor = [0.8588 0.9294 0.7608];
            app.ApprovedTargetsPanel.SizeChangedFcn = createCallbackFcn(app, @ApprovedTargetsPanelSizeChanged, true);
            app.ApprovedTargetsPanel.Position = [12 69 994 150];

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
            app.PlotSkyMapCurrentlyshowsgeneralskymapPanel.Position = [1020 389 497 358];

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
            app.PlotFlagVisibleCheckBox.Text = 'Visibility';
            app.PlotFlagVisibleCheckBox.Position = [8 132 65 22];

            % Create UpdateSkyMapButton
            app.UpdateSkyMapButton = uibutton(app.Panel_10, 'push');
            app.UpdateSkyMapButton.ButtonPushedFcn = createCallbackFcn(app, @UpdateSkyMapButtonPushed, true);
            app.UpdateSkyMapButton.FontWeight = 'bold';
            app.UpdateSkyMapButton.FontColor = [0.4941 0.1843 0.5569];
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
            app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel.Position = [1020 2 497 381];

            % Create AxesGraphsPlot
            app.AxesGraphsPlot = uiaxes(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            title(app.AxesGraphsPlot, 'Title')
            xlabel(app.AxesGraphsPlot, 'X')
            ylabel(app.AxesGraphsPlot, 'Y')
            zlabel(app.AxesGraphsPlot, 'Z')
            app.AxesGraphsPlot.FontName = 'Helvetica';
            app.AxesGraphsPlot.ButtonDownFcn = createCallbackFcn(app, @AxesGraphsPlotButtonDown, true);
            app.AxesGraphsPlot.Position = [12 28 476 265];

            % Create cooSysLabel_2
            app.cooSysLabel_2 = uilabel(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.cooSysLabel_2.HorizontalAlignment = 'right';
            app.cooSysLabel_2.Position = [86 305 42 22];
            app.cooSysLabel_2.Text = 'CalObj';

            % Create PlotCalibObjDropDown
            app.PlotCalibObjDropDown = uidropdown(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.PlotCalibObjDropDown.Items = {};
            app.PlotCalibObjDropDown.ValueChangedFcn = createCallbackFcn(app, @PlotCalibObjDropDownValueChanged, true);
            app.PlotCalibObjDropDown.Position = [143 303 155 22];
            app.PlotCalibObjDropDown.Value = {};

            % Create cooSysLabel_3
            app.cooSysLabel_3 = uilabel(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.cooSysLabel_3.HorizontalAlignment = 'right';
            app.cooSysLabel_3.Position = [12 333 115 22];
            app.cooSysLabel_3.Text = 'Unique Target Name';

            % Create GraphPlotUniqueTargetDropDown
            app.GraphPlotUniqueTargetDropDown = uidropdown(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.GraphPlotUniqueTargetDropDown.Items = {};
            app.GraphPlotUniqueTargetDropDown.Position = [142 330 205 22];
            app.GraphPlotUniqueTargetDropDown.Value = {};

            % Create ButtonGroup
            app.ButtonGroup = uibuttongroup(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel);
            app.ButtonGroup.SelectionChangedFcn = createCallbackFcn(app, @ButtonGroupSelectionChanged, true);
            app.ButtonGroup.Position = [366 304 116 49];

            % Create VisibilityButton
            app.VisibilityButton = uiradiobutton(app.ButtonGroup);
            app.VisibilityButton.Text = 'Visibility';
            app.VisibilityButton.Position = [8 23 65 22];
            app.VisibilityButton.Value = true;

            % Create CalibrationStarButton
            app.CalibrationStarButton = uiradiobutton(app.ButtonGroup);
            app.CalibrationStarButton.Text = 'Calibration Star';
            app.CalibrationStarButton.Position = [8 2 105 22];

            % Create OpenCalObjTableButton
            app.OpenCalObjTableButton = uibutton(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel, 'push');
            app.OpenCalObjTableButton.ButtonPushedFcn = createCallbackFcn(app, @OpenCalObjTableButtonPushed, true);
            app.OpenCalObjTableButton.Position = [308 302 47 23];
            app.OpenCalObjTableButton.Text = 'CalObj';

            % Create OpenGraphsPlotWindowButton
            app.OpenGraphsPlotWindowButton = uibutton(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel, 'push');
            app.OpenGraphsPlotWindowButton.ButtonPushedFcn = createCallbackFcn(app, @OpenGraphsPlotWindowButtonPushed, true);
            app.OpenGraphsPlotWindowButton.FontWeight = 'bold';
            app.OpenGraphsPlotWindowButton.Position = [462 364 34 17];
            app.OpenGraphsPlotWindowButton.Text = '*';

            % Create HelpGraphsPlotWindowButton
            app.HelpGraphsPlotWindowButton = uibutton(app.PlotGraphsDoubleClickUniqueTargetorPlanrowPanel, 'push');
            app.HelpGraphsPlotWindowButton.ButtonPushedFcn = createCallbackFcn(app, @HelpGraphsPlotWindowButtonPushed, true);
            app.HelpGraphsPlotWindowButton.FontSize = 9;
            app.HelpGraphsPlotWindowButton.FontWeight = 'bold';
            app.HelpGraphsPlotWindowButton.Position = [424 363 34 17];
            app.HelpGraphsPlotWindowButton.Text = '?';

            % Create TabGroup
            app.TabGroup = uitabgroup(app.UIFigure);
            app.TabGroup.Position = [13 624 474 123];

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
            app.PlanTitleEditField.Position = [91 5 336 22];

            % Create StartTimeEditFieldLabel
            app.StartTimeEditFieldLabel = uilabel(app.PlanParamsTab);
            app.StartTimeEditFieldLabel.HorizontalAlignment = 'right';
            app.StartTimeEditFieldLabel.Position = [217 68 60 22];
            app.StartTimeEditFieldLabel.Text = 'Start Time';

            % Create StartTimeEditField
            app.StartTimeEditField = uieditfield(app.PlanParamsTab, 'text');
            app.StartTimeEditField.ValueChangedFcn = createCallbackFcn(app, @StartTimeEditFieldValueChanged2, true);
            app.StartTimeEditField.Editable = 'off';
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
            app.EndTimeEditField.Position = [291 37 136 22];

            % Create HelpPlanParamsButton
            app.HelpPlanParamsButton = uibutton(app.PlanParamsTab, 'push');
            app.HelpPlanParamsButton.ButtonPushedFcn = createCallbackFcn(app, @HelpPlanParamsButtonPushed, true);
            app.HelpPlanParamsButton.FontSize = 9;
            app.HelpPlanParamsButton.FontWeight = 'bold';
            app.HelpPlanParamsButton.Position = [435 72 34 17];
            app.HelpPlanParamsButton.Text = '?';

            % Create Panel_8
            app.Panel_8 = uipanel(app.UIFigure);
            app.Panel_8.TitlePosition = 'centertop';
            app.Panel_8.BackgroundColor = [0.902 0.902 0.902];
            app.Panel_8.Position = [13 2 989 61];

            % Create StatusTextAreaLabel
            app.StatusTextAreaLabel = uilabel(app.Panel_8);
            app.StatusTextAreaLabel.HorizontalAlignment = 'right';
            app.StatusTextAreaLabel.Position = [24 29 39 22];
            app.StatusTextAreaLabel.Text = 'Status';

            % Create StatusTextArea
            app.StatusTextArea = uitextarea(app.Panel_8);
            app.StatusTextArea.Editable = 'off';
            app.StatusTextArea.BackgroundColor = [1 0.9882 0.8196];
            app.StatusTextArea.Position = [78 13 833 40];

            % Create RefreshApprovedTargetsButton_2
            app.RefreshApprovedTargetsButton_2 = uibutton(app.Panel_8, 'push');
            app.RefreshApprovedTargetsButton_2.ButtonPushedFcn = createCallbackFcn(app, @RefreshApprovedTargetsButton_2Pushed, true);
            app.RefreshApprovedTargetsButton_2.FontSize = 11;
            app.RefreshApprovedTargetsButton_2.Position = [927 28 52 25];
            app.RefreshApprovedTargetsButton_2.Text = 'Clear';

            % Create TabGroup2
            app.TabGroup2 = uitabgroup(app.UIFigure);
            app.TabGroup2.Position = [530 625 475 123];

            % Create Tab
            app.Tab = uitab(app.TabGroup2);
            app.Tab.Title = 'או';

            % Create BuildEditFieldLabel
            app.BuildEditFieldLabel = uilabel(app.Tab);
            app.BuildEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.BuildEditFieldLabel.HorizontalAlignment = 'right';
            app.BuildEditFieldLabel.FontWeight = 'bold';
            app.BuildEditFieldLabel.Position = [6 69 35 22];
            app.BuildEditFieldLabel.Text = 'Build';

            % Create BuildTimeEditField
            app.BuildTimeEditField = uieditfield(app.Tab, 'text');
            app.BuildTimeEditField.Editable = 'off';
            app.BuildTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BuildTimeEditField.Position = [79 69 136 22];

            % Create ValidationEditFieldLabel
            app.ValidationEditFieldLabel = uilabel(app.Tab);
            app.ValidationEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.ValidationEditFieldLabel.HorizontalAlignment = 'right';
            app.ValidationEditFieldLabel.FontWeight = 'bold';
            app.ValidationEditFieldLabel.Position = [6 38 62 22];
            app.ValidationEditFieldLabel.Text = 'Validation';

            % Create ValidationTimeEditField
            app.ValidationTimeEditField = uieditfield(app.Tab, 'text');
            app.ValidationTimeEditField.Editable = 'off';
            app.ValidationTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ValidationTimeEditField.Position = [78 38 136 22];

            % Create SubmitEditFieldLabel
            app.SubmitEditFieldLabel = uilabel(app.Tab);
            app.SubmitEditFieldLabel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.SubmitEditFieldLabel.HorizontalAlignment = 'right';
            app.SubmitEditFieldLabel.FontWeight = 'bold';
            app.SubmitEditFieldLabel.Position = [6 6 46 22];
            app.SubmitEditFieldLabel.Text = 'Submit';

            % Create SubmitTimeEditField
            app.SubmitTimeEditField = uieditfield(app.Tab, 'text');
            app.SubmitTimeEditField.Editable = 'off';
            app.SubmitTimeEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SubmitTimeEditField.Position = [78 6 136 22];

            % Create BuildShortStatusEditField
            app.BuildShortStatusEditField = uieditfield(app.Tab, 'text');
            app.BuildShortStatusEditField.Editable = 'off';
            app.BuildShortStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.BuildShortStatusEditField.Position = [226 69 145 22];

            % Create ValidationShortStatusEditField
            app.ValidationShortStatusEditField = uieditfield(app.Tab, 'text');
            app.ValidationShortStatusEditField.Editable = 'off';
            app.ValidationShortStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.ValidationShortStatusEditField.Position = [226 38 145 22];

            % Create SubmitShortStatusEditField
            app.SubmitShortStatusEditField = uieditfield(app.Tab, 'text');
            app.SubmitShortStatusEditField.Editable = 'off';
            app.SubmitShortStatusEditField.BackgroundColor = [1 0.9882 0.8196];
            app.SubmitShortStatusEditField.Position = [226 6 145 22];

            % Create BuildStatusButton
            app.BuildStatusButton = uibutton(app.Tab, 'push');
            app.BuildStatusButton.ButtonPushedFcn = createCallbackFcn(app, @BuildStatusButtonPushed, true);
            app.BuildStatusButton.Tooltip = {'Load unique targets from text file'};
            app.BuildStatusButton.Position = [377 69 36 23];
            app.BuildStatusButton.Text = '...';

            % Create ValidationStatusButton
            app.ValidationStatusButton = uibutton(app.Tab, 'push');
            app.ValidationStatusButton.ButtonPushedFcn = createCallbackFcn(app, @ValidationStatusButtonPushed, true);
            app.ValidationStatusButton.Tooltip = {'Load unique targets from text file'};
            app.ValidationStatusButton.Position = [378 38 36 23];
            app.ValidationStatusButton.Text = '...';

            % Create SubmitStatusButton
            app.SubmitStatusButton = uibutton(app.Tab, 'push');
            app.SubmitStatusButton.ButtonPushedFcn = createCallbackFcn(app, @SubmitStatusButtonPushed, true);
            app.SubmitStatusButton.Tooltip = {'Load unique targets from text file'};
            app.SubmitStatusButton.Position = [378 6 36 23];
            app.SubmitStatusButton.Text = '...';

            % Create HelpStatusInfoButton
            app.HelpStatusInfoButton = uibutton(app.Tab, 'push');
            app.HelpStatusInfoButton.ButtonPushedFcn = createCallbackFcn(app, @HelpStatusInfoButtonPushed, true);
            app.HelpStatusInfoButton.FontSize = 9;
            app.HelpStatusInfoButton.FontWeight = 'bold';
            app.HelpStatusInfoButton.Position = [431 71 34 17];
            app.HelpStatusInfoButton.Text = '?';

            % Create PanelTopHeader
            app.PanelTopHeader = uipanel(app.UIFigure);
            app.PanelTopHeader.BorderType = 'none';
            app.PanelTopHeader.Position = [2 800 1513 38];

            % Create LabelTopStatus
            app.LabelTopStatus = uilabel(app.PanelTopHeader);
            app.LabelTopStatus.BackgroundColor = [1 1 0.0667];
            app.LabelTopStatus.HorizontalAlignment = 'center';
            app.LabelTopStatus.FontWeight = 'bold';
            app.LabelTopStatus.FontColor = [0 0 1];
            app.LabelTopStatus.Visible = 'off';
            app.LabelTopStatus.Position = [373 7 498 22];
            app.LabelTopStatus.Text = 'The plan was submitted and cannot be modified.';

            % Create LabelTopNamespace
            app.LabelTopNamespace = uilabel(app.PanelTopHeader);
            app.LabelTopNamespace.BackgroundColor = [0 0 0];
            app.LabelTopNamespace.HorizontalAlignment = 'center';
            app.LabelTopNamespace.FontSize = 24;
            app.LabelTopNamespace.FontWeight = 'bold';
            app.LabelTopNamespace.FontColor = [1 1 1];
            app.LabelTopNamespace.Position = [1057 2 272 32];
            app.LabelTopNamespace.Text = 'OPER';

            % Create LabelTopUser
            app.LabelTopUser = uilabel(app.PanelTopHeader);
            app.LabelTopUser.BackgroundColor = [1 1 0.0667];
            app.LabelTopUser.HorizontalAlignment = 'center';
            app.LabelTopUser.FontSize = 24;
            app.LabelTopUser.FontWeight = 'bold';
            app.LabelTopUser.FontColor = [0 0 1];
            app.LabelTopUser.Position = [1345 3 165 32];
            app.LabelTopUser.Text = 'Please login';

            % Create LabelTopTime
            app.LabelTopTime = uilabel(app.PanelTopHeader);
            app.LabelTopTime.BackgroundColor = [1 1 1];
            app.LabelTopTime.HorizontalAlignment = 'center';
            app.LabelTopTime.FontSize = 24;
            app.LabelTopTime.FontWeight = 'bold';
            app.LabelTopTime.Position = [12 2 299 32];
            app.LabelTopTime.Text = 'UTC: 2025-01-01 00:00:00';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = PlannerMain

            runningApp = getRunningApp(app);

            % Check for running singleton app
            if isempty(runningApp)

                % Create UIFigure and components
                createComponents(app)

                % Register the app with App Designer
                registerApp(app, app.UIFigure)

                % Execute the startup function
                runStartupFcn(app, @startupFcn)
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