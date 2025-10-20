classdef PlannerMain < matlab.apps.AppBase

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
            app.MainModule = ultrasat.planner.guiutils.MainModule(app.StartupNamespaceId);
            app.MainModule.MainApp = app;

            % Create AppUtils and set in MainModule
            app.AppUtils = ultrasat.planner.guiutils.AppUtils(app.MainModule);
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
            PlanTitle = app.MainModule.GuiHelper.getFieldTitle( app.NewPlanApp.TitleEditField.Value );
            StartTime = app.MainModule.GuiHelper.getFieldDateTime( app.NewPlanApp.StartTimeEditField.Value );
            EndTime = app.MainModule.GuiHelper.getFieldDateTime( app.NewPlanApp.EndTimeEditField.Value );            

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
            %Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
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
                    Name = app.MainModule.GuiHelper.getFieldUniqueTargetName( app.AddUniqueTargetApp.NameEditField.Value );
                    RA = app.MainModule.GuiHelper.getFieldRA( app.AddUniqueTargetApp.RAEditField.Value );
                    Dec = app.MainModule.GuiHelper.getFieldDec( app.AddUniqueTargetApp.DecEditField.Value );
    
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
                Name = app.MainModule.GuiHelper.getFieldUniqueTargetName( ParamsApp.NameEditField.Value );
                RA = app.MainModule.GuiHelper.getFieldRA( ParamsApp.RAEditField.Value );
                Dec = app.MainModule.GuiHelper.getFieldDec( ParamsApp.DecEditField.Value );
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
            Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
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
            Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
            
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
                    style = app.MainModule.GuiHelper.getValidationStatusStyle(status);
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
                StartTime = app.MainModule.GuiHelper.getFieldDateTime(app.EnterStartTimeApp.GroupStartTimeEditField.Value);
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
                app.ValidationStatusApp.StatusEditField.BackgroundColor = app.MainModule.GuiHelper.getValidationStatusBackgroundColor(Response.status);

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
                            style = app.MainModule.GuiHelper.getValidationStatusStyle(status);
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
            Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);            
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
                Planner.Exptime = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.ExposureEditField.Value);
                
                % Apply per-type parameters
                if strcmp(Planner.Type, 'LCS')
                    Planner.DailyWindowStartTime = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.LcsDailyWindowStartTimeEditField.Value);
                    Planner.DailyWindowMaxDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.LcsDailyWindowMaxDurationEditField.Value);
                elseif strcmp(Planner.Type, 'AllSS')                
                    Planner.DailyWindowStartTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.AllSkyDailyWindowStartTimeEditField.Value);
                    Planner.DailyWindowMaxDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.AllSkyDailyWindowMaxDurationEditField.Value);
                    Planner.AllSSHighLatThresh = ParamsApp.AllSkyGalacticLatTresholdEditField.Value;
                    Planner.LowLatVisits = ParamsApp.AllSkyLatVisitsEditField.Value;

                    % Future
                    %Planner.= ParamsApp.AllSkyLowLatVisitsEditField.Value;
                    %Planner.= ParamsApp.AllSkyHighGalacticLatDitherPatternDropDown.Value;
                elseif strcmp(Planner.Type, 'TOO')                
                    Planner.TOOStartTime = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.TooStartTimeEditField.Value);
                    Planner.TOOWindowDuration = app.MainModule.GuiHelper.getFieldDuration(ParamsApp.TooWindowMaxDurationEditField.Value);
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
                StartTime = app.MainModule.GuiHelper.getFieldDateTime(StartTimeValue);
                EndTime = app.MainModule.GuiHelper.getFieldDateTime(EndTimeValue);

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
                StartTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.CheckStartTimeEditField.Value);
                EndTime = app.MainModule.GuiHelper.getFieldDateTime(ParamsApp.CheckEndTimeEditField.Value);

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
                Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
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
                    % 'plotTstart', app.MainModule.GuiHelper.getFieldDateTime(app.PlotStartTimeEditField.Value), ...
                    % 'plotTend', app.MainModule.GuiHelper.getFieldDateTime(app.PlotEndTimeEditField.Value) );
        
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
                %app.msglog('hasPlanner: None');
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
        function startupFcn(app, varargin)
            % This function is automaticalled called on application startup
            app.init();

            if ~isempty(varargin)
                app.StartupNamespaceId = varargin{1};
                fprintf('[startupFcn] NamespaceId specified: "%s"\n', NamespaceId);
            else
                app.StartupNamespaceId = 'sim-OPER';
                fprintf('[startupFcn] No NamespaceId specified — using default ""\n');
            end          
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
