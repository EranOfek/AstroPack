classdef PlannerMain < matlab.apps.AppBase

    % =====================================================================
    %                              Properties
    % =====================================================================
    
    properties (Access = public)
     
        % Helper classes in astro/+ultrasat/+planner/+guiutils/
        StorageHelper                           % PlannerMainStorageHelper
        NewPlanHelper                           % PlannerMainNewPlanHelper        
        PlanParamsHelper                        % PlannerMainPlanParamsHelper
        UniqueTargetsHelper                     % PlannerMainUniqueTargetsHelper               
        PlanTargetsHelper                       % PlannerMainPlanTargetsHelper
        ApprovedTargetsHelper                   % PlannerMainApprovedTargetsHelper
        BuildHelper                             % PlannerMainBuildHelper
        PlotHelper                              % PlannerMainPlotHelper        
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

            % Create helpers
            app.StorageHelper           = ultrasat.planner.guiutils.PlannerMainStorageHelper();            
            app.NewPlanHelper           = ultrasat.planner.guiutils.PlannerMainNewPlanHelper();
            app.PlanParamsHelper        = ultrasat.planner.guiutils.PlannerMainPlanParamsHelper();
            app.UniqueTargetsHelper     = ultrasat.planner.guiutils.PlannerMainUniqueTargetsHelper();
            app.PlanTargetsHelper       = ultrasat.planner.guiutils.PlannerMainPlanTargetsHelper();
            app.ApprovedTargetsHelper   = ultrasat.planner.guiutils.PlannerMainApprovedTargetsHelper();
            app.BuildHelper             = ultrasat.planner.guiutils.PlannerMainBuildHelper();
            app.PlotHelper              = ultrasat.planner.guiutils.PlannerMainPlotHelper();

            %
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
            app.NewPlanHelper.createNewPlan(app);
        end

    end

    % =====================================================================
    %           Plan - Open, Save, Close, Load, Duplicate Plan
    % =====================================================================    
    methods (Access = public)

        function openPlan(app)
            % Load plan from database, requires login and server connection
            app.StorageHelper.openPlan(app);
        end

        function savePlan(app)
            % Save current plan to database, requires login and server connection
            app.StorageHelper.savePlan(app);
        end

        function closePlan(app)
            %
            app.StorageHelper.closePlan(app);
        end

        function deletePlan(app)
            %
            app.StorageHelper.deletePlan(app);
        end

        function savePlanToFile(app)
            % Save current plan to text file, open dialog to ask user for file name
            app.StorageHelper.savePlanToFile(app);
        end

        function loadPlanFromFile(app)
            % Load plan from file as matlab object
            app.StorageHelper.loadPlanFromFile(app);
        end

        function duplicatePlan(app)
            % Duplicate the current observation plan.
            app.StorageHelper.duplicatePlan(app);
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
            app.UniqueTargetsHelper.addUniqueTarget(app);
        end

        function editUniqueTarget(app)
            % Edit Unique-Target with editUniqTarg()
            app.UniqueTargetsHelper.editUniqueTarget(app);
        end

        function setUniqueTargetParamsFields(app, UniqTarg, Index, ParamsApp)           
            % Helper: Set field values - Currently there are 9 fields for Unique Target
            app.UniqueTargetsHelper.setUniqueTargetParamsFields(app, UniqTarg, Index, ParamsApp);
        end        

        function deleteUniqueTarget(app)
            % Delete Unique-Target with delUniqTarg()
            app.UniqueTargetsHelper.deleteUniqueTarget(app);
        end

        function loadUniqueTargetsFromFile(app)
            % Load unique targets list from text file (csv). Open dialog to
            % ask user for file name or paste the text.
            app.UniqueTargetsHelper.loadUniqueTargetsFromFile(app);
        end

        function saveUniqueTargetsToFile(app)
            % Save unique targets list to text file (csv). Open dialog to
            % ask user for file name or paste the text.
            app.UniqueTargetsHelper.saveUniqueTargetsToFile(app);
        end

        function clearUniqueTargets(app)
            % CLEAR ALL Unique-Targets with clearUniqueTargets()
            app.UniqueTargetsHelper.clearUniqueTargets(app);
        end

        function uniqueTargetSelected(app, Index)
            % Helper: Called on Unique Target selection in table - @Todo
            app.UniqueTargetsHelper.uniqueTargetSelected(app, Index);
        end

        function showUniqueTargets(app)
            % Helper: Update the Unique Targets GUI table with data from Planner
            % Update the display of Unique Targets table
            app.UniqueTargetsHelper.showUniqueTargets(app);
        end

        function uniqueTargetClick(app)
            % Called on Unique-Target selection (single click) in the table
            app.UniqueTargetsHelper.uniqueTargetClick(app);
        end

        function uniqueTargetDoubleClick(app)
            % Called on Unique-Target double-click in the table
            app.UniqueTargetsHelper.uniqueTargetDoubleClick(app);
        end        

    end

    % =====================================================================
    %                            Plan Targets
    % =====================================================================
    methods (Access = public)

        function editPlanTarget(app)
            % Edit plan target by editPlanRow()
            app.PlanTargetsHelper.editPlanTarget(app);
        end

        function setPlanTargetParamsFiels(app, Plan, Index, ParamsApp)
            % Helper: Set field values - Currently there are 23 fields for Plan Target
            app.PlanTargetsHelper.setPlanTargetParamsFiels(app, Plan, Index, ParamsApp);
        end

        function applyPlanTargetParams(app, Index, ParamsApp)
            % Helper: Apply plan parameters from dialog to plan
            app.PlanTargetsHelper.applyPlanTargetParams(app, Index, ParamsApp);
        end

        function deletePlanTarget(app)
            % Delete plan target with delPlanRow()
            app.PlanTargetsHelper.deletePlanTarget(app);
        end

        function clearPlanTargets(app)
            % Clear all plan targets with clearPlan()
            app.PlanTargetsHelper.clearPlanTargets(app);
        end    

        function showPlanTargets(app)
            % Update the display of Plan Targets table
            app.PlanTargetsHelper.showPlanTargets(app)
        end

        function adjustGroupStartTime(app)
            % Adjust group of targets with adjustGroupStartTime()
            app.PlanTargetsHelper.adjustGroupStartTime(app);
        end

        function planTargetSelected(app, Index)
            % Called on plan target selection (single click)
            app.PlanTargetsHelper.planTargetSelected(app, Index);
        end

        function planRowClick(app)
            % Called on plan target selection (single click)
            app.PlanTargetsHelper.planRowClick(app);
        end
    
        function planRowDoubleClick(app)
            % Called on plan target double click
            app.PlanTargetsHelper.planRowDoubleClick(app);
        end
    end

    % =====================================================================
    %                               Windows
    % =====================================================================
    methods (Access = public)    

        function showUniqueTargetsWindow(app)
            % Show separate window with Unique Targets table
            app.UniqueTargetsHelper.showUniqueTargetsWindow(app);
        end        
       

        function showPlanTargetsWindow(app)
            % Show separate window with Plan Targets table
            app.PlanTargetsHelper.showPlanTargetsWindow(app);
        end                

        function showApprovedTargetsWindow(app)
            % Show separate window with Approved Targets table
            app.ApprovedTargetsHelper.showApprovedTargetsWindow(app);
        end                

        function copyUITable(app, SourceUITable, TargetUITable)
            % Copies data, column names, editability settings, and styles from SourceUITable to TargetUITable
            app.MainModule.GuiHelper.copyUITable(SourceUITable, TargetUITable);
        end
        
    end

    % =====================================================================
    %                               Build
    % =====================================================================
    methods (Access = public)

        function build(app)
            % Build plan according to plan type, calls doBuild...() below
            app.BuildHelper.build(app);
        end

        function setBuildStatus(app, Status)
            app.MainModule.PlanData.setStatus('BuildStatus', Status);
        end

        function showBuildStatusWindow(app)
            % Show window with last build status
            app.BuildHelper.showBuildStatusWindow(app);
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
            app.ApprovedTargetsHelper.retrieveApprovedTargets(app);
        end

        function showApprovedTargets(app)
            % Update the GUI of Approved Targets table
            app.ApprovedTargetsHelper.showApprovedTargets(app);
        end

        function clearApprovedTargets(app)
            % Clear the list of approved targets
            app.ApprovedTargetsHelper.clearApprovedTargets(app);
        end

        function approvedTargetSelected(app, Index)
            % Called on selecting (single click) approved target from table
            app.ApprovedTargetsHelper.approvedTargetSelected(app, Index);
        end        


        function showOverriddenApprovedTargets(app, PlanTargetIndex)
            % Update the display with list of approved targets
            app.ApprovedTargetsHelper.showOverriddenApprovedTargets(app, PlanTargetIndex);
        end

    end

    % =====================================================================
    %                         Plan Params Window
    % =====================================================================
    methods (Access = public)
        
        function showPlanParamsWindow(app)
            % Show window with Plan Parameters
            app.PlanParamsHelper.showPlanParamsWindow(app);
        end

        function applyPlanParams(app, ParamsApp)
            % Helper: Apply plan parameters in current planner from PlanParams app
            % Called from showPlanParamsWindow            
            app.PlanParamsHelper.applyPlanParams(app, ParamsApp);
        end

        function setPlanStartEndTime(app, StartTimeValue, EndTimeValue)
            %
            app.PlanParamsHelper.setPlanStartEndTime(app, StartTimeValue, EndTimeValue);
        end

        function applyCheckTimes(app, ParamsApp)
            % Helper: Update Planner.CheckTimes with values from the edit fields
            % Note: Called from applyPlanParams() above
            % Note: REMOVED: Called from PlanParams.CheckTimesUpdateButtonPushed()
            app.PlanParamsHelper.applyCheckTimes(app, ParamsApp);
        end                

        function showPlanHistory(app)
            app.PlanParamsHelper.showPlanHistory(app);
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

    end

    % =====================================================================
    %                               Plots
    % =====================================================================    
    methods (Access = public)

        function showSkyMapPlot(app)
            app.PlotHelper.showSkyMapPlot(app);
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
            app.PlotHelper.showCalibObjTable(app);
        end

        function showCalibObjTable(app)
            app.PlotHelper.showCalibObjTable(app);
        end

        function uniqueTargetSelectedInPlot(app, UniqueTargetIndex)
            app.PlotHelper.uniqueTargetSelectedInPlot(app, UniqueTargetIndex);
        end

        function plotCalibObjSub(app)
            app.PlotHelper.plotCalibObjSub(app);
        end

        function plotVisibility(app)
            app.PlotHelper.plotVisibility(app);
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
                
                if strcmp(Planner.Status, 'submitted')
                    app.setTopLabel('The plan was submitted and cannot be modified.', [0.00,0.00,1.00], [1.00,1.00,0.07]);
                else
                    app.setTopLabel('', [], []);
                end            

            % Planner is empty, clear fields
            else
                app.BuildTimeEditField.Value = '';
                app.ValidationTimeEditField.Value = '';
                app.SubmitTimeEditField.Value = '';

                app.setStatusField(app.BuildShortStatusEditField, '', '');
                app.setStatusField(app.ValidationShortStatusEditField, '', '');
                app.setStatusField(app.SubmitShortStatusEditField, '', '');                

                app.setTopLabel('', [], []);
            end

        end


        function setStatusField(app, EditField, Status, StatusText)
            % Helper: Set the background color of the EditField based on the Status value.
            % Valid values for Status: OK, Warning, Error, (empty)
            app.MainModule.GuiHelper.setStatusField(app, EditField, Status, StatusText);
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
