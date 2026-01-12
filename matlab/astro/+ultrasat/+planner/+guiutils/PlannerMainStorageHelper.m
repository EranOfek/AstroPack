%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainStorageHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 11/11/2025
% Description : Storage Helper for Main Planner (Open, Save, Close, Delete, etc.)
%==========================================================================

classdef PlannerMainStorageHelper < ultrasat.api.Loggable
    % Helper class for PlannerMain.mlapp
    %
    % All methods require the PlannerMain instance as the first argument, named 'app'.
    % This is NOT implicit: even when calling from PlannerMain.mlapp, pass 'app'
    % explicitly to the helper method.
    %
    % Internal call example (from PlannerMain.mlapp):
    %   app.UniqueTargetsHelper.setUniqueTargetParamsFields(app, UniqTarg, Index, ParamsApp);
    %
    % External call example (from another window/module):
    %   app.MainModule.MainApp.PlanParamsHelper.applyCheckTimes(app.MainModule.MainApp, ParamsApp);
    %
    % Notes:
    %   - 'app' always refers to the PlannerMain instance.
    %   - Additional parameters (e.g., ParamsApp) are the calling window/modules as needed.
    %

    methods (Access = public)

        function obj = PlannerMainStorageHelper()
            % Constructor
            obj.LogPrefix = 'StorageHelper';
        end


        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function openPlan(obj, app)
            % Load plan from database, requires login and server connection
            app.msglog('openPlan');

            % User is not connected, suggset to load plan from local file
            if ~app.SessionHelper.isLogin(app)
                if strcmp(app.AppUtils.askYesNo('You are not connected to the ULTRASAT DB, would you like to open a local file?', 'Open'), 'Yes')
                    obj.loadPlanFromFile(app);
                end
                return;
            end

            % Ask user to confirm
            if app.MainModule.Modified
                if ~strcmp(app.AppUtils.askYesNo('Your changes are not saved. Do you want to discard them and create a new plan?', 'Confirm'), 'Yes')
                    return;
                end
            end

            % Create OpenPlanApp
            if isempty(app.OpenPlanApp) || ~isvalid(app.OpenPlanApp)
                app.OpenPlanApp = ultrasat.planner.gui.OpenPlan(app.MainModule);
            end

            % Configure the table in the OpenPlanApp window
            app.OpenPlanApp.UITable.SelectionType = "row";
            app.OpenPlanApp.UITable.Multiselect = "off";
            app.OpenPlanApp.UITable.RowName = "numbered";

            % Query backend database for saved plans
            app.showPleaseWait('Fetching plans...');
            try
                response = app.MainModule.ApiClient.getPlansList([], [], []);
            catch ME
                app.msgex('openPlan', ME);
            end
            app.closePleaseWait();

            if ~isfield(response, 'ok') || ~response.ok
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
                app.showPleaseWait('Loading plan...');
                try
                    response = app.MainModule.ApiClient.loadPlan(Pk);
                catch ME
                    app.msgex('openPlan', ME);
                end
                app.closePleaseWait();
                if response.ok
                    obj.doOpenPlan(app, app.MainModule.ApiClient.PlanData);
                end

            end
            app.SessionHelper.setButtons(app);
        end


        function doOpenPlan(obj, app, PlanData)
            % called from openPlan()
            app.msglog(sprintf('doOpenPlan: %d', PlanData.pk));

            % Check active planner user name
            if ~strcmp(PlanData.planner.AstPlanner, app.MainModule.UserName)
                Result = app.AppUtils.askYesNoCancel('The AstPlanner field in this plan differs from the currently logged-in user. Click Yes to duplicate the plan or No to open in read-only mode.', 'Confirmation');
                if strcmp(Result, 'Yes')
                    obj.duplicatePlan(app);
                elseif strcmp(Result, 'No')
                    PlanData.planner.Editable = false;
                else
                    obj.closePlan(app);
                    return;
                end
            end

            app.MainModule.setPlanData(PlanData);
            app.updateStatus();
            app.showPlanAll();
        end


        function savePlan(obj, app)
            % Save current plan to database, requires login and server connection
            app.msglog('savePlan');
            if ~app.hasPlanner(), return; end

            % User is not connected to server, suggest saving to local file
            if ~app.SessionHelper.isLogin(app)
                if strcmp(app.AppUtils.askYesNo('You are not connected to the ULTRASAT DB, would you like to save to local file?', 'Save'), 'Yes')
                    obj.savePlanToFile(app);
                end
                return;
            end

            % Call backend to save the plan in database
            app.showPleaseWait('Saving your plan. This may take a while. Please wait...');
            try
                % Set update_time
                app.MainModule.PlanData.update_time = ultrasat.api.ModelBase.nowUtc();
                app.MainModule.ApiClient.savePlan();
                app.clearModified();

                % Update Pk display (required if this plan saved for the first time)
                app.PlanPkEditField.Value = num2str(app.MainModule.Planner.Pk);
            catch ME
                app.msgex('savePlan', ME);
            end

            app.closePleaseWait();
            app.MainModule.setStatus('OK', 'Plan saved successfully.');
        end


        function closePlan(obj, app)
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

            obj.doClosePlan(app);
            app.clearModified();
            app.SessionHelper.setButtons(app);
        end


        function doClosePlan(obj, app)
            %
            app.msglog('doClosePlan');
            app.MainModule.clearData();

            app.showPlanAll();
            app.clearModified();
            app.SessionHelper.setButtons(app);
        end


        function deletePlan(obj, app)
            %
            app.msglog('deletePlan');
            if app.MainModule.Modified
                % Ask user to save current modified plan or to discard it
            end

            %app.clearModified();
        end


        function savePlanToFile(obj, app)
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
                app.showPleaseWait('Saving plan to file...');
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


        function loadPlanFromFile(obj, app)
            % Load plan from file as matlab object
            app.msglog('loadPlanFromFile');

            if app.MainModule.Modified
                % @Todo - Save or discard
            end

            % Create app and set initial values from preferences
            if isempty(app.LoadPlanFromFileApp) || ~isvalid(app.LoadPlanFromFileApp)
                app.LoadPlanFromFileApp = ultrasat.planner.gui.LoadPlanFromFile(app.MainModule);

                % Set initial values from preferences
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

                    if ~isfile(FileName)
                        app.AppUtils.msgError(sprintf('File not found: %s', FileName));
                        return;
                    end

                    % Load plan from file
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
            app.SessionHelper.setButtons(app);

            % Check active planner user name
            if ~isempty(app.MainModule.UserName) && ~strcmp(app.MainModule.Planner.AstPlanner, app.MainModule.UserName)
                if ~strcmp(app.AppUtils.askYesNo('The AstPlanner field in this plan differs from the currently logged-in user. Click Yes to override AstPlanner or No to cancel opening.', 'Confirmation'), 'Yes')
                    return;
                end

                app.msglog(sprintf('loadPlanFromFile: Setting AstPlanner field of open plan: %s, %s', app.MainModule.Planner.AstPlanner, app.MainModule.UserName));
                app.MainModule.Planner.AstPlanner = app.MainModule.UserName;
            end
        end


        function duplicatePlan(obj, app)
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

                    % Save current pk for addHistory() below
                    OldPk = PlanData.pk;
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

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)
    end

end

