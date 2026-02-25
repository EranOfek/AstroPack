%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainStorageHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 23/02/2026
% Description : Storage Helper for Main Planner (Open, Save, Close, Delete, etc.)
%==========================================================================

classdef PlannerMainStorageHelper < ultrasat.api.core.Loggable
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

        function getPlansListToUITable(obj, app, start_time, end_time, title_subtext, UITable)
            % Retrieves a filtered list of observation plans from the API.
            %
            % - Reads filter parameters from the UI fields.
            % - Sends a request to the API client to fetch plans.
            % - Updates the table with the retrieved plans or clears it if no results are found.
            % - Displays an alert if the request fails.
           
            % Convert empty fields to [] so API gets empty values if not provided
            if isempty(start_time)
                start_time = [];
            end
            if isempty(end_time)
                end_time = [];
            end
            if isempty(title_subtext)
                title_subtext = [];
            end
        
            % Fetch the plans list from API
            try
                response = app.MainModule.PlansClient.getPlansList(start_time, end_time, title_subtext);                        
            catch ME
                uialert(app.UIFigure, sprintf('Failed to retrieve plans list: %s', ME.message), 'Error');
                return;
            end

            if ~response.ok
                % @Todo Show alert (use msgbox or uialert)
                uialert(app.UIFigure, 'Failed to retrieve plans list', 'Error');
                return;
            end
            
            % Convert struct array to table if not empty
            if ~isempty(response.plans)
                %Data = struct2table(response.plans);
                Data = app.MainModule.TableHelper.plansToTopLevelTable(response.plans);
                Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
                Data = app.MainModule.TableHelper.selectTableColumns(Data, ...
                    {'pk', 'plan_type', 'ast_planner', 'title','status', 'created_time', 'updated_time', 'start_time', 'end_time'});

                % Sort table by updated_time or created_time
                % Safely detect if all updated_time cells are empty
                
                % Convert updated_time cells into strings (empty cells become "")
                update_str = cell(size(Data.updated_time));
                for i = 1:numel(Data.updated_time)
                    if isempty(Data.updated_time{i})
                        update_str{i} = "";
                    else
                        update_str{i} = string(Data.updated_time{i});
                    end
                end
                update_str = string(update_str);
                
                % If all update times are empty, sort by created_time
                if all(update_str == "")
                    Data = sortrows(Data, 'created_time', 'descend');
                else
                    % Replace column temporarily for sorting
                    Data.updated_time = update_str;
                    Data = sortrows(Data, 'updated_time', 'descend');
                end

                UITable.Data = Data;
                UITable.ColumnName = Data.Properties.VariableNames;  
                UITable.ColumnSortable = true;
            else
                % Clear the table if no plans are found
                UITable.Data = [];
            end
        end


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

            % Show app
            if strcmp(app.showModal(app.OpenPlanApp), 'Open')

                % Call the backend to load plan from database
                Pk = app.OpenPlanApp.Pk;
                %app.showPleaseWait('Loading plan...');
                try
                    % Get plan from database
                    response = app.MainModule.PlansClient.getPlan(Pk);
                    if response.ok && isfield(response, 'data') && ~isempty(response.data)
                        planStruct = response.data;
                        PlanData = ultrasat.api.models.PlanData.fromStruct(planStruct);

                        % Get planner data from database
                        matResp = app.MainModule.PlansClient.getMatlabMat(Pk);
                        if matResp.ok && isfield(matResp, 'data') && ~isempty(matResp.data)
                            PlanData.planner = ultrasat.api.utils.MatBase64Utils.base64ToMat(matResp.data, 'planner');
                        end

                        % Open plan
                        if ~isempty(PlanData.planner)
                            obj.doOpenPlan(app, PlanData);
                        else
                            app.msglog('openPlan: No planner data (matlab_mat) for pk=%d', Pk);
                        end
                    end
                catch ME
                    app.msgex('openPlan', ME);
                end
                %app.closePleaseWait();
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
            %app.showPleaseWait('Saving your plan. This may take a while. Please wait...');
            try
                % Set updated_time
                app.MainModule.PlanData.updated_time = ultrasat.api.utils.DateTimeUtils.nowUtc();

                % Sync fdata from uplanner object to PlanData
                ultrasat.api.utils.PlanDataUtils.syncFromPlanner(app.MainModule.PlanData, app.MainModule.Planner);

                % Convert PlanData to struct
                planStruct = app.MainModule.PlanData.toStruct();

                % Temporary fix (23/02/2026) !!!!!!!!!! - NEED to fix model
                planStruct = rmfield(planStruct, 'history');
                planStruct = rmfield(planStruct, 'metadata');        

                % Save the plan struct to the backend
                resp = app.MainModule.PlansClient.savePlan(planStruct);
                if ~resp.ok
                    app.msglog(sprintf('Warning: savePlan failed: %s', resp.status));
                else
                    % Use returned pk (new for insert, same for update)
                    if isfield(resp, 'data') && ~isempty(resp.data)
                        oldPk = app.MainModule.PlanData.pk;
                        savedPk = resp.data;
                        % Only allow pk to be updated from 0 (unsaved) to positive, or persist the current positive pk.
                        if (isempty(oldPk) ||  (oldPk == 0)) && savedPk > 0
                            app.MainModule.PlanData.pk = savedPk;
                            app.MainModule.Planner.Pk = savedPk;
                            app.msglog(sprintf('New plan saved successfully, Pk=%d', savedPk));
                        elseif app.MainModule.PlanData.pk > 0
                            % Do not overwrite existing positive pk
                            % For robustness, ensure Planner pk also matches PlanData pk
                            if savedPk ~= app.MainModule.PlanData.pk
                                app.msglog(sprintf('Warning: Planner pk does not match saved pk, updating Planner pk to %d', savedPk));
                                app.MainModule.PlanData.pk = savedPk;
                                app.MainModule.Planner.Pk = savedPk;
                            end
                        end

                        base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(app.MainModule.Planner, 'planner');
                        try
                            resp = app.MainModule.PlansClient.saveMatlabMat(savedPk, base64Str);
                            if ~resp.ok
                                app.msglog(sprintf('Warning: saveMatlabMat failed: %s', resp.status));
                            end
                        catch matME
                            app.msglog(sprintf('Warning: saveMatlabMat exception: %s', matME.message));
                        end
                    end
                end

                % Clear modified flag
                app.clearModified();

                % Update Pk display (required if this plan saved for the first time)
                app.PlanPkEditField.Value = num2str(app.MainModule.Planner.Pk);
            catch ME
                app.msgex('savePlan', ME);
            end

            %app.closePleaseWait();
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

                % Update initial values from preferences
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

                    % Update preferences with the new file name and folder
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

                    % Update preferences with the new file name and folder
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

            app.DuplicatePlanApp.PlanTitleEditField.Value = sprintf('Duplicated on %s', ultrasat.api.utils.DateTimeUtils.nowUtcStr());
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
                    PlanData.created_time = ultrasat.api.utils.DateTimeUtils.nowUtc();
                    PlanData.updated_time = PlanData.created_time;
                    PlanData.history = struct();
                    PlanData.addHistory(sprintf('Duplicated from pk=%d, %s', OldPk, ultrasat.api.utils.DateTimeUtils.datetimeStr(PlanData.updated_time)));

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

