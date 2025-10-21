%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainNewPlanHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 21/10/2025
% Description : Create New Plan - HCS, LCS, DDT, AllSS, TOO
%==========================================================================

classdef PlannerMainNewPlanHelper < ultrasat.api.Loggable
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

        function obj = PlannerMainNewPlanHelper()
            % Constructor
            obj.LogPrefix = 'NewPlanHelper';
        end


        function createNewPlan(obj, app)
            % Create new plan
            app.msglog('createNewPlan');

            % Ask user confirmation if there are unsaved changes
            if app.MainModule.Modified
                if ~strcmp(app.AppUtils.askYesNo('Your changes are not saved. Do you want to discard the changes and create a new plan?', 'Confirm'), 'Yes')
                    return;
                end
            end

            % Close existing plan if any
            app.closePlan();

            % Create NewPlanApp
            if isempty(app.NewPlanApp) || ~isvalid(app.NewPlanApp)
                app.NewPlanApp = ultrasat.planner.gui.NewPlan(app.MainModule);
            end

            % Set PlannerName field value, if logged in, use UserName, otherwise allow user to enter name
            if app.SessionHelper.isLogin(app)
                app.NewPlanApp.PlannerNameEditField.Value = app.MainModule.UserName;
                app.NewPlanApp.PlannerNameEditField.Enable = false;
            else
                app.NewPlanApp.PlannerNameEditField.Value = '';
                app.NewPlanApp.PlannerNameEditField.Enable = true;
            end

            % Show NewPlanApp and wait for user to click "Create" button
            try
                result = app.showModal(app.NewPlanApp);
            catch ME
                app.msgex('createNewPlan - showModal', ME);
                return;
            end
            if ~strcmp(result, 'Create'), return; end

            % Create new plan according to parameters in NewPlanApp
            app.msglog(sprintf('New plan type: %s ....', app.MainModule.PlanType));
            try
                obj.doCreateNewPlan(app);
            catch ME
                app.msgex('createNewPlan', ME);
            end

            %
            app.SessionHelper.setButtons(app);
        end


        function doCreateNewPlan(obj, app)

            % Create new plan according to parameters in app.NewPlanApp
            PlanType = app.NewPlanApp.PlanType;
            app.msglog(sprintf('doCreateNewPlan: PlanType: %s', PlanType));

            % Create new PlanData instance
            app.MainModule.createPlanData();

            % Call the designated function according to PlanType
            switch PlanType
                case 'HCS',   obj.doCreateNewPlanHCS(app);
                case 'LCS',   obj.doCreateNewPlanLCS(app);
                case 'DDT',   obj.doCreateNewPlanDDT(app);
                case 'AllSS', obj.doCreateNewPlanAllSS(app);
                case 'TOO',   obj.doCreateNewPlanTOO(app);
                otherwise
                    app.msglog(sprintf('doCreateNewPlan: Unknown PlanType: %s', PlanType));
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

            % Set Modified flag to true and show UniqueTargets and PlanTargets
            app.setModified('doCreateNewPlan');
            app.showUniqueTargets();
            app.showPlanTargets();
            app.setStatus('OK', 'New plan created successfully');
            app.msglog('doCreateNewPlan done');
        end


        function doCreateNewPlanHCS(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanHCS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName(app);

            % Create new uplanner instance
            upHCS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'HCS', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upHCS);

            app.MainModule.setPlanner(upHCS);
            app.setModified('doCreateNewPlanHCS');
            app.PlanParamsHelper.updatePlanParams(app);
            %app.debugSave('upHCS.mat', app.MainModule.Planner);
            app.msglog('doCreateNewPlanHCS done');
        end


        function doCreateNewPlanLCS(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanLCS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName(app);

            % Create new uplanner instance
            upLCS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'LCS', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upLCS);

            app.MainModule.setPlanner(upLCS);
            app.setModified('doCreateNewPlanLCS');
            app.PlanParamsHelper.updatePlanParams(app);
            %app.debugSave('upLCS.mat', app.MainModule.Planner);
            app.msglog('doCreateNewPlanLCS done');
        end


        function doCreateNewPlanDDT(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanDDT started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName(app);

            % Create new uplanner instance
            upDDT = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'DDT', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upDDT);

            app.MainModule.setPlanner(upDDT);
            app.setModified('doCreateNewPlanDDT');
            app.PlanParamsHelper.updatePlanParams(app);
            %app.debugSave('upDDT.mat', app.MainModule.Planner);
            app.msglog('doCreateNewPlanDDT done');
        end


        function doCreateNewPlanTOO(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanTOO started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName(app);

            % Create new uplanner instance
            upTOO = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'TOO', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upTOO);

            app.MainModule.setPlanner(upTOO);
            app.setModified('doCreateNewPlanTOO');
            app.PlanParamsHelper.updatePlanParams(app);
            %app.debugSave('upTOO.mat', app.MainModule.Planner');
            app.msglog('doCreateNewPlanTOO done');
        end


        function doCreateNewPlanAllSS(obj, app)
            % Create new plan according to parameters in app.NewPlanApp
            app.msglog('doCreateNewPlanAllSS started');

            % Get logged-in user name, or user name entered in the dialog
            UserName = obj.getNewPlanUserName(app);

            % Create new uplanner instance
            upAllSS = ultrasat.planner.uplanner('AstPlanner', UserName, 'Type', 'AllSS', 'BaseDataDir', app.MainModule.BaseDataDir);
            obj.setNewPlanDataFromCreateDialog(app, upAllSS);

            app.MainModule.setPlanner(upAllSS);
            app.setModified('doCreateNewPlanAllSS');
            app.PlanParamsHelper.updatePlanParams(app);
            %app.debugSave('upLCS.mat', app.MainModule.Planner);
            app.msglog('doCreateNewPlanAllSS done');
        end


        function UserName = getNewPlanUserName(obj, app)
            % Get logged-in user name, or user name entered in NewPlanApp dialog
            if app.SessionHelper.isLogin(app)
                UserName = app.MainModule.UserName;
            else
                UserName = strtrim(app.NewPlanApp.PlannerNameEditField.Value);
            end
        end


        function setNewPlanDataFromCreateDialog(obj, app, Planner)
            % Set planner data from the create dialog: PlanTitle, StartTime, EndTime

            if isempty(app.NewPlanApp)
                app.msglog('setNewPlanDataFromCreateDialog: NewPlanApp not initialized');
                return;
            end
            
            % Get PlanTitle, StartTime, EndTime from NewPlanApp dialog
            PlanTitle = app.MainModule.GuiHelper.getFieldTitle( app.NewPlanApp.TitleEditField.Value );
            StartTime = app.MainModule.GuiHelper.getFieldDateTime( app.NewPlanApp.StartTimeEditField.Value );
            EndTime = app.MainModule.GuiHelper.getFieldDateTime( app.NewPlanApp.EndTimeEditField.Value );

            % Set Planner data
            Planner.Title = PlanTitle;
            Planner.StartTime = StartTime;
            Planner.EndTime = EndTime;
        end

    end

    % =====================================================================
    %                           Helper Methods
    % =====================================================================

    methods (Access = private)
    end

end
