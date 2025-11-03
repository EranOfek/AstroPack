%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainSubmitHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 21/10/2025
% Description : Submit Helper for Main Planner
%==========================================================================
% @TODO - Check again code review especially for submi()

classdef PlannerMainSubmitHelper < ultrasat.api.Loggable
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

        function obj = PlannerMainSubmitHelper()
            % Constructor
            obj.LogPrefix = 'SubmitHelper';
        end

        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function submit(obj, app)
            % Submit plan by sending it to Mission Control service
            % Debug: see files in D:\Ultrasat\AstroPack\matlab\astro\+ultrasat\+api\sim
            app.msglog('submit');
            if ~app.hasPlanner(), return; end

            % Submit is not allowed when plan is read-only
            if app.isReadOnlyMsg(), return; end

            % Submit is not allowed when not logged-in
            if ~app.SessionHelper.isLogin(app, true), return; end

            % Must save before submit, because backend need to access the
            % plan in the database.
            if app.MainModule.Modified
                if ~strcmp(app.AppUtils.askYesNo('The plan has been modified and not saved. You must save it before submitting. Do you want to save your changes?', 'Confirm'), 'Yes')
                    return;
                end
                app.StorageHelper.savePlan(app);
            end

            if ~strcmp(app.MainModule.Planner.Status, 'validated')
                if ~strcmp(app.AppUtils.askYesNo('The plan is not validated, or validation was not successful. Are you sure you want to submit this plan?', 'Confirm'), 'Yes')
                    return;
                end
            end

            % Ask user for confirmation
            if ~strcmp(app.AppUtils.askYesNo('Submit this plan to Mission Control? Are you sure?', 'Confirm'), 'Yes')
                return;
            end

            app.showPleaseWait('Submitting your plan. This may take a while. Please wait...');
            try
                % Send submit request to backend, uplanner.submit() calls MissionClient.submitPlan().
                app.MainModule.Planner.submit();

                app.MainModule.PlanData.setStatus('SubmitStatus', 'OK');
                app.addHistory('submit');
                app.StorageHelper.savePlan(app);

                % After submit the plan should become read-only.
                app.setReadOnly(true);
            catch ME
                app.msgex('submit', ME);
            end
            app.closePleaseWait();
            app.updateStatus();
        end


        % =================================================================
        %                         DISPLAY / UPDATE
		% =================================================================

        function showSubmitStatusWindow(obj, app)
            % Show window with submit status
            app.msglog('showSubmitStatusWindow');
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
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)
    end

end
