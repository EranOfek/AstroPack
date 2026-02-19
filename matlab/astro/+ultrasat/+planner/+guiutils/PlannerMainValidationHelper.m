%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainValidationHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 11/11/2025
% Description : Submit Helper for Main Planner (Submit & Validation)
%==========================================================================
% @TODO - Check again code review especially for submi()

classdef PlannerMainValidationHelper < ultrasat.api.core.Loggable
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

        function obj = PlannerMainValidationHelper()
            % Constructor
            obj.LogPrefix = 'ValidationHelper';
        end

        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function validate(obj, app)
            % Validate plan by sending it to the Validation service
            app.msglog('validate');
            if ~app.hasPlanner(), return; end

            % Validation is not allowed when plan is read-only
            if ~app.isEditableMsg(), return; end

            % Validation is not allowed when not logged-in
            if ~app.SessionHelper.isLogin(app, true), return; end

            % Ask user to confirm - currently not
            %if ~strcmp(app.AppUtils.askYesNo('Send plan with GCS Validator?', 'Confirm'), 'Yes')
            %    return;
            %end

            % Temporary for now
            app.AppUtils.msgOk('Validation is not working yet, as IAI side is not ready yet.  Artificially changing the status to validated for now.');

            % Start validation
            app.showPleaseWait('Validating your plan. This make take a while. Please wait...');
            try
                app.MainModule.PlanData.addHistory('validation started');
                app.MainModule.Planner.validate();
                app.MainModule.PlanData.addHistory('validation end');
                app.MainModule.PlanData.setStatus('ValidationStatus', 'OK');
            catch ME
                app.msgex('Planner.validate', ME);
                app.MainModule.PlanData.setStatus('ValidationStatus', 'Error');
            end
            app.closePleaseWait();
            app.updateStatus();
            app.AppUtils.msgOk('Validation completed, see detailed status in validation report window.')

            % User will open the status window
            % app.showValidationStatusWindow();
        end


        function updateValidateStatus(obj, app)
            % Update the validation status field
            if ~isempty(app.MainModule.Planner)
                app.setStatusField(app.ValidationShortStatusEditField, app.MainModule.ValidateStatus, app.MainModule.ValidateStatusText);
                app.setStatusField(app.ValidationTimeEditField, app.MainModule.ValidateStatus, ultrasat.planner.guiutils.FormatUtils.DateTime2Str(app.MainModule.Planner.ValidatedTime));
            end
        end

        % =================================================================
        %                         DISPLAY / UPDATE
		% =================================================================

        function showValidationStatusWindow(obj, app)
            % Show window with validation status
            app.msglog('showValidationStatusWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.ValidationStatusApp) || ~isvalid(app.ValidationStatusApp)
                app.ValidationStatusApp = ultrasat.planner.gui.ValidationStatus(app.MainModule);
            end

            try
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
                obj.showValidationResponse(app, Response);

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
            catch ME
                app.msgex('showValidationStatusWindow', ME)
            end
        end


        function validationHistorySelected(obj, app)
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
                obj.showValidationResponse(app, Response);
            catch ME
                app.msgex('validationHistorySelected', ME);
            end
        end


        function showValidationResponse(obj, app, Response)
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
                Html = ultrasat.planner.guiutils.FormatUtils.jsonToHtml(Response);
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

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)
    end

end
