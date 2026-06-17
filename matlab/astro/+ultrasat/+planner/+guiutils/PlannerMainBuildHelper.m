%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainBuildHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 23/04/2026
% Description : Build Helper for Main Planner
%==========================================================================

classdef PlannerMainBuildHelper < ultrasat.api.core.Loggable
    % Helper class for PlannerMain.mlapp
    % Provides build logic (HCS/LCS/DDT/TOO/AllSS) for PlannerMain.mlapp.
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

    properties (Access = private)
        LcsStartTimesApp
        LastLcsStartCheckStatus = ''
    end

    methods (Access = public)

        function obj = PlannerMainBuildHelper()
            % Constructor
            obj.LogPrefix = 'BuildHelper';
        end

        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function build(obj, app)
            % Build plan according to plan type, calls doBuild...() below
            app.msglog('build');
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end
            if height(app.MainModule.Planner.UniqTarg) == 0, return; end

            % Get plan type
            PlanType = app.MainModule.Planner.Type;

            % Set AfterBuild flag to true if plan is not empty
            app.MainModule.AfterBuild = height(app.MainModule.Planner.Plan) > 0;
            if app.MainModule.AfterBuild && ~strcmp(PlanType, 'DDT')
                if ~strcmp(app.AppUtils.askYesNo('Build was already executed, this will override your existing plan. Are you sure you want to execute build?', 'Confirm'), 'Yes')
                    return;
                end
            end

            % Save current plan, if build fails we will restore it
            SavedPlan = app.MainModule.Planner.Plan;
            BuildOk = false;
            obj.LastLcsStartCheckStatus = '';

            % Clear plan if it is not DDT
            if  ~strcmp(PlanType, 'DDT')
                app.MainModule.Planner.Plan = [];
            end

            % Show "Please Wait" dialog
            app.showPleaseWait('Building your plan... expected duration: up to ~30 seconds.');
            try
                app.MainModule.clearStatus();
                app.updateStatus();
                app.msglog(sprintf('build: PlanType: %s', PlanType));

                % Call the designated function according to PlanType
                switch PlanType
                    case 'HCS',  BuildOk = obj.doBuildHCS(app);
                    case 'LCS',  BuildOk = obj.doBuildLCS(app);
                    case 'DDT',  BuildOk = obj.doBuildDDT(app);
                    case 'TOO',  BuildOk = obj.doBuildTOO(app);
                    case 'AllSS', BuildOk = obj.doBuildAllSS(app);
                    otherwise,   app.msglog(sprintf('build: Unknown PlanType "%s"', PlanType));
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

            % Restore current plan if build failed
            if ~BuildOk
                LcsStartStatus = obj.LastLcsStartCheckStatus;
                obj.LastLcsStartCheckStatus = '';

                if strcmp(PlanType, 'LCS') && strcmp(LcsStartStatus, 'updated')
                    app.MainModule.Planner.Plan = SavedPlan;
                    app.msglog('build: LCS start date updated; user must click Build again.');
                    app.MainModule.AppUtils.msgOk('LCS start date updated. Click Build again.', 'LCS Start Date');
                elseif strcmp(PlanType, 'LCS') && strcmp(LcsStartStatus, 'aborted')
                    app.MainModule.Planner.Plan = SavedPlan;
                    app.msglog('build: LCS build aborted (start date check).');
                else
                    app.msglog('build: Build failed, restoring previous plan.');
                    app.MainModule.AppUtils.msgOk('Build failed, restoring previous plan', 'Build Failed');

                    % Restore the previous plan (only for DDT)
                    if strcmp(PlanType, 'DDT')
                        app.MainModule.Planner.Plan = SavedPlan;
                        app.msglog('build: Restored previous plan');
                    end
                end
                return;
            end

            % Update display
            app.setModified('build');
            app.updateStatus();
            app.PlanTargetsHelper.showPlanTargets(app);
            app.addHistory('Build completed');
        end

        % =================================================================
        %                         DISPLAY / UPDATE
		% =================================================================

        function setBuildStatus(obj, app, Status)
            % Set build status in PlanData

            if isempty(app.MainModule.PlanData)
                app.msglog('Warning: setBuildStatus called before PlanData initialized.');
                return;
            end

            app.MainModule.PlanData.setStatus('BuildStatus', Status);
        end


        function showBuildStatusWindow(obj, app)
            % Show window with last build status
            app.msglog('showBuildStatusWindow');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.BuildStatusApp) || ~isvalid(app.BuildStatusApp)
                app.BuildStatusApp = ultrasat.planner.gui.BuildStatus(app.MainModule);
            end

            % Set fields and show the app
            %app.BuildStatusApp.setData(app.MainModule.BuildStatus);

            % If you plan to add fields to BuildStatusApp, consider:
            % so the modal always displays latest info.
            % app.BuildStatusApp.setData(app.MainModule.PlanData.getStatus());

            app.showModal(app.BuildStatusApp);
        end


        function Status = checkLcsStartDate(obj, app, upLCS)
            % Check LCS start date feasibility on Build; offer alternatives via LcsStartTimes dialog
            % Returns 'feasible', 'updated', or 'aborted'
            Status = 'aborted';
            app.msglog('checkLcsStartDate started');

            StartDate = dateshift(upLCS.StartTime, 'start', 'day');
            if obj.isLcsStartDateFeasible(app, StartDate)
                app.msglog('checkLcsStartDate: start date is feasible');
                Status = 'feasible';
                return;
            end

            app.closePleaseWait();
            app.showPleaseWait('Searching for alternative LCS start dates...');

            try
                Plans = ultrasat.planner.LcsHelper_v4_findPlans(StartDate, 2, 'Verbose', false);
                FeasibleMask = strcmp(Plans.status, 'FEASIBLE');
                Plans = Plans(FeasibleMask, :);

                if isempty(Plans) || height(Plans) == 0
                    app.closePleaseWait();
                    app.AppUtils.msgError('No feasible alternative LCS start dates found near the selected start date.');
                    return;
                end

                app.closePleaseWait();
                DialogStatus = obj.showLcsStartTimesDialog(app, Plans);
                if ~strcmp(DialogStatus, 'Ok')
                    return;
                end

                RowIndex = obj.LcsStartTimesApp.UITable.Selection;
                if isempty(RowIndex) || RowIndex < 1
                    app.AppUtils.msgError('Please select an alternative start date from the list.');
                    return;
                end

                DisplayData = obj.LcsStartTimesApp.UITable.Data;
                if RowIndex > height(DisplayData)
                    app.AppUtils.msgError('Invalid row selection.');
                    return;
                end

                PlanStartDate = string(DisplayData.plan_start_date(RowIndex));
                Duration = upLCS.EndTime - upLCS.StartTime;
                upLCS.StartTime = datetime(PlanStartDate) + upLCS.DailyWindowStartTime;
                upLCS.EndTime = upLCS.StartTime + Duration;
                app.setModified('checkLcsStartDate');
                app.PlanParamsHelper.updatePlanParams(app);
                Status = 'updated';
            catch ME
                app.closePleaseWait();
                app.msgex('checkLcsStartDate', ME);
            end

            app.msglog('checkLcsStartDate done');
        end

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)

        % =================================================================
        %                     BUILD HELPERS BY PLAN TYPE
        % =================================================================

        function BuildOk = doBuildHCS(obj, app)
            % Build HCS
            BuildOk = false;            
            app.msglog('doBuildHCS started');
            if ~app.hasPlanner(), return; end

            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = obj.getUniqueTargetsIndexByOrderColumn(app, app.UITableUniqueTargets.Data);
            if numel(SelectedRows) ~= 1
                app.AppUtils.msgError('HCS requires a single unique target, if there are multiple unique targets, select one by putting 1 in its Order column.');
                return;
            end

            % Build HCS plan
            upHCS = app.MainModule.Planner;
            upHCS.buildHCS('HCS_UniqTarg', SelectedRows);

            % Update history & status
            app.addHistory('BuildHCS Ok');
            obj.setBuildStatus(app, 'OK');
            app.MainModule.setStatus('OK', 'Build HCS completed successfully');
            %app.debugSave('upHCS.mat', upHCS);

            app.msglog('doBuildHCS done');
            BuildOk = true;
        end


        function BuildOk = doBuildLCS(obj, app)
            % Build LCS
            BuildOk = false;            
            app.msglog('doBuildLCS started');
            if ~app.hasPlanner(), return; end

            upLCS = app.MainModule.Planner;

            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = obj.getUniqueTargetsIndexByOrderColumn(app, app.UITableUniqueTargets.Data);

            if isempty(SelectedRows)
                app.AppUtils.msgError('No targets selected for LCS build.');
                return;
            end

            StartStatus = obj.checkLcsStartDate(app, upLCS);
            if ~strcmp(StartStatus, 'feasible')
                obj.LastLcsStartCheckStatus = StartStatus;
                return;
            end

            app.closePleaseWait();
            app.showPleaseWait('Building your plan... expected duration: up to ~30 seconds.');
            upLCS.buildLCS1('TargetList', SelectedRows);
            BuildOk = height(upLCS.Plan) > 0;

            if ~BuildOk
                app.AppUtils.msgError('Build failed for the selected start date.');
            end

            if BuildOk
                app.addHistory('BuildLCS Ok');
                obj.setBuildStatus(app, 'OK');
                app.MainModule.setStatus('OK', 'Build LCS completed successfully');
            end

            app.msglog('doBuildLCS done');
        end


        function BuildOk = doBuildDDT(obj, app)
            % Build DDT
            BuildOk = false;            
            app.msglog('doBuildDDT started');
            if ~app.hasPlanner(), return; end

            upDDT = app.MainModule.Planner;

            % Get list of the selected rows with 'Order' column set (or all if none of them has Order set)
            SelectedRows = obj.getUniqueTargetsIndexByOrderColumn(app, app.UITableUniqueTargets.Data);
            if isempty(SelectedRows)
                return;
            end

            % Create EnterStartTimeApp
            if isempty(app.EnterStartTimeApp) || ~isvalid(app.EnterStartTimeApp)
                app.EnterStartTimeApp = ultrasat.planner.gui.EnterStartTime(app.MainModule);
            end

            % Set start time field from the planner
            app.EnterStartTimeApp.GroupStartTimeEditField.Value = ultrasat.planner.guiutils.FormatUtils.DateTime2Str(app.MainModule.Planner.StartTime);

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
                    BuildOk = true;
                catch ME
                    app.msgex('addDDT2Plan', ME);
                end
			else
				app.AppUtils.msgOk('Building DDT cancelled - Start time is required.');
            end

            %app.debugSave('upDDT.mat', upDDT);
            app.msglog('doBuildDDT done');
        end


        function BuildOk = doBuildTOO(app)
            % Build TOO - @Todo @Yossi
            % @Todo: Implement actual TOO build logic (requires external trigger inputs)
            BuildOk = false;            
            app.msglog('doBuildTOO started');
            if ~app.hasPlanner(), return; end

            try
                upTOO = app.MainModule.Planner;

                Fields = upTOO.UniqTarg(1);
                upTOO.buildTOO('RA', Fields.RA, 'Dec', Fields.Dec, 'Name', Fields.Name);
                %app.debugSave('upTOO.mat', upTOO);
                BuildOk = true;

            catch ME
                app.msgex('doBuildTOO', ME);
            end
            app.msglog('doBuildTOO done');
        end


        function BuildOk = doBuildAllSS(obj, app)
            % Build AllSS - @Todo @Yossi
            % @Todo: Implement actual build logic
            BuildOk = false;            
            app.msglog('doBuildAllSS started');
            if ~app.hasPlanner(), return; end

            try
                % @Todo: Implement actual build logic
                % BuildOk = true;
            catch ME
                app.msgex('doBuildAllSS', ME);
            end

            app.msglog('doBuildAllSS done');
        end

        % =================================================================
        %                         LCS START DATE HELPERS
        % =================================================================

        function IsFeasible = isLcsStartDateFeasible(obj, app, StartDate)
            % Return true if StartDate is a feasible LCS v4 plan start (offset 0)
            IsFeasible = false;
            try
                Plans = ultrasat.planner.LcsHelper_v4_findPlans(StartDate, 1, ...
                    'MaxRadius', 0, 'Verbose', false);
                if height(Plans) > 0
                    IsFeasible = strcmp(Plans.status(1), 'FEASIBLE') && Plans.offset_days(1) == 0;
                end
            catch ME
                app.msgex('isLcsStartDateFeasible', ME);
            end
        end


        function Status = showLcsStartTimesDialog(obj, app, PlansTable)
            % Show LcsStartTimes modal with feasible alternative start dates
            if isempty(obj.LcsStartTimesApp) || ~isvalid(obj.LcsStartTimesApp)
                obj.LcsStartTimesApp = ultrasat.planner.gui.LcsStartTimes(app.MainModule);
            end

            obj.LcsStartTimesApp.EnterfilenametoloadfromfileorpastetextbelowLabel_2.Text = ...
                'Please choose an alternative start date, then click Build again.';

            DisplayData = PlansTable(:, {'plan_start_date', 'offset_days', 'num_observations', ...
                'nA', 'nB', 'nC', 'nD', 'variant_used'});
            obj.LcsStartTimesApp.UITable.Data = DisplayData;
            obj.LcsStartTimesApp.UITable.ColumnName = DisplayData.Properties.VariableNames;
            obj.LcsStartTimesApp.UITable.SelectionType = 'row';
            obj.LcsStartTimesApp.UITable.Multiselect = 'off';

            Status = app.showModal(obj.LcsStartTimesApp);
        end

        % =================================================================
        %                         UTILITY FUNCTIONS
        % =================================================================

        function Result = getUniqueTargetsIndexByOrderColumn(obj, app, Data)
            % Returns the row indices sorted by 'Order' column.
            % If only one row exists, returns 1.
            % If only one row has a non-empty 'Order' value, returns its index.
            % Otherwise, returns indices of rows with non-empty 'Order', sorted by value.

            try
                if isempty(Data)
                    Result = 0;
                    return;
                end

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
            catch ME
                app.msgex('getUniqueTargetsIndexByOrderColumn', ME);
                Result = 1;
            end
        end


        function Result = getUniqueTargetsIndexByOrderColumn0(obj, app, Data)
            % Deprecated version kept for reference. Use getUniqueTargetsIndexByOrderColumn().

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

    end
end
