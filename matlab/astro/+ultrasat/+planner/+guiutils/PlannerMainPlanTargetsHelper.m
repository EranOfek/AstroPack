%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainPlanTargetsHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 21/10/2025
% Description : Plan Targets Helper for Main Planner
%==========================================================================

classdef PlannerMainPlanTargetsHelper < ultrasat.api.Loggable
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

        function obj = PlannerMainPlanTargetsHelper()
            % Constructor
            obj.LogPrefix = 'PlanTargetsHelper';
        end

        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function editPlanTarget(obj, app)
            % Edit plan target by editPlanRow()

            app.msglog('editPlanTarget');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            % Get index of selectred plan target
            Index = app.UITablePlanTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end

            % Create PlanTargetParamsApp
            if isempty(app.PlanTargetParamsApp) || ~isvalid(app.PlanTargetParamsApp)
                app.PlanTargetParamsApp = ultrasat.planner.gui.PlanTargetParams(app.MainModule);
            end

            % Set field values - Currently there are 23 fields for Plan Target
            ParamsApp = app.PlanTargetParamsApp;
            Plan = app.MainModule.Planner.Plan;
            obj.setPlanTargetParamsFields(app, Plan, Index, ParamsApp);
            ParamsApp.setEditMode(false);

            % Show PlanTargetParamsApp and wait for user to click "Save" button
            if strcmp(app.showModal(app.PlanTargetParamsApp), 'Save')
                try
                    % Apply the paramters from the dialog to the plan
                    obj.applyPlanTargetParams(app, Index, ParamsApp);
                catch ME
                    app.msgex('editPlanRow', ME);
                end

                % Refresh GUI
                app.showPlanAll();
                app.setModified('editPlanTarget');
            end
        end


        function deletePlanTarget(obj, app)
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


        function clearPlanTargets(obj, app)
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


        function adjustGroupStartTime(obj, app)
            % Adjust group of targets with adjustGroupStartTime()

            app.msglog('adjustGroupStartTime');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            % Create app
            if isempty(app.AdjustGroupStartTimeApp) || ~isvalid(app.AdjustGroupStartTimeApp)
                app.AdjustGroupStartTimeApp = ultrasat.planner.gui.AdjustGroupStartTime(app.MainModule);
            end

            try
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
                    app.showPlanAll();
                end
            catch ME
                app.msgex('adjustGroupStartTime', ME)
            end
        end

        % =================================================================
        %                         DISPLAY / UPDATE
		% =================================================================

        function showPlanTargets(obj, app)
            % Update the display of Plan Targets table

            app.msglog('showPlanTargets');
            if ~app.hasPlanner()
                app.UITablePlanTargets.Data = [];
                return;
            end

            app.showPleaseWait('Updating plan targets display...');
            try
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

                % Copy table content from PlannerMain to PlanTargetsApp
                if ~isempty(app.PlanTargetsApp) && isvalid(app.PlanTargetsApp)
                    app.GuiHelper.copyUITable(app.UITablePlanTargets, app.PlanTargetsApp.UITable);
                end
            catch ME
                app.msgex('showPlanTargets', ME)
            end
            app.closePleaseWait();
        end


        function showPlanTargetsWindow(obj, app)
            % Show separate window with Plan Targets table
            app.msglog('showPlanTargetsWindow');
            if ~app.hasPlanner(), return; end

            % Create and show PlanTargetsApp
            if isempty(app.PlanTargetsApp) || ~isvalid(app.PlanTargetsApp)
                app.PlanTargetsApp = ultrasat.planner.gui.PlanTargets(app.MainModule);
            end
            app.PlanTargetsApp.UIFigure.Visible = 'on';

            % Copy table content from PlannerMain to PlanTargetsApp
            if ~isempty(app.PlanTargetsApp) && isvalid(app.PlanTargetsApp)
                app.GuiHelper.copyUITable(app.UITablePlanTargets, app.PlanTargetsApp.UITable);
            end
        end

        % =================================================================
        %                           UI Callbacks
        % =================================================================

        function planTargetSelected(obj, app, Index)
            % Handle plan target selection (single click), called from UITable callback
            % Called from UITable callback

            app.msglog(sprintf('Plan target selected: %d', Index));
            if ~app.hasPlanner(), return; end
            try
                Data = app.getSelectedTableRowAsStruct(app.MainModule.Planner.Plan, Index);
                if ~isempty(Data)
                    app.msglog(sprintf('planTargetSelected: %d - %s', Index, Data.Name));
                    app.ApprovedTargetsHelper.showOverriddenApprovedTargets(app, Index);
                end
            catch ME
                app.msgex('planTargetSelected', ME)
            end
        end


        function planTargetClick(obj, app)
            % Handle plan target single click - Select the corresponding Unique-Target
            % Called from UITable callback

            app.msglog('planRowClick');
            if ~app.hasPlanner(), return; end
            try
                Index = app.UITablePlanTargets.Selection;
                if isempty(Index) || (Index < 1)
                    return
                end

                % Select the corresponding Unique-Target
                UniqueTargetIndex = app.MainModule.Planner.Plan.UniqTargInd(Index);
                app.UITableUniqueTargets.Selection = UniqueTargetIndex;
            catch ME
                app.msgex('planRowClick', ME)
            end
        end


        function planTargetDoubleClick(obj, app)
            % Handle plan target double click - Select the corresponding Unique-Target and show graphs
            % Called from UITable callback

            app.msglog('planRowDoubleClick');
            if ~app.hasPlanner(), return; end
            try
                Index = app.UITablePlanTargets.Selection;
                if isempty(Index) || (Index < 1)
                    return
                end

                % Select the Unique-Target
                UniqueTargetIndex = app.MainModule.Planner.Plan.UniqTargInd(Index);
                app.UITableUniqueTargets.Selection = UniqueTargetIndex;

                % 'Double Click' on Unique-Target to plot its graphs
                app.UniqueTargetsHelper.uniqueTargetDoubleClick(app, UniqueTargetIndex);
                app.plotGraphs();
            catch ME
                app.msgex('planRowDoubleClick', ME)
            end
        end

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)

        % =================================================================
        %                         UTILITY HELPERS
        % =================================================================

        function setPlanTargetParamsFields(obj, app, Plan, Index, ParamsApp)
            % Set field values - Currently there are 23 fields for Plan Target

            app.msglog('setPlanTargetParamsFields');

            try
                ParamsApp.PlanTargetIndexEditField.Value = int2str(Index);

                % Editable fields
                ParamsApp.ExposureTimeEditField.Value = seconds(Plan.ExpTime(Index));  % Numeric field
                ParamsApp.EpochsPerVisitEditField.Value = Plan.Nexposures(Index);
                app.MainModule.GuiHelper.updateCheckboxesFromTiles(ParamsApp, Plan.Tiles(Index));

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
            catch ME
                app.msgex('setPlanTargetParamsFields', ME);
            end
        end


        function applyPlanTargetParams(obj, app, Index, ParamsApp)
            % Apply plan parameters from dialog to plan

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
                if app.PlanParamsHelper.checkPlanSelfConsistency(app)
                    app.msglog('applyPlanTargetParams: success');
                end
            catch ME
                app.msgex('applyPlanTargetParams', ME);
            end
        end

    end

end
