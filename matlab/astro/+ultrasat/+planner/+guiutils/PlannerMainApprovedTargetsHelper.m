%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainApprovedTargetsHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 18/12/2025
% Description : Approved Targets Helper for Main Planner
%==========================================================================

classdef PlannerMainApprovedTargetsHelper < ultrasat.api.core.Loggable
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

        function obj = PlannerMainApprovedTargetsHelper()
            % Constructor
            obj.LogPrefix = 'ApprovedTargetsHelper';
        end

        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function retrieveApprovedTargets(obj, app)
            % Retreive the list of approved targets from the backend server

            % User must be connected to server and logged-in
            app.msglog('retrieveApprovedTargets');
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end

            % If build has been already executed, make sure that user is
            % aware of the meaning of this operation
            if app.MainModule.AfterBuild
                if ~strcmp(app.AppUtils.askYesNo('Retrieving approved targets after build may result in inconsistent plan. Are you sure you want to retrieve mission approved targets?', 'Confirm'), 'Yes')
                    return;
                end
            end

            app.showPleaseWait('Retreiving approved targets. This make take a while. Please wait...');
            try
                app.MainModule.Planner.retrieveMissionApprovedPlan();
            catch ME
                app.msgex('retrieveApprovedTargets', ME);
            end
            app.closePleaseWait();

            % Update GUI with updated list of targets
            obj.showApprovedTargets(app);
        end


        function clearApprovedTargets(obj, app)
            % Clear the list of approved targets

            % Do nothing if Planner is not available
            app.msglog('clearApprovedTargets');
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end
            if height(app.MainModule.Planner.MissionApprovedPlan) == 0, return; end

            % Ask user confirmation
            if ~strcmp(app.AppUtils.askYesNo('Clear all approved targets?', 'Confirm'), 'Yes')
                return;
            end

            try
                % Call uplanner to clear the list of approved targets
                app.MainModule.Planner.clearMissionApprovedPlan();

                % Refresh display
                app.showPlanAll();
            catch ME
                app.msgex('clearApprovedTargets', ME);
            end
        end

        % =================================================================
        %                         DISPLAY / UPDATE
        % =================================================================

        function showApprovedTargets(obj, app)
            % Update the GUI of Approved Targets table

            % Do nothing if Planner is not available
            app.msglog('showApprovedTargets');
            if ~app.hasPlanner()
                app.UITableApprovedTargets.Data = [];
                return;
            end

            % Set table properties
            app.showPleaseWait('Updating approved targets display...');
            try
                % Set table properties to allow single row selection and no multi-selection
                app.UITableApprovedTargets.SelectionType = "row";
                app.UITableApprovedTargets.Multiselect = "off";
                app.UITableApprovedTargets.RowName = "numbered";
                app.UITableApprovedTargets.ColumnSortable = true;

                % Set table data from Planner
                Data = app.MainModule.Planner.MissionApprovedPlan;
                Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);

                % Add Index column with the row number
                Data = addvars(Data, (1:height(Data))', 'Before', 1, 'NewVariableNames', 'Index');

                % Apply style to the entire 'Index' column
                s = uistyle("BackgroundColor",[1.00,0.99,0.82]); % Cream color
                addStyle(app.UITableApprovedTargets, s, "column", 1);                

                % Set table data
                app.UITableApprovedTargets.Data = Data;

                % Update the column names
                if ~isempty(Data)
                    app.UITableApprovedTargets.ColumnName = Data.Properties.VariableNames;
                end

                % Update title above the table with current date
                app.ApprovedTargetsPanel.Title = sprintf('Approved Targets: (%s - %s)  - Retrieved: %s', ...
                    ultrasat.api.utils.DateTimeUtils.datetimeStr(app.MainModule.Planner.LastApprovedTargetsWindowStart), ...
                    ultrasat.api.utils.DateTimeUtils.datetimeStr(app.MainModule.Planner.LastApprovedTargetsWindowEnd), ...
                    ultrasat.api.utils.DateTimeUtils.datetimeStr(app.MainModule.Planner.RetrivedMissionTime));

                % Update the table content from PlannerMain to ApprovedTargetsApp
                if ~isempty(app.ApprovedTargetsApp) && isvalid(app.ApprovedTargetsApp)
                    app.GuiHelper.copyUITable(app.UITableApprovedTargets, app.ApprovedTargetsApp.UITable);
                end
            catch ME
                app.msgex('showApprovedTargets', ME);
            end
            app.closePleaseWait();
        end


        function showApprovedTargetsWindow(obj, app)
            % Show separate window with Approved Targets table

            app.msglog('showApprovedTargetsWindow');
            if ~app.hasPlanner(), return; end

            % Create and show ApprovedTargetsApp
            if isempty(app.ApprovedTargetsApp) || ~isvalid(app.ApprovedTargetsApp)
                app.ApprovedTargetsApp = ultrasat.planner.gui.ApprovedTargets(app.MainModule);
            end
            app.ApprovedTargetsApp.UIFigure.Visible = 'on';

            % Copy table content from PlannerMain to ApprovedTargetsApp
            if ~isempty(app.ApprovedTargetsApp) && isvalid(app.ApprovedTargetsApp)
                app.GuiHelper.copyUITable(app.UITableApprovedTargets, app.ApprovedTargetsApp.UITable);
            end
        end


        function showOverriddenApprovedTargets(obj, app, PlanTargetIndex)
            % Highlight approved targets that overlap the selected plan target
            % Refreshes the approved-targets table, then marks conflicting rows in light red.
            % :param PlanTargetIndex: row index in Planner.Plan for the selected target

            app.msglog(sprintf('showOverriddenApprovedTargets: %d', PlanTargetIndex));
            if ~app.hasPlanner(), return; end

            % Refresh full approved-targets table first (clears any prior row styles)
            obj.showApprovedTargets(app);

            % Resolve selected plan target from Planner.Plan
            PlanTarget = app.getSelectedTableRowAsStruct(app.MainModule.Planner.Plan, PlanTargetIndex);
            if isempty(PlanTarget)
                return;
            end

            % Overlap highlighting: read OverlapTargets from the plan target struct.
            % OverlapTargets holds row indexes into MissionApprovedPlan for time/field
            % conflicts; indexes are computed by uplanner during planning/build.
            try
                Targets = PlanTarget.OverlapTargets;
                if ~isempty(Targets)

                    % Apply light-red row background to each overlapping approved target
                    Style = uistyle("BackgroundColor", [1, 0.6, 0.6]);
                    addStyle(app.UITableApprovedTargets, Style, "row", Targets);

                    % Scroll table so the first overlapping row is visible
                    scroll(app.UITableApprovedTargets, "row", Targets(1));
                end
            catch ME
                app.msgex('showOverriddenApprovedTargets', ME);
            end
        end

        % =================================================================
        %                           UI CALLBACKS
        % =================================================================

        function approvedTargetSelected(obj, app, Index)
            % Handle approved target selection in table - Currently does NOTHING!!!
            % Called from UITable callback

            app.msglog(sprintf('approvedTargetSelected: %d', Index));
            if ~app.hasPlanner(), return; end

            % Get the selected row as struct
            Data = app.getSelectedTableRowAsStruct(app.MainModule.Planner.MissionApprovedPlan, Index);
            if ~isempty(Data)
                app.msglog(sprintf('approvedTargetSelected: %d - %s', Index, Data.Name));
            end
        end

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)
    end

end
