%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainApprovedTargetsHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 06/10/2025
% Description : Approved Targets Helper for Main Planner
%==========================================================================

classdef PlannerMainApprovedTargetsHelper < ultrasat.api.Loggable
    
    methods
        
        function obj = PlannerMainApprovedTargetsHelper()
            % Constructor
            obj.LogPrefix = 'ApprovedTargetsHelper';
            obj.msglog('PlannerMainApprovedTargetsHelper created successfully');
        end


        function retrieveApprovedTargets(obj, app)
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


        function showApprovedTargets(obj, app)
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


        function clearApprovedTargets(obj, app)
            % Clear the list of approved targets
            app.msglog('clearApprovedTargets');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            app.MainModule.Planner.clearMissionApprovedPlan();
            app.showPlanAll();
        end


        function approvedTargetSelected(obj, app, Index)
            % Called on selecting (single click) approved target from table
            Data = app.getSelectedTableRowAsStruct(app.MainModule.Planner.MissionApprovedPlan, Index);
            if ~isempty(Data)
                app.msglog(sprintf('approvedTargetSelected: %d - %s', Index, Data.Name));
            end
        end        


        function showOverriddenApprovedTargets(obj, app, PlanTargetIndex)
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


        function showApprovedTargetsWindow(obj, app)
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

    end
end

