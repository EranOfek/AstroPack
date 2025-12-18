%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainPlotHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 26/10/2025
% Description : Plot Helper for Main Planner
%==========================================================================

classdef PlannerMainPlotHelper < ultrasat.api.Loggable
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

        function obj = PlannerMainPlotHelper()
            % Constructor
            obj.LogPrefix = 'PlotHelper';
        end


        function clearPlots(obj, app)
            % Clear the SkyMap and Graphs plots, on this window and the standalone windows.
            app.msglog('clearPlots');

            % Clear the plot in the main window
            cla(app.AxesSkymapPlot, 'reset');
            cla(app.AxesGraphsPlot, 'reset');

            % Clear the plot in the standalone PlotSkyMapApp window
            if ~isempty(app.PlotSkyMapApp) && isvalid(app.PlotSkyMapApp)
                cla(app.PlotSkyMapApp.AxesSkymapPlot, 'reset');
            end

            % Clear the plot in the standalone PlotGraphsApp window
            if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
            end
        end

        % =================================================================
        %                           SKY-MAP PLOTS
        % =================================================================

        function showSkyMapPlot(obj, app)
            % Update GUI plot with SkyMap
            app.msglog('showSkyMapPlot');

            % No planner object - just clear
            if ~app.hasPlanner() || ~obj.hasData(app)
                obj.clearPlots(app);
                return;
            end

            % Future? @Todo @Yossi
            % Get index of selected item - Currently unused - @Yossi
            % UniqueTargetIndex = app.UITableUniqueTargets.Selection;
            % if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
            %     return
            % end
            try
                % Update the plot embedded in this window
                obj.doPlotSkyMap(app, app.AxesSkymapPlot);

                % Update also the plot in the standalone window
                if ~isempty(app.PlotSkyMapApp) && isvalid(app.PlotSkyMapApp)
                    obj.doPlotSkyMap(app, app.PlotSkyMapApp.AxesSkymapPlot);
                end
            catch ME
                app.msgex('showSkyMapPlot', ME);
            end
        end


        function showSkyMapPlotWindow(obj, app)
            % Show stand-alone window with SkyMap plot, the user need to
            % click the Update button in the embedded plot in this
            app.msglog('showSkyMapPlotWindow');
            if ~app.hasPlanner() || ~obj.hasData(app), return; end

            % Create and show PlotSkyMapApp
            if isempty(app.PlotSkyMapApp) || ~isvalid(app.PlotSkyMapApp)
                app.PlotSkyMapApp = ultrasat.planner.gui.PlotSkyMap(app.MainModule);
            end
            app.PlotSkyMapApp.UIFigure.Visible = 'on';
        end

        % =================================================================
        %                           GRAPHS PLOT
        % =================================================================

        function plotGraphs(obj, app)
            % Plot CalibObj or Visibility according to selected radio button

            app.msglog('plotGraphs');
            try
                % No planner object - just clear the graphs                
                if ~app.hasPlanner() || ~obj.hasData(app)
                    obj.clearPlots(app);
                    return;
                end

                % Plot CalibObj (Calibration Star)
                if app.CalibrationStarButton.Value
                    obj.plotCalibObj(app);

                % Plot Visibility
                elseif app.VisibilityButton.Value
                    obj.plotVisibility(app);
                else
                    app.msglog('plotGraphs: No Calibration Star or Visibility target selected');
                end
            catch ME
                app.msgex('plotGraphs', ME);
            end
        end


        function showGraphsPlotWindow(obj, app)
            % Show stand-alone window with Graphs plot
            app.msglog('showGraphsPlotWindow');
            if ~app.hasPlanner() || ~obj.hasData(app), return; end

            % Create and show PlotGraphsApp
            if isempty(app.PlotGraphsApp) || ~isvalid(app.PlotGraphsApp)
                app.PlotGraphsApp = ultrasat.planner.gui.PlotGraphs(app.MainModule);
            end
            app.PlotGraphsApp.UIFigure.Visible = 'on';
        end

        % =================================================================
        %                          CALIB OBJ PLOT
        % =================================================================

        function plotCalibObj(obj, app)
            % Plot Calibration Objects graph of the currently selected Unique Target in GraphPlotUniqueTargetDropDown

            app.msglog('plotCalibObj');
            if ~app.hasPlanner() || ~obj.hasData(app), return; end
            Planner = app.MainModule.Planner;

            % Get index of selected unique target in the drop-down
            UniqueTargetIndex = find(strcmp(app.GraphPlotUniqueTargetDropDown.Value, app.GraphPlotUniqueTargetDropDown.Items));

            % Get index of selected item from DropDown
            % app.GraphPlotUniqueTargetDropDown.Value = Planner.UniqTarg.Name(UniqueTargetIndex);

            %UniqueTargetIndex = app.UITableUniqueTargets.Selection;
            %if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
            %    return
            %end

            try
                % Get table of CalibObj, check that it is not empty
                % When calling showCalibObj('PlotSpectrum', false) the
                % function return table of CalibObj, and does not plot anything
                app.UniqueTargetCalibObj = Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', false);
                if isempty(app.UniqueTargetCalibObj) || height(app.UniqueTargetCalibObj) == 0
                    app.setStatus('Warning', 'showCalibObj returned none')
                    return
                end

                % Set selected value in Unique Targets drop-down (next to the plot)
                app.GraphPlotUniqueTargetDropDown.Value = Planner.UniqTarg.Name(UniqueTargetIndex);

                % Extract unique values from the 'obj' column of the table
                ObjValues = unique(app.UniqueTargetCalibObj.obj, 'stable');

                % Set the dropdown items to these values
                app.PlotCalibObjDropDown.Items = string(ObjValues);
                app.PlotCalibObjDropDown.Value = ObjValues{1};

                % Update the plot embedded in this window
                cla(app.AxesGraphsPlot, 'reset');
                Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', 1, 'AxesHandle', app.AxesGraphsPlot);

                % Update also the plot in the standalone window
                if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                    cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
                    Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', 1, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot);
                end
            catch ME
                app.msgex('plotCalibObj', ME);
            end
        end


        function showCalibObjTable(obj, app)
            % showCalibObjTable  Open (or create) the CalibObjTable window and update its content.
            %
            % This function ensures the CalibObjTable window exists and is valid,
            % makes it visible, and populates it with UniqueTargetCalibObj data if available.
            % All errors are logged via app.msglog, never thrown.

            app.msglog('showCalibObjTable');
            if ~obj.hasData(app), return; end

            try
                % Ensure the CalibObjTable app instance is valid
                if isempty(app.CalibObjTableApp) || ~isvalid(app.CalibObjTableApp)
                    app.CalibObjTableApp = ultrasat.planner.gui.CalibObjTable(app.MainModule);
                end

                % Make the figure visible if it exists
                if ~isempty(app.CalibObjTableApp) && isvalid(app.CalibObjTableApp)
                    app.CalibObjTableApp.UIFigure.Visible = 'on';

                    % Update the table data
                    if isprop(app.CalibObjTableApp, 'UITableData')
                        app.CalibObjTableApp.UITableData.Data = app.UniqueTargetCalibObj;
                        app.CalibObjTableApp.UITableData.ColumnSortable = true;

                        % Update column names if table is non-empty
                        if ~isempty(app.UniqueTargetCalibObj) && istable(app.UniqueTargetCalibObj)
                            app.CalibObjTableApp.UITableData.ColumnName = ...
                                app.UniqueTargetCalibObj.Properties.VariableNames;
                        else
                            app.msglog('showCalibObjTable: UniqueTargetCalibObj is empty or not a table.');
                        end
                    else
                        app.msglog('showCalibObjTable: UITableData property not found in CalibObjTableApp.');
                    end
                else
                    app.msglog('showCalibObjTable: CalibObjTableApp is invalid and could not be created.');
                end

            catch ME
                app.msglog(sprintf('showCalibObjTable: unexpected error - %s', ME.message));
            end
        end


        function plotCalibObjSub(obj, app)
            % % Plot the selected calibration object (sub-component) in both embedded and standalone plot windows
            % Called on selecting CalibObj in the drop-down next to the Graphs plot

            app.msglog('plotCalibObjSub');
            if ~obj.hasData(app), return; end

            try
                % Update the plot embedded in this window
                Value = app.PlotCalibObjDropDown.Value;
                if isempty(Value) || strcmp(Value, '')
                    app.msglog('plotCalibObjSub: No value in CalObj DropDown');
                    return;
                end
                
                % app.UniqueTargetCalibObj is table returned by Planner.showCalibObj()        
                CalObjIndex = find(strcmp(app.UniqueTargetCalibObj.obj, Value));
                if isempty(CalObjIndex)
                    app.msglog('plotCalibObjSub: CalObj not found in UniqueTargetCalibObj (table returned by Planner.showCalibObj)');
                    return;
                end                

                % Get index of selected unique target
                UniqueTargetIndex = app.UITableUniqueTargets.Selection;

                % No selection in the table
                if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                
                    % If there is exactly one unique target, use it
                    if height(app.MainModule.Planner.UniqTarg) == 1
                        UniqueTargetIndex = 1;
                
                    % If multiple exist, determine index from dropdown selection
                    elseif height(app.MainModule.Planner.UniqTarg) > 1
                        UniqueTargetIndex = find(strcmp(app.GraphPlotUniqueTargetDropDown.Value, ...
                                                        app.GraphPlotUniqueTargetDropDown.Items));
                
                        % If still not found, default to first
                        if isempty(UniqueTargetIndex)
                            UniqueTargetIndex = 1;
                        end
                
                    % If no targets exist, just return
                    else
                        return;
                    end
                end               

                % Update the plot embedded in this window
                cla(app.AxesGraphsPlot, 'reset');
                app.MainModule.Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', CalObjIndex, 'AxesHandle', app.AxesGraphsPlot);

                % Update also the plot in the standalone window
                if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                    cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
                    app.MainModule.Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', CalObjIndex, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot);
                end
            catch ME
                app.msgex('plotCalibObjSub', ME);
            end
        end

        % =================================================================
        %         OTHER WINDOWS - NOT IMPLEMENTED YET (18/12/2025)
        % =================================================================

        function showRefImagesTable(obj, app)
            % showRefImagesTable Open (or create) the RefImagesTable window and update its content.
            %
            % This function ensures the RefImagesTable window exists and is valid,
            % makes it visible, and populates it with RefImages data if available.
            % All errors are logged via app.msglog, never thrown.

            app.msglog('showRefImagesTable');
            if ~obj.hasData(app), return; end

            try
                % Ensure the CalibObjTable app instance is valid
                if isempty(app.RefImagesTableApp) || ~isvalid(app.RefImagesTableApp)
                    app.RefImagesTableApp = ultrasat.planner.gui.RefImagesTable(app.MainModule);
                end

                % Make the figure visible if it exists
                if ~isempty(app.RefImagesTableApp) && isvalid(app.RefImagesTableApp)
                    app.RefImagesTableApp.UIFigure.Visible = 'on';

                    % Update the table data
                    if isprop(app.RefImagesTableApp, 'UITableData')
                        app.RefImagesTableApp.UITableData.Data = app.UniqueTargetCalibObj;
                        app.RefImagesTableApp.UITableData.ColumnSortable = true;

                        % Update column names if table is non-empty
                        if ~isempty(app.UniqueTargetCalibObj) && istable(app.UniqueTargetCalibObj)
                            app.RefImagesTableApp.UITableData.ColumnName = ...
                                app.UniqueTargetCalibObj.Properties.VariableNames;
                        else
                            app.msglog('showRefImagesTable: UniqueTargetCalibObj is empty or not a table.');
                        end
                    else
                        app.msglog('showRefImagesTable: UITableData property not found in RefImagesTableApp.');
                    end
                else
                    app.msglog('showRefImagesTable: RefImagesTableApp is invalid and could not be created.');
                end

            catch ME
                app.msglog(sprintf('showRefImagesTable: unexpected error - %s', ME.message));
            end
        end


        function showExtSurveysTable(obj, app)
            % showExtSurveysTable Open (or create) the ExtSurveysTable window and update its content.
            %
            % This function ensures the ExtSurveysTable window exists and is valid,
            % makes it visible, and populates it with ExtSurveys data if available.
            % All errors are logged via app.msglog, never thrown.

            app.msglog('showExtSurveysTable');
            if ~obj.hasData(app), return; end

            try
                % Ensure the CalibObjTable app instance is valid
                if isempty(app.ExtSurveysTableApp) || ~isvalid(app.ExtSurveysTableApp)
                    app.ExtSurveysTableApp = ultrasat.planner.gui.ExtSurveysTable(app.MainModule);
                end

                % Make the figure visible if it exists
                if ~isempty(app.ExtSurveysTableApp) && isvalid(app.ExtSurveysTableApp)
                    app.ExtSurveysTableApp.UIFigure.Visible = 'on';

                    % Update the table data
                    if isprop(app.ExtSurveysTableApp, 'UITableData')
                        app.ExtSurveysTableApp.UITableData.Data = app.UniqueTargetCalibObj;
                        app.ExtSurveysTableApp.UITableData.ColumnSortable = true;

                        % Update column names if table is non-empty
                        if ~isempty(app.UniqueTargetCalibObj) && istable(app.UniqueTargetCalibObj)
                            app.ExtSurveysTableApp.UITableData.ColumnName = ...
                                app.UniqueTargetCalibObj.Properties.VariableNames;
                        else
                            app.msglog('showExtSurveysTable: UniqueTargetCalibObj is empty or not a table.');
                        end
                    else
                        app.msglog('showExtSurveysTable: UITableData property not found in ExtSurveysTableApp.');
                    end
                else
                    app.msglog('showExtSurveysTable: ExtSurveysTableApp is invalid and could not be created.');
                end

            catch ME
                app.msglog(sprintf('showExtSurveysTable: unexpected error - %s', ME.message));
            end
        end


        function showFieldObjTable(obj, app)
            % showFieldObjTable Open (or create) the FieldObjTable window and update its content.
            %
            % This function ensures the FieldObjTable window exists and is valid,
            % makes it visible, and populates it with FieldObj data if available.
            % All errors are logged via app.msglog, never thrown.

            app.msglog('showFieldObjTable');
            if ~obj.hasData(app), return; end

            try
                % Ensure the CalibObjTable app instance is valid
                if isempty(app.FieldObjTableApp) || ~isvalid(app.FieldObjTableApp)
                    app.FieldObjTableApp = ultrasat.planner.gui.FieldObjTable(app.MainModule);
                end

                % Make the figure visible if it exists
                if ~isempty(app.FieldObjTableApp) && isvalid(app.FieldObjTableApp)
                    app.FieldObjTableApp.UIFigure.Visible = 'on';

                    % Update the table data
                    if isprop(app.FieldObjTableApp, 'UITableData')
                        app.FieldObjTableApp.UITableData.Data = app.UniqueTargetCalibObj;
                        app.FieldObjTableApp.UITableData.ColumnSortable = true;

                        % Update column names if table is non-empty
                        if ~isempty(app.UniqueTargetCalibObj) && istable(app.UniqueTargetCalibObj)
                            app.FieldObjTableApp.UITableData.ColumnName = ...
                                app.UniqueTargetCalibObj.Properties.VariableNames;
                        else
                            app.msglog('showFieldObjTable: UniqueTargetCalibObj is empty or not a table.');
                        end
                    else
                        app.msglog('showFieldObjTable: UITableData property not found in FieldObjTableApp.');
                    end
                else
                    app.msglog('showFieldObjTable: FieldObjTableApp is invalid and could not be created.');
                end

            catch ME
                app.msglog(sprintf('showFieldObjTable: unexpected error - %s', ME.message));
            end
        end

        % =================================================================
        %                         VISIBILITY PLOT
        % =================================================================

        function plotVisibility(obj, app)
            % Plot Visibility graph of currently select Unique Target

            if ~app.hasPlanner() || ~obj.hasData(app), return; end
            Planner = app.MainModule.Planner;

            try
                % Get index of selected unique target in the drop-down
                UniqueTargetIndex = find(strcmp(app.GraphPlotUniqueTargetDropDown.Value, app.GraphPlotUniqueTargetDropDown.Items));
    
                % Get index of selected item
                %UniqueTargetIndex = app.UITableUniqueTargets.Selection;
                %if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                %    return
                %end
                
                % Update the plot embedded in this window
                cla(app.AxesGraphsPlot, 'reset');
                Planner.plotVisibility(UniqueTargetIndex, 'AxesHandle', app.AxesGraphsPlot);

                % Update also the plot in the standalone window
                if ~isempty(app.PlotGraphsApp)
                    cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
                    Planner.plotVisibility(UniqueTargetIndex, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot);
                end
            catch ME
                app.msgex('plotVisibility', ME);
            end
        end

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)

        function doPlotSkyMap(obj, app, AxesHandle)
            % Plot SkyMap on the specified Axes (embedded or stand-alone)
            app.msglog('doPlotSkyMap');
            try
                Planner = app.MainModule.Planner;
                cla(AxesHandle, 'reset');
                Planner.plotMapPlan('AxesHandle', AxesHandle, ...
                    'disp_uniqTarg', app.PlotFlagUniqueCheckBox.Value, ...
                    'disp_plan',  app.PlotFlagPlanCheckBox.Value, ...
                    'ExtinctionMap',   app.PlotFlagExtinctionCheckBox.Value, ...
                    'CalObjMap', app.PlotFlagCalibrationCheckBox.Value, ...
                    'disp_MissAprvPlan', app.PlotFlagApprovedCheckBox.Value, ...
                    'vis_at_time_map', app.PlotFlagVisibleCheckBox.Value);  % , ...

                    % @TODO: Currently not implemented:

                    % 'cooSys', app.PlotCooSysDropDown.Value, ...
                    % 'plotTstart', app.MainModule.GuiHelper.getFieldDateTime(app.PlotStartTimeEditField.Value), ...
                    % 'plotTend', app.MainModule.GuiHelper.getFieldDateTime(app.PlotEndTimeEditField.Value) );

                    % Currently not implemented:
                    % In addition, there are 3 lists that can be set to select a subsample from
                    % the full sample: 'UniqTargInds', 'plan_rows', 'MissAprvPlan_rows'.
                    % If they are empty (the default) will plot the entire UniqTarg list / Plan / MissionApprovedPlan.
            catch ME
                app.msgex('doPlotSkyMap', ME);
            end
        end


        function Result = hasData(obj, app)
            Result = app.hasPlanner() && (height(app.MainModule.Planner.UniqTarg) > 0) || (height(app.MainModule.Planner.Plan) > 0);
        end

        % =================================================================
        %                        CURRENTLY UNUSED
        % =================================================================

        function uniqueTargetSelectedInPlot(obj, app, UniqueTargetIndex)
            % Currently unused

            app.msglog('uniqueTargetSelectedInPlot');
            if ~app.hasPlanner(), return; end
            Planner = app.MainModule.Planner;


            %
            app.GraphPlotUniqueTargetDropDown.Value = Planner.UniqTarg.Name(UniqueTargetIndex);

            %
            app.UniqueTargetCalibObj = Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', false);
            if isempty(app.UniqueTargetCalibObj) || height(app.UniqueTargetCalibObj) == 0
                app.setStatus('Warning', 'showCalibObj returned none')
                return
            end

            if ~isempty(app.CalibObjTableApp)
                app.CalibObjTableApp.UITableData.Data = app.UniqueTargetCalibObj;
                app.CalibObjTableApp.UITableData.ColumnName = app.UniqueTargetCalibObj.Properties.VariableNames;
            end

            % Extract unique values from the 'obj' column of the table
            ObjValues = unique(app.UniqueTargetCalibObj.obj, 'stable');

            % Set the dropdown items to these values
            app.PlotCalibObjDropDown.Items = string(ObjValues);
            app.PlotCalibObjDropDown.Value = ObjValues{1};
        end

    end

end

