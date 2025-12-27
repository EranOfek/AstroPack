%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainPlotHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 26/12/2025
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
            if ~obj.hasData(app)
                obj.clearPlots(app);
                return;
            end

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
            if ~obj.hasData(app), return; end

            % Create and show PlotSkyMapApp
            if isempty(app.PlotSkyMapApp) || ~isvalid(app.PlotSkyMapApp)
                app.PlotSkyMapApp = ultrasat.planner.gui.PlotSkyMap(app.MainModule);
            end
            app.PlotSkyMapApp.UIFigure.Visible = 'on';
        end

        % =================================================================
        %          GRAPHS PLOT FOR UNIQE-TARGET - BY RADIO BUTTON 
        % =================================================================

        function plotGraphs(obj, app)
            % Plot CalibObj or Visibility according to selected radio button

            app.msglog('plotGraphs');
            try
                % No planner object - just clear the graphs                
                if ~obj.hasData(app)
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
            if  ~obj.hasData(app), return; end

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
            if ~obj.hasData(app), return; end
            Planner = app.MainModule.Planner;

            % Get index of selected unique target in the drop-down
            UniqueTargetIndex = app.UniqueTargetsHelper.getUniqueTargetIndexFromDropDown(app);

            try
                % Check if the unique target has a calibration object, if not, clear the plot and return
                HasCalObj = ~isempty( Planner.UniqTarg.CalObj{UniqueTargetIndex} );
                if ~HasCalObj
                    cla(app.AxesGraphsPlot, 'reset');
                    if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                        cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
                    end
                    return;
                end

                % Get table of CalibObj, check that it is not empty
                CalibObjTable = Planner.getCalibObj(UniqueTargetIndex);
                if isempty(CalibObjTable) || height(CalibObjTable) == 0
                    app.setStatus('Warning', 'showCalibObj returned none')
                    return
                end

                 % Plot the CalibObj spectrum
                cla(app.AxesGraphsPlot, 'reset');
                Planner.plotCalibSpectrum(CalibObjTable, 'subInd2plot', 1, 'AxesHandle', app.AxesGraphsPlot);

                % Plot the CalibObj spectrum in the standalone window
                if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                    cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
                    Planner.plotCalibSpectrum(CalibObjTable, 'subInd2plot', 1, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot);
                end
            catch ME
                app.msgex('plotCalibObj', ME);
            end
        end

        % =================================================================
        %                         VISIBILITY PLOT
        % =================================================================

        function plotVisibility(obj, app)
            % Plot Visibility graph of currently select Unique Target

            app.msglog('plotVisibility');
            if ~obj.hasData(app), return; end
            Planner = app.MainModule.Planner;

            try
                % Get index of selected unique target in the drop-down
                UniqueTargetIndex = app.UniqueTargetsHelper.getUniqueTargetIndexFromDropDown(app);
                if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                    return
                end
    
                % Get Sun, Earth, Moon checks, and Time value (UTC/JD)
                SunFlag = app.PlotFlagVisibilitySunCheckBox.Value;
                EarthFlag = app.PlotFlagVisibilityEarthCheckBox.Value;
                MoonFlag = app.PlotFlagVisibilityMoonCheckBox.Value;
                TimeUnits = app.VisibilityPlotTimeUnitsDropDown.Value;
                TimeUTC = strcmpi(string(TimeUnits), 'UTC');
                
                % Update the plot embedded in this window
                cla(app.AxesGraphsPlot, 'reset');
                Planner.plotVisibility(UniqueTargetIndex, 'AxesHandle', app.AxesGraphsPlot, 'plotSun', SunFlag, 'plotEarth', EarthFlag, 'plotMoon', MoonFlag, 'TimeUTC', TimeUTC);

                % Update also the plot in the standalone window
                if ~isempty(app.PlotGraphsApp)
                    cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
                    Planner.plotVisibility(UniqueTargetIndex, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot, 'plotSun', SunFlag, 'plotEarth', EarthFlag, 'plotMoon', MoonFlag, 'TimeUTC', TimeUTC);
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
            % Check if there is data in the Planner (UniqTarg and Plan tables)
            Result = app.hasPlanner() && (height(app.MainModule.Planner.UniqTarg) > 0) || (height(app.MainModule.Planner.Plan) > 0);
        end

    end

end
