%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainPlotHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 06/10/2025
% Description : Plot Helper for Main Planner
%==========================================================================

classdef PlannerMainPlotHelper < ultrasat.api.Loggable

    methods
        
        function obj = PlannerMainPlotHelper()
            % Constructor
            obj.LogPrefix = 'PlotHelper';
            obj.msglog('PlannerMainPlotHelper created successfully');
        end


        function plotGraphs(obj, app)
            % Plot CalibObj or Visibility according to selected radio button
            try
                if ~app.hasPlanner()
                    app.clearPlots();
                    return;
                end

                % Plot CalibObj
                if app.CalibrationStarButton.Value
                    app.plotCalibObj();
                end

                % Plot Visibility
                if app.VisibilityButton.Value
                    app.plotVisibility();
                end                
            catch ME
                app.msgex('plotCalibObj', ME);
            end                
        end


        function clearPlots(obj, app)
            % Clear the SkyMap and Graphs plots, on this window and the standalone windows.
            app.msglog('clearPlots');
            cla(app.AxesSkymapPlot, 'reset');
            cla(app.AxesGraphsPlot, 'reset');

            if ~isempty(app.PlotSkyMapApp) && isvalid(app.PlotSkyMapApp)
                cla(app.PlotSkyMapApp.AxesSkymapPlot, 'reset');
            end

            if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');
            end
        end

        
        function showSkyMapPlot(obj, app)
            % Update GUI plot with SkyMap
            app.msglog('showSkyMapPlot');
            if ~app.hasPlanner(), return; end            

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
                app.msgex('plotMapPlan', ME);
            end   
        end

       
        function showGraphsPlotWindow(obj, app)
            % Create app
            if isempty(app.PlotGraphsApp) || ~isvalid(app.PlotGraphsApp)
                app.PlotGraphsApp = ultrasat.planner.gui.PlotGraphs(app.MainModule);                
            end
            app.PlotGraphsApp.UIFigure.Visible = 'on';            
        end

        
        function showSkyMapPlotWindow(obj, app)
            % Show stand-alone window with SkyMap plot, the user need to
            % click teh Update button in the embedded plot in this 
            app.msglog('plotCalibObj');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.PlotSkyMapApp) || ~isvalid(app.PlotSkyMapApp)
                app.PlotSkyMapApp = ultrasat.planner.gui.PlotSkyMap(app.MainModule);                
            end
            app.PlotSkyMapApp.UIFigure.Visible = 'on';
        end


        function doPlotSkyMap(obj, app, AxesHandle)
            % Plot SkyMap on the specified Axes (embedded or stand-alone)
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

        % =================================================================
        %
        % =================================================================        

        function plotCalibObj(obj, app)

            % Plot Calibration Objects graph
            app.msglog('plotCalibObj');
            if ~app.hasPlanner(), return; end

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
                

        function uniqueTargetSelectedInPlot(obj, app, UniqueTargetIndex)
            % Helper: 
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


        function plotCalibObjSub(obj, app)

            % Called on selecting CalibObj in the drop-down next to the Graphs plot
            try
                if ~isempty(app.MainModule.Planner)
                    UniqueTargetIndex = app.UITableUniqueTargets.Selection;
                    if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                        return
                    end

                    % Update the plot embedded in this window
                    Value = app.PlotCalibObjDropDown.Value;
                    CalObjIndex = find(strcmp(app.UniqueTargetCalibObj.obj, Value));

                    % Update the plot embedded in this window
                    cla(app.AxesGraphsPlot, 'reset');                
                    app.MainModule.Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', CalObjIndex, 'AxesHandle', app.AxesGraphsPlot);
    
                    % Update also the plot in the standalone window
                    if ~isempty(app.PlotGraphsApp) && isvalid(app.PlotGraphsApp)
                        cla(app.PlotGraphsApp.AxesGraphsPlot, 'reset');                
                        app.MainModule.Planner.showCalibObj(UniqueTargetIndex, 'PlotSpectrum', true, 'subInd2plot', CalObjIndex, 'AxesHandle', app.PlotGraphsApp.AxesGraphsPlot);
                    end
                end
            catch ME
                app.msgex('plotCalibObjSub', ME);
            end                            
        end


        function plotVisibility(obj, app)

            % Plot Visibility graph of currently select Unique Target
            if ~app.hasPlanner(), return; end
    
            Planner = app.MainModule.Planner;

            % Get index of selected unique target in the drop-down
            UniqueTargetIndex = find(strcmp(app.GraphPlotUniqueTargetDropDown.Value, app.GraphPlotUniqueTargetDropDown.Items));

            % Get index of selected item
            %UniqueTargetIndex = app.UITableUniqueTargets.Selection;
            %if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
            %    return
            %end

            try
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
end

