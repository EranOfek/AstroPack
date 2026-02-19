%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainTablesHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 26/12/2025
% Description : Tables (CalibObj, RefImages, ExtSurveys, FieldObj) Helper for Main Planner
%==========================================================================

classdef PlannerMainTablesHelper < ultrasat.api.core.Loggable
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

        function obj = PlannerMainTablesHelper()
            % Constructor
            obj.LogPrefix = 'TablesHelper';
        end


        % =================================================================
        %                     CALIBRATION OBJECTS TABLE
        % =================================================================
        
        function showCalibObjTable(obj, app)
            % showCalibObjTable  Open (or create) the CalibObjTable window and update its content.
            app.msglog('showCalibObjTable');
            if ~obj.hasData(app), return; end

            try
                % Ensure the CalibObjTable app instance is valid
                if isempty(app.CalibObjTableApp) || ~isvalid(app.CalibObjTableApp)
                    app.CalibObjTableApp = ultrasat.planner.gui.CalibObjTable(app.MainModule);
                end

                % Make the figure visible if it exists
                app.CalibObjTableApp.UIFigure.Visible = 'on';

                % Update the table data
                obj.updateCalibObjTable(app);
            catch ME
                app.msglog(sprintf('showCalibObjTable: unexpected error - %s', ME.message));
            end
        end
        

        function updateCalibObjTable(obj, app)
            % Update the CalibObjTable with the selected unique target.
            app.msglog('updateCalibObjTable');
            if ~obj.hasData(app), return; end
            if isempty(app.CalibObjTableApp) || ~isvalid(app.CalibObjTableApp)
                return
            end
        
            try
                % Get index of selected unique target in the drop-down
                UniqueTargetIndex = app.UniqueTargetsHelper.getUniqueTargetIndexFromDropDown(app);
                if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                    return
                end

                % Set the caption and edit fields in the table window
                obj.setTargetHeaderAndFields(app, app.CalibObjTableApp, UniqueTargetIndex, 'Calibration Objects');

                % Get CalibObj table for the selected unique target
                CalibObjTable = app.MainModule.Planner.getCalibObj(UniqueTargetIndex);
                if isempty(CalibObjTable) || height(CalibObjTable) == 0
                    return
                end

                % Update the table data
                app.CalibObjTableApp.UITableData.Data = CalibObjTable;
                app.CalibObjTableApp.UITableData.ColumnSortable = true;

                % Update column names if table is non-empty
                if ~isempty(CalibObjTable) && istable(CalibObjTable)
                    app.CalibObjTableApp.UITableData.ColumnName = CalibObjTable.Properties.VariableNames;
                else
                    app.msglog('showCalibObjTable: UniqueTargetCalibObj is empty or not a table.');
                end
            catch ME
                app.msglog(sprintf('showCalibObjTable: unexpected error - %s', ME.message));
            end
        end

        % =================================================================
        %                      REFERENCE IMAGES TABLE
        % =================================================================

        function showRefImagesTable(obj, app)
            % @TODO - NOT IMPLEMENTED YET - showRefImagesTable Open (or create) the RefImagesTable window and update its content.
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
                end

                % Update the table data
                obj.updateRefImagesTable(app);
            catch ME
                app.msglog(sprintf('showRefImagesTable: unexpected error - %s', ME.message));
            end
        end


        function updateRefImagesTable(obj, app)
            % @TODO - NOT IMPLEMENTED YET - Update the RefImagesTable with the selected unique target.
            app.msglog('updateRefImagesTable');
            if ~obj.hasData(app), return; end
            if isempty(app.RefImagesTableApp) || ~isvalid(app.RefImagesTableApp)
                return
            end

            try
                % Get index of selected unique target
                UniqueTargetIndex = app.UITableUniqueTargets.Selection;   
                if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                    return
                end

                % Set the caption and edit fields in the table window
                obj.setTargetHeaderAndFields(app, app.RefImagesTableApp, UniqueTargetIndex, 'Reference Images');

                % Get reference images table for the selected unique target
                Data = app.MainModule.Planner.getRefImagesForTarget(UniqueTargetIndex);

                % Update the table data
                app.RefImagesTableApp.UITableData.Data = Data;
                app.RefImagesTableApp.UITableData.ColumnSortable = true;

                % Update column names if table is non-empty
                if ~isempty(Data) && istable(Data)
                    app.RefImagesTableApp.UITableData.ColumnName = Data.Properties.VariableNames;
                else
                    app.msglog('updateRefImagesTable: Data is empty or not a table.');
                end            
            catch ME
                app.msglog(sprintf('updateRefImagesTable: unexpected error - %s', ME.message));
            end
        end

        % =================================================================        
        %                      EXTERNAL SURVEYS TABLE
        % =================================================================

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
                end

                % Update the table data
                obj.updateExtSurveysTable(app);
            catch ME
                app.msglog(sprintf('showExtSurveysTable: unexpected error - %s', ME.message));
            end
        end


        function updateExtSurveysTable(obj, app)
            % Update the ExtSurveysTable with the selected unique target.
            app.msglog('updateExtSurveysTable');
            if ~obj.hasData(app), return; end
            if isempty(app.ExtSurveysTableApp) || ~isvalid(app.ExtSurveysTableApp)
                return
            end

            try
                % Get index of selected unique target
                UniqueTargetIndex = app.UniqueTargetsHelper.getUniqueTargetIndexFromDropDown(app);                                        
                if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                    return
                end

                % Set the caption and edit fields in the table window
                obj.setTargetHeaderAndFields(app, app.ExtSurveysTableApp, UniqueTargetIndex, 'External Surveys');

                % Get external surveys table for the selected unique target
                Data = app.MainModule.Planner.getExtSurveysForTarget(UniqueTargetIndex);

                % Clear the data in the Shape column, but keep the column in the table
                if ismember('Shape', Data.Properties.VariableNames)
                    %Data.Shape(:) = {[]};
                end

                % Update the table data
                app.ExtSurveysTableApp.UITableData.Data = Data;
                app.ExtSurveysTableApp.UITableData.ColumnSortable = true;

                % Update column names if table is non-empty
                if ~isempty(app.UniqueTargetCalibObj) && istable(app.UniqueTargetCalibObj)
                    app.ExtSurveysTableApp.UITableData.ColumnName = ...
                        Data.Properties.VariableNames;
                else
                    app.msglog('showExtSurveysTable: UniqueTargetCalibObj is empty or not a table.');
                end
            catch ME
                app.msglog(sprintf('updateExtSurveysTable: unexpected error - %s', ME.message));
            end
        end

        % =================================================================
        %                      FIELD OBJECTS TABLE
        % =================================================================

        function showFieldObjTable(obj, app)
            % showFieldObjTable Open (or create) the FieldObjTable window and update its content.
            %
            % Ensures the FieldObjTable window exists and is valid,
            % makes it visible, and populates it with FieldObj data if available.

            app.msglog('showFieldObjTable');
            if ~obj.hasData(app), return; end

            try
                % Ensure the FieldObjTable app instance is valid
                if isempty(app.FieldObjTableApp) || ~isvalid(app.FieldObjTableApp)
                    app.FieldObjTableApp = ultrasat.planner.gui.FieldObjTable(app.MainModule);
                end

                % Make the FieldObjTable figure visible if it exists
                app.FieldObjTableApp.UIFigure.Visible = 'on';

                % Update the FieldObjTable table data
                obj.updateFieldObjTable(app);
            catch ME
                app.msglog(sprintf('showFieldObjTable: unexpected error - %s', ME.message));
            end
        end
        
        
        function updateFieldObjTable(obj, app)
            % Update the FieldObjTable with the selected unique target and table name.
            app.msglog('updateFieldObjTable');
            if ~obj.hasData(app), return; end
            if isempty(app.FieldObjTableApp) || ~isvalid(app.FieldObjTableApp)
                return
            end

            try
                % Get index of selected unique target
                UniqueTargetIndex = app.UniqueTargetsHelper.getUniqueTargetIndexFromDropDown(app);                                        
                if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                    return
                end

                % Set the caption and edit fields in the table window
                obj.setTargetHeaderAndFields(app, app.FieldObjTableApp, UniqueTargetIndex, 'Field Objects');
                
                % Get selected table name
                SelectedTableName = app.FieldObjTableApp.TableDropDown.Value;

                % Get field objects table for the selected unique target and table name
                Data = app.MainModule.Planner.getFieldObjForTarget(UniqueTargetIndex, SelectedTableName);

                % Update FieldObj counters in the UI
                FieldNames = {'TransPlanets','MassiveStars','Clusters','Blazars','Small'};

                for k = 1:numel(FieldNames)
                    fname = FieldNames{k};

                    % Get corresponding EditField name
                    editName = [fname 'EditField'];

                    % Defensive check (in case UI changes)
                    if isprop(app.FieldObjTableApp, editName)

                        % Get table for this field
                        T = app.MainModule.Planner.getFieldObjForTarget(UniqueTargetIndex, fname);

                        % Set value to number of rows
                        app.FieldObjTableApp.(editName).Value = sprintf('%d', height(T));
                    end
                end

                % Update the table data
                app.FieldObjTableApp.UITableData.Data = Data;
                app.FieldObjTableApp.UITableData.ColumnSortable = true;

                % Update column names if table is non-empty
                if ~isempty(Data) && istable(Data)
                    app.FieldObjTableApp.UITableData.ColumnName = ...
                        Data.Properties.VariableNames;
                else
                    app.msglog('updateFieldObjTable: Data is empty or not a table.');
                end

            catch ME
                app.msglog(sprintf('updateFieldObjTable: unexpected error - %s', ME.message));
            end
        end

        % =================================================================
        %                              GUI CALLBACKS
        % =================================================================

        function FieldObjTableDropDownValueChanged(obj, app)
            % Handle FieldObjTable drop-down value changed, called from FieldObjTable app window
            obj.showFieldObjTable(app);
        end

    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)

        function Result = hasData(obj, app)
            % Check if there is data in the Planner (UniqTarg and Plan tables)
            Result = app.hasPlanner() && (height(app.MainModule.Planner.UniqTarg) > 0) || (height(app.MainModule.Planner.Plan) > 0);
        end


        function setTargetHeaderAndFields(obj, app, form, UniqueTargetIndex, captionPrefix)
            % Updates caption & top-row info fields in a table window for selected target.
        
            try
                % Build caption
                captionText = app.UniqueTargetsHelper.makeUniqTargetCaption(app, UniqueTargetIndex);
                if nargin > 4 && ~isempty(captionPrefix)
                    caption = sprintf("%s - %s", captionPrefix, captionText);
                else
                    caption = captionText;
                end
        
                % Apply caption to window title / label - prefer TitleLabel if exists (visual)
                if isprop(form, "TitleLabel")
                    form.TitleLabel.Text = ultrasat.planner.guiutils.safeText(caption);
                elseif isprop(form, "UIFigure")
                    form.UIFigure.Name = ultrasat.planner.guiutils.safeText(caption);
                end
        
                % Extract row, index, name
                row  = app.UniqueTargetsHelper.getRowByIndex(app, UniqueTargetIndex);
                name = app.UniqueTargetsHelper.getNameByIndex(app, UniqueTargetIndex);
                idx  = UniqueTargetIndex;
        
                % Populate UI edit fields (optional)
                if isprop(form, "RowEditField")
                    form.RowEditField.Value = num2str(row);
                end
                
                if isprop(form, "IndexEditField")
                    form.IndexEditField.Value = num2str(idx);
                end
                
                if isprop(form, "NameEditField")
                    form.NameEditField.Value = string(name);
                end
        
            catch ME
                app.msgex("PlannerMainTablesHelper.setTargetHeaderAndFields", ME);
            end
        end
        
    end

end
