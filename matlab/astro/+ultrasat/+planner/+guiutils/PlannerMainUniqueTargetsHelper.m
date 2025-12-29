%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainUniqueTargetsHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 26/12/2025
% Description : Unique Targets Helper for Main Planner
%==========================================================================

classdef PlannerMainUniqueTargetsHelper < ultrasat.api.Loggable
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

        function obj = PlannerMainUniqueTargetsHelper()
            % Constructor
            obj.LogPrefix = 'UniqueTargetsHelper';
        end

        % =================================================================
        %                           CORE ACTIONS
        % =================================================================

        function addUniqueTarget(obj, app)
            % Add Unique-Target with addUniqTargets()
            app.msglog('addUniqueTarget');
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end

            % Create AddUniqueTargetApp
            if isempty(app.AddUniqueTargetApp) || ~isvalid(app.AddUniqueTargetApp)
                app.AddUniqueTargetApp = ultrasat.planner.gui.AddUniqueTarget(app.MainModule);
            end

            % Show AddUniqueTargetApp, if closed by the 'Add' button perform the add operation
            if strcmp(app.showModal(app.AddUniqueTargetApp), 'Add')
                try
                    % Get field values
                    Name = app.MainModule.GuiHelper.getFieldUniqueTargetName( app.AddUniqueTargetApp.NameEditField.Value );
                    RA = app.MainModule.GuiHelper.getFieldRA( app.AddUniqueTargetApp.RAEditField.Value );
                    Dec = app.MainModule.GuiHelper.getFieldDec( app.AddUniqueTargetApp.DecEditField.Value );

                    % Check if RA/Dec are valid
                    if isnan(RA) || isnan(Dec)
                        app.AppUtils.msgError('Invalid RA/Dec values.');
                        return;
                    end

                    % Add to Planner
                    app.MainModule.Planner.addUniqTargets(RA, Dec, 'Name', Name);
                    app.setModified('addUniqueTarget');

                    % Refresh table
                    obj.showUniqueTargets(app);
                catch ME
                    app.msgex('addUniqueTarget', ME);
                end
            end
        end


        function editUniqueTarget(obj, app)
            % Edit the selected Unique-Target row in the grid with editUniqTarg()
            app.msglog('editUniqueTarget');
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end

            try
                % Get index of selected Unique Target row in the grid
                Index = app.UITableUniqueTargets.Selection;
                if isempty(Index) || (Index < 1)
                    return
                end

                % Create UniqueTargetParamsApp if not exists
                if isempty(app.UniqueTargetParamsApp) || ~isvalid(app.UniqueTargetParamsApp)
                    app.UniqueTargetParamsApp = ultrasat.planner.gui.UniqueTargetParams(app.MainModule);
                end

                % Set field values in the UniqueTargetParamsApp
                ParamsApp = app.UniqueTargetParamsApp;
                UniqTarg = app.MainModule.Planner.UniqTarg;
                obj.setUniqueTargetParamsFields(app, UniqTarg, Index, ParamsApp);

                % Show the form, update values if closed with Save
                if strcmp(app.showModal(app.UniqueTargetParamsApp), 'Save')1

                    % Get field values from the UniqueTargetParamsApp
                    Name = app.MainModule.GuiHelper.getFieldUniqueTargetName( ParamsApp.NameEditField.Value );
                    RA = app.MainModule.GuiHelper.getFieldRA( ParamsApp.RAEditField.Value );
                    Dec = app.MainModule.GuiHelper.getFieldDec( ParamsApp.DecEditField.Value );
                    app.setModified('editUniqueTarget');
                    try
                        % Update the Unique Target in the Planner
                        app.MainModule.Planner.editUniqTarg(Index, 'Name', Name, 'RA', RA, 'Dec', Dec);
                        if app.PlanParamsHelper.checkPlanSelfConsistency(app)
                            app.msglog('editUniqueTarget successfully');
                        end
                    catch ME
                        app.msgex('editUniqueTarget', ME);
                    end
                    app.showPlanAll();
                end
            catch ME
                app.msgex('editUniqueTarget', ME);
            end
        end


        function deleteUniqueTarget(obj, app)
            % Delete the selected Unique-Target row in the grid with delUniqTarg()
            app.msglog('deleteUniqueTarget');
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end

            % Get index of selected Unique Target row in the grid
            Index = app.UITableUniqueTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end

            % Ask user to confirm deleting the unique target
            Name = app.MainModule.Planner.UniqTarg.Name(Index);
            if ~strcmp(app.AppUtils.askYesNo(sprintf('Delete selected unique target (%s)?', Name)), 'Yes')
                return;
            end

            app.setModified('deleteUniqueTarget');
            try
                % Try to delete unique target, catch exception if it is being used in the plan
                app.MainModule.Planner.delUniqTarg(Index, 'abort_if_in_plan', true);
            catch ME
                % Unique target is being used in plan, ask user to confirm deleting the unique target and all targets that use it
                app.msgex('delUniqTarg', ME);

                % Ask user to confirm deleting the unique target and all targets that use it
                if strcmp(app.AppUtils.askYesNo(sprintf('Unique target is used, deleting it will delete plan targets. Are you sure (%s)?', Name), 'Confirm'), 'Yes')

                    % Force deleting the unique target and all targets that use it
                    try
                        app.MainModule.Planner.delUniqTarg(Index, 'abort_if_in_plan', false);
                    catch ME
                        app.msgex('delUniqTarg', ME);
                    end

                end
            end

            % Refresh the entire display
            app.showPlanAll();
        end


        function loadUniqueTargetsFromFile(obj, app)
            % Load unique targets list from text file (csv). Open dialog to
            % ask user for file name or paste the text.
            app.msglog('loadUniqueTargetsFromFile');
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end

            % Create app and set initial values from preferences
            if isempty(app.LoadUniqueTargetsFromFileApp) || ~isvalid(app.LoadUniqueTargetsFromFileApp)
                app.LoadUniqueTargetsFromFileApp = ultrasat.planner.gui.LoadUniqueTargetsFromFile(app.MainModule);
                if ~isempty(app.Preferences.UniqueTargetsFileName)
                    app.LoadUniqueTargetsFromFileApp.FileNameEditField.Value = app.Preferences.UniqueTargetsFileName;
                    app.LoadUniqueTargetsFromFileApp.Folder = app.Preferences.UniqueTargetsFolder;
                end
            end

            % Show app
            if strcmp(app.showModal(app.LoadUniqueTargetsFromFileApp), 'Load')
                app.showPleaseWait('Loading unique targets...');
                try
                    % Write loaded/edited text from dialog to tempfile
                    Text = app.LoadUniqueTargetsFromFileApp.Text;
                    FileName = tempname;
                    fid = fopen(FileName, 'w');
                    fwrite(fid, Text);
                    fclose(fid);

                    % Load data from tempfile
                    % NOTE: readtable() expects uniform data types within each column, and it
                    % might misinterpret the file structure.
                    Data = [];
                    if ~isempty(FileName) && isfile(FileName)
                        Data = readtable(FileName);
                    elseif ~isempty(Text)
                        Data = app.MainModule.loadTableFromCsvText(Text);
                    end

                    % Add the loaded unique targets to planner
                    if ~isempty(Data)
                        app.MainModule.Planner.addUniqTargets(Data.RA, Data.Dec, 'Name', Data.Name);
                        app.setModified('loadUniqueTargetsFromFile');
                        obj.showUniqueTargets(app);
                        app.setStatus('OK', 'Unique targets loaded successfully');

                        % Update preferences
                        app.Preferences.UniqueTargetsFileName = app.LoadUniqueTargetsFromFileApp.FileNameEditField.Value;
                        app.Preferences.UniqueTargetsFolder = fileparts(app.LoadUniqueTargetsFromFileApp.FileNameEditField.Value);
                        app.savePreferences();
                    end
                catch ME
                    app.msgex('loadUniqueTargetsFromFile', ME);
                end

                app.closePleaseWait();
            end
        end


        function saveUniqueTargetsToFile(obj, app)
            % Save unique targets list to text file (csv). Open dialog to
            % ask user for file name or paste the text.
            app.msglog('saveUniqueTargetsToFile');
            if ~app.hasPlanner(), return; end

            % Create app
            if isempty(app.SaveUniqueTargetsToFileApp) || ~isvalid(app.SaveUniqueTargetsToFileApp)
                app.SaveUniqueTargetsToFileApp = ultrasat.planner.gui.SaveUniqueTargetsToFile(app.MainModule);

                if ~isempty(app.Preferences.UniqueTargetsFolder)
                    app.SaveUniqueTargetsToFileApp.Folder = app.Preferences.UniqueTargetsFolder;
                end
            end

            % Save to temp file and load as text, display in the dialog
            try
                % Set auto-generated file name
                app.SaveUniqueTargetsToFileApp.FileNameEditField.Value = fullfile(app.SaveUniqueTargetsToFileApp.Folder, datestr(datetime('now', 'TimeZone', 'UTC'), 'yyyy-mm-dd_HH-MM-SS.txt'));

                TempFile = [tempname, '.txt'];
                app.MainModule.Planner.saveUniqTargCooList(TempFile);
                Text = fileread(TempFile);
                app.SaveUniqueTargetsToFileApp.TextArea.Value = Text;
            catch ME
                app.msgex('saveUniqTargCooList', ME);
            end

            % Show app
            if strcmp(app.showModal(app.SaveUniqueTargetsToFileApp), 'Save')
                try
                    % Get field values
                    FileName = app.SaveUniqueTargetsToFileApp.FileName;
                    if ~isempty(FileName)
                        app.MainModule.Planner.saveUniqTargCooList(FileName);

                        % Update preferences
                        if isfile(app.SaveUniqueTargetsToFileApp.FileNameEditField.Value)
                            app.Preferences.UniqueTargetsFolder = fileparts(app.SaveUniqueTargetsToFileApp.FileNameEditField.Value);
                            app.savePreferences();
                        end
                    end
                catch ME
                    app.msgex('saveUniqueTargetsToFile', ME);
                end
            end
        end


        function clearUniqueTargets(obj, app)
            % CLEAR ALL Unique-Targets with clearUniqueTargets()
            app.msglog('clearUniqueTargets');
            if ~app.hasPlanner(), return; end
            if ~app.isEditableMsg(), return; end
            if height(app.MainModule.Planner.UniqTarg) == 0, return; end

            % Ask user to confirm, should we ask again???
            if ~strcmp(app.AppUtils.askYesNo('Are you sure you want to delete ALL UNIQUE TARGETS ???', 'Delete all unique targets'), 'Yes')
                return;
            end

            try
                app.MainModule.Planner.clearUniqueTargets();
                obj.showUniqueTargets(app);
            catch ME
                app.msgex('clearUniqueTargets', ME)
            end
            app.showPlanAll();
        end

        % =================================================================
        %                         DISPLAY / UPDATE
		% =================================================================

        function showUniqueTargets(obj, app)
            % Helper: Update the Unique Targets GUI table with data from Planner
            % Update the display of Unique Targets table
            app.msglog('showUniqueTargets');
            if ~app.hasPlanner()
                app.UITableUniqueTargets.Data = [];
                return;
            end

            % Setup GUI table properties
            app.UITableUniqueTargets.SelectionType = "row";
            app.UITableUniqueTargets.Multiselect = "off";
            app.UITableUniqueTargets.RowName = "numbered";
            app.UITableUniqueTargets.ColumnSortable = true;

            % Check if the table is valid and not empty
            Data = app.MainModule.Planner.UniqTarg;
            if isempty(Data) || ~istable(Data)
                app.UITableUniqueTargets.Data = [];
                return;
            end

            % Convert datetime objects to string
            Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);

            % Replace array fiels with their length
            Data = app.MainModule.TableHelper.replaceArrayColumnWithItsLength(Data, 'CalObj');
            Data = app.MainModule.TableHelper.replaceArrayColumnWithItsLength(Data, 'RefImageIDs');
            Data = app.MainModule.TableHelper.replaceArrayColumnWithItsLength(Data, 'ExtSurveys');

            % -------------------------------------------------
            % Replace 'FieldObj' column in Data with the total length of all cell arrays in the struct for each row
            if ismember('FieldObj', Data.Properties.VariableNames)
                lengths = zeros(height(Data), 1);
                for i = 1:height(Data)
                    fieldStruct = Data.FieldObj{i};
                    if isstruct(fieldStruct)
                        fn = fieldnames(fieldStruct);
                        cnt = 0;
                        for f = 1:numel(fn)
                            fieldValue = fieldStruct.(fn{f});
                            if iscell(fieldValue)
                                cnt = cnt + numel(fieldValue);
                            end
                        end
                        lengths(i) = cnt;
                    else
                        lengths(i) = 0;
                    end
                end
                Data = removevars(Data, 'FieldObj');
                Data = addvars(Data, lengths, 'NewVariableNames', 'FieldObj');
            end

            % -------------------------------------------------
            % Remove columns 'HealpixArray' and 'DitherGroup' from display
            if ismember('HealpixArray', Data.Properties.VariableNames)
                Data = removevars(Data, 'HealpixArray');
            end
            if ismember('DitherGroup', Data.Properties.VariableNames)
                Data = removevars(Data, 'DitherGroup');
            end
            % -------------------------------------------------

            % Add 'Order' column
            Data = addvars(Data, repmat("", height(Data), 1), 'Before', 1, 'NewVariableNames', 'Order');

            % Add Index column with the row number
            Data = addvars(Data, (1:height(Data))', 'Before', 2, 'NewVariableNames', 'Index');

            % Make only the first column editable, others non-editable
            nColumns = width(Data);
            editableArray = false(1, nColumns);
            editableArray(1) = true;
            app.UITableUniqueTargets.ColumnEditable = editableArray;

            % Apply style to the entire 'Order' column (first column)
            s = uistyle("BackgroundColor",[1 0.85 0.4]); % Light orange color
            addStyle(app.UITableUniqueTargets, s, "column", 1);

            % Apply style to the entire 'Index' column
            s = uistyle("BackgroundColor",[1.00,0.99,0.82]); % Cream color
            addStyle(app.UITableUniqueTargets, s, "column", 2);

            % Set table data
            app.UITableUniqueTargets.Data = Data;

            % Update also the table in the window
            if ~isempty(Data)
                app.UITableUniqueTargets.ColumnName = Data.Properties.VariableNames;
            end

            % Extract unique values from the 'obj' column of the table
            Values = unique(app.MainModule.Planner.UniqTarg.Name, 'stable');
            if isempty(Values)
                app.GraphPlotUniqueTargetDropDown.Items = {};
                app.GraphPlotUniqueTargetDropDown.Value = {};
            else
                % Set the dropdown items to these values
                SaveValue = app.GraphPlotUniqueTargetDropDown.Value;
                app.GraphPlotUniqueTargetDropDown.Items = string(Values);

                % Set selected item in GraphPlot area
                if ~isempty(SaveValue) && any(ismember(app.MainModule.Planner.UniqTarg.Name, SaveValue))
                    app.GraphPlotUniqueTargetDropDown.Value = SaveValue;
                else
                    app.GraphPlotUniqueTargetDropDown.Value = Values{1};
                end
            end

            % Copy table content from PlannerMain to UniqueTargetsApp
            if ~isempty(app.UniqueTargetsApp) && isvalid(app.UniqueTargetsApp)
                app.GuiHelper.copyUITable(app.UITableUniqueTargets, app.UniqueTargetsApp.UITable);
            end
        end


        function showUniqueTargetsWindow(obj, app)
            % Show separate window with Unique Targets table
            app.msglog('showUniqueTargetsWindow');
            if ~app.hasPlanner(), return; end

            % Create and show UniqueTargetsApp
            if isempty(app.UniqueTargetsApp) || ~isvalid(app.UniqueTargetsApp)
                app.UniqueTargetsApp = ultrasat.planner.gui.UniqueTargets(app.MainModule);
            end
            app.UniqueTargetsApp.UIFigure.Visible = 'on';

            % Copy table content from PlannerMain to UniqueTargetsApp
            if ~isempty(app.UniqueTargetsApp) && isvalid(app.UniqueTargetsApp)
                app.GuiHelper.copyUITable(app.UITableUniqueTargets, app.UniqueTargetsApp.UITable);
            end
        end


        function setOrderColumnByGridSort(obj, app)
            % Set the 'Order' column of the Unique Targets table by the grid sort
            % Note: Any write to UITable.Data resets sorting. Always.
            app.msglog('setOrderColumnByGridSort');
            if ~app.hasPlanner(), return; end
            try
                % Set the 'Order' column to the row number of the current sorted table
                if any(strcmp('Order', app.UITableUniqueTargets.Data.Properties.VariableNames))
                    Data = app.UITableUniqueTargets.DisplayData;
                    Data.Order = string((1:height(Data))');
                    app.UITableUniqueTargets.Data = Data;   %.Order = (1:height(app.UITableUniqueTargets.Data))';
                else
                    app.msglog('Table does not have ''Order'' column.');
                end
            catch ME
                app.msgex('setOrderColumnByGridSort', ME)
            end
        end


        function clearOrderColumn(obj, app)
            % Clear the 'Order' column of the Unique Targets table
            % Note: Any write to UITable.Data resets sorting. Always.
            app.msglog('clearOrderColumn');
            if ~app.hasPlanner(), return; end
            try
                if any(strcmp('Order', app.UITableUniqueTargets.Data.Properties.VariableNames))
                    Data = app.UITableUniqueTargets.DisplayData;
                    Data.Order = strings(height(app.UITableUniqueTargets.Data), 1);   % sets all to ""
                    app.UITableUniqueTargets.Data = Data;
                end
            catch ME
                app.msgex('clearOrderColumn', ME)
            end
        end        

        % =================================================================

        function updateUniqueTargetPlotsAndTables(obj, app)
            % Update the plots and tables of the selected unique target
            app.msglog('clearOrderColumn');
            if ~app.hasPlanner(), return; end
            try

                % Get index of selected unique target in the drop-down
                % UniqueTargetIndex = obj.getUniqueTargetIndexFromDropDown(app);

                %Planner = app.MainModule.Planner;
                %Value = Planner.UniqTarg.Name(UniqueTargetIndex);

                % Update selected value in drop down
                %app.GraphPlotUniqueTargetDropDown.Value = Value;

                % Plot the graphs of this unique target
                app.PlotHelper.plotGraphs(app);
                
                % Update tables if windows are displayed: CalibObj, ExtSurveys, ObjFields, RefImages
                app.TablesHelper.updateCalibObjTable(app);
                app.TablesHelper.updateExtSurveysTable(app);
                app.TablesHelper.updateFieldObjTable(app);
                app.TablesHelper.updateRefImagesTable(app);
            catch ME
                app.msgex('clearOrderColumn', ME)
            end            
        end

        % =================================================================
        %                           UI CALLBACKS
        % =================================================================

        function uniqueTargetSelected(obj, app, Index)
            % Handle Unique Target selection in table - @Todo - Currently does NOTHING!!!
            % Called from UITable callback

            app.msglog(sprintf('Unique target selected: %d', Index));
            try
                Data = app.getSelectedTableRowAsStruct(app.MainModule.Planner.UniqTarg, Index);
                if ~isempty(Data)
                    app.msglog(sprintf('uniqueTargetSelected done: %d - %s', Index, Data.Name));
                end
            catch ME
                app.msgex('uniqueTargetSelected', ME)
            end
        end


        function uniqueTargetClick(obj, app)
            % Handle Unique Target single click) in table - Currently does NOTHING!!!
            % Called from UITable callback

            app.msglog('uniqueTargetClick');
            if ~app.hasPlanner(), return; end
            try
                Index = app.UITableUniqueTargets.Selection;
                if isempty(Index) || (Index < 1)
                    return
                end
            catch ME
                app.msgex('uniqueTargetClick', ME)
            end
        end


        function uniqueTargetDoubleClick(obj, app)
            % Handle Unique Target double click - Plot graphs of the selected Unique Target
            % Called from UITable callback

            app.msglog('uniqueTargetDoubleClick');
            if ~app.hasPlanner(), return; end
            try
                % Get the selected unique targets
                UniqueTargetIndex = app.UITableUniqueTargets.Selection;
                if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                    return
                end

                % Set the selected unique target index in the drop-down
                obj.setUniqueTargetIndexInDropDown(app, UniqueTargetIndex);

                % Update the plots and tables of the selected unique target
                obj.updateUniqueTargetPlotsAndTables(app, UniqueTargetIndex);
            catch ME
                app.msgex('uniqueTargetDoubleClick', ME)
            end
        end


        function uniqueTargetDropDownValueChanged(obj, app)
            % Handle Unique Target double click - Plot graphs of the selected Unique Target
            % Called from UITable callback

            app.msglog('uniqueTargetDoubleClick');
            if ~app.hasPlanner(), return; end
            try
                % Get the selected unique targets
                %UniqueTargetIndex = app.UITableUniqueTargets.Selection;
                UniqueTargetIndex = obj.getUniqueTargetIndexFromDropDown(app);
                if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                    return
                end

                % Update drop-down with unique target double-clicked
                Planner = app.MainModule.Planner;
                Value = Planner.UniqTarg.Name(UniqueTargetIndex);
                app.GraphPlotUniqueTargetDropDown.Value = Value;

                % Update the CalibObj drop-down according to the selected unique target
                obj.updateCalibObjDropDown(app);

                % Plot the graphs of this unique target
                obj.updateUniqueTargetPlotsAndTables(app);
            catch ME
                app.msgex('uniqueTargetDoubleClick', ME)
            end            
        end


        function UniqueTargetIndex = getUniqueTargetIndexFromDropDown(obj, app)
            % Get index of selected unique target in the drop-down
            UniqueTargetIndex = find(strcmp(app.GraphPlotUniqueTargetDropDown.Value, app.GraphPlotUniqueTargetDropDown.Items));
        end


        function setUniqueTargetIndexInDropDown(obj, app, UniqueTargetIndex)
            % Set index of selected unique target in the drop-down
            if UniqueTargetIndex < 1 || UniqueTargetIndex > length(app.GraphPlotUniqueTargetDropDown.Items)
                app.msglog(sprintf('setUniqueTargetIndexInDropDown: Invalid unique target index: %d', UniqueTargetIndex));
                return;
            end

            % Set the selected unique target name in the drop-down
            app.GraphPlotUniqueTargetDropDown.Value = string(app.MainModule.Planner.UniqTarg.Name(UniqueTargetIndex));

            % Update the CalibObj drop-down according to the selected unique target
            obj.updateCalibObjDropDown(app);
        end


        function updateCalibObjDropDown(obj, app)
            % Update the CalibObj drop-down with the unique target index

            app.msglog('updateCalibObjDropDown');

            % Get index of selected unique target in the drop-down
            UniqueTargetIndex = obj.getUniqueTargetIndexFromDropDown(app);
            if isempty(UniqueTargetIndex) || (UniqueTargetIndex < 1)
                return
            end

            % Get CalibObj table for the selected unique target
            CalibObjTable = app.MainModule.Planner.getCalibObj(UniqueTargetIndex);

            % If no calibration objects, clear the drop-down
            if isempty(CalibObjTable) || height(CalibObjTable) == 0
                app.PlotCalibObjDropDown.Items = {};
                app.PlotCalibObjDropDown.Value = {};
                return
            end

            % Extract unique values from the 'obj' column of the table
            ObjValues = unique(CalibObjTable.obj, 'stable');

            % Set the dropdown items to these values
            app.PlotCalibObjDropDown.Items = string(ObjValues);
            app.PlotCalibObjDropDown.Value = ObjValues{1};
        end


        function row = getRowByIndex(obj, app, UniqueTargetIndex)
            % Returns the *visible* row index in the UITable that corresponds
            % to the given unique target index.
        
            row = [];    
            try
                Data = app.UITableUniqueTargets.Data;   % sorted displayed table
        
                % Must have Index column
                if ~ismember("Index", Data.Properties.VariableNames)
                    app.msglog("getRowByIndex: 'Index' column missing in grid.");
                    return;
                end
        
                % Locate row where Index matches
                idx = find(Data.Index == UniqueTargetIndex, 1);
        
                if ~isempty(idx)
                    row = idx;
                end        
            catch ME
                app.msgex("getRowByIndex", ME);
            end
        end
        

        function name = getNameByIndex(obj, app, UniqueTargetIndex)
            % Returns the name of the unique target with the given index
            name = "";
            try
                % Read from Planner (true data owner)
                name = string(app.MainModule.Planner.UniqTarg.Name(UniqueTargetIndex));
            catch ME
                app.msgex("getNameByIndex", ME);
            end
        end
        

        function caption = makeUniqTargetCaption(obj, app, UniqueTargetIndex)
            % Returns the caption for the unique target with the given index, as "#Row – Index: index – Name: name"
            caption = "";
            try
                row  = obj.getRowByIndex(app, UniqueTargetIndex);
                name = obj.getNameByIndex(app, UniqueTargetIndex);
        
                if isempty(row)
                    caption = sprintf("Index: %d - Name: %s", UniqueTargetIndex, name);
                else
                    caption = sprintf("#%d - Index: %d - Name: %s", row, UniqueTargetIndex, name);
                end
        
            catch ME
                app.msgex("makeUniqTargetCaption", ME);
            end
        end
        
    end

    % =====================================================================
    %                           PRIVATE METHODS
    % =====================================================================

    methods (Access = private)

        % =================================================================
        %                            HELPERS
		% =================================================================

        function setUniqueTargetParamsFields(obj, app, UniqTarg, Index, ParamsApp)
            % Helper: Set field values - Currently there are 9 fields for Unique Target
            try
                ParamsApp.UniqueTargetIndexEditField.Value = int2str(Index);

                % Editable fields
                ParamsApp.NameEditField.Value = UniqTarg.Name(Index);
                ParamsApp.RAEditField.Value = app.MainModule.ra2Str( UniqTarg.RA(Index) );
                ParamsApp.DecEditField.Value = app.MainModule.dec2Str( UniqTarg.Dec(Index) );

                % Read-only fields
                ParamsApp.A_UEditField.Value = app.MainModule.num2Str( UniqTarg.A_U(Index) );
                ParamsApp.CalObjEditField.Value = app.MainModule.length2Str( UniqTarg.CalObj(Index) );
                ParamsApp.RefImagesIDsEditField.Value = app.MainModule.length2Str( UniqTarg.RefImageIDs(Index) );
                ParamsApp.ExtSurveysEditField.Value = app.MainModule.length2Str( UniqTarg.ExtSurveys(Index) );
                ParamsApp.FieldObjEditField.Value = app.MainModule.length2Str( UniqTarg.FieldObj(Index) );
                ParamsApp.HealpixArrayEditField.Value = app.MainModule.length2Str( UniqTarg.HealpixArray(Index) );
            catch ME
                app.msgex('setUniqueTargetParamsFields', ME);
            end
        end

    end

end
