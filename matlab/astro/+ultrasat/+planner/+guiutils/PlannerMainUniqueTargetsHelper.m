%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainUniqueTargetsHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 21/10/2025
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
            if app.isReadOnlyMsg(), return; end

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
            % Edit Unique-Target with editUniqTarg()
            app.msglog('editUniqueTarget');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            try
                % Get index of selected Unique Target
                Index = app.UITableUniqueTargets.Selection;
                if isempty(Index) || (Index < 1)
                    return
                end

                % Create app
                if isempty(app.UniqueTargetParamsApp) || ~isvalid(app.UniqueTargetParamsApp)
                    app.UniqueTargetParamsApp = ultrasat.planner.gui.UniqueTargetParams(app.MainModule);
                end

                % Set field values - Currently there are 9 fields for Unique Target
                ParamsApp = app.UniqueTargetParamsApp;
                UniqTarg = app.MainModule.Planner.UniqTarg;
                obj.setUniqueTargetParamsFields(app, UniqTarg, Index, ParamsApp);

                % Show the form, update values if closed with Save
                if strcmp(app.showModal(app.UniqueTargetParamsApp), 'Save')
                    Name = app.MainModule.GuiHelper.getFieldUniqueTargetName( ParamsApp.NameEditField.Value );
                    RA = app.MainModule.GuiHelper.getFieldRA( ParamsApp.RAEditField.Value );
                    Dec = app.MainModule.GuiHelper.getFieldDec( ParamsApp.DecEditField.Value );
                    app.setModified('editUniqueTarget');
                    try
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
            % Delete Unique-Target with delUniqTarg()
            app.msglog('deleteUniqueTarget');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

            % Get index of selected Unique Target
            Index = app.UITableUniqueTargets.Selection;
            if isempty(Index) || (Index < 1)
                return
            end

            % Ask user to confirm
            Name = app.MainModule.Planner.UniqTarg.Name(Index);
            if ~strcmp(app.AppUtils.askYesNo(sprintf('Delete selected unique target (%s)?', Name)), 'Yes')
                return;
            end

            app.setModified('deleteUniqueTarget');
            try
                % Try to delete unique target, catch exception if it is
                % being used in the plan
                app.MainModule.Planner.delUniqTarg(Index, 'abort_if_in_plan', true);
            catch ME
                % Unqique target is being used in plan, ask user to confirm
                app.msgex('delUniqTarg', ME);
                if ~strcmp(app.AppUtils.askYesNo(sprintf('Unique target is used, deleting it will delete plan targets. Are you sure (%s)?', Name), 'Confirm'), 'Yes')
                    return;
                end
            end

            % Force deleting the unique target and all targets that use it
            try
                app.MainModule.Planner.delUniqTarg(Index, 'abort_if_in_plan', false);
            catch ME
                app.msgex('delUniqTarg', ME);
            end
            app.showPlanAll();
        end


        function loadUniqueTargetsFromFile(obj, app)
            % Load unique targets list from text file (csv). Open dialog to
            % ask user for file name or paste the text.
            app.msglog('loadUniqueTargetsFromFile');
            if ~app.hasPlanner(), return; end
            if app.isReadOnlyMsg(), return; end

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
            if app.isReadOnlyMsg(), return; end
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

            % Add 'Order' column
            Data = app.MainModule.Planner.UniqTarg;

            if isempty(Data) || ~istable(Data)
                app.UITableUniqueTargets.Data = [];
                return;
            end

            Data = app.MainModule.TableHelper.convertTableDatetimeToString(Data);
            Data = addvars(Data, repmat("", height(Data), 1), 'Before', 1, 'NewVariableNames', 'Order');

            % Currently unused - add column of checkboxes
            %Data = addvars(Data, false(height(Data), 1), 'Before', 1, 'NewVariableNames', 'Checked');

            % Make only the first column editable, others non-editable
            nColumns = width(Data);
            editableArray = false(1, nColumns);
            editableArray(1) = true;
            app.UITableUniqueTargets.ColumnEditable = editableArray;

            % Apply style to the entire 'Order' column (first column)
            s = uistyle("BackgroundColor",[1 0.85 0.4]); % Light orange color
            addStyle(app.UITableUniqueTargets, s, "column", 1);

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

                % Update drop-down with unique target double-clicked
                Planner = app.MainModule.Planner;
                Value = Planner.UniqTarg.Name(UniqueTargetIndex);
                app.GraphPlotUniqueTargetDropDown.Value = Value;

                % Plot the graphs of this unique target
                app.PlotHelper.plotGraphs(app);
            catch ME
                app.msgex('uniqueTargetDoubleClick', ME)
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
                ParamsApp.NameEditField.Value = UniqTarg.Name(Index);
                ParamsApp.RAEditField.Value = app.MainModule.ra2Str( UniqTarg.RA(Index) );
                ParamsApp.DecEditField.Value = app.MainModule.dec2Str( UniqTarg.Dec(Index) );
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
