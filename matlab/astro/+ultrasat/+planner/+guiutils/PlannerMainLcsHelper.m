%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainLcsHelper.m
% Author      : Chen Tishler
% Created     : 10/06/2026
% Updated     : 22/06/2026
% Description : LCS Fields view helper for PlannerMain / LcsFields.mlapp
%==========================================================================

classdef PlannerMainLcsHelper < ultrasat.api.core.Loggable
    % Helper class for PlannerMain.mlapp and LcsFields.mlapp
    %
    % All methods require the PlannerMain instance as the first argument, named 'app'.

    methods (Access = public)

        function obj = PlannerMainLcsHelper()
            % Construct LCS helper and set log prefix
            obj.LogPrefix = 'LcsHelper';
        end


        function showLcsFields(obj, app)
            % Open or raise LcsFields and populate group summary
            % Called from PlannerMain.mlapp when LCSFieldsButton is pushed
            app.msglog('showLcsFields');
            if ~app.hasPlanner(), return; end
            if ~strcmp(app.MainModule.Planner.Type, 'LCS'), return; end

            % Create LcsFields app if it doesn't exist
            if isempty(app.LcsFieldsApp) || ~isvalid(app.LcsFieldsApp)
                app.LcsFieldsApp = ultrasat.planner.gui.LcsFields(app.MainModule);
            end

            % Populate all data and show the window
            LcsApp = app.LcsFieldsApp;
            obj.populateAll(app, LcsApp);
            LcsApp.UIFigure.Visible = 'on';
        end


        function populateAll(obj, app, lcsApp)
            % Populate group summary and clear dependent panels
            obj.populateGroupSummary(app, lcsApp);
            obj.clearDependentPanels(lcsApp);
        end


        function populateGroupSummary(obj, app, lcsApp)
            % Fill UITableGroupSummary from planner LCS_obj schedule
            app.msglog('populateGroupSummary');
            if ~obj.hasLcsData(app)
                obj.setTableData(lcsApp.UITableGroupSummary, table());
                return;
            end

            % Build summary table with rows per base group letter
            Planner = app.MainModule.Planner;
            Summary = obj.buildGroupSummaryTable(Planner);

            % Update display in LCS window
            lcsApp.UITableGroupSummary.Multiselect = 'on';
            obj.setTableData(lcsApp.UITableGroupSummary, Summary);
        end


        function onGroupSummarySelectionChanged(obj, app, lcsApp, selection)
            % Multi-row group selection: union fields for selected groups
            % Called from LcsFields.mlapp when group in UITableGroupSummary is selected

            app.msglog('onGroupSummarySelectionChanged');
            obj.clearFieldDetailsPanel(lcsApp);

            % If no selection, clear field details panel
            if isempty(selection) || ~obj.hasLcsData(app)
                obj.setTableData(lcsApp.UITableGroupFields, table());
                return;
            end

            % Get summary data
            SummaryData = lcsApp.UITableGroupSummary.Data;
            if isempty(SummaryData) || height(SummaryData) == 0
                return;
            end

            % Validate selection (UITable.Selection is n-by-2 [row, col])
            selection = obj.rowIndicesFromTableSelection(selection, height(SummaryData));
            if isempty(selection)
                return;
            end

            % Build fields table
            GroupLetters = string(SummaryData.Group(selection));
            FieldsTable = obj.buildGroupFieldsTable(app.MainModule.Planner, GroupLetters);
            obj.setTableData(lcsApp.UITableGroupFields, FieldsTable);
        end


        function onGroupFieldsSelectionChanged(obj, app, lcsApp, selection)
            % Field row selected: show observation dates and time range
            % Called from LcsFields.mlapp when field in UITableGroupFields is selected

            app.msglog('onGroupFieldsSelectionChanged');
            if isempty(selection) || ~obj.hasLcsData(app)
                obj.clearFieldDetailsPanel(lcsApp);
                return;
            end

            % Get fields data
            FieldsData = lcsApp.UITableGroupFields.Data;
            if isempty(FieldsData) || height(FieldsData) == 0
                obj.clearFieldDetailsPanel(lcsApp);
                return;
            end

            % Validate selection (UITable.Selection is n-by-2 [row, col])
            RowIndices = obj.rowIndicesFromTableSelection(selection, height(FieldsData));
            if isempty(RowIndices)
                obj.clearFieldDetailsPanel(lcsApp);
                return;
            end

            % Get the first row index from the selection
            RowIndex = RowIndices(1);
            if RowIndex < 1 || RowIndex > height(FieldsData)
                obj.clearFieldDetailsPanel(lcsApp);
                return;
            end

            % Build dates table
            FieldName = string(FieldsData.FieldName(RowIndex));
            [DatesTable, MinDate, MaxDate] = obj.buildFieldDatesTable(app.MainModule.Planner, FieldName);

            % Update display in LCS window
            obj.setTableData(lcsApp.UITableFieldDates, DatesTable);
            obj.setFieldTimeRange(lcsApp, MinDate, MaxDate);
        end

    end

    % =================================================================
    %                         PRIVATE METHODS
    % =================================================================

    methods (Access = private)

        function RowIndices = rowIndicesFromTableSelection(obj, selection, maxRow)
            % Extract unique valid row indices from UITable.Selection (n-by-2)
            RowIndices = [];
            if isempty(selection)
                return;
            end

            % Get the row indices from the selection of the first column
            if size(selection, 2) >= 1
                RowIndices = selection(:, 1);
            else
                RowIndices = selection(:);
            end

            % Keep only the valid row indices
            RowIndices = unique(RowIndices(RowIndices >= 1 & RowIndices <= maxRow));
        end


        function Result = hasLcsData(obj, app)
            % Check if there is LCS data in the Planner
            Result = false;

            % Check if there is a Planner
            if ~app.hasPlanner()
                return;
            end

            % Validate schedule table exists (LCS_obj is created by the planner)
            Planner = app.MainModule.Planner;
            if isempty(Planner.LCS_obj) || ~istable(Planner.LCS_obj.Schedule)
                return;
            end

            % Non-empty schedule means there is LCS data to display
            Result = height(Planner.LCS_obj.Schedule) > 0;
        end


        function Summary = buildGroupSummaryTable(obj, Planner)
            % Aggregate LCS schedule rows into one summary row per base group letter
            Schedule = Planner.LCS_obj.Schedule;
            Plan = Planner.Plan;

            % Parallel arrays: schedule category label and field ID per row
            Categories = string(Schedule.category);
            NumRows = height(Schedule);

            % Map category strings (e.g. 'A_xxx') to base group letter ('A')
            BaseGroups = strings(NumRows, 1);
            for K = 1:NumRows
                BaseGroups(K) = obj.baseCategoryLetter(Categories(K));
            end
            FieldIds = Schedule.Field;

            % Sort groups alphabetically (A, B, C, D, ...)
            UniqueGroups = unique(BaseGroups);
            NumGroups = numel(UniqueGroups);
            GroupCol = strings(NumGroups, 1);
            NumFieldsCol = zeros(NumGroups, 1);
            StartDateCol = strings(NumGroups, 1);
            EndDateCol = strings(NumGroups, 1);

            % One summary row per group: field count + plan date span
            for I = 1:NumGroups
                GroupLetter = UniqueGroups(I);
                Mask = BaseGroups == GroupLetter;
                FieldIdsInGroup = unique(FieldIds(Mask));

                % fieldNamesForIds applies AllSky -> UniqTarg -> raw ID fallback
                FieldNames = obj.fieldNamesForIds(Planner, FieldIdsInGroup);

                % Aggregate per-group statistics: number of unique fields + date range in the plan
                GroupCol(I) = GroupLetter;
                NumFieldsCol(I) = numel(FieldNames);

                % Get the min and max dates for the fields in the group
                [MinDate, MaxDate] = obj.dateRangeForFields(Plan, FieldNames);
                StartDateCol(I) = obj.formatDateOnly(MinDate);
                EndDateCol(I) = obj.formatDateOnly(MaxDate);
            end

            % Build the summary table
            Summary = table(GroupCol, NumFieldsCol, StartDateCol, EndDateCol, ...
                'VariableNames', {'Group', 'NumFields', 'StartDate', 'EndDate'});
        end


        function FieldsTable = buildGroupFieldsTable(obj, Planner, groupLetters)
            % Build per-field table for one or more selected group letters
            Schedule = Planner.LCS_obj.Schedule;
            Plan = Planner.Plan;

            % Map schedule categories to base group letters (prefix before '_')
            Categories = string(Schedule.category);
            NumRows = height(Schedule);
            BaseGroups = strings(NumRows, 1);
            for K = 1:NumRows
                BaseGroups(K) = obj.baseCategoryLetter(Categories(K));
            end

            % Collect unique field IDs that belong to the selected group letters
            GroupLetters = string(groupLetters(:));
            Mask = ismember(BaseGroups, GroupLetters);
            FieldIdsAll = unique(Schedule.Field(Mask));
            if isempty(FieldIdsAll)
                FieldsTable = table();
                return;
            end

            % Resolve names and AllSky properties per field ID, then dedupe by name
            [RAColAll, DecColAll, AUColAll] = obj.fieldPropsForIds(Planner, FieldIdsAll);
            NamesAll = strings(numel(FieldIdsAll), 1);
            for I = 1:numel(FieldIdsAll)
                NamesAll(I) = obj.fieldNameForId(Planner, FieldIdsAll(I));
            end

            % Deduplicate the fields by name
            [NameCol, UniqueIdx] = unique(NamesAll, 'stable');
            FieldIds = FieldIdsAll(UniqueIdx);

            % Get the RA, Dec, A_U from the UniqTarg table for the deduplicated field IDs
            RACol = RAColAll(UniqueIdx);
            DecCol = DecColAll(UniqueIdx);
            AUCol = AUColAll(UniqueIdx);

            % Build per-field min/max observation dates based on the current plan
            NumFields = numel(NameCol);
            MinObsCol = strings(NumFields, 1);
            MaxObsCol = strings(NumFields, 1);

            % Get the min and max dates for the fields in the group
            for I = 1:NumFields
                [MinDate, MaxDate] = obj.dateRangeForFields(Plan, NameCol(I));
                MinObsCol(I) = obj.formatDateOnly(MinDate);
                MaxObsCol(I) = obj.formatDateOnly(MaxDate);
            end

            % Build the fields table
            VarNames = {'FieldName', 'RA', 'Dec', 'A_U', 'MinObsDate', 'MaxObsDate'};
            FieldsTable = table(NameCol, RACol, DecCol, AUCol, MinObsCol, MaxObsCol, ...
                'VariableNames', VarNames);

            % Group B: add 1-day cadence window dates from schedule day indices
            if any(GroupLetters == "B")
                BMask = BaseGroups == "B";
                StartDate = Planner.LCS_obj.StartDate;
                CadenceStartCol = strings(NumFields, 1);
                CadenceEndCol = strings(NumFields, 1);

                % Get the 1-day cadence window dates for the fields in the group
                for I = 1:NumFields
                    BFieldMask = BMask & (Schedule.Field == FieldIds(I));
                    if any(BFieldMask)
                        MinStartDay = min(Schedule.start(BFieldMask));
                        MaxEndDay = max(Schedule.end(BFieldMask));
                        CadenceStartCol(I) = obj.formatDateOnly(StartDate + days(MinStartDay - 1));
                        CadenceEndCol(I) = obj.formatDateOnly(StartDate + days(MaxEndDay - 1));
                    end
                end

                % Add the 1-day cadence window dates to the fields table
                FieldsTable = [FieldsTable, table(CadenceStartCol, CadenceEndCol, ...
                    'VariableNames', {'1-Day Cadence Start', '1-Day Cadence End'})];
            end
        end


        function [DatesTable, MinDate, MaxDate] = buildFieldDatesTable(obj, Planner, fieldName)
            % Build table of observation dates/times for a specific field name
            Plan = Planner.Plan;
            FieldName = string(fieldName);
            MinDate = NaT('TimeZone', Planner.SysTimeZone);
            MaxDate = NaT('TimeZone', Planner.SysTimeZone);
            DatesTable = table();

            % Guard: no plan data means nothing to display
            if isempty(Plan) || height(Plan) == 0
                return;
            end

            % Filter the plan rows to the requested field
            Mask = string(Plan.Name) == FieldName;
            if ~any(Mask)
                return;
            end

            % Sort by start time for readable chronological display
            SubPlan = Plan(Mask, :);
            SubPlan = sortrows(SubPlan, 'Tstart');
            NumRows = height(SubPlan);

            % Build display columns: 1-based index, date-only, start-time only
            IndexCol = (1:NumRows)';
            DateCol = strings(NumRows, 1);
            StartTimeCol = strings(NumRows, 1);

            % Build the display columns
            for I = 1:NumRows
                Tstart = SubPlan.Tstart(I);
                DateCol(I) = datestr(Tstart, 'yyyy-mm-dd');
                StartTimeCol(I) = datestr(Tstart, 'HH:MM:SS');
            end

            % Build the dates table
            DatesTable = table(IndexCol, DateCol, StartTimeCol, ...
                'VariableNames', {'Index', 'Date', 'StartTime'});
            MinDate = min(SubPlan.Tstart);
            MaxDate = max(SubPlan.Tend);
        end


        function [MinDate, MaxDate] = dateRangeForFields(obj, Plan, fieldNames)
            % Compute min start / max end times across plan rows for given field names
            MinDate = NaT;
            MaxDate = NaT;

            if isempty(Plan) || height(Plan) == 0 || isempty(fieldNames)
                return;
            end

            % Match plan rows by field name (supports scalar name or list of names)
            Names = string(fieldNames(:));
            Mask = ismember(string(Plan.Name), Names);
            if ~any(Mask)
                return;
            end

            MinDate = min(Plan.Tstart(Mask));
            MaxDate = max(Plan.Tend(Mask));
        end


        function Letter = baseCategoryLetter(obj, categoryStr)
            % Extract base group letter from category string (prefix before '_')
            Parts = split(string(categoryStr), '_');
            Letter = Parts(1);
        end


        function Names = fieldNamesForIds(obj, Planner, fieldIds)
            % Resolve LCS field IDs to display names with three-tier fallback chain
            if isempty(fieldIds)
                Names = strings(0, 1);
                return;
            end

            NumIds = numel(fieldIds);
            Names = strings(NumIds, 1);
            for I = 1:NumIds
                Names(I) = obj.fieldNameForId(Planner, fieldIds(I));
            end

            % Unique (stable) so downstream displays don't duplicate fields
            Names = unique(Names, 'stable');
        end


        function Name = fieldNameForId(obj, Planner, fieldId)
            % Resolve a single LCS field ID to a display name
            AllSky = Planner.LCS_obj.AllSky;
            RowIdx = find(AllSky.Field == fieldId, 1);
            if isempty(RowIdx)
                Name = string(fieldId);
                return;
            end

            % Get the name from the AllSky table if it exists
            if ismember('Name', AllSky.Properties.VariableNames)
                Name = string(AllSky.Name(RowIdx));

            % Get the name from the UniqTarg table if the field ID is within the range
            elseif fieldId >= 1 && fieldId <= height(Planner.UniqTarg)
                Name = string(Planner.UniqTarg.Name(fieldId));

            % Get the name from the AllSky table if the field ID is not within the range
            else
                Name = string(AllSky.Field(RowIdx));
            end
        end


        function [RACol, DecCol, AUCol] = fieldPropsForIds(obj, Planner, fieldIds)
            % Look up RA, Dec, A_U from AllSky for a list of field IDs
            AllSky = Planner.LCS_obj.AllSky;
            NumIds = numel(fieldIds);
            RACol = nan(NumIds, 1);
            DecCol = nan(NumIds, 1);
            AUCol = nan(NumIds, 1);

            % Get the RA, Dec, A_U from the AllSky table for each field ID
            for I = 1:NumIds
                RowIdx = find(AllSky.Field == fieldIds(I), 1);
                if isempty(RowIdx)
                    continue;
                end

                % Get the RA, Dec, A_U from the AllSky table for the field ID
                RACol(I) = AllSky.RA(RowIdx);
                DecCol(I) = AllSky.Dec(RowIdx);
                AUCol(I) = AllSky.A_U(RowIdx);
            end
        end


        function clearDependentPanels(obj, lcsApp)
            % Clear UI panels that depend on group selection
            obj.setTableData(lcsApp.UITableGroupFields, table());
            obj.clearFieldDetailsPanel(lcsApp);
        end


        function clearFieldDetailsPanel(obj, lcsApp)
            % Clear the per-field details panel (dates + time range)
            obj.setTableData(lcsApp.UITableFieldDates, table());
            obj.setFieldTimeRange(lcsApp, NaT, NaT);
        end


        function setFieldTimeRange(obj, lcsApp, MinDate, MaxDate)
            % Update min/max time range edit fields (if present in the UI)
            if isprop(lcsApp, 'StartTimeEditField')
                lcsApp.StartTimeEditField.Value = obj.formatDateTime(MinDate);
            end
            if isprop(lcsApp, 'EndTimeEditField')
                lcsApp.EndTimeEditField.Value = obj.formatDateTime(MaxDate);
            end
        end


        function setTableData(obj, uiTable, Data)
            % Set UITable data and enforce consistent column/selection behavior
            if isempty(Data) || height(Data) == 0
                uiTable.Data = table();
                return;
            end

            uiTable.Data = Data;
            
            % Keep UI table columns aligned with the table's variable names
            uiTable.ColumnName = Data.Properties.VariableNames;

            % Display-only tables in this view (sorting enabled, cell editing disabled)
            uiTable.ColumnEditable = false(1, width(Data));
            uiTable.ColumnSortable = true(1, width(Data));
            uiTable.SelectionType = 'row';
        end


        function S = formatDateOnly(obj, Dt)
            % Format datetime as yyyy-mm-dd, returning "" for empty/NaT
            if isempty(Dt) || (isdatetime(Dt) && any(isnat(Dt)))
                S = "";
            else
                S = string(datestr(Dt(1), 'yyyy-mm-dd'));
            end
        end


        function S = formatDateTime(obj, Dt)
            % Format datetime as yyyy-mm-dd HH:MM:SS, returning "" for empty/NaT
            if isempty(Dt) || (isdatetime(Dt) && isnat(Dt))
                S = "";
            else
                S = string(datestr(Dt, 'yyyy-mm-dd HH:MM:SS'));
            end
        end

    end

end
