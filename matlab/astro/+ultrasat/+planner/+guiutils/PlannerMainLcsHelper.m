%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/PlannerMainLcsHelper.m
% Author      : Chen Tishler
% Created     : 10/06/2026
% Modified    : 15/06/2026
% Description : LCS Fields view helper for PlannerMain / LcsFields.mlapp
%==========================================================================

classdef PlannerMainLcsHelper < ultrasat.api.core.Loggable
    % Helper class for PlannerMain.mlapp and LcsFields.mlapp
    %
    % All methods require the PlannerMain instance as the first argument, named 'app'.

    methods (Access = public)

        function obj = PlannerMainLcsHelper()
            obj.LogPrefix = 'LcsHelper';
        end

        function showLcsFields(obj, app)
            % Open or raise LcsFields and populate group summary
            app.msglog('showLcsFields');
            if ~app.hasPlanner(), return; end

            if isempty(app.LcsFieldsApp) || ~isvalid(app.LcsFieldsApp)
                app.LcsFieldsApp = ultrasat.planner.gui.LcsFields(app.MainModule);
            end

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

            Planner = app.MainModule.Planner;
            Summary = obj.buildGroupSummaryTable(Planner);
            lcsApp.UITableGroupSummary.Multiselect = 'on';
            obj.setTableData(lcsApp.UITableGroupSummary, Summary);
        end


        function onGroupSummarySelectionChanged(obj, app, lcsApp, selection)
            % Multi-row group selection: union fields for selected groups
            app.msglog('onGroupSummarySelectionChanged');
            obj.clearFieldDetailsPanel(lcsApp);

            if isempty(selection) || ~obj.hasLcsData(app)
                obj.setTableData(lcsApp.UITableGroupFields, table());
                return;
            end

            SummaryData = lcsApp.UITableGroupSummary.Data;
            if isempty(SummaryData) || height(SummaryData) == 0
                return;
            end

            selection = selection(selection >= 1 & selection <= height(SummaryData));
            if isempty(selection)
                return;
            end

            GroupLetters = string(SummaryData.Group(selection));
            FieldsTable = obj.buildGroupFieldsTable(app.MainModule.Planner, GroupLetters);
            obj.setTableData(lcsApp.UITableGroupFields, FieldsTable);
        end


        function onGroupFieldsSelectionChanged(obj, app, lcsApp, selection)
            % Field row selected: show observation dates and time range
            app.msglog('onGroupFieldsSelectionChanged');
            if isempty(selection) || ~obj.hasLcsData(app)
                obj.clearFieldDetailsPanel(lcsApp);
                return;
            end

            FieldsData = lcsApp.UITableGroupFields.Data;
            if isempty(FieldsData) || height(FieldsData) == 0
                obj.clearFieldDetailsPanel(lcsApp);
                return;
            end

            RowIndex = selection(1);
            if RowIndex < 1 || RowIndex > height(FieldsData)
                obj.clearFieldDetailsPanel(lcsApp);
                return;
            end

            FieldName = string(FieldsData.FieldName(RowIndex));
            [DatesTable, MinDate, MaxDate] = obj.buildFieldDatesTable(app.MainModule.Planner, FieldName);
            obj.setTableData(lcsApp.UITableFieldDates, DatesTable);
            obj.setFieldTimeRange(lcsApp, MinDate, MaxDate);
        end

    end

    methods (Access = private)

        function Result = hasLcsData(obj, app)
            Result = false;
            if ~app.hasPlanner()
                return;
            end
            Planner = app.MainModule.Planner;
            if isempty(Planner.LCS_obj) || ~istable(Planner.LCS_obj.Schedule)
                return;
            end
            Result = height(Planner.LCS_obj.Schedule) > 0;
        end


        function Summary = buildGroupSummaryTable(obj, Planner)
            Schedule = Planner.LCS_obj.Schedule;
            Plan = Planner.Plan;

            Categories = string(Schedule.category);
            NumRows = height(Schedule);
            BaseGroups = strings(NumRows, 1);
            for K = 1:NumRows
                BaseGroups(K) = obj.baseCategoryLetter(Categories(K));
            end
            FieldIds = Schedule.Field;

            UniqueGroups = unique(BaseGroups, 'stable');
            NumGroups = numel(UniqueGroups);
            GroupCol = strings(NumGroups, 1);
            NumFieldsCol = zeros(NumGroups, 1);
            StartDateCol = strings(NumGroups, 1);
            EndDateCol = strings(NumGroups, 1);

            for I = 1:NumGroups
                GroupLetter = UniqueGroups(I);
                Mask = BaseGroups == GroupLetter;
                FieldIdsInGroup = unique(FieldIds(Mask));
                FieldNames = obj.fieldNamesForIds(Planner, FieldIdsInGroup);

                GroupCol(I) = GroupLetter;
                NumFieldsCol(I) = numel(FieldNames);
                [MinDate, MaxDate] = obj.dateRangeForFields(Plan, FieldNames);
                StartDateCol(I) = obj.formatDateOnly(MinDate);
                EndDateCol(I) = obj.formatDateOnly(MaxDate);
            end

            Summary = table(GroupCol, NumFieldsCol, StartDateCol, EndDateCol, ...
                'VariableNames', {'Group', 'NumFields', 'StartDate', 'EndDate'});
        end


        function FieldsTable = buildGroupFieldsTable(obj, Planner, groupLetters)
            Schedule = Planner.LCS_obj.Schedule;
            Plan = Planner.Plan;

            Categories = string(Schedule.category);
            NumRows = height(Schedule);
            BaseGroups = strings(NumRows, 1);
            for K = 1:NumRows
                BaseGroups(K) = obj.baseCategoryLetter(Categories(K));
            end

            GroupLetters = string(groupLetters(:));
            Mask = ismember(BaseGroups, GroupLetters);
            FieldIds = unique(Schedule.Field(Mask));
            FieldNames = obj.fieldNamesForIds(Planner, FieldIds);

            NumFields = numel(FieldNames);
            NameCol = FieldNames(:);
            MinObsCol = strings(NumFields, 1);
            MaxObsCol = strings(NumFields, 1);

            for I = 1:NumFields
                [MinDate, MaxDate] = obj.dateRangeForFields(Plan, NameCol(I));
                MinObsCol(I) = obj.formatDateOnly(MinDate);
                MaxObsCol(I) = obj.formatDateOnly(MaxDate);
            end

            FieldsTable = table(NameCol, MinObsCol, MaxObsCol, ...
                'VariableNames', {'FieldName', 'MinObsDate', 'MaxObsDate'});
        end


        function [DatesTable, MinDate, MaxDate] = buildFieldDatesTable(obj, Planner, fieldName)
            Plan = Planner.Plan;
            FieldName = string(fieldName);
            MinDate = NaT('TimeZone', Planner.SysTimeZone);
            MaxDate = NaT('TimeZone', Planner.SysTimeZone);
            DatesTable = table();

            if isempty(Plan) || height(Plan) == 0
                return;
            end

            Mask = string(Plan.Name) == FieldName;
            if ~any(Mask)
                return;
            end

            SubPlan = Plan(Mask, :);
            SubPlan = sortrows(SubPlan, 'Tstart');
            NumRows = height(SubPlan);

            IndexCol = (1:NumRows)';
            DateCol = strings(NumRows, 1);
            StartTimeCol = strings(NumRows, 1);

            for I = 1:NumRows
                Tstart = SubPlan.Tstart(I);
                DateCol(I) = datestr(Tstart, 'yyyy-mm-dd');
                StartTimeCol(I) = datestr(Tstart, 'HH:MM:SS');
            end

            DatesTable = table(IndexCol, DateCol, StartTimeCol, ...
                'VariableNames', {'Index', 'Date', 'StartTime'});
            MinDate = min(SubPlan.Tstart);
            MaxDate = max(SubPlan.Tend);
        end


        function [MinDate, MaxDate] = dateRangeForFields(obj, Plan, fieldNames)
            MinDate = NaT;
            MaxDate = NaT;

            if isempty(Plan) || height(Plan) == 0 || isempty(fieldNames)
                return;
            end

            Names = string(fieldNames(:));
            Mask = ismember(string(Plan.Name), Names);
            if ~any(Mask)
                return;
            end

            MinDate = min(Plan.Tstart(Mask));
            MaxDate = max(Plan.Tend(Mask));
        end


        function Letter = baseCategoryLetter(obj, categoryStr)
            Parts = split(string(categoryStr), '_');
            Letter = Parts(1);
        end


        function Names = fieldNamesForIds(obj, Planner, fieldIds)
            if isempty(fieldIds)
                Names = strings(0, 1);
                return;
            end

            AllSky = Planner.LCS_obj.AllSky;
            NumIds = numel(fieldIds);
            Names = strings(NumIds, 1);

            for I = 1:NumIds
                FieldId = fieldIds(I);
                RowIdx = find(AllSky.Field == FieldId, 1);
                if isempty(RowIdx)
                    Names(I) = string(FieldId);
                    continue;
                end

                if ismember('Name', AllSky.Properties.VariableNames)
                    Names(I) = string(AllSky.Name(RowIdx));
                elseif FieldId >= 1 && FieldId <= height(Planner.UniqTarg)
                    Names(I) = string(Planner.UniqTarg.Name(FieldId));
                else
                    Names(I) = string(AllSky.Field(RowIdx));
                end
            end

            Names = unique(Names, 'stable');
        end


        function clearDependentPanels(obj, lcsApp)
            obj.setTableData(lcsApp.UITableGroupFields, table());
            obj.clearFieldDetailsPanel(lcsApp);
        end


        function clearFieldDetailsPanel(obj, lcsApp)
            obj.setTableData(lcsApp.UITableFieldDates, table());
            obj.setFieldTimeRange(lcsApp, NaT, NaT);
        end


        function setFieldTimeRange(obj, lcsApp, MinDate, MaxDate)
            if isprop(lcsApp, 'StartTimeEditField')
                lcsApp.StartTimeEditField.Value = obj.formatDateTime(MinDate);
            end
            if isprop(lcsApp, 'EndTimeEditField')
                lcsApp.EndTimeEditField.Value = obj.formatDateTime(MaxDate);
            end
        end


        function setTableData(obj, uiTable, Data)
            if isempty(Data) || height(Data) == 0
                uiTable.Data = table();
                return;
            end

            uiTable.Data = Data;
            uiTable.ColumnName = Data.Properties.VariableNames;
            uiTable.ColumnEditable = false(1, width(Data));
            uiTable.ColumnSortable = true(1, width(Data));
            uiTable.SelectionType = 'row';
        end


        function S = formatDateOnly(obj, Dt)
            if isempty(Dt) || (isdatetime(Dt) && any(isnat(Dt)))
                S = "";
            else
                S = string(datestr(Dt(1), 'yyyy-mm-dd'));
            end
        end


        function S = formatDateTime(obj, Dt)
            if isempty(Dt) || (isdatetime(Dt) && isnat(Dt))
                S = "";
            else
                S = string(datestr(Dt, 'yyyy-mm-dd HH:MM:SS'));
            end
        end

    end

end
