%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/TableHelper.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 18/12/2025
% Description : Table Helper for Main Planner
%==========================================================================

classdef TableHelper < ultrasat.api.core.Loggable

    methods
        function obj = TableHelper()
            % Constructor
            obj.LogPrefix = 'TableHelper';
        end

        % =================================================================
        %                            Utilities
        % =================================================================

        function Result = loadTableFromCsvText(obj, CsvText)
            % Read the table from the temporary file
            %
            % This robust version includes error handling and logging.
            % If file operations or table reading fails, the error is logged and
            % an empty table is returned instead of throwing an exception.
            %
            % Logs include details about file creation, writing, and reading steps.

            Result = table();  % Default empty result

            % Create a temporary file name
            try
                tempFile = [tempname, '.csv'];
            catch ME
                obj.msglog(sprintf('loadTableFromCsvText: tempname failed: %s', ME.message));
                return;
            end

            % Write CSV text to file
            fid = fopen(tempFile, 'w');
            if fid == -1
                obj.msglog(sprintf('loadTableFromCsvText: failed to open temp file %s for writing', tempFile));
                return;
            end

            try
                fwrite(fid, CsvText);
            catch ME
                obj.msglog(sprintf('loadTableFromCsvText: fwrite failed for file %s: %s', tempFile, ME.message));
                return;
            end

            try
                fclose(fid);
            catch ME
                obj.msglog(sprintf('loadTableFromCsvText: fclose failed for file %s: %s', tempFile, ME.message));
                return;
            end

            % Read the table from the temporary file
            try
                Result = readtable(tempFile);
            catch ME
                obj.msglog(sprintf('loadTableFromCsvText: readtable failed for file %s: %s', tempFile, ME.message));
            end

            % Cleanup temporary file
            try
                if exist(tempFile, 'file')
                    delete(tempFile);
                end
            catch ME
                obj.msglog(sprintf('loadTableFromCsvText: failed to delete temp file %s: %s', tempFile, ME.message));
            end
        end

        % =================================================================

        function Result = getSortedRowNumbers_UNUSED(obj, Data)
            % Extract row indices for non-empty 'Order' values, sorted by 'Order'.
            %
            % Expected Input:
            %   Data - A table or struct with a field/column named 'Order'.
            %          'Order' can be numeric, cell array of char, or string array.
            %
            % Output:
            %   Result - Row indices sorted by the numeric values of 'Order'.
            %
            % Notes:
            %   - Empty strings or whitespace in 'Order' are treated as NaN and skipped.
            %   - The function never throws: if input is malformed or errors occur,
            %     they are logged and an empty array is returned.
            %   - Supports char-based or string-based content by converting to numeric.

            Result = [];

            try
                % Check if 'Order' column exists
                if ~isfield(Data, 'Order') && ~ismember('Order', Data.Properties.VariableNames)
                    obj.msglog('getSortedRowNumbers: missing ''Order'' column.');
                    return;
                end

                orderCol = Data.Order;

                % Handle empty column
                if isempty(orderCol)
                    return;
                end

                % Case 1: numeric - use directly
                if isnumeric(orderCol)
                    validIdx = find(~isnan(orderCol));
                    [~, sortedIdx] = sort(orderCol(validIdx));
                    Result = validIdx(sortedIdx);
                    return;
                end

                % Case 2: string or char - convert to numeric safely
                if isstring(orderCol) || ischar(orderCol) || iscell(orderCol)
                    % Convert string scalars to cellstr if needed
                    if isstring(orderCol)
                        orderCol = cellstr(orderCol);
                    elseif ischar(orderCol)
                        % single char array (e.g., single row): wrap into cell
                        orderCol = {orderCol};
                    end

                    % Trim and detect empty entries
                    trimmed = cellfun(@strtrim, orderCol, 'UniformOutput', false);
                    isEmpty = cellfun(@(x) isempty(x) || all(isspace(x)), trimmed);

                    % Convert non-empty to numeric, empty to NaN
                    numericVals = nan(size(trimmed));
                    for i = 1:numel(trimmed)
                        if ~isEmpty(i)
                            numVal = str2double(trimmed{i});
                            if isnan(numVal)
                                obj.msglog(sprintf('getSortedRowNumbers: non-numeric value "%s" at row %d treated as NaN', trimmed{i}, i));
                            end
                            numericVals(i) = numVal;
                        end
                    end

                    % Filter out NaNs and sort
                    validIdx = find(~isnan(numericVals));
                    [~, sortedIdx] = sort(numericVals(validIdx));
                    Result = validIdx(sortedIdx);
                    return;
                end

                % Case 3: anything else
                obj.msglog(sprintf('getSortedRowNumbers: unsupported Order column type %s', class(orderCol)));

            catch ME
                obj.msglog(sprintf('getSortedRowNumbers: error %s', ME.message));
                Result = [];
            end
        end

        % =================================================================
        %
        % =================================================================

        function tbl = convertTableDatetimeToString(obj, tbl)
            % Converts all datetime columns in a table to string format (yyyy-MM-dd HH:mm:ss)
            % Handles empty datetime (NaT) values correctly.
            %
            % Expected Input:
            %   tbl - A MATLAB table. Any columns with datatype 'datetime' will be
            %         converted to string arrays in the format 'yyyy-MM-dd HH:mm:ss'.
            %
            % Output:
            %   tbl - The same table, with datetime columns converted to string columns.
            %
            % Notes:
            %   - NaT values are converted to empty strings ("").
            %   - If conversion fails for a column or row, an error is logged and the
            %     original column is left unchanged.

            try
                % Get all variable (column) names
                varNames = tbl.Properties.VariableNames;
            catch ME
                obj.msglog(sprintf('convertTableDatetimeToString: failed to get VariableNames: %s', ME.message));
                return;
            end

            % Iterate through each column
            for i = 1:numel(varNames)
                colName = varNames{i};

                try
                    % Check if the column contains datetime values
                    if isa(tbl.(colName), 'datetime')
                        % Initialize new column as cell array of strings
                        try
                            nRows = height(tbl);
                        catch ME
                            obj.msglog(sprintf('convertTableDatetimeToString: failed to get height for column %s: %s', colName, ME.message));
                            continue;
                        end

                        newCol = cell(nRows, 1);

                        % Loop through all rows in the column
                        for j = 1:nRows
                            try
                                val = tbl.(colName)(j);
                                if isnat(val)  % Check if it's NaT
                                    newCol{j} = "";  % Empty string for NaT values
                                else
                                    newCol{j} = datestr(val, 'yyyy-mm-dd HH:MM:SS'); % Convert datetime to string
                                end
                            catch ME
                                obj.msglog(sprintf('convertTableDatetimeToString: row %d conversion failed for column %s: %s', j, colName, ME.message));
                                newCol{j} = "";
                            end
                        end

                        % Convert cell array to string array and assign back to table
                        try
                            tbl.(colName) = string(newCol);
                        catch ME
                            obj.msglog(sprintf('convertTableDatetimeToString: failed to assign converted column %s: %s', colName, ME.message));
                        end
                    end
                catch ME
                    obj.msglog(sprintf('convertTableDatetimeToString: error processing column %s: %s', colName, ME.message));
                end
            end
        end


        function T = plansToTopLevelTable(obj, plans)
            % plansToTopLevelTable Extract only top-level fields from cell array of structs
            %
            %   T = plansToTopLevelTable(plans)
            %   Input : cell array of plan structs (from JSON)
            %   Output: table with only top-level scalar fields
            %
            % Example:
            %   Data = plansToTopLevelTable(response.plans);
            %   app.OpenPlanApp.UITable.Data = Data;
            %
            %   Robust version: logs errors, skips bad rows, never crashes

            % Default return
            T = table();

            % Empty input
            if isempty(plans)
                return;
            end

            try
                % Support both cell array of structs and struct array
                if iscell(plans)
                    allFields = {};
                    for i = 1:numel(plans)
                        try
                            if isstruct(plans{i})
                                allFields = [allFields; fieldnames(plans{i})]; %#ok<AGROW>
                            else
                                obj.msglog(sprintf('plansToTopLevelTable: Row %d is not struct', i));
                            end
                        catch ME
                            obj.msglog(sprintf('plansToTopLevelTable: fieldnames failed for row %d: %s', i, ME.message));
                        end
                    end
                    getPlan = @(i) plans{i};
                    n = numel(plans);
                elseif isstruct(plans)
                    allFields = fieldnames(plans);
                    getPlan = @(i) plans(i);
                    n = numel(plans);
                else
                    obj.msglog(sprintf('plansToTopLevelTable: unexpected input type %s', class(plans)));
                    return;
                end
            catch ME
                obj.msglog(sprintf('plansToTopLevelTable: failed to collect fieldnames: %s', ME.message));
                return;
            end

            allFields = unique(allFields);
            S = repmat(struct(), n, 1);

            % Extract values per row
            for i = 1:n
                try
                    p = getPlan(i);
                    if ~isstruct(p)
                        obj.msglog(sprintf('plansToTopLevelTable: Row %d skipped (not struct)', i));
                        continue;
                    end

                    for f = 1:numel(allFields)
                        fld = allFields{f};
                        try
                            if isfield(p, fld)
                                val = p.(fld);
                                if ischar(val)
                                    S(i).(fld) = val; % char OK
                                elseif (isstring(val) || islogical(val) || isnumeric(val) ...
                                        || isdatetime(val) || isduration(val)) && isscalar(val)
                                    S(i).(fld) = val; % scalar OK
                                else
                                    S(i).(fld) = [];
                                end
                            else
                                S(i).(fld) = [];
                            end
                        catch ME
                            obj.msglog(sprintf('plansToTopLevelTable: failed to extract field "%s" in row %d: %s', fld, i, ME.message));
                            S(i).(fld) = [];
                        end
                    end
                catch ME
                    obj.msglog(sprintf('plansToTopLevelTable: failed to process row %d: %s', i, ME.message));
                end
            end

            % Convert to table
            try
                T = struct2table(S, 'AsArray', true);
            catch ME
                obj.msglog(sprintf('plansToTopLevelTable: struct2table failed: %s', ME.message));
                % Return empty table if conversion fails
                T = table();
            end
        end


        function T = selectTableColumns(obj, Data, columnList)
            % selectTableColumns Select specific columns from a table in a given order.
            %
            %   T = selectTableColumns(Data, columnList)
            %   Input : Data       - table
            %           columnList - cell array of column names (strings or chars)
            %   Output: T          - table with only those columns in that order
            %
            % Logs and returns empty table if input is invalid or columns are missing.

            T = table();
            try
                if ~istable(Data)
                    obj.msglog('selectTableColumns: Data is not a table.');
                    return;
                end

                % Normalize columnList to cell array of char
                if isstring(columnList)
                    columnList = cellstr(columnList);
                elseif ischar(columnList)
                    columnList = {columnList};
                end

                existingCols = Data.Properties.VariableNames;
                missing = setdiff(columnList, existingCols);
                if ~isempty(missing)
                    obj.msglog(sprintf('selectTableColumns: missing columns: %s', strjoin(missing, ', ')));
                    % Keep only those that exist
                    columnList = intersect(columnList, existingCols, 'stable');
                end

                if ~isempty(columnList)
                    T = Data(:, columnList);
                end
            catch ME
                obj.msglog(sprintf('selectTableColumns: error %s', ME.message));
                T = table();
            end
        end

        % =================================================================

        function T = replaceArrayColumnWithItsLength(obj, Data, columnName)
            % replaceArrayColumnWithItsLength Replace an array column with its length
            %
            %   T = replaceArrayColumnWithItsLength(Data)
            %   Input : Data - table
            %   Output: T    - table with the array column replaced with its length
            %
            % Logs and returns empty table if input is invalid.
            T = Data;
            try
                if ~istable(Data)
                    obj.msglog('replaceArrayColumnWithItsLength: Data is not a table.');
                    return;
                end
                if ~ismember(columnName, Data.Properties.VariableNames)
                    obj.msglog(sprintf('replaceArrayColumnWithItsLength: column %s not found in Data.', columnName));
                    return;
                end
                if ~iscolumn(Data.(columnName))
                    obj.msglog(sprintf('replaceArrayColumnWithItsLength: column %s is not a column vector.', columnName));
                    return;
                end
                lengths = cellfun(@length, Data.(columnName));
                T = removevars(Data, columnName);
                T = addvars(T, lengths, 'NewVariableNames', columnName);
            catch ME
                obj.msglog(sprintf('replaceArrayColumnWithItsLength: error %s', ME.message));
                T = table();
            end
        end

        % =================================================================

        function sortState = getUITableSortState(obj, app, event)
            % Initialize with default values
            % Call from DisplayDataChanged callback by sortState = app.getCurrentSortState(app, event);
            sortState = struct('Variable', '', 'Direction', 'none', 'ColumnIdx', []);            
            try
                if strcmp(event.Interaction, 'sort') && ~isempty(event.InteractionColumn)
                    sortState.ColumnIdx = event.InteractionColumn;
                    sortState.Variable = event.InteractionVariable;
                    
                    % Extract the data as currently displayed to determine direction
                    % We use the Variable name for accuracy
                    viewData = event.Source.DisplayData.(sortState.Variable);
                    
                    if issorted(viewData, 'ascend')
                        sortState.Direction = 'ascend';
                    elseif issorted(viewData, 'descend')
                        sortState.Direction = 'descend';
                    else
                        sortState.Direction = 'none';
                    end
                end
            catch ME
                app.msgex('getCurrentSortState', ME);
            end
        end        


        function sortedTable = reapplySortToData(obj, app, dataTable, sortState)
            % NOT TESTED YET (18/12/2025)
            % Re-sort the data manually before assignment to keep visual consistency
            % myData = app.reapplySortToData(app, myData, sortState);
            
            sortedTable = dataTable; % Default to original if sort fails            
            try
                % Only sort if we have a valid variable and a direction that isn't 'none'
                if ~isempty(sortState.Variable) && ~strcmp(sortState.Direction, 'none')
                    % Check if the variable still exists in the provided table
                    if any(strcmp(sortState.Variable, dataTable.Properties.VariableNames))
                        sortedTable = sortrows(dataTable, sortState.Variable, sortState.Direction);
                    end
                end
            catch ME
                app.msgex('reapplySortToData', ME);
            end
        end

    end

end
