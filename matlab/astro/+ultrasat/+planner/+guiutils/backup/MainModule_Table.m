%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/MainModule.m
% Author:  Chen Tishler
% Created: 07/01/2025
% Updated: 28/01/2025
% Title:   
%==========================================================================
% Debug:
%   DM = ultrasat.planner.gui.MainModule()
%

classdef MainModule < ultrasat.api.Loggable
    % This class serves like a DataModule in Delphi.
   

    methods

        % =================================================================
        %                            Utilities
        % =================================================================
        function Result = loadTableFromCsvText(obj, CsvText)
            % Read the table from the temporary file
            tempFile = [tempname, '.csv'];
            fid = fopen(tempFile, 'w');
            fwrite(fid, CsvText);
            fclose(fid);
            Result = readtable(tempFile);
            delete(tempFile);
        end


        function Result = getSortedRowNumbers(obj, Data)
            % Extract row indices for non-empty 'Order' values, sorted by 'Order'
            
            % Convert to cell array if necessary (handles both strings and chars)
            if iscell(Data.Order) || isstring(Data.Order)
                % Trim whitespace and convert empty strings to NaN for filtering
                trimmedOrder = strtrim(Data.Order);
                isValid = ~strcmp(trimmedOrder, "") & ~strcmp(trimmedOrder, " ");  % Check for truly empty strings
                Data.Order(~isValid) = NaN;  % Replace empty strings with NaN
            end
        
            % Find non-empty (non-NaN) rows
            nonEmptyRows = find(~isnan(Data.Order));
        
            % Sort by 'Order' column
            [~, sortedIdx] = sort(Data.Order(nonEmptyRows));
        
            % Return sorted row indices
            Result = nonEmptyRows(sortedIdx);
        end
        
        % =================================================================
        %
        % =================================================================
        
        function tbl = convertTableDatetimeToString(obj, tbl)
            % Converts all datetime columns in a table to string format (yyyy-MM-dd HH:mm:ss)
            % Handles empty datetime (NaT) values correctly.
        
            % Get all variable (column) names
            varNames = tbl.Properties.VariableNames;
        
            % Iterate through each column
            for i = 1:numel(varNames)
                colName = varNames{i};
        
                % Check if the column contains datetime values
                if isa(tbl.(colName), 'datetime')
                    % Initialize new column as cell array of strings
                    newCol = cell(height(tbl), 1);
        
                    % Loop through all rows in the column
                    for j = 1:height(tbl)
                        if isnat(tbl.(colName)(j))  % Check if it's NaT
                            newCol{j} = "";  % Empty string for NaT values
                        else
                            newCol{j} = datestr(tbl.(colName)(j), 'yyyy-MM-dd HH:mm:ss'); % Convert datetime to string
                        end
                    end
        
                    % Convert cell array to string array and assign back to table
                    tbl.(colName) = string(newCol);
                end
            end
        end

        % -------------------------------------------------------------------

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
        
            if isempty(plans)
                T = table();
                return;
            end

            % Support both cell array of structs and struct array
            if iscell(plans)
                % gather all unique top-level fields
                allFields = {};
                for i = 1:numel(plans)
                    allFields = [allFields; fieldnames(plans{i})]; %#ok<AGROW>
                end
                getPlan = @(i) plans{i};
            elseif isstruct(plans)
                allFields = fieldnames(plans);
                getPlan = @(i) plans(i);
            else
                error('plans must be a cell array of structs or a struct array');
            end
        
            allFields = unique(allFields);
            n = numel(plans);
        
            S = repmat(struct(), n, 1);
        
            for i = 1:n
                p = getPlan(i);
                for f = 1:numel(allFields)
                    fld = allFields{f};
                    if isfield(p, fld)
                        val = p.(fld);
        
                        % ACCEPT rules:
                        % - char vectors (any length)
                        % - string/logical/numeric/datetime/duration *scalars*
                        if ischar(val)
                            S(i).(fld) = val;                           % char row OK
                        elseif (isstring(val) || islogical(val) || isnumeric(val) ...
                                || isdatetime(val) || isduration(val)) && isscalar(val)
                            S(i).(fld) = val;                           % scalar OK
                        else
                            % Skip nested structs, cells, arrays, vectors, etc.
                            S(i).(fld) = [];
                        end
                    else
                        S(i).(fld) = [];
                    end
                end
            end
        
            % Convert to table (struct array with one column dimension is OK)
            T = struct2table(S);
        end
                

        function T = selectTableColumns(obj, Data, columnList)
            % selectTableColumns Select specific columns from a table in a given order
            %
            %   T = selectTableColumns(Data, columnList)
            %   Input : Data       - table
            %           columnList - cell array of column names (strings or chars)
            %   Output: T          - table with only those columns in that order
            %
            % Example:
            %   cols = {'pk','title','status','reviewer','review_time'};
            %   Data = selectTableColumns(Data, cols);
        
            % Validate input is a table
            if ~istable(Data)
                error('Input must be a table.');
            end
        
            % Ensure requested columns exist in Data
            existingCols = Data.Properties.VariableNames;
            missing = setdiff(columnList, existingCols);
            if ~isempty(missing)
                error('Missing columns in Data: %s', strjoin(missing, ', '));
            end
        
            % Select and reorder
            T = Data(:, columnList);
        end
        
    end

end
