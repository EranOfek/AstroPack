function [T] = table_cell2string(T)
    % Given a table object, convert columns in cell array format to strings
    % Input  : - A table.
    % Output : - The corrected table.
    % Author : Eran Ofek (2024 Oct) 
    % Example: T=table;
    %          T.A={'a';'b'}
    %          T = tools.table.table_cell2string(T);
    
    %ColTypes = varfun(@class, T, 'OutputFormat', 'cell');

    ColNames = T.Properties.VariableNames;

    % Loop through each column and check if it's a cell array
    for I = 1:length(ColNames)
        if isa(T.(ColNames{I}), 'cell')
            % Convert the cell array column to a string array
            T.(ColNames{I}) = string(T.(ColNames{I}));
        end

    end
end
