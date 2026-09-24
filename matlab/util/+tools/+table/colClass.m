function [Result] = colClass(T)
    % Return the classes of the columns in a table.
    % Input  : - Table.
    % Output : - String array of classes, one per column.
    % Author : Eran Ofek (2025 Mar) 
    % Example: tools.table.colClass(T)

    Result = string(varfun(@class, T, 'OutputFormat', 'cell'));

end
