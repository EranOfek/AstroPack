function [Result] = table2cellOfCols(T)
    % Convert a table into a cell array of columns which are numeric vectors
    % Input  : - A table.
    % Output : - A cell array containing in each element a column of the
    %            table.
    % Author : Eran Ofek (2024 Nov) 
    % Example: C=tools.table.table2cellOfCols(T)

    ColNames = T.Properties.VariableNames;
    Ncol   = numel(ColNames);
    Result = cell(1,Ncol);
    for Icol=1:1:Ncol
        Result{Icol} = T.(ColNames{Icol});
    end

end
