function [Result,FirstExist]=isColumn(T, Cols)
    % Check if columns exist in table, and return first existing column name.
    % Input  : - A table object.
    %          - Column name, or cell array of columns.
    % Output : - A vector of logicals indicating if each column exist in
    %            table.
    %          - A char array of the first column name in the input list
    %            that exist in the table.
    % AUthor : Eran Ofek (Mar 2023)
    % Example: T = table(rand(10,1),rand(10,1),'VariableNames',{'A','B'})
    %          tools.table.isColumn(T,{'B','C','A'})

    Result = ismember(Cols, T.Properties.VariableNames);

    if nargout>1
        I = find(Result, 1, 'first');
        if isempty(I)
            error('Column name not found');
        end
        FirstExist = Cols{I};
    end

end
