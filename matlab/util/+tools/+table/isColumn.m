function Result=isColumn(T, Cols)
    % Check if columns exist in table
    % Input  : - A table object.
    %          - Column name, or cell array of columns.
    % Output : - A vector of logicals indicating if each column exist in
    %            table.
    % AUthor : Eran Ofek (Mar 2023)
    % Example: T = table(rand(10,1),rand(10,1),'VariableNames',{'A','B'})
    %          tools.table.isColumn(T,{'B','C','A'})

    Result = ismember(Cols, T.Properties.VariableNames);

end
