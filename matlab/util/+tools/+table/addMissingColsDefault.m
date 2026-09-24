function [Tab] = addMissingColsDefault(Tab, Cols, Args)
    % Add to table missing columns with default (no reorder).
    %   See also: tools.table.reorderAddMissing
    % Input  : - A table.
    %          - A cell array/string array of column names.
    %            Columns in this list that do not appear in the table will
    %            be added with the defealt value ('DefVal' argument).
    %            No changes other than that.
    %          * ...,key,val,... 
    %            'DefVal' - Default value for missing columns.
    %                   Default is NaN.
    % Output : - A table in which the missing columns are added.
    % Author : Eran Ofek (2026 Feb) 
    % Example: T=table(rand(5,1),rand(5,1),rand(5,1),'VariableNames',{'B','C','E'})
    %          tools.table.addMissingColsDefault(T, {'A','B','C','D'})


    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Feb) 
    % Example: tools.table.addMissingColumns(T, {'A','B','C'}, true)

    arguments
        Tab
        Cols
        Args.DefVal  = NaN;
    end

    Nrow  = size(Tab,1);
    IsCol = tools.table.isColumn(Tab, Cols);
    IndMissing = find(~IsCol);
    for Im=1:1:numel(IndMissing)
        Tab.(Cols{IndMissing(Im)}) = repmat(Args.DefVal, Nrow,1);
    end


end
