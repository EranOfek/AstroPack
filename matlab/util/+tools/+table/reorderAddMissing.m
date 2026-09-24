function [Tab] = reorderAddMissing(Tab, Cols, Args)
    % Reorder table columns according to column names template and add missing columns.
    %   See alos: tools.table.addMissingColsDefault
    % Input  : - A table.
    %          - A cell array/string array of column names.
    %            Columns in this list that do not appear in the table will
    %            be added with the defealt value ('DefVal' argument).
    %            Next, the columns will be ordered according to their order
    %            in this input.
    %            Columns in the table that are not listed in this list,
    %            will be dropped from the table.
    %          * ...,key,val,... 
    %            'DefVal' - Default value for missing columns.
    %                   Default is NaN.
    % Output : - A table in which all the columns appear in the order
    %            listed in the 2nd input argument, and missing columns are
    %            populated with the default value.
    % Author : Eran Ofek (2026 Feb) 
    % Example: T=table(rand(5,1),rand(5,1),rand(5,1),'VariableNames',{'B','C','E'})
    %          tools.table.reorderAddMissing(T, {'A','B','C','D'})

    arguments
        Tab
        Cols
        Args.Defval    = NaN;
    end
  

    Nrow = size(Tab,1);
    ColTab = Tab.Properties.VariableNames;
    MissingCols = setdiff(Cols, ColTab);
    Nmissing    = numel(MissingCols);
    for Im=1:1:Nmissing
        Tab.(MissingCols{Im}) = repmat(Args.Defval, Nrow, 1);
    end

    % reoragnize
    Tab = Tab(:, Cols);

end
