function [T] = addStructFields2TableCols(T, St)
    % Add struct fields into new/existing columns in table.
    % Input  : - A table. 
    %          - A struct with some fields. The number of elements in each
    %            field must be identical to the number of lines in the table.
    % Output : - A table with the new/modified columns.
    % Author : Eran Ofek (2026 Mar) 
    % Example: T=tools.table.addStructFields2TableCols(T, St);

    arguments
        T
        St
    end

    FN = fieldnames(St);
    Nf = numel(FN);
    for If=1:1:Nf
        T.(FN{If}) = St.(FN{If})(:);
    end

end
