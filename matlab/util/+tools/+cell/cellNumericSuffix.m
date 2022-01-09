function CellS = cellNumericSuffix(Str, Num, Format)
    % Add a range of numeric suffix to a string and put result in a cell array.
    % Input  : - A string.
    %          - A vector of numeric suffix.
    %          - Format of numeric suffix. Default is '_%d'.
    % Output : - A cell array.
    % Author : Eran Ofek (Jan 2022)
    % Example: CellS = tools.cell.cellNumericSuffix('SN', [1:3])
    
    arguments
        Str
        Num
        Format                 = '_%d';
    end
    
    FullFormat = sprintf('%%s%s',Format);
    
    N = numel(Num);
    CellS = cell(1,N);
    for I=1:1:N
        CellS{I} = sprintf(FullFormat, Str,Num(I));
    end
    
end