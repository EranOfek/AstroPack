function [Mat,CellCell] = cellcell2mat_nan(CellCell)
    % Convert array of cells of cells to numric array
    % Input  : - Array of cells of cells.
    % Output : - A numeric array.
    %          - The cell array, where if the inner cell is of length one
    %            then the cell is repalced with the inner object.
    % Author : Eran Ofek (2025 Jan) 
    % Example: Tmp{1}{1}='a'; Tmp{2}{1}=1; Tmp{3}{1}='2'; Tmp{4}='3'; Tmp{5}=4;
    %          [M,C]=tools.cell.cellcell2mat_nan(Tmp);

    arguments
        CellCell
    end


    Size = size(CellCell);
    Mat  = nan(Size);
    N    = numel(Mat);
    for I=1:1:N
        if iscell(CellCell{I})
            if numel(CellCell{I})==1
                if isnumeric(CellCell{I}{1})
                    if ~isempty(CellCell{I}{1})
                        Mat(I) = CellCell{I}{1};
                    end
                else
                    Mat(I) = str2double(CellCell{I}{1});
                end
                CellCell{I} = CellCell{I}{1};
            
            end
        else
            if isnumeric(CellCell{I})
                if isempty(CellCell{I})
                    Mat(I) = NaN;
                else
                    Mat(I) = CellCell{I};
                end
            elseif ischar(CellCell{I}) || isstring(CellCell{I})
                Mat(I) = str2double(CellCell{I});
            else
                % do nothing
            end
        end
    end
                
               

end
