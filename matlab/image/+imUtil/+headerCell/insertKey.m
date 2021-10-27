function NewCell=insertKey(Cell,KeyVal,Pos)
% Insert key/val into a header in cell array format
% Input  : - A 2 or 3 columns cell array.
%          - Either a string or a cell array of key/vals to insert.
%            If a string or a cell-column of keys, than these will be
%            padded with '' and inserted to the cell.
%            Alternatively, this can be a sub cell-header.
%          - Position to insert the new block: ['end-1'], 'end' | Inf or number.
% Author: Eran Ofek  (Mar 2021)
% Example:
% NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},'C','end-1')
% NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},'C','end')
% NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},'C',1)
% NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},{'C';'D'},2)
% NewCell=imUtil.headerCell.insertKey({'A','','';'B',1,'';'END','',''},{'C',1;'D',2},'end-1')

    arguments
        Cell
        KeyVal
        Pos                 = 'end-1';
    end

    if isinf(Pos)
        Pos = 'end';
    end
    
    if ~iscell(KeyVal)
        KeyVal = {KeyVal};
    end
    
    [Nline,Ncol] = size(Cell);
    if ischar(Pos)
        switch lower(Pos)
            case 'end'
                Pos = Nline+1;  % Ind is the new position to insert
            case 'end-1'
                Pos = Nline;
            otherwise
                error('Unknown Pos string option');
        end
    end
    
    SizeKeyVal = size(KeyVal);
    % complete columns
    KeyVal = [KeyVal, cell(SizeKeyVal(1), Ncol-SizeKeyVal(2))];
    
    % if Cell is empty
    if isempty(Cell)
        Pos = 1;
    end
    
    if Pos==1
        NewCell = [KeyVal;Cell];
    elseif Pos==(Nline+1)
        NewCell = [Cell;KeyVal];
    else
        NewCell = [Cell(1:Pos-1,:); KeyVal; Cell(Pos:end,:)];
    end

end