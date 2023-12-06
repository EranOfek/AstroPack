function Cell = deleteKey(Cell,Key)
% delete lines with specific keys
% Input  : - A 2 or 3 columns cell array
%          - A key name or a cell array of key names.
% Output : - The input cell in which the lines with specific keys in the
%            first column were removed.
% Author: Eran Ofek  (Mar 2021)
% Example:
% imUtil.headerCell.deleteKey({'A','a','';'B','a','';'C',1,''},'A')
% imUtil.headerCell.deleteKey({'A','a','';'B','a','';'C',1,''},{'A','C'})

    arguments
        Cell
        Key
        %Args
    end
    
    VecInd   = (1:1:size(Cell,1)).';
    [~,~,II] = imUtil.headerCell.getByKey(Cell,Key,'Fill',[]);
    Flag     = ~ismember(VecInd,II);
    Cell     = Cell(Flag,:);
    
end

