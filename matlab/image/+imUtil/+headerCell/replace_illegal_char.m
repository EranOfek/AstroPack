function Cell=replace_illegal_char(Cell,Args)
% replace illegal characters in keyword names (e.g. '-').
% Description: Given a 3 column cell array. Replace illegal
%              characters in the first column.
%              List of illegal characters:
%              '-' replace with '_'
% Input  : - A 3 columns cell array
%          * ...,key,val,... or ...,key=val',... list
%            'Col' - Column index on which to replace chars.
%                   Default is 1.
% Output : - A 3 column cell array.
% Example: Cell=imUtil.headerCell.replace_illegal_char({'A-NB',1,'';'a','1',''})
% Reliable: 2

    arguments
        Cell cell
        Args.Col(1,1) uint8    = 1;
    end

    Cell(:,Args.Col) = regexprep(Cell(:,Args.Col),'-','_');

end