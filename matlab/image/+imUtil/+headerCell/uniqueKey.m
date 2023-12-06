function [Result,UI]=uniqueKey(Cell,Args)
% Remove non-unique keywords from a cell header
% Input  : - A 2 or 3 column cell array.
%          * ..,key,val,... or ...,key=val',... list
%            'Col' - Column on which to operate the unique function.
%                   Default is 1.
%            'Occur' - Select occurence: ['first'] | 'last'.
% Output : - A cell array with unique keys.
%          - Indices of the selected rows.
% Author: Eran Ofek  (Mar 2021)
% Example: [SC,UI]=imUtil.headerCell.uniqueKey({'ExpTime',2,'';'A','a','';'ExpTime','3',''})

    arguments
        Cell
        Args.Col(1,1) uint8           = 1;
        Args.Occur                    = 'first'; % 'first' | 'last'
    end
    
    [~,UI] = unique(Cell(:,Args.Col),Args.Occur);
    Result = Cell(UI,:);
    
end

