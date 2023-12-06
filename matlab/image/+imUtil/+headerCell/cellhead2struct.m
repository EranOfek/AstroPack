function St=cellhead2struct(CellHeader,Args)
% convert a 3-column cell array containing header into a structure.
% Description: Remove duplicate keys, illegal characters and the 'END' key.
% Input  : - A 3-column cell array.
%          * ...,key,val,...
%            'Occurence' - If duplicate keys which one to choose:
%                   ['first'] | 'last'.
%            'RemoveEnd' - Remove the 'END' keyword. Default is true.
%            'ColKey' - Column index of keyword name. Default is 1.
%            'ColVal' - Column index of value. Default is 2.
%            'ColComment' - Column index of comment. Default is 3.
% Output : - A structure, in which the field names are the keys.
% Author : Eran Ofek (Mar 2021)
% Example: St=imUtil.headerCell.cellhead2struct({'A',1,'';'B',2,'';'A','a','';'C','aaa','';'END','',''})

    arguments
        CellHeader cell         
        Args.Occurence                = 'first';
        Args.RemoveEnd(1,1) logical   = true;
        Args.ColKey(1,1)              = 1;
        Args.ColVal(1,1)              = 2;
        Args.ColComment(1,1)          = 3;
    end
    
    
    Flag = ~cellfun(@isempty,CellHeader(:,Args.ColKey));
    Cell = imUtil.headerCell.replace_illegal_char(CellHeader(Flag,:));
    % select only unique keys
    [~,UI] = unique(Cell(:,1),Args.Occurence);
    Cell   = Cell(UI,:);
    St     = cell2struct(Cell(:,Args.ColVal),Cell(:,Args.ColKey),1);
    if Args.RemoveEnd
        if isfield(St,'END')
            St = rmfield(St,'END');
        end
    end
               
end
