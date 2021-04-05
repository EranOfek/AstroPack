function Cell=replaceKey(Cell,Key,Val,Args)
% Replace an cell-header keywords and values.
% Input  : - A 3 column cell array
%          - A key name or a cell array of key names.
%          - A vector or a cell array of values, corresponding to the key
%            names.
%          * ...,key,val,... or ...,key=val',... list
%            'SearchAlgo' - search using: ['strcmp'] | 'regexp'
%            'CaseSens' - Default is true.
%            'DelDup'   - Remove duplicate keys. Default is true.
%            'RepVal'   - Replace value. Default is true.
%            'Comment'  - A cell array of optional comments.
%                   If empty, then do not replace comment. Default is [].
%            'NewKey' - A cell array of new keys to replace the old keys.
%                   If empty, then do not replace keys.
%                   Default is {}.
%            'ColKey' - Column index of keys. Default is 1.
%            'ColVal' - Column index of values. Default is 2.
%            'ColComment' - Column index of comments. Default is 3.
% Output : - A new cell array with the replaced keys/values.
% Example:
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'C','aaa','a'},'A','AA')
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},[10 12])
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},[10 12],'DelDup',false)
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},[10 12],'RepVal',false,'NewKey',{'q','r'})
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},{'pp' 12},'RepVal',true,'NewKey',{'q','r'})


    arguments
        Cell
        Key
        Val
        Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
        Args.CaseSens(1,1) logical                    = true;
        Args.DelDup(1,1) logical                      = true;
        Args.RepVal(1,1) logical                      = true;
        Args.Comment                                  = [];
        Args.NewKey                                   = {};
        Args.ColKey(1,1) uint8                        = 1;
        Args.ColVal(1,1) uint8                        = 2;
        Args.ColComment(1,1) uint8                    = 3;
    end

    
    
    if ~iscell(Key)
        Key = {Key};
    end
    if ~iscell(Val)
        Val = num2cell(Val);
    end
    
    Nkey = numel(Key);
    
    [~,~,~,IK] = imUtil.headerCell.getByKey(Cell,Key,'SearchAlgo',Args.SearchAlgo,'CaseSens',Args.CaseSens,'ReturnN',Inf);

    
    Nline    = size(Cell,1);
    FlagLine = true(Nline,1); % flag lines to keep
    for Ikey=1:1:Nkey
        if Args.DelDup
            FlagLine(IK{Ikey}(2:end)) = false;
        end
        
        if ~isempty(Args.NewKey)
            [Cell(IK{Ikey},Args.ColKey)] = deal(Args.NewKey(Ikey));
        end
        if Args.RepVal
            [Cell(IK{Ikey},Args.ColVal)] = deal(Val(Ikey));
        end
        
        if iscell(Args.Comment) || ischar(Args.Comment)
            [Cell(IK{Ikey},Args.ColComment)] = deal(Args.Comment(Ikey));
        end
       
    end
            
    if Args.DelDup
        Cell = Cell(FlagLine,:);
    end
    

end