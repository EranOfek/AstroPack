function Cell=replaceKey(Cell,Key,Val,Args)
% Replace an cell-header keywords and values, or add if doesn't exist
% Input  : - A 3 column cell array
%          - A key name or a cell array of key names.
%          - A cell array of values, corresponding to the key
%            names. Alternatively, a vector of numbers corresponding to the
%            key names.
%          * ...,key,val,... or ...,key=val',... list
%            'SearchAlgo' - search using: ['strcmp'] | 'regexp'
%            'CaseSens' - Default is true.
%            'RepVal'   - Replace value. Default is true.
%            'Comment'  - A cell array of optional comments.
%                   If empty, then do not replace comment. Default is [].
%            'NewKey' - A cell array of new keys to replace the old keys.
%                   If empty, then do not replace keys.
%                   Default is {}.
%            'AddKey' - Add key if doens't exist. Default is true.
%            'AddPos' - Position in which to add key if doesn't exist.
%                   Default is 'end-1'.
%            'ColKey' - Column index of keys. Default is 1.
%            'ColVal' - Column index of values. Default is 2.
%            'ColComment' - Column index of comments. Default is 3.
% Output : - A new cell array with the replaced keys/values.
% Example:
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'C','aaa','a'},'A',{'AA'})
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},{10 12})
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},{10 12},'RepVal',false,'NewKey',{'q','r'})
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'B','aaa','a'},{'A','B'},{'pp' 12},'RepVal',true,'NewKey',{'q','r'})
% Cell=imUtil.headerCell.replaceKey({'A',1,'';'B','a','';'C','aaa','a'},'D',{'AA'})


    arguments
        Cell
        Key
        Val 
        Args.SearchAlgo char                          = 'strcmp'; 
        Args.CaseSens(1,1) logical                    = true;
        Args.RepVal(1,1) logical                      = true;
        Args.Comment                                  = [];
        Args.NewKey                                   = {};
        Args.AddKey(1,1) logical                      = true;
        Args.AddPos                                   = 'end-1';
        Args.ColKey(1,1) uint8                        = 1;
        Args.ColVal(1,1) uint8                        = 2;
        Args.ColComment(1,1) uint8                    = 3;
    end

    if ~iscell(Val)
        if isnumeric(Val)
            Val = num2cell(Val);
        else
            Val = {Val};
        end
    end
    
    if ischar(Key)
        Key = {Key};
    end
%     if ischar(Val)
%         Val = num2cell(Val);
%     end
    
    Nkey = numel(Key);
    
    %[~,~,~,IK] = imUtil.headerCell.getByKey(Cell,Key,'SearchAlgo',Args.SearchAlgo,'CaseSens',Args.CaseSens,'ReturnN',Inf);
    [IK] = imUtil.headerCell.getIndKey(Cell,Key,'SearchAlgo',Args.SearchAlgo,'CaseSens',Args.CaseSens,'ReturnN',Inf);

    
    Nline    = size(Cell,1);
    %FlagLine = true(Nline,1); % flag lines to keep
    for Ikey=1:1:Nkey
        %         if Args.DelDup
        %             FlagLine(IK{Ikey}(2:end)) = false;
        %         end
        
        if ~isempty(Args.NewKey)
            [Cell(IK{Ikey},Args.ColKey)] = deal(Args.NewKey(Ikey));
        end
        if Args.RepVal
            if Args.AddKey && isempty(IK{Ikey})
                % add key/val if doesn't exist
                Cell = imUtil.headerCell.insertKey(Cell, [Key(Ikey), Val(Ikey)], Args.AddPos);
            else
                [Cell(IK{Ikey},Args.ColVal)] = deal(Val(Ikey));
            end
        end
        
        if iscell(Args.Comment) || ischar(Args.Comment)
            [Cell(IK{Ikey},Args.ColComment)] = deal(Args.Comment(Ikey));
        end
       
    end
            
%     if Args.DelDup
%         Cell = Cell(FlagLine,:);
%     end
    

end