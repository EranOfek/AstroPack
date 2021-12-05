function [SubCell,FlagExist,IndFound,IndKey]=getByKey(Cell,Key,Args)
% get keyword value from an header in a cell format
% Package: @headCl
% Description: Given a 3 column cell array [Key, Val, Comment]
%              search for a keyword name and return the sub header that
%              contains the keyword.
% Input  : - A 2 or 3 columnn cell array [Key, Val, Comment].
%          - Keyword name, or a cell array of keyword names to serch.
%          * ...,key,val,... or ...,key=val',... list
%            'SearchAlgo' - search using: ['strcmp'] | 'regexp'
%            'CaseSens' - Default is true.
%            'Fill' - Fill value if not exist: [] - skip. Default is NaN.
%            'Col' - Column in cell in which to search. Default is 1.
%            'ReturnN' - What to do if more than one is found.
%                   [1] - return first, Inf - return all; 2 - return
%                   second,...
%            'Val2Num' - Attempt to convert value to number. Default is
%                   true.
% Output : - A 3 column cell array with only the lines that
%            satisfy the search, or the filled values.
%          - A vector of logicals (one per Key) that indicat if the key
%            exist in the Cell header.
%          - Line indices in the input cell that satisfy the
%            search criteria. Filled values are not included.
%          - A cell array (number of elements equal to the number of keys)
%            in which each cell contains the indices of the found keys in
%            the cell-header.
% Author: Eran Ofek  (Mar 2021)
% Example: [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'ExpTime')
%          [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},{'ExpTime','A'})
%          [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'a','Col',2)
%          [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'Exp*','SearchAlgo','regexp')
%          [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'TYPE','Fill',[])
%          [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime',2,'';'A','a',''},'TYPE','Fill',NaN)
%          [SC,FE,II]=imUtil.headerCell.getByKey({'ExpTime','2','';'A','a',''},'ExpTime')
%          [SC,FE,II,IK]=imUtil.headerCell.getByKey({'ExpTime','2','';'A','NaN',''},{'ExpTime','A'})
%          [SC,FE,II,IK]=imUtil.headerCell.getByKey({'ExpTime','2','';'A','NaN','';'A',1,''},{'ExpTime','A'},'ReturnN',Inf)
%          [SC,FE,II,IK]=imUtil.headerCell.getByKey({'ExpTime','2','';'A','NaN','';'A',1,''},{'ExpTime','A'},'ReturnN',1)
% Reliable: 2

    arguments
        Cell cell
        Key                  
        Args.SearchAlgo char                   = 'strcmp'; 
        Args.CaseSens(1,1) logical             = true;
        Args.Fill                              = NaN;
        Args.Col(1,1) double                   = 1;
        Args.ReturnN(1,1) double               = 1;
        Args.Val2Num(1,1) logical              = true;
    end
    ColVal = 2;
    
    
    if (ischar(Key))
        Key = {Key};
    end
    Nkey = numel(Key);
    
    if ~Args.CaseSens
        Key = lower(Key);
        Cell(:,Args.Col) = lower(Cell(:,Args.Col));
    end
    
    % for each keyword name
    IndFound  = [];
    FlagExist = false(Nkey,1); 
    SubCell   = cell(0,3);
    IndKey    = cell(1,Nkey);
    for Ikey=1:1:Nkey
        switch lower(Args.SearchAlgo)
            case 'strcmp'
                % use exact name search
                Flag = strcmp(Cell(:,Args.Col),Key{Ikey});
            case 'strcmpi'
                % use exact name search
                Flag = strcmpi(Cell(:,Args.Col),Key{Ikey});
            case 'regexp'
                Flag = ~cellfun(@isempty,regexp(Cell(:,Args.Col),Key{Ikey},'match'));
        end
        IndKey{Ikey} = find(Flag,Args.ReturnN,'first');
        IndFound     = [IndFound; IndKey{Ikey}];
        
        if ~isempty(Args.Fill) && ~any(Flag)
            SubCell = [SubCell; {Key{Ikey}, Args.Fill, ''}];
        else
            SubCell = [SubCell; Cell(IndKey{Ikey},:)];
        end
        
        
        
        if any(Flag)
           FlagExist(Ikey) = true;
        end
    end
    
    if Args.Val2Num
        %ValNum  = str2double(SubCell(:,ColVal));
        ValNum  = real(str2doubleq(SubCell(:,ColVal)));  % faster
        FlagNum = ~isnan(ValNum) | strcmpi(SubCell(:,ColVal),'nan');
        [SubCell(FlagNum,ColVal)] = deal(num2cell(ValNum(FlagNum)));
    end
        

end  % end getByKey function