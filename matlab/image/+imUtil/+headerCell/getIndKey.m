function [IndKey]=getIndKey(Cell,Key,Args)
% get a cell array in which each cell contains the indices of the found keys in the cell-header.
%       This function is a subset of imUtil.headerCell.getBeyKey
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
% Output : - A cell array (number of elements equal to the number of keys)
%            in which each cell contains the indices of the found keys in
%            the cell-header.
% Author: Eran Ofek  (Mar 2021)
% Example: [II]=imUtil.headerCell.getIndKey({'ExpTime',2,'';'A','a',''},'ExpTime')
% Reliable: 2

    arguments
        Cell cell
        Key                  
        Args.SearchAlgo char                   = 'strcmp'; 
        Args.CaseSens(1,1) logical             = true;
        Args.Fill                              = NaN;
        Args.Col(1,1) double                   = 1;
        Args.ReturnN(1,1) double               = 1;
    end
    
    
    if (ischar(Key))
        Key = {Key};
    end
    Nkey = numel(Key);
    
    if ~Args.CaseSens
        Key = lower(Key);
        Cell(:,Args.Col) = lower(Cell(:,Args.Col));
    end
    
    % for each keyword name
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
    end

end  % end getIndKey function