function [Ind, Name] = strNameDict2ind(StrName, Dict)
    % Search the first appearance of string in dictionary in another cell.
    % Input  : - A cell array of names.
    %          - A cell array of dictionary names.
    % Outout : - Index of the dictionary first found name in the cell of
    %            strings.
    %          - String name at that index.
    % Author : Eran Ofek (Jul 2021)
    % Example: [Ind, Name] = tools.cell.strNameDict2ind({'d','b','a','c'},{'a','A','c'})
    
    arguments
        StrName cell
        Dict cell
    end
    
    Ind  = find(ismember(StrName, Dict),1);
    Name = StrName{Ind};
    
end