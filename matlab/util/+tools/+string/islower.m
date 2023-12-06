function Result = islower(Str)
    % return true is char array is lower case
    % Input  : - A char array.
    % Output : - A logical indicating if input is lower case.
    % Author : Eran Ofek (Jun 2022)
    % Example: tools.string.islower('aa')
    
    Result = strcmp(lower(Str), Str);
        
end