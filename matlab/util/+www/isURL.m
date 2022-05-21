function Result = isURL(List)
    % Return a vector of logicals indicating if each element in an array is a URL string
    %   where a URL string starts with http:// or https://
    % Input  : - A char array, or a cell array of strings.
    % Output : - A vector of logical indicating, for ecah element in the
    %            input array, if the string is a URL.
    % Author : Eran Ofek (May 2022)
    % Example: Result = www.isURL('http://aaa')
   
    Out = regexp(List, '^http[s]?://', 'match');
    Result = ~cellfun(@isempty, Out);
    
end