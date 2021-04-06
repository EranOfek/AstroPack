function SearchFun = stringSearchFun(UseRegExp, CaseSens)
% Return a function handle for a string search (i.e., strcmp, strcmpi, regexp, regexpi
% Input  : - A logical indicating if to use regular expression (true) or
%            strcmp (false).
%          - A logical indicating if to use case sensetive search (true) or
%            not (false).
% Output : - A function handle @(str1, str2)
% Author : Eran Ofek (Apr 2021)
% Example: SearchFun = tools.string.stringSearchFun(true, true)
if UseRegExp
    if CaseSens
        SearchFun = @(x,y) ~tools.cell.isempty_cell(regexp(x,y,'match'));
    else
        SearchFun = @(x,y) ~tools.cell.isempty_cell(regexpi(x,y,'match'));
    end
else
    if CaseSens
        SearchFun = @(x,y) strcmp(x,y);
    else
        SearchFun = @(x,y) strcmpi(x,y);
    end
end
            