function TrimStr=spacetrim(Str)
% Recursively replace any occurance of two spaces with a single space.
% Package: tools.string
% Description: Given a string, recursively replace any occurance of two
%              spaces with a single
%              space, such that the final product is a string with a
%              single spaces between words.
% Input  : - A string.
%          - A special charachter (default is ' ').
% Output : - A trimmed string.
% Tested : Matlab 7.13
%     By : Eran O. Ofek                    Aug 2012
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% See also: spacedel.m
% Example: tools.string.spacetrim({'aa    a','ysys  a a'});
%          tools.string.spacetrim('aa    a');
% Reliable: 2
%--------------------------------------------------------------------------


TrimStr = regexprep(Str,'  ',' ');

if (iscell(TrimStr))
    If = find(tools.cell.isempty_cell(strfind(TrimStr,'  '))==0);
else
    If = strfind(TrimStr,'  ');
end

if (~isempty(If))
   TrimStr=tools.string.spacetrim(TrimStr);
end



