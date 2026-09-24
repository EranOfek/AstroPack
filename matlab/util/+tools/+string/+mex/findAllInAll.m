% Find the indices of a string in the first input, in the second input.
% Input  : - A "search" cell array of char arrays.
%          - A "dictionary" cell array of char arrays.
% Output : - An array of indices. Each index correspond to the position of
%            the string in the first input, in the second input.
%            Return NaN if not found.
% Author : Eran Ofek (2025 Mar) 
% Example: A = {'a', 'b', 'c', 'x', 'y'};
%          B = {'c', 'a'};
%          Ind = tools.string.mex.findAllInAll(A,B);
%          Output is: 2, NaN, 1, NaN, NaN