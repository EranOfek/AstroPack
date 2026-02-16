% A fast mex function for multi-value simultaneous binary search on a sorted vector
% Input  : - A vector of sorted values in which to search.
%          - A vector of target values to search in the 1st input argument.
%          - (DirIfFound) 
%            Controls behavior when multiple equal values are found:
%               -1  or 'first'   : return index of first occurrence (default)
%                0  or 'any'     : return any matching index
%                1  or 'last'    : return index of last occurrence
%
%          - (DirNotFound)
%            Controls behavior when the value is NOT found:
%                0     or 'exact'   : return 0
%               -1     or 'down'    : return index of largest DATA < ITEM
%                1     or 'up'      : return index of smallest DATA > ITEM
%                2     or 'closest' : return index of closest value (default)
%                0.5   or 'frac'    : return fractional position between neighbors
%          - A logical indicating if to check that input vector is sorted.
%            Default is false.
%          - A logical indicating if the output indeces are in double
%            format (true), or uint32 (false).
%            Default is true.
% Output : - A vector of indices of found elements (or nearby elements) in
%            the input vector.
% Author : ChatGPT + Eran Ofek (Feb 2026)
% Example: Ind=tools.find.mex.binarySearch(Vector, Target)