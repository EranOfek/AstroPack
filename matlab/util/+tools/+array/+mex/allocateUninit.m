% Array memory allocation without initialization
%   This function is much faster than repmat, but sometimes zeros is faster
%   Use only in cases no initialziation is required.
% Input  : - Vector array size (length >=2).
%          - Data type (e.g., 'single');
% Output : - Allocated array.
% Compilation: mex -R2018a allocateUninit.cpp
% Author : ChatGPT + Eran Ofek (Apr 2026)
% Example: Array=tools.array.mex.allocateUninit([1716 1716 3],'single');