% Fast mex for robust std using 0.25-0.75 quantiles
% Input  : - Array
%          - Type (like matlab std). 0|1. 
%            If empty, then use 0.
%            Default is 0.
%          - Dimension along which to calculate the rstd.
%            Defauly is 1.
% Output : - The robust std.
% Author : Eran Ofek (2025 Oct) 
% Example: Rs=tools.math.stat.mex.rstd_mex(Array,[],1);