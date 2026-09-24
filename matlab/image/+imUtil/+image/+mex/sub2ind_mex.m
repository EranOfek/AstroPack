% Fast mex for convert 2D matrix subscripts to linear index.
%   No boundries checking.
%   For vector of linear indices >~100, this function is faster than 
%   sub2ind but slower than imUtil.image.sub2ind_fast
%   For small arrays use sub2ind
% Input  : - A matrix [I, J] size.
%          - I 2D matrix subscript.
%          - J 2D matrix subscript.
% Output : - Linear index.
% Author : Eran Ofek (2026 Feb) 
% Example: [LI]=imUtil.image.mex.sub2ind_mex(Size, I, J);