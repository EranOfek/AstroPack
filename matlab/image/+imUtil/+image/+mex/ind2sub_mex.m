% Fast mex for convert linear index to 2D matrix subscript.
%   No boundries checking.
%   For vector of linear indices >~100, this function is faster than 
%   ind2sub and imUtil.image.ind2sub_fast
%   For small arrays use ind2sub.
% Input  : - A matrix [I, J] size.
%          - Linear index.
% Output : - I matrix subscript (without checking boundries).
%          - J matrix subscript.
% Author : Eran Ofek (2026 Feb) 
% Example: [I,J]=imUtil.image.mex.ind2sub_mex(Size, LI);