function [Y,X]=ind2sub_fast(Size,Ind)
% ind2sub fast version for 2D matrices
% Description: A fast version of ind2sub for 2D arrays.
%              If a non 2D array is provided then will use the built in
%              ind2sub function.
% Input  : - Array size [Y,X].
%          - Index
% Output : - Y (i) positions (whole pixels)
%          - X (j) positions (whole pixels)
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Mar 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [I,J]=imUtil.image.ind2sub_fast([3 3],2)
% Reliable: 2
%--------------------------------------------------------------------------

Y = mod(Ind-1,Size(1))+1;
X = ceil(Ind./Size(1));
    