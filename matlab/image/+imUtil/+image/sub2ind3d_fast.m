function Ind=sub2ind3d_fast(Size,Y,X,Z)
% sub2ind fast version for 2D matrices
% Description: A fast version of sub2ind for 3D arrays.
% Input  : - Array size [Y,X].
%          - Y (i) positions (whole pixels)
%          - X (j) positions (whole pixels)
%          - Z (k) positions (whole pixels)
% Output : - Linear index of position in array in uint32 format.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Sep 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Ind=imUtil.image.sub2ind3d_fast([10 11 12],3,5,4)
% Reliable: 2
%--------------------------------------------------------------------------

%Ind = uint32((Z-1).*Size(1).*Size(2) + Size(1).*(X-1) + Y);
Ind = uint32((Z-1).*Size(1).*Size(2)) + uint32(Size(1).*(X-1) + Y);

