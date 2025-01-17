% 2D bi-linear interpolation - fast MEX
%       (x2.6 times faster than interp2).
% Input  : - A 2D array which coordinates are the pixel indices.
%            Single or double.
%          - A 2D matrix of X interpolated position.
%          - A 2D matrix of Y interpolated position.
% Output : - Interpolated matrix
% Author : Eran Ofek (2025 Jan) 
% Example:  V=rand(1700,1700);                                                             
%           [MatX,MatY]=meshgrid((1:1700),(1:1700)); MatX1=MatX+0.1; MatY1=MatY+0.2;
%           tic;for I=1:1:100, Vq=tools.interp.mex.interp2d_bilinear(V,MatX1,MatY1);end,toc
%           tic;for I=1:1:100, Vq1=interp2(V,MatX1,MatY1);end,toc                          
%           max(abs(Vq-Vq1),[],'all')