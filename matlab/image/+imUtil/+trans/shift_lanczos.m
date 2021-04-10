function [ShiftedImage]=shift_lanczos(Image,ShiftXY,A,ShiftType)
% Shift Image using the sub pixel Lanczos filter
% Package: imUtil.image
% Description: Shift an image using the Lanczos interpolation kernel.
%              This is works for sub pixel shifts and can treat images
%              which are larger then 9x9.
% Input  : - An image (2D matrix), or a cube of images in which the image
%            index is the 3rd dimension.
%          - A two element vector of [ShiftX, ShiftY] in pixels.
%            If the first input is a cube, then this may be a two column
%            matrix with shift per image (in each row).
%            Otherwise all the images will be shifted by the same shift.
%          - Lanczos parameters (e.g., 2 or 3). Default is 3.
%          - ShiftType: 'circ' (circular) or 'noncirc' (non-circular).
%            Default is 'noncirc'.
% Output : - Shifted image with the same size as the input image.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Jun 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: G=imUtil.kernel2.gauss(2,[31 31]);
%          imUtil.image.moment2(G,16,16) 
%          ShiftedImage=imUtil.trans.shift_lanczos(G,[1.22,-2.1],3,'circ');
%          imUtil.image.moment2(ShiftedImage,16,16) 
%          ShiftedImage=imUtil.trans.shift_lanczos(G,[1.22,-2.1],3,'noncirc');
%          imUtil.image.moment2(ShiftedImage,16,16) 
% Reliable: 

if nargin<4
    ShiftType = 'noncirc';
    if nargin<3
        A = 3;
    end
end

WholeShiftXY = floor(ShiftXY);  % whole pix shoft
PhaseShiftXY = mod(ShiftXY,1);  % reminder (sub pix) shift)
Nsh = size(ShiftXY,1);

SizeXY   = [9 9];
CenterXY = [5 5];
PosXY    = PhaseShiftXY + CenterXY;

SizeIm = size(Image);
Nim = size(Image,3);
ShiftedImage = zeros(size(Image));
for Iim=1:1:Nim
    Ish = min(Iim,Nsh);
    
    switch lower(ShiftType)
        case 'circ'
            ShiftedImage(:,:,Iim) = circshift(Image(:,:,Iim),WholeShiftXY(Ish,1),2);
            ShiftedImage(:,:,Iim) = circshift(ShiftedImage(:,:,Iim),WholeShiftXY(Ish,2),1);
        case 'noncirc'

            VecX = (1:1:SizeIm(2));
            VecY = (1:1:SizeIm(1));
            ShiftedImage(:,:,Iim) = interp2(VecX,VecY,Image(:,:,Iim),VecX-WholeShiftXY(Ish,1), VecY(:)-WholeShiftXY(Ish,2),'nearest',0);
        otherwise
            error('Unknown ShiftType option');

    end 

    [F] = imUtil.kernel2.lanczos(A,SizeXY,PosXY(Ish,:));
    ShiftedImage(:,:,Iim) = imUtil.filter.conv2_fast(ShiftedImage(:,:,Iim),F);
end