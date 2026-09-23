function [ShiftedImage]=shift_lanczos(Image, ShiftXY, A, IsCircFilt, PadVal)
% Shift Image using the sub pixel Lanczos filter
% Package: imUtil.image
% Description: Shift an image using the Lanczos interpolation kernel.
%              This is works for sub pixel shifts and can treat images
%              which are larger then 9x9.
% Input  : - An image (2D matrix), or a cube of images in which the image
%            index is the 3rd dimension.
%          - A two element vector of [ShiftX, ShiftY] in pixels, where X is
%            the column direction and Y is the row direction.
%            If the first input is a cube, then this may be a two column
%            matrix with shift per image (in each row).
%            Otherwise all the images will be shifted by the same shift.
%            The shift is split into a whole-pixel part, applied by
%            indexing (or by circshift for IsCircFilt), and a remainder in
%            [-0.5,0.5] applied by the Lanczos kernel. Note that until
%            Sep 2026 the whole-pixel part was applied only for
%            abs(shift)>1, so any shift in (-1,0) came out +1 pixel off
%            and a shift of exactly +-1 was a no-op (issue #1298).
%          - Lanczos parameters (e.g., 2 or 3). Default is 3.
%          - IsCirc - use circular shift (true), or non (false).
%            Default is false.
%          - Pad value for non circular shift. Default is 0.
% Output : - Shifted image with the same size as the input image.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Jun 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: G=imUtil.kernel2.gauss(2,[31 31]);
%          imUtil.image.moment2(G,16,16) 
%          ShiftedImage=imUtil.trans.shift_lanczos(G,[1.22,-2.1],3,true);
%          imUtil.image.moment2(ShiftedImage,16,16) 
%          ShiftedImage=imUtil.trans.shift_lanczos(ShiftedImage,[-1.22,2.1],3,true);
%          imUtil.image.moment2(ShiftedImage,16,16) 
%          max(abs(ShiftedImage-G),[],'all')
%          G=imUtil.kernel2.gauss([2;3],[31 31]);
%          imUtil.image.moment2(G,16,16) 
% Reliable: 

arguments
    Image
    ShiftXY
    A                         = 3;
    IsCircFilt(1,1) logical   = false;
    PadVal                    = 0;
end 

% Split into a whole-pixel shift and a remainder. Rounding (rather than floor)
% keeps the remainder in [-0.5,0.5], so for the common sub-pixel case the
% whole-pixel part is zero and only the Lanczos convolution is performed.
WholeShiftXY = round(ShiftXY);          % whole pix shift
PhaseShiftXY = ShiftXY - WholeShiftXY;  % sub pix remainder, in [-0.5,0.5]
Nsh = size(ShiftXY,1);

SizeXY   = [9 9];
CenterXY = [5 5];
PosXY    = PhaseShiftXY + CenterXY;

Nim = size(Image,3);
ShiftedImage = zeros(size(Image), 'like',Image);



A   = A.*ones(Nim,1);
[F] = imUtil.kernel2.lanczos(A,SizeXY,PosXY);
F   = cast(F, 'like',Image);


for Iim=1:1:Nim
    Ish = min(Iim,Nsh);
    if any(WholeShiftXY(Ish,:)~=0)
        if IsCircFilt
            Image(:,:,Iim) = circshift(Image(:,:,Iim), WholeShiftXY(Ish,[2 1]));
        else
            Image(:,:,Iim) = shiftWholePix(Image(:,:,Iim), WholeShiftXY(Ish,1), WholeShiftXY(Ish,2), PadVal);
        end 
    end

    ShiftedImage(:,:,Iim) = imUtil.filter.conv2_fast(Image(:,:,Iim),F(:,:,Iim));
end
end

function Image = shiftWholePix(Image, Dx, Dy, PadVal)
    % Shift a 2D image by a whole number of pixels, padding the vacated edge
    %     Replaces an interp2(...,'nearest') call, which is an expensive way
    %     of indexing when the shift is an exact number of pixels.
    % Input  : - A 2D image.
    %          - Shift along X (the column direction) [pix].
    %          - Shift along Y (the row direction) [pix].
    %          - Value with which to pad the vacated pixels.
    % Output : - The shifted image, of the same size and class as the input.
    % Author : A.M. Krassilchtchikov (Sep 2026)
    [Nrow, Ncol] = size(Image);
    Shifted      = repmat(cast(PadVal,'like',Image), Nrow, Ncol);
    DestRow      = max(1, 1+Dy):min(Nrow, Nrow+Dy);
    DestCol      = max(1, 1+Dx):min(Ncol, Ncol+Dx);
    if ~isempty(DestRow) && ~isempty(DestCol)   % otherwise the shift emptied the image
        Shifted(DestRow, DestCol) = Image(DestRow-Dy, DestCol-Dx);
    end
    Image = Shifted;
end
