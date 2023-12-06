function [Cube,RoundXorig,RoundYorig]=find_within_radius_mat(Matrix,X,Y,Radius,Circle)
% Construct a cube of stamps around specific locations in a 2D image.
% Package: imUtil.image
% Description: Given an image and a list of coordinates, construct a cybe
%              of stamps around the coordinates. Region outside the image
%              are padded with zeros.
%              coordinate a vector (within a cell array) of indices of
%              the points in the image
%              that are within a given radius from the coordinate.
%              Also return a corresponding vector of distances squared of
%              each point in the image from the coordinate.
%              See also find_within_radius_mat.m, but this function is
%              usually faster.
% Input  : - An array size [Y, X] (e.g., size(Image)).
%          - A vector of X coordinates around to search for the points
%            within the radius.
%          - A vector of Y coordinates around to search for the points
%            within the radius.
%          - Radius [pix].
%          - If true, then will set all points outside the radius to NaN.
%            Default is false.
% Output : - A cube of size 2.*Radius+1 by 2.*Radius+1 by number of
%            coordinates.
%            The cube contains the stamps around the requested rounded
%            coordinates. The third dimension is the coordinate index.
%          - Rounded X coordinates.
%          - Roundex Y coordinates.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Jun 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Image=rand(1024,1024); X=rand(100,1).*1023+1; Y=rand(100,1).*1023+1;
%          [Cube,RoundX,RoundY]=imUtil.cut.find_within_radius_mat(Image,X,Y,3);
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Matrix
    X
    Y
    Radius
    Circle(1,1) logical      = false;
end

PadVal = NaN;


Nsrc = numel(X);

ExtraPad    = 1;
SizeMatrix  = size(Matrix);

if any(X<0.5 | X>(SizeMatrix(2)+0.5)) || any(Y<0.5 | Y>(SizeMatrix(1)+0.5))
    error('Some X/Y coordinates are outside image boundries');
end


PadMatrix   = padarray(Matrix,[Radius+ExtraPad,Radius+ExtraPad],PadVal,'both');
SizePadMatrix = size(PadMatrix);

%VecX        = (1:1:SizeMatrix(2)+2.*(Radius+ExtraPad));  % add a pad+1, image start at Radius+2
%VecY        = (1:1:SizeMatrix(1)+2.*(Radius+ExtraPad));  
%[MatX,MatY] = meshgrid(VecX,VecY);

VecSub      = (-Radius:1:Radius); % center at 0
[SubMatX,SubMatY] = meshgrid(VecSub,VecSub);

RoundXorig = round(X(:));
RoundYorig = round(Y(:));

RoundX = RoundXorig + Radius + ExtraPad; % rounded coordinate in new system
RoundY = RoundYorig + Radius + ExtraPad;

% coordinate f each stamp:
% source per row, 
XcooMat = SubMatX(:).' + RoundX;
YcooMat = SubMatY(:).' + RoundY;
Ind  = imUtil.image.sub2ind_fast(SizePadMatrix,YcooMat,XcooMat);
Cube = reshape(PadMatrix(Ind)',Radius.*2+1,Radius.*2+1,Nsrc);

if Circle
    % set the values outside a circle radius to NaN
    MatR = sqrt(SubMatX.^2 + SubMatY.^2);
    CircMat = zeros(size(MatR));
    CircMat(MatR>Radius) = NaN;
    Cube = Cube + CircMat;
end
    
