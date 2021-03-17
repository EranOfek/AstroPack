function K=circ(Radius,SizeXY,PosXY)
% create a matrix or a cube of 2D normalized corcular shape in each image 
% Package: +imUtil.kernel2
% Input  : - (Radius) Vector of radii of circles.
%            If not provided then the default is [3].
%          - Stamp size [X,Y]. Default is [7 7].
%          - [X,Y] Position of the circle center in the stamp.
%            Default is the ceil(stamp_size/2).
% Output : - A matrix or a cube with the 2D circle which sum is
%            normalized to 1.
%            If a cube, the third dimension corresponds to the template
%            index.
%      By: Eran O. Ofek                         May 2020
% Example: imUtil.kernel2.circ


if nargin<3
    PosXY = [];
    if nargin<2
        SizeXY = [7 7];
        if nargin<1
            Radius = [3];
        end
    end
end

if isempty(PosXY)
    PosXY = ceil(SizeXY.*0.5);
end

Ntemp = numel(Radius);

[MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(1), (1:1:SizeXY(2))-PosXY(2) );
MatR        = sqrt(MatX.^2 + MatY.^2);

K = zeros(SizeXY(2),SizeXY(1),Ntemp);
for I=1:1:Ntemp
    Tmp = zeros(SizeXY(2),SizeXY(1));
    Tmp(MatR<Radius(I)) = 1;
    
    K(:,:,I) =  Tmp./sum(Tmp,'all');
    
end