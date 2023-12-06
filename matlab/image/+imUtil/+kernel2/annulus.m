function K=annulus(Radii,SizeXY,PosXY)
% create a matrix or a cube of 2D normalized annulus shape in each image 
% Package: +imUtil.kernel2
% Input  : - (Radii) A two column matrix of [Inner Outer] radius of the
%            annulus.
%            If not provided then the default is [7 11].
%          - Stamp size [X,Y]. Default is max(radii.*2+1)
%          - [X,Y] Position of the annulus center in the stamp.
%            Default is the ceil(stamp_size/2).
% Output : - A matrix or a cube with the 2D annulus which sum is
%            normalized to 1.
%            If a cube, the third dimension corresponds to the template
%            index.
%      By: Eran O. Ofek                         May 2020
% Example: imUtil.kernel2.annulus


if nargin<3
    PosXY = [];
    if nargin<2
        SizeXY = [];
        if nargin<1
            Radii = [7 11];
        end
    end
end

if isempty(SizeXY)
    SizeXY = ones(1,2).*(max(Radii(:)).*2 + 1);
end

if isempty(PosXY)
    PosXY = ceil(SizeXY.*0.5);
end

Ntemp = size(Radii,1);

[MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(1), (1:1:SizeXY(2))-PosXY(2) );
MatR        = sqrt(MatX.^2 + MatY.^2);

K = zeros(SizeXY(2),SizeXY(1),Ntemp);
for I=1:1:Ntemp
    Tmp = zeros(SizeXY(2),SizeXY(1));
    Tmp(MatR<Radii(I,2) & MatR>Radii(I,1)) = 1;
    
    K(:,:,I) =  Tmp./sum(Tmp,'all');
    
end