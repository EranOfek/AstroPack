function [K,CenterXY]=lanczos(A,SizeXY,PosXY)
% create a matrix or a cube of 2D normalized lanczis filter in each image 
% Package: +imUtil.kernel2
% Input  : - (A) A vector of the Lanczos parameters a=2 or a=3.
%            If not provided then the default is [2].
%          - Stamp size [X,Y]. Default is [7 7].
%          - [X,Y] Position of the circle center in the stamp.
%            Default is the ceil(stamp_size/2).
%            In order to generate an interpolation shift filter by sub
%            pixel phi, PosXY should be: ceil(stamp_size/2)+phi.
% Output : - A matrix or a cube with the 2D Lanczos filter which sum is
%            normalized to 1.
%            If a cube, the third dimension corresponds to the template
%            index.
%      By: Eran O. Ofek                         May 2020
% Example: imUtil.kernel2.lanczos(3,[8 8],[4.5 4.5])
%          imUtil.kernel2.lanczos([3;2],[8 8],[4.5 4.5])
%          imUtil.kernel2.lanczos([3;2],[8 8],[4.5 4.5; 2 2])


arguments
    A       = 2;
    SizeXY  = [7 7];
    PosXY   = [];
end

A2 = A.^2;

CenterXY = ceil(SizeXY.*0.5);
if isempty(PosXY)
    PosXY = ceil(SizeXY.*0.5);
end

Ntemp = numel(A);

Npos = size(PosXY,1);

K = zeros(SizeXY(2),SizeXY(1),Ntemp);
for I=1:1:Ntemp
    Ipos = min(I, Npos);
    %[MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(Ipos,1), (1:1:SizeXY(2))-PosXY(Ipos,2) );
    %MatR        = sqrt(MatX.^2 + MatY.^2);
    
    VecX = (1:1:SizeXY(1))-PosXY(Ipos,1);
    VecY = ((1:1:SizeXY(2))-PosXY(Ipos,2)).'; 
    MatR2 = VecX.^2 + VecY.^2;
    
    %Tmp = zeros(SizeXY(2),SizeXY(1));
    
    %Tmp = sinc(MatX).*sinc(MatX./A(I)).*sinc(MatY).*sinc(MatY./A(I));
    Tmp = sinc(VecX).*sinc(VecX./A(I)).*sinc(VecY).*sinc(VecY./A(I));
    Tmp(MatR2>A2(I)) = 0;
    
    K(:,:,I) =  Tmp./sum(Tmp,'all');
    
end