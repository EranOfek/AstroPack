function K=line(Par,SizeXY,PosXY)
% create a matrix or a cube of 2D normalized corcular shape in each image 
% Package: +imUtil.kernel2
% Input  : - (Par) A three or four column matrix of
%            [Length, Width, Angle, Gap, [sigma]] of line. If three columns are given
%            then Gap is set to zero.
%            sigma is an optional parameter. If given, then the output will
%            be convolved with a Gaussian with this width.
%            Length is the line length (odd number).
%            Width is the line width.
%            Angle is the angle in degrees as measured from the X-axis
%            anti-clock wise.
%            Gap is the radius of a gap in the center of the line.
%            Default is [11 1 0 0];
%          - Stamp size [X,Y]. Default is [11 11].
%          - [X,Y] Position of the circle center in the stamp.
%            Default is the ceil(stamp_size/2).
% Output : - A matrix or a cube with the 2D lines which sum is
%            normalized to 1.
%            If a cube, the third dimension corresponds to the template
%            index.
%      By: Eran O. Ofek                         May 2020
% Example: imUtil.kernel2.line
%          imUtil.kernel2.line([11 2 20 5],[30 30])


if nargin<3
    PosXY = [];
    if nargin<2
        SizeXY = [12 12];
        if nargin<1
            Par = [11 1 0 0];
        end
    end
end

if isempty(PosXY)
    PosXY = ceil(SizeXY.*0.5);
end

Length = Par(:,1);
Width  = Par(:,2);
Angle  = Par(:,3);
Ntemp  = numel(Length);
if size(Par,2)==3
    Gap = zeros(Ntemp,1);
else
    Gap    = Par(:,4);
end
   
% set angle to the [-90..+90] range
Angle = mod(Angle,180);
Flag = Angle>90;
Angle(Flag) = Angle(Flag) - 180;

if any(isnan(SizeXY))
    SizeXY = ceil(max(Length));
    SizeXY = [SizeXY, SizeXY];
end

X = (1:1:SizeXY(1))-PosXY(1);
Y = (1:1:SizeXY(2))-PosXY(2);

[MatX,MatY] = meshgrid( X, Y);
MatR        = sqrt(MatX.^2 + MatY.^2);

K = zeros(SizeXY(2),SizeXY(1),Ntemp);
for I=1:1:Ntemp
    Tmp = zeros(SizeXY(2),SizeXY(1));
    
    Tmp(abs(MatY)<(Width.*0.5) & abs(MatX)<(Length.*0.5)) = 1;

    Tmp = imrotate(Tmp,Angle(I),'nearest','crop');
    Tmp(MatR<Gap(I)) = 0;
        
    if size(Par,2)>4
        % convolve with Gaussian
        KG  = imUtil.kernel2.gauss(Par(I,5), SizeXY);
        Tmp = conv2(Tmp, KG, 'same');
    end
    
    K(:,:,I) =  Tmp./sum(Tmp,'all');
end

