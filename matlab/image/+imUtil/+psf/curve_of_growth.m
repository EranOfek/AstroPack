function Curve=curve_of_growth(Matrix,CenterPSFxy,Step)
% Calculate the curve of growth of a PSF
% Package: imUtil.psf
% Description: Calculate the curve of growth of a PSF.
% Input  : - 2-D matrix of a PSF.
%          - PSF center [x,y] coordinates. If [], or not provided, default
%            is half the PSF matrix size.
%          - Step size of curve of growth. Default is 1.
% Output : - A structure with the radiao profile, including the following
%            fields:
%            .Radius - radius
%            .Sum    -sum
%            .Npix   - number of pixels in annulus
%            .Mean   - mean
%            .Med    - median
%            .CumSum - cumulative sum.
% Tested : Matlab R2011b
%     By : Eran O. Ofek                    Mar 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Curve=imUtil.psf.curve_of_growth(imUtil.kernel2.gauss)
% Reliable: 2
%--------------------------------------------------------------------------

SizeXY = fliplr(size(Matrix));
if nargin<3
    Step = 1;
    
    if nargin<2
        CenterPSFxy = SizeXY.*0.5;  % [x,y] (not i,j)
    end
end

if isempty(CenterPSFxy)
    CenterPSFxy = SizeXY.*0.5;  % [x,y] (not i,j)
end


[MatX,MatY] = meshgrid((1:1:SizeXY(1))-CenterPSFxy(1),(1:1:SizeXY(2))-CenterPSFxy(2));
MatR = sqrt(MatX.^2 + MatY.^2);

MaxR = min([SizeXY - CenterPSFxy; abs(1-CenterPSFxy)]);

Radius = (0:Step:floor(MaxR)).';
Nrad   = numel(Radius) - 1;
%Curve.Radius = (Radius(1:end-1) + Radius(2:end)).*0.5;
% radius of area that is contained within the area of the inner and outer
% radius:
Curve.Radius = sqrt(mean([Radius(1:end-1), Radius(2:end)].^2,2));
Curve.Sum    = zeros(Nrad,1);
Curve.Npix   = zeros(Nrad,1);
Curve.Mean   = zeros(Nrad,1);
Curve.Med    = zeros(Nrad,1);
for Irad=1:1:Nrad
    Flag = MatR(:)>=Radius(Irad) & MatR(:)<Radius(Irad+1);
    Curve.Sum(Irad) = sum(Matrix(Flag));
    Curve.Npix(Irad) = sum(Flag);
    Curve.Mean(Irad) = Curve.Sum(Irad)./Curve.Npix(Irad);
    Curve.Med(Irad)  = median(Matrix(Flag));
end
Curve.CumSum = cumsum(Curve.Sum);
    
    

