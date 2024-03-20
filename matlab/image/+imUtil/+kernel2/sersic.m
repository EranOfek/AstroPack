function K=sersic(Par,SizeXY,PosXY)
% create a matrix or a cube of 2D normalized Sersic functions 
% Package: +imUtil.kernel2
% Input  : - A matrix with three columns containing the Sersic parameters
%            [EffectiveRadius, n, k].
%           (Sigma) A one column or three column matrix.
%            One row per Gaussian template (in the output cube).
%            The columns are: (SigmaX, [SigmaY, Rho]]).
%            SigmaX is the gaussian sigma in the X direction (=FWHMx/2.35)
%            If three elements are provided then the second element is
%            SigmaY (sigma in the Y direction), and the third is the corr.
%            coef. Rho.
%            If one element is given, then the default is SigmaY=SigmaX,
%            and Rho=0.
%            If not provided then the default is [2 2 0].
%          - Stamp size [X,Y]. Default is [21 21].
%          - [X,Y] Position of the Gaussian center in the stamp.
%            Default is the ceil(stamp_size/2).
% Output : - A matrix or a cube with the 2D Gaussian which sum is
%            normalized to 1.
%            If a cube, the third dimension corresponds to the template
%            index.
%      By: Eran O. Ofek                         May 2020
% Example: imUtil.kernel2.sersic


if nargin<3
    PosXY = [];
    if nargin<2
        SizeXY = [21 21];
        if nargin<1
            Par = [2 4 1];  %Re, n, k
        end
    end
end


if isempty(PosXY)
    PosXY = ceil(SizeXY.*0.5);
end

Npar  = size(Par,1);
if size(Par,2)==1
    Par   = [Par, ones(Npar,1).*4, ones(Npar,1)];
elseif size(Par,2)==2
    Par   = [Par, ones(Npar,1)];
else
    % do nothing
end


SerRe = Par(:,1);
SerN  = Par(:,2);
SerK  = Par(:,3);


[MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(1), (1:1:SizeXY(2))-PosXY(2) );
MatR2 = MatX.^2 + MatY.^2;

K = zeros(SizeXY(2),SizeXY(1),Npar);
for I=1:1:Npar
    K(:,:,I) = exp(-SerK(I).*sqrt(MatR2./SerRe(I)).^(1./SerN(I)));
end