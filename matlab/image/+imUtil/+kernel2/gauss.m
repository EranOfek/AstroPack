function K=gauss(Sigma,SizeXY,PosXY)
% create a matrix or a cube of 2D normalized Gaussians 
% Package: +imUtil.kernel2
% Input  : - (Sigma) A one column or three column matrix.
%            One row per Gaussian template (in the output cube).
%            The columns are: (SigmaX, [SigmaY, Rho]]).
%            SigmaX is the gaussian sigma in the X direction (=FWHMx/2.35)
%            If three elements are provided then the second element is
%            SigmaY (sigma in the Y direction), and the third is the corr.
%            coef. Rho.
%            If one element is given, then the default is SigmaY=SigmaX,
%            and Rho=0.
%            If not provided then the default is [2 2 0].
%          - Stamp size [X,Y]. Default is [15 15].
%          - [X,Y] Position of the Gaussian center in the stamp.
%            Default is the ceil(stamp_size/2).
% Output : - A matrix or a cube with the 2D Gaussian which sum is
%            normalized to 1.
%            If a cube, the third dimension corresponds to the template
%            index.
%      By: Eran O. Ofek                         Apr 2020
% Example: imUtil.kernel2.gauss
%          imUtil.kernel2.gauss([1;2;3;4]); % a template bank of Gaussians.


if nargin<3
    PosXY = [];
    if nargin<2
        SizeXY = [15 15];
        if nargin<1
            Sigma = [2 2 0];
        end
    end
end

if isempty(PosXY)
    PosXY = ceil(SizeXY.*0.5);
end

[NrowSigma, NcolSigma] = size(Sigma);
if NcolSigma==1
    Sigma = [Sigma, Sigma, zeros(NrowSigma,1)];
end

SigmaX = Sigma(:,1);
SigmaY = Sigma(:,2);
Rho    = Sigma(:,3);

[MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(1), (1:1:SizeXY(2))-PosXY(2) );

K = zeros(SizeXY(2),SizeXY(1),NrowSigma);
for I=1:1:NrowSigma
%     K(:,:,I) = 1./(2.*pi.*SigmaX(I).*SigmaY(I).*sqrt(1-Rho(I).^2)) .* ...
%                        exp(-1./(2.*(1-Rho(I).^2)) .* ...
%                            (MatX.^2./SigmaX(I).^2 + ...
%                             MatY.^2./SigmaY(I).^2 - ...
%                 2.*Rho(I).*MatX.*MatY./(SigmaX(I).*SigmaY(I))));
    K(:,:,I) = exp(-1./(2.*(1-Rho(I).^2)) .* ...
                           (MatX.^2./SigmaX(I).^2 + ...
                            MatY.^2./SigmaY(I).^2 - ...
                2.*Rho(I).*MatX.*MatY./(SigmaX(I).*SigmaY(I))));
    
end

% doing direct normalization
% otherwise bug when sigma is small
K = K./sum(K,[1 2]);