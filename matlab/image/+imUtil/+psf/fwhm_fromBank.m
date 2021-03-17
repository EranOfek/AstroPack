function [FWHM,Nstars]=fwhm_fromBank(Image,varargin)
% Measure the FWHM of an image by cross corr. with a Gaussian template bank
% Package: +imUtil.psf
% Description: Measure the FWHM of an image by cross corr. with a Gaussian
%              template bank and choose the best FWHM by the mode of most
%              detection above some S/N.
% Input  : - An image in matrix format.
%          * list of ...,key,val,...
%            'MinSN' - Minimum S/N to use. Default is 50.
%            'Background' - A background image. Default is [].
%            'Variance'   - A variance image. Default is [].
%            'SigmaVec'   - Vector of the Gaussian bank sigmas.
%                           Default is logspace(0,1,25).'
%            'MinStars'   - Minimum numbre of stars needed to estimate
%                           FWHM. Default is 5.
%            'PixScale'   - Pixel scale ["/pix]. Default is 1.
% Output : - FWHM [arcsec].
%          - Number of stars used for estimating the FWHM.
% Author : Eran Ofek (Mar 2021)
% Example: FWHM=imUtil.psf.fwhm_fromBank(Image);

InPar = inputParser;
addOptional(InPar,'MinSN',50); 
addOptional(InPar,'Background',[]); 
addOptional(InPar,'Variance',[]); 
addOptional(InPar,'SigmaVec',logspace(0,1,25).');
addOptional(InPar,'MinStars',5);
addOptional(InPar,'PixScale',1);  % "/pix
parse(InPar,varargin{:});
InPar = InPar.Results;


InPar.SigmaVec = [0.1, InPar.SigmaVec];

% filter image with filter bandk of gaussians with variable width
SN = imUtil.filter.filter2_snBank(Image,InPar.Background,InPar.Variance,@imUtil.kernel2.gauss,InPar.SigmaVec);
% Pos contains: [X,Y,SN,index]
[~,Pos,MaxIsn]=imUtil.image.local_maxima(SN,1,InPar.MinSN);

% remove sharp objects
Pos = Pos(Pos(:,4)~=1,:);

Nstars = size(Pos,1);

if Nstars<InPar.MinStars;
    FWHM = NaN;
else
    % instead one can check if the SN improves...
    FWHM = 2.35.*InPar.PixScale.*InPar.SigmaVec(mode(Pos(:,4),'all'));
end
