function [Back,Var]=annulus_filter(Image,varargin) 
%
% Package: @imUtil.background
% Input  : - An array.
%          - Vector of dimensions over which to calculate the robust
%            variance. Default is [1 2].
% Output : - The robust median calculated using the scaled iqr
%      By: Eran O. Ofek                       Apr 2020             
% Example: imUtil.background.rvar(randn(1000,1000))


InPar = inputParser;

addOptional(InPar,'BackFunOut',{'back','var'});  % back, var, std


parse(InPar,varargin{:});
InPar = InPar.Results;


% construct 3 roughly equal area filters: 1 circular and 2 annular
Radius   = 11;
Annulus1 = 15;
Annulus1 = [Annulus1, ceil(sqrt(Radius.^2 + Annulus1.^2))];
Annulus2 = 25;
Annulus2 = [Annulus2, ceil(sqrt(Radius.^2 + Annulus2.^2))];

% construct unit-normalized filter
Filt1 = Kernel2.aper(Radius,2.*Radius+1,2.*Radius+1);
Filt2 = Kernel2.annulus(Annulus1(1), Annulus1(2), 2.*Annulus1(2)+1, 2.*Annulus1(2)+1);
Filt3 = Kernel2.annulus(Annulus2(1), Annulus2(2), 2.*Annulus2(2)+1, 2.*Annulus2(2)+1);

AreaFilt1 = sum(Filt1(:)>0);
AreaFilt2 = sum(Filt2(:)>0);
AreaFilt3 = sum(Filt3(:)>0);

ImFilt1 = imUtil.filter.conv2_fast(Image,Filt1);
ImFilt2 = imUtil.filter.conv2_fast(Image,Filt2);
ImFilt3 = imUtil.filter.conv2_fast(Image,Filt3);


