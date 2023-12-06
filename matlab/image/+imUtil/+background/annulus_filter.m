function [FiltImAnn, AreaAnnulus] = annulus_filter(Image, Annulus) 
% Apply annului filters to an image
% Package: @imUtil.background
% Input  : - A 2D image.
%          - A two column matrix of [Inner, Outer] radii of annulus.
%            Default is [10 15; 20 23; 40 41.6].
% Output : - A 3D matrix in which the 3rd dim is the image index (equal to
%            the number of filters). And the first 2D are the filtered
%            images.
%          - A vector of annuli area.
% Author: Eran Ofek (Jun 2021)
% Example: [F,A] = imUtil.background.annulus_filter(100+randn(1000,1000));

arguments
    Image
    Annulus            = [10 15; 20 23; 40 41.6];
end

MaxRadius = ceil(max(Annulus(:)));
%FilterCirc = imUtil.kernel2.circ(Annulus(:,1), [2.*MaxRadius+1, 2.*MaxRadius+1]);
FilterAnn  = imUtil.kernel2.annulus(Annulus, [2.*MaxRadius+1, 2.*MaxRadius+1]);

AreaAnnulus = pi.*(Annulus(:,2).^2 - Annulus(:,1).^2);

Nann = size(Annulus, 1);
SizeIm    = size(Image);
FiltImAnn = zeros(SizeIm(1), SizeIm(2), Nann);
for Iann=1:1:Nann
    %FiltImCirc = imUtil.filter.conv2_fast(Image, FilterCirc(:,:,Iann));
    FiltImAnn(:,:,Iann)  = imUtil.filter.conv2_fast(Image, FilterAnn(:,:,Iann));
end
    


