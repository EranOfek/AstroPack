% Fast mex for sigma clipped mean of a cube of images
%   Analog to:
%         MA=mean(A,3,'omitnan'); SA=std(A,[],3,'omitnan'); Z= (A-MA)./SA;
%         Flag=Z<-2 | Z>2; A(Flag)=NaN; MA=mean(A,3,'omitnan'); NN=sum(~isnan(A),3);
% Input  : - A cube of images. The image index is in the 3rd dim.
%          - One iteration sigma clipping: [abs(LowNsigma), HighNsigma]
% Output : - Mean image.
%          - Image with the number of used images in each pixel
% Author : Eran Ofek (2026 Jan) 
% Example: [M,N]=sigma_clip_cube(A,[2 2]);