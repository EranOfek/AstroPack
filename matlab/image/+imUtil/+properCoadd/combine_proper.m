function [R,PR,R_f,PR_f]=combine_proper(Data,PSF,Args)
% Proper coaddition of images in a cube
% Package: imUtil.image
% Description: Proper coaddition (Zackay & Ofek 2017) of images in a cube
% Input  : - A cube of images, where the 3rd dimension is the image index.
%          - A cube of PSFs, where the 3rd dimension is the image index.
%          * Arbitrary number of ...,key,val,... arguments.
%            The following keywords are available:
%            'F' - A vector of weights (one weight per image).
%                   Default is 1.
%            'Var' - A vector of variances (one variance per image).
%                   Default is 1.
%            'PsfType' - A string indicating where is the center of the
%                   PSF.
%                   'center' - PSF is centered in stamp.
%                   'corner' - PSF is in corner.
%            'Norm' - A logical flag indicating if to normalize the PSF to
%                   unity prior to coaddition. 
%                   Default is true.
% Output : - The proper coadded image.
%          - The proper PSF
%          - FFT of the proper coadded image.
%          - FFT of the proper PSF.
% Reference: Ofek & Zackay 2017, ApJ 836, 188
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    May 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Psf = imUtil.kernel2.gauss([1 2 3 4 5]');
%          Data = Psf + randn(size(Psf)).*0.001;
%          [R,PR,R_f,PR_f]=imUtil.image.combine_proper(Data,Psf)
% Reliable: 
%--------------------------------------------------------------------------

arguments
    Data
    PSF
    Args.F                     = 1;
    Args.Var                   = 1;
    Args.PsfType               = 'center';
    Args.Norm(1,1) logical     = true;
end



SizeData = size(Data);
SizePsf  = size(PSF);

IndexDim = 3;


% normalize PSF sum to unity
if Args.Norm
    PSF = PSF./sum(PSF,[1 2]);
end

   
% prep the PSF
switch lower(Args.PsfType)
    case 'center'
        % put PSF in corner
        PSF = ifftshift(ifftshift(PSF,1),2);
        PadSize = SizeData - SizePsf;
        PSF = padarray(PSF,PadSize,0,'post');
    case 'corner'
        PadSize = SizeData - SizePsf;
        PSF = padarray(PSF,PadSize,0,'post');
    otherwise
        error('Unknown PsfType option');
end



% proper coaddition: Zackay & Ofek 2017
PSF_f = fft2(PSF);

% FFU: use norm instaed of sqrt(sum... faster
PR_f  = sqrt(sum((Args.F.^2./Args.Var) .* abs(PSF_f).^2,IndexDim));
R_f   = sum((Args.F./Args.Var) .* fft2(Data).*conj(PSF_f),IndexDim)./PR_f;
R     = ifft2(R_f);
PR    = ifft2(PR_f);


