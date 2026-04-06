function [R,PR,R_f,PR_f]=combine_proper(Data,PSF,Args)
% Proper coaddition of images in a cube
% Package: imUtil.image
% Description: Proper coaddition (Zackay & Ofek 2017) of images in a cube
% Input  : - A cube of images, where the 3rd dimension is the image index.
%          - A cube of PSFs, where the 3rd dimension is the image index.
%            Alternatively, this can be a single PSF.
%            The PSF is centered in the stamp.
%          * Arbitrary number of ...,key,val,... arguments.
%            The following keywords are available:
%            'F' - A vector of weights (one weight per image).
%                   Default is 1.
%            'Var' - A vector of variances (one variance per image).
%                   Default is 1.
%            'Norm' - A logical flag indicating if to normalize the PSF to
%                   unity prior to coaddition. 
%                   Default is true.
%            --- PSF ---
%            'CenterPSF' - Center the PSF to the image center.
%                   Default is false.
%            'SizePSF'   - If CenterPSF is true, then will cut the PSF image size
%                   to this size. If empty, do nothing. Default is [].
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
%          [R,PR,R_f,PR_f]=imUtil.properCoadd.combine_proper(Data,Psf)
% Reliable: 
%--------------------------------------------------------------------------

arguments
    Data
    PSF
    Args.F                     = 1;
    Args.Var                   = 1;
    %Args.PsfType               = 'center';
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
%PSF = imUtil.psf.padShift(PSF, SizeData(1:2));
PSF = imUtil.psf.stamp2full(PSF, SizeData(1:2), 'CenterPosition','corner');

% switch lower(Args.PsfType)
%     case 'center'
%         % put PSF in corner
%         PSF = ifftshift(ifftshift(PSF,1),2);
%         PadSize = SizeData(1:2) - SizePsf(1:2);
%         PSF = padarray(PSF,PadSize,0,'post');
%     case 'corner'
%         PadSize = SizeData(1:2) - SizePsf(1:2);
%         PSF = padarray(PSF,PadSize,0,'post');
%     otherwise
%         error('Unknown PsfType option');
% end



% proper coaddition: Zackay & Ofek 2017
PSF_f = fft2(PSF);

% FFU: use norm instaed of sqrt(sum... faster
WW_n  = reshape( (Args.F.^2./Args.Var), 1, 1, []);
WW_d  = reshape( Args.F./Args.Var, 1, 1, []);

PR_f  = sqrt(sum(WW_n .* abs(PSF_f).^2,IndexDim));
R_f   = sum(WW_d .* fft2(Data).*conj(PSF_f),IndexDim)./PR_f;
R     = ifft2(R_f);
PR    = ifft2(PR_f);


