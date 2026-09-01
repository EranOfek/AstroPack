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
%            'F' - Vector of per-image flux zero-points (transparencies) F_j.
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
%            'AnnulusPre' - [Inner Outer] annulus cosbell taper radii (pix) to apply
%                   to all PSF prior to coadding.
%                   Preferably this should be done in an earlier step.
%                   If empty, then skip this step. Default is [].
%            'AnnulusPost' - [Inner Outer] annulus cosbell taper radii (pix) to apply
%                   to coadd PSF.
%                   If empty, then skip this step. Default is [5 8].
%            'ReCalcAfterAnnPost' - If 'AnnulusPost' is not empty, and this
%                   argument is true, then will redo the coaddition with
%                   the tapered PSF. Default is true.
%            'Full2stamp' - If false, then final PSF is of the image size.
%                   If true, then the final PSF stamp size is equal to the input
%                   PSF stamp size.
%                   Default is true.
%            'Convert2real' - Convert final coadd image to real.
%                   Useful since sometimes the output may have small
%                   imaginary part.
%                   Default is true.
% Output : - The proper coadded image R (ZO17 Eq. 7). Zero-point F_R.
%          - The proper PSF P_R, normalized to unit sum (ZO17 convention;
%            note: cropping by Full2stamp removes wing flux, so the
%            returned stamp sums to slightly less than 1).
%          - FFT of R.
%          - FFT of P_R (unit DC).
% Reference: Zackay & Ofek 2017, ApJ 836, 188
% Tested : Matlab R2015b
% Author : Eran Ofek (May 2020)
% Example: Psf = imUtil.kernel2.gauss([1 2 3 4 5]');
%          Data = Psf + randn(size(Psf)).*0.001;
%          [R,PR,R_f,PR_f]=imUtil.properCoadd.combine_proper(Data,Psf)

arguments
    Data
    PSF
    Args.F                     = 1;
    Args.Var                   = 1;
    %Args.PsfType               = 'center';
    Args.Norm(1,1) logical     = true;

    Args.AnnulusPre            = []; % preferably this should be done earlier
    Args.AnnulusPost           = [5 8];
    Args.ReCalcAfterAnnPost    = true; % only if AnnulusPost is not empty
    Args.Full2stamp            = true;
    Args.Convert2real          = true;
end

SizeData = size(Data);
SizePsf  = size(PSF);

IndexDim = 3;

% normalize PSF sum to unity
if Args.Norm
    PSF = PSF./sum(PSF,[1 2]);
end

StampSize = size(PSF);
   
% prep the PSF
%PSF = imUtil.psf.padShift(PSF, SizeData(1:2));
if ~all(StampSize==size(Data))
    PSF = imUtil.psf.stamp2full(PSF, SizeData(1:2), 'CenterPosition','corner');
end

if ~isempty(Args.AnnulusPre)
    PSF = imUtil.psf.mex.cosbellCorners(PSF, Args.AnnulusPre);
end

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
Data_f = fft2(Data);
R_f   = sum(WW_d .* Data_f.*conj(PSF_f),IndexDim)./PR_f;
R     = ifft2(R_f);
PR    = ifft2(PR_f);
if ~isempty(Args.AnnulusPost)
    PR = imUtil.psf.mex.cosbellCorners(PR, Args.AnnulusPost);
    
    if Args.ReCalcAfterAnnPost
        PR_f  = fft2(PR);
        R_f   = sum(WW_d .* Data_f.*conj(PSF_f),IndexDim)./PR_f;
        R     = ifft2(R_f);
    end 
end

if Args.Full2stamp
    % convert PSF size to stamp size

    % need to update with new function:
    PR = imUtil.psf.full2stampPsf(PR,StampSize, 'FullPosition','corner');
end

if Args.Convert2real
    R  = real(R);
    PR = real(PR);
end






