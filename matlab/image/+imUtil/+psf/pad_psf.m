function PadPSF = pad_psf(PSF, SizeImageIJ, PadVal)
    % Pad PSF such that it size will be equal to the image size.
    % Input  : - PSF stamp.
    %          - [I J] size of image.
    %          - Pad value. Default is 0.
    % Output : - Padded PSF to the image size.
    %            The PSF center is designed such that ifftshift will put
    %            the center of the PSF closest to I,J=1,1.
    % Author : Eran Ofek (Dec 2021)
    % Example: PadPSF = imUtil.psf.pad_psf(ones(3,3), [6 7])
    
    arguments
        PSF
        SizeImageIJ
        PadVal       = 0;
    end
    SizeImageIJ = SizeImageIJ(:).';
    
    SizePSF = size(PSF);
    
    RemIJ = (SizeImageIJ - SizePSF).*0.5;
    
    PreIJ  = ceil(RemIJ);
    PostIJ = floor(RemIJ);
    
    PadPSF = padarray(PSF, PreIJ, PadVal, 'pre');
    PadPSF = padarray(PadPSF, PostIJ, PadVal, 'post');
end