function PadPSF = pad_psf(PSF, SizeImageIJ, PadVal)
    % Pad PSF such that it size will be equal to the image size.
    %   If the requested size is smaller than the PSF along an axis, the
    %   PSF is cropped along that axis instead, keeping the same layout
    %   (center at floor(N/2)+1, i.e. the fftshift position).
    %   For general case see: imUtil.psf.stamp2full
    % Input  : - PSF stamp.
    %          - [I J] size of image.
    %          - Pad value. Default is 0.
    % Output : - Padded PSF to the image size.
    %            The PSF center is designed such that ifftshift will put
    %            the center of the PSF closest to I,J=1,1.
    % Author : Eran Ofek (Dec 2021)
    % Example: PadPSF = imUtil.psf.pad_psf(ones(3,3), [6 7])
    %          PadPSF = imUtil.psf.pad_psf(ones(25,25), [24 24])  % crop
    
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
    
    % negative pre/post padding is a crop (same layout as the padding)
    PadPSF = PSF(1-min(PreIJ(1),0):end+min(PostIJ(1),0), 1-min(PreIJ(2),0):end+min(PostIJ(2),0), :);
    PadPSF = padarray(PadPSF, max(PreIJ,0),  PadVal, 'pre');
    PadPSF = padarray(PadPSF, max(PostIJ,0), PadVal, 'post');
end