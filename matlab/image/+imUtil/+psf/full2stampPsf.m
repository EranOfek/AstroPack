function StampCube = full2stampPsf(FullCube, StampSizeIJ, Args)
    % full2stampPsf Extract a centered PSF stamp from a full image/cube.
    %   Extract a stamp of size StampSizeIJ = [Y, X] from a full image or
    %   cube of images. The PSF in the full image may be either centered or
    %   in FFT-corner (fftshift) ordering. In the output stamp the PSF is
    %   always centered.
    %
    %   This is a corrected re-implementation of imUtil.psf.full2stamp:
    %   1. The center is taken as the integer center *pixel*
    %      floor((N+1)/2), consistent with the even-size PSF convention used
    %      by imUtil.kernel2.* and by fftshift/ifftshift. The old
    %      (N+1)/2 convention is a half-integer for even N, which
    %      mis-centered even-sized inputs by 0.5 pixel and, for a symmetric
    %      PSF, rounded the peak pixel by a full pixel when the output size
    %      parity differed from the input (e.g. even -> odd).
    %   2. Because both centers are integers, the extraction is a pure
    %      integer crop/embed - there is no fractional Lanczos resample, so
    %      there is no flux smearing and no edge ringing (the old code
    %      shifted the whole array before cropping).
    %   3. Output class always follows the input (integer copy only).
    %   4. A requested stamp that does not intersect the input is an error,
    %      not a silent all-zero return.
    %   5. Works on a cube [Y,X,N], extracting one centered stamp per slice.
    %
    % Input  : - (FullCube) Full image [Y,X] or cube [Y,X,N].
    %          - (StampSizeIJ) Requested output stamp size [Y, X]. A scalar
    %            is interpreted as a square stamp [S, S].
    %          * ...,key,val,...
    %            'FullPosition' - Position of the PSF in the full image:
    %                   "center" : PSF is centered in FullCube (default).
    %                   "corner" : PSF is in FFT (fftshift) order.
    %            'Supress' - A logical indicating if to call
    %                   imUtil.psf.suppressEdges to suppress the PSF wings.
    %                   Default is true.
    %            'SupressFunPars' - The FunPars argument passed to
    %                   imUtil.psf.suppressEdges. Default is [5 7].
    %            'suppressEdgesArgs' - A cell array of additional arguments
    %                   to pass to imUtil.psf.suppressEdges. Default is {}.
    %            'Norm' - A logical indicating if to normalize each output
    %                   stamp to unit sum. If 'Supress' is true the
    %                   normalization is done by suppressEdges; this flag
    %                   forces normalization also when 'Supress' is false.
    %                   Default is false.
    %
    % Output : - (StampCube) Output centered stamp [Y,X] or cube [Y,X,N].
    %
    % Author : Eran Ofek + Claude (Aug 2026) 
    % Example:
    %   S = imUtil.psf.full2stampPsf(imUtil.kernel2.gauss(2,[20 20]), [15 15]);
    %   S = imUtil.psf.full2stampPsf(FullFFT, 15, 'FullPosition',"corner");
    %   C = imUtil.psf.full2stampPsf(rand(20,20,50), [15 15]);  % cube

    arguments
        FullCube
        StampSizeIJ
        Args.FullPosition        = "center";
        Args.Supress logical     = true;
        Args.SupressFunPars      = [5 7];
        Args.suppressEdgesArgs cell = {};
        Args.Norm logical        = false;
    end

    % --- validate FullPosition (fixes the missing 'otherwise' in full2stamp) ---
    Args.FullPosition = string(Args.FullPosition);
    if ~ismember(Args.FullPosition, ["center","corner"])
        error('imUtil:psf:full2stampPsf:BadFullPosition', ...
              'FullPosition must be "center" or "corner".');
    end

    % --- validate/normalize sizes ---
    Nd = ndims(FullCube);
    if Nd~=2 && Nd~=3
        error('imUtil:psf:full2stampPsf:BadInput','FullCube must be 2-D or 3-D.');
    end

    if isscalar(StampSizeIJ)
        StampSizeIJ = [StampSizeIJ, StampSizeIJ];
    end
    NstampI = StampSizeIJ(1);
    NstampJ = StampSizeIJ(2);

    SizeFull = size(FullCube);
    NfullI   = SizeFull(1);
    NfullJ   = SizeFull(2);
    if Nd==2
        Nim = 1;
    else
        Nim = SizeFull(3);
    end

    % --- convert to centered layout if needed ---
    switch Args.FullPosition
        case "center"
            FullCentered = FullCube;
        case "corner"
            FullCentered = fftshift(fftshift(FullCube, 1), 2);
    end

    % --- integer center pixel: floor(N/2)+1 (the FFT / fftshift DC index) ---
    % This is exactly where fftshift places the DC/center: N/2+1 for even N
    % and (N+1)/2 for odd N. It matches FFT-order PSFs (e.g. PR=ifft2(...)
    % in imProc.stack.coadd_Proper), whose even-sized center sits at N/2+1 -
    % NOT at N/2. (Using floor((N+1)/2)=N/2 for even shifts the stamp by 1.)
    CenterFullI  = floor(NfullI ./2) + 1;
    CenterFullJ  = floor(NfullJ ./2) + 1;
    CenterStampI = floor(NstampI./2) + 1;
    CenterStampJ = floor(NstampJ./2) + 1;

    % Map: Full index for a given stamp index s is  s + Off
    OffI = CenterFullI - CenterStampI;
    OffJ = CenterFullJ - CenterStampJ;

    % Source range in the full image spanned by the stamp
    SrcI1 = 1       + OffI;   SrcI2 = NstampI + OffI;
    SrcJ1 = 1       + OffJ;   SrcJ2 = NstampJ + OffJ;

    % Overlap of that range with the actual full image
    OverlapI1 = max(1, SrcI1);   OverlapI2 = min(NfullI, SrcI2);
    OverlapJ1 = max(1, SrcJ1);   OverlapJ2 = min(NfullJ, SrcJ2);

    if OverlapI1 > OverlapI2 || OverlapJ1 > OverlapJ2
        % Cannot happen for a centered extraction with positive sizes, but
        % guard explicitly instead of silently returning zeros.
        error('imUtil:psf:full2stampPsf:NoOverlap', ...
              'Requested stamp does not intersect the input image.');
    end

    % Corresponding destination indices in the stamp (stamp = full - Off)
    DestI1 = OverlapI1 - OffI;   DestI2 = OverlapI2 - OffI;
    DestJ1 = OverlapJ1 - OffJ;   DestJ2 = OverlapJ2 - OffJ;

    % --- integer crop / zero-pad embed (class follows the input) ---
    if Nd==2
        StampCube = zeros(NstampI, NstampJ, 'like', FullCube);
        StampCube(DestI1:DestI2, DestJ1:DestJ2) = ...
            FullCentered(OverlapI1:OverlapI2, OverlapJ1:OverlapJ2);
    else
        StampCube = zeros(NstampI, NstampJ, Nim, 'like', FullCube);
        StampCube(DestI1:DestI2, DestJ1:DestJ2, :) = ...
            FullCentered(OverlapI1:OverlapI2, OverlapJ1:OverlapJ2, :);
    end

    % --- optional edge suppression (normalizes by default) ---
    if Args.Supress
        StampCube = imUtil.psf.suppressEdges(StampCube, 'FunPars',Args.SupressFunPars, ...
                                             Args.suppressEdgesArgs{:});
    elseif Args.Norm
        StampCube = StampCube ./ sum(StampCube, [1 2]);
    end

end
