function StampCube = full2stamp(FullCube, StampSizeIJ, Args)
    % full2stamp Extract a centered PSF stamp from a full image/cube.
    %   Extract a stamp of size StampSizeIJ = [Y, X] from a full image/cube.
    %   The PSF in the full image may be either centered or in FFT-corner
    %   ordering. In the output stamp, the PSF is always centered.
    %
    % Input  : - (FullCube) Full image [Y,X] or cube [Y,X,N].
    %          - (StampSizeIJ) Requested output stamp size [Y, X].
    %          * ...,key,val,...
    %            'FullPosition' - Position of the PSF in the full image:
    %                   "center" : PSF is centered in FullCube.
    %                   "corner" : PSF is in FFT order (cornered) in FullCube.
    %                   Default is "center".
    %           'Supress' - A logical indicating if o call
    %                   imUtil.psf.suppressEdges to supress PSF wings.
    %                   Default is true.
    %           'SupressFunPars' - The FunPars argument to pass to the
    %                   imUtil.psf.suppressEdges function.
    %                   Default is [5 7].
    %           'suppressEdgesArgs' - A cell array of additional arguments
    %                   to pass to: imUtil.psf.suppressEdges
    %                   Default is {}.
    %
    % Output : - (StampCube) Output centered stamp or cube of stamps.
    %
    % Notes:
    %   - Supports odd/even size combinations.
    %   - If a sub-pixel correction is needed, uses:
    %       imUtil.trans.mex.shift_lanczos3(Image, DX, DY)
    %   - Embedding/cropping is vectorized over dim 3.
    %
    % Author : ChatGPT + Eran Ofek (Apr 2026)
    % Example:
    %   Stamp = full2stamp(Full, [31 31]);
    %   Stamp = full2stamp(FullFFT, [31 31], FullPosition="corner");

    arguments
        FullCube 
        StampSizeIJ
        Args.FullPosition      = "center";
        Args.Supress           = true;
        Args.SupressFunPars    = [5 7];
        Args.suppressEdgesArgs = {};
    end

    %-----------------------------
    % Validate size
    %-----------------------------
    Nd = ndims(FullCube);
    if Nd~=2 && Nd~=3
        error('FullCube must be 2-D or 3-D.');
    end

    SizeFull = size(FullCube);
    NfullI   = SizeFull(1);
    NfullJ   = SizeFull(2);

    if Nd==2
        Nim = 1;
    else
        Nim = SizeFull(3);
    end

    NstampI = StampSizeIJ(1);
    NstampJ = StampSizeIJ(2);

    %-----------------------------
    % Convert to centered layout if needed
    %-----------------------------
    switch Args.FullPosition
        case "center"
            FullCentered = FullCube;

        case "corner"
            if Nd==2
                FullCentered = fftshift(FullCube);
            else
                FullCentered = fftshift(FullCube, 1);
                FullCentered = fftshift(FullCentered, 2);
            end
    end

    %-----------------------------
    % Compute required shift from full-center to stamp-center
    %-----------------------------
    CenterFullI  = (NfullI  + 1)./2;
    CenterFullJ  = (NfullJ  + 1)./2;
    CenterStampI = (NstampI + 1)./2;
    CenterStampJ = (NstampJ + 1)./2;

    % Shift needed so that after cropping, the PSF stays centered in stamp
    ShiftI = CenterStampI - CenterFullI;
    ShiftJ = CenterStampJ - CenterFullJ;

    IntShiftI  = floor(ShiftI);
    IntShiftJ  = floor(ShiftJ);
    FracShiftI = ShiftI - IntShiftI;
    FracShiftJ = ShiftJ - IntShiftJ;

    %-----------------------------
    % Apply fractional shift if needed
    %-----------------------------
    FullShifted = FullCentered;

    if FracShiftI ~= 0 || FracShiftJ ~= 0
        if Nd==2
            FullShifted = imUtil.trans.mex.shift_lanczos3(FullCentered, FracShiftJ, FracShiftI);
        else
            % If shift_lanczos3 supports cubes directly, replace this loop
            FullShifted = zeros(size(FullCentered), 'like', FullCentered);
            for Iim = 1:Nim
                FullShifted(:,:,Iim) = imUtil.trans.mex.shift_lanczos3( ...
                    FullCentered(:,:,Iim), FracShiftJ, FracShiftI);
            end
        end
    end

    %-----------------------------
    % Integer crop
    %-----------------------------
    StartI = 1 - IntShiftI;
    StartJ = 1 - IntShiftJ;
    EndI   = StartI + NstampI - 1;
    EndJ   = StartJ + NstampJ - 1;

    if Nd==2
        StampCube = zeros(NstampI, NstampJ, 'like', FullCube);
    else
        StampCube = zeros(NstampI, NstampJ, Nim, 'like', FullCube);
    end

    OverlapI1 = max(1, StartI);
    OverlapI2 = min(NfullI, EndI);
    OverlapJ1 = max(1, StartJ);
    OverlapJ2 = min(NfullJ, EndJ);

    if OverlapI1 > OverlapI2 || OverlapJ1 > OverlapJ2
        return;
    end

    StampI1 = OverlapI1 - StartI + 1;
    StampI2 = OverlapI2 - StartI + 1;
    StampJ1 = OverlapJ1 - StartJ + 1;
    StampJ2 = OverlapJ2 - StartJ + 1;

    if Nd==2
        StampCube(StampI1:StampI2, StampJ1:StampJ2) = ...
            FullShifted(OverlapI1:OverlapI2, OverlapJ1:OverlapJ2);
    else
        StampCube(StampI1:StampI2, StampJ1:StampJ2, :) = ...
            FullShifted(OverlapI1:OverlapI2, OverlapJ1:OverlapJ2, :);
    end

    if Args.Supress
        StampCube = imUtil.psf.suppressEdges(StampCube, 'FunPars',Args.SupressFunPars, Args.suppressEdgesArgs{:});
    end

end
