function Full = stamp2full(PSF, SizeIJ, Args)
    % stamp2full Pad an PSF stamp or cube into a larger image/cube and pad by zeros
    %
    % Description:
    %   Insert a PSF stamp into a larger image of size SizeIJ = [Y, X].
    %   PSF may be either a 2-D image or a 3-D cube, where the image index is
    %   always in the 3rd dimension.
    %   The function keep the PSF center, in the center (or corner) of the image 
    %   and if needed shift the PSF (1/2 pixel shift).
    %
    % Input : - (PSF)  2-D stamp [Ypsf, Xpsf] or 3-D cube [Ypsf, Xpsf, N].
    %         - (SizeIJ) Output image size [Y, X].
    %         * ...,key,val,...
    %           'CenterPosition' - Placement mode:
    %               "center" : PSF centered in the output image.
    %               "corner" : FFT-style placement. PSF center is shifted to the
    %                  origin with periodic wrapping.
    %               Default is "center".
    %
    % Output : (Full) Output image/cube.
    %
    % Notes:
    %   - Fractional shifts are handled using:
    %       imUtil.trans.mex.shift_lanczos3(PSF, DX, DY)
    %   - Embedding is vectorized over the 3rd dimension.
    %   - If shift_lanczos3 does not support cubes, a loop over dim-3 is used
    %     only for that step.
    % Author : ChatGPT + Eran Ofek (Apr 2026)
    % Example: K=imUtil.kernel2.gauss(2.*ones(100,1));
    %          F=stamp2full(K,[1716 1716],'CenterPosition','center');
    %          M=imUtil.image.moment2(F(:,:,2),16,16); M


    arguments
        PSF
        SizeIJ 
        Args.CenterPosition = 'center';
    end

    %-----------------------------
    % Validate dimensions
    %-----------------------------
    Nd = ndims(PSF);
    if Nd~=2 && Nd~=3
        error('PSF must be 2-D or 3-D.');
    end

    SizePSF = size(PSF);
    NpsfI   = SizePSF(1);
    NpsfJ   = SizePSF(2);

    if Nd==2
        Nim = 1;
    else
        Nim = SizePSF(3);
    end

    NfullI = SizeIJ(1);
    NfullJ = SizeIJ(2);

    %-----------------------------
    % Geometric centers
    %-----------------------------
    CenterPSF_I = (NpsfI + 1)./2;
    CenterPSF_J = (NpsfJ + 1)./2;

    TargetCenterI = (NfullI + 1)./2;
    TargetCenterJ = (NfullJ + 1)./2;

    ShiftToCenterI = TargetCenterI - CenterPSF_I;
    ShiftToCenterJ = TargetCenterJ - CenterPSF_J;

    % Integer + fractional parts
    IntShiftI  = floor(ShiftToCenterI);
    IntShiftJ  = floor(ShiftToCenterJ);
    FracShiftI = ShiftToCenterI - IntShiftI;
    FracShiftJ = ShiftToCenterJ - IntShiftJ;

    %-----------------------------
    % Fractional shift
    %-----------------------------
    PSF1 = PSF;

    if FracShiftI ~= 0 || FracShiftJ ~= 0
        if Nd==2
            PSF1 = imUtil.trans.mex.shift_lanczos3(PSF1, FracShiftJ, FracShiftI);
        else
            % If shift_lanczos3 supports cubes, replace this loop by one call:
            % PSF1 = imUtil.trans.mex.shift_lanczos3(PSF1, FracShiftJ, FracShiftI);
            PSF1 = zeros(size(PSF), 'like', PSF);
            for Iim = 1:Nim
                PSF1(:,:,Iim) = imUtil.trans.mex.shift_lanczos3(PSF(:,:,Iim), FracShiftJ, FracShiftI);
            end
        end
    end

    %-----------------------------
    % Prepare centered full image/cube
    %-----------------------------
    if Nd==2
        Full0 = zeros(NfullI, NfullJ, 'like', PSF);
    else
        Full0 = zeros(NfullI, NfullJ, Nim, 'like', PSF);
    end

    StartI = 1 + IntShiftI;
    StartJ = 1 + IntShiftJ;
    EndI   = StartI + NpsfI - 1;
    EndJ   = StartJ + NpsfJ - 1;

    OverlapI1 = max(1, StartI);
    OverlapI2 = min(NfullI, EndI);
    OverlapJ1 = max(1, StartJ);
    OverlapJ2 = min(NfullJ, EndJ);

    if OverlapI1 <= OverlapI2 && OverlapJ1 <= OverlapJ2
        PSFI1 = OverlapI1 - StartI + 1;
        PSFI2 = OverlapI2 - StartI + 1;
        PSFJ1 = OverlapJ1 - StartJ + 1;
        PSFJ2 = OverlapJ2 - StartJ + 1;

        if Nd==2
            Full0(OverlapI1:OverlapI2, OverlapJ1:OverlapJ2) = ...
                PSF1(PSFI1:PSFI2, PSFJ1:PSFJ2);
        else
            Full0(OverlapI1:OverlapI2, OverlapJ1:OverlapJ2, :) = ...
                PSF1(PSFI1:PSFI2, PSFJ1:PSFJ2, :);
        end
    end

    %-----------------------------
    % Final output mode
    %-----------------------------
    switch Args.CenterPosition
        case "center"
            Full = Full0;

        case "corner"
            if Nd==2
                Full = ifftshift(Full0);
            else
                Full = ifftshift(Full0, 1);
                Full = ifftshift(Full, 2);
            end
    end

end