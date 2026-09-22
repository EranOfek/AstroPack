function PSF = shiftResampleRotate(PSF, Shift, Oversample, RotAngle, Args)
    % Resample a PSF stack / cell array downto Oversampling = 1, make it odd-sized, rotate and shift
    %     NB: after all the operations, some small negative values may appear at the borders
    %         one may need to employ imUtil.psf.suppressEdges
    % Input  : - a 3D PSF array with the source index in the 3rd dimension or a cell-array of 2D PSFs
    %          - an 2-column array of XY subpixel shifts, in the coordinate
    %            convention of imUtil.art.addSources / imUtil.art.createSourceCube:
    %            the FIRST column shifts along the first array dimension (rows) and
    %            the second column along the second one (columns). NB: this is the
    %            transpose of the [ShiftX, ShiftY] taken by imUtil.trans.shift_* ,
    %            where X is the column direction, hence the swaps below.
    %          - a vector of oversampling factors (e.g. Oversample = 3 means
    %            that the PSF grid is 3 times finer than that of the detector)
    %          - a vector of rotation angles [deg]
    %          * ...,key,val,...
    %         'Recenter' - true/false whether to shift the PSFs on the subpixel scale
    %         'RecenterMethod' - 'lanczos' (default), 'fft', or 'nearest'; usually 'nearest' goes with Oversampling > 1
    %         'ShiftOversampled' - if true (default) and the PSF is rescaled, the
    %                    subpixel shift is applied on the OVERSAMPLED grid, before
    %                    the rescaling, rather than on the detector grid after it.
    %                    The interpolation kernel then works on a well sampled
    %                    profile and does not ring: measured over 200 random
    %                    offsets of a sigma = 0.85 pix core, the flux left in
    %                    negative pixels drops from -0.18% to 0, and the centroid
    %                    error from 0.015 to 0.003 pix. It costs ~4x more in the
    %                    rescale+shift step, the stamps being Oversample^2 larger
    %                    at the time of the shift.
    %         'InterpMethod' - imresize kernel for the Oversample -> 1 rescaling,
    %                    forwarded to imUtil.psf.oversampling. Default is '' =
    %                    chosen there automatically ('box' when downsampling).
    %         'SuppressEdges' - taper width [pix], measured inward from the stamp
    %                    outer radius, of the cosine-bell edge suppression applied
    %                    after the subpixel shift: the shift kernels (lanczos, fft)
    %                    ring at the stamp borders, the more so the sharper the
    %                    stamp. The taper is flux-conserving -- the sum of each
    %                    stamp is restored after it -- so it redistributes the
    %                    counts inward but never removes any. 0 or [] disables it.
    %                    Default is [] (disabled): the taper only reaches the outermost
    %                    pixels, whereas the shift kernels ring immediately around the
    %                    core, and for a stamp that does not fully contain the PSF (e.g.
    %                    the ULTRASAT lab PSF at large field radii, where >30% of the
    %                    light lies in the tapered zone) it would pull that light inward.
    %         'Renorm'   - true/false whether to renormalize the stamps
    %         'ForceOdd' - false/true whether to make the even-sized stamps odd-sized
    % Output : - a stack or a cell array of resampled and shifted PSFs
    % Author : A.M. Krassilchtchikov (2024 May)
    % Example:  for i = 1:4; P(:,:,i) = imUtil.kernel2.gauss([2 2 0],[12 12]) + 1e-2*rand(12,12); end
    %           Shift = rand(4,2); Oversample = 3;
    %           imUtil.psf.shiftResampleRotate(P, Shift, Oversample, 'ForceOdd', true);
    arguments
        PSF
        Shift                  = [0 0];
        Oversample             = 0;
        RotAngle               = [];      % [deg] counterclockwise
        Args.Recenter logical  = true;
        Args.RecenterMethod    = 'lanczos';   % lanczos, fft, or nearest
        Args.ShiftOversampled logical = true; % shift before rescaling, on the oversampled grid
        Args.InterpMethod      = '';          % '' = auto ('box' downsampling, 'bilinear' upsampling)
        Args.SuppressEdges     = [];          % [pix] cosbell taper width, [] or 0 = disabled
        Args.Renorm   logical  = true;
        Args.ForceOdd logical  = false;
    end
    %
    if ~iscell(PSF)
        NumPsf = size(PSF,3);
    else
        NumPsf = numel(PSF);
    end
    %
    if ~isempty(RotAngle) && numel(RotAngle) < NumPsf
        RotAngle = repmat(RotAngle(1),1,NumPsf);
    end
    %
    if ~iscell(PSF) % if the PSFs are in an 3D array (hence they are of the same dimensions)
        % rotate
        if any( abs(RotAngle) > 1 & abs(RotAngle-360) > 1 )
            M = size(PSF,1);
            M1 = ceil(M * abs(cosd(RotAngle)) + M * abs(sind(RotAngle)) ); % the sizes of rotated images
            MaxSize = max(M1); RotPSF  = zeros(MaxSize,MaxSize,NumPsf);
            for Isrc = 1:1:NumPsf
                X1   = ceil(MaxSize/2-M1(Isrc)/2)+1; % the position of the lower left corner
                RotPSF(X1:X1+M1(Isrc)-1,X1:X1+M1(Isrc)-1,Isrc) = imrotate(PSF(:,:,Isrc), RotAngle(Isrc), 'bilinear', 'loose');
            end
            PSF = RotPSF;
        end
        % rescale, but do not normalize as of yet
        % NB: will work only with Oversample = scalar or a 2-element vector,
        % i.e. the same oversampling factors for all the PSFs
        Rescale = ~isempty(Oversample) && all(Oversample > 0);
        if Rescale && numel(Oversample) < 2
            Oversample(2) = Oversample(1);
        end
        % apply the subpixel shift while the PSF is still oversampled, where the
        % interpolation kernel does not ring (see the ShiftOversampled help above)
        ShiftOnFine = Rescale && Args.ShiftOversampled && ...
                      any(strcmpi(Args.RecenterMethod,{'fft','lanczos'}));
        if ShiftOnFine
            ShiftFine = zeros(size(Shift));
            if Args.Recenter
                ShiftFine = Shift.*Oversample;
            end
            % An even-sized rescaled stamp would be made odd further below by padding
            % it and shifting it back by half a DETECTOR pixel, which rings just as
            % the subpixel shift does. Pad the oversampled stamp instead, so that the
            % rescaled size comes out odd on its own, and fold the (at most half an
            % oversampled pixel) recentring into the shift performed here.
            if Args.ForceOdd
                [PSF, CompRC] = padOversampledToOdd(PSF, Oversample);
                ShiftFine     = ShiftFine + CompRC;
            end
            if any(ShiftFine~=0, 'all')
                PSF = subPixShift(PSF, ShiftFine, Args.RecenterMethod);
            end
        end
        if Rescale
            if Args.Recenter && strcmpi(Args.RecenterMethod,'nearest')
                % need to check the following block and, probably, make it faster and more compact
                ShiftRow = round(Shift(:,1) * Oversample(1)); % to the scale of the oversampled PSF
                ShiftCol = round(Shift(:,2) * Oversample(2));
                ShiftedPSF = zeros(size(PSF));
                for Ipsf = 1:NumPsf
                    if ShiftRow(Ipsf) > 0
                        ShiftedPSF(ShiftRow(Ipsf)+1:end, :, Ipsf) = PSF(1:end-ShiftRow(Ipsf), :, Ipsf);
                    else
                        ShiftedPSF(1:end+ShiftRow(Ipsf), :, Ipsf) = PSF(-ShiftRow(Ipsf)+1:end, :, Ipsf);
                    end
                    if ShiftCol(Ipsf) > 0
                        ShiftedPSF(:, ShiftCol(Ipsf)+1:end, Ipsf) = ShiftedPSF(:, 1:end-ShiftCol(Ipsf), Ipsf);
                        ShiftedPSF(:, 1:ShiftCol(Ipsf), Ipsf) = 0;
                    else
                        ShiftedPSF(:, 1:end+ShiftCol(Ipsf), Ipsf) = ShiftedPSF(:, -ShiftCol(Ipsf)+1:end, Ipsf);
                        ShiftedPSF(:, end+ShiftCol(Ipsf)+1:end, Ipsf) = 0;
                    end
                end
                PSF = ShiftedPSF;
            end
            PSF = imUtil.psf.oversampling(PSF, Oversample, 1,'ReNorm',false,...
                                          'InterpMethod',Args.InterpMethod);
        end
        % force odd size, independently per dimension (rows and columns may have
        % different parity for a non-square stamp)
        if Args.ForceOdd
            PadRow = mod( size(PSF,1), 2 ) == 0;
            PadCol = mod( size(PSF,2), 2 ) == 0;
            if PadRow || PadCol
                PSF = padarray(PSF, double([PadRow, PadCol]), 0, 'post');
                PSF = imUtil.trans.shift_fft(PSF, 0.5*PadCol, 0.5*PadRow);
            end
        end
        % shift on subpixel scale, unless it was already done on the oversampled grid
        if Args.Recenter && ~ShiftOnFine
            PSF = subPixShift(PSF, Shift, Args.RecenterMethod);
        end
        % suppress the border ringing left by the shift kernel (flux-conserving)
        PSF = taperEdges(PSF, Args.SuppressEdges);
        % normalize
        if Args.Renorm
            PSF = imUtil.psf.normPSF(PSF);
        end
    else % if the PSF stack is a cell array, we are to work one by one
        OverRC = Oversample(:).';
        if isscalar(OverRC)
            OverRC = [OverRC OverRC];
        end
        for Ipsf = 1:NumPsf
            % rotate
            if any( abs(RotAngle) > 1 & abs(RotAngle-360) > 1 )
                PSF{Ipsf} = imrotate(PSF{Ipsf}, RotAngle{Ipsf}, 'bilinear', 'loose');
            end
            % rescale
            RescaleCell = ~isempty(Oversample) && all(Oversample > 0);
            ShiftOnFine = RescaleCell && Args.ShiftOversampled && ...
                          any(strcmpi(Args.RecenterMethod,{'fft','lanczos'}));
            if ShiftOnFine
                ShiftFine = [0 0];
                if Args.Recenter
                    ShiftFine = shiftRow(Shift,Ipsf).*OverRC;
                end
                if Args.ForceOdd
                    [PSF{Ipsf}, CompRC] = padOversampledToOdd(PSF{Ipsf}, OverRC);
                    ShiftFine           = ShiftFine + CompRC;
                end
                if any(ShiftFine~=0)
                    PSF{Ipsf} = subPixShift(PSF{Ipsf}, ShiftFine, Args.RecenterMethod);
                end
            end
            if RescaleCell
                PSF{Ipsf} = imUtil.psf.oversampling(PSF{Ipsf}, Oversample, 1,'ReNorm',false,...
                                          'InterpMethod',Args.InterpMethod);
            end
            % force odd size, independently per dimension (rows and columns may have
            % different parity for a non-square stamp)
            if Args.ForceOdd
                PadRow = mod( size(PSF{Ipsf},1), 2 ) == 0;
                PadCol = mod( size(PSF{Ipsf},2), 2 ) == 0;
                if PadRow || PadCol
                    PSF{Ipsf} = padarray(PSF{Ipsf}, double([PadRow, PadCol]), 0, 'post');
                    PSF{Ipsf} = imUtil.trans.shift_fft(PSF{Ipsf}, 0.5*PadCol, 0.5*PadRow);
                end
            end
            % shift on subpixel scale, unless it was already done on the oversampled grid
            if Args.Recenter && ~ShiftOnFine
                PSF{Ipsf} = subPixShift(PSF{Ipsf}, shiftRow(Shift,Ipsf), Args.RecenterMethod);
            end
            % suppress the border ringing left by the shift kernel (flux-conserving)
            PSF{Ipsf} = taperEdges(PSF{Ipsf}, Args.SuppressEdges);
            % normalize
            if Args.Renorm
                PSF{Ipsf} = imUtil.psf.normPSF(PSF{Ipsf});
            end
        end
    end
end

%%%
%%% internal functions
%%%

function PSF = subPixShift(PSF, Shift, Method)
    % Apply a subpixel shift to a PSF stamp / stack
    %     NB: imUtil.trans.shift_* take [ShiftX, ShiftY] with X along the columns,
    %     whereas the first column of Shift is the first array dimension, hence the swap
    % Input  : - A PSF stamp or a stack of them.
    %          - A 2-column array of [dim1, dim2] shifts, in pixels of the current grid.
    %          - 'fft', 'lanczos', or 'nearest' (which is handled before the rescaling).
    % Output : - The shifted stamp / stack.
    % Author : A.M. Krassilchtchikov (Sep 2026)
    if strcmpi(Method,'fft')
        PSF = imUtil.trans.shift_fft(PSF, Shift(:,2), Shift(:,1));
    elseif strcmpi(Method,'lanczos')
        PSF = imUtil.trans.shift_lanczos(PSF, Shift(:,[2 1]));
    end
end

function [PSF, CompRC] = padOversampledToOdd(PSF, Oversample)
    % Pad an oversampled stamp so that rescaling it to Oversampling = 1 gives an odd size
    %     Doing it here, on the oversampled grid, replaces the pad-and-shift-back-by-
    %     half-a-pixel that would otherwise be performed on the detector grid, where
    %     the interpolation kernel rings on an undersampled core.
    % Input  : - An oversampled PSF stamp or stack.
    %          - The oversampling factors [rows, columns].
    % Output : - The padded stamp / stack.
    %          - The [rows, columns] recentring shift, in oversampled pixels, that the
    %            padding calls for: 0 when it came out symmetric, 0.5 otherwise.
    % Author : A.M. Krassilchtchikov (Sep 2026)
    SizeRC  = size(PSF, [1 2]);
    PadPre  = [0 0];
    PadPost = [0 0];
    CompRC  = [0 0];
    for Idim = 1:2
        Fac = Oversample(Idim);
        if abs(Fac - round(Fac)) < 1e-10 && Fac >= 1
            Fac  = round(Fac);
            Nout = ceil(SizeRC(Idim)./Fac);
            if mod(Nout, 2) == 0
                Nout = Nout + 1;          % the rescaled stamp is to be odd-sized
            end
            Pad           = Fac.*Nout - SizeRC(Idim);
            PadPre(Idim)  = floor(Pad./2);
            PadPost(Idim) = Pad - PadPre(Idim);
            CompRC(Idim)  = Pad./2 - PadPre(Idim);
        end
    end
    if any([PadPre PadPost] > 0)
        PSF = padarray(PSF, PadPre,  0, 'pre');
        PSF = padarray(PSF, PadPost, 0, 'post');
    end
end

function ShiftXY = shiftRow(Shift, Ipsf)
    % Pick the shift of one PSF out of the input array, broadcasting a single row
    % Input  : - A 2-column array of shifts, or a single [dim1, dim2] pair.
    %          - The PSF index.
    % Output : - The 1x2 shift for that PSF.
    % Author : A.M. Krassilchtchikov (Sep 2026)
    if numel(Shift) == 2
        ShiftXY = Shift(1,:);
    else
        ShiftXY = Shift(Ipsf,:);
    end
end

function PSF = taperEdges(PSF, Width)
    % Apply a flux-conserving cosine-bell taper to the borders of a PSF stamp / cube
    %     The taper kills the ringing the subpixel shift kernels leave at the stamp
    %     borders. The sum of every stamp is restored afterwards, so the counts are
    %     redistributed inward and the injected flux is unchanged.
    % Input  : - a 2D PSF stamp or a 3D stack (stamp index in the 3rd dimension)
    %          - the taper width [pix] inward from the stamp outer radius,
    %            0 or [] to return the stamp unchanged
    % Output : - the tapered stamp / stack, with the original per-stamp sum
    % Author : A.M. Krassilchtchikov (Sep 2026)
    if isempty(Width) || Width <= 0
        return
    end
    SizeXY = [size(PSF,2) size(PSF,1)];
    FunPars = imUtil.psf.suppressEdgesPars(Width, SizeXY);
    if FunPars(1) < 1   % the stamp is too small to be tapered over this width
        return
    end
    Sum0 = sum(PSF, [1 2]);
    try
        PSF = imUtil.psf.mex.cosbellTaper(PSF, FunPars);
    catch
        % the mex is unavailable or refused the input: use the m-code equivalent
        % (NB: the two centre even-sized stamps differently, see imUtil.psf.suppressEdgesPars)
        PSF = imUtil.psf.suppressEdges(PSF, 'FunPars', FunPars, 'Norm', false);
    end
    Sum1 = sum(PSF, [1 2]);
    Keep = Sum1 ~= 0;
    PSF(:,:,Keep) = PSF(:,:,Keep) .* (Sum0(Keep)./Sum1(Keep));
end
