function PSF = shiftResampleRotate(PSF, Shift, Oversample, RotAngle, Args)
    % Resample a PSF stack / cell array downto Oversampling = 1, make it odd-sized, rotate and shift
    %     NB: after all the operations, some small negative values may appear at the borders
    %         one may need to employ imUtil.psf.suppressEdges
    % Input  : - a 3D PSF array with the source index in the 3rd dimension or a cell-array of 2D PSFs
    %          - an 2-column array of XY subpixel shifts
    %          - a vector of oversampling factors (e.g. Oversample = 3 means
    %            that the PSF grid is 3 times finer than that of the detector)
    %          - a vector of rotation angles [deg]
    %          * ...,key,val,...
    %         'Recenter' - true/false whether to shift the PSFs on the subpixel scale
    %         'RecenterMethod' - 'lanczos' (default), 'fft', or 'nearest'; usually 'nearest' goes with Oversampling > 1
    %         'InterpMethod' - imresize kernel for the Oversample -> 1 rescaling.
    %                    Default is '' = choose automatically: 'box' when downsampling
    %                    (Oversample > 1), which integrates the flux over the detector
    %                    pixel area and thus does not broaden the PSF, and 'bilinear'
    %                    when upsampling, where 'box' would only replicate pixels.
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
        if ~isempty(Oversample) && all(Oversample > 0)
            if numel(Oversample) < 2
                Oversample(2) = Oversample(1);
            end
            if Args.Recenter && strcmpi(Args.RecenterMethod,'nearest')
                % need to check the following block and, probably, make it faster and more compact
                ShiftX = round(Shift(:,1) * Oversample(1)); % to the scale of the oversampled PSF
                ShiftY = round(Shift(:,2) * Oversample(2));
                ShiftedPSF = zeros(size(PSF));
                for Ipsf = 1:NumPsf
                    if ShiftX(Ipsf) > 0
                        ShiftedPSF(:, ShiftX(Ipsf)+1:end, Ipsf) = PSF(:, 1:end-ShiftX(Ipsf), Ipsf);
                    else
                        ShiftedPSF(:, 1:end+ShiftX(Ipsf), Ipsf) = PSF(:, -ShiftX(Ipsf)+1:end, Ipsf);
                    end
                    if ShiftY(Ipsf) > 0
                        ShiftedPSF(ShiftY(Ipsf)+1:end, :, Ipsf) = ShiftedPSF(1:end-ShiftY(Ipsf), :, Ipsf);
                        ShiftedPSF(1:ShiftY(Ipsf), :, Ipsf) = 0;
                    else
                        ShiftedPSF(1:end+ShiftY(Ipsf), :, Ipsf) = ShiftedPSF(-ShiftY(Ipsf)+1:end, :, Ipsf);
                        ShiftedPSF(end+ShiftY(Ipsf)+1:end, :, Ipsf) = 0;
                    end
                end
                PSF = ShiftedPSF;
            end
            PSF = imUtil.psf.oversampling(PSF, Oversample, 1,'ReNorm',false,...
                                          'InterpMethod',resampleKernel(Args.InterpMethod, Oversample));
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
        % shift on subpixel scale
        if Args.Recenter
            if strcmpi(Args.RecenterMethod,'fft')
                PSF = imUtil.trans.shift_fft(PSF, Shift(:,1), Shift(:,2));
            elseif strcmpi(Args.RecenterMethod,'lanczos')
                PSF = imUtil.trans.shift_lanczos(PSF, Shift);
            end
        end
        % suppress the border ringing left by the shift kernel (flux-conserving)
        PSF = taperEdges(PSF, Args.SuppressEdges);
        % normalize
        if Args.Renorm
            PSF = imUtil.psf.normPSF(PSF);
        end
    else % if the PSF stack is a cell array, we are to work one by one
        for Ipsf = 1:NumPsf
            % rotate
            if any( abs(RotAngle) > 1 & abs(RotAngle-360) > 1 )
                PSF{Ipsf} = imrotate(PSF{Ipsf}, RotAngle{Ipsf}, 'bilinear', 'loose');
            end
            % rescale
            if all(Oversample > 0)
                PSF{Ipsf} = imUtil.psf.oversampling(PSF{Ipsf}, Oversample, 1,'ReNorm',false,...
                                          'InterpMethod',resampleKernel(Args.InterpMethod, Oversample));
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
            % shift on subpixel scale
            if Args.Recenter
                if numel(Shift) == 2
                    ShiftXY = Shift(1,:);
                else
                    ShiftXY = Shift(Ipsf,:);
                end
                if strcmpi(Args.RecenterMethod,'fft')
                    PSF{Ipsf} = imUtil.trans.shift_fft(PSF{Ipsf}, ShiftXY(1), ShiftXY(2));
                elseif strcmpi(Args.RecenterMethod,'lanczos')
                    PSF{Ipsf} = imUtil.trans.shift_lanczos(PSF{Ipsf}, ShiftXY);
                end
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

function Method = resampleKernel(Method, Oversample)
    % Pick the imresize kernel for an Oversample -> 1 rescaling, unless forced by the user
    % Input  : - a user-requested kernel name, or '' to choose automatically
    %          - the oversampling factor(s) of the input PSF grid
    % Output : - the imresize kernel name
    % Author : A.M. Krassilchtchikov (Sep 2026)
    if isempty(Method)
        if all(Oversample > 1)
            Method = 'box';      % a detector pixel integrates the flux over its area
        else
            Method = 'bilinear'; % upsampling: 'box' would merely replicate pixels
        end
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
