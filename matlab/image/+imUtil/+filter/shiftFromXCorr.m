function [XCorr, Shift] = shiftFromXCorr(ImRef, ImNew, Args)
% Find the Sub-pixel translation between two images from their cross-correlation.
%
%   [XCorr, Shift] = shiftFromXCorr(ImRef, ImNew, ...)
%
%   Estimates the relative shift (pure translation) between two same-size
%   images using FFT cross-correlation, then refines it to sub-pixel
%   accuracy by sinc (Dirichlet) interpolation of the correlation surface
%   in a small neighbourhood of its peak. The sinc interpolation is
%   performed with a localized upsampled inverse DFT evaluated by direct
%   matrix multiplication (Guizar-Sicairos, Thurman & Fienup 2008), which
%   is mathematically exact periodic-sinc interpolation of ifft2 -- no
%   parabola/Gaussian peak fit is used.
%
% Input  : - (ImRef) Reference image (2-D numeric, real).
%          - (ImNew) New/moving image, same size as ImRef.
%          * ...Key,Val... :
%            'Method'           - Only 'fft' is implemented. Default 'fft'.
%            'SubPixel'         - Refine to sub-pixel via sinc interpolation.
%                          Default true.
%            'UpSampling'       - Sinc upsampling factor (fine-grid resolution is
%                          1/UpSampling pixel). Default 100.
%            'SubtractMean'     - Subtract each image mean before correlating
%                          (removes the DC term -> sharper, unbiased peak).
%                          Default true.
%            'Window'           - Apodization window: 'none'|'hann'|'hamming'|
%                          'tukey'. Suppresses FFT edge/wrap artifacts.
%                          Default 'none'.
%            'WindowParam'      - Tukey cosine-fraction alpha in [0,1]. Default 0.5.
%            'MaxShift'         - Restrict the integer-peak search to +/- this many
%                          pixels around zero lag. Scalar or [Ry Rx].
%                          Use it when the shift is known to be small to
%                          reject spurious far peaks. Default [] (no limit).
%            'AbsCorr'          - Locate the peak on abs(XCorr) instead of
%                          real(XCorr) (use if correlation may be negative).
%                          Default false.
%            'ReplaceNonFinite' - Replace NaN/Inf pixels by the finite image mean
%                          before correlating. Default false (a warning is
%                          issued instead, since FFT propagates non-finites).
%
% Output : - (XCorr) Real cross-correlation surface, fftshifted so that zero lag is
%            at the centre pixel (row floor(Ny/2)+1, col floor(Nx/2)+1).
%          - (Shift) Structure with fields:
%              .Shift        [DY DX]  sub-pixel shift, [row col].
%              .ShiftXY      [DX DY]  same shift in [x y] order.
%              .IntShift     [DY DX]  integer (coarse) shift.
%              .PeakCorr     value of XCorr at the integer peak.
%              .NormPeakCorr peak normalised to ~[-1,1] (circular NCC).
%              .SubPeakCorr  interpolated correlation value at sub-pixel peak.
%              .PeakInd      [row col] index of the integer peak in XCorr.
%              .Method, .UpSampling, .SubPixel, .ImSize, .Convention.
%
% CONVENTION:
%   Shift is the position of ImNew RELATIVE TO ImRef: a feature located at
%   (row r, col c) in ImRef appears near (r+DY, c+DX) in ImNew. Equivalently
%   ImNew ~= ImRef shifted by Shift. To register ImNew back onto ImRef,
%   translate ImNew by -Shift. Quick self-test:
%       ImNew = circshift(ImRef,[3 -2]);
%       [~,S]  = shiftFromXCorr(ImRef,ImNew);   % S.Shift ~= [3 -2]
%
% Author: Claude + Eran Ofek (Jun 2026)
% Example:
%   [XC, S] = shiftFromXCorr(A, B, 'UpSampling', 200, 'Window', 'tukey');
%

    arguments
        ImRef                                   {mustBeNumeric, mustBeNonempty}
        ImNew                                   {mustBeNumeric, mustBeNonempty}
        Args.Method                             {mustBeMember(Args.Method,{'fft'})} = 'fft'
        Args.SubPixel         (1,1) logical     = true
        Args.UpSampling       (1,1) double      {mustBePositive} = 100
        Args.SubtractMean     (1,1) logical     = true
        Args.Window                             {mustBeMember(Args.Window,{'none','hann','hamming','tukey'})} = 'none'
        Args.WindowParam      (1,1) double      {mustBeInRange(Args.WindowParam,0,1)} = 0.5
        Args.MaxShift                           {mustBeNumeric} = []
        Args.AbsCorr          (1,1) logical     = false
        Args.ReplaceNonFinite (1,1) logical     = false
    end

    % --- checks ----------------------------------------------------------
    if ~isequal(size(ImRef), size(ImNew))
        error('shiftFromXCorr:sizeMismatch', ...
              'ImRef and ImNew must have the same size.');
    end
    ImRef = double(ImRef);
    ImNew = double(ImNew);
    [Ny, Nx] = size(ImRef);

    % --- preprocessing ---------------------------------------------------
    if Args.ReplaceNonFinite
        ImRef = replaceNonFinite(ImRef);
        ImNew = replaceNonFinite(ImNew);
    elseif ~(all(isfinite(ImRef(:))) && all(isfinite(ImNew(:))))
        warning('shiftFromXCorr:nonFinite', ...
            ['Non-finite pixels present; the FFT will propagate NaN/Inf. ', ...
             'Set ReplaceNonFinite=true or pre-clean the images.']);
    end

    if Args.SubtractMean
        ImRef = ImRef - mean(ImRef(:));
        ImNew = ImNew - mean(ImNew(:));
    end

    if ~strcmp(Args.Window, 'none')
        W2 = localWindow(Ny, Nx, Args.Window, Args.WindowParam);
        ImRef = ImRef .* W2;
        ImNew = ImNew .* W2;
    end

    % --- FFT cross-correlation ------------------------------------------
    % Cross-power spectrum. With CPS = Fref .* conj(Fnew), the peak lag of
    % ifft2(CPS) equals the displacement of ImNew relative to ImRef.
    Fref = fft2(ImRef);
    Fnew = fft2(ImNew);
    CPS  = Fref .* conj(Fnew);

    c0    = ifft2(CPS);            % circular cross-correlation, zero lag at (1,1)
    XCorr = fftshift(real(c0));    % zero lag moved to the centre pixel

    % Centre (zero-lag) index, valid for even and odd sizes.
    Cy = floor(Ny/2) + 1;
    Cx = floor(Nx/2) + 1;

    % --- integer (coarse) peak ------------------------------------------
    if Args.AbsCorr
        CC = abs(XCorr);
    else
        CC = XCorr;
    end

    if ~isempty(Args.MaxShift)
        if isscalar(Args.MaxShift)
            Ry = Args.MaxShift;  Rx = Args.MaxShift;
        else
            Ry = Args.MaxShift(1);  Rx = Args.MaxShift(2);
        end
        [GX, GY] = meshgrid((1:Nx) - Cx, (1:Ny) - Cy);
        Mask = (abs(GY) <= Ry) & (abs(GX) <= Rx);
        CC(~Mask) = -Inf;
    end

    [~, Ind]   = max(CC(:));
    [Ipy, Ipx] = ind2sub([Ny, Nx], Ind);

    IntShiftY = Ipy - Cy;         % centred integer lag (row), may be negative
    IntShiftX = Ipx - Cx;         % centred integer lag (col)

    PeakCorr     = real(XCorr(Ipy, Ipx));
    NormFactor   = sqrt(sum(ImRef(:).^2) * sum(ImNew(:).^2));
    if NormFactor == 0, NormFactor = eps; end
    NormPeakCorr = PeakCorr / NormFactor;     % ~[-1,1], circular NCC

    % --- sub-pixel refinement: sinc interpolation near the peak ----------
    if Args.SubPixel
        usfac    = Args.UpSampling;
        W        = ceil(usfac * 1.5);         % fine window spans ~+/-0.75 px
        dftshift = floor(W / 2);              % index of the window centre
        % Sample c0 on a W x W fine grid centred on the integer peak.
        roff = dftshift - IntShiftY * usfac;
        coff = dftshift - IntShiftX * usfac;
        CCups = idftups(CPS, W, W, usfac, roff, coff);

        [~, IndU]  = max(abs(CCups(:)));
        [Ruy, Rux] = ind2sub([W, W], IndU);

        SubShiftY   = IntShiftY + ((Ruy - 1) - dftshift) / usfac;
        SubShiftX   = IntShiftX + ((Rux - 1) - dftshift) / usfac;
        SubPeakCorr = real(CCups(Ruy, Rux));
    else
        SubShiftY   = IntShiftY;
        SubShiftX   = IntShiftX;
        SubPeakCorr = PeakCorr;
    end

    % --- pack output -----------------------------------------------------
    Shift = struct();
    Shift.Shift        = [SubShiftY, SubShiftX];   % [row col]
    Shift.ShiftXY      = [SubShiftX, SubShiftY];   % [x   y]
    Shift.IntShift     = [IntShiftY, IntShiftX];
    Shift.PeakCorr     = PeakCorr;
    Shift.NormPeakCorr = NormPeakCorr;
    Shift.SubPeakCorr  = SubPeakCorr;
    Shift.PeakInd      = [Ipy, Ipx];
    Shift.Method       = Args.Method;
    Shift.UpSampling   = Args.UpSampling;
    Shift.SubPixel     = Args.SubPixel;
    Shift.ImSize       = [Ny, Nx];
    Shift.Convention   = ['Shift = position of ImNew relative to ImRef ', ...
                          '(feature at r in ImRef appears at r+Shift in ImNew).'];
end

% ======================================================================= %
%                          Local helper functions                         %
% ======================================================================= %
function out = idftups(in, nor, noc, usfac, roff, coff)
% Localized upsampled inverse DFT via matrix multiplication.
% Returns ifft2(in) sampled on an (nor x noc) grid at 1/usfac-pixel spacing,
% with the output origin offset by (roff, coff) fine-grid samples. This is
% exact periodic-sinc (Dirichlet) interpolation of ifft2; for usfac=1,
% roff=coff=0, nor=nr, noc=nc it reproduces ifft2(in).
    [nr, nc] = size(in);
    % Frequencies in FFT order (centred ramp -> ifftshift gives fft ordering).
    fr  = ifftshift( (0:nr-1) - floor(nr/2) );    % 1 x nr  (rows)
    fc  = ifftshift( (0:nc-1) - floor(nc/2) );    % 1 x nc  (cols)
    or_ = ( (0:nor-1).' - roff );                 % nor x 1 (fine row positions)
    oc_ = ( (0:noc-1)   - coff );                 % 1 x noc (fine col positions)
    % Separable inverse-DFT kernels (+2*pi*i for the inverse transform).
    kernr = exp( (1i*2*pi/(nr*usfac)) * (or_ * fr) );    % nor x nr
    kernc = exp( (1i*2*pi/(nc*usfac)) * (fc.' * oc_) );  % nc  x noc
    out   = (kernr * in * kernc) / (nr * nc);            % nor x noc
end

function X = replaceNonFinite(X)
% Replace NaN/Inf by the mean of the finite pixels.
    bad = ~isfinite(X);
    if any(bad(:))
        good = X(~bad);
        if isempty(good)
            X(:) = 0;
        else
            X(bad) = mean(good);
        end
    end
end

function W2 = localWindow(Ny, Nx, type, alpha)
% Separable 2-D apodization window (no toolbox dependency).
    wy = window1(Ny, type, alpha);
    wx = window1(Nx, type, alpha);
    W2 = wy(:) * wx(:).';
end

function w = window1(M, type, alpha)
    if M < 2
        w = ones(M, 1);
        return;
    end
    n = (0:M-1).';
    switch lower(type)
        case 'hann'
            w = 0.5 * (1 - cos(2*pi*n/(M-1)));
        case 'hamming'
            w = 0.54 - 0.46 * cos(2*pi*n/(M-1));
        case 'tukey'
            w = ones(M, 1);
            edge = floor(alpha * (M-1) / 2);
            if edge >= 1
                k     = (0:edge-1).';
                taper = 0.5 * (1 + cos(pi * (2*k/(alpha*(M-1)) - 1)));
                w(1:edge)            = taper;
                w(end-edge+1:end)    = flipud(taper);
            end
        otherwise   % 'none'
            w = ones(M, 1);
    end
end