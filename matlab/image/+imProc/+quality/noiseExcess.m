function [Flag, Ratio, Sigma, SigmaExp] = noiseExcess(AI, Args)
    % Identify raw images with excess pixel-to-pixel (salt-and-pepper) noise
    %   The pixel-to-pixel noise is measured in a grid of square boxes
    %   spread evenly over the illuminated region (CCDSEC), as the robust
    %   std of the differences between adjacent pixels along rows
    %   (1.4826*MAD/sqrt(2)); sky gradients and the sky level cancel in
    %   these differences. In each box it is compared with the noise
    %   expected from the box median: SigmaExp = sqrt((Median-Offset)/Gain
    %   + (RN/Gain)^2), where the Offset is the median of the overscan, and
    %   Gain [e/ADU] and RN [e] are read from the header. The image ratio
    %   is that of the worst box, so noise away from the centre is caught
    %   too (issue LAST_issues #242).
    %   Such frames give tens of thousands of false sources per sub image
    %   (issue #1359).
    % Input  : - An array of AstroImage objects (raw images).
    %          * ...,key,val,...
    %            'HalfSize' - Half size [pix] of the square boxes in which
    %                   the noise is measured. Default is 250.
    %            'Nbox' - Number of boxes [along X, along Y]. [1 1] is a
    %                   single box at the centre of CCDSEC.
    %                   Default is [4 6].
    %            'CCDSEC' - Illuminated region [Xmin Xmax Ymin Ymax] over
    %                   which the boxes are spread. If empty, or not inside
    %                   the image, the whole image is used.
    %                   Default is [1 6388 25 9600].
    %            'OverscanSec' - Overscan section [Xmin Xmax Ymin Ymax]
    %                   from which the offset is measured. If empty, or
    %                   not inside the image, Args.Offset is used.
    %                   Default is [6392 6420 100 9500].
    %            'Offset' - Offset [ADU] used when the overscan is not
    %                   available. If empty, the ratio of such an image is
    %                   NaN and the image is not flagged. Default is [].
    %            'MaxRatio' - An image with Sigma/SigmaExp above this value
    %                   is flagged as bad. Default is 2.3.
    %            'GainKey' - Header keyword of the gain [e/ADU].
    %                   Default is 'GAIN'.
    %            'ReadNoiseKey' - Header keyword of the read noise [e].
    %                   Default is 'READNOI'.
    %            'Gain' - Gain [e/ADU] used when the header value is
    %                   missing. Default is 0.75.
    %            'ReadNoise' - Read noise [e] used when the header value is
    %                   missing. Default is 3.
    % Output : - Array of flags indicating if the image is ok, i.e., its
    %            Ratio is not above Args.MaxRatio. Images for which the
    %            ratio can not be measured (NaN) are not flagged.
    %          - Array of Sigma/SigmaExp ratios of the worst box.
    %          - Array of the measured pixel-to-pixel noise [ADU] in the
    %            worst box.
    %          - Array of the expected noise [ADU] in the worst box.
    % Author : A.M. Krassilchtchikov (2026 Oct)
    % Example: [IsGood, Ratio] = imProc.quality.noiseExcess(AI);
    %          [IsGood, Ratio] = imProc.quality.noiseExcess(AI, 'Nbox',[1 1]); % centre only

    arguments
        AI AstroImage
        Args.HalfSize       = 250;
        Args.Nbox           = [4 6];
        Args.CCDSEC         = [1 6388 25 9600];
        Args.OverscanSec    = [6392 6420 100 9500];
        Args.Offset         = [];
        Args.MaxRatio       = 2.3;
        Args.GainKey        = 'GAIN';
        Args.ReadNoiseKey   = 'READNOI';
        Args.Gain           = 0.75;
        Args.ReadNoise      = 3;
    end

    Size     = size(AI);
    Ratio    = nan(Size);
    Sigma    = nan(Size);
    SigmaExp = nan(Size);

    Nim = numel(AI);
    for Iim=1:1:Nim
        Image    = AI(Iim).ImageData.Image;
        [Ny, Nx] = size(Image);

        % region over which the boxes are spread
        Sec = Args.CCDSEC;
        if isempty(Sec) || Sec(1)<1 || Sec(2)>Nx || Sec(3)<1 || Sec(4)>Ny
            Sec = [1 Nx 1 Ny];
        end

        if (Sec(2)-Sec(1)+1)>=2*Args.HalfSize && (Sec(4)-Sec(3)+1)>=2*Args.HalfSize
            Gain      = headerValue(AI(Iim), Args.GainKey, Args.Gain);
            ReadNoise = headerValue(AI(Iim), Args.ReadNoiseKey, Args.ReadNoise);

            % offset: overscan median, or the fallback value
            Offset = Args.Offset;
            OSec   = Args.OverscanSec;
            if ~isempty(OSec) && OSec(1)>=1 && OSec(2)<=Nx && OSec(3)>=1 && OSec(4)<=Ny
                Offset = median(Image(OSec(3):OSec(4), OSec(1):OSec(2)), 'all', 'omitnan');
            end
            % double: on a raw integer image the arithmetic below would saturate
            Offset = double(Offset);

            if ~isempty(Offset)
                % lower-left corners of the boxes (a single box is centred)
                X1 = boxStarts(Sec(1), Sec(2), Args.HalfSize, Args.Nbox(1));
                Y1 = boxStarts(Sec(3), Sec(4), Args.HalfSize, Args.Nbox(2));
                Box = 2.*Args.HalfSize - 1;

                BoxSigma    = nan(numel(Y1), numel(X1));
                BoxSigmaExp = nan(numel(Y1), numel(X1));
                for Iy=1:1:numel(Y1)
                    for Ix=1:1:numel(X1)
                        Patch = double(Image(Y1(Iy):Y1(Iy)+Box, X1(Ix):X1(Ix)+Box));

                        Diff  = diff(Patch, 1, 2);
                        Diff  = Diff - median(Diff, 'all', 'omitnan');
                        BoxSigma(Iy,Ix) = 1.4826.*median(abs(Diff), 'all', 'omitnan')./sqrt(2);

                        Sky   = max(median(Patch, 'all', 'omitnan') - Offset, 0);
                        BoxSigmaExp(Iy,Ix) = sqrt(Sky./Gain + (ReadNoise./Gain).^2);
                    end
                end

                % worst box
                [Ratio(Iim), Imax] = max(BoxSigma(:)./BoxSigmaExp(:));
                Sigma(Iim)    = BoxSigma(Imax);
                SigmaExp(Iim) = BoxSigmaExp(Imax);
            end
        end
    end

    Flag  = ~(Ratio > Args.MaxRatio);

end

% Aux functions:
function Val = headerValue(AI, Key, Default)
    % Positive numeric header value, or Default if missing/invalid
    Val = AI.HeaderData.getVal(Key);
    if ~isnumeric(Val) || ~isscalar(Val) || ~isfinite(Val) || Val<=0
        Val = Default;
    end
end

function Start = boxStarts(Min, Max, HalfSize, Nbox)
    % First pixels of Nbox boxes of size 2*HalfSize spread evenly over [Min, Max]
    if Nbox==1
        Start = floor((Min+Max+1)./2) - HalfSize;
    else
        Start = round(linspace(Min, Max-2.*HalfSize+1, Nbox));
    end
end
