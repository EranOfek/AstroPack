function [Flag, Ratio, Sigma, SigmaExp] = noiseExcess(AI, Args)
    % Identify raw images with excess pixel-to-pixel (salt-and-pepper) noise
    %   The pixel-to-pixel noise is measured in a central patch as the
    %   robust std of the differences between adjacent pixels along rows
    %   (1.4826*MAD/sqrt(2)); sky gradients and the sky level cancel in
    %   these differences. It is compared with the noise expected from the
    %   patch median: SigmaExp = sqrt((Median-Offset)/Gain + (RN/Gain)^2),
    %   where the Offset is the median of the overscan, and Gain [e/ADU]
    %   and RN [e] are read from the header.
    %   Such frames give tens of thousands of false sources per sub image
    %   (issue #1359).
    % Input  : - An array of AstroImage objects (raw images).
    %          * ...,key,val,...
    %            'HalfSize' - Half size [pix] of the central square patch
    %                   in which the noise is measured. Default is 250.
    %            'OverscanSec' - Overscan section [Xmin Xmax Ymin Ymax]
    %                   from which the offset is measured. If empty, or
    %                   not inside the image, Args.Offset is used.
    %                   Default is [6392 6420 100 9500].
    %            'Offset' - Offset [ADU] used when the overscan is not
    %                   available. If empty, the ratio of such an image is
    %                   NaN and the image is not flagged. Default is [].
    %            'MaxRatio' - An image with Sigma/SigmaExp above this value
    %                   is flagged as bad. Default is 4.
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
    %          - Array of Sigma/SigmaExp ratios.
    %          - Array of the measured pixel-to-pixel noise [ADU].
    %          - Array of the expected noise [ADU].
    % Author : A.M. Krassilchtchikov (2026 Oct)
    % Example: [IsGood, Ratio] = imProc.quality.noiseExcess(AI);

    arguments
        AI AstroImage
        Args.HalfSize       = 250;
        Args.OverscanSec    = [6392 6420 100 9500];
        Args.Offset         = [];
        Args.MaxRatio       = 4;
        Args.GainKey        = 'GAIN';
        Args.ReadNoiseKey   = 'READNOI';
        Args.Gain           = 0.75;
        Args.ReadNoise      = 3;
    end

    Size     = size(AI);
    Sigma    = nan(Size);
    SigmaExp = nan(Size);

    Nim = numel(AI);
    for Iim=1:1:Nim
        Image    = AI(Iim).ImageData.Image;
        [Ny, Nx] = size(Image);
        if Ny>=2*Args.HalfSize && Nx>=2*Args.HalfSize
            Gain      = headerValue(AI(Iim), Args.GainKey, Args.Gain);
            ReadNoise = headerValue(AI(Iim), Args.ReadNoiseKey, Args.ReadNoise);

            % offset: overscan median, or the fallback value
            Offset = Args.Offset;
            Sec    = Args.OverscanSec;
            if ~isempty(Sec) && Sec(1)>=1 && Sec(2)<=Nx && Sec(3)>=1 && Sec(4)<=Ny
                Offset = median(Image(Sec(3):Sec(4), Sec(1):Sec(2)), 'all', 'omitnan');
            end
            % double: on a raw integer image the arithmetic below would saturate
            Offset = double(Offset);

            if ~isempty(Offset)
                Cx    = floor(Nx./2);
                Cy    = floor(Ny./2);
                Patch = double(Image(Cy-Args.HalfSize+1:Cy+Args.HalfSize, Cx-Args.HalfSize+1:Cx+Args.HalfSize));

                Diff  = diff(Patch, 1, 2);
                Diff  = Diff - median(Diff, 'all', 'omitnan');
                Sigma(Iim) = 1.4826.*median(abs(Diff), 'all', 'omitnan')./sqrt(2);

                Sky   = max(median(Patch, 'all', 'omitnan') - Offset, 0);
                SigmaExp(Iim) = sqrt(Sky./Gain + (ReadNoise./Gain).^2);
            end
        end
    end

    Ratio = Sigma./SigmaExp;
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
