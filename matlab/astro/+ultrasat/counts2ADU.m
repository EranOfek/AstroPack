function [ADU, Info] = counts2ADU(Image, LowGain, Args)
    % Convert an ULTRASAT image in electrons into the packed ADU words of the camera, with a bias and the ADU read noise
    %   Each pixel is multiplied by the e-/ADU gain of its gain (high or low), the
    %   bias level of that gain is added, and so is the read noise the camera adds
    %   after the gain, in each gain's own readout; the value is then packed by
    %   ultrasat.e2ADU. Only the part of the ADU read noise that is not already in
    %   the image is added, with the standard deviation
    %   sqrt(max(ReadNoiseADU^2 - (ReadNoiseE*Gain)^2, 0)).
    %   The default bias and ADU read noise are the medians measured on 44 DESY
    %   laboratory devices (run 41) in frames of zero exposure at -50 C (223 K);
    %   they are not measured at other temperatures (issue #1505).
    % Input  : - Image in counts [e-].
    %          - The gain of each pixel: 1 = low gain, 0 = high gain; a scalar or a
    %            matrix of the size of Image.
    %          * ...,key,val,...
    %            'Gain' - [high low] e-/ADU conversion [ADU/e-]. Default is [1.185 0.074].
    %            'Bias' - [high low] bias levels [ADU], or a 1x2 cell {High, Low} of
    %                   scalars or maps of the size of Image. Default is [398.75 412.80].
    %            'ReadNoiseADU' - [high low] read noise added after the gain [ADU].
    %                   Default is [1.93 1.99].
    %            'ReadNoiseE' - read noise already in Image [e-]: a scalar or a map of
    %                   the size of Image. Default is 0.
    %            'IncludeGainBit' - put the gain in the top bit (see ultrasat.e2ADU).
    %                   Default is true.
    % Output : - The ADU image (int16, the size of Image).
    %          - Structure: .Gain, .Bias (means of the maps), .ReadNoiseADU,
    %            .ExtraNoiseADU (rms of the added noise), all [high low]; .BiasIsMap;
    %            .BiasTemp (223 [K]); .Header, an N x 3 cell {key, value, comment} of
    %            these values for the header of an ADU image.
    % Author : A.M. Krassilchtchikov (Nov 2026)
    % Example: ADU = ultrasat.counts2ADU(100*ones(10), 0);
    %          [ADU, Info] = ultrasat.counts2ADU(Image, Image > 1.6e4, 'ReadNoiseE', sqrt(8));
    %          ADU = ultrasat.counts2ADU(Image, 1, 'Bias', [0 0], 'ReadNoiseADU', [0 0]); % no bias, no ADU noise

    arguments
        Image {mustBeNumeric}
        LowGain
        Args.Gain (1,2) {mustBeNumeric, mustBePositive} = [1.185 0.074];
        Args.Bias = [398.75 412.80];
        Args.ReadNoiseADU (1,2) {mustBeNumeric, mustBeNonnegative} = [1.93 1.99];
        Args.ReadNoiseE {mustBeNumeric, mustBeNonnegative} = 0;
        Args.IncludeGainBit (1,1) logical = true;
    end

    BiasTemp = 223;   % [K] the temperature of the DESY tests that give the default bias and noise

    Size = size(Image);
    LowGain = logical(LowGain);
    if ~isscalar(LowGain) && ~isequal(size(LowGain), Size)
        error('counts2ADU: the gain flag must be a scalar or of the size of the image, exiting..');
    end
    if ~isscalar(Args.ReadNoiseE) && ~isequal(size(Args.ReadNoiseE), Size)
        error('counts2ADU: ReadNoiseE must be a scalar or of the size of the image, exiting..');
    end

    % the bias of each gain: a scalar or a map
    if isnumeric(Args.Bias) && numel(Args.Bias) == 2
        Bias = {Args.Bias(1), Args.Bias(2)};
    elseif iscell(Args.Bias) && numel(Args.Bias) == 2
        Bias = Args.Bias;
    else
        error('counts2ADU: Bias must be [high low] or a cell {High, Low}, exiting..');
    end
    for Ig = 1:2
        if ~isnumeric(Bias{Ig}) || ~(isscalar(Bias{Ig}) || isequal(size(Bias{Ig}), Size)) || ...
                ~all(isfinite(Bias{Ig}), 'all')
            error('counts2ADU: each bias must be a finite scalar or a map of the size of the image, exiting..');
        end
    end

    % the value of each pixel's own gain; LowGain may be a scalar
    Sel = @(High, Low) single(High) .* ~LowGain + single(Low) .* LowGain;

    Gain  = Sel(Args.Gain(1), Args.Gain(2));
    Value = single(Image) .* Gain + Sel(Bias{1}, Bias{2});

    % the read noise the image is missing after the gain [ADU]
    RNe   = single(Args.ReadNoiseE);
    Extra = sqrt(max(Sel(Args.ReadNoiseADU(1), Args.ReadNoiseADU(2)).^2 - (RNe .* Gain).^2, 0));
    if any(Extra > 0, 'all')
        Value = Value + Extra .* randn(Size, 'single');
    end

    % ultrasat.e2ADU takes the log10 of its input: keep it at 1 ADU or above
    ADU = ultrasat.e2ADU(max(Value, 1), LowGain, Args.IncludeGainBit);

    if nargout > 1
        ExtraHL = sqrt(max(Args.ReadNoiseADU.^2 - mean(RNe(:).^2) .* Args.Gain.^2, 0));
        Info.Gain          = Args.Gain;
        Info.Bias          = [mean(Bias{1}, 'all') mean(Bias{2}, 'all')];
        Info.BiasIsMap     = ~isscalar(Bias{1}) || ~isscalar(Bias{2});
        Info.ReadNoiseADU  = Args.ReadNoiseADU;
        Info.ExtraNoiseADU = ExtraHL;
        Info.BiasTemp      = BiasTemp;
        Info.Header = { ...
            'BIASHI',   Info.Bias(1),         'bias, high gain [ADU], mean of a map'; ...
            'BIASLO',   Info.Bias(2),         'bias, low gain [ADU], mean of a map'; ...
            'BIASMAP',  ternary(Info.BiasIsMap), 'the bias is a per-pixel map'; ...
            'RDNADUHI', Args.ReadNoiseADU(1), 'read noise after the gain, high gain [ADU]'; ...
            'RDNADULO', Args.ReadNoiseADU(2), 'read noise after the gain, low gain [ADU]'; ...
            'BIASTEMP', BiasTemp,             'T [K] of DESY tests for default BIAS/RDNADU'; ...
            };
    end
end

function Str = ternary(Flag)
    % 'T' or 'F' for a header value
    if Flag
        Str = 'T';
    else
        Str = 'F';
    end
end
