function [Result, Counts] = ADU2e(ADU, Args)
    % convert ULTRASAT ADU into the count number (electrons) 
    % with 13 bits we can only keep 3 precision digits (10 bits) and the exponent (3 bits)
    % the first bit determines the gain, so we have a 14-bit binary 
    % NB: the range of normally converted numbers is from 1 to 10^8
    % Input:  - the ADU code
    %       * ...,key,val,... 
    %       'LowGain' - the low gain conversion ratio
    %       'HighGain' - the high gain conversion ratio
    %       'GainMask' - the gain mask (1 = low gain) of the same size as the ADU image, needed
    %                    for 13-bit ADU without the gain bit (see ultrasat.e2ADU). If empty,
    %                    the gain of each pixel is taken from the top bit. Default is [].
    %       'Bias'     - [high low] bias levels [ADU] subtracted before the division by the gain,
    %                    or a 1x2 cell {High, Low} of scalars or maps of the size of the ADU image
    %                    (see ultrasat.counts2ADU, whose default is [398.75 412.80]). Default is [0 0].
    % Output: - the converted value (ADU), the bias not subtracted
    %         - the count (electron) number: (ADU - bias) / gain
    % Author: A.M. Krassilchtchikov (Jul 2023)
    arguments
        ADU
        Args.LowGain   = 0.074;
        Args.HighGain  = 1.185;       
        Args.GainMask  = [];
        Args.Bias      = [0 0];
    end
    
    ADU = single(ADU);
    
    Bit14 = floor( ADU ./ 2^13 );
    Exp   = floor( ( ADU - Bit14 * 2^13 ) ./ 2^10 );
    Prec = ADU - Bit14 .* 2^13 - Exp .* 2^10;
    Result = (Prec./10^2) .* 10.^(Exp);
    
    % the gain of each pixel
    IsLow = Bit14 > 0.5;
    if ~isempty(Args.GainMask)
        if ~isequal(size(Args.GainMask), size(ADU))
            error('ADU2e: the sizes of the gain mask and of the ADU image differ, exiting..');
        end
        Mask = Args.GainMask > 0.5;
        if any(IsLow & ~Mask, 'all')
            error('ADU2e: some pixels have the low gain bit, but are high gain in the gain mask, exiting..');
        end
        IsLow = Mask;
    end
    Gain = repmat(single(Args.HighGain), size(ADU));
    Gain(IsLow) = Args.LowGain;

    % the bias of each pixel's gain
    if isnumeric(Args.Bias) && numel(Args.Bias) == 2
        Bias = {Args.Bias(1), Args.Bias(2)};
    elseif iscell(Args.Bias) && numel(Args.Bias) == 2
        Bias = Args.Bias;
    else
        error('ADU2e: Bias must be [high low] or a cell {High, Low}, exiting..');
    end
    if ~all(cellfun(@(B) isnumeric(B) && (isscalar(B) || isequal(size(B), size(ADU))), Bias))
        error('ADU2e: each bias must be a scalar or a map of the size of the ADU image, exiting..');
    end
    if any(Bias{1} ~= 0, 'all') || any(Bias{2} ~= 0, 'all')
        Counts = (Result - (single(Bias{1}) .* ~IsLow + single(Bias{2}) .* IsLow)) ./ Gain;
    else
        Counts = Result ./ Gain;
    end
    
end