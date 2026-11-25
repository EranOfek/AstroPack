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
    % Output: - the converted value (ADU)
    %         - the count (electron) number
    % Author: A.M. Krassilchtchikov (Jul 2023)
    arguments
        ADU
        Args.LowGain   = 0.074;
        Args.HighGain  = 1.185;       
        Args.GainMask  = [];
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
    Counts = Result ./ Gain;
    
end