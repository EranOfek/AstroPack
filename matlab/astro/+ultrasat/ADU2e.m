function [Result, Counts] = ADU2e(ADU, Args)
    % convert ULTRASAT ADU into the count number (electrons) 
    % with 13 bits we can only keep 3 precision digits (10 bits) and the exponent (3 bits)
    % the first bit determines the gain, so we have a 14-bit binary 
    % NB: the range of normally conveted numbers is from 1 to 10^8
    % Input:  - the ADU code
    %       * ...,key,val,... 
    %       'LowGain' - the low gain conversion ratio
    %       'HighGain' - the high gain conversion ratio
    % Output: - the converted value and the count (electron) number
    % Author: A.M. Krassilchthcikov (Jul 2023)
    arguments
        ADU
        Args.LowGain   = 0.074;
        Args.HighGain  = 1.185;
    end
    
    ADU = single(ADU);
    
    Bit14 = floor( ADU ./ 2^13 );
    Exp   = floor( ( ADU - Bit14 * 2^13 ) ./ 2^10 );
    Prec = ADU - Bit14 .* 2^13 - Exp .* 2^10;
    Result = (Prec./10^2) .* 10.^(Exp);
    
    if Bit14 == 1
        Counts = Result / Args.LowGain;
    else
        Counts = Result / Args.HighGain;     
    end
    
end