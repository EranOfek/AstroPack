function Result = e2ADU(Count, LowGain, IncludeGainBit)
    % convert ULTRASAT image count number (electrons) into the ADU output number
    % with 13 bits we can only keep 3 precision digits (10 bits) and the exponent (3 bits)
    % the first bit determines the gain, so we have a 14-bit binary
    % NB: the range of normally conveted numbers is from 1 to 10^8
    %
    % NB (rounding/quantization, 2026): this is a floating-point-style encoding, not a
    % fixed-LSB (least significant bit) linear ADC model. Count is rounded to 3
    % significant digits: Prec = round(Count / 10^(Exp-2)) in [100, 999], with
    % Exp = floor(log10(Count)) recording the decimal exponent, so the effective
    % rounding step size is 10^(Exp-2) electrons and grows with Count itself (e.g. ~1 e-
    % near Count ~ 100-999, ~10 e- near Count ~ 1000-9999, ~100 e- near Count ~ 1e4-1e5,
    % etc.) -- it is NOT a constant 1 e- (high gain) / 10 e- (low gain) step across the
    % whole range, even though it happens to coincide with those figures in the low
    % hundreds/thousands. LowGain only sets the top gain-flag bit (2^13) recording which
    % gain regime a pixel used; it does not otherwise change the rounding behavior above.
    %
    % Input: - the number of electrons
    %        - the low gain flag
    %        - IncludeGainBit: if true (default), pack the gain flag into the top bit
    %          (2^13) as usual, producing the standard 14-bit value. If false, omit it
    %          entirely (LowGain is then unused), producing a pure 13-bit
    %          precision+exponent value -- for a production-mode readout where the gain
    %          selection is instead recorded in a separate per-pixel gain map.
    % Output: - an integer number, which can be losslessly converted to a 14-bit
    %           (IncludeGainBit = true) or 13-bit (IncludeGainBit = false) binary
    % Author: A.M. Krassilchthcikov (Jul 2023)
    % Example: Count = [1 100 1e5]; LowGain = [0 0 1];
    %          R = ultrasat.e2ADU(Count, LowGain)
    %          R13 = ultrasat.e2ADU(Count, LowGain, false); % no gain bit
    arguments
        Count
        LowGain
        IncludeGainBit (1,1) logical = true
    end

    Exp  = floor( log10(Count) );

    Prec = round( Count ./ 10.^(Exp-2) );

    if IncludeGainBit
        Result = int16( 2^13 .* LowGain + 2^10 .* Exp + 2^0 .* Prec);
    else
        Result = int16( 2^10 .* Exp + 2^0 .* Prec);
    end

    % test output:

%     dec2bin( 2^0  .* Prec )
%     dec2bin( 2^10 .* Exp )
%     dec2bin(Result)

end