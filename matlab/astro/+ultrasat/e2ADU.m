function Result = e2ADU(Count, LowGain)
    % convert ULTRASAT image count number (electrons) into the ADU output number
    % with 13 bits we can only keep 3 precision digits (10 bits) and the exponent (3 bits)
    % the first bit determines the gain, so we have a 14-bit binary 
    % NB: the range of normally conveted numbers is from 1 to 10^8
    % Input: - the number of electrons
    %        - the low gain flag
    % Output: - an integer number, which can be losslessly converted to a 14-bit binary
    % Author: A.M. Krassilchthcikov (Jul 2023)
    arguments
        Count
        LowGain
    end
    
    Exp  = floor( log10(Count) );
    
    Prec = round( Count ./ 10.^(Exp-2) );
        
    Result = int16( 2^13 .* LowGain + 2^10 .* Exp + 2^0 .* Prec);     
   
    % test output:
   
%     dec2bin( 2^0  .* Prec )
%     dec2bin( 2^10 .* Exp )
%     dec2bin(Result)
    
end